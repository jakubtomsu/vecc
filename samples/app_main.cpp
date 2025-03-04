#include <windows.h>
#include <stdint.h>
#include <math.h>

#define SOKOL_AUDIO_IMPL
#include "sokol_audio.h"

#pragma comment(lib, "Gdi32.lib")
#pragma comment(lib, "User32.lib")
#pragma comment(lib, "Winmm.lib")

LARGE_INTEGER g_frequency = {0};
V8U32* g_framebuffer;
uint64_t g_start_clock;
uint64_t g_prev_clock;
uint32_t g_frame = 0;
uint32_t g_keys;

HBITMAP g_framebuffer_bitmap;

uint64_t time_clock() {
    LARGE_INTEGER counter;
    QueryPerformanceCounter(&counter);
    return counter.QuadPart;
}

#define NUM_SAMPLES 1024
#define AUDIO_BUFFER_CAP (NUM_SAMPLES * 4)
float g_audio_buffer[AUDIO_BUFFER_CAP];


double time_diff(uint64_t now, uint64_t prev) {
    return (double)(now - prev) / (double)g_frequency.QuadPart;
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    switch (msg) {
        case WM_CLOSE: {
            PostQuitMessage(0);
            return 0;
        } break;

        case WM_CREATE: {
            HDC dc = GetDC(hwnd);

            BITMAPINFO bitmap_info = {0};
            bitmap_info.bmiHeader.biSize        = sizeof(BITMAPINFOHEADER);
            bitmap_info.bmiHeader.biWidth       = RESOLUTION_X;
            bitmap_info.bmiHeader.biHeight      = -RESOLUTION_Y;
            bitmap_info.bmiHeader.biPlanes      = 1;
            bitmap_info.bmiHeader.biBitCount    = 32;
            bitmap_info.bmiHeader.biCompression = BI_RGB;

            g_framebuffer_bitmap = CreateDIBSection(dc, (BITMAPINFO*)&bitmap_info, DIB_RGB_COLORS, (void**)&g_framebuffer, 0, 0);
        } break;

        case WM_KEYDOWN: {
            switch (wParam) {
            case VK_LEFT:  g_keys |= KEY_LEFT_BIT; break;
            case VK_RIGHT: g_keys |= KEY_RIGHT_BIT; break;
            case VK_UP:    g_keys |= KEY_UP_BIT; break;
            case VK_DOWN:  g_keys |= KEY_DOWN_BIT; break;
            case 'Z':      g_keys |= KEY_Z_BIT; break;
            case 'X':      g_keys |= KEY_X_BIT; break;
            }
            return 0;
        } break;

        case WM_KEYUP: {
            switch (wParam) {
            case VK_LEFT:  g_keys &= ~KEY_LEFT_BIT; break;
            case VK_RIGHT: g_keys &= ~KEY_RIGHT_BIT; break;
            case VK_UP:    g_keys &= ~KEY_UP_BIT; break;
            case VK_DOWN:  g_keys &= ~KEY_DOWN_BIT; break;
            case 'Z':      g_keys &= ~KEY_Z_BIT; break;
            case 'X':      g_keys &= ~KEY_X_BIT; break;
            }
            return 0;
        } break;

        case WM_PAINT: {
            uint64_t begin_clock = time_clock();
            double delta = time_diff(begin_clock, g_prev_clock);
            double time = time_diff(begin_clock, g_start_clock);
            g_prev_clock = begin_clock;

            if (delta < 0.01) {
                // printf("sleep %i\n", frame_ms_left);
                int frame_ms_left = 15 - (int)(delta * 1000);
                Sleep(frame_ms_left / 2);
            }

            uint64_t compute_clock = time_clock();

            int num_audio_samples = saudio_expect();
            // printf("audio frames %i\n", num_audio_samples);

            if (num_audio_samples > AUDIO_BUFFER_CAP) {
                num_audio_samples = AUDIO_BUFFER_CAP;
            }

            compute_frame(
                g_framebuffer,
                {{RESOLUTION_X, RESOLUTION_Y}},
                (float)time,
                (float)delta,
                g_frame,
                g_keys,
                g_audio_buffer,
                num_audio_samples);

            if (num_audio_samples > 0) {
                saudio_push(g_audio_buffer, num_audio_samples);
            }

            uint64_t end_clock = time_clock();
            double compute_time = time_diff(end_clock, compute_clock);

            if ((g_frame % 30) == 0) {
                // printf("delta time: %g ms (%g fps), compute: %g ms\n", delta * 1e3, 1.0 / delta, compute_time * 1e3);
            }

            PAINTSTRUCT ps;
            HDC dc = BeginPaint(hwnd, &ps);

            HDC bitmap_dc = CreateCompatibleDC(dc);
            HGDIOBJ old_bitmap_handle = SelectObject(bitmap_dc, (HGDIOBJ)(g_framebuffer_bitmap));

            RECT client_rect;
            GetClientRect(hwnd, &client_rect);
            int width = client_rect.right - client_rect.left;
            int height = client_rect.bottom - client_rect.top;

            int scale_x = width / RESOLUTION_X;
            int scale_y = height / RESOLUTION_Y;
            int scale = scale_x < scale_y ? scale_x : scale_y;

            StretchBlt(dc, 0, 0, RESOLUTION_X * scale, RESOLUTION_Y * scale, bitmap_dc,
                0, 0, RESOLUTION_X, RESOLUTION_Y, SRCCOPY);

            SelectObject(bitmap_dc, old_bitmap_handle);
            DeleteDC(bitmap_dc);

            EndPaint(hwnd, &ps);

            g_frame += 1;

            return 0;
        }
    }
    return DefWindowProc(hwnd, msg, wParam, lParam);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    // Help sleep precision
    timeBeginPeriod(1);

    WNDCLASS wc = {0};
    wc.lpfnWndProc = WndProc;
    wc.hInstance = hInstance;
    wc.lpszClassName = "VecC_Sample";
    RegisterClass(&wc);

    RECT rect = {0, 0, RESOLUTION_X * RESOLUTION_SCALE, RESOLUTION_Y * RESOLUTION_SCALE};
    AdjustWindowRect(&rect,
        WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_VISIBLE,
        FALSE
    );

    HWND hwnd = CreateWindow(
        wc.lpszClassName, SAMPLE_NAME.data,
        WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_VISIBLE,
        CW_USEDEFAULT, CW_USEDEFAULT, rect.right - rect.left, rect.bottom - rect.top,
        // CW_USEDEFAULT, CW_USEDEFAULT, RESOLUTION_X * RESOLUTION_SCALE, RESOLUTION_Y * RESOLUTION_SCALE,
        NULL, NULL, hInstance, NULL);

    QueryPerformanceFrequency(&g_frequency);
    g_start_clock = time_clock();
    g_prev_clock = g_start_clock;

    if (AttachConsole(ATTACH_PARENT_PROCESS)) {
        // Redirect unbuffered STDOUT to the console
        HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
        if (h != INVALID_HANDLE_VALUE) {
            freopen("CONOUT$", "w", stdout);
            setvbuf(stdout, NULL, _IONBF, 0);
        }
    }

    saudio_desc desc = {0};
    desc.sample_rate = 44100;
    desc.buffer_frames = 1024;
    desc.packet_frames = 128;
    desc.num_packets = 8;
    saudio_setup(&desc);

    MSG msg;
    while (GetMessage(&msg, NULL, 0, 0)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
        InvalidateRect(hwnd, NULL, FALSE); // Force repaint
    }

    saudio_shutdown();

    return 0;
}