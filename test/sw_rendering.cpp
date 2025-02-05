#include <windows.h>
#include <stdint.h>

#pragma comment(lib, "Gdi32.lib")
#pragma comment(lib, "User32.lib")

#define WIDTH  320
#define HEIGHT 180

uint32_t buffer[WIDTH * HEIGHT];

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    switch (msg) {
        case WM_CLOSE:
            PostQuitMessage(0);
            return 0;
        case WM_PAINT: {
            PAINTSTRUCT ps;
            HDC hdc = BeginPaint(hwnd, &ps);

            BITMAPINFO bmi = {0};
            bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
            bmi.bmiHeader.biWidth = WIDTH;
            bmi.bmiHeader.biHeight = -HEIGHT; // Top-down
            bmi.bmiHeader.biPlanes = 1;
            bmi.bmiHeader.biBitCount = 32;
            bmi.bmiHeader.biCompression = BI_RGB;

            StretchDIBits(hdc, 0, 0, WIDTH * 3, HEIGHT * 3,  // Scale up
                         0, 0, WIDTH, HEIGHT,
                         buffer, &bmi, DIB_RGB_COLORS, SRCCOPY);

            EndPaint(hwnd, &ps);
            return 0;
        }
    }
    return DefWindowProc(hwnd, msg, wParam, lParam);
}

void fill_buffer() {
    for (int y = 0; y < HEIGHT; y++) {
        for (int x = 0; x < WIDTH; x++) {
            buffer[y * WIDTH + x] = ((x ^ y) & 0xFF);
        }
    }
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    WNDCLASS wc = {0};
    wc.lpfnWndProc = WndProc;
    wc.hInstance = hInstance;
    wc.lpszClassName = "SoftwareRenderer";
    RegisterClass(&wc);

    HWND hwnd = CreateWindow(wc.lpszClassName, "Win32 Software Rendering",
                             WS_OVERLAPPEDWINDOW | WS_VISIBLE,
                             CW_USEDEFAULT, CW_USEDEFAULT, WIDTH * 3, HEIGHT * 3,
                             NULL, NULL, hInstance, NULL);

    MSG msg;
    fill_buffer(); // Fill buffer with some pattern

    while (GetMessage(&msg, NULL, 0, 0)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
        InvalidateRect(hwnd, NULL, FALSE); // Force repaint
    }

    return 0;
}