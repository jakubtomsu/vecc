@echo off
where cl
if %ERRORLEVEL% NEQ 0 vcvarsall.bat x86_amd64
odin run . -- test/sw_renderer.vecc && cl test/sw_renderer.cpp /O2 /arch:AVX2