@echo off
where cl
if %errorlevel% NEQ 0 vcvarsall.bat x86_amd64
odin build . -out:vecc.exe

vecc test/julia_set.vecc && cl /I. /I.. /O2 /arch:AVX2 test/julia_set.cpp
if %errorlevel% NEQ 0 exit /b 2

vecc test/game.vecc && cl /I. /I.. /O2 /arch:AVX2 test/game.cpp
@REM vecc test/game.vecc && clang -I. -I.. -O2 -mavx2 test/game.cpp
if %errorlevel% NEQ 0 exit /b 2

@REM odin run . -- test/sw_renderer.vecc && clang test/sw_renderer.cpp -O2 -mavx2 -S -masm=intel