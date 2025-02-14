@echo off
where /Q cl || vcvarsall.bat x86_amd64

odin build .. -out:vecc.exe || goto :error

@REM vecc julia_set.vecc || goto :error
@REM cl /I. /I.. /O2 /arch:AVX2 julia_set.cpp || goto :error
@REM odin run . -- julia_set.vecc && clang julia_set.cpp -O2 -mavx2 -S -masm=intel

vecc %1.vecc || goto :error
cl /I. /I.. /O2 /arch:AVX2 %1.cpp || goto :error
@REM vecc game.vecc && clang -I. -I.. -O2 -mavx2 game.cpp


echo SUCCESS
goto :EOF

:error
echo FAILED
exit /b 2