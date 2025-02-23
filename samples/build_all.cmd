call build hello_world || goto :error
call build game || goto :error
call build text || goto :error
call build julia_set || goto :error

goto :EOF

:error
exit /b 2