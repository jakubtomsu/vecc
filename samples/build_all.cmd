build hello_world || goto :error
build test1 || goto :error
build text || goto :error
build julia_set || goto :error
build game || goto :error

goto :EOF
:error
exit /b 2