SET mypath=%~dp0
FOR /F %%G IN ('DIR /b %mypath%*.ahk') DO (
    mklink %ProgramData%"\Microsoft\Windows\Start Menu\Programs\Startup\%%G" %mypath%%%G
)
