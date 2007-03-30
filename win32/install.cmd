@echo off
rem
rem Simple Header install script for libfirm and friens, uses NT command shell
rem syntax
rem
rem (C) 2005 Universitaet Karlsruhe
rem
rem Michael Beck   beck@ipd.info.uni-karlsruhe.de
rem
rem 

set SRC=%1
set DST=%2

if "%1" == "" goto usage
if "%2" == "" goto usage

echo Installing Header files to %DST% ...

if not exist %DST% mkdir %DST%
for /F "eol=# tokens=1,2" %%i in (header.list) do if not exist "%DST%\%%j" mkdir %DST%\%%j
for /F "eol=# tokens=1,2" %%i in (header.list) do echo   %%i && copy /Y %SRC%\%%i %DST%\%%j >NUL
echo done.

goto end

:usage
echo Usage: %0 SRC-DIR DST-DIR
:end
