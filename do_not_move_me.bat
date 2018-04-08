@ ECHO OFF

setlocal ENABLEDELAYEDEXPANSION
set word=/
set str=%~dp0
set str=%str:\=!word!%

@ echo setwd('%str%') > R\working_directory.R

TASKKILL /F /IM EXCEL.exe

ECHO -
ECHO -
ECHO -
ECHO R is running - please wait until this black window disappears 
ECHO R can take up to 1 hour depending on the number of transactions booked
ECHO -
ECHO You can also check the log folder to check the program status
ECHO -
ECHO Please wait... [up to 1 hour]
ECHO -
ECHO This program is interactive:
ECHO -
ECHO YOU NEED TO ANSWER USER PROMPTS!

CALL "C:\Program Files\R\R-3.4.2\bin\x64\RScript.exe" "R\working_directory.R"

if not exist "log\" mkdir log\

CALL "C:\Program Files\R\R-3.4.2\bin\x64\RScript.exe" "R\booking.R" > log\log_%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%-%TIME:~0,2%h%TIME:~3,2%m.txt 2>&1

PAUSE

TASKKILL /F /IM EXCEL.exe