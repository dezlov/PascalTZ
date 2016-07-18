@echo off
setlocal

cd /d %~dp0
lazbuild -B -q tester.lpi
if %errorlevel% neq 0 (
	echo FAILED TO BUILD PROJECT!
	exit /b %errorlevel%
)
echo Built successfully!
