@echo off

cd OFuncLib
call build.cmd Test
if errorlevel 1 (
  exit /b %errorlevel%
)
cd ..

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

SET TARGET="MainBuild"

IF NOT [%1]==[] (set TARGET="%1")

"packages\FAKE\tools\Fake.exe" "build.fsx" "target=%TARGET%"

PAUSE
