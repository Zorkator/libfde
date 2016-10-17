@echo off

rem usage copylib.bat <tgtDir> <src> [...]
set tgtDir=%1     && shift
set srcPattern=%1 && shift

:loop
shift
if not "%1"=="" (
  set srcPattern=%srcPattern% %1
  goto :loop
)

for %%f in (%srcPattern%) do (
  copy %%f %tgtDir%
)

