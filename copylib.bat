@echo off
rem usage copylib.bat [--tgt] <dir> <src> ... [--tgt <dir> <src> ...]

echo %0 %*

set tgtDir=
set srcPattern=

rem initial --tgt is not mandatory
if not "%1" == "--tgt" (
  set tgtDir=%1 && shift
  goto :srcLoop
)

:tgtLoop
if not "%1" == "--tgt" ( goto :eof )
shift
set tgtDir=%1
shift

set srcPattern=
:srcLoop
if not "%1" == "" (
  if "%1" == "--tgt" ( goto :doCopy )
  set srcPattern=%srcPattern% %1 && shift
  goto :srcLoop
)

:doCopy
echo ####
echo #### copying %srcPattern%  to  %tgtDir%
for %%f in (%srcPattern%) do (
  echo copy %%f %tgtDir%
  copy /Y %%f %tgtDir%
)
goto :tgtLoop

