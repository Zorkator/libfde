@echo off
rem usage copylib.bat <srcDir> <tgtDir> <flagId>

set srcDir=%1
set tgtDir=%2
set flagId=%3

for %%f in (%srcDir%\adt_*.mod %srcDir%\libadt*) do (
  copy %%f %tgtDir%
)
type nul >> %tgtDir%\%flagId%

