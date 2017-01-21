@echo off
REM SIMPLE COMMAND.COM SCRIPT TO ASSEMBLE GAMEBOY FILES
REM REQUIRES MAKELNK.BAT
REM JOHN HARRISON
REM UPDATED 2008-01-28

if not exist obj mkdir obj

if exist bin\%1.gb del bin\%1.gb
REM IF THERE ARE SETTINGS WHICH NEED TO BE DONE ONLY ONCE, PUT THEM BELOW
rem if not %ASSEMBLE%1 == 1 goto begin
rem path=%path%;c:\GB_asm\
rem doskey UNNECESSARY ON DESKTOP --- DOSKEY ALREADY INSTALLED
rem set dir=c:\GB_asm\curren~1\
cmd /c makelnk %1 > obj\%1.link

:begin
set assemble=1
echo assembling...
rgbasm95 -iinc\ -oobj\%1.obj src\%1.asm
if errorlevel 1 goto end
echo linking...
xlink95 -mobj\map obj\%1.link
if errorlevel 1 goto end
echo fixing...
rgbfix95 -v obj\%1

:end
rem del *.obj