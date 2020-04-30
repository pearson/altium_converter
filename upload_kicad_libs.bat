rem Modify the line below to select the output directory for converted libraries
set DESTINATION=G:\Applications\KiCad

cd dblib-converter
convert_all.exe
cd ..

mkdir %DESTINATION%\PcbLib.new
xcopy PcbLib %DESTINATION%\PcbLib.new /S /EXCLUDE:upload_exclude.txt
rmdir /s /q %DESTINATION%\PcbLib.old
move %DESTINATION%\PcbLib %DESTINATION%\PcbLib.old
move %DESTINATION%\PcbLib.new %DESTINATION%\PcbLib

mkdir %DESTINATION%\SchLib.new
xcopy dblib-converter\output %DESTINATION%\SchLib.new /S /EXCLUDE:upload_exclude.txt
rmdir /s /q %DESTINATION%\SchLib.old
move %DESTINATION%\SchLib %DESTINATION%\SchLib.old
move %DESTINATION%\SchLib.new %DESTINATION%\SchLib

pause