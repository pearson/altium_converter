cd dblib-converter
convert_all.exe
cd ..

mkdir G:\Applications\KiCad\PcbLib.new
xcopy PcbLib G:\Applications\KiCad\PcbLib.new /S /EXCLUDE:upload_exclude.txt
rmdir /s /q G:\Applications\KiCad\PcbLib.old
move G:\Applications\KiCad\PcbLib G:\Applications\KiCad\PcbLib.old
move G:\Applications\KiCad\PcbLib.new G:\Applications\KiCad\PcbLib

mkdir G:\Applications\KiCad\SchLib.new
xcopy altium_converter\dblib-converter\output G:\Applications\KiCad\SchLib.new /S /EXCLUDE:upload_exclude.txt
rmdir /s /q G:\Applications\KiCad\SchLib.old
move G:\Applications\KiCad\SchLib G:\Applications\KiCad\SchLib.old
move G:\Applications\KiCad\SchLib.new G:\Applications\KiCad\SchLib

pause