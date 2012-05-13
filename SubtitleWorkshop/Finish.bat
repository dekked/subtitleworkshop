@echo off
"D:\Archivos de programa\Borland\Delphi7\Bin\dcc32.exe" "G:\URUSoft\Programs\SubtitleAPI_Old\SubtitleAPI.dpr"
"G:\UPX\upx.exe" "G:\URUSoft\Programs\Subtitle Workshop\2.51\Bin\SubtitleAPI\SubtitleAPI.dll" --compress-exports=1 --compress-resources=1 --strip-relocs --best --compress-icons=1

"D:\Archivos de programa\Borland\Delphi7\Bin\dcc32.exe" "G:\URUSoft\Programs\Subtitle Workshop\2.51\SubtitleWorkshop.dpr"
"G:\UPX\upx.exe" "G:\URUSoft\Programs\Subtitle Workshop\2.51\Bin\SubtitleWorkshop.exe" --compress-exports=1 --compress-resources=1 --strip-relocs --best --compress-icons=1
