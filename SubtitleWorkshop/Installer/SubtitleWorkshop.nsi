;-------------------------------------------------------------;
;               URUSoft Subtitle Workshop 2.51                ;
;                   Installer Script File                     ;
;-------------------------------------------------------------;

; Program information
!define PRODUCT "Subtitle Workshop"
!define PRODUCT_SHORTNAME "SubtitleWorkshop"
!define PRODUCT_EXENAME "subtitleworkshop.exe"
!define VER_MAJOR 2
!define VER_MINOR 51
!define VERSION "${VER_MAJOR}.${VER_MINOR}"
!define WEBSITE "http://www.urusoft.net/"
Name "${PRODUCT}"

; Various settings
SetCompressor lzma
BrandingText "Subtitle Workshop ${VER_MAJOR}.${VER_MINOR}"
SetOverwrite on
!include "${NSISDIR}\Contrib\Modern UI\System.nsh"
!include "Sections.nsh"
!include "LogicLib.nsh"
!define MUI_ABORTWARNING
!define MUI_UNINSTALLER
!define MUI_UNCONFIRMPAGE

; Cosmetic settings
!define MUI_UI "Res\usmodern.exe"
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "Res\usmodern-header2.bmp"
!define MUI_HEADERIMAGE_RIGHT
Icon "Res\my-install.ico"
UninstallIcon "Res\my-uninstall.ico"

; Path to files & installer output
!define FILESPATH "G:\URUSoft\Programs\Subtitle Workshop\${VER_MAJOR}.${VER_MINOR}\Bin"
!define LICENSEPATH "G:\URUSoft\Programs"
OutFile "${PRODUCT_SHORTNAME}${VER_MAJOR}${VER_MINOR}.exe"

!define MUI_FINISHPAGE_LINK "http://www.urusoft.net"
!define MUI_FINISHPAGE_LINK_LOCATION ${WEBSITE}

; Path to EXE, to display in the finish page after successful instalation
!define MUI_FINISHPAGE_RUN "$INSTDIR\${PRODUCT_EXENAME}"

; Installer pages
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE $(urusoftlicense)
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

; Installer language files
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "Spanish"

; License file
LicenseLangString urusoftlicense ${LANG_ENGLISH} "${LICENSEPATH}\License.txt"
LicenseLangString urusoftlicense ${LANG_SPANISH} "${LICENSEPATH}\Licencia.txt"

; Language strings for component-selection page - Titles
LangString TITLE_MainFiles ${LANG_ENGLISH} "Main files"
LangString TITLE_MainFiles ${LANG_SPANISH} "Archivos principales"
LangString TITLE_Manual ${LANG_ENGLISH} "Manual"
LangString TITLE_Manual ${LANG_SPANISH} "Manual"
LangString TITLE_CustomFormats ${LANG_ENGLISH} "Custom formats"
LangString TITLE_CustomFormats ${LANG_SPANISH} "Formatos personalizados"
LangString TITLE_LangFiles ${LANG_ENGLISH} "Language files"
LangString TITLE_LangFiles ${LANG_SPANISH} "Archivos de lenguaje"
LangString TITLE_ShortCuts ${LANG_ENGLISH} "Shortcuts"
LangString TITLE_ShortCuts ${LANG_SPANISH} "Accesos directos"
LangString TITLE_StartMenuShortCuts ${LANG_ENGLISH} "Start menu"
LangString TITLE_StartMenuShortCuts ${LANG_SPANISH} "Menú inicio"
LangString TITLE_DesktopShortCuts ${LANG_ENGLISH} "Desktop"
LangString TITLE_DesktopShortCuts ${LANG_SPANISH} "Escritorio"
LangString TITLE_QuickLaunchShortCuts ${LANG_ENGLISH} "Quick launch"
LangString TITLE_QuickLaunchShortCuts ${LANG_SPANISH} "Barra de inicio rápido"

; Language strings for component-selection page - Descriptions
LangString DESC_MainFiles ${LANG_ENGLISH} "Copy the ${PRODUCT}'s main files into the desired folder."
LangString DESC_MainFiles ${LANG_SPANISH} "Copia los archivos principales de ${PRODUCT} a la carpeta deseada."
LangString DESC_Manual ${LANG_ENGLISH} "Installs the manual in all available translations."
LangString DESC_Manual ${LANG_SPANISH} "Instala el manual en todas las traducciones disponibles."
LangString DESC_CustomFormats ${LANG_ENGLISH} "Copies the custom format examples."
LangString DESC_CustomFormats ${LANG_SPANISH} "Copia los ejemplos de formatos personalizados."
LangString DESC_LangFiles ${LANG_ENGLISH} "Installs the additional language files pack."
LangString DESC_LangFiles ${LANG_SPANISH} "Instala el pack adicional de archivos de lenguaje."
LangString DESC_ShortCuts ${LANG_ENGLISH} "Creates additional shortcuts."
LangString DESC_ShortCuts ${LANG_SPANISH} "Crea accesos directos adicionales."
LangString DESC_StartMenuShortCuts ${LANG_ENGLISH} "Creates shortcuts in the start menu."
LangString DESC_StartMenuShortCuts ${LANG_SPANISH} "Crea accesos directos en el menú inicio."
LangString DESC_DesktopShortCuts ${LANG_ENGLISH} "Creates shortcuts in the desktop."
LangString DESC_DesktopShortCuts ${LANG_SPANISH} "Crea accesos directos en el escritorio."
LangString DESC_QuickLaunchShortCuts ${LANG_ENGLISH} "Creates shortcuts in the quick launch."
LangString DESC_QuickLaunchShortCuts ${LANG_SPANISH} "Crea accesos directos en la barra de inicio rápido."

; Translations of the manual
LangString TITLE_ManualEnglish ${LANG_ENGLISH} "English"
LangString TITLE_ManualSpanish ${LANG_ENGLISH} "Spanish"
LangString TITLE_ManualBulgarian ${LANG_ENGLISH} "Bulgarian"
;LangString TITLE_ManualFrench ${LANG_ENGLISH} "French"
LangString TITLE_ManualRussian ${LANG_ENGLISH} "Russian"

LangString TITLE_ManualEnglish ${LANG_SPANISH} "Inglés"
LangString TITLE_ManualSpanish ${LANG_SPANISH} "Español"
LangString TITLE_ManualBulgarian ${LANG_SPANISH} "Búlgaro"
;LangString TITLE_ManualFrench ${LANG_SPANISH} "Francés"
LangString TITLE_ManualRussian ${LANG_SPANISH} "Ruso"

; Folder-selection page
InstallDir "$PROGRAMFILES\URUSoft\${PRODUCT}"

; -------------------------------- ;
;         Files to install         ;
; -------------------------------- ;

Section $(TITLE_MainFiles) MainFiles
  SectionIn RO

  SetOutPath $INSTDIR
    File "${FILESPATH}\${PRODUCT_EXENAME}"
    File "${FILESPATH}\shortcuts.key"
    File "${FILESPATH}\FPS.ini"
  SetOutPath $INSTDIR\SubtitleAPI
    File "${FILESPATH}\SubtitleAPI\SubtitleAPI.dll"
  SetOutPath $INSTDIR\OCRScripts
    File "${FILESPATH}\OCRScripts\*.ocr"
  SetOutPath $INSTDIR\PascalScripts
    File "${FILESPATH}\PascalScripts\*.pas"
  CreateDirectory $INSTDIR\CustomFormats

  WriteRegStr HKLM "SOFTWARE\URUSoft\${PRODUCT}" "Install_Dir" "$INSTDIR"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_SHORTNAME}" "DisplayName" "${PRODUCT} ${VERSION}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_SHORTNAME}" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteUninstaller "uninstall.exe"

SectionEnd

;--------------------------------------------------------------------

SubSection $(TITLE_Manual) Manual

  Section $(TITLE_ManualEnglish) ManualEnglish
    SetOutPath $INSTDIR\Help
    File "${FILESPATH}\Help\Manual.html"
  SectionEnd

  Section $(TITLE_ManualSpanish) ManualSpanish
    SetOutPath $INSTDIR\Help
    File "${FILESPATH}\Help\ManualSPA.html"
  SectionEnd

  Section $(TITLE_ManualBulgarian) ManualBulgarian
    SetOutPath $INSTDIR\Help
    File "${FILESPATH}\Help\ManualBG.html"
  SectionEnd

/*  Section $(TITLE_ManualFrench) ManualFrench
    SetOutPath $INSTDIR\Help
    File "${FILESPATH}\Help\ManualFR.html"
  SectionEnd
*/

  Section $(TITLE_ManualRussian) ManualRussian
    SetOutPath $INSTDIR\Help
    File "${FILESPATH}\Help\ManualRUS.html"
  SectionEnd

SubSectionEnd

;--------------------------------------------------------------------

Section $(TITLE_CustomFormats) CustomFormats
  SetOutPath "$INSTDIR\Custom Formats Examples"
    File "${FILESPATH}\Custom Formats Examples\*.*"
SectionEnd

;--------------------------------------------------------------------

Section $(TITLE_LangFiles) LangFiles
  SetOutPath $INSTDIR\Langs
    File "${FILESPATH}\Langs\*.lng"
    File "${FILESPATH}\Langs\Charsets.txt"
SectionEnd

;--------------------------------------------------------------------

SubSection $(TITLE_ShortCuts) ShortCuts

  Section $(TITLE_StartMenuShortCuts) StartMenuShortCuts
    CreateDirectory "$SMPROGRAMS\URUSoft\${PRODUCT}"

    CreateDirectory "$SMPROGRAMS\URUSoft\${PRODUCT}\Help"

    ${If} ${SectionIsSelected} ${ManualEnglish}
        CreateShortCut "$SMPROGRAMS\URUSoft\${PRODUCT}\Help\Manual (English).lnk" "$INSTDIR\Manual\Manual.html" "" "$INSTDIR\Manual\Manual.html" 0
    ${EndIf}
    ${If} ${SectionIsSelected} ${ManualSpanish}
      CreateShortCut "$SMPROGRAMS\URUSoft\${PRODUCT}\Help\Manual (Español).lnk" "$INSTDIR\Manual\ManualSPA.html" "" "$INSTDIR\Manual\ManualSPA.html" 0
    ${EndIf}
    ${If} ${SectionIsSelected} ${ManualBulgarian}
      CreateShortCut "$SMPROGRAMS\URUSoft\${PRODUCT}\Help\Manual (Bulgarian).lnk" "$INSTDIR\Manual\ManualBG.html" "" "$INSTDIR\Manual\ManualBG.html" 0
    ${EndIf}
    ;${If} ${SectionIsSelected} ${ManualFrench}
      ;CreateShortCut "$SMPROGRAMS\URUSoft\${PRODUCT}\Help\Manual (French).lnk" "$INSTDIR\Manual\ManualFR.html" "" "$INSTDIR\Manual\ManualFR.html" 0
    ;${EndIf}
    ${If} ${SectionIsSelected} ${ManualRussian}
      CreateShortCut "$SMPROGRAMS\URUSoft\${PRODUCT}\Help\Manual (Russian).lnk" "$INSTDIR\Manual\ManualRUS.html" "" "$INSTDIR\Manual\ManualRUS.html" 0
    ${EndIf}

    CreateShortCut "$SMPROGRAMS\URUSoft\${PRODUCT}\${PRODUCT}.lnk" "$INSTDIR\${PRODUCT_EXENAME}" "" "$INSTDIR\${PRODUCT_EXENAME}" 0
    CreateShortCut "$SMPROGRAMS\URUSoft\${PRODUCT}\Uninstall ${PRODUCT}.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  SectionEnd

  Section $(TITLE_DesktopShortCuts) DesktopShortCuts
    CreateShortCut "$DESKTOP\${PRODUCT}.lnk" "$INSTDIR\${PRODUCT_EXENAME}" "" "$INSTDIR\${PRODUCT_EXENAME}" 0
  SectionEnd

  Section $(TITLE_QuickLaunchShortCuts) QuickLaunchShortCuts
    CreateShortCut "$QUICKLAUNCH\${PRODUCT}.lnk" "$INSTDIR\${PRODUCT_EXENAME}" "" "$INSTDIR\${PRODUCT_EXENAME}" 0
  SectionEnd

SubSectionEnd

;--------------------------------------------------------------------

; When installer is launched...

Function .onInit
  !insertmacro MUI_LANGDLL_DISPLAY
FunctionEnd

; Descriptions
!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${MainFiles} $(DESC_MainFiles)
  !insertmacro MUI_DESCRIPTION_TEXT ${Manual} $(DESC_Manual)
  !insertmacro MUI_DESCRIPTION_TEXT ${CustomFormats} $(DESC_CustomFormats)
  !insertmacro MUI_DESCRIPTION_TEXT ${LangFiles} $(DESC_LangFiles)
  !insertmacro MUI_DESCRIPTION_TEXT ${ShortCuts} $(DESC_ShortCuts)
  !insertmacro MUI_DESCRIPTION_TEXT ${StartMenuShortCuts} $(DESC_StartMenuShortCuts)
  !insertmacro MUI_DESCRIPTION_TEXT ${DesktopShortCuts} $(DESC_DesktopShortCuts)
  !insertmacro MUI_DESCRIPTION_TEXT ${QuickLaunchShortCuts} $(DESC_QuickLaunchShortCuts)
!insertmacro MUI_FUNCTION_DESCRIPTION_END

; -------------------------------- ;
;        Uninstaller section       ;
; -------------------------------- ;

Section "Uninstall"

  ; remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_SHORTNAME}"
  DeleteRegKey HKLM "Software\URUSoft\${PRODUCT}"

  Delete "$INSTDIR\*.*"
  Delete "$INSTDIR\OCRScripts\*.*"
  RMDir  "$INSTDIR\OCRScripts"
  Delete "$INSTDIR\PascalScripts\*.*"
  RMDir  "$INSTDIR\PascalScripts"
  Delete "$INSTDIR\SubtitleAPI\*.*"
  RMDir  "$INSTDIR\SubtitleAPI"
  Delete "$INSTDIR\Langs\*.*"
  Delete "$INSTDIR\Custom Formats Examples\*.*"
  RMDir  "$INSTDIR\Custom Formats Examples\"
  Delete "$INSTDIR\CustomFormats\*.*"
  RMDir  "$INSTDIR\CustomFormats\"
  RMDir  "$INSTDIR\Langs\"
  Delete "$INSTDIR\Help\*.*"
  RMDir  "$INSTDIR\Help\"
  Delete "$SMPROGRAMS\URUSoft\${PRODUCT}\*.*"
  Delete "$SMPROGRAMS\URUSoft\${PRODUCT}\Help\*.*"
  RMDir  "$SMPROGRAMS\URUSoft\${PRODUCT}\Help"
  RMDir  "$SMPROGRAMS\URUSoft\${PRODUCT}"
  Delete "$DESKTOP\${PRODUCT}.lnk"
  Delete "$QUICKLAUNCH\${PRODUCT}.lnk"
  RMDir  "$INSTDIR"
  RMDir  "$SMPROGRAMS\URUSoft"

SectionEnd

; When uninstaller is launched...
Function un.onInit
  ; Get language from registry
  ReadRegStr $LANGUAGE HKCU "Software\URUSoft\${PRODUCT}" "Installer Language"
FunctionEnd
