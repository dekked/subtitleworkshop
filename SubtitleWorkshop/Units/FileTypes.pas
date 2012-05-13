unit FileTypes;

interface

uses Forms, Windows, Classes, SysUtils, USubtitlesFunctions, General,
     Registry, ShlObj;

// -----------------------------------------------------------------------------

procedure SeparateExtensions(var List: TStrings; ExtStr: String; TxtAndScr: Boolean = False);
function GetExtStr(List: TStrings): String;
function GetExtensionsList: String;
procedure AssociateExtensions(Extensions: String; Associate: Boolean = True);

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

procedure SeparateExtensions(var List: TStrings; ExtStr: String; TxtAndScr: Boolean = False);
var
  i      : Integer;
  NewExt : String;
begin
  if not Assigned(List) then List := TStringList.Create;
  List.Clear;
  if ExtStr = '' then exit;
  for i := 0 to StringCount(';', ExtStr) do
  begin
    if Pos(';', ExtStr) > 0 then
      NewExt := Copy(ExtStr, 0, Pos(';', ExtStr)-1) else
      NewExt := ExtStr;
    if TxtAndScr = False then
    begin
      if (NewExt <> '*.scr') and (NewExt <> '*.txt') and (NewExt <> '*.rtf') then
        List.Add(NewExt);
    end else
      List.Add(NewExt);
    ExtStr := Copy(ExtStr, Pos(';', ExtStr)+1, Length(ExtStr));
  end;
end;

// -----------------------------------------------------------------------------

function GetExtStr(List: TStrings): String;
var
  i: Integer;
begin
  Result := '';
  if Assigned(List) then
  begin
    for i := 0 to List.Count-1 do
      Result := Result + List[i] + ';';
    Delete(Result, Length(Result), 1);
  end;
end;

// -----------------------------------------------------------------------------

function GetExtensionsList: String;
var
  i,a  : Integer;
  Name : String;
  Ext  : String;
  Exts : String;
begin
  Exts := '';
  for i := 1 to SubtitleAPI.FormatsCount do
  begin
    SubtitleAPI.GetFormatInfo(i, Name, Ext);
    if Pos(';', Ext) = 0 then
    begin
      if Pos(Ext, Exts) = 0 then
        Exts := Exts + Ext + ';';
    end else
    begin
      for a := 0 to StringCount(';', Ext) do
      begin
        if Pos(';', Ext) > 0 then
        begin
          if (Pos(Copy(Ext, 0, Pos(';', Ext) - 1), Exts) = 0) then
            Exts := Exts + Copy(Ext, 0, Pos(';', Ext) - 1) + ';';
        end else
        begin
          if (Pos(Ext, Exts) = 0) then
            Exts := Exts + Ext + ';';
        end;
        Ext := Copy(Ext, Pos(';', Ext) + 1, Length(Ext));
      end;
    end;
  end;
  if Exts[Length(Exts)] = ';' then
    Delete(Exts, Length(Exts), 1);
  Result := Exts;
end;

// -----------------------------------------------------------------------------

procedure AssociateExtensions(Extensions: String; Associate: Boolean = True);
var
  Reg         : TRegistry;
  i           : Integer;
  Exts        : TStrings;
  FileType    : String;
  Exe         : String;
  Description : String;
begin
  if Extensions = '' then exit;
  if Associate = True then
  begin
    FileType    := 'SubtitleWorkshop';
    Description := 'Subtitle file';
    Exe         := Application.ExeName;
  end else
  begin
    FileType    := '';
    Description := '';
    Exe         := '';
  end;

  Reg := TRegistry.Create;
  try
    SeparateExtensions(Exts, Extensions);

    for i := 0 to Exts.Count-1 do
    begin
      Exts[i] := Copy(Exts[i], 2, Length(Exts[i]));
      with Reg do
      begin
        RootKey := HKEY_CLASSES_ROOT;
        OpenKey(Exts[i], True);

        WriteString('', FileType);
        CloseKey;

        OpenKey(FileType, True);
        WriteString('', Description);
        CloseKey;

        OpenKey(FileType + '\DefaultIcon', True);
        if Associate = True then
          WriteString('', Exe + ',0') else // 0 = Icon index
          WriteString('', '');
        CloseKey;

        OpenKey(FileType + '\Shell\Open', True);
        if Associate = True then
          WriteString('', '&Open with ' + ID_PROGRAM) else
          WriteString('', '');
        CloseKey;

        OpenKey(FileType + '\Shell\Open\Command', True);
        if Associate = True then
          WriteString('', '"' + Exe + '" /OPEN("%1")') else
          WriteString('', '""');
        CloseKey;
      end;
    end;
  finally
    Reg.Free;
    Exts.Free;
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED,SHCNF_IDLIST,nil, nil);
end;

// -----------------------------------------------------------------------------

end.
