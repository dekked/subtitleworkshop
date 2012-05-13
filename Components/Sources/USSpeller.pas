unit USSpeller;

interface

// -----------------------------------------------------------------------------

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Variants,
  ComObj, ActiveX, Registry;

// -----------------------------------------------------------------------------

type
  TUSSpell = class(TComponent)
  private
    FChangedText  : String;
    FConnected    : Boolean;
    FHandle       : HWND;
    FHookCaption  : Boolean;
    FNumChanges   : Integer;
    FOleOn        : Boolean;
    FSpellCaption : String;
    FWordVersion  : String;
    hMapObject    : Cardinal;
    pMem          : Pointer;
    // OLE Variants
    FWordApp  : OLEVariant;
    FRange    : OLEVariant;
    FADoc     : OLEVariant;
    FCustDics : OLEVariant;
    function GetCheckGWS: Boolean;
    function GetGrammarErrors: Integer;
    function GetSpellCaption: String;
    function GetSpellChecked: Boolean;
    function GetSpellErrors: Integer;
    function GetWordVersion: String;
    procedure SetCheckGWS(const Value: Boolean);
    procedure SetHookCaption(Value: Boolean);
    procedure SetSpellCaption(const Value: String);
  protected
    function Internal_checkGrammar: Boolean;
    function Internal_checkSpelling: Boolean;
    procedure SetHook;
    procedure UnSetHook;
  public
    function AddCustomDic(const FileName: String): Integer;
    function CheckClipboardGrammar: Boolean;
    function CheckClipboardSpell: Boolean;
    function CheckGrammar(const Text: String): Boolean;
    function CheckSpelling(const Text: String): Boolean;
    procedure ClearText;
    procedure Connect;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Disconnect;
    procedure RemoveCustomDic(const Index: Integer); overload;
    procedure RemoveCustomDic(const Name: String); overload;
    procedure ResetIgnoreAll;
    procedure SpellingOptions;
    property ChangedText: String read FChangedText;
    property CheckGrammarWithSpelling: Boolean read GetCheckGWS write SetCheckGWS;
    property Connected: Boolean read FConnected;
    property GrammarErrorCount: Integer read GetGrammarErrors;
    property NumChanges: Integer read FNumChanges;
    property SpellChecked: Boolean read GetSpellChecked;
    property SpellErrorCount: Integer read GetSpellErrors;
    property WordVersion: String read GetWordVersion;
  published
    property HookCaption: Boolean read FHookCaption write SetHookCaption;
    property SpellCaption: String read GetSpellCaption write SetSpellCaption;
  end;

// -----------------------------------------------------------------------------

function IsWordInstalled: Boolean;
procedure Register;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

// This constants used to implement the shared memory space
// using a FileMapping to hold two Strings (up to 255 char each)
const
  USShareSize    = 512;
  USCaptionStart = 0;
  USClassStart   = 256;
  //memspace looks like this: |..Caption...|...WindowsClassID..|

  //Constants for MS Word
  MSDialogWndClass2000 = 'bosa_sdm_Microsoft Word 9.0';
  MSDialogWndClass97   = 'bosa_sdm_Microsoft Word 8.0';
  MSWordWndClass       = 'OpusApp';

// -----------------------------------------------------------------------------

type
    pTCWPRetStruct = ^TCWPRetStruct;

// -----------------------------------------------------------------------------

var
  nHook: HHOOK;

// -----------------------------------------------------------------------------

constructor TUSSpell.Create(AOwner: TComponent);
var
  Init: Integer;
begin
  inherited;
  FConnected   := False;
  FChangedText := '';
  init         := CoInitialize(nil);
  FHookCaption := False;
  if (init = S_OK) or (init = S_FALSE) then
    FOleOn := True else
    raise EOleSysError.CreateFmt('Error initialising COM library', []);
end;

// -----------------------------------------------------------------------------

function IsWordInstalled: Boolean;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Result      := Reg.KeyExists('Word.Application');
  finally
    Reg.Free;
  end;
end;

// -----------------------------------------------------------------------------

function MessWatch(nCode, wParam, lParam: Integer): Integer; stdcall;
var
  p          : pTCWPRetStruct;
  h          : HWND;
  pMem       : Pointer;
  q          : PChar;
  hMapObject : Cardinal;
begin
  Result := 0;
  if nCode < 0 then
    Result := CallNextHookEx(nHook, nCode, wParam, lParam) else
  begin
    p := pTCWPRetStruct(lParam);
    if (p.message = WM_NCPAINT) then //wait for NC area to be drawn
    begin 
      //open shared memory
      hMapObject := OpenFileMapping(FILE_MAP_READ, False, 'US_spell_share');
      pMem := MapViewOfFile(hMapObject, FILE_MAP_READ, 0, 0, 0);
      if (pMem <> nil) then
      begin
        q := pMem;
        h := FindWindow(q + USClassStart, nil);
        if (h <> 0) then
          SetWindowText(h, q + USCaptionStart);
      end;
      CloseHandle(hMapObject); //close shared memory
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ TUSSpeller }

// -----------------------------------------------------------------------------

function TUSSpell.GetCheckGWS: Boolean;
begin
  Result := False;
  if FConnected then
    Result := FWordApp.Options.CheckGrammarWithSpelling;
end;

// -----------------------------------------------------------------------------

function TUSSpell.GetGrammarErrors: Integer;
begin
  if FConnected then
    Result := FRange.GrammaticalErrors.Count else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TUSSpell.GetSpellCaption: String;
begin
  Result := FSpellCaption;
end;

// -----------------------------------------------------------------------------

function TUSSpell.GetSpellChecked: Boolean;
// returns false if spelling has yet to be checked
begin
  Result := True;
  if FConnected then
    Result := not FRange.SpellingChecked;
end;

// -----------------------------------------------------------------------------

function TUSSpell.GetSpellErrors: Integer;
begin
  if FConnected then
    Result := FRange.SpellingErrors.Count else
    Result := 0;
end;

// -----------------------------------------------------------------------------

function TUSSpell.GetWordVersion: String;
begin
  Result := FWordVersion;
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.SetCheckGWS(const Value: Boolean);
begin
  if FConnected then
    FWordApp.Options.CheckGrammarWithSpelling := Value;
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.SetHookCaption(Value: Boolean);
begin
  FHookCaption := Value;
  if (csDesigning in ComponentState) = False then
  begin
    if Value then SetHook else UnSetHook;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.SetSpellCaption(const Value: String);
var
  p: PChar;
begin
  FSpellCaption := Value;
  if FConnected then
  begin
    // copy to shared memory
    p := pMem;
    if (p <> nil) then
      StrCopy(p + USCaptionStart, PChar(FSpellCaption));
  end;
end;

// -----------------------------------------------------------------------------

function TUSSpell.Internal_checkGrammar: Boolean;
begin
  // ensures dialogs appear in front
  SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE + SWP_HIDEWINDOW); 
  FADoc.TrackRevisions := True; // note if changes are made
  FNumChanges := 0;
  OleCheck(FRange.CheckGrammar);
  FWordApp.Visible := False; // need to stop ActiveDocument appearing
  FNumChanges := FRange.Revisions.Count div 2; // seems revisions counts the old word and the new one separately
  Result := (FRange.Revisions.Count > 0);
  if Result then FRange.Revisions.AcceptAll; // accept all changes
  FADoc.TrackRevisions := False; // don't track future changes
end;

// -----------------------------------------------------------------------------

function TUSSpell.Internal_checkSpelling: Boolean;
begin
  // ensures dialogs appear in front 
  SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE + SWP_HIDEWINDOW);
  FADoc.TrackRevisions := True; // note if changes are made
  FNumChanges := 0;
  
  if CheckGrammarWithSpelling then
    OleCheck(FADoc.CheckGrammar) else // Check spelling and grammar
    OleCheck(FADoc.CheckSpelling); // We check spelling, not grammar

  FWordApp.Visible := False; // need to stop ActiveDocument appearing
  FNumChanges := FRange.Revisions.Count div 2; // seems revisions counts the old word and the new one separately
  Result := (FRange.Revisions.Count > 0);
  if Result then FRange.Revisions.AcceptAll; // accept all changes
  FADoc.TrackRevisions := False; // don't track future changes
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.SetHook;
begin
  if (nHook = 0) then nHook := SetWindowsHookEx(WH_CALLWNDPROCRET, MessWatch, HInstance, 0);
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.UnSetHook;
begin
  if (nHook <> 0) then UnHookWindowsHookEx(nHook);
  nHook := 0;
end;

// -----------------------------------------------------------------------------

function TUSSpell.AddCustomDic(const FileName: String): Integer;
begin
  FCustDics.Add(FileName);
  Result := FCustDics.Count;
end;

// -----------------------------------------------------------------------------

function TUSSpell.CheckClipboardGrammar: Boolean;
// returns true if changes were made. Corrected text is on
// the clipboard
begin
  Result := False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!

  if FConnected then
  begin
    FRange.Paste; // replace with new text to check
    Result := Internal_CheckGrammar;
    if Result then FRange.Copy;
  end;
end;

// -----------------------------------------------------------------------------

function TUSSpell.CheckClipboardSpell: Boolean;
// returns true if changes were made. Corrected text is on
// the clipboard
begin
  Result := False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!
  if FConnected then
  begin
    FRange.Paste; // replace with new text to check
    Result := Internal_checkSpelling;
    if Result then FRange.Copy; // put onto clipboard
  end;
end;

// -----------------------------------------------------------------------------

function TUSSpell.CheckGrammar(const Text: String): Boolean;
// returns true if changes were made and the corrected text is
// placed in the Text String
begin
  Result := False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!
  if FConnected then
  begin
    FChangedText := '';
    FRange.Text  := Text; // replace with new text to check
    Result       := Internal_CheckGrammar;
    if Result then FChangedText := FRange.Text;
  end;
end;

// -----------------------------------------------------------------------------

function TUSSpell.CheckSpelling(const Text: String): Boolean;
// returns true if changes were made and the corrected text is
// placed in the Text String
begin
  Result := False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!
  if FConnected then
  begin
    FChangedText := '';
    FRange.Text  := Text; // replace with new text to check
    Result       := Internal_CheckSpelling;
    if Result then FChangedText := FRange.Text;
  end else
    Result := False;
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.ClearText;
begin
  if FConnected then FRange.Text := '';
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.Connect;
var
  s: String;
  p: PChar;
begin
  if FConnected then Exit; // don't create two instances
  try
    // Word application
    FWordApp                := CreateOleObject('Word.Application');
    FConnected              := True;
    FWordApp.Visible        := False; // hides the application
    FWordApp.ScreenUpdating := False; // speed up winword's processing
    //FWordApp.WindowState    := $00000002; // minimise

    FADoc  := FWordApp.Documents.Add(EmptyParam, False); // this will hold the text to be checked
    FRange := FADoc.Range;
    FRange.WholeStory; // makes FRange point to all text in document
    FCustDics    := FWordApp.CustomDictionaries;
    FWordVersion := FWordApp.Version;
    s := FADoc.Name + ' - ' + FWordApp.Name;
    FHandle := FindWindow(MSWordWndClass, PChar(s)); // Word main window's handle

    s := MSDialogWndClass2000;
    if FWordVersion[1] = '9' then
      s := MSDialogWndClass2000 else s := MSDialogWndClass97;

    //set up shared memory space
    hMapObject := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, USShareSize, 'US_spell_share');
    pMem := MapViewOfFile(hMapObject, FILE_MAP_WRITE, 0, 0, 0);
    if pMem <> nil then
    begin
      FillChar(pMem^, USShareSize, 0);
      p := pMem;
      StrCopy(p + USClassStart, PChar(s));
      StrCopy(p + USCaptionStart, PChar(FSpellCaption));
    end;
    //memory share set up

  except
    FWordApp := Unassigned;
    FConnected := False;
    //MessageDlg('Unable to initialise MS Word', mtError, [mbYes], 0);
  end;
end;

// -----------------------------------------------------------------------------

destructor TUSSpell.Destroy;
begin
  Disconnect;
  UnSetHook;
  if FOleOn then CoUninitialize;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.Disconnect;
var
  SaveChanges: OleVariant;
begin
  if VarIsEmpty(FWordApp) = False then
  begin
    SaveChanges := False;
    FWordApp.Quit(SaveChanges); // don't save changes
    FRange     := Unassigned;
    FADoc      := Unassigned;
    FWordApp   := Unassigned;
    FCustDics  := Unassigned;
    FConnected := False;
    CloseHandle(hMapObject);
  end;
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.RemoveCustomDic(const Index: Integer);
var
  Dic: OleVariant;
begin
  Dic := FCustDics.Item(Index);
  if VarIsEmpty(Dic) = False then
    Dic.Delete;
  Dic := Unassigned;
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.RemoveCustomDic(const Name: String);
var
  Dic: OleVariant;
begin
  Dic := FCustDics.Item(Name);
  if VarIsEmpty(Dic) = False then
    Dic.Delete;
  Dic := Unassigned;
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.ResetIgnoreAll;
begin
  if FConnected then
  begin
    FRange.Text := ''; // ResetIgnoreAll performs an automatic spell check
    FWordApp.ResetIgnoreAll;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUSSpell.SpellingOptions;
begin
  BringWindowToTop(FHandle); // ensures that dialog opens on top
  FWordApp.Dialogs.Item($000000D3).Show;
  FWordApp.Visible := False;
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('URUSoft Components', [TUSSpell]);
end;

// -----------------------------------------------------------------------------  .

end.

