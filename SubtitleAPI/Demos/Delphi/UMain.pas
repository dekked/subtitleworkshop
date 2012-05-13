//----------------------------------------------------------------------------//
//                                                                            //
//             URUSoft Subtitle API - Delphi Example Application              //
//                                                                            //
//                       Copyright ® 2002-2003 URUSoft.                       //
//                                                                            //
//    This example is not intended to correctly load and save all subtitle    //
//    files but only to test the power and speed of URUSoft Subtitle API.     //
//                                                                            //
//----------------------------------------------------------------------------//

unit UMain;

interface

uses
  Windows, SysUtils, Forms, Classes, Controls, StdCtrls, ComCtrls, ExtCtrls,
  Dialogs, USubtitleAPI;

type
  TfrmMain = class(TForm)
    cmdOpen: TButton;
    cmdClose: TButton;
    cmdShowAt: TButton;
    txtTime: TEdit;
    pnlCurrentSubtitle: TPanel;
    lblFormat: TLabel;
    lblCount: TLabel;
    dlgOpen: TOpenDialog;
    lblModuleVersion: TLabel;
    lblFormats: TLabel;
    cmdShowFormats: TButton;
    lsvSubtitles: TListView;
    txtShowAt: TEdit;
    cboFPS: TComboBox;
    lblFPS: TLabel;
    lblCurrentSubtitle: TLabel;
    dlgSave: TSaveDialog;
    btnSaveAs: TButton;
    procedure cmdCloseClick(Sender: TObject);
    procedure cmdShowAtClick(Sender: TObject);
    procedure lsvSubtitlesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure cmdShowFormatsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmdOpenClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  // Subtitle API Variable
  SubtitleAPI: TSubtitleAPI;

// -----------------------------------------------------------------------------

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TfrmMain.FormCreate(Sender: TObject);
var
  APIVersion: String;
begin
  // We initialize SubtitleAPI, we have to give it the path to the DLL and it's
  // file name first...
  SubtitleAPI := TSubtitleAPI.Create(ExtractFilePath(Application.ExeName) + 'SubtitleAPI.dll');

  // Check if SubtitleAPI is initialized, if it is not then the path or filename
  // was not valid, the file didn't exist or it was an invalid DLL, so we display
  // an error message and exit the application since we have no reading & writing
  // engine
  if SubtitleAPI.Initialized = False then
  begin
    Application.MessageBox('Could not initialize SubtitleAPI dll.', 'Error', MB_ICONERROR);
    SubtitleAPI.Free;
    Application.Terminate;
  end;

  // Set the filter of the open dialogue to all the supported formats, by using
  // a function which is in USubtitleAPI.pas
  dlgOpen.Filter := SubtitleAPI.FillDialogFilter;
  dlgSave.Filter := SubtitleAPI.FillDialogFilter(''); // This time we don't have 'all supported files' text

  // Get the module version and add the corresponding dot to display it...
  APIVersion := IntToHex(SubtitleAPI.ModuleVersion, 3);
  Insert('.', APIVersion, Length(APIVersion)-1);
  lblModuleVersion.Caption := 'Module version: ' + APIVersion;

  // We display the number of supported formats
  lblFormats.Caption := 'Supported formats: ' + IntToStr(SubtitleAPI.FormatsCount);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  SubtitleAPI.Free; // Free SubtitleAPI variable and DLL instance
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmdOpenClick(Sender: TObject);
var
  i            : Integer;
  SubtitleItem : TListItem;
begin
  if (dlgOpen.Execute) and (dlgOpen.FileName <> '') then
  begin
    // First of all, clear the list...
    lsvSubtitles.Clear;

    // By default we are not going to use the extension of the subtitle file to check
    // it's format.
    // If you specify a FormatNumber parameter the DLL will interpret that the subtitle
    // file you are about to load is in the specified format, so no format checking
    // will be performed. Useful if the automatic format recognition doesn't work for
    // certain file.
    DecimalSeparator := ','; // To work with StrToFloat()...
    if SubtitleAPI.LoadSubtitle(dlgOpen.FileName, StrToFloat(cboFPS.Items[cboFPS.ItemIndex])) then
    begin
      // Reverse the text of all subtitles, keeping lines order...
      SubtitleAPI.ReverseSubtitleText;

      // We have now the file in memory. Now we are going to play with it a bit...

      // Insert subtitles...
      SubtitleAPI.InsertSubtitle(0, 100, 200, 'TEST: INSERT FUNCTION');
      SubtitleAPI.InsertSubtitle(1, 300, 400, 'WE HAVE REVERSED EACH SUBTITLE''S TEXT');
      SubtitleAPI.InsertSubtitle(2, 500, 600, 'AND WE HAVE DELETED FOURTH SUBTITLE');

      SubtitleAPI.DeleteSubtitle(3); // Delete fourth subtitle (zero-based)

      lblCount.Caption := 'Lines count: ' + IntToStr(SubtitleAPI.SubtitleCount);
      lblFormat.Caption := 'Format: ' + SubtitleAPI.CurrentFormatName;

      // Add subtitles to the list...
      for i := 0 to SubtitleAPI.SubtitleCount-1 do
      begin
        SubtitleItem := lsvSubtitles.Items.Add;
        SubtitleItem.Caption := IntToStr(SubtitleAPI.GetInitialTime(i));    // Get start time (milliseconds)
        SubtitleItem.SubItems.Add(IntToStr(SubtitleAPI.GetFinalTime(i))); // Get final time (milliseconds)
        SubtitleItem.SubItems.Add(SubtitleAPI.GetText(i)); // Get text
      end;
    end
    else
      ShowMessage('Unable to load: ' + dlgOpen.FileName);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.btnSaveAsClick(Sender: TObject);
var
  DecimalSep: Char;
begin
  if dlgSave.Execute then
  begin
    DecimalSep       := DecimalSeparator;
    DecimalSeparator := ','; // To work with StrToFloat()...
    SubtitleAPI.SaveSubtitle(dlgSave.FileName, dlgSave.FilterIndex, StrToFloat(cboFPS.Items[cboFPS.ItemIndex]));
    DecimalSeparator := DecimalSep;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmdCloseClick(Sender: TObject);
begin
  // Close the current subtitle file (free it from memory)
  SubtitleAPI.CloseSubtitle;
  lsvSubtitles.Clear;
  lblFormat.Caption := 'Format:';
  lblCount.Caption := 'Lines Count:';
  lblCurrentSubtitle.Caption := '';
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmdShowFormatsClick(Sender: TObject);
var
  i, x : Integer;
  s    : String;
begin
  i := SubtitleAPI.FormatsCount; // Total supported formats
  if i = 0 then Exit; // If we have no supported formats, exit procedure

  s := '';
  for x := 1 To i Do
  begin
    if s = '' then
      s := Format('%s', [SubtitleAPI.GetFormatName(x)]) else // Get the format name of the current format index (x variable)
      s := Format('%s, %s', [s, SubtitleAPI.GetFormatName(x)]);
  end;

  Application.MessageBox(PChar(s), 'Supported formats', MB_ICONINFORMATION);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cmdShowAtClick(Sender: TObject);
begin
  // The GetSubtitleText function gets the text of the subtitle that is
  // going to be displayed in certain time, this is very useful for movie
  // players.
  txtShowAt.Text := SubtitleAPI.GetSubtitleText(StrToInt(txtTime.Text));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.lsvSubtitlesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  lblCurrentSubtitle.Caption := Item.SubItems.Strings[1];
end;

// -----------------------------------------------------------------------------

end.
