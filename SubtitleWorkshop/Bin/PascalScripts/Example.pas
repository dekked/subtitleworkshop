// -------------------------------------------------------------------------- //
//                  Subtitle Workshop - Pascal scripts sample                 //
//                        Copyright © 2001-2004 URUSoft                       //
//                           http://www.urusoft.net                           //
//                                                                            //
// This sample will go through all the subtitles in the file and will delete  //
// all the ones which's text length is longer than 20 characters. This is     //
// just a small sample of what you can do with Pascal Scripts in Subtitle     //
// Workshop.                                                                  //
//                                                                            //
// This code uses only few of the functions available. A list of all the      //
// available functions/procedures and their explanation is below. Just note   //
// that in every function that use the "Num" parameter, it refers to the      //
// subtitle number.                                                           //
//                                                                            //
// Declaration : function IsOriginalLoaded: Boolean;                          //
// Use         : returns true if the original file is loaded                  //
//                                                                            //
// Declaration : function IsTranslatedLoaded: Boolean;                        //
// Use         : returns true if the translated file is loaded                //
//                                                                            //
// Declaration : procedure EnableWorkArea;                                    //
// Use         : enables the working area (when no file is loaded)            //
//                                                                            //
// Declaration : function GetSubtitleCount: Integer;                          //
// Use         : retrieves the total number of subtitles that are loaded      //
//                                                                            //
// Declaration : function IsSubtitleSelected(const Num: Integer): Boolean;    //
// Use         : returns true if a subtitle is selected, and false if not     //
//                                                                            //
// Declaration : function GetSubtitleText(const Num: Integer): String;        //
// Use         : returns the text of a subtitle                               //
//                                                                            //
// Declaration : procedure SetSubtitleText(const Num: Integer;                //
//                                         const Text: String);               //
// Use         : sets the text of a specified subtitle to the value you pass  //
//                                                                            //
// Declaration : function GetSubtitleTrans(const Num: Integer): String;       //
// Use         : returns the translation of a subtitle                        //
//                                                                            //
// Declaration : procedure SetSubtitleTrans(const Num: Integer;               //
//                                          const Text: String);              //
// Use         : sets the translation of a subtitle to the value you pass     //
//                                                                            //
// Declaration : function GetSubtitleInitialTime(const Num: Integer): Integer;//
// Use         : returns the initial time of a subtitle, in milliseconds      //
//                                                                            //
// Declaration : procedure SetSubtitleInitialTime(const Num: Integer;         //
//                                                const InitialTime: Integer);//
// Use         : sets the initial time of a subtitle to the value you pass,   //
//               in milliseconds                                              //
//                                                                            //
// Declaration : function GetSubtitleFinalTime(const Num: Integer): Integer;  //
// Use         : returns the final time of a subtitle, in milliseconds        //
//                                                                            //
// Declaration : procedure SetSubtitleFinalTime(const Num: Integer;           //
//                                              const FinalTime: Integer);    //
// Use         : sets the final time of a subtitle to the value you pass,     //
//               in milliseconds                                              //
//                                                                            //
// Declaration : procedure InsertSubtitle(const Pos: Integer;                 //
//                                       const InitialTime,                   //
//                                             FinalTime: Integer;            //
//                                       const Text, Translation: String);    //
// Use         : inserts a subtitle. Position is where you want to insert it. //
//                                                                            //
// Declaration : procedure DeleteSubtitle(const Num: Integer);                //
// Use         : deletes the specified subtitle                               //
//                                                                            //
// Declaration : function MsgBox(const AMsg, BCap1, BCap2, BCap3: String;     //
//                               const IconInd: Integer): Integer;            //
// Use         : displays a message box. AMsg is the text. BCap1 is the       //
//               text of the first button. BCap2 and BCap3 are the text of    //
//               the second and third buttons respectively. If you want them  //
//               to be hidden, just pass '' (blank string) as the parameter.  //
//               IconInd is the icon index used by the MessageBox() function, //
//               the constants are defined in Delphi's Windows.pas            //
//               (eg. MB_ICONERROR, MB_ICONQUESTION, etc).                    //
//                                                                            //
// Date and time functions available:                                         //
//                                                                            //
//   function FormatDateTime(const Format: String;                            //
//                           DateTime: TDateTime): String;                    //
//   function GetYear: Integer;                                               //
//   function MyGetMonth: Integer;                                            //
//   function MyGetDay: Integer;                                              //
//   function MyGetHour: Integer;                                             //
//   function MyGetMinute: Integer;                                           //
//   function MyGetSecond: Integer;                                           //
//   function MyGetDate: String;                                              //
//   function MyGetTime: String;                                              //
//   function MyGetDateTime: String;                                          //
//                                                                            //
// -------------------------------------------------------------------------- //

program DeleteShortSubs;

// -----------------------------------------------------------------------------

const MB_ICONQUESTION = $00000020;

// -----------------------------------------------------------------------------

var
  // We declarate the variables that are going to be our controls
  Form : TForm;
  Pnl  : TPanel;
  Chk  : TCheckBox;
  Btn1 : TButton;
  Btn2 : TButton;

// -----------------------------------------------------------------------------

procedure Btn1Click(Sender: TObject);
var
  a: Integer;
begin
  for a := GetSubtitleCount-1 downto 0 do
  begin
    if Length(GetSubtitleText(a)) > 20 then
    begin
      if Chk.Checked then
      begin
        case MsgBox('Do you want to delete subtitle number ' + IntToStr(a) + '?', '&Yes', '&No', '&Cancel', MB_ICONQUESTION) of
          1: DeleteSubtitle(a);
          2: SetSubtitleText(a, 'YOU DECIDED NOT TO DELETE THIS SUBTITLE! :)');
          3: exit;
        end;
      end else
        DeleteSubtitle(a);
    end;
  end;
end;

// -----------------------------------------------------------------------------
var
  n  : Integer;
begin
  // Enable working area if no file is loaded
  if IsOriginalLoaded = False then EnableWorkArea;

  // "now" is a Delphi function to get current date and time
  // For more information on "FormatDateTime" refer to Delphi's help
  InsertSubtitle(0,1000,2000, 'We insert new subtitle, in position 0 (first node)', 'This is the translation');
  InsertSubtitle(1,3000,4000, 'We insert new subtitle, in position 1 (second node)', 'This is the translation');
  InsertSubtitle(2,5000,6000, FormatDateTime('c', now), 'This is the translation');

  n := GetSubtitleCount;
  InsertSubtitle(n, GetSubtitleFinalTime(n-1)+1000, GetSubtitleFinalTime(n-1)+3000,
                 'We insert new subtitle, in position ' + IntToStr(n) + ' (last node)', 'This is the translation');

  // Create the main form. It's not mandatory to create one - I do it here just
  // to show you what can be done.
  Form := TForm.Create(Application);
  try

    Form.Font.Name   := 'Tahoma';
    Form.Font.Size   := 8;
    Form.Caption     := 'Delete long subtitles';
    Form.BorderStyle := bsDialog;
    Form.BorderIcons := [];
    Form.Position    := poScreenCenter;
    Form.Width       := 206;
    Form.Height      := 107;

    // Create panel, to make the form look nicer
    Pnl        := TPanel.Create(Application);
    Pnl.Left   := 8;
    Pnl.Top    := 8;
    Pnl.Height := 35;
    Pnl.Width  := 185;
    Pnl.Parent := Form;

    // Create the "Show confirmation" checkbox
    Chk            := TCheckBox.Create(Application);
    Chk.Parent     := Pnl;
    Chk.ParentFont := True;
    Chk.Top        := 8;
    Chk.Left       := 8;
    Chk.Height     := 17;
    Chk.Width      := 169;
    Chk.Caption    := 'Confirm each deletion';

    // Create Ok button
    Btn1             := TButton.Create(Application);
    Btn1.Parent      := Form;
    Btn1.Default     := True;
    Btn1.Caption     := '&Ok';
    Btn1.ModalResult := mrOk;
    Btn1.OnClick     := @Btn1Click;
    Btn1.Top         := 50;
    Btn1.Left        := 8;
    Btn1.Width       := 88;
    Btn1.Height      := 25;
    Btn1.Font.Style  := Btn1.Font.Style + [fsBold];

    // Create Cancel button
    Btn2             := TButton.Create(Application);
    Btn2.Parent      := Form;
    Btn2.Caption     := '&Cancel';
    Btn2.ModalResult := mrCancel;
    Btn2.Top         := 50;
    Btn2.Left        := 104;
    Btn2.Width       := 88;
    Btn2.Height      := 25;

    // Show the form, as modal, so that you can't use Subtitle Workshop while
    // the form is loaded.
    Form.ShowModal;

  finally
    // Free everything from memory
    Chk.Free;
    Pnl.Free;
    Btn1.Free;
    Btn2.Free;
    Form.Free;
  end;

end.
