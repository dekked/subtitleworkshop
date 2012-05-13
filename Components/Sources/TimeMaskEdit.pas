unit TimeMaskEdit;

// -----------------------------------------------------------------------------

interface

// -----------------------------------------------------------------------------

uses
  Windows, Messages, Classes, SysUtils, Graphics, Controls, Mask, ComCtrls, dialogs,
  CommCtrl, USTimeUtils;

// -----------------------------------------------------------------------------

const TimeMask1 = '!90:00:00,000;1; ';
      TimeMask2 = '!90:00:00:00;1; ';
      MaxTime   = 86399999;

type
  TTimeMode = (tmTime, tmFrames, tmHHMMSSFF);
  TOnTimeEventChange = procedure(Sender: TObject; NewTime: Cardinal) of Object;
  TOnTimeEventChangeFromEditOnly = procedure(Sender: TObject; NewTime: Cardinal) of Object;
  TTimeMaskEdit = class(TCustomMaskEdit)
  private
    { Private declarations }
    //FCurrentTimeMask    : String;
    FUpDown             : TUpDown;
    FTime               : Cardinal;
    FTimeMode           : TTimeMode;
    FMinTime            : Cardinal; // Minimum time
    FFPS                : Single; // FPS (only for frames mode)
    FEnabled            : Boolean;
    FChangeTimeOnModify : Boolean;
    FOnTimeChange             : TOnTimeEventChange;
    FOnTimeChangeFromEditOnly : TOnTimeEventChangeFromEditOnly;
    procedure UpdateText;
    procedure SetTime(const NewTime: Cardinal);
    procedure SetTimeMode(const NewTimeMode: TTimeMode);
    procedure SetMinTime(const Value: Cardinal);
    procedure SetFPS(const NewFPS: Single);
    procedure CMEnter(var Message: TCMGotFocus); message CM_GOTFOCUS;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetEditRect;
    function GetMinHeight: Integer;
    procedure UDButton(const Up: Boolean);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
  protected
    { Protected declarations }
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetEnabled(Value: Boolean); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    { Published declarations }
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property ChangeTimeOnModify: Boolean read FChangeTimeOnModify write FChangeTimeOnModify;
    property Enabled read FEnabled write SetEnabled;
    property Font;
    property FPS: Single read FFPS write SetFPS;
    property HideSelection;
    property MaxLength;
    property MinTime: Cardinal read FMinTime write SetMinTime;    
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Time: Cardinal read FTime write SetTime; // Time (in milliseconds)
    property TimeMode: TTimeMode read FTimeMode write SetTimeMode; // Time or frames mode
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnTimeChange: TOnTimeEventChange read FOnTimeChange write FOnTimeChange;
    property OnTimeChangeFromEditOnly: TOnTimeEventChangeFromEditOnly read FOnTimeChangeFromEditOnly write FOnTimeChangeFromEditOnly;
  end;

// -----------------------------------------------------------------------------

procedure Register;

// -----------------------------------------------------------------------------

implementation

// -------------------------------------------------------------------------- //
//                                                                            //
//                                TTimeMaskEdit                               //
//                                                                            //
// -------------------------------------------------------------------------- //

constructor TTimeMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  Width               := 20;
  Height              := 22;
  FTimeMode           := tmTime;
  FEnabled            := True;
  FFPS                := 25;
  FMinTime            := 0;
  Time                := 0;
  FChangeTimeOnModify := False;

  // Create up-down button
  FUpDown              := TUpDown.Create(Self);
  FUpDown.Parent       := Self;
  FUpDown.Visible      := True;
  FUpDown.OnChangingEx := UpDownChangingEx;
  FUpDown.Wrap         := True; // Avoid important bug when FUpDown reaches Max
  FUpDown.Increment    := 1;
end;

// -----------------------------------------------------------------------------

destructor TTimeMaskEdit.Destroy;
begin
  FUpDown := nil;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.UpdateText;
begin
  case FTimeMode of
    tmTime     : Text := TimeToString(FTime);
    tmFrames   : Text := IntToStr(TimeToFrames(FTime, FFPS));
    tmHHMMSSFF : Text := MSToHHMMSSFFTime(FTime, FFPS, ':');
  end;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.SetTime(const NewTime: Cardinal);
begin
  if NewTime > 0 then
    FTime := NewTime else
    FTime := 0;
  UpdateText;
  if Assigned(OnTimeChange) then OnTimeChange(Self, NewTime);
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.SetMinTime(const Value: Cardinal);
begin
  FMinTime := Value;
  if FTime < FMinTime then
    Time := FMinTime;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.SetTimeMode(const NewTimeMode: TTimeMode);
begin
  FTimeMode := NewTimeMode;
  UpdateText;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.SetFPS(const NewFPS: Single);
begin
  if (NewFPS <= 100) and (NewFPS > 0) then
    FFPS := NewFPS;
  UpdateText;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.SetEnabled(Value: Boolean);
begin
  FEnabled        := Value;
  FUpDown.Enabled := Value;
  inherited SetEnabled(Value);
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  inherited SetBounds(ALeft, ATop, W, H);
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FUpDown.Width - 2;
  Loc.Top := 0;  
  Loc.Left := 0;  
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
end;

// -----------------------------------------------------------------------------

function TTimeMaskEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  if Height < MinHeight then
    Height := MinHeight else
  if FUpDown <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FUpDown.SetBounds(Width - FUpDown.Width - 5, 0, FUpDown.Width, Height - 5) else
      FUpDown.SetBounds(Width - FUpDown.Width, 1, FUpDown.Width, Height - 3);
    SetEditRect;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.CMEnter(var Message: TCMGotFocus);
begin
  DoEnter;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.DoEnter;
var
  i: Integer;
begin
  if ReadOnly = False then
  begin
    if FTimeMode = tmTime then
    begin
      i := SelStart;
      Text := '';
      EditMask := TimeMask1;
      Text := TimeToString(FTime);
      if i-1 < 0 then i := 1;
      SelStart := i-1;
    end else
    if FTimeMode = tmHHMMSSFF then
    begin
      i := SelStart;
      Text := '';
      EditMask := TimeMask2;
      Text := MSToHHMMSSFFTime(FTime, FFPS, ':');
      if i-1 < 0 then i := 1;
      SelStart := i-1;
    end;
  end;

  if Assigned(OnEnter) then OnEnter(Self);
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.CMExit(var Message: TCMExit);
begin
  DoExit;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.DoExit;
begin
  if ReadOnly = False then
  begin
    if (FTimeMode = tmTime) or (FTimeMode = tmHHMMSSFF) then
    begin
      EditMask := '';
      UpdateText;
    end;
  end;
  if Assigned(OnExit) then OnExit(Self);
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  t: Integer;
begin
  if not (Key in [Ord('0')..Ord('9'), Ord(VK_RETURN)]) then Key := Ord(#0);
  t := -1;

  if (Key = Word(Ord(VK_RETURN))) or (FChangeTimeOnModify) then
  begin
    case FTimeMode of
      tmTime: t := StringToTime(Text);
      tmFrames:
        begin
          t := StrToIntDef(Trim(Text), -1);
          if t > -1 then
            t := FramesToTime(t, FFPS);
        end;
      tmHHMMSSFF: t := HHMMSSFFTimeToMS(Text, FFPS);
    end;

    if t > -1 then
    begin
      if FChangeTimeOnModify = False then
      begin
        if t < MaxTime then
          Time := t else
          Time := MaxTime;
      end else
      begin
        if t < MaxTime then
          Time := t else
          Time := MaxTime;
      end;
      if Assigned(FOnTimeChangeFromEditOnly) then FOnTimeChangeFromEditOnly(Self, Time);

      if FChangeTimeOnModify = False then
        UpdateText;
    end;

    Key := Ord(#0);
  end;

  inherited KeyUp(Key, Shift);
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  tmp: Integer;
begin
  if Key = VK_UP then UDButton(True) else
  if Key = VK_DOWN then UDButton(False);

  // Prevent giving an error when user inputs an incorrect value
  if (Key = Word(Ord(VK_RETURN))) then
  begin
    case FTimeMode of
      tmTime:
        begin
          tmp := SelStart;
          if StringToTime(Text) = -1 then
            Text := TimeToString(FTime);
          SelStart := tmp;
        end;
      tmHHMMSSFF:
        begin
          tmp := SelStart;
          if HHMMSSFFTimeToMS(Text, FFPS) = -1 then
            Text := MSToHHMMSSFFTime(FTime, FFPS, ':');
          SelStart := tmp;
        end;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.UDButton(const Up: Boolean);
var
  TimeToChange : Integer;
  Edit         : (eHours, eMinutes, eSeconds, eMilliseconds, eFrames);
  s            : Integer;
begin
  if (ReadOnly = False) then
  begin
    s := SelStart;
    //  0 0 : 0 0 : 0 0 , 0  0  0
    //  0 1 2 3 4 5 6 7 8 9  10 11
    TimeToChange := 0;
    if (TimeMode = tmTime) or (TimeMode = tmHHMMSSFF) then
    begin
      if TimeMode = tmHHMMSSFF then
        Edit := eFrames else
        Edit := eMilliseconds;
      if GetFocus = Handle then
      begin
        case SelStart of
          0..1  : Edit := eHours;
          2..4  : Edit := eMinutes;
          5..7  : Edit := eSeconds;
          8..12 :
            begin
              if TimeMode = tmHHMMSSFF then
                Edit := eFrames else
                Edit := eMilliseconds;
            end;
        end;
      end;
      case Edit of
        eHours        : TimeToChange := 1000 * 60 * 60;
        eMinutes      : TimeToChange := 1000 * 60;
        eSeconds      : TimeToChange := 1000;
        eMilliseconds : TimeToChange := 100;
        eFrames       : TimeToChange := FramesToTime(1, FFPS);
      end;
    end else
      TimeToChange := FramesToTime(1, FFPS);

    if Up = False then
      TimeToChange := - TimeToChange;

    // Only if new time is lower than 23:59:59,999
    if (Time + Cardinal(TimeToChange)) < MaxTime then
      Time := Time + Cardinal(TimeToChange);

    if Assigned(FOnTimeChangeFromEditOnly) then FOnTimeChangeFromEditOnly(Self, Time);

    SelStart := s;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTimeMaskEdit.UpDownChangingEx(Sender: TObject; var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if (ReadOnly = False) then
    UDButton(Direction = updUp);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('URUSoft Components', [TTimeMaskEdit]);
end;

// -----------------------------------------------------------------------------

end.
