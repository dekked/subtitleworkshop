// -------------------------------------------------------------------------- //
//                 Skinned seek bar for Subtitle Workshop 2.xx                //
//                         Copyright (c) 2003 DeKSoft                         //
//                                                                            //
// -------------------------------------------------------------------------- //

unit SWSeekBar;

interface

uses
  Windows, Messages, SysUtils, Graphics, Classes, Controls, USGraphics;

type
  //TStates = (sNormal, sPressed, sDisabled});
  TSWSeekBar = class(TCustomControl)
  private
    FinalBitmap  : TBitmap;
    FMainSeekBar : TBitmap;
    FNormal      : TBitmap;
    FDisabled    : TBitmap;
    FMax         : Int64;
    FPosition    : Int64;
    FEnabled     : Boolean;
    FMouseIsDown : Boolean;
    procedure SetMax(Max: Int64);
    procedure SetPosition(Position: Int64);
    procedure SetMainSeekBarBitmap(Bitmap: TBitmap);
    procedure SetNormalBtnBitmap  (Bitmap: TBitmap);
    procedure SetDisabledBtnBitmap(Bitmap: TBitmap);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MainBar   : TBitmap read FMainSeekBar write SetMainSeekBarBitmap;
    property Max       : Int64   read FMax         write SetMax;
    property Position  : Int64   read FPosition    write SetPosition;
    property Normal    : TBitmap read FNormal      write SetNormalBtnBitmap;
    property Disabled  : TBitmap read FDisabled    write SetDisabledBtnBitmap;
    property Enabled   : Boolean read FEnabled     write SetEnabled;
    property Align;
    property Anchors;
    property Visible;
    property ShowHint;
    property Hint;
    //property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
  end;

procedure Register;

implementation

// -----------------------------------------------------------------------------

constructor TSWSeekBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FinalBitmap  := TBitmap.Create;
  FMainSeekBar := TBitmap.Create;
  FNormal      := TBitmap.Create;
  FDisabled    := TBitmap.Create;
  FEnabled     := True;
  FMouseIsDown := False;
  FMax         := 1000;
  FPosition    := 0;
  Width  := 100;
  Height := 10;
end;

// -----------------------------------------------------------------------------

destructor TSWSeekBar.Destroy;
begin
  inherited Destroy;
  FinalBitmap.Free;
  FMainSeekBar.Free;
  FNormal.Free;
  FDisabled.Free;
end;

// -----------------------------------------------------------------------------

procedure TSWSeekBar.Paint;
var
  a    : Integer;
  Top  : Integer;
  Left : Integer;
begin
  Top := (Height div 2) - (FMainSeekBar.Height div 2);
  FinalBitmap.Width  := Width;
  FinalBitmap.Height := Height;

  // Draw main bar...
  FinalBitmap.Canvas.Brush.Color := clBtnFace;
  FinalBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));

//  DrawTransparentBitmap(FinalBitmap.Canvas.Handle, FMainSeekBar.Handle, 0, Top, clFuchsia);
  BitBlt(FinalBitmap.Canvas.Handle, 0, Top, FMainSeekBar.Width, FMainSeekBar.Height, FMainSeekBar.Canvas.Handle, 0, 0, srcCopy);

  a := 91;
  while a < Width do
  begin
    //DrawTransparentBitmap(FinalBitmap.Canvas.Handle, FMainSeekBar.Handle, a, Top, clFuchsia);
    BitBlt(FinalBitmap.Canvas.Handle, a, Top, FMainSeekBar.Width, FMainSeekBar.Height, FMainSeekBar.Canvas.Handle, 2, 0, srcCopy);
    Inc(a, 89);
  end;
  //DrawTransparentBitmap(FinalBitmap.Canvas.Handle, FMainSeekBar.Handle, Width-3, Top, clFuchsia);
  BitBlt(FinalBitmap.Canvas.Handle, Width - 3, Top, FMainSeekBar.Width, FMainSeekBar.Height, FMainSeekBar.Canvas.Handle, 89, 0, srcCopy);

  // Calculate
  if FMax <> 0 then
    Left := Trunc((FPosition * 100 / FMax) * (Width-FNormal.Width) / 100) else
    Left := 0;
  Top := (Height div 2) - (FNormal.Height div 2);

  // Draw line until the button
  FinalBitmap.Canvas.Brush.Color := $97A8B2;
  FinalBitmap.Canvas.FillRect(Rect(1, (Height div 2) - (FMainSeekBar.Height div 2) + 1, Left, ((Height div 2) - (FMainSeekBar.Height div 2)) + FMainSeekBar.Height - 1));

  // Draw button
  if FEnabled then
    DrawTransparentBitmap(FinalBitmap.Canvas.Handle, FNormal.Handle, Left, Top, clFuchsia) else
    DrawTransparentBitmap(FinalBitmap.Canvas.Handle, FDisabled.Handle, Left, Top, clFuchsia);
  // Draw all to the control
  BitBlt(Canvas.Handle, 0, 0, FinalBitmap.Width, FinalBitmap.Height, FinalBitmap.Canvas.Handle, 0, 0, srcCopy);
end;

// -----------------------------------------------------------------------------

procedure TSWSeekBar.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    EnableWindow(Handle, Value);
    Paint;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWSeekBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Assigned(FNormal)) and (ssLeft in Shift) and (FEnabled) then
  begin
    FPosition := ((X - (FNormal.Width div 2)) * FMax) div (Width-(FNormal.Width));
    if FPosition < 0 then FPosition := 0;
    if FPosition > (FMax) then FPosition := FMax;
    Paint;
  end;
  FMouseIsDown := True;
  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y);
end;

// -----------------------------------------------------------------------------

procedure TSWSeekBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseIsDown then
  begin
    if (Assigned(FNormal)) and (ssLeft in Shift) and (FEnabled) then
    begin
      FPosition := ((X - (FNormal.Width div 2)) * FMax) div (Width-(FNormal.Width));
      if FPosition < 0 then FPosition := 0;
      if FPosition > (FMax) then FPosition := FMax;
      Paint;
    end;
    if Assigned(OnMouseUp) then
      OnMouseUp(Self, Button, Shift, X, Y);
    FMouseIsDown := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWSeekBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FMouseIsDown = True then
  begin
    if (Assigned(FNormal)) and (ssLeft in Shift) and (FEnabled) then
    begin
      FPosition := ((X - (FNormal.Width div 2)) * FMax) div (Width-(FNormal.Width));
      if FPosition < 0 then FPosition := 0;
      if FPosition > (FMax) then FPosition := FMax;
      Paint;
    end;
  end;
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, X, Y);
end;

// -----------------------------------------------------------------------------

procedure TSWSeekBar.SetMax(Max: Int64);
begin
  if FMax <> Max then
  begin
    FMax := Max;
    Paint;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWSeekBar.SetPosition(Position: Int64);
begin
  if FPosition <> Position then
  begin
    FPosition := Position;
    Paint;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWSeekBar.SetMainSeekBarBitmap(Bitmap: TBitmap);
begin
  FMainSeekBar.Assign(Bitmap);
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWSeekBar.SetNormalBtnBitmap(Bitmap: TBitmap);
begin
  FNormal.Assign(Bitmap);
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWSeekBar.SetDisabledBtnBitmap(Bitmap: TBitmap);
begin
  FDisabled.Assign(Bitmap);
  Paint;
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('URUSoft Components', [TSWSeekBar]);
end;

// -----------------------------------------------------------------------------

end.
