// -------------------------------------------------------------------------- //
//                  Skinned button for Subtitle Workshop 2.xx                 //
//                         Copyright (c) 2003 DeKSoft                         //
//                                                                            //
// -------------------------------------------------------------------------- //

unit SWButton;

interface

uses
  Windows, Messages, SysUtils, Graphics, Classes, Controls, USGraphics;

type
  TStates = (sNormal, sMouseOver, sPressed);
  TSWButton = class(TCustomControl)
  private
    FNormal     : TBitmap;
    FMouseOver  : TBitmap;
    FPressed    : TBitmap;
    FDisabled   : TBitmap;
    FState      : TStates;
    FEnabled    : Boolean;
    FCanBeDown  : Boolean;
    FIsCurrDown : Boolean;
    procedure SetNormalBitmap    (Bitmap: TBitmap);
    procedure SetMouseOverBitmap (Bitmap: TBitmap);
    procedure SetPressedBitmap   (Bitmap: TBitmap);
    procedure SetDisabledBitmap  (Bitmap: TBitmap);
    procedure SetState(State: TStates);
    procedure SetCurrDown(IsDown: Boolean);
  protected
    procedure CMMouseEnter  (var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave  (var Msg: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure DblClick; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Normal    : TBitmap read FNormal     write SetNormalBitmap;
    property MouseOver : TBitmap read FMouseOver  write SetMouseOverBitmap;
    property Pressed   : TBitmap read FPressed    write SetPressedBitmap;
    property Disabled  : TBitmap read FDisabled   write SetDisabledBitmap;
    property State     : TStates read FState      write SetState;
    property Enabled   : Boolean read FEnabled    write SetEnabled;
    property CanBeDown : Boolean read FCanBeDown  write FCanBeDown;
    property Down      : Boolean read FIsCurrDown write SetCurrDown;
    property Anchors;
    property Visible;
    property ShowHint;
    property Hint;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
  end;

procedure Register;

implementation

// -----------------------------------------------------------------------------

constructor TSWButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  fState       := sNormal;
  FNormal      := TBitmap.Create;
  FMouseOver   := TBitmap.Create;
  FPressed     := TBitmap.Create;
  FDisabled    := TBitmap.Create;
  FEnabled     := True;
  FCanBeDown   := False;
  Width        := 40;
  Height       := 40;
end;

// -----------------------------------------------------------------------------

destructor TSWButton.Destroy;
begin
  FNormal.Free;
  FMouseOver.Free;
  FPressed.Free;
  FDisabled.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.Paint;
begin
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(Rect(0, 0, Self.Width, Self.Height));
//  BitBlt(Canvas.Handle, 0, 0, FNormal.Width, FNormal.Height, FNormal.Canvas.Handle, 0, 0, SrcCopy);
  if FEnabled then
  begin
    case FState of
      sNormal:
        begin
          if not (csDesigning in ComponentState) then
          begin
            Width  := FNormal.Width;
            Height := FNormal.Height;
          end;
          DrawTransparentBitmap(Canvas.Handle, FNormal.Handle, 0, 0, clFuchsia);
          //BitBlt(Canvas.Handle, 0, 0, FNormal.Width, FNormal.Height, FNormal.Canvas.Handle, 0, 0, SrcCopy);
        end;
      sMouseOver:
        begin
          if not (csDesigning in ComponentState) then
          begin
            Width  := FMouseOver.Width;
            Height := FMouseOver.Height;
          end;
          DrawTransparentBitmap(Canvas.Handle, FMouseOver.Handle, 0, 0, clFuchsia);
          //BitBlt(Canvas.Handle, 0, 0, FMouseOver.Width, FMouseOver.Height, FMouseOver.Canvas.Handle, 0, 0, SrcCopy);
        end;
      sPressed:
        begin
          if not (csDesigning in ComponentState) then
          begin
            Width  := FPressed.Width;
            Height := FPressed.Height;
          end;
          DrawTransparentBitmap(Canvas.Handle, FPressed.Handle, 0, 0, clFuchsia);
          //BitBlt(Canvas.Handle, 0, 0, FPressed.Width, FPressed.Height, FPressed.Canvas.Handle, 0, 0, SrcCopy);
        end;
    end;
  end else
  begin
    if not (csDesigning in ComponentState) then
    begin
      Width  := FDisabled.Width;
      Height := FDisabled.Height;
    end;
    DrawTransparentBitmap(Canvas.Handle, FDisabled.Handle, 0, 0, clFuchsia);
    //BitBlt(Canvas.Handle, 0, 0, FDisabled.Width, FDisabled.Height, FDisabled.Canvas.Handle, 0, 0, SrcCopy);
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Paint;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.Click;
begin
  if FEnabled then
  begin
    if Assigned(OnClick) then
      OnClick(Self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.DblClick;
begin
  if (FEnabled) and (Assigned(OnClick)) and (Assigned(FPressed)) then
  begin
    FState := sPressed;
    OnClick(Self);
    Paint;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.CMMouseEnter(var Msg: TMessage);
begin
  if (FEnabled) and (Assigned(FMouseOver)) and not (csDesigning in ComponentState) then
  begin
    if FCanBeDown = True then
    begin
      if FIsCurrDown = False then
        FState := sMouseOver else
        FState := sPressed;
    end else
      FState := sMouseOver;
    Paint;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.CMMouseLeave(var Msg: TMessage);
begin
  if (FEnabled) and (Assigned(FNormal)) then
  begin
    if FCanBeDown = True then
    begin
      if FIsCurrDown = False then
        FState := sNormal else
        FState := sPressed;
    end else
      FState := sNormal;
    Paint;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FEnabled then
  begin
    if (Assigned(FPressed)) then
    begin
      FState := sPressed;
      Paint;
    end;
    if Assigned(OnMouseDown) then
      OnMouseDown(Self, Button, Shift, X, Y);
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FEnabled then
  begin
    FIsCurrDown := not FIsCurrDown;
    if FCanBeDown = False then
    begin
     if Assigned(FMouseOver) then
       begin
       if (X > 0) and (X < Width) and (Y > 0) and (Y < Height) then
          FState := sMouseOver else
          FState := sNormal;
        Paint;
      end;
    end else
    begin
      if FIsCurrDown = True then
        FState := sPressed else
        FState := sNormal;
      Paint;
    end;
    if Assigned(OnMouseUp) then
      OnMouseUp(Self, Button, Shift, X, Y);
  end;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.SetNormalBitmap(Bitmap: TBitmap);
begin
  FNormal.Assign(Bitmap);
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.SetMouseOverBitmap(Bitmap: TBitmap);
begin
  FMouseOver.Assign(Bitmap);
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.SetPressedBitmap(Bitmap: TBitmap);
begin
  FPressed.Assign(Bitmap);
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.SetDisabledBitmap(Bitmap: TBitmap);
begin
  FDisabled.Assign(Bitmap);
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.SetState(State: TStates);
begin
  FState := State;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWButton.SetCurrDown(IsDown: Boolean);
begin
  FIsCurrDown := IsDown;
  if IsDown = True then FState := sPressed else FState := sNormal;
  if FEnabled = True then
    Paint;
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('URUSoft Components', [TSWButton]);
end;

// -----------------------------------------------------------------------------

end.

