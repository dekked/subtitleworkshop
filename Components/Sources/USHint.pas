{*
 *  ViPlay3 - Rise Of The Players
 *  Copyright (C) 2001-2005 Aldo Lacavalla
 *
 *  Email   : aml@urusoft.net
 *  Website : www.urusoft.net
 *
 *}

unit USHint;

// -----------------------------------------------------------------------------

interface

uses
  Windows, Classes, Controls, Graphics, Forms;

type

  { TUSHint }

  TUSHint = class(TComponent)
  private
    FOnShowHint  : TShowHintEvent;
    FActive      : Boolean;
    FFade        : Byte;
    FBackColor   : TColor;
    FBorderColor : TColor;
    FTextColor   : TColor;
    procedure ShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure SetActive(Value: Boolean);
    procedure SetBackColor(Value: TColor);
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnShowHint  : TShowHintEvent read FOnShowHint  write FOnShowHint;
    property Active      : Boolean        read FActive      write SetActive;
    property Fade        : Byte           read FFade        write FFade;
    property BackColor   : TColor         read FBackColor   write SetBackColor;
    property BorderColor : TColor         read FBorderColor write FBorderColor;
    property TextColor   : TColor         read FTextColor   write FTextColor;
  end;

  { TUSHintWindow }

  TCursorPos = (NE, NW, SE, SW);

  TUSHintWindow = class(THintWindow)
  private
    FCursorPos : TCursorPos;
    FHint      : TUSHint;
    function FindHintComponent: TUSHint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    procedure ActivateHint(HintRect: TRect; const AHint: String); Override;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TUSHint }

// -----------------------------------------------------------------------------

constructor TUSHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FActive      := False;
  FFade        := 255;  
  FBackColor   := Application.HintColor;
  FBorderColor := clBlack;
  FTextColor   := clInfoText;
end;

// -----------------------------------------------------------------------------

destructor TUSHint.Destroy;
begin
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TUSHint.ShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  if Assigned(FOnShowHint) then
    FOnShowHint(HintStr, CanShow, HintInfo);
end;

// -----------------------------------------------------------------------------

procedure TUSHint.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;

    if not (csDesigning in ComponentState) then
      if Value then
      begin
        HintWindowClass        := TUSHintWindow;
        Application.OnShowHint := ShowHint;
      end
      else
      begin
        HintWindowClass        := THintWindow;
        Application.OnShowHint := NIL;
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TUSHint.SetBackColor(Value: TColor);
begin
  FBackColor            := Value;
  Application.HintColor := FBackColor;
end;

// -----------------------------------------------------------------------------

{ TUSHintWindow }

// -----------------------------------------------------------------------------

function TUSHintWindow.FindHintComponent: TUSHint;
var
  Component: Integer;
begin
  Result := NIL;

  with Application.MainForm do
    for Component := 0 to ComponentCount - 1 do
      if Components[Component] is TUSHint then
      begin
        Result := TUSHint(Components[Component]);
        Break;
      end;
end;

// -----------------------------------------------------------------------------

procedure TUSHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style   := Params.Style - WS_BORDER;
  Params.ExStyle := Params.ExStyle or WS_EX_LAYERED;

  FHint := FindHintComponent;
end;

// -----------------------------------------------------------------------------

procedure TUSHintWindow.ActivateHint(HintRect: TRect; const AHint: String);
var
  Pnt                                      : TPoint;
  HintHeight, HintWidth                    : Integer;
  NordWest, NordEast, SouthWest, SouthEast : TRect;
begin
  // Asignar Fuente a utilizar
  Canvas.Font.Assign(Application.MainForm.Font);

  // Asignar Texto
  Caption := AHint;

  // Calcular RECT
  DrawText(Canvas.Handle, @AHint[1], Length(AHint), HintRect, DT_CALCRECT);
  Inc(HintRect.Right, 22);
  Inc(HintRect.Bottom, 36);

  // Dividir la pantalla en 4
  NordWest  := Rect(0, 0, Screen.Width shr 1, Screen.Height shr 1);
  NordEast  := Rect(Screen.Width shr 1, 0, Screen.Width, Screen.Height shr 1);
  SouthWest := Rect(0, Screen.Height shr 1, Screen.Width shr 1, Screen.Height);
  SouthEast := Rect(Screen.Width shr 1, Screen.Height shr 1, Screen.Width, Screen.Height);

  GetCursorPos(Pnt);

  if PtInRect(NordWest, Pnt) then
    FCursorPos := NW
  else if PtInRect(NordEast, Pnt) then
    FCursorPos := NE
  else if PtInRect(SouthWest, Pnt) then
    FCursorPos := SW
  else
    FCursorPos := SE;

  // Calcular la posición del Hint
  HintHeight := HintRect.Bottom - HintRect.Top;
  HintWidth  := HintRect.Right  - HintRect.Left;

  case FCursorPos of
    NW: HintRect := Rect(Pnt.x - 10, Pnt.y + 15, Pnt.x + HintWidth - 10, Pnt.y + HintHeight + 15);
    NE: HintRect := Rect(Pnt.x - HintWidth + 10, Pnt.y + 15, Pnt.x + 10, Pnt.y + HintHeight + 15);
    SW: HintRect := Rect(Pnt.x - 10, Pnt.y - HintHeight - 5, Pnt.x + HintWidth - 10, Pnt.y - 5);
  else
        HintRect := Rect(Pnt.x - HintWidth + 10, Pnt.y - HintHeight - 5, Pnt.x + 10, Pnt.y - 5);
  end;

  BoundsRect := HintRect;
  Pnt        := ClientToScreen(Point(0, 0));

  Paint; // Sin esto a veces no lo pinta bien :S

  SetWindowPos(Handle, HWND_TOPMOST, Pnt.X, Pnt.Y, 0, 0, SWP_SHOWWINDOW or
    SWP_NOACTIVATE or SWP_NOSIZE);
end;

// -----------------------------------------------------------------------------

procedure TUSHintWindow.Paint;
var
  TextRect         : TRect;
  Region1, Region2 : HRGN;
  Points           : array[0..2] of TPoint;
begin
  // Crear primer región y rect del texto
  case FCursorPos of
    NW: begin
          Points[0].X := 10; Points[0].Y := 0;
          Points[1].X := 20; Points[1].Y := 10;
          Points[2].X := 10; Points[2].Y := 10;
          Region2  := CreateRoundRectRgn(0, 10, Width, Height, 10, 10);
          TextRect := Rect(ClientRect.Left + 1, ClientRect.Top + 10, ClientRect.Right - 1, ClientRect.Bottom - 1);
        end;
    NE: begin
          Points[0].X := Width-10; Points[0].Y := 0;
          Points[1].X := Width-20; Points[1].Y := 10;
          Points[2].X := Width-10; Points[2].Y := 10;
          Region2  := CreateRoundRectRgn(0, 10, Width, Height, 10, 10);
          TextRect := Rect(ClientRect.Left + 1, ClientRect.Top + 10, ClientRect.Right - 1, ClientRect.Bottom - 1);
        end;
    SW: begin
          Points[0].X := 10; Points[0].Y := Height;
          Points[1].X := 20; Points[1].Y := Height-10;
          Points[2].X := 10; Points[2].Y := Height-10;
          Region2  := CreateRoundRectRgn(0, 0, Width, Height-9, 10, 10);
          TextRect := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 10);
        end;
    else
        begin
          Points[0].X := Width-10; Points[0].Y := Height;
          Points[1].X := Width-20; Points[1].Y := Height-10;
          Points[2].X := Width-10; Points[2].Y := Height-10;
          Region2  := CreateRoundRectRgn(0, 0, Width, Height-9, 10, 10);
          TextRect := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 10);
        end;
  end;

  // Crear segunda región y unir a la primera
  Region1 := CreatePolygonRgn(Points[0], 3, ALTERNATE);
  CombineRgn(Region1, Region1, Region2, RGN_OR);

  // Liberar la segunda región
  DeleteObject(Region2);

  Canvas.Brush.Style := bsSolid;

  // Pintar el fondo
  Canvas.Brush.Color := FHint.FBackColor;
  FillRgn(Canvas.Handle, Region1, Canvas.Brush.Handle);

  // Dibujar el borde
  Canvas.Brush.Color := FHint.FBorderColor;
  FrameRgn(Canvas.Handle, Region1, Canvas.Brush.Handle, 1, 1);

  // Asignar la región a la ventana del Hint
  SetWindowRgn(Handle, Region1, true);

  // Liberar la primer región (segun MS no hay que hacerlo!)
  //DeleteObject(Region1);

  // Dibujar el Texto
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color  := FHint.FTextColor;
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextRect, DT_CENTER or DT_SINGLELINE or DT_VCENTER);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('URUSoft Components', [TUSHint]);
end;

// -----------------------------------------------------------------------------

end.
