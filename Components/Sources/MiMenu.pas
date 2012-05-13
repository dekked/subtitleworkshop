// MIMENU (VIPLAY)
// Copyright © 2002 AML / DEKSOFT.

unit MiMenu;

// -----------------------------------------------------------------------------

interface

uses
  Windows, Classes, Graphics, Menus, Forms, SysUtils;

type
  TMiMenu = class(TComponent)
  private
    { Private declarations }
    vActivo                              : Boolean;              // ACTIVAR NUESTRO MENU?
    vBordeFlat                           : Boolean;              // BORDE FLAT?
    vSombra                              : Boolean;
    vFondo, vFranja, vBorde              : TColor;               // COLORES
    vTexto, vTextoDeshabilitado          : TColor;               // COLORES
    vSelFondo, vSelBorde, vSelTexto      : TColor;               // COLORES
    vSeparador                           : TColor;               // COLORES
    vTick, vTickSeleccionado, vTickBorde : TColor;               // COLORES
    vDegradado                           : Boolean;              // DIBUJAR DEGRADADO?
    vSeleccion                           : Boolean;              // DIBUJAR SELECCION?
    vSuavizado                           : Byte;                 // SUAVIZADO DE ICONO
    vFuente                              : TFont;                // FUENTE
//    vBuscar                              : Boolean;              // BUSCAR NUEVOS ITEMS?
    vDegradadoBMP                        : TBitmap;
    Formulario                           : TScrollingWinControl; // FORMULARIO A UTILIZAR
    procedure SetearActivo(Valor: Boolean);
    procedure ActivarMiMenu(Valor, Actualizar: Boolean);
//    procedure ActivarItem(MenuItem: TMenuItem);
    procedure DibujarChequeado(Seleccionado: Boolean; ACanvas: TCanvas; CheckedRect: TRect);
    procedure DibujarIconoGris(ABitmap: TBitmap; Value: Integer);
    procedure DibujarSombraIcono(B: TBitmap; ACanvas: TCanvas; X, Y: integer; ShadowColor: TColor);
    procedure DibujarIconoSuavizado(ABitmap: TBitmap; Value: Integer);
    Function ColorCercano(ACanvas: TCanvas; Clr: TColor; Value: Integer): TColor;
    Function ColorGris(Clr: TColor; Value: Integer): TColor;
    Function ColorSombra(Clr: TColor; Value: Integer): TColor;
    procedure DibujarDegradado(ACanvas: TCanvas; ARect: TRect);
    procedure CrearDegradado(Valor: Boolean);
    procedure DibujarBordeVentana(ACanvas: TCanvas);
  protected
    { Protected declarations }
    procedure DibujarMenu(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure DeterminarEspacio(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    Property Activo: Boolean Read vActivo Write SetearActivo Default False;
    //Property BuscarItems: Boolean Read vBuscar Write vBuscar Default False;
    Property Borde: Boolean Read vBordeFlat Write vBordeFlat Default False;
    Property ColorFondo: TColor Read vFondo Write vFondo;
    Property ColorFranja: TColor Read vFranja Write vFranja;
    Property ColorBorde: TColor Read vBorde Write vBorde;
    Property ColorTexto: TColor Read vTexto Write vTexto;
    Property ColorTextoDeshabilitado: TColor Read vTextoDeshabilitado Write vTextoDeshabilitado;
    Property ColorSeleccionFondo: TColor Read vSelFondo Write vSelFondo;
    Property ColorSeleccionBorde: TColor Read vSelBorde Write vSelBorde;
    Property ColorSeleccionTexto: TColor Read vSelTexto Write vSelTexto;
    Property ColorSeparador: TColor Read vSeparador Write vSeparador;
    Property ColorTick: TColor Read vTick Write vTick;
    Property ColorTickSeleccionado: TColor Read vTickSeleccionado Write vTickSeleccionado;
    Property ColorTickBorde: TColor Read vTickBorde Write vTickBorde;
    Property Degradado: Boolean Read vDegradado Write CrearDegradado Default False;
    Property DibujarSeleccion: Boolean Read vSeleccion Write vSeleccion Default True;
    Property Fuente: TFont Read vFuente Write vFuente;
    Property Suavizado: Byte Read vSuavizado Write vSuavizado Default 30;
    Property Sombra: Boolean Read vSombra Write vSombra Default True;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

Constructor TMiMenu.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  Formulario := Owner As TScrollingWinControl; // ASIGNAMOS EL FORMULARIO

  // ASIGNAMOS COLORES POR DEFECTO
  vFondo              := RGB(240, 240, 240);
  vFranja             := clBtnFace;
  vBorde              := RGB(100, 100, 100);
  vTexto              := clMenuText;
  vTextoDeshabilitado := clInactiveCaption;
  vSelFondo           := RGB(175, 183, 207);
  vSelBorde           := clHighlight;
  vSelTexto           := clMenuText;
  vSeparador          := clBtnFace;
  vTick               := clMenuText;
  vTickSeleccionado   := clMenuText;
  vTickBorde          := clHighlight;

  vFuente             := TFont.Create;
  vFuente.Color       := vTexto;
  vFuente.Style       := [];
  vFuente.Name        := 'Tahoma';
  vFuente.Size        := 8;

  vSuavizado          := 30;

  vActivo             := False;
  vBordeFlat          := False;
//  vBuscar             := False;
  vDegradado          := False;
  vSeleccion          := True;
  vSombra             := True;

  vDegradadoBMP       := NIL;
end;

// -----------------------------------------------------------------------------

Destructor TMiMenu.Destroy;
begin
  ActivarMiMenu(False, False); // DESACTIVAMOS NUESTRO MENU
  vFuente.Free;
  vFuente := NIL;
  if vDegradado = True Then CrearDegradado(False);
  Inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMiMenu.ActivarMiMenu(Valor, Actualizar: Boolean);
  { ACTIVA O DESACTIVA EL ITEM INDICADO }
  procedure Activar(MenuItem: TMenuItem);
  begin
    if Valor = True Then
    begin
      MenuItem.OnDrawItem    := DibujarMenu;
      MenuItem.OnMeasureItem := DeterminarEspacio;
    end
    else
    begin
      MenuItem.OnDrawItem    := NIL;
      MenuItem.OnMeasureItem := NIL;
    end;
  end;
  { ACTIVA O DESACTIVA CADA SUBITEM DEL ITEM INDICADO }
  procedure Recursivo(MenuItem: TMenuItem);
  var
    i: Integer;
  begin
    Activar(MenuItem);
    for i := 0 To MenuItem.Count-1 Do
      Recursivo(MenuItem.Items[i]);
  end;
{ BUSCA EN EL FORMULARIO INDICADO LOS COMPONENTES DE MENU }
var
  Componente: TComponent;
  i, x: Integer;
begin
  for i := 0 To Formulario.ComponentCount-1 Do
  begin
    Componente := Formulario.Components[i];
    if (Componente Is TMainMenu) Then // MAINMENU
    begin
      for x := 0 To TMainMenu(Componente).Items.Count-1 Do
      begin
        TMainMenu(Componente).OwnerDraw := Valor;
        Activar(TMainMenu(Componente).Items[x]);
        Recursivo(TMainMenu(Componente).Items[x]);
      end;
    end
    else if (Componente Is TPopupMenu) Then // POPUPMENU
    begin
      for x := 0 To TPopupMenu(Componente).Items.Count-1 Do
      begin
        TPopupMenu(Componente).OwnerDraw := Valor;
        Activar(TPopupMenu(Componente).Items[x]);
        Recursivo(TPopupMenu(Componente).Items[x]);
      end;
    end;
  end;

 if Actualizar = True Then // REDIBUJAR MENU
   Windows.DrawMenuBar(Formulario.Handle);
end;

// -----------------------------------------------------------------------------

{CHEQUEA Y ACTIVA NUEVOS ITEMS}
{procedure TMiMenu.ActivarItem(MenuItem: TMenuItem);
  procedure Activar(MenuItem: TMenuItem);
  begin
    if Addr(MenuItem.OnDrawItem) <> Addr(TMiMenu.DibujarMenu) Then
    begin
      if (Not Assigned(MenuItem.OnAdvancedDrawItem)) And (vActivo = True) Then
        MenuItem.OnDrawItem := DibujarMenu;
      if (Not Assigned(MenuItem.OnMeasureItem)) And (vActivo = True) Then
        MenuItem.OnMeasureItem := DeterminarEspacio;
    end;
  end;

var
  i, j: Integer;
begin
  Activar(MenuItem);
  for i := 0 To MenuItem.Parent.Count-1 Do
  begin
    Activar(MenuItem.Parent.Items[i]);
    for j := 0 To MenuItem.Parent.Items[i].Count-1 Do
      ActivarItem(MenuItem.Parent.Items[i].Items[j]);
  end;
end;}

// -----------------------------------------------------------------------------

procedure TMiMenu.DibujarMenu(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  MenuPrincipal : Boolean;
  tmpMenu       : TMenu;
  tmpItem       : TMenuItem;
  Texto         : String;
  X, Y          : Integer;
  R             : TRect;
  B, O          : TBitMap;
begin
  MenuPrincipal := False;

  tmpItem := TMenuItem(Sender);
  tmpMenu := tmpItem.Parent.GetParentMenu;
  if tmpMenu Is TMainMenu Then
    for x := 0 To tmpItem.GetParentMenu.Items.Count-1 Do
      if tmpItem.GetParentMenu.Items[x] = tmpItem Then
      begin
        MenuPrincipal := True;
        Break;
      end;

  With Sender As TMenuItem Do
  begin
    B := TBitmap.Create;
    Try
      B.Height := ARect.Bottom - ARect.Top;
      B.Width  := ARect.Right  - ARect.Left;

      if MenuPrincipal = True Then
      begin
        With B.Canvas Do
        begin
          if (Selected = True) And (Enabled = True) Then
          begin
            Brush.Color := vSelFondo;
            Pen.Color   := vSelBorde;
            FillRect(Rect(0, 0, B.Width, B.Height));

            // BORDE
            MoveTo(0, B.Height - 1);
            LineTo(0, 0);
            LineTo(B.Width - 6, 0);
            LineTo(B.Width - 6, B.Height);
            // FONDO
            Brush.Color := clBtnFace;//clMenuBar//clMenu;
            Pen.Color   := clBtnFace;//clMenu;

            FillRect(Rect(B.Width - 5, 0, B.Width, B.Height));
            // SOMBRA
            Brush.Color := ColorSombra(clBtnFace, 75);
            Pen.Color   := ColorSombra(clBtnFace, 75);
            FillRect(Rect(B.Width - 5, 3, B.Width - 2, B.Height));
          end
          else
          begin
            Brush.Color := clBtnFace;//clMenu;
            Pen.Color   := clBtnFace;//clMenu;
            FillRect(Rect(0, 0, B.Width, B.Height));
          end;
        end;
      end
      else
      begin
        With B.Canvas Do
        begin
          Brush.Color := vFranja;
          Pen.Color   := vFranja;

          if vDegradado = False Then
            Rectangle(0, 0, 25, B.Height)
          else
            Draw(0, 0, vDegradadoBMP);

          Brush.Color := vFondo;
          Pen.Color   := vFondo;
          Rectangle(25 , 0, B.Width, B.Height);

          if (Selected = True) And (Enabled = True) And (vSeleccion = True) Then
          begin
            Brush.Color := vSelFondo;
            Pen.Color   := vSelBorde;
            Rectangle(0, 0, B.Width, B.Height);
          end;
        end;

        if Caption = '-' Then
        begin
          With B.Canvas Do
          begin
            Pen.Color := vSeparador;
            MoveTo(25+6, B.Height Div 2);
            LineTo(B.Width, B.Height Div 2);
          end;
        end
        else
        begin
          if Assigned(TMenuItem(Sender).Parent.GetParentMenu.Images) Then
          begin
            if ImageIndex <> -1 Then
            begin
              O := TBitmap.Create;
              Try
                O.Canvas.Brush.Color := ACanvas.Brush.Color;
                O.Canvas.FillRect(Rect(0, 0, O.Width, O.Height));

                TMenuItem(Sender).Parent.GetParentMenu.Images.GetBitmap(ImageIndex, O);

                X := (25 - O.Width) Div 2 + 1;
                Y := (B.Height - O.Height) Div 2;

                if (Selected) And (Enabled) And (vSombra = True) Then
                begin
                  Dec(X);
                  Dec(Y);
                end;

                if Enabled = False then
                begin
                  DibujarIconoGris(O, 10);
                  DibujarIconoSuavizado(O, 40);
                end
                else if (Selected = False) And (Enabled = True) Then
                  DibujarIconoSuavizado(O, vSuavizado)
                else if (Selected = True) And (Enabled = True) And (vSombra = True) Then
                  DibujarSombraIcono(O, B.Canvas, X+1, Y+1, ColorSombra(vSelFondo, 50));

                O.Transparent := True;
                B.Canvas.Draw(X, Y, O);
              Finally
                O.Free;
              end;
            end;
          end;

          if (Checked = True) And (ImageIndex = -1) Then
          begin
            R := Rect((25 - 10) Div 2, (B.Height-10) Div 2, 10+(25-10) Div 2, 10+(B.Height-10) Div 2);
            DibujarChequeado(selected, B.Canvas, R);
          end;
        end;
      end;

      if Caption <> '-' Then
      begin
        B.Canvas.Font.Assign(vFuente);
        if Selected = True then
          B.Canvas.Font.Color := vSelTexto else
          B.Canvas.Font.Color := vTexto;

        if Enabled = False then
          B.Canvas.Font.Color := vTextoDeshabilitado;

        if MenuPrincipal = True then
        begin
          X := 5;
          B.Canvas.Font.Color := clMenuText;
        end else
          X := 25+6;

        Y := (B.Height - B.Canvas.TextHeight(Caption)) Div 2;
        R := Rect(X, Y, B.Width, B.Height);

        B.Canvas.Brush.Style := bsClear;

        if Default = True then
          B.Canvas.Font.Style := B.Canvas.Font.Style + [fsBold];

        DrawText(B.Canvas.Handle, PChar(Caption), Length(Caption), R, DT_LEFT Or DT_VCENTER);

        if ((Sender As TMenuItem).Count <= 0) then
        begin
          R := Rect(X, Y, B.Width-11, B.Height);
          Texto := ShortCutToText((Sender As TMenuItem).ShortCut);
          B.Canvas.Font.Style := B.Canvas.Font.Style - [fsBold];
          DrawText(B.Canvas.Handle, PChar(Texto), Length(Texto), R, DT_RIGHT Or DT_VCENTER);
        end;
      end;

      ACanvas.Draw(ARect.Left, ARect.Top, B);
    Finally
      B.Free;
    end;
  end;

  if Not (csDesigning In ComponentState) then
    if (vBordeFlat = True) And (MenuPrincipal = False) then
      DibujarBordeVentana(ACanvas);

  // CHEQUEAR NUEVOS ITEMS?
//  if vBuscar = True then ActivarItem(TMenuItem(Sender));
end;

// -----------------------------------------------------------------------------

procedure TMiMenu.DeterminarEspacio(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
var
  MenuPrincipal : Boolean;
  tmpMenu       : TMenu;
  tmpItem       : TMenuItem;
  Texto         : String;
  i             : Integer;
begin
  MenuPrincipal := False;

  tmpItem := TMenuItem(Sender);
  tmpMenu := tmpItem.Parent.GetParentMenu;
  if tmpMenu Is TMainMenu then
    for i := 0 To tmpItem.GetParentMenu.Items.Count-1 Do
      if tmpItem.GetParentMenu.Items[i] = tmpItem then
      begin
        MenuPrincipal := True;
        Break;
      end;

  Texto := (Sender As TMenuItem).Caption;
  if Trim(ShortCutToText(TMenuItem(Sender).ShortCut)) <> '' then
    Texto := Texto + ShortCutToText(TMenuItem(Sender).ShortCut);

  ACanvas.Font.Assign(vFuente);

  if MenuPrincipal = True then
    Inc(Width, 2) else
    Width := ACanvas.TextWidth(Texto+'WWW') + 15;

  if (Sender As TMenuItem).Caption <> '-' then
    Height := ACanvas.TextHeight((Sender As TMenuItem).Caption) + 8
  else
    Height:= 3;
end;

// -----------------------------------------------------------------------------

procedure TMiMenu.DibujarChequeado(Seleccionado: Boolean; ACanvas: TCanvas; CheckedRect: TRect);
var
  X1, X2: integer;
begin
  if Seleccionado = True then
    ACanvas.Brush.Color := vFondo // COLOR DEL FONDO CUANDO ESTÁ SELECCIONADO
  else
    ACanvas.Brush.Color := vFranja; // COLOR DEL FONDO

  ACanvas.Pen.Color := vTickBorde;  // COLOR DEL BORDE
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Rectangle(CheckedRect.Left, CheckedRect.Top, CheckedRect.Right, CheckedRect.Bottom);

  // COLOR DEL TICK
  if Seleccionado = True then
    ACanvas.Pen.Color := vTickSeleccionado
  else
    ACanvas.Pen.Color := vTick;

  // DIBUJAMOS EL TICK...
  x1 := CheckedRect.Left + 1;
  x2 := CheckedRect.Top + 5;
  ACanvas.MoveTo(x1, x2);

  x1 := CheckedRect.Left + 4;
  x2 := CheckedRect.Bottom - 2;
  ACanvas.LineTo(x1, x2);
  //--
  x1 := CheckedRect.Left + 2;
  x2 := CheckedRect.Top + 5;
  ACanvas.MoveTo(x1, x2);

  x1 := CheckedRect.Left + 4;
  x2 := CheckedRect.Bottom - 3;
  ACanvas.LineTo(x1, x2);
  //--
  x1 := CheckedRect.Left + 2;
  x2 := CheckedRect.Top + 4;
  ACanvas.MoveTo(x1, x2);

  x1 := CheckedRect.Left + 5;
  x2 := CheckedRect.Bottom - 3;
  ACanvas.LineTo(x1, x2);
  //-----------------

  x1 := CheckedRect.Left + 4;
  x2 := CheckedRect.Bottom - 3;
  ACanvas.MoveTo(x1, x2);

  x1 := CheckedRect.Right + 2;
  x2 := CheckedRect.Top - 1;
  ACanvas.LineTo(x1, x2);
  //--
  x1 := CheckedRect.Left + 4;
  x2 := CheckedRect.Bottom - 2;
  ACanvas.MoveTo(x1, x2);

  x1 := CheckedRect.Right - 2;
  x2 := CheckedRect.Top + 3;
  ACanvas.LineTo(x1, x2);
end;

// -----------------------------------------------------------------------------

Function TMiMenu.ColorCercano(ACanvas: TCanvas; Clr: TColor; Value: Integer): TColor;
var
  r, g, b: Integer;
begin
  if Value > 100 then Value := 100;

  Clr := ColorToRGB(Clr);
  r := (Clr and $000000FF);
  g := (Clr and $0000FF00) shr 8;
  b := (Clr and $00FF0000) shr 16;

  r := r + Round((255 - r) * (Value / 100));
  g := g + Round((255 - g) * (Value / 100));
  b := b + Round((255 - b) * (Value / 100));

  Result := Windows.GetNearestColor(ACanvas.Handle, RGB(r, g, b));
end;

// -----------------------------------------------------------------------------

Function TMiMenu.ColorGris(Clr: TColor; Value: Integer): TColor;
var
  r, g, b, avg: integer;
begin
  Clr := ColorToRGB(clr);
  r := (Clr and $000000FF);
  g := (Clr and $0000FF00) shr 8;
  b := (Clr and $00FF0000) shr 16;

  Avg := (r + g + b) Div 3;
  Avg := Avg + Value;

  if Avg > 240 then Avg := 240;
   Result := RGB(Avg, Avg, Avg);
end;

// -----------------------------------------------------------------------------

Function TMiMenu.ColorSombra(Clr: TColor; Value: Integer): TColor;
var
  r, g, b: integer;
begin
  Clr := ColorToRGB(clr);
  r := (Clr And $000000FF);
  g := (Clr And $0000FF00) shr 8;
  b := (Clr And $00FF0000) shr 16;

  r := (r - value);
  if r < 0   then r := 0;
  if r > 255 then r := 255;

  g := (g - Value) + 2;
  if g < 0   then g := 0;
  if g > 255 then g := 255;

  b := (b - Value);
  if b < 0   then b := 0;
  if b > 255 then b := 255;

  Result := RGB(r, g, b);
end;

// -----------------------------------------------------------------------------

procedure TMiMenu.DibujarIconoGris(ABitmap: TBitmap; Value: Integer);
var
  x, y: integer;
  LastColor1, LastColor2, Color: TColor;
begin
  LastColor1 := 0;
  LastColor2 := 0;

  for y := 0 To ABitmap.Height Do
    for x := 0 To ABitmap.Width Do
    begin
      Color := ABitmap.Canvas.Pixels[x, y];
      if Color = LastColor1 then
        ABitmap.Canvas.Pixels[x, y] := LastColor2
      else
      begin
        LastColor2 := ColorGris(Color, Value);
        ABitmap.Canvas.Pixels[x, y] := LastColor2;
        LastColor1 := Color;
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure TMiMenu.DibujarSombraIcono(B: TBitmap; ACanvas: TCanvas; X, Y: integer; ShadowColor: TColor);
var
  BX, BY: integer;
  TransparentColor: TColor;
begin
  TransparentColor := B.Canvas.Pixels[0, B.Height - 1];

  for BY := 0 To B.Height-1 Do
    for BX := 0 To B.Width-1 Do
    begin
      if B.Canvas.Pixels[BX, BY] <> TransparentColor then
        ACanvas.Pixels[X + BX, Y + BY] := ShadowColor;
    end;
end;

// -----------------------------------------------------------------------------

procedure TMiMenu.DibujarIconoSuavizado(ABitmap: TBitmap; Value: Integer);
var
  x, y: integer;
  LastColor1, LastColor2, Color: TColor;
begin
  if Value > 100 then Value := 100;
  LastColor1 := -1;
  LastColor2 := -1;

  for y := 0 To ABitmap.Height-1 Do
    for x := 0 To ABitmap.Width-1 Do
    begin
      Color := ABitmap.Canvas.Pixels[x, y];
      if Color = LastColor1 then
        ABitmap.Canvas.Pixels[x, y] := LastColor2
      else
      begin
        LastColor2 := ColorCercano(ABitmap.Canvas, Color, Value);
        ABitmap.Canvas.Pixels[x, y] := LastColor2;
        LastColor1 := Color;
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure TMiMenu.DibujarDegradado(ACanvas: TCanvas; ARect: TRect);
var
  R, G, B                : Byte;
  R1, R2, G1, G2, B1, B2 : Byte;
  Paso                   : Integer;
  F                      : Double;
begin
  R1:= GetRValue(vFranja); R2:= GetRValue(vFondo);
  G1:= GetGValue(vFranja); G2:= GetGValue(vFondo);
  B1:= GetBValue(vFranja); B2:= GetBValue(vFondo);

  for Paso := ARect.Left To ARect.Right Do
  begin
    F := Paso / (ARect.Right - ARect.Left);
    R := R1 + Round((R2-R1)*F);
    G := G1 + Round((G2-G1)*F);
    B := B1 + Round((B2-B1)*F);

    ACanvas.Pen.Color := RGB(R, G, B);
    ACanvas.MoveTo(Paso, ARect.Top);
    ACanvas.LineTo(Paso, ARect.Bottom);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMiMenu.DibujarBordeVentana(ACanvas: TCanvas);
var
  hWnd    : THandle;
  BCanvas : TCanvas;
  R       : TRect;
begin
  hWnd := WindowFromDC(ACanvas.Handle);

  if hWnd <> Formulario.Handle then
  begin
    BCanvas := TCanvas.Create;
    BCanvas.Handle := GetDC(0);

    Windows.GetWindowRect(hWnd, R);

    BCanvas.Brush.Color := vBorde;
    BCanvas.FrameRect(R);

    InflateRect(R, -1, -1);
    BCanvas.Brush.Color := vFondo;
    BCanvas.FrameRect(R);

    InflateRect(R, -1, -1);
    BCanvas.FrameRect(R);

    ReleaseDC(0, BCanvas.Handle);
    BCanvas.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMiMenu.CrearDegradado(Valor: Boolean);
begin
  if Valor = True then
  begin
    if Assigned(vDegradadoBMP) then vDegradadoBMP.Free;
    vDegradadoBMP := NIL;
    vDegradadoBMP := TBitmap.Create;
    vDegradadoBMP.Width  := 25;
    vDegradadoBMP.Height := 25;
    DibujarDegradado(vDegradadoBMP.Canvas, Rect(0, 0, vDegradadoBMP.Width, vDegradadoBMP.Height));
    vDegradado    := True;
  end
  else
  begin
    if Assigned(vDegradadoBMP) then vDegradadoBMP.Free;
    vDegradadoBMP := NIL;
    vDegradado    := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMiMenu.SetearActivo(Valor: Boolean);
begin
  vActivo := Valor;
  ActivarMiMenu(Valor, True);
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('URUSoft Components', [TMiMenu]);
end;

// -----------------------------------------------------------------------------

end.
