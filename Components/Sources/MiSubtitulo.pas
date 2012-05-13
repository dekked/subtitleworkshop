// MISUBTITULO (VIPLAY)
// Copyright © 2002 AML.
//

unit MiSubtitulo;

// --------------------------------------------------------------------

interface

uses
  Windows, Messages, Classes, Graphics, Controls, SysUtils, USGraphics;

const
  TransparentColor = $FF00FF;

type
  TMiSubtitulo = class(TCustomControl)
  private
    { Private declarations }
    FImage             : TBitmap;
    FBorder            : Boolean;
    FShadow            : Boolean;
    FForceTransparency : Boolean;
    FBorderWidth       : Byte;
    FShadowWidth       : Byte;
    FUseTags           : Boolean;
    FText              : String;
    FTextColor         : TColor;
    FBackgroundColor   : TColor;
    procedure DrawSubtitleText;
    procedure SetText(Text: String);
    procedure SetShadow(Value: Boolean);
    procedure SetBorder(Value: Boolean);
    procedure SetBorderWidth(Value: Byte);
    procedure SetShadowWidth(Value: Byte);
    procedure SetUseTags(Value: Boolean);
    procedure SetTextColor(Value: TColor);
    procedure SetBackgroundColor(Value: TColor);
    procedure SetForceTransparency(Value: Boolean);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Text            : String  read FText            write SetText;
    property Shadow          : Boolean read FShadow          write SetShadow;
    property Border          : Boolean read FBorder          write SetBorder;
    property UsarTags        : Boolean read FUseTags         write SetUseTags;
    property TextColor       : TColor  read FTextColor       write SetTextColor;
    property BackgroundColor : TColor  read FBackgroundColor write SetBackgroundColor;
    property BorderWidth     : Byte    read FBorderWidth     write SetBorderWidth;
    property ShadowWidth     : Byte    read FShadowWidth     write SetShadowWidth;
    property ForceTransparency: Boolean read FForceTransparency write SetForceTransparency;
    property Anchors;
    property Enabled;
    property Font;
    property Visible;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClick;
  end;

procedure Register;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

function RemoveSWTags(Text: String; Bold, Italic, Underline, Color: Boolean): String;
begin
  if Bold      = True then Text := StringReplace(Text, '<b>', '', [rfReplaceAll, rfIgnoreCase]);
  if Italic    = True then Text := StringReplace(Text, '<i>', '', [rfReplaceAll, rfIgnoreCase]);
  if Underline = True then Text := StringReplace(Text, '<u>', '', [rfReplaceAll, rfIgnoreCase]);
  if Color = True then
    while Pos('<c:#', Text) > 0 Do
      Delete(Text, Pos('<c:#', Text), Pos('>', Copy(Text, Pos('<c:#', Text), Length(Text))));
  Result := Text;
end;

// -----------------------------------------------------------------------------

constructor TMiSubtitulo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  FImage      := TBitmap.Create;

  Width  := 50;
  Height := 50;

  FTextColor       := clWhite;
  FBackgroundColor := clBtnFace;

  FBorder      := True;
  FShadow      := True;
  FBorderWidth := 1;
  FShadowWidth := 1;
  FText        := '';

  Font.Name    := 'Tahoma';
  Font.Size    := 24;
  Font.Style   := [fsBold];
end;

// -----------------------------------------------------------------------------

destructor TMiSubtitulo.Destroy;
begin
  FImage.Free;
  FImage := nil;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.Paint;
var
  Region: HRGN;
begin
  if (FImage.Width > 0) And (FImage.Height > 0) Then
    BitBlt(Canvas.Handle, 0, 0, FImage.Width, FImage.Height, FImage.Canvas.Handle, 0, 0, SrcCopy);
  if  not (csDesigning in ComponentState) then
  begin
    if (FForceTransparency) then
    begin
      Region := ConvertBitmapToRegion(FImage, TransparentColor);
      SetWindowRgn(Handle, Region, True);
    end else
    begin
      Region := CreateRectRGN(0, 0, Width, Height);
      SetWindowRgn(Handle, Region, True);
    end;
  end
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.DrawSubtitleText;
  function GetSubColor(Text: String): Integer;
  const
    HTMLChars: set of Char = ['A'..'F', '0'..'9'];
  var
    i: Integer;
  begin
    Result := -1;
    if (Pos('<c:#', Text) > 0) then
    begin
      Text := Copy(Text, Pos('<c:#', Text) + 4, 7);
      if Copy(Text, 7, 1) = '>' then
      begin
        Delete(Text, 7, 1);
        Text := AnsiUpperCase(Text);
       
        for i := 0 to Length(Text) do
          if (Text[i] <> '') and (Text[i] in HTMLChars = False) then exit;

        // convert hexadecimal values to RGB
        Result := Integer(StrToInt('$' + Copy(Text, 1, 2))) +
                  Integer(StrToInt('$' + Copy(Text, 3, 2))) shl 8 +
                  Integer(StrToInt('$' + Copy(Text, 5, 2))) shl 16;
      end;
    end;
  end;
var
  R, RTexto   : TRect;
  a, b        : Integer;
  Color       : Integer;
  TextToPaint : String;
  tmpFont     : TFont;
Begin
  tmpFont := nil;
  Color   := TextColor;
  TextToPaint := FText;

  if FUseTags = True then
  begin
    tmpFont := TFont.Create;
    tmpFont.Assign(Font);
    
    if Pos('<i>', TextToPaint) > 0 then
      tmpFont.Style := tmpFont.Style + [fsItalic];
    if Pos('<b>', TextToPaint) > 0 then
      tmpFont.Style := tmpFont.Style + [fsBold];
    if Pos('<u>', TextToPaint) > 0 then
      tmpFont.Style := tmpFont.Style + [fsUnderline];
    Color := GetSubColor(TextToPaint);
    if Color = -1 then
     Color := TextColor;
      
    TextToPaint := RemoveSWTags(TextToPaint, True, True, True, True);

    FImage.Canvas.Font.Assign(tmpFont);
    //tmpFont.Free;
  end else
  FImage.Canvas.Font.Assign(Font);
  
  // CALCULAR RECT
  RTexto := Rect(0, 0, 0, 0);
  DrawText(FImage.Canvas.Handle, PChar(TextToPaint), -1, RTexto, DT_CALCRECT);
  RTexto := Rect(0, 0, RTexto.Right + 10, RTexto.Bottom + 10);

  // SETEAR IMAGEN
  FImage.Width              := RTexto.Right;
  FImage.Height             := RTexto.Bottom;
  FImage.Canvas.Brush.Style := bsSolid;
  if FForceTransparency = False then
  begin
    FImage.Canvas.Brush.Color := FBackgroundColor;
    FImage.Canvas.Pen.Color   := FBackgroundColor;
  end else
  begin
    FImage.Canvas.Brush.Color := TransparentColor;
    FImage.Canvas.Pen.Color   := TransparentColor;
  end;
  FImage.Canvas.Rectangle(0, 0, FImage.Width, FImage.Height);
  FImage.Canvas.Brush.Style := bsClear;

  // DIBUJAR SOMBRA
  If FShadow = True Then
  Begin
    FImage.Canvas.Font.Color := clBlack;          // 1 // 2 to 4
                                                  // 2 // 4 to 6
                                                  // 3 // 6 to 8
   { For a := //(FShadowWidth * 2) to (FShadowWidth * 4 - 2) do //2 To 4 Do // 4 to 6
      For b := //(FShadowWidth * 2) to (FShadowWidth * 4 - 2) do //2 To 4 Do // 4 to 6}
    For a := (2 + FShadowWidth) to (FShadowWidth * 2 + 2) do//(FShadowWidth * 2) to ((FShadowWidth * 2) + 2) do
      For b := (2 + FShadowWidth) to (FShadowWidth * 2 + 2) do//(FShadowWidth * 2) to ((FShadowWidth * 2) + 2) do
      Begin
        R := Rect(RTexto.Left + a + 2, RTexto.Top + b + 1, RTexto.Right - 8 + a, RTexto.Bottom - 8 + a);
        DrawText(FImage.Canvas.Handle, PChar(TextToPaint), -1, R, DT_CENTER);
      end;
  end;   

  // DIBUJAR BORDE
  If FBorder = True Then
  Begin
    FImage.Canvas.Font.Color := clBlack;
    For a := (2 - FBorderWidth) to (2 + FBorderWidth) do
      For b := (2 - FBorderWidth) to (2 + FBorderWidth) do
      Begin
        R := Rect(RTexto.Left + a, RTexto.Top + b, RTexto.Right - 8 + a, RTexto.Bottom - 8 + a);
        DrawText(FImage.Canvas.Handle, PChar(TextToPaint), -1, R, DT_CENTER);
      end;
  end;

  // DIBUJAR TEXTO
  FImage.Canvas.Font.Color := Color;
  R := Rect(RTexto.Left + 2, RTexto.Top + 2, RTexto.Right - 6, RTexto.Bottom - 6);
  DrawText(FImage.Canvas.Handle, PChar(TextToPaint), -1, R, DT_CENTER);

  // AJUSTAR TAMAÑO
  SetBounds(Left, Top, FImage.Width, FImage.Height);

  if FUseTags then
    tmpFont.Free;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.SetText(Text: String);
var
  PrevVisible: Boolean;
begin
  if (Text = FText) then Exit;

  PrevVisible := Visible;
  Visible := False;
  FText := Text;

  if FText <> '' then
    DrawSubtitleText else
  begin
    FImage.Width  := 0;
    FImage.Height := 0;
    SetBounds(Left, Top, 0, 0);
  end;

  Paint;
  Visible := PrevVisible;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.SetShadow(Value: Boolean);
Begin
  FShadow := Value;

  DrawSubtitleText;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.SetBorder(Value: Boolean);
Begin
  FBorder := Value;

  DrawSubtitleText;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.SetBorderWidth(Value: Byte);
begin
  FBorderWidth := Value;

  DrawSubtitleText;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.SetShadowWidth(Value: Byte);
begin
  FShadowWidth := Value;

  DrawSubtitleText;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.SetTextColor(Value: TColor);
Begin
  FTextColor := Value;

  DrawSubtitleText;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.SetUseTags(Value: Boolean);
Begin
  FUseTags := Value;

  DrawSubtitleText;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.SetBackgroundColor(Value: TColor);
Begin
  FBackgroundColor := Value;

  DrawSubtitleText;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.SetForceTransparency(Value: Boolean);
begin
  FForceTransparency := Value;

  DrawSubtitleText;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TMiSubtitulo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  DrawSubtitleText;
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('ViPlay Components', [TMiSubtitulo]);
end;

// -----------------------------------------------------------------------------

End.
