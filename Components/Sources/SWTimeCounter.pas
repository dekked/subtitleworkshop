unit SWTimeCounter;

// -----------------------------------------------------------------------------

interface

// -----------------------------------------------------------------------------

uses
  SysUtils, Math, Classes, Graphics, Controls;

// -----------------------------------------------------------------------------

type
  TSWTimeCounter = class(TCustomControl)
  private
    FText1     : String;
    FText2     : String;
    FText3     : String;
    FText4     : String;
    FBackColor : TColor;
    FTextColor : TColor;
    FCharset   : TFontCharset;
    procedure SetText1(const Text1: String);
    procedure SetText2(const Text2: String);
    procedure SetText3(const Text3: String);
    procedure SetText4(const Text4: String);
    procedure SetBackColor(const Color: TColor);
    procedure SetTextColor(const Color: TColor);
    procedure SetFontCharset(const Value: TFontCharset);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Text1: String read FText1 write SetText1;
    property Text2: String read FText2 write SetText2;
    property Text3: String read FText3 write SetText3;
    property Text4: String read FText4 write SetText4;
    property BackColor: TColor read FBackColor write SetBackColor;
    property TextColor: TColor read FTextColor write SetTextColor;
    property FontCharset: TFontCharset read FCharset write SetFontCharset;
    property Anchors;
    property Visible;
  end;

procedure Register;

implementation

// -----------------------------------------------------------------------------

constructor TSWTimeCounter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Width        := 125;
  Height       := 35;
  FBackColor   := clBlack;
  FTextColor   := clWhite;
end;

// -----------------------------------------------------------------------------

destructor TSWTimeCounter.Destroy;
begin
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TSWTimeCounter.Paint;
var
  X: Integer;
begin
  X := Max(Canvas.TextWidth(FText1), Canvas.TextWidth(FText2));
  Canvas.Font.Charset := FCharset;
  Canvas.Brush.Color := FBackColor;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  Canvas.Font.Name  := 'Tahoma';
  Canvas.Font.Size  := 8;
  Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  Canvas.Font.Color := FTextColor;
  Canvas.TextOut(4, 0, FText1);
  Canvas.TextOut(4, Canvas.TextHeight(FText1), FText2);
  Canvas.Pen.Color := FTextColor;
  Canvas.Pen.Width := 2;

  Canvas.MoveTo(8 + X, Canvas.TextHeight(FText1)*2);
  Canvas.LineTo(8 + X, 0);
  Canvas.TextOut(4 + X + 8, 0, FText3);
  Canvas.TextOut(4 + X + 8, Canvas.TextHeight(FText3), FText4);
end;

// -----------------------------------------------------------------------------

procedure TSWTimeCounter.SetText1(const Text1: String);
begin
  FText1 := Text1;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWTimeCounter.SetText2(const Text2: String);
begin
  FText2 := Text2;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWTimeCounter.SetText3(const Text3: String);
begin
  FText3 := Text3;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWTimeCounter.SetText4(const Text4: String);
begin
  FText4 := Text4;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWTimeCounter.SetBackColor(const Color: TColor);
begin
  FBackColor := Color;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWTimeCounter.SetTextColor(const Color: TColor);
begin
  FTextColor := Color;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure TSWTimeCounter.SetFontCharset(const Value: TFontCharset);
begin
  FCharset := Value;
  Paint;
end;

// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('URUSoft Components', [TSWTimeCounter]);
end;

// -----------------------------------------------------------------------------

end.
