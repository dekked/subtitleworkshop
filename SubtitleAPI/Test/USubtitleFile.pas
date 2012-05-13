// Subtitle Class
// Copyright © 2002-2003 URUSoft.

unit USubtitleFile;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils;

const
  MaxListSize = MaxInt Div 16;

type

  { TSubtitleFile }

  PStringItemList = ^TStringItemList;
  TStringItemList = array[0..MaxListSize] of String;

  TSubtitleFile = class
  private
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    function Get(Index: Integer): String;
    procedure Put(Index: Integer; const S: String);
    procedure SetCapacity(NewCapacity: Integer);
    procedure Grow;
    function GetTextStr: String;
    procedure SetTextStr(const Value: String);
  public
    constructor Create(FileName: String = ''; Trim: Boolean = True);
    destructor Destroy; override;
    procedure LoadFromFile(FileName: String; Trim: Boolean = True);
    procedure SaveToFile(FileName: String);
    function Add(const S: String; Trim: Boolean = True): Integer;
    procedure Insert(Index: Integer; const S: String; Trim: Boolean = True);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Delete(Index: Integer);
    procedure Clear;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property Strings[Index: Integer]: String read Get write Put; default;
    property Text: String read GetTextStr write SetTextStr;
  end;

  { TSubtitles }

  TSubtitleItem = record
    Text: String;
    InitialTime, FinalTime: Integer;
  end;

  PSubtitleItemList = ^TSubtitleItemList;
  TSubtitleItemList = array[0..MaxListSize] of TSubtitleItem;

  TSubtitles = class
  private
    FList: PSubtitleItemList;
    FCount: Integer;
    FFormat: ShortInt;
    FCapacity: Integer;
    procedure SetFormat(Format: ShortInt);
    function GetItem(Index: Integer): TSubtitleItem;
    procedure PutItem(Index: Integer; const Item: TSubtitleItem);
    function GetText(Index: Integer): String;
    procedure PutText(Index: Integer; const S: String);
    function GetInitialTime(Index: Integer): Integer;
    procedure PutInitialTime(Index: Integer; const Time: Integer);
    function GetFinalTime(Index: Integer): Integer;
    procedure PutFinalTime(Index: Integer; const Time: Integer);
    procedure SetCapacity(NewCapacity: Integer);
    procedure Grow;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const StartTime, EndTime: Integer; const Caption: String; CheckSub: Boolean = True): Integer; overload;
    function Add(const Item: TSubtitleItem; CheckSub: Boolean = True): Integer; overload;
    procedure Insert(Index: Integer; const StartTime, EndTime: Integer; const Caption: String; CheckSub: Boolean = True); overload;
    procedure Insert(Index: Integer; const Item: TSubtitleItem; CheckSub: Boolean = True); overload;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Delete(Index: Integer);
    procedure Clear;
    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Format: ShortInt read FFormat write SetFormat;
    property Items[Index: Integer]: TSubtitleItem read GetItem write PutItem; default;
    property Text[Index: Integer]: String read GetText write PutText;
    property InitialTime[Index: Integer]: Integer read GetInitialTime write PutInitialTime;
    property FinalTime[Index: Integer]: Integer read GetFinalTime write PutFinalTime;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

{ TSubtitleFile }

// -----------------------------------------------------------------------------

constructor TSubtitleFile.Create(FileName: String = ''; Trim: Boolean = True);
begin
  FCount    := 0;
  FCapacity := 0;

  If FileName <> '' Then
    LoadFromFile(FileName, Trim);
end;

// -----------------------------------------------------------------------------

destructor TSubtitleFile.Destroy;
begin
  If FCount <> 0 Then
    Finalize(FList^[0], FCount);

  FCount := 0;
  SetCapacity(0);
end;

// -----------------------------------------------------------------------------

procedure TSubtitleFile.LoadFromFile(FileName: String; Trim: Boolean = True);
var
  f : TextFile;
  s : String;
begin
  Clear;

  {$I-}
  AssignFile(f, FileName);
  Try
    Reset(f);

    If IOResult = 0 Then
      While Not Eof(f) Do
      Begin
        ReadLn(f, s);
        Add(s, Trim);
      End;
  Finally
    CloseFile(f);
    {$I+}
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleFile.SaveToFile(FileName: String);
var
  f : TextFile;
  i : Integer;
begin
  If FCount = 0 Then Exit;

  {$I-}
  AssignFile(f, FileName);
  Try
    ReWrite(f);

    If IOResult = 0 Then
      For i := 0 To FCount-1 Do
        WriteLn(f, Get(i));

    Flush(f);
  Finally
    CloseFile(f);
    {$I+}
  End;
end;

// -----------------------------------------------------------------------------

function TSubtitleFile.Get(Index: Integer): String;
begin
  If (Index >= 0) Or (Index < FCount) Then
    Result := FList^[Index];
end;

// -----------------------------------------------------------------------------

procedure TSubtitleFile.Put(Index: Integer; const S: String);
begin
  If (Index >= 0) Or (Index < FCount) Then
    FList^[Index] := S;
end;

// -----------------------------------------------------------------------------

function TSubtitleFile.Add(const S: String; Trim: Boolean = True): Integer;
begin
  Result := FCount;
  Insert(Result, S, Trim);
end;

// -----------------------------------------------------------------------------

procedure TSubtitleFile.Insert(Index: Integer; const S: String; Trim: Boolean = True);
var
  FLine: String;
begin
  If Trim = True Then
  Begin
    FLine := SysUtils.Trim(S);
    If FLine = '' Then Exit;
  End
  Else
    FLine := S;

  If FCount = FCapacity Then
    Grow;

  If Index < FCount Then
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(String));

  Pointer(FList^[Index]) := NIL;
  FList^[Index]          := FLine;

  Inc(FCount);
end;

// -----------------------------------------------------------------------------

procedure TSubtitleFile.Move(CurIndex, NewIndex: Integer);
var
  TempString: String;
begin
  If (CurIndex >= 0) And (CurIndex < FCount) And (CurIndex <> NewIndex) Then
  Begin
    TempString := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, TempString, False);
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleFile.Delete(Index: Integer);
begin
  If (Index < 0) Or (Index >= FCount) Then Exit;

  Finalize(FList^[Index]);
  Dec(FCount);

  If Index < FCount Then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(String));
end;

// -----------------------------------------------------------------------------

procedure TSubtitleFile.Clear;
begin
  If FCount <> 0 Then
  Begin
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleFile.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(String));
  FCapacity := NewCapacity;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleFile.Grow;
var
  Delta: Integer;
begin
  If FCapacity > 64 Then
    Delta := FCapacity Div 4
  Else If FCapacity > 8 Then
    Delta := 16
  Else
    Delta := 4;

  SetCapacity(FCapacity + Delta);
end;

// -----------------------------------------------------------------------------

function TSubtitleFile.GetTextStr: String;
var
  I, L, Size : Integer;
  P          : PChar;
  S, LB      : String;
begin
  Size := 0;
  LB   := #13#10;

  For I := 0 To FCount - 1 Do
    Inc(Size, Length(Get(I)) + Length(LB));

  SetString(Result, NIL, Size);
  P := Pointer(Result);

  For I := 0 To FCount - 1 Do
  Begin
    S := Get(I);
    L := Length(S);

    If L <> 0 Then
    Begin
      System.Move(Pointer(S)^, P^, L);
      Inc(P, L);
    End;

    L := Length(LB);

    If L <> 0 Then
    Begin
      System.Move(Pointer(LB)^, P^, L);
      Inc(P, L);
    End;
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitleFile.SetTextStr(const Value: String);
var
  P, Start : PChar;
  S        : String;
begin
  Clear;
  P := Pointer(Value);

  If P <> NIL Then
    While P^ <> #0 Do
    Begin
      Start := P;

      While Not (P^ in [#0, #10, #13]) Do
        Inc(P);

      SetString(S, Start, P - Start);
      Add(S);

      If P^ = #13 Then Inc(P);
      If P^ = #10 Then Inc(P);
    End;
end;

// -----------------------------------------------------------------------------

{ TSubtitles }

// -----------------------------------------------------------------------------

constructor TSubtitles.Create;
begin
  FCount    := 0;
  FCapacity := 0;
  FFormat   := -1;
end;

// -----------------------------------------------------------------------------

destructor TSubtitles.Destroy;
begin
  If FCount <> 0 Then
    Finalize(FList^[0], FCount);

  FCount  := 0;
  FFormat := -1;
  SetCapacity(0);
end;

// -----------------------------------------------------------------------------

function TSubtitles.GetItem(Index: Integer): TSubtitleItem;
begin
  If (Index >= 0) Or (Index < FCount) Then
    With FList^[Index] Do
    Begin
      Result.Text        := Text;
      Result.InitialTime := InitialTime;
      Result.FinalTime   := FinalTime;
    End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.PutItem(Index: Integer; const Item: TSubtitleItem);
begin
  If (Index >= 0) Or (Index < FCount) Then
    With FList^[Index] Do
    Begin
      Text        := Item.Text;
      InitialTime := Item.InitialTime;
      FinalTime   := Item.FinalTime;
    End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.SetFormat(Format: ShortInt);
begin
  FFormat := Format;
end;

// -----------------------------------------------------------------------------

function TSubtitles.GetText(Index: Integer): String;
begin
  If (Index >= 0) Or (Index < FCount) Then
    Result := FList^[Index].Text;
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.PutText(Index: Integer; const S: String);
begin
  If (Index >= 0) Or (Index < FCount) Then
    FList^[Index].Text := S;
end;

// -----------------------------------------------------------------------------

function TSubtitles.GetInitialTime(Index: Integer): Integer;
begin
  Result := 0;

  If (Index >= 0) Or (Index < FCount) Then
    Result := FList^[Index].InitialTime;
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.PutInitialTime(Index: Integer; const Time: Integer);
begin
  If (Index >= 0) Or (Index < FCount) Then
    FList^[Index].InitialTime := Time;
end;

// -----------------------------------------------------------------------------

function TSubtitles.GetFinalTime(Index: Integer): Integer;
begin
  Result := 0;

  If (Index >= 0) Or (Index < FCount) Then
    Result := FList^[Index].FinalTime;
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.PutFinalTime(Index: Integer; const Time: Integer);
begin
  If (Index >= 0) Or (Index < FCount) Then
    FList^[Index].FinalTime := Time;
end;

// -----------------------------------------------------------------------------

function TSubtitles.Add(const StartTime, EndTime: Integer; const Caption: String; CheckSub: Boolean = True): Integer;
begin
  If CheckSub = True Then
    If (Trim(Caption) = '') or (StartTime > EndTime) Then
    Begin
      Result := -1;
      Exit;
    End;
  Result := FCount;
  Insert(Result, StartTime, EndTime, Caption, False);
end;

// -----------------------------------------------------------------------------

function TSubtitles.Add(const Item: TSubtitleItem; CheckSub: Boolean = True): Integer;
begin
  If CheckSub = True Then
    If (Trim(Item.Text) = '') or (Item.InitialTime > Item.FinalTime) Then
    Begin
      Result := -1;
      Exit;
    End;
  Result := FCount;
  Insert(Result, Item.InitialTime, Item.FinalTime, Item.Text, CheckSub);
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.Insert(Index: Integer; const StartTime, EndTime: Integer; const Caption: String; CheckSub: Boolean = True);
begin
  If CheckSub = True Then
  Begin
    If (Trim(Caption) = '') or (StartTime > EndTime) or (StartTime = -1) Then
      Exit;
  End;

  If FCount = FCapacity Then
    Grow;

  If Index < FCount Then
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(TSubtitleItem));

  With FList^[Index] Do
  Begin
    Pointer(Text) := NIL;
    Text          := Caption;
    InitialTime   := StartTime;
    FinalTime     := EndTime;
  End;

  Inc(FCount);
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.Insert(Index: Integer; const Item: TSubtitleItem; CheckSub: Boolean = True);
begin
  Insert(Index, Item.InitialTime, Item.FinalTime, Item.Text, CheckSub);
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.Move(CurIndex, NewIndex: Integer);
var
  TempItem: TSubtitleItem;
begin
  If (CurIndex >= 0) And (CurIndex < FCount) And (CurIndex <> NewIndex) Then
  Begin
    TempItem := GetItem(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, TempItem.InitialTime, TempItem.FinalTime, TempItem.Text);
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.Delete(Index: Integer);
begin
  If (Index < 0) Or (Index >= FCount) Then Exit;

  Finalize(FList^[Index]);
  Dec(FCount);

  If Index < FCount Then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(TSubtitleItem));
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.Clear;
begin
  If FCount <> 0 Then
  Begin
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
  End;
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TSubtitleItem));
  FCapacity := NewCapacity;
end;

// -----------------------------------------------------------------------------

procedure TSubtitles.Grow;
var
  Delta: Integer;
begin
  If FCapacity > 64 Then
    Delta := FCapacity Div 4
  Else If FCapacity > 8 Then
    Delta := 16
  Else
    Delta := 4;

  SetCapacity(FCapacity + Delta);
end;

// -----------------------------------------------------------------------------

end.
