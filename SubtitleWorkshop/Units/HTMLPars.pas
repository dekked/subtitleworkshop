unit HTMLPars;

interface

uses
  Windows, SysUtils, Classes, dialogs;

// -----------------------------------------------------------------------------

type
  THTMLParam = class
  private
    FKey   : String;
    FValue : String;
    procedure SetKey(Key: String);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Key   : String read FKey write SetKey;
    property Value : String read FValue;
  end;

// -----------------------------------------------------------------------------

type
  THTMLTag = class
  private
    FName: String;
    procedure SetName(Name: String);
  public
    Params: TList;
    constructor Create;
    destructor Destroy; override;
  published
    property Name : String read FName write SetName; // uppercased TAG (without <>)
  end;

// -----------------------------------------------------------------------------

type
  THTMLText = class
  private
    FLine: String;
    procedure SetLine(Line: String);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Line: String read FLine write SetLine;
  end;

// -----------------------------------------------------------------------------

type
  THTMLParser = class(TObject)
  private
    Text  : String;
    Tag   : String;
    IsTag : Boolean;
    procedure AddText;
    procedure AddTag;
  public
    Parsed : TList;
    Lines  : TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

constructor THTMLParser.Create;
begin
  inherited Create;
  Lines  := TStringList.Create;
  Parsed := TList.Create;
end;

// -----------------------------------------------------------------------------

destructor THTMLParser.Destroy;
begin
  Lines.Free;
  Parsed.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure THTMLParser.AddText;
var
  HTMLText: THTMLText;
begin
  if IsTag = False then
  begin
    if Text <> '' then
    begin
      HTMLText      := THTMLText.Create;
      HTMLText.Line := Trim(Text);
      Text := '';
      Parsed.Add(HTMLText);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure THTMLParser.AddTag;
var
  HTMLTag: THTMLTag;
begin
  IsTag        := False;
  HTMLTag      := THTMLTag.Create;
  HTMLTag.Name := Tag;
  Tag := '';
  Parsed.Add(HTMLTag);
end;

// -----------------------------------------------------------------------------

procedure THTMLParser.Execute;
var
  i         : Integer;
  s         : String;
  IsComment : Boolean;
begin
  Text      := '';
  Tag       := '';
  IsTag     := False;
  isComment := False;
  for i := 0 to Lines.Count-1 do
  begin
    s := Lines[i];
    while Length(s) > 0 do
    begin
      if Copy(s, 1, 4) = '<!--' then
        IsComment := True;
      if Copy(s, 1, 3) = '-->' then
      begin
        IsComment := False;
        Delete(s, 1, 3);
      end else
      begin
        if IsComment = False then
        begin
          if S[1] = '<' then
          begin
            AddText;
            IsTag := True;
          end else
          begin
            if S[1] = '>' then
              AddTag else
            if IsTag then
              Tag := Tag + S[1] else //If it is a tag we add each character
              Text := Text + S[1]; // If it's not, then is text
          end;
        end;
        Delete(s, 1, 1);
      end;
    end;
    if (IsTag = False) and (Text <> '') then
      Text := Text + #10;
  end;
  if (IsTag = False) and (Tag <> '') then AddTag;
  if (IsTag = False) and (Text <> '') then AddText;
end;

// -----------------------------------------------------------------------------


constructor THTMLTag.Create;
begin
  inherited Create;
  Params := TList.Create;
end;

// -----------------------------------------------------------------------------

destructor THTMLTag.Destroy;
var
  i: Integer;
begin
  for i := Params.Count downto 1 do
  begin
    THTMLParam(Params[i-1]).Free;
    Params.Delete(i-1);
  end;
  Params.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure THTMLTag.SetName(Name: String);
var
  Tag       : String;
  Param     : String;
  HTMLParam : THTMLParam;
  IsQuote   : Boolean;
begin
  Params.Clear;
  while (Length(Name) > 0) and (Name[1] <> ' ') do
  begin
    Tag := Tag + Name[1];
    Delete(Name, 1, 1);
  end;
  FName := AnsiUpperCase(Tag);

  while (Length(Name) > 0) do
  begin
    Param   := '';
    IsQuote := False;
    while (Length(Name) > 0) and (not ((Name[1] = ' ') and (IsQuote = False))) do
    begin
      if Name[1] = '"' then
        IsQuote := not(IsQuote);
      Param := Param + Name[1];
      Delete(Name, 1, 1);
    end;
    if (Length(Name) > 0) and (Name[1] = ' ') then
      Delete(Name, 1, 1);
    if Param <> '' then
    begin
      HTMLParam     := THTMLParam.Create;
      HTMLParam.Key := Param;
      Params.Add(HTMLParam);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure THTMLText.SetLine(Line: String);
{var j,i:integer;
    isEntity:boolean;
    Entity:string;
    EnLen,EnPos:integer;
    d,c:integer;
begin
 while pos(#10,Line)>0 do Line[Pos(#10,Line)]:=' ';
 while pos('  ',Line)>0 do delete(Line,pos('  ',Line),1);

 i:=1;
 isEntity:=false;
 EnPos:=0;
 while (i<=Length(Line)) do
  begin
   if Line[i]='&' then begin EnPos:=i;isEntity:=true;Entity:='';end;
   if isEntity then Entity:=Entity+Line[i];
   if isEntity then
   if (Line[i]=';') or (Line[i]=' ') then begin
                         EnLen:=Length(Entity);

                         // charset encoded entity
                         if (EnLen>2) and (Entity[2]='#') then
                          begin
                           delete(Entity,EnLen,1); //delete the ;
                           delete(Entity,1,2); // delete the &#
                           if uppercase(Entity[1])='X' then Entity[1]:='$'; // it's hex (but not supported!!!)
                           if (Length(Entity)<=3) then // we cant convert e.g. cyrillic/chinise capitals
                            begin
                             val(Entity,d,c);
                             if c=0 then // conversion successful
                              begin
                               delete(Line,EnPos,EnLen);
                               insert(Charset[d],Line,EnPos);
                               i:=EnPos; // set new start
                              end;
                            end;
                          end
                          else
                          begin // its an entity
                           j:=1;
                           while (j<=100) do
                            begin
                             if Entity=(Entities[j,1]) then
                              begin
                               delete(Line,EnPos,EnLen);
                               insert(Entities[j,2],Line,Enpos);
                               j:=102; // stop searching
                              end;
                             j:=j+1;
                            end;
                          // reset Line
                          if j=103 then i:=EnPos-1
                                   else i:=EnPos;
                          end;

                         IsEntity:=false;
                       end;
   i:=i+1;
  end; }
begin
  FLine := Line;
end;

// -----------------------------------------------------------------------------

procedure THTMLParam.SetKey(Key: String);
begin
  FValue := '';
  if Pos('=', Key) > 0 then
  begin
    FValue := Key;
    Delete(FValue, 1, Pos('=', Key));
    Key := Copy(Key, 1, Pos('=', Key)-1);

    if Length(FValue) > 1 then
    begin
      if (FValue[1] = '"') and (FValue[Length(FValue)] = '"') then
      begin
        Delete(FValue, 1, 1);
        Delete(FValue, Length(FValue), 1);
      end;
    end;
  end;
  FKey := AnsiUpperCase(key);
end;

// -----------------------------------------------------------------------------

constructor THTMLParam.Create;
begin
  inherited Create;
end;

// -----------------------------------------------------------------------------

destructor THTMLParam.Destroy;
begin
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

constructor THTMLText.Create;
begin
  inherited Create;
end;

// -----------------------------------------------------------------------------

destructor THTMLText.Destroy;
begin
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

end.
