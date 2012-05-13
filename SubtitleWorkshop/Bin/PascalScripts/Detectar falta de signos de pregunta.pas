program DetectBadQuestionMarks;

var
  a: Integer;
  b: String;
begin
  for a := GetSubtitleCount-1 downto 0 do
  begin
    b := GetSubtitleText(a);
		if (Pos('¿', b) = 0) and (Pos('?', b) > 0) then
		  SetSubtitleText(a, 'FALTAPREGUNTA! ' + b);
  end;
end.
