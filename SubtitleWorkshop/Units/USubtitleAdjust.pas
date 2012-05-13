// URUSoft Subtitle Adjust
// Copyright ® 2003 URUSoft.

unit USubtitleAdjust;

// -----------------------------------------------------------------------------

interface

uses
  Windows, SysUtils, Forms, Classes, Controls, StdCtrls, ComCtrls, ExtCtrls;

const
    ZEROFILL    = 0;
    EXTRAPOLATE = 1;
    NEIGHBOUR   = 2;

type
  poly = Record
    num: Integer;
    value: Integer;
  end;

  TAdjust = class
    private
      Size: Integer;
      Sorted: Integer;
      m: array[0..5000] of poly;
      procedure Sort();

    public
      function CalcLineValue (v: Integer; method: Integer): Integer;
      function GetValue (v: Integer): Integer;
      function GetNum (v: Integer): Integer;
      function GetSize (): Integer;

      procedure Add(n: Integer; v: Integer);
      procedure Init();
  end;

// -----------------------------------------------------------------------------
implementation
// -----------------------------------------------------------------------------

procedure TAdjust.Init();
begin
  Size := 0;
  Sorted := 0;
end;

// -----------------------------------------------------------------------------

procedure TAdjust.Add(n: Integer; v: Integer);
var detected: boolean;
    i: Integer;
begin
   if (size<5000) then
   begin
      detected := false;

      if(size > 0) then
      begin
         for i:= 1 to size do
         begin
            if(m[i].num = n) then
            begin
               detected := true;
               m[i].value := v;
               break;
            end;
         end;
      end;

      if(detected = false) then
      begin
         size := size + 1;
         m[size].num := n;
         m[size].value := v;
      end;
   end;

   Sorted := 0;
end;

// -----------------------------------------------------------------------------

Procedure TAdjust.Sort();
var changed: Byte;
    i: Integer;
    temp: poly;
begin
    repeat
       changed := 0;
       for i:=0 to size-1 do
       begin
          if (m[i].num>m[i+1].num) then
          begin
             temp := m[i];
             m[i] := m[i+1];
             m[i+1] := temp;

             changed := 1;
          end;
       end;
    until changed = 0;

    Sorted := 1;
end;

// -----------------------------------------------------------------------------

Function TAdjust.CalcLineValue (v: Integer; method: Integer): Integer;
var gt, lt, i: integer;
    gtv, ltv, total: Real;
    found: boolean;
begin
    if (Sorted = 0) then
    begin
       Sort();
    end;

    total := 0;
    found := false;

    if (size = 0) then
    begin
       found := true;
    end;

    if ((found = false) and (size = 1)) then
    begin
       if (method = ZEROFILL) then
       begin
          total := 0;
       end
       else
       begin
          total := m[1].value;
       end;
       found := true;
    end;

    if (found = false) then
    begin
       for i:= 1 to size do
       begin
           if (m[i].num = v) then
           begin
              total := m[i].value;
              found := true;
              break;
           end;
       end;
    end;

    if (found = false) then
    begin
        if ((v > m[1].num) and (v < m[size].num)) then
        begin
           gt := 0;
           lt := 0;
           gtv := 0;
           ltv := 0;
           for i:= 1 to size do
           begin
               if (v > m[i].num) then
               begin
                  lt := m[i].num;
                  ltv := m[i].value;
               end;
               if (v < m[i].num) then
               begin
                  gt := m[i].num;
                  gtv := m[i].value;
               end;
           end;

           total := ltv + ((gtv-ltv)/(gt-lt)) * (v-lt);
        end
        else
        begin
           if (method = ZEROFILL) then
           begin
               total := 0;
           end;

           if (method = NEIGHBOUR) then
           begin
              if (v < m[1].num) then
              begin
                 total := m[1].value;
              end
              else
              begin
                 total := m[size].value;
              end;
           end;

           if (method = EXTRAPOLATE) then
           begin
              if (v < m[1].num) then
              begin
                 lt := m[1].num;
                 ltv := m[1].value;
                 gt := m[2].num;
                 gtv := m[2].value;
              end
              else
              begin
                 lt := m[size-1].num;
                 ltv := m[size-1].value;
                 gt := m[size].num;
                 gtv := m[size].value;
              end;

              total := ltv + ((gtv-ltv)/(gt-lt)) * (v-lt);
           end;
        end;
    end;

    CalcLineValue := v + round(total);
end;

// -----------------------------------------------------------------------------

Function TAdjust.GetValue (v: Integer): Integer;
begin
    GetValue := round(m[v].value);
end;

// -----------------------------------------------------------------------------

Function TAdjust.GetNum (v: Integer): Integer;
begin
    GetNum := round(m[v].num);
end;

// -----------------------------------------------------------------------------

Function TAdjust.GetSize (): Integer;
begin
    GetSize := size;
end;

// -----------------------------------------------------------------------------

end.
