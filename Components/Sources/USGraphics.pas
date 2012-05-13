{*
 *  USGraphics
 *  Copyright (C) 2001-2003 URUSoft
 *
 *  Website : http://urusoft.co.nr
 *
 *}

unit USGraphics;

// -----------------------------------------------------------------------------

interface

uses
  Windows, Graphics;

function ConvertBitmapToRegion(hBmp: TBitmap; TransColor: TColor): HRGN;  
procedure DrawTransparentBitmap(hCanvas: HDC; hBMP: HBITMAP; X, Y: Integer; TransparentColor: COLORREF);

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

function ConvertBitmapToRegion(hBmp: TBitmap; TransColor: TColor): HRGN;
const
  ALLOC_UNIT = 100;

var
  MemDC, DC     : HDC;
  BitmapInfo    : TBitmapInfo;
  hbm32, holdBmp,
  holdMemBmp    : HBitmap;
  pbits32       : Pointer;
  bm32          : BITMAP;
  maxRects      : DWORD;
  hData         : HGLOBAL;
  pData         : PRgnData;
  b, CR, CG, CB : Byte;
  p32           : PByte;
  x, x0, y      : Integer;
  p             : PLongInt;
  pr            : PRect;
  h             : HRGN;
begin
  Result := 0;

  if hBmp <> NIL then
  begin
    { Cria um Device Context onde serﬂ armazenado o Bitmap }
    MemDC := CreateCompatibleDC(0);
    if MemDC <> 0 then
    begin
     { Cria um Bitmap de 32 bits sem compress“o }
      with BitmapInfo.bmiHeader do
      begin
        biSize          := SizeOf(TBitmapInfoHeader);
        biWidth         := hBmp.Width;
        biHeight        := hBmp.Height;
        biPlanes        := 1;
        biBitCount      := 32;
        biCompression   := BI_RGB;
        biSizeImage     := 0;
        biXPelsPerMeter := 0;
        biYPelsPerMeter := 0;
        biClrUsed       := 0;
        biClrImportant  := 0;
      end;

      hbm32 := CreateDIBSection(MemDC, BitmapInfo, DIB_RGB_COLORS, pbits32, 0, 0);
      if hbm32 <> 0 then
      begin
        holdMemBmp := SelectObject(MemDC, hbm32);
        {
          Calcula quantos bytes por linha o bitmap de 32 bits ocupa.
        }
        GetObject(hbm32, SizeOf(bm32), @bm32);
        while (bm32.bmWidthBytes mod 4) > 0 do
          inc(bm32.bmWidthBytes);

        DC := CreateCompatibleDC(MemDC);
        { Copia o bitmap para o Device Context }
        holdBmp := SelectObject(DC, hBmp.Handle);
        BitBlt(MemDC, 0, 0, hBmp.Width, hBmp.Height, DC, 0, 0, SRCCOPY);
        {
          Para melhor performance, serﬂ utilizada a fun˛“o ExtCreasteRegion
          para criar o HRGN. Esta fun˛“o recebe uma estrutura RGNDATA.
          Cada estrutura terﬂ 100 ret‘ngulos por padr“o (ALLOC_UNIT)
        }
        maxRects := ALLOC_UNIT;
        hData    := GlobalAlloc(GMEM_MOVEABLE, SizeOf(TRgnDataHeader) + SizeOf(TRect) * maxRects);
        pData    := GlobalLock(hData);
        pData^.rdh.dwSize   := SizeOf(TRgnDataHeader);
        pData^.rdh.iType    := RDH_RECTANGLES;
        pData^.rdh.nCount   := 0;
        pData^.rdh.nRgnSize := 0;
        SetRect(pData^.rdh.rcBound, MaxInt, MaxInt, 0, 0);
        { Separa o pixel em suas cores fundamentais }
        CR := GetRValue(ColorToRGB(TransColor));
        CG := GetGValue(ColorToRGB(TransColor));
        CB := GetBValue(ColorToRGB(TransColor));
        {
          Processa os pixels bitmap de baixo para cima, jﬂ que bitmaps s“o
          verticalmente invertidos.
        }
        p32 := bm32.bmBits;
        inc(PChar(p32), (bm32.bmHeight - 1) * bm32.bmWidthBytes);
        for y := 0 to hBmp.Height-1 do
        begin
          { Processa os pixels do bitmap da esquerda para a direita }
          x := -1;
          while x+1 < hBmp.Width do
          begin
            inc(x);
            { Procura por uma faixa cont›nua de pixels n“o transparentes }
            x0 := x;
            p  := PLongInt(p32);
            inc(PChar(p), x * SizeOf(LongInt));
            while x < hBmp.Width do
            begin
              b := GetBValue(p^);
              if (b = CR) then
              begin
                b := GetGValue(p^);
                if (b = CG) then
                begin
                  b := GetRValue(p^);
                  if (b = CB) then
                    Break;
                end;
              end;
              inc(PChar(p), SizeOf(LongInt));
              inc(x);
            end;

            if x > x0 then
            begin
              {
                Adiciona o intervalo de pixels [(x0, y),(x, y+1)] como um novo
                ret‘ngulo na regi“o.
              }
              if pData^.rdh.nCount >= maxRects then
              begin
                GlobalUnlock(hData);
                inc(maxRects, ALLOC_UNIT);
                hData := GlobalReAlloc(hData, SizeOf(TRgnDataHeader) + SizeOf(TRect) * maxRects, GMEM_MOVEABLE);
                pData := GlobalLock(hData);
                Assert(pData <> NIL);
              end;
              pr := @pData^.Buffer[pData^.rdh.nCount * SizeOf(TRect)];
              SetRect(pr^, x0, y, x, y+1);
              if x0 < pData^.rdh.rcBound.Left then
                pData^.rdh.rcBound.Left := x0;
              if y < pData^.rdh.rcBound.Top then
                pData^.rdh.rcBound.Top := y;
              if x > pData^.rdh.rcBound.Right then
                pData^.rdh.rcBound.Left := x;
              if y+1 > pData^.rdh.rcBound.Bottom then
                pData^.rdh.rcBound.Bottom := y+1;
              inc(pData^.rdh.nCount);
              {
               No Windows98, a fun˛“o ExtCreateRegion() pode falhar se o n∑mero
               de ret‘ngulos for maior que 4000. Por este motivo, a regi“o deve
               ser criada por partes com menos de 4000 ret‘ngulos. Neste caso, foram
               padronizadas regißes com 2000 ret‘ngulos.
              }
              if pData^.rdh.nCount = 2000 then
              begin
                h := ExtCreateRegion(NIL, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), pData^);
                Assert(h <> 0);
               { Combina a regi“o parcial, rec⁄m criada, com as anteriores }
                if Result <> 0 then
                begin
                  CombineRgn(Result, Result, h, RGN_OR);
                  DeleteObject(h);
                end
                else
                  Result := h;

                pData^.rdh.nCount := 0;
                SetRect(pData^.rdh.rcBound, MaxInt, MaxInt, 0, 0);
              end;
            end;
          end;
          Dec(PChar(p32), bm32.bmWidthBytes);
        end;
        { Cria a regi“o geral }
        h := ExtCreateRegion(NIL, SizeOf(TRgnDataHeader) + (SizeOf(TRect) * maxRects), pData^);
        Assert(h <> 0);

        if Result <> 0 then
        begin
          CombineRgn(Result, Result, h, RGN_OR);
          DeleteObject(h);
        end
        else
          Result := h;
        { Com a regi“o final completa, o bitmap de 32 bits pode ser
          removido da memæria, com todos os outros ponteiros que foram criados.}
        GlobalFree(hData);
        SelectObject(DC, holdBmp);
        DeleteDC(DC);
        DeleteObject(SelectObject(MemDC, holdMemBmp));
      end;
    end;
    DeleteDC(MemDC);
  end;
end;

// -----------------------------------------------------------------------------

procedure DrawTransparentBitmap(hCanvas: HDC; hBMP: HBITMAP; X, Y: Integer; TransparentColor: COLORREF);
var
  bm     : BITMAP;
  cColor : COLORREF;
  ptSize : TPOINT;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave : HDC;
  bmAndBack, bmAndObject, bmAndMem, bmSave     : HBITMAP;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld  : HBITMAP;
begin
  hdcTemp := CreateCompatibleDC(hCanvas);
  SelectObject(hdcTemp, hBmp); // Select the bitmap

  GetObject(hBmp, SizeOf(BITMAP), @bm);
  ptSize.x := bm.bmWidth;     // Get width of bitmap
  ptSize.y := bm.bmHeight;    // Get height of bitmap
  DPtoLP(hdcTemp, ptSize, 1); // Convert from device to logical point

  // Create some DCs to hold temporary data.
  hdcBack   := CreateCompatibleDC(hCanvas);
  hdcObject := CreateCompatibleDC(hCanvas);
  hdcMem    := CreateCompatibleDC(hCanvas);
  hdcSave   := CreateCompatibleDC(hCanvas);

  // Set proper mapping mode.
  SetMapMode(hdcTemp, GetMapMode(hCanvas));

  // Create a bitmap for each DC. DCs are required for a number of GDI functions.

  // Monochrome DC
  bmAndBack   := CreateBitmap(ptSize.x, ptSize.y, 1, 1, NIL);

  // Monochrome DC
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, NIL);

  bmAndMem    := CreateCompatibleBitmap(hCanvas, ptSize.x, ptSize.y);
  bmSave      := CreateCompatibleBitmap(hCanvas, ptSize.x, ptSize.y);

  // Each DC must select a bitmap object to store pixel data.
  bmBackOld   := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld    := SelectObject(hdcMem, bmAndMem);
  bmSaveOld   := SelectObject(hdcSave, bmSave);

  // Save the bitmap sent here, because it will be overwritten.
  BitBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SrcCopy);

  // Set the background color of the source DC to the color.
  // contained in the parts of the bitmap that should be transparent
  cColor := SetBkColor(hdcTemp, TransparentColor);

  // Create the object mask for the bitmap by performing a BitBlt
  // from the source bitmap to a monochrome bitmap.
  BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SrcCopy);

  // Set the background color of the source DC back to the original color.
  SetBkColor(hdcTemp, cColor);

  // Create the inverse of the object mask.
  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NotSrcCopy);

  // Copy the background of the main DC to the destination.
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hCanvas, x, y, SrcCopy);

  // Mask out the places where the bitmap will be placed.
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SrcAnd);

  // Mask out the transparent colored pixels on the bitmap.
  BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcBack, 0, 0, SrcAnd);

  // XOR the bitmap with the background on the destination DC.
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SrcPaint);

  // Copy the destination to the screen.
  BitBlt(hCanvas, x, y, ptSize.x, ptSize.y, hdcMem, 0, 0, SrcCopy);

  // Place the original bitmap back into the bitmap sent here.
  BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcSave, 0, 0, SrcCopy);

  // Delete the memory bitmaps.
  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

  // Delete the memory DCs.
  DeleteDC(hdcMem);
  DeleteDC(hdcBack);
  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

// -----------------------------------------------------------------------------

end.
