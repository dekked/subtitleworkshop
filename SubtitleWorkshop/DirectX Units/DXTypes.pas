{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  File:       extracted from various DirectX SDK include files              *}
{*                                                                            *}
{*  Content:    DirectX 9.0 headers common types                              *}
{*                                                                            *}
{*  Direct3DX 9.0 Delphi adaptation by Alexey Barkovoy                        *}
{*  E-Mail: clootie@ixbt.com                                                  *}
{*                                                                            *}
{*  Modified: 19-Jan-2004                                                     *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*     http://clootie.narod.ru/delphi                                         *}
{*                                                                            *}
{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is DXTypes.pas.                                            }
{                                                                              }
{******************************************************************************}
unit DXTypes;

interface

uses Windows;

type
  // TD3DValue is the fundamental Direct3D fractional data type
  D3DVALUE = Single;
  {$EXTERNALSYM D3DVALUE}
  TD3DValue = D3DVALUE;
  PD3DValue = ^TD3DValue;

  D3DCOLOR = {$IFDEF TYPE_IDENTITY}type {$ENDIF}DWord;
  {$EXTERNALSYM D3DCOLOR}
  TD3DColor = D3DCOLOR;
  PD3DColor = ^TD3DColor;

  _D3DVECTOR = packed record
    x: Single;
    y: Single;
    z: Single;
  end {_D3DVECTOR};
  {$EXTERNALSYM _D3DVECTOR}
  D3DVECTOR = _D3DVECTOR;
  {$EXTERNALSYM D3DVECTOR}
  TD3DVector = _D3DVECTOR;
  PD3DVector = ^TD3DVector;

implementation

end.

