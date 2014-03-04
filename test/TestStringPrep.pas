{ TestStringPrep.pas is part of the fpOdf.

  fpOdf is a library used to help users to create and to modify OpenDocument
  Files(ODF)

  Copyright (C) 2013-2014 Daniel F. Gaspary https://github.com/dgaspary

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


unit TestStringPrep;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, odf_types;

type

  { TStringPrepTest }

  TStringPrepTest = class(TTestCase)
  private
         procedure PrepareTextAux(TestString, Segment1, Segment2: string;
           AElementType: TElementType; NoSpaces: integer);

  published
           procedure TestPrepareText_Spaces;
           procedure TestPrepareText_Tab;
           procedure TestPrepareText_VerticalTab;
  end;

implementation

const
     cTab = #9;
     cVerticalTab = #11;
     cSeg1 = 'abcd';
     cSeg2 = 'efgh';

     //Space
     cLine1 = cSeg1 + cCharSpace + cSeg2;
     cLine2 = cSeg1 + cCharSpace + cCharSpace + cSeg2;
     cLine3 = cSeg1 + cCharSpace + cCharSpace + cCharSpace + cSeg2;
     cLine4 = cCharSpace + cSeg1;
     cLine5 = cCharSpace + cLine4;
     cLine6 = cCharSpace + cLine5;
     cLine7 = cSeg1 + cCharSpace;
     cLine8 = cLine7 + cCharSpace;
     cLine9 = cLine8 + cCharSpace;
     cLine10 = cCharSpace + cCharSpace;
     cLine11 = cCharSpace + cCharSpace + cCharSpace;

     //Tab
     cLine12 = cSeg1 + cTab + cSeg2;
     cLine13 = cSeg1 + cVerticalTab + cSeg2;


procedure TStringPrepTest.PrepareTextAux(TestString, Segment1, Segment2: string;
                         AElementType: TElementType; NoSpaces: integer);
var
   s1, s2: UTF8String;
   ns: word;
   et: TElementType;
begin
     et:=OdfPrepareString(TestString, s1, s2, ns);
     AssertEquals(Segment1, s1);
     AssertEquals(Segment2, s2);
     if et <> AElementType
     then
         Fail('Unexpected Element type');
     AssertEquals(NoSpaces, ns);
end;

procedure TStringPrepTest.TestPrepareText_Spaces;
begin
     PrepareTextAux(cLine1, cLine1, '', oetNone, 0);
     PrepareTextAux(cLine2, cSeg1 + cCharSpace, cSeg2, oetTextS, 1);
     PrepareTextAux(cLine3, cSeg1 + cCharSpace, cSeg2, oetTextS, 2);
     PrepareTextAux(cLine4, cLine4, '', oetNone, 0);
     PrepareTextAux(cLine5, cCharSpace, cSeg1, oetTextS, 1);
     PrepareTextAux(cLine6, cCharSpace, cSeg1, oetTextS, 2);
     PrepareTextAux(cLine7, cLine7, '', oetNone, 0);
     PrepareTextAux(cLine8, cLine7, '', oetTextS, 1);
     PrepareTextAux(cLine9, cLine7, '', oetTextS, 2);
     PrepareTextAux(cLine10, cCharSpace, '', oetTextS, 1);
     PrepareTextAux(cLine11, cCharSpace, '', oetTextS, 2);
end;

procedure TStringPrepTest.TestPrepareText_Tab;
begin
     PrepareTextAux(cLine12, cSeg1, cSeg2, oetTextTab, 0);
end;

procedure TStringPrepTest.TestPrepareText_VerticalTab;
begin
     PrepareTextAux(cLine13, cSeg1, cSeg2, oetTextLineBreak, 0);
end;

initialization

  RegisterTest(TStringPrepTest);
end.


