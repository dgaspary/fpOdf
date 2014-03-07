{ SearchTest.pas is part of the fpOdf.

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

unit SearchTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, testdecorator,
  Laz2_DOM, LazUTF8, odf_types;

type

    { TSearchTestSetup }

    TSearchTestSetup = class(TTestSetup)
      procedure OneTimeSetup; override;
      procedure OneTimeTearDown; override;
    end;


  { TSearchTestCase }

  TSearchTestCase = class(TTestCase)
  private
    function GetTextNode: TDOMText;
  protected
    procedure SetUp; override;
  published
    procedure TestSearch;
    procedure TestReplace;
  end;

implementation

const
     cP1 = 'abc';
     cP2 = 'ghi';
     cSearchFor = 'DEF';
     cReplaceWith = '123';

     cOriginal = cP1 + cSearchFor + cP2;
     cReplaced = cP1 + cReplaceWith + cP2;

var
   doc: TOdfTextDocument;
   paragraph: TOdfParagraph;

{ TSearchTestSetup }

procedure TSearchTestSetup.OneTimeSetup;
begin
     doc:=TOdfTextDocument.Create;
     paragraph:=doc.AddParagraph('Standard');
end;

procedure TSearchTestSetup.OneTimeTearDown;
begin
     doc.Free;
end;

procedure TSearchTestCase.SetUp;
begin
     paragraph.TextContent:=cOriginal;
end;

function TSearchTestCase.GetTextNode: TDOMText;
var
   p: TOdfParagraph;
   dt: TDOMText;
begin
     result:=nil;
     if doc.SearchText(cSearchFor, dt, p)
     then
         Result:=dt;
end;

procedure TSearchTestCase.TestSearch;
begin
     AssertEquals(GetTextNode <> nil, true);
end;

procedure TSearchTestCase.TestReplace;
var
   t: TDOMText;
begin
     t:=GetTextNode;

     if Assigned(t)
     then
     begin
          t.TextContent:=UTF8StringReplace(t.TextContent, cSearchFor,
                         cReplaceWith, [rfReplaceAll]);

          AssertEquals(cReplaced, t.TextContent);
     end
     else
         Fail('Text Node not found.');
end;


initialization
  RegisterTestDecorator(TSearchTestSetup, TSearchTestCase);
end.

