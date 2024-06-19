{ ElementFunctionsTests.pas is part of the fpOdf.

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

  Reworked: (C) 2024 Joe Care https://github.com/joecare99
}

unit ElementFunctionsTests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, testdecorator, Laz2_DOM, odf_types, SysUtils;

const
     cPath = 'test';
     cTestFile = 'OlaMundo.fodt';

type

    { TElementFunctionsTestSetup }

    TElementFunctionsTestSetup = class(TTestSetup)
      procedure OneTimeSetup; override;
      procedure OneTimeTearDown; override;
    end;


  { TElementFunctionsTests }

  TElementFunctionsTests = class(TTestCase)
  private
         FDataPath:string;
         Fdoc: TOdfDocument;
         procedure Test_OdfGetElement_aux(Recursive: boolean);
         class procedure Test2_OdfGetElement_aux(Recursive: boolean; aDoc: TOdfDocument);
  protected
         procedure SetUp; override;
         procedure TearDown; override;
  published
           procedure Test_OdfGetElement_Default;
           procedure Test_OdfGetElement_Recursive;
           procedure Test_OdfGetElement_NonRecursive;
           procedure Test2_OdfGetElement_Default;
           procedure Test2_OdfGetElement_Recursive;
           procedure Test2_OdfGetElement_NonRecursive;
  end;

implementation

var
   doc: TOdfDocument;

{ TElementFunctionsTestSetup }

procedure TElementFunctionsTestSetup.OneTimeSetup;
var I:integer;
   DataPath: string;
begin
     DataPath := cPath;
     for I := 0 to 2 do
     if not DirectoryExists(DataPath) then
       DataPath:='..' + DirectorySeparator + DataPath;
     doc:=TOdfDocument.LoadFromSingleXml(DataPath +DirectorySeparator + cTestFile);
end;

procedure TElementFunctionsTestSetup.OneTimeTearDown;
begin
     doc.Free;
end;

{ TElementFunctionsTests }

procedure TElementFunctionsTests.SetUp;
var I: integer;
begin
     FDataPath := cPath;
     for I := 0 to 2 do
     if not DirectoryExists(FDataPath) then
       FDataPath:='..' + DirectorySeparator +FDataPath;
     Fdoc:=TOdfDocument.LoadFromSingleXml(FDataPath +DirectorySeparator + cTestFile);
end;

procedure TElementFunctionsTests.TearDown;
begin
     Fdoc.Free;
end;

procedure TElementFunctionsTests.Test_OdfGetElement_aux(Recursive: boolean);
var
   e: TDOMElement;
begin
     e:=OdfGetElement(oetTextP, doc.XmlDocument.DocumentElement, Recursive);
     AssertEquals(Assigned(e), Recursive);
end;

procedure TElementFunctionsTests.Test_OdfGetElement_Default;
var
   e: TDOMElement;
begin
     e:=OdfGetElement(oetTextP, doc.XmlDocument.DocumentElement);
     AssertEquals(Assigned(e), false);
end;

procedure TElementFunctionsTests.Test_OdfGetElement_Recursive;
begin
     Test_OdfGetElement_aux(true);
end;

procedure TElementFunctionsTests.Test_OdfGetElement_NonRecursive;
begin
     Test_OdfGetElement_aux(false);
end;

class procedure TElementFunctionsTests.Test2_OdfGetElement_aux(
  Recursive: boolean; aDoc: TOdfDocument);
var
   e: TDOMElement;
begin
     e:=OdfGetElement(oetTextP, aDoc.XmlDocument.DocumentElement, Recursive);
     AssertEquals(Assigned(e), Recursive);
end;

procedure TElementFunctionsTests.Test2_OdfGetElement_Default;
var
   e: TDOMElement;
begin
     e:=OdfGetElement(oetTextP, Fdoc.XmlDocument.DocumentElement);
     AssertEquals(Assigned(e), false);
end;

procedure TElementFunctionsTests.Test2_OdfGetElement_Recursive;
begin
     Test2_OdfGetElement_aux(true,FDoc);
end;

procedure TElementFunctionsTests.Test2_OdfGetElement_NonRecursive;
begin
     Test2_OdfGetElement_aux(false,FDoc);
end;

initialization
  RegisterTestDecorator(TElementFunctionsTestSetup, TElementFunctionsTests);
end.

