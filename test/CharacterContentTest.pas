{ CharacterContentTest.pas is part of the fpOdf.

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


unit CharacterContentTest;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, testdecorator, odf_types;

type

    { TCharacterContentTestSetup }

    TCharacterContentTestSetup = class(TTestSetup)
      procedure OneTimeSetup; override;
      procedure OneTimeTearDown; override;
    end;


    { TCharacterContentTest }

    TCharacterContentTest= class(TTestCase)
    published
      procedure TestContent_Recursive;
      procedure TestContent_NonRecursive;
    end;

implementation

const
     c1 = 'abc';
     c2 = 'def';
     c3 = 'ghi';

var
   doc: TOdfTextDocument;
   paragraph: TOdfParagraph;

{ TCharacterContentTestSetup }

procedure TCharacterContentTestSetup.OneTimeSetup;
begin
     doc:=TOdfTextDocument.Create;
     paragraph:=doc.AddParagraph('Standard');
     with paragraph do
     begin
          TextContent:=c1;
          AddSpan(c2, []);
          AppendChild(doc.XmlDocument.CreateTextNode(c3));
     end;
end;

procedure TCharacterContentTestSetup.OneTimeTearDown;
begin
     doc.Free;
end;

procedure TCharacterContentTest.TestContent_Recursive;
begin
     AssertEquals(c1+c2+c3, paragraph.TextContent);
end;

procedure TCharacterContentTest.TestContent_NonRecursive;
begin
     AssertEquals(c1+c3, paragraph.GetCharacterContent(False));
end;

initialization
    RegisterTestDecorator(TCharacterContentTestSetup, TCharacterContentTest);
end.

