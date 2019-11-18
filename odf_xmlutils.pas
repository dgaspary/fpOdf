{ This file is part of the fpOdf.

  fpOdf is a library used to help users to create and to modify OpenDocument
  Files(ODF)

  Copyright (C) 2013-2019 Daniel F. Gaspary https://github.com/dgaspary

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

unit odf_xmlutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM,


  { $Define UseStaxWriter}

  {$ifdef UseStaxWriter}
   StaxWriter_Dom
  {$else}
   laz2_XMLWrite
  {$endif}

  ;


procedure OdfWriteXmlToStream(ADoc: TXMLDocument;  AStream: TStream);
procedure OdfWriteXmlToFile(ADoc: TXMLDocument;  AFilename: string);

function OdfAttributesAsStrings(e: TDOMElement; OnlyNames: boolean = true): TStrings;

implementation

procedure OdfWriteXmlToStream(ADoc: TXMLDocument;  AStream: TStream);
begin
     {$IfDef UseStaxWriter}
            XmlStreamWrite(ADoc, AStream, 'utf-8', '1.0');
     {$Else}
            WriteXMLFile(ADoc, AStream,[xwfPreserveWhiteSpace]);
     {$EndIf}
end;

procedure OdfWriteXmlToFile(ADoc: TXMLDocument;  AFilename: string);
var
   fs: TFileStream;
begin
     try
        fs:=TFileStream.Create(AFilename, fmCreate);
        OdfWriteXmlToStream(ADoc, fs);
     finally
            fs.Free;
     end;
end;

function OdfAttributesAsStrings(e: TDOMElement; OnlyNames: boolean): TStrings;
var
  list: TDOMNamedNodeMap;
  i: integer;
  att: TDOMAttr;
  s: string;
begin
     Result:=TStringList.Create;

     list:=e.Attributes;

     for i:=0 to pred(list.Length) do
     begin
          att:=list[i] as TDOMAttr;
          s:=att.Name;

          if not OnlyNames
          then
              s+='=' + att.Value;

          Result.Add(s);
     end;

     //list.Free; Don't Free it !!.. The List belongs to the element.
end;

end.

