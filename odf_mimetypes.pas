{ This file is part of the fpOdf.

  fpOdf is a library used to help users to create and to modify OpenDocument
  Files(ODF)

  Copyright (C) 2013-2015 Daniel F. Gaspary https://github.com/dgaspary

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


unit odf_mimetypes;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil;

type
    TOdfMimetype = (omtText, omtTextTemplate, omtGraphics, omtGraphicsTemplate,
                    omtPresentation, omtPresentationTemplate, omtSpreadsheet,
                    omtSpreadSheetTemplate, omtChart, omtChartTemplate, omtImage,
                    omtImageTemplate, omtFormula, omtFormulaTemplate,
                    omtTextMaster, omtTextWeb, omtBase);

Const

  cOasisOpenDocument = 'application/vnd.oasis.opendocument.';

  TOdfMimetypes: array[TOdfMimetype] of string = (
                 'text', 'text-template',  'graphics', 'graphics-template',
                 'presentation', 'presentation-template', 'spreadsheet',
                 'spreadsheet-template', 'chart', 'chart-template', 'image',
                 'image-template', 'formula', 'formula-template',
                 'text-master', 'text-web',  'base');

  TOdfMimetypeExtensions: array[TOdfMimetype] of string = ('odt', 'ott', 'odg', 'otg', 'odp', 'otp',
                                                           'ods', 'ots', 'odc', 'otc', 'odi', 'oti',
                                                           'odf', 'otf', 'odm', 'oth', 'odb');

  TOdfMimetypeDescriptions: array[TOdfMimetype] of string = ('Text document', 'Text document used as template',
                            'Graphics document (Drawing)', 'Drawing document used as template',
                            'Presentation document', 'Presentation document used as template',
                            'Spreadsheet document', 'Spreadsheet document used as template',
                            'Chart document', 'Chart document used as template', 'Image document',
                            'Image document used as template', 'Formula document',
                            'Formula document used as template', 'Global Text document. See section 19.806.',
                            'Text document used as template for HTML', 'Database front end document');


function OdfGetMimeTypeName(AType: TOdfMimetype): string;
function OdfGetMimeTypeByName(AName: string): TOdfMimetype;

function OdfGetMimeTypeFromFile(AFile: string): TOdfMimetype;

implementation

function OdfGetMimeTypeName(AType: TOdfMimetype): string;
begin
     result:=cOasisOpenDocument + TOdfMimetypes[AType];
end;

function OdfGetMimeTypeByName(AName: string): TOdfMimetype;
var
   t: TOdfMimetype;
   found: boolean;
begin
     found:=false;
     for t in TOdfMimetype do
       if UpperCase(AName) = UpperCase(OdfGetMimeTypeName(t))
       then
       begin
            found:=true;
            result:=t;
            break;
       end;

     if not found
     then
         raise Exception.Create('Mime type not found. Name: ' +
             AnsiQuotedStr(AName, '"'));
end;

function OdfGetMimeTypeFromFile(AFile: string): TOdfMimetype;
var
   s: string;
begin
     s:=ReadFileToString(AFile);
     result:=OdfGetMimeTypeByName(s);
end;

end.
