{ styles-decl.inc is part of the fpOdf.

  fpOdf is a library used to help users to create and to modify OpenDocument
  Files(ODF)

  Copyright (C) 2013 Daniel F. Gaspary https://github.com/dgaspary

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


//Family
////////
type
    TStyleFamilyValue = (sfvNone, sfvText, sfvParagraph, sfvSection, sfvRuby,
                         sfvTble, sfvTableColumn, sfvTbleRow, sfvTableCell,
                         sfvGraphic, sfvPresentation, sfvDrawingPage, sfvChart);

const
     StyleFamilyValues : array[TStyleFamilyValue] of String =
                        ('', 'text', 'paragraph', 'section', 'ruby',
                         'table', 'table-column', 'table-row', 'table-cell',
                         'graphic', 'presentation','drawing-page', 'chart');

     function OdfGetStyleFamilyValue(AFamily: TStyleFamilyValue): string;
     function OdfGetStyleFamilyValueFromString(AFamily: string): TStyleFamilyValue;

type


    { TODO -oGaspary : Interfaces seems to be the best option to styles properties }

    TOdfCustomStyle = class(TOdfElement)

      procedure SetStyleFamily(family: TStyleFamilyValue);
      function GetStyleFamily: TStyleFamilyValue;

      property OdfStyleFamily: String index oatStyleFamily
                  read GetAttributeString write SetAttribute;
    end;


    TOdfStyleStyle = class(TOdfCustomStyle)
      property OdfStyleAutoUpdate: String
                  index oatStyleAutoUpdate
                  read GetAttributeString
                  write SetAttribute;

      property OdfStyleClass: String
                  index oatStyleClass
                  read GetAttributeString
                  write SetAttribute;

      property OdfStyleDataStyleName: String
                  index oatStyleDataStyleName
                  read GetAttributeString
                  write SetAttribute;

      property OdfStyleDefaultOutlineLevel: String
                  index oatStyleDefaultOutlineLevel
                  read GetAttributeString
                  write SetAttribute;

      property OdfStyleDisplayName: String
                  index oatStyleDisplayName
                  read GetAttributeString
                  write SetAttribute;

      property OdfStyleListLevel: String
                  index oatStyleListLevel
                  read GetAttributeString
                  write SetAttribute;

      property OdfStyleListStyleName: String
                  index oatStyleListStyleName
                  read GetAttributeString
                  write SetAttribute;

      property OdfStyleMasterPageName: String
                  index oatStyleMasterPageName
                  read GetAttributeString
                  write SetAttribute;

      property OdfStyleName: String
                  index oatStyleName
                  read GetAttributeString
                  write SetAttribute;

      property OdfStyleNextStyleName: String
                  index oatStyleNextStyleName
                  read GetAttributeString
                  write SetAttribute;

      property OdfStyleParentStyleName: String
                  index oatStyleParentStyleName
                  read GetAttributeString
                  write SetAttribute;

      property OdfStylePercentageDataStyleName: String
                  index oatStylePercentageDataStyleName
                  read GetAttributeString
                  write SetAttribute;
    end;

    TOdfStyleDefaultStyle = class(TOdfCustomStyle);

    TOdfStyleType = (stUndefined, stCommon, stAutomatic, stMaster);

const
     OdfStyleNameAttributes : array [1..2] of TAttributeType =
         (oatTextStyleName, oatTableStyleName);


     function OdfGetStyleName(AElement: TDOMElement; out ALocalName: String;
                              out AUri: String): boolean;


//Font Style p1-20.184
////////////

type
    TOdfFontStyle = (ofsNone, ofsNormal, ofsItalic, ofsOblique);

const
     OdfFontStyleValues : array[TOdfFontStyle] of String =
                        ('', 'normal', 'italic', 'oblique');

     function OdfGetFontStyleValue(AFontStyle: TOdfFontStyle): string;
     function OdfGetFontStyleFromString(AFontStyle: string): TOdfFontStyle;

     procedure OdfSetFontStyleAttribute(AElement: TDOMElement; FontStyle: TOdfFontStyle);
     procedure OdfDeleteFontStyleAttribute(AElement: TDOMElement);


//Font Weight p1-20.186
/////////////


type
   TOdfFontWeight = (fwNone, fwNormal, fwBold, fw100, fw200, fw300, fw400,
                     fw500, fw600, fw700, fw800, fw900);

const
    OdfFontWeightValues : array[TOdfFontWeight] of String =
                       ('', 'normal', 'bold', '100', '200', '300', '400',
                        '500', '600', '700', '800', '900');

    function OdfGetFontWeightValue(AFontWeight: TOdfFontWeight): string;
    function OdfGetFontWeightFromString(AFontWeight: string): TOdfFontWeight;

    procedure OdfSetFontWeightAttribute(AElement: TDOMElement; FontWeight: TOdfFontWeight);
    procedure OdfDeleteFontWeightAttribute(AElement: TDOMElement);

