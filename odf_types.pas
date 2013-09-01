{ odf_types.pas is part of the fpOdf.

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

unit odf_types;

{$mode objfpc}

//{$define patched_dom}
//{$define fpodf_debug}

interface

uses
  Classes, SysUtils, FileUtil, zipper, zstream, LazUTF8,

  {$ifdef patched_dom}
   DOM_patched, XMLRead_patched, XMLWrite_patched
  {$else}
   laz2_DOM, laz2_XMLRead, Laz2_XMLWrite
  {$endif},

  xmlutils, odf_mimetypes;

const
     cCharSpace = #32;
     cNone = '<NONE>';
     cFileContent = 'content.xml';
     cFileStyles = 'styles.xml';
     cFileMimetype = 'mimetype';
     cIsoDateFormat = 'yyyy-mm-dd"T"hh:nn:ss'; //Is defined somewhere at fcl?
     cMetaGenerator = 'fpOdf 0.1'; { TODO : Need to change to a proper format,
                                            as specified at p1-4.3.2.1 }

     cUrnOpenDocument = 'urn:oasis:names:tc:opendocument:xmlns:';

     cUrlOasis = 'http://docs.oasis-open.org/';
     cUriOffice12Meta = cUrlOasis + 'ns/office/1.2/meta/';
     cUriOdfMeta =  cUriOffice12Meta + 'odf#'; { TODO : Check why it's not on namespace enum and array }
     cUrlW3 = 'http://www.w3.org/';


//Namespaces
////////////
type

    TOdfNamespace = (onsNone, onsOffice, onsGrddl, onsStyle, onsRdf,
                     onsManifest, onsMeta, onsDc, onsConfig, onsScript,
                     onsPresentation, onsSvg, onsText, onsChart, onsTable,
                     onsForm, onsXforms, onsDraw, onsMath, onsDr3d, onsNumber,
                     onsAnim, onsDb, onsXlink, onsXml, onsFo, onsSmil,
                     onsXhtml, onsPkg, onsOf);

    TOdfNamespaces = set of TOdfNamespace;

    const
         OdfNamespaceDefaultPrefixes: array[TOdfNamespace] of String = (cNone,
                    'office',
                    'grddl',
                    'style',
                    'rdf',
                    'manifest',
                    'meta',
                    'dc',
                    'config',
                    'script',
                    'presentation',
                    'svg',
                    'text',
                    'chart',
                    'table',
                    'form',
                    'xforms',
                    'draw',
                    'math',
                    'dr3d',
                    'number',
                    'anim',
                    'db',
                    'xlink',
                    'xml',
                    'fo',
                    'smil',
                    'xhtml',
                    'pkg',
                    'of');

         OdfNamespaceURIs: array[TOdfNamespace] of String = (cNone,
                    cUrnOpenDocument + 'office:1.0',
                    cUrlW3 + '2003/g/data-view#',
                    cUrnOpenDocument + 'style:1.0',
                    cUrlW3 + '1999/02/22-rdf-syntax-ns#',
                    cUrnOpenDocument + 'manifest:1.0',
                    cUrnOpenDocument + 'meta:1.0',
                    'http://purl.org/dc/elements/1.1/',
                    cUrnOpenDocument + 'config:1.0',
                    cUrnOpenDocument + 'script:1.0',
                    cUrnOpenDocument + 'presentation:1.0',
                    cUrnOpenDocument + 'svg-compatible:1.0',
                    cUrnOpenDocument + 'text:1.0',
                    cUrnOpenDocument + 'chart:1.0',
                    cUrnOpenDocument + 'table:1.0',
                    cUrnOpenDocument + 'form:1.0',
                    cUrlW3 + '2002/xforms',
                    cUrnOpenDocument + 'drawing:1.0',
                    cUrlW3 + '1998/Math/MathML',
                    cUrnOpenDocument + 'dr3d:1.0',
                    cUrnOpenDocument + 'datastyle:1.0',
                    cUrnOpenDocument + 'animation:1.0',
                    cUrnOpenDocument + 'database:1.0',
                    cUrlW3 + '1999/xlink',
                    cUrlW3 + 'XML/1998/namespace',
                    cUrnOpenDocument + 'xsl-fo-compatible:1.0',
                    cUrnOpenDocument + 'smil-compatible:1.0',
                    cUrlW3 + '1999/xhtml',
                    cUriOffice12Meta + 'pkg#',
                    cUrnOpenDocument + 'of:1.2');

         function GetURI(ns: TOdfNamespace): string;


//Elements
//////////

{$INCLUDE incs/ElementEnum.inc}


//    TOdfRootElements = oetOfficeDocumentContent .. oetOfficeDocumentSettings;


    TElementTypeArray = array of TElementType; //Max Length: oetTextSenderEmail

    function OdfGetElementLocalName(et: TElementType): string;
    function OdfGetElementQName(et: TElementType; out uri: string): string; overload;
    function OdfGetElementQName(et: TElementType): string; overload;
    procedure OdfElementGetNsAndName(et: TElementType; out prefix: string;
                                      out localname: string;
                                      out uri: string);

    function OdfGetElementTypeByName(ALocalName, ANsUri: string): TElementType;
    function OdfGetElement(et: TElementType; AParent: TDOMElement): TDOMElement;

    procedure OdfElementSetNamespaceAtt(DestElement: TDOMElement; ns: TOdfNamespace); overload;
    procedure OdfElementSetNamespaceAtt(DestElement: TDOMElement; nsSet: TOdfNamespaces); overload;


//Attributes
////////////

//type
    {$INCLUDE incs/Atts-Enum.inc}

    //TAttributeTypeSet = set of TAttributeType;

    TAttributeTypeArray = array of TAttributeType;


    function OdfGetAttributeLocalName(at: TAttributeType; out uri: string): string; overload;
    function OdfGetAttributeLocalName(at: TAttributeType): string; overload;
    function OdfGetAttributeQName(at: TAttributeType; out uri: string): string; overload;
    function OdfGetAttributeQName(at: TAttributeType): string; overload;
    function OdfGetAttributeTypeByName(const AAttributeName: string): TAttributeType;
    function OdfGetAttribute(at: TAttributeType; AParent: TDOMElement): TDOMAttr;
    function OdfGetAttributeDefaultValue(at: TAttributeType;
               et: TElementType): string;

    function OdfSetAttributeValue(at: TAttributeType; AElement: TDOMElement;
                               AValue: string): TDOMAttr;

    function OdfSetAttributeDefaultValue(at: TAttributeType;
               AElement: TDOMElement): TDOMAttr;

    function OdfGetAttributeNamespace(at: TAttributeType): TOdfNamespace;

    procedure OdfElementGetChildrenAndAtts(et: TElementType;
                                            out Children: TElementTypeArray;
                                            out Atts: TAttributeTypeArray);
    function OdfElementGetChildren(et: TElementType): TElementTypeArray;
    function OdfElementGetAtts(et: TElementType): TAttributeTypeArray;


//Package Files
///////////////
type

    //p1-3.1.3.1 and p3-2.2.1
    TOdfFile = (ofMimetype, ofImage, ofObject, ofManifestRdf, ofManifest,
                ofContent, ofStyles, ofMeta, ofSettings);

    TOdfXmlFiles = ofManifestRdf .. High(TOdfFile);

    const
         OdfXmlFileRoot: array[TOdfXmlFiles] of TElementType =
                              (
                               oetNone,//oetRdfRdf,
                               oetManifestManifest,
                               oetOfficeDocumentContent,
                               oetOfficeDocumentStyles,
                               oetOfficeDocumentMeta,
                               oetOfficeDocumentSettings
                              );

         OdfXmlFilename: array[TOdfXmlFiles] of string =
                         (
                          'manifest.rdf',
                          'META-INF/manifest.xml',
                          'content.xml',
                          'styles.xml',
                          'meta.xml',
                          'settings.xml'
                         );

    function OdfGetXmlFileRoot(f: TOdfXmlFiles): TElementType;

    procedure OdfFileGetDetails(f: TOdfFile; out RootLocalname: string;
                                out RootNsPrefix: string;
                                out RootNsUri: string;
                                out Filename: string;
                                out Children: TElementTypeArray);


type

    { TDomNodeEnumerator }

    TDomNodeEnumerator = class
    private
           FCurrent: TDOMNode;
           FParent: TDOMNode;
    public
          constructor Create(AParent: TDOMNode);
          function MoveNext: Boolean;
          property Current: TDOMNode read FCurrent;
    end;


    { TElementEnumerator }

    TElementEnumerator = class
    private
           FCurrent: TDOMElement;
           FParent: TDOMElement;
    public
          constructor Create(AParentElement: TDOMElement);
          function MoveNext: Boolean;
          property Current: TDOMElement read FCurrent;
    end;


    TOdfElement = class;
    TOdfElementClass = class of TOdfElement;
    TOdfDocument = class;

    { TOdfElement }

    TOdfElement = class(TDOMElement)
    private
           class function GetElementType(AIndex: TElementType): TElementType;

    public
          class function CreateDomElement(AType: TElementType;
                             Doc: TXMLDocument): TDOMElement; overload;
          class function CreateDomElement(AType: TElementType; Doc: TXMLDocument;
                             at: TAttributeType; AttValue: String): TDOMElement; overload;

          class function CreateOdfElement(AType: TElementType;
                                          Doc: TXMLDocument): TOdfElement; overload;
          class function CreateOdfElement(AType: TElementType;
            AClass: TOdfElementClass; Doc: TXMLDocument): TOdfElement; overload;

          class function OdfGetElementType(e: TDOMElement): TElementType;



          class procedure SetAttribute(at: TAttributeType;
                                       AParent: TDOMElement;
                                       AValue: string);

          class procedure SetAttributes(atts: array of TAttributeType;
                                       AParent: TDOMElement;
                                       const Values: array of string);


          function AppendOdfElement(AType: TElementType): TOdfElement;
          function AppendOdfElement(AType: TElementType; at: TAttributeType;
                                       AValue: string): TOdfElement;
          function OdfGetFirstElement: TOdfElement;
          function HasOdfElement(AType: TElementType): boolean;

          function GetAttribute(AType: TAttributeType): TDOMAttr;
          function GetAttributeString(AType: TAttributeType): String;
          function HasAttribute(AType: TAttributeType): boolean;
          function RemoveAttribute(AType: TAttributeType): TDOMAttr;
          procedure SetAttribute(AType: TAttributeType; AValue: string);
          procedure SetAttributes(const atts: array of TAttributeType;
                                  const Values: array of String);

          function GetEnumerator: TDomNodeEnumerator;

          //property OdfElementType: TElementType read GetElementType;
          //class property OdfElementType: TElementType index oetNone read GetElementType;
    end;

    { TFontFaceDecls }

    TFontFaceDecls = class(TOdfElement)

       function AddFontFace(StyleName, SvgFontFamily: string): TOdfElement;
       function AddFontFace(StyleName, SvgFontFamily, FontFamilyGeneric,
                                 FontPitch: string): TOdfElement;
    end;


    { TTextSequenceDecls }

    TTextSequenceDecls = class(TOdfElement)

       function AddSequenceDecl(DisplayOutlineLevel, AName: string): TOdfElement;

    end;


    { TConfigConfigItemSet }

    TConfigConfigItemSet = class(TOdfElement)
    public
          function AddConfigItem(AName, AType, AContent: string): TOdfElement;

          property ConfigName: string index oatConfigName
                    read GetAttributeString write SetAttribute;
    end;

    { TStyleStyle }

    TStyleStyle = class(TOdfElement)
    public
         property StyleName: string index oatStyleName
                    read GetAttributeString write SetAttribute;
         property StyleFamily: string index oatStyleFamily
                    read GetAttributeString write SetAttribute;
         property ParentStyleName: string index oatStyleParentStyleName
                    read GetAttributeString write SetAttribute;
    end;



    { TOdfDocument }

    TOdfDocument = class
    private
           FXmlDocument: TXMLDocument;

           //Document Root Elements (Ref: Table 7)
           { TODO : Are the variables really Needed? Maybe the best is to use
                    an indexed(with TElementType) get method. }
           FMeta: TDOMElement;
           FSettings: TDOMElement;
           FScripts: TDOMElement;
           FFontFaceDecls: TDOMElement;
           FStyles: TDOMElement;
           FAutomaticStyles: TDOMElement;
           FMasterStyles: TDOMElement;

           FBody: TDOMElement;
           FManifest: TDOMElement;
           FManifestRdf: TDOMElement;

           function GetRootChild(et: TElementType): TDOMElement;

           procedure InitXmlDocument; virtual;

           function GetMimeType: TOdfMimetype;
           procedure SetCreationMeta;

           procedure SetMimeType(AValue: TOdfMimetype);

           class function ParseXmlFile(AStream: TStream): TXMLDocument;
           class procedure ReadPackage(ADir: String; AOdf: TOdfDocument);
           class procedure WritePackage(DestFile: String;
                                        AOdf: TOdfDocument;
                                        ATempDir: string = '');
    public
          constructor Create;

          class function ElementOdfClassByType(et: TElementType): TOdfElementClass;

          class function ConvertOdfElement(AClass: TOdfElementClass;
                                       e: TDOMElement): TOdfElement;

          class function LoadFromFile(AFilename: string): TOdfDocument;
          class function LoadFromZipFile(AFilename, TempDir: string): TOdfDocument;
          class function LoadFromSingleXml(AFilename: string): TOdfDocument;

          class procedure SaveToSingleXml(AOdf: TOdfDocument;
                                          AFilename: string); overload;

          function CreateOdfElement(et: TElementType): TOdfElement; overload;
          function CreateOdfElement(et: TElementType; at: TAttributeType;
                                          AttValue: String): TOdfElement; overload;

          procedure SaveToSingleXml(AFilename: string); overload;

          class procedure SaveToZipFile(AOdf: TOdfDocument;
            AFilename: string; ATempDir: string=''); unimplemented;

          procedure SaveToZipFile(AFilename: string); overload; unimplemented;

          property XmlDocument: TXMLDocument read FXmlDocument write FXmlDocument;

          property MimeType: TOdfMimetype read GetMimeType write SetMimeType;
          property Body: TDOMElement read FBody write FBody;


          property Meta: TDOMElement read FMeta write FMeta;
          //property Meta: TDOMElement index oetOfficeMeta read GetRootChild;

          property Settings: TDOMElement read FSettings write FSettings;
          property Scripts: TDOMElement read FScripts write FScripts;
          property FontFaceDecls: TDOMElement read FFontFaceDecls write FFontFaceDecls;
          property Styles: TDOMElement read FStyles write FStyles;
          property AutomaticStyles: TDOMElement read FAutomaticStyles write FAutomaticStyles;
          property MasterStyles: TDOMElement read FMasterStyles write FMasterStyles;

          property Manifest: TDOMElement read FManifest write FManifest;
          property ManifestRdf: TDOMElement read FManifestRdf write FManifestRdf;
    end;

    { TOdfContent }

    TOdfContent = class(TOdfElement)

    private
           function AddTextNode(const AValue: DOMString): TDOMText;
           procedure OdfSetTextContent(const AValue: DOMString);

    public
          //p1-6.1.1
          function GetCharacterContent: string;
          property TextContent read GetTextContent write OdfSetTextContent;
    end;


    { TOdfParagraph }

    TOdfParagraph = class(TOdfContent)

    public
          //property Style: TOdfStyle

          //property OdfElementType: TElementType index oetTextP;
    end;


    { TOdfTextDocument }

    TOdfTextDocument = class(TOdfDocument)
    private
           FText: TDOMElement;
           procedure InitXmlDocument; override;
    public
          constructor Create;

          function AddParagraph(ATextStyleName: String): TOdfParagraph;

          property Text: TDOMElement read FText write FText;
    end;

    TDefaultStyle = class(TOdfElement)

    public
          property StyleFamily: string index oatStyleFamily
                    read GetAttributeString write SetAttribute;
    end;



{$INCLUDE incs/Elements-Proc-Declaration.inc}
{$INCLUDE incs/Atts-Proc-Declaration.inc}

procedure WriteStringToFile(str, AFileName: string);
procedure OdfXmlSetDefaultAtts(doc: TXMLDocument);

function OdfPrepareString(AText: UTF8String; out Segment1: UTF8String;
                          out Segment2: UTF8String;
                          out NoSpaces: word): TElementType;

{$INCLUDE incs/styles-decl.inc}


implementation

{$INCLUDE incs/proc.inc}
{$INCLUDE incs/Atts-Proc-Implemetation.inc}

procedure NotYetImplemented(FunctionName: string);
begin
     raise Exception.Create(FunctionName + ' not yet implemented');
end;

procedure OdfXmlSetDefaultAtts(doc: TXMLDocument);
begin
     OdfSetAttributeDefaultValue(oatGrddlTransformation, doc.DocumentElement);
     OdfSetAttributeDefaultValue(oatOfficeVersion, doc.DocumentElement);
end;

procedure WriteStringToFile(str, AFileName: string);
var
   f: TextFile;
begin
     AssignFile(f, AFileName);
     try
        Rewrite(f);
        Write(f, str);
     finally
            CloseFile(f);
     end;
end;

function OdfPrepareString(AText: UTF8String; out Segment1: UTF8String;
                          out Segment2: UTF8String;
                          out NoSpaces: word): TElementType;
var
   s: UTF8String;
   SpaceFound: boolean;
   i, index, vTextLength: PtrInt;

   function GetSegment2: string;
   begin
        if NoSpaces>0
        then
            result:=UTF8Copy(AText, i, vTextLength)
        else
            result:=UTF8Copy(AText, i + 1, vTextLength - i);
   end;

begin
     result:=oetNone;
     SpaceFound:=false;
     Segment1:='';
     Segment2:='';

     NoSpaces:=0;

     vTextLength:=UTF8Length(AText);

     for i:=1 to vTextLength do
     begin
          s:=UTF8Copy(AText, i, 1);

          if (s<>cCharSpace) and (NoSpaces>0)
          then
          begin
               Segment2:=GetSegment2;
               result:=oetTextS;
               break;
          end;

          case s of
               #9         : result:=oetTextTab;
               #11{?}, #13: result:=oetTextLineBreak; //????
               cCharSpace : begin
                                 if SpaceFound
                                 then
                                     Inc(NoSpaces)
                                 else
                                     SpaceFound:=true;
                            end;
          end;

          if result = oetNone
          then
          begin
               if NoSpaces=0
               then
                   Segment1+=s;
          end
          else
          begin
               Segment2:=GetSegment2;
               break;
          end;
     end;

     if NoSpaces > 0
     then
         result:=oetTextS;
end;

procedure ReorderElements(AOdf: TOdfDocument);
begin
     with AOdf, AOdf.XmlDocument.DocumentElement do
     begin
          while AOdf.XmlDocument.DocumentElement.ChildNodes.Count>0 do
                RemoveChild(FirstChild);
     end;

     with AOdf, AOdf.XmlDocument.DocumentElement do
     begin
          AppendChild(Meta);
          AppendChild(Settings);
          AppendChild(Scripts);
          AppendChild(FontFaceDecls);
          AppendChild(Styles);
          AppendChild(AutomaticStyles);
          AppendChild(MasterStyles);
          AppendChild(Body);
     end;
end;

{ TOdfContent }

function TOdfContent.AddTextNode(const AValue: DOMString): TDOMText;
begin
     result:=self.OwnerDocument.CreateTextNode(AValue);
     self.AppendChild(result);
end;

procedure TOdfContent.OdfSetTextContent(const AValue: DOMString);
var
   et: TElementType;
   s, s1, s2: UTF8String;
   vSpaces: word;

   vChild: TOdfElement;
begin
     inherited SetTextContent('');

     s:=AValue;
     repeat
           et:=OdfPrepareString(s, s1, s2, vSpaces);

           if s1 <> ''
           then
               AddTextNode(s1);

           if et <> oetNone
           then
           begin
                vChild:=AppendOdfElement(et);

                //No attribute = single space  ( p1-19.763 )
                if (et = oetTextS) and (vSpaces>1)
                then
                    vChild.SetAttribute(oatTextC, IntToStr(vSpaces));
           end;

           s:=s2;

     until (et = oetNone) and (s='');
end;

function TOdfContent.GetCharacterContent: string;
begin
     NotYetImplemented('TOdfContent.GetCharacterContent');
end;

{ TConfigConfigItemSet }

function TConfigConfigItemSet.AddConfigItem(AName, AType, AContent: string
  ): TOdfElement;
begin
     result:=self.AppendOdfElement(oetConfigConfigItem);
     result.SetAttributes([oatConfigName, oatConfigType], [AName, AType]);
     result.TextContent:=AContent;
end;

{ TTextSequenceDecls }

function TTextSequenceDecls.AddSequenceDecl(DisplayOutlineLevel, AName: string
  ): TOdfElement;
begin
     result:=self.AppendOdfElement(oetTextSequenceDecl);
     result.SetAttributes([oatTextDisplayOutlineLevel, oatTextName],
        [DisplayOutlineLevel, AName]);
end;

{ TFontFaceDecls }

function TFontFaceDecls.AddFontFace(StyleName, SvgFontFamily: string): TOdfElement;
begin
     result:=TOdfElement.CreateOdfElement(oetStyleFontFace, self.OwnerDocument as TXMLDocument);

     with TOdfElement(result) do
     begin
          SetAttributes([oatStyleName, oatSvgFontFamily],
                           [StyleName, SvgFontFamily]);
     end;

    self.AppendChild(result);
end;

function TFontFaceDecls.AddFontFace(StyleName, SvgFontFamily, FontFamilyGeneric,
  FontPitch: string): TOdfElement;
begin
     result:=AddFontFace(StyleName, SvgFontFamily);
     TOdfElement(result).SetAttributes(
         [oatStyleFontFamilyGeneric, oatStyleFontPitch],
         [FontFamilyGeneric, FontPitch]);
end;

{ TOdfTextDocument }

procedure TOdfTextDocument.InitXmlDocument;
begin
     inherited InitXmlDocument;

     FText:=TOdfElement(FBody).AppendOdfElement(oetOfficeText);

     MimeType:=omtText;
end;

constructor TOdfTextDocument.Create;
begin
     inherited Create;
end;

function TOdfTextDocument.AddParagraph(ATextStyleName: String): TOdfParagraph;
begin
     result:=TOdfParagraph(CreateOdfElement(oetTextP));
     result.SetAttribute(oatTextStyleName, ATextStyleName);
     FText.AppendChild(result);
end;

{ TDomNodeEnumerator }

constructor TDomNodeEnumerator.Create(AParent: TDOMNode);
begin
     FCurrent:=nil;
     FParent:=AParent;
end;

function TDomNodeEnumerator.MoveNext: Boolean;
begin
     if Assigned(FCurrent)
     then
         FCurrent := FCurrent.NextSibling
     else
         FCurrent:=FParent.FirstChild;

     Result := Assigned(FCurrent);
end;

{ TOdfElement }

//A "reflexive" function, to allow descendants specify type by property index
class function TOdfElement.GetElementType(AIndex: TElementType): TElementType;
begin
     Result:=AIndex;
end;

class function TOdfElement.CreateDomElement(AType: TElementType;
                                         Doc: TXMLDocument): TDOMElement;
var
   qname, uri: string;
begin
     qname:=OdfGetElementQName(AType, uri);

     result:=doc.CreateElementNS(uri, qname);
end;

class function TOdfElement.CreateDomElement(AType: TElementType;
  Doc: TXMLDocument; at: TAttributeType; AttValue: String): TDOMElement;
begin
     result:=TOdfElement.CreateDomElement(AType, Doc);
     SetAttribute(at, result, AttValue);
end;

class function TOdfElement.CreateOdfElement(AType: TElementType;
                                            AClass: TOdfElementClass;
                                            Doc: TXMLDocument): TOdfElement;
var
   vQname, vUri: string;
begin
     vQname:=OdfGetElementQName(AType, vUri);

     {$ifdef patched_dom}
     result:=(doc.CreateElementNS(vUri, vQname, AClass) as aclass);
     {$IFDEF fpodf_debug}WriteLn(result.ClassName);{$ENDIF}
     {$else}
     result:=TOdfElement(doc.CreateElementNS(vUri, vQname));
     {$endif}
end;


class function TOdfElement.CreateOdfElement(AType: TElementType;
                                            Doc: TXMLDocument): TOdfElement;
begin
     result:=CreateOdfElement(AType, TOdfElement, Doc);
end;

class procedure TOdfElement.SetAttribute(at: TAttributeType;
                                         AParent: TDOMElement;
                                         AValue: string);
begin
     OdfSetAttributeValue(at, AParent, Avalue);
end;

class procedure TOdfElement.SetAttributes(atts: array of TAttributeType;
                                       AParent: TDOMElement;
                                       const Values: array of string);

var
   at: TAttributeType;
   i: integer;
   v: string;
begin
     if Length(atts)<>Length(Values)
     then
         raise Exception.Create('Different lengths between Attributes names and values.');

     i:=0;
     for at in atts do
     begin
          v:=Values[i];
          TOdfElement.SetAttribute(at, AParent, v);
          Inc(i);
     end;
end;

function TOdfElement.AppendOdfElement(AType: TElementType): TOdfElement;
begin
     result:=CreateOdfElement(AType, self.OwnerDocument as TXMLDocument);
     self.AppendChild(result);
end;

function TOdfElement.AppendOdfElement(AType: TElementType; at: TAttributeType;
  AValue: string): TOdfElement;
begin
     result:=AppendOdfElement(AType);
     result.SetAttribute(at, AValue);
end;

function TOdfElement.OdfGetFirstElement: TOdfElement;
var
   n: TDOMNode;
begin
     for n in self do
         if n is TDOMElement
         then
         begin
              result:=(n as TOdfElement); { TODO -oGaspary : Need Fix. Will rise Exception when find a element that is not a TOdfElement }
              break;
         end;
end;

function TOdfElement.HasOdfElement(AType: TElementType): boolean;

var
   vPrefix, vLocalname, vUri: string;
   n: TDOMNode;
begin
     OdfElementGetNsAndName(AType, vPrefix, vLocalname, vUri);

     for n in self do
         if (n is TDOMElement)
         then
             with (n as TDOMElement) do
             begin
                  result:=(NamespaceURI = vUri) and (LocalName = vLocalname);

                  if Result
                  then
                      break;
             end;
end;

function TOdfElement.GetAttribute(AType: TAttributeType): TDOMAttr;
var
   uri, vLocalName: string;
begin
     vLocalName:=OdfGetAttributeLocalName(AType, uri);
     result:=GetAttributeNodeNS(uri, vLocalName);
end;

function TOdfElement.GetAttributeString(AType: TAttributeType): String;
var
   vAttr: TDOMAttr;
begin
     result:='';
     vAttr:=GetAttribute(AType);

     if Assigned(vAttr)
     then
         result:=vAttr.Value;
end;

function TOdfElement.HasAttribute(AType: TAttributeType): boolean;
var
   uri, vLocalName: string;
begin
     vLocalName:=OdfGetAttributeLocalName(AType, uri);
     result:=hasAttributeNS(uri, vLocalName);
end;

function TOdfElement.RemoveAttribute(AType: TAttributeType): TDOMAttr;
begin
     result:=RemoveAttributeNode(GetAttribute(AType));
end;

procedure TOdfElement.SetAttribute(AType: TAttributeType; AValue: string);
begin
     SetAttribute(AType, self, AValue);
end;

procedure TOdfElement.SetAttributes(const atts: array of TAttributeType;
  const Values: array of String);
begin
     SetAttributes(atts, self, Values);
end;

class function TOdfElement.OdfGetElementType(e: TDOMElement): TElementType;
var
   vLocal, vUri: string;
begin
     vUri:=e.NamespaceURI;
     vLocal:=e.LocalName;

     result:=OdfGetElementTypeByName(vLocal, vUri);
end;

function TOdfElement.GetEnumerator: TDomNodeEnumerator;
begin
     result:=TDomNodeEnumerator.Create(self);
end;

{ TElementEnumerator }

constructor TElementEnumerator.Create(AParentElement: TDOMElement);
begin
     FCurrent:=nil;
     FParent:=AParentElement;
end;

function TElementEnumerator.MoveNext: Boolean;
begin
     if FCurrent = nil
     then
         FCurrent:=(FParent.FirstChild as TDOMElement)
     else
         FCurrent := (FCurrent.NextSibling as TDOMElement);

     Result := FCurrent <> nil;
end;


class function TOdfDocument.ParseXmlFile(AStream: TStream): TXMLDocument;
var
   Parser: TDOMParser;
   Src: TXMLInputSource;
begin
     try
        Src:=TXMLInputSource.Create(AStream);

        Parser := TDOMParser.Create;
        Parser.Options.Namespaces:=True;
        Parser.Options.IgnoreComments:=True;

        Parser.Parse(Src, result);
     finally
         Src.Free;
         Parser.Free;
     end;
end;

function TOdfDocument.GetMimeType: TOdfMimetype;
var
   s: string;
begin
     s:=XmlDocument.DocumentElement.GetAttributeNS(GetURI(onsOffice), 'mimetype');
     result:=OdfGetMimeTypeByName(s);
end;

class function TOdfDocument.LoadFromFile(AFilename: string): TOdfDocument;
var
   s: string;
begin
     s:=ExtractFileExt(AFilename);
     s:=UpperCase(s);

     case s of
          '.ODT' : result:=LoadFromZipFile(AFilename, SysUtils.GetTempFileName);
          { TODO -oGaspary : Other Extensions }
          else
              result:=LoadFromSingleXml(AFilename);
     end;

end;

function GetChildElementByName(ANsURI, ALocalName: String; AParent: TDOMElement): TDOMElement;
begin
     result:=nil;
     with AParent.GetElementsByTagNameNS(ANsURI, ALocalName) do
     begin
          try
             if Count >= 1
             then
             begin
                  result:=TDomElement(Item[0]);
             end;
          finally
                 free;
          end;
     end;
end;


function MoveElement(ANsUri, ALocalName: string; OldParent, NewParent: TDOMElement): TDOMElement;
var
   e: TDOMElement;
begin
     e:=GetChildElementByName(ANsUri, ALocalName, OldParent);

     result:=(e.CloneNode(true, NewParent.OwnerDocument) as TDOMElement);

     e.Free;

     result:=(NewParent.AppendChild(result) as TDOMElement);
end;

procedure CloneElements(AParent, ANewParent: TDOMElement;
                        ChildrenToClone: TElementTypeArray);
var
   n: TDOMNode;

   function ElementFound: boolean;
   var
      et: TElementType;
      vPrefix, vLocalname, vUri: string;
   begin
        result:=false;
        for et in  ChildrenToClone do
        begin
             OdfElementGetNsAndName(et, vPrefix, vLocalname, vUri);

             with (n as TDOMElement) do
             begin
                  //WriteLn;
                  //WriteLn('Debug: Element - uri='+NamespaceURI + ' local=' + LocalName);
                  //WriteLn('Debug: enum - uri='+vUri + ' local=' + vLocalname);

                  result:=(NamespaceURI = vUri);
                  result:=result and (LocalName=vLocalname);
             end;

             if result
             then
                 break;
        end;
   end;

begin
     n:=AParent.FirstChild;

     while Assigned(n) do
     begin
          if (n is TDOMElement)
          then
              if ElementFound
              then
              begin
                   {$IFDEF fpodf_debug}
                   with (n as TDOMElement) do
                   WriteLn('Element Cloned- uri='+NamespaceURI +
                                ' local=' + LocalName);
                   {$ENDIF}

                   ANewParent.AppendChild(n.CloneNode(true,
                                          ANewParent.OwnerDocument));
              end;

          n:=n.NextSibling;
     end;
end;

function GetMimeTypeFromFile(AFile: string): TOdfMimetype;
var
   s: string;
begin
     {$IFDEF fpodf_debug}WriteLn(AFile);{$ENDIF}

     s:=ReadFileToString(AFile);
     result:=OdfGetMimeTypeByName(s);
end;

procedure MergeChildren(StylesParent, ContentParent: TDOMElement;
             AOdf: TOdfDocument);

  function AttsSameValues(cn, sn: TDOMElement): boolean;
  const
       cName = 'name';
  var
     vUri: string;
     vSn, vCn: string;
  begin
       vUri:=GetURI(onsStyle);
       result:=sn.hasAttributeNS(vUri, cName);

       vSn:=sn.GetAttributeNS(vUri, cName);
       vCn:=cn.GetAttributeNS(vUri, cName);

       result:=result and (vSn = vCn);

       result:=result and (sn.TagName = cn.TagName);
  end;

var
   cn, sn: TDOMElement;

   found: boolean;

   eList: TList;
begin
     if not Assigned(StylesParent)
     then
     begin
          //StylesParent:=AOdf.XmlDocument.CreateElementNS(GetURI(onStyle),
          //       OdfGetElementQName(etStyleName));
     end;

     if not Assigned(ContentParent)
     then
         exit;

     cn:=(ContentParent.FirstChild as TDOMElement);

     eList:=TList.Create;
     while Assigned(cn) do
     begin
          found:=false;
          sn:=(StylesParent.FirstChild as TDOMElement);
          while Assigned(sn) do
          begin
               if AttsSameValues(cn, sn)
               then
               begin
                    found:=true;
                    break;
               end;

               sn:=(sn.NextSibling as TDOMElement);
          end;

          if (not found) and (eList.IndexOf(cn)<0)
          then
          begin
               eList.Add(cn);
          end;

          cn:=(cn.NextSibling as TDOMElement);
     end;

     while eList.Count>0 do
     begin
          cn:=TDOMElement(elist[0]);
          StylesParent.AppendChild(cn);
          eList.Delete(0);
     end;

     eList.Free;
end;

class procedure TOdfDocument.ReadPackage(ADir: String; AOdf: TOdfDocument);
var
   vDoc: TXMLDocument;

   oldParent, newParent,
   ContentFFD: TDomElement;
   ContentAutoStyles: TDomElement;

   vNsURI: string;


   procedure SetOldParent(AFile: string);
   var
      fs: TFileStream;
   begin
        if assigned(vDoc)
        then
            vDoc.Free;

        fs:=TFileStream.Create(ADir + AFile, fmOpenRead);
        try
           vDoc:=ParseXmlFile(fs);

        finally
               fs.Free;
        end;

        oldParent:=vDoc.DocumentElement;
   end;

   function MoveElem(ANsURI, ALocalName: string): TDOMElement;
   begin
        result:=MoveElement(ANsURI, ALocalName, oldParent, newParent);
   end;

begin
     vDoc:=nil;
     ADir:=IncludeTrailingPathDelimiter(ADir);
     with AOdf do
     begin
          XmlDocument:=TXMLDocument.Create;
          newParent:=XmlDocument.CreateElementNS(GetURI(onsOffice), OdfGetElementQName(oetOfficeDocument));
          XmlDocument.AppendChild(newParent);

          MimeType:=GetMimeTypeFromFile(ADir + 'mimetype');

          OdfXmlSetDefaultAtts(XmlDocument);
     end;

     { TODO -oGaspary : All steps bellow could be automatized using an Enum items in a list }

     // content.xml //
     SetOldParent('content.xml');
     vNsURI:=GetURI(onsOffice);

     AOdf.Scripts:=MoveElem(vNsURI, 'scripts');

     ContentFFD:=MoveElem(vNsURI, 'font-face-decls');

     ContentAutoStyles:=MoveElem(vNsURI, 'automatic-styles');
     AOdf.Body:=MoveElem(vNsURI, 'body');
     //vDoc.Free;

     // styles.xml //
     SetOldParent('styles.xml');
     AOdf.FontFaceDecls:=MoveElem(vNsURI, 'font-face-decls');
     MergeChildren(AOdf.FontFaceDecls, ContentFFD, AOdf); { TODO -oGaspary : Need more references about the correct way to merge. }
     if Assigned(ContentFFD)
     then
         ContentFFD.Free;

     AOdf.Styles:=MoveElem(vNsURI, 'styles');

     AOdf.AutomaticStyles:=MoveElem(vNsURI, 'automatic-styles');
     MergeChildren(AOdf.AutomaticStyles, ContentAutoStyles, AOdf);
     if Assigned(ContentAutoStyles)
     then
         ContentAutoStyles.Free;

     AOdf.MasterStyles:=MoveElem(vNsURI, 'master-styles');

     SetOldParent('meta.xml');
     AOdf.Meta:=MoveElem(vNsURI, 'meta');

     SetOldParent('settings.xml');
     AOdf.Settings:=MoveElem(vNsURI, 'settings');

     vDoc.Free;

     DeleteDirectory(ADir, false);
end;

function OdfCreateManifestRdfFile: TXMLDocument;
var
   uriRdf, uriPkg: string;
   document: TDOMElement;
   xml: TXMLDocument;

   procedure AddPart(resource: string);
   var
      part: TDOMElement;
   begin
        part:=xml.CreateElementNS(uriPkg, 'pkg:hasPart');
        part.SetAttributeNS(uriRdf, 'rdf:resource', resource);
        document.AppendChild(part);
   end;

   procedure AddElement(localname, filename: string);
   var
      el: TDOMElement;
   begin
        el:=xml.CreateElementNS(cUriOdfMeta, 'odf:'+localname);
        el.SetAttributeNS(uriRdf, 'rdf:about', filename);
        xml.DocumentElement.AppendChild(el);
   end;

begin
     result:=nil;
     uriRdf:=GetURI(onsRdf);

     xml:=TXMLDocument.Create;
     xml.AppendChild(xml.CreateElementNS(uriRdf, 'rdf:RDF'));

     uriPkg:=GetURI(onsPkg);
     document:=xml.CreateElementNS(uriPkg, 'pkg:Document');
     xml.DocumentElement.AppendChild(document);

     AddPart(cFileContent);
     AddPart(cFileStyles);

     AddElement('ContentFile', cFileContent);
     AddElement('StylesFile', cFileStyles);

     xml.DocumentElement.SetAttributeNS(stduri_xmlns, 'xmlns:odf', cUriOdfMeta);

     result:=xml;
end;

{ TODO -oGaspary : Generate dynamicly, detecting the files that exists. }
function OdfCreateManifestFile: TXMLDocument;
var
   xml: TXMLDocument;

   procedure AddFileEntry(FullPath: string; MediaType: string = ''; version: string = '');
   var
      fe: TDOMElement;
   begin
        fe:=TOdfElement.CreateDomElement(oetManifestFileEntry, xml);
        OdfSetAttributeValue(oatManifestFullPath, fe, FullPath);

        OdfSetAttributeValue(oatManifestMediaType, fe, MediaType);

        if version<>''
        then
            OdfSetAttributeValue(oatManifestVersion, fe, version);

        xml.DocumentElement.AppendChild(fe);
   end;

begin
     result:=nil;

     xml:=TXMLDocument.Create;
     xml.AppendChild(TOdfElement.CreateDomElement(oetManifestManifest,xml));

     { TODO -oGaspary : Get The MimeType as a Constructor parameter }
     AddFileEntry('/', OdfGetMimeTypeName(omtText), '1.2');


     AddFileEntry('content.xml', 'text/xml');
     AddFileEntry('manifest.rdf', 'application/rdf+xml');
     AddFileEntry('styles.xml', 'text/xml');
     AddFileEntry('meta.xml', 'text/xml');
     AddFileEntry('Thumbnails/thumbnail.png', 'image/png');
     AddFileEntry('Thumbnails/');


     { TODO : Configurations2 seems to be an OpenOffice specific directory. }
     AddFileEntry('Configurations2/accelerator/current.xml');
     AddFileEntry('Configurations2/accelerator/');
     AddFileEntry('Configurations2/progressbar/');
     AddFileEntry('Configurations2/floater/');
     AddFileEntry('Configurations2/popupmenu/');
     AddFileEntry('Configurations2/toolpanel/');
     AddFileEntry('Configurations2/menubar/');
     AddFileEntry('Configurations2/toolbar/');
     AddFileEntry('Configurations2/images/Bitmaps/');
     AddFileEntry('Configurations2/images/');
     AddFileEntry('Configurations2/statusbar/');
     AddFileEntry('Configurations2/', 'application/vnd.sun.xml.ui.configuration');
     AddFileEntry('settings.xml', 'text/xml');

     OdfSetAttributeDefaultValue(oatManifestVersion, xml.DocumentElement);

     result:=xml;
end;


class procedure TOdfDocument.WritePackage(DestFile: String;
  AOdf: TOdfDocument; ATempDir: string);
var
   s: string;
   f: TOdfFile;
   z: TZipper;
   zfe: TCollectionItem;

   procedure WriteFileToDisk;
   var
      vDoc: TXMLDocument;
      vRoot: TDOMElement;
      vPrefix, vUri, vLocalname, vFilename: string;

      SubDir: string;

      vChildren: TElementTypeArray;
      nsSet: TOdfNamespaces;
   begin
        OdfFileGetDetails(f, vLocalname, vPrefix, vUri, vFilename, vChildren);

        nsSet:=[];
        case f of
             ofManifestRdf : vDoc:=OdfCreateManifestRdfFile;
             ofManifest    : vDoc:=OdfCreateManifestFile;
             else
             begin
                  vDoc:=TXMLDocument.Create;
                  vRoot:=vDoc.CreateElementNS(vUri, vPrefix + ':' + vLocalname);
                  vDoc.AppendChild(vRoot);

                  { TODO -oGaspary : Need to fix the namespace, some of them
                    are lost on cloning. }

                  {$IFDEF fpodf_debug}
                  WriteLn;
                  WriteLn('===== ' + vFilename + ' Clones =====');
                  {$ENDIF}

                  CloneElements(AOdf.XmlDocument.DocumentElement, vRoot,
                     vChildren);

                  nsSet:=[onsStyle, onsFo, onsSvg];

                  case f of
                       ofContent: nsSet:=nsSet + [onsText];//Include(nsSet, onsText);
                       ofMeta, ofSettings   : nsSet:=[];
                  end;
             end;
        end;

        //Needed to create internal subdirs. Example: META/
        SubDir:=ExtractFileDir(ATempDir +  vFilename);
        ForceDirectoriesUTF8(SubDir);

        if f in [ofContent, ofMeta, ofSettings, ofStyles]
        then
        begin
             OdfSetAttributeDefaultValue(oatOfficeVersion, vDoc.DocumentElement);
             OdfSetAttributeDefaultValue(oatGrddlTransformation, vDoc.DocumentElement);
        end
        else
            vRoot:=vDoc.DocumentElement;

        OdfElementSetNamespaceAtt(vRoot, nsSet);

        WriteXMLFile(vDoc, ATempDir +  vFilename);

        z.Entries.AddFileEntry(ATempDir +  vFilename, vFilename);

        vDoc.Free;
   end;

begin
     if ATempDir=''
     then
         ATempDir:=SysUtils.GetTempFileName;

     ATempDir:=IncludeTrailingPathDelimiter(ATempDir);
     CreateDirUTF8(ATempDir); //Gaspary: Need to test if is possible to
                              //write at temp dir.

     z:=TZipper.Create;
     z.FileName:=DestFile;

     s:=OdfGetMimeTypeName(AOdf.MimeType);

     WriteStringToFile(s, ATempDir + cFileMimetype);

     zfe:=z.Entries.AddFileEntry(ATempDir + cFileMimetype, cFileMimetype);
     (zfe as TZipFileEntry).CompressionLevel:=clnone;

     for f in TOdfXmlFiles do
     begin
          WriteFileToDisk;
     end;

     z.ZipAllFiles;

     for zfe in z.Entries do
       with (zfe as TZipFileEntry) do
       begin
            s:=DiskFileName;

            {$IFDEF fpodf_debug}
            WriteLn;
            WriteLn('Debug - DiskFilename: ' + s);
            WriteLn('Debug - Path: ' + ExtractFilePath(s));
            {$ENDIF}

            if FileExistsUTF8(s)
            then
                DeleteFileUTF8(s);

            if DirectoryExistsUTF8(s)
            then
                RemoveDirUTF8(s);

            s:=ExtractFilePath(s);
            RemoveDirUTF8(s);
       end;
     z.Free;
end;

procedure TOdfDocument.SetCreationMeta;

  procedure SetChildValue(AChild: TElementType; AValue: string);
  var
     e: TDOMElement;
  begin
       e:=OdfGetElement(AChild, FMeta);
       if e = nil
       then
       begin
            e:=TOdfElement.CreateDomElement(AChild, FXmlDocument);
            FMeta.AppendChild(e);
       end;
       e.TextContent:=AValue;
  end;

begin
     if not Assigned(FMeta)
     then
         raise Exception.Create('Element not defined: "office:meta"');

     //Add Namespace Attribute at root to avoid repeating it on children nodes
     //FMeta.SetAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:meta', 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0');
     OdfElementSetNamespaceAtt(FMeta, onsMeta);

     SetChildValue(oetMetaCreationDate, FormatDateTime(cIsoDateFormat, now));
     SetChildValue(oetMetaGenerator, cMetaGenerator);
end;

function TOdfDocument.GetRootChild(et: TElementType): TDOMElement;
begin
     result:=OdfGetElement(et, FXmlDocument.DocumentElement);
end;

procedure TOdfDocument.InitXmlDocument;
var
   e: TOdfElement;
begin
     FXmlDocument:=TXMLDocument.Create;

     e:=TOdfElement.CreateOdfElement(oetOfficeDocument, FXmlDocument);
     FXmlDocument.AppendChild(e);

     OdfXmlSetDefaultAtts(FXmlDocument);

     with e do
     begin
          FMeta:=AppendOdfElement(oetOfficeMeta);
          SetCreationMeta;

          FSettings:=AppendOdfElement(oetOfficeSettings);
          FScripts:=AppendOdfElement(oetOfficeScripts);
          FFontFaceDecls:=AppendOdfElement(oetOfficeFontFaceDecls);
          FStyles:=AppendOdfElement(oetOfficeStyles);
          FAutomaticStyles:=AppendOdfElement(oetOfficeAutomaticStyles);
          FMasterStyles:=AppendOdfElement(oetOfficeMasterStyles);
          FBody:=AppendOdfElement(oetOfficeBody);
     end;
end;

constructor TOdfDocument.Create;
begin
     inherited Create;

     InitXmlDocument;
end;

class function TOdfDocument.ElementOdfClassByType(et: TElementType
  ): TOdfElementClass;
begin
     { TODO -oGaspary : Transform this case to a List and fill it at
           a  "RegisterClass" on initialization }
     result:=TOdfElement;
     case et of
          oetOfficeFontFaceDecls: result:=TFontFaceDecls;
          oetTextSequenceDecls: result:=TTextSequenceDecls;
          oetConfigConfigItemSet: result:=TConfigConfigItemSet;
          oetStyleStyle: result:=TStyleStyle;
     end;
end;

{ TODO -oGaspary : Not working as expected.

Still cannot make Typecasting(result as TheTypeReturned). }
class function TOdfDocument.ConvertOdfElement(AClass: TOdfElementClass;
  e: TDOMElement): TOdfElement;
begin
     result:=nil;

     if AClass = TOdfElement
     then
         result:=TOdfElement(e)
     else if AClass = TFontFaceDecls
          then
              result:=TFontFaceDecls(e)
     else if AClass = TTextSequenceDecls
          then
              result:=TTextSequenceDecls(e)
     else if AClass = TConfigConfigItemSet
          then
              result:=TConfigConfigItemSet(e)
     else if AClass = TStyleStyle
               then
                   result:=TStyleStyle(e);
end;


{function TOdfDocument.CreateElement(AClass: TOdfElementClass): TOdfElement;
var
   vUri, vQName: string;
begin
     OdfGetElementTypeByName
     vQName:=OdfGetElementQName(AClass.OdfElementType(), vUri);
     result:=TOdfElement(FXmlDocument.CreateElementNS(vUri, vQName));
end;}

procedure TOdfDocument.SetMimeType(AValue: TOdfMimetype);
var
   s: string;
begin
     s:=OdfGetMimeTypeName(AValue);
     XmlDocument.DocumentElement.SetAttributeNS(GetURI(onsOffice), 'mimetype', s);
end;

class function TOdfDocument.LoadFromZipFile(AFilename, TempDir: string): TOdfDocument;
var
   z: TUnZipper;
begin
     z:=TUnZipper.Create;

     z.OutputPath:=TempDir;
     z.FileName:=AFilename;
     z.UnZipAllFiles;

     FreeAndNil(z);

     TempDir:=IncludeTrailingPathDelimiter(TempDir);

     result:=TOdfDocument.Create;

     ReadPackage(TempDir, result);

     RemoveDirUTF8(TempDir);
end;

class function TOdfDocument.LoadFromSingleXml(AFilename: string
  ): TOdfDocument;
var
   fs: TFileStream;
begin
     result:=nil;
     fs:=TFileStream.Create(AFilename, fmOpenRead);
     try
        result:=TOdfDocument.Create;
        result.XmlDocument:=ParseXmlFile(fs);
     finally
            fs.Free;
     end;
end;

class procedure TOdfDocument.SaveToSingleXml(AOdf: TOdfDocument;
  AFilename: string);
begin
     ReorderElements(AOdf);
     OdfXmlSetDefaultAtts(AOdf.XmlDocument);
     WriteXMLFile(AOdf.XmlDocument, AFilename);
end;

function TOdfDocument.CreateOdfElement(et: TElementType): TOdfElement;
var
   qname, uri: string;
   e: TDOMElement;
   ElementClass: TOdfElementClass;
begin
     qname:=OdfGetElementQName(et, uri);

     ElementClass:=ElementOdfClassByType(et);

     e:=XmlDocument.CreateElementNS(uri, qname);

     result:=ConvertOdfElement(ElementClass, e);
end;

function TOdfDocument.CreateOdfElement(et: TElementType; at: TAttributeType;
  AttValue: String): TOdfElement;
begin
     result:=CreateOdfElement(et);
     OdfSetAttributeValue(at, result, AttValue);
end;

procedure TOdfDocument.SaveToSingleXml(AFilename: string);
begin
     SaveToSingleXml(self, AFilename);
end;

class procedure TOdfDocument.SaveToZipFile(AOdf: TOdfDocument;
  AFilename: string; ATempDir: string = '');
begin
     WritePackage(AFilename, AOdf, ATempDir);
end;

procedure TOdfDocument.SaveToZipFile(AFilename: string);
begin
     NotYetImplemented('TOdfDocument.SaveToZipFile');
end;


function OdfGetElementLocalName(et: TElementType): string;
var
   ns: TOdfNamespace;
   children: TElementTypeArray;
   atts: TAttributeTypeArray;
begin
     OdfElementGetDetails(et, ns, result, children, atts);
end;

function OdfGetElementQName(et: TElementType; out uri: string): string;
var
   prefix, localname: string;
begin
     OdfElementGetNsAndName(et, prefix, localname, uri);
     result:=prefix + ':' + localname;
end;

function OdfGetElementQName(et: TElementType): string;
var
   uri: string;
begin
     result:=OdfGetElementQName(et, uri);
end;

procedure OdfElementGetNsAndName(et: TElementType; out prefix: string;
                                 out localname: string;
                                 out uri: string);
var
   ns: TOdfNamespace;
   children: TElementTypeArray;
   atts: TAttributeTypeArray;
begin
     OdfElementGetDetails(et, ns, localname, children, atts);
     prefix:=OdfNamespaceDefaultPrefixes[ns];
     uri:=OdfNamespaceURIs[ns];
end;

procedure OdfElementGetChildrenAndAtts(et: TElementType;
                                       out Children: TElementTypeArray;
                                       out Atts: TAttributeTypeArray);
var
   ns: TOdfNamespace;
   s: string;
begin
     OdfElementGetDetails(et, ns, s, children, Atts);
end;

function OdfElementGetChildren(et: TElementType): TElementTypeArray;
var
   atts: TAttributeTypeArray;
begin
     OdfElementGetChildrenAndAtts(et, result, atts);
end;

function OdfElementGetAtts(et: TElementType): TAttributeTypeArray;
var
   children: TElementTypeArray;
begin
     OdfElementGetChildrenAndAtts(et, children, result);
end;

function OdfGetElementTypeByName(ALocalName, ANsUri: string): TElementType;
var
   et: TElementType;
   vPrefix, vLocal, vUri: string;
begin
     result:=oetNone;
     for et in TElementType do
     begin
          OdfElementGetNsAndName(et, vPrefix, vLocal, vUri);
          if (vLocal = ALocalName) and (vUri = ANsUri)
          then
          begin
               result:=et;
               break;
          end;
     end;
end;

function OdfGetElement(et: TElementType; AParent: TDOMElement): TDOMElement;
var
   vUri, vLocalName: string;
   s: string;
begin
     OdfElementGetNsAndName(et, s, vLocalName, vUri);
     result:=GetChildElementByName(vUri, vLocalName, AParent);
end;

procedure OdfElementSetNamespaceAtt(DestElement: TDOMElement; ns: TOdfNamespace
  );
begin
     OdfElementSetNamespaceAtt(DestElement, [ns]);
end;

procedure OdfElementSetNamespaceAtt(DestElement: TDOMElement; nsSet: TOdfNamespaces
  );
var
   ns: TOdfNamespace;
   vPrefix, vUri: string;
begin
     for ns in nsSet do
     begin
          vPrefix:=OdfNamespaceDefaultPrefixes[ns];
          vUri:=OdfNamespaceURIs[ns];
          DestElement.SetAttributeNS(stduri_xmlns, 'xmlns:' + vPrefix, vUri);
     end;
end;

function OdfGetAttributeLocalName(at: TAttributeType; out uri: string): string;
var
   ns: TOdfNamespace;
begin
     OdfAttributeGetDetails(at, ns, result);
     uri:=OdfNamespaceURIs[ns];
end;

function OdfGetAttributeLocalName(at: TAttributeType): string;
var
   uri: string;
begin
     result:=OdfGetAttributeLocalName(at, uri);
end;

function OdfGetAttributeQName(at: TAttributeType; out uri: string): string;
var
   ns: TOdfNamespace;
   local: string;
begin
     OdfAttributeGetDetails(at, ns, local);
     uri:=OdfNamespaceURIs[ns];
     result:=OdfNamespaceDefaultPrefixes[ns] + ':' + local;
end;

function OdfGetAttributeQName(at: TAttributeType): string;
var
   uri: string;
begin
     result:=OdfGetAttributeQName(at, uri);
end;

function OdfGetAttributeTypeByName(const AAttributeName: string): TAttributeType;
var
   at: TAttributeType;
   s: string;
begin
     result:=oatNone;

     s:=UpperCase(AAttributeName);

     { TODO -oGaspary : Need fix. }

{     for at in TAttributeType do
         if UpperCase(AttributeNameArray[at][1]) = s
         then
         begin
              result:=at;
              break;
         end;
}
end;

function OdfGetAttribute(at: TAttributeType; AParent: TDOMElement): TDOMAttr;
var
   vUri: string;
begin
     vUri:=GetURI(OdfGetAttributeNamespace(at));
     result:=AParent.GetAttributeNodeNS(vUri, OdfGetAttributeLocalName(at));
end;

function OdfSetAttributeValue(at: TAttributeType; AElement: TDOMElement;
  AValue: string): TDOMAttr;
var
   vUri,
   vQname: string;

begin
     vUri:=GetURI(OdfGetAttributeNamespace(at));
     vQname:=OdfGetAttributeQName(at);
     AElement.SetAttributeNS(vUri, vQname, AValue);
     result:=AElement.GetAttributeNodeNS(vUri, OdfGetAttributeLocalName(at));
end;

function OdfSetAttributeDefaultValue(at: TAttributeType; AElement: TDOMElement
  ): TDOMAttr;
var
   s: string;
begin
     s:=OdfGetAttributeDefaultValue(at, oetNone); //{ TODO -oGaspary : Need to use a TOdfElement.ElementType }

     result:=OdfSetAttributeValue(at, AElement, s);
end;

function OdfGetAttributeDefaultValue(at: TAttributeType; et: TElementType): string;
begin
     result:='';
     if et<>oetNone
     then
     begin
          //Search by element Context;
          exit;
     end;

     case at of
          oatGrddlTransformation: result:=cUrlOasis + 'office/1.2/xslt/odf2rdf.xsl';
          oatOfficeVersion,
          oatManifestVersion    : result:='1.2';
     end;
end;

function OdfGetAttributeNamespace(at: TAttributeType): TOdfNamespace;
var
   ns: TOdfNamespace;
   local: string;
begin
     OdfAttributeGetDetails(at, ns, local);
     result:=ns;
end;

function OdfGetXmlFileRoot(f: TOdfXmlFiles): TElementType;
begin
     result:=OdfXmlFileRoot[f];
end;


procedure OdfFileGetDetails(f: TOdfFile; out RootLocalname: string; out
  RootNsPrefix: string; out RootNsUri: string; out Filename: string;
  out Children: TElementTypeArray);
var
   RootElement: TElementType;
   ns: TOdfNamespace;
   atts: TAttributeTypeArray;
begin
     RootLocalname:='';
     RootNsPrefix:='';
     RootNsUri:='';
     Filename:='';
     SetLength(Children, 0);
     ns:=onsNone;

     case f of
          ofManifestRdf: begin
                              Filename:='manifest.rdf';
                              RootLocalname:='RDF';
                              RootNsPrefix:=OdfNamespaceDefaultPrefixes[onsRdf];
                              RootNsUri:=GetURI(onsRdf);
                              RootElement:=oetNone;
                         end;
          ofManifest: begin
                           Filename:='META-INF/manifest.xml';
                           RootElement:=oetManifestManifest;
                           ns:=onsManifest;
                      end;
          ofContent: begin
                          Filename:=cFileContent;
                          RootElement:=oetOfficeDocumentContent;
                     end;
          ofStyles: begin
                         Filename:=cFileStyles;
                         RootElement:=oetOfficeDocumentStyles;
                    end;
          ofMeta: begin
                       Filename:='meta.xml';
                       RootElement:=oetOfficeDocumentMeta;
                  end;
          ofSettings: begin
                           Filename:='settings.xml';
                           RootElement:=oetOfficeDocumentSettings;
                      end;

     end;

     if (RootElement<>oetNone) and not(f in[ofMimetype, ofImage, ofObject])
     then
     begin
          OdfElementGetDetails(RootElement, ns, RootLocalname, Children, atts);
          RootNsPrefix:=OdfNamespaceDefaultPrefixes[ns];
          RootNsUri:=GetURI(ns);
     end;

end;

function GetURI(ns: TOdfNamespace): string;
begin
     result:=OdfNamespaceURIs[ns];
end;

end.

