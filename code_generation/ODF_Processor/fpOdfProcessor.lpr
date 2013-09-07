{ fpOdfProcessor.pas is part of the fpOdf.

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

program fpOdfProcessor;

{$mode objfpc}{$H+}

//{$DEFINE OnElement_DumpPath}
//{$DEFINE AfterAttribute_DumpPath}

uses
  Classes, sysutils, DOM, XMLWrite,
  RelaxNG_Model, RelaxNG_Processor;

type

    { TDefineDetails }

    TDefineDetails = class
    private
           FName: string;
           FAtts,
           FElements: TStringList;

    public
          constructor create(AName: string);
          destructor Destroy; override;

          function AddAtt(AttName: string): integer;

          property Name: string read FName;
          property Atts: TStringList read FAtts;
          property Elements: TStringList read FElements write FElements;
    end;


    { TElementChildren }

    TElementChildren = class
    private
           ChildElements: TStringList;
           Atts: TStringList;
           FName: String;
           FStats: integer;
    public
          constructor create(AElementName: string);
          destructor Destroy; override;

          function AddAttribute(AName: string): integer;
          procedure AddAttributes(AttList: TStrings);
          function AddElement(AName: string): integer;
          procedure AddElements(Elements: TStrings);

          function GetAtts: TStringList;
          function GetChildrenElements: TStringList;

          procedure IncStats;

          property ElementName: String read FName write FName;
          property Stats: integer read FStats Write FStats;
    end;



    { TEnhancedElementList }

    TEnhancedElementList = class(TStringList)
    private
        function GetOrAddChildren(ElementName: string): TElementChildren;
    public
        constructor Create;

        function Add(const S: string): Integer; override;

        function AddChildElement(ElementName, ChildName: string): integer;
        procedure AddChildrenElements(ElementName: string; children: TStrings);

        function AddAtt(ElementName, AttName: string): integer;
        procedure AddAtts(ElementName: string; AttList: TStrings);

        function GetChildren(item: integer): TElementChildren; overload;
        function GetChildren(ElementName: string): TElementChildren; overload;

        //procedure PrintTopItems(AItemsNo: integer = 10);
    end;


    { TOdfSpecProcessor }

    TOdfSpecProcessor = class(TRelaxNgProcessor)

        ElementList: TEnhancedElementList;

        ProcessedDefines: TStringList;

        procedure OnElement(AProcessor: TRelaxNgProcessor;
                            Element: TRelaxNGElement;
                            var ProcessChildren: boolean = true);

        procedure AfterElement(AProcessor: TRelaxNgProcessor;
                               Element: TRelaxNGElement;
                               ChildrenProcessed: boolean;
                               APathIndex: integer);

        procedure OnDefine(AProcessor: TRelaxNgProcessor;
                           Element: TRelaxNGElement;
                           var ProcessChildren: boolean = true);


        procedure OnReference(AProcessor: TRelaxNgProcessor;
                              Element: TRelaxNGElement;
                              var AProcessChildren: boolean = true);

        procedure AfterReference(AProcessor: TRelaxNgProcessor;
                                 Element: TRelaxNGElement;
                                 ChildrenProcessed: boolean;
                                 APathIndex: integer);


        procedure AfterAttribute(AProcessor: TRelaxNgProcessor;
                                 Element: TRelaxNGElement;
                                 ChildrenProcessed: boolean;
                                 APathIndex: integer);

    private
           procedure DumpPath;
           function FindLastElement(Types: TRelaxNgElementTypes;
                                    SkipIndex: integer = -1): TRelaxNGElement;

           function GetDefineDetails(DefineName: String): TDefineDetails;
    public
          Constructor Create;
          destructor Destroy; override;
          procedure WriteToFile(DestFile: string);
    end;

{ TDefineDetails }

destructor TDefineDetails.Destroy;
begin
     FreeAndNil(FAtts);
     FreeAndNil(FElements);

     inherited Destroy;
end;

constructor TDefineDetails.create(AName: string);
begin
     FName:=AName;
     FAtts:=TStringList.Create;
     FAtts.Sorted:=true;
     FAtts.Duplicates:=dupError;
     FElements:=TStringList.Create;
     FElements.Sorted:=true;
     FElements.Duplicates:=dupIgnore;
end;

function TDefineDetails.AddAtt(AttName: string): integer;
begin
     result:=FAtts.IndexOf(AttName);

     if result<0
     then
         result:=FAtts.Add(AttName);
end;


{ TElementDetails }

constructor TElementChildren.create(AElementName: string);
begin
     FStats:=0;
     ChildElements:=TStringList.Create;
     ChildElements.Duplicates:=dupIgnore;
     ChildElements.Sorted:=true;

     Atts:=TStringList.Create;
     Atts.Duplicates:=dupIgnore;
     Atts.Sorted:=true;

     FName:=AElementName;
end;

destructor TElementChildren.Destroy;
begin
     FreeAndNil(ChildElements);
     FreeAndNil(Atts);
     inherited Destroy;
end;

function TElementChildren.AddAttribute(AName: string): integer;
begin
     result:=atts.IndexOf(AName);

     if result<0
     then
         result:=atts.Add(AName);
end;

procedure TElementChildren.AddAttributes(AttList: TStrings);
begin
     Atts.AddStrings(AttList);
end;

function TElementChildren.AddElement(AName: string): integer;
begin
     result:=ChildElements.IndexOf(AName);

     if result<0
     then
         result:=ChildElements.Add(AName);
end;

procedure TElementChildren.AddElements(Elements: TStrings);
begin
     ChildElements.AddStrings(Elements);
end;

function TElementChildren.GetAtts: TStringList;
begin
     result:=Atts;
end;

function TElementChildren.GetChildrenElements: TStringList;
begin
     result:=ChildElements;
end;

procedure TElementChildren.IncStats;
begin
     Inc(FStats);
end;

{ TEnhancedElementList }

constructor TEnhancedElementList.Create;
begin
     inherited Create;
     Duplicates:=dupIgnore;
end;

function TEnhancedElementList.Add(const S: string): Integer;
var
   i: integer;
begin
     i:=IndexOf(s);

     if i >= 0
     then
     begin
          GetChildren(i).IncStats;
          result:=i;
     end
     else
     begin
          result:=inherited Add(s);
     end;
end;

function TEnhancedElementList.GetOrAddChildren(ElementName: string): TElementChildren;
var
  i: Integer;
begin
     i:=IndexOf(ElementName);

     if i>=0
     then
     begin
          result:=TElementChildren(Objects[i]);
          if not Assigned(result)
          then
          begin
               result:=TElementChildren.create(ElementName);
               Objects[i]:=Result;
          end;
     end
     else
     begin
          result:=TElementChildren.create(ElementName);
          AddObject(ElementName, result);
     end;
end;


function TEnhancedElementList.AddChildElement(ElementName, ChildName: string): integer;
begin
     if ChildName = ''
     then
         exit;
     result:=GetOrAddChildren(ElementName).AddElement(ChildName);
end;

procedure TEnhancedElementList.AddChildrenElements(ElementName: string;
  children: TStrings);
begin
     if (children.Count=0) or (ElementName='')
     then
         exit;

     GetOrAddChildren(ElementName).AddElements(children);
end;

function TEnhancedElementList.AddAtt(ElementName, AttName: string): integer;
begin
     if AttName = ''
     then
         exit;
     result:=GetOrAddChildren(ElementName).AddAttribute(AttName);
end;

procedure TEnhancedElementList.AddAtts(ElementName: string; AttList: TStrings);
begin
     if (AttList.Count=0) or (ElementName='')
     then
         exit;
     GetOrAddChildren(ElementName).AddAttributes(AttList);
end;

function TEnhancedElementList.GetChildren(item: integer): TElementChildren;
begin
     result:=TElementChildren(Objects[item]);
end;

function TEnhancedElementList.GetChildren(ElementName:string): TElementChildren;
begin
     result:=GetChildren(IndexOf(ElementName));
end;

{procedure TEnhancedElementList.PrintTopItems(AItemsNo: integer = 10);
type
    TpItem = ^TStringItem;
var
   vItems: array of TStringItem;
   pItem: TpItem;

   i, j: integer;

   vListCount, vRankingCount: integer;

   procedure SetStats(s: String; c: integer);
   begin
        with pItem^ do
        begin
             FString:=s;
             TStats(FObject).aInteger:=c;
        end;
   end;

   function GetElementCount(StatObj: TObject): integer;
   begin
        result:=TStats(StatObj).aInteger;
   end;

begin
     SetLength(vItems, AItemsNo);

     //Initialize
     for i:=Low(vItems) to High(vItems) do
     begin
          vItems[i].FObject:=TStats.Create;

          pItem:=@vItems[i];
          SetStats('', 0);
     end;

     for i:=0 to Pred(Count) do
     begin
          vListCount:=GetElementCount(Objects[i]);

          for j:=Low(vItems) to High(vItems) do
          begin
               pItem:=@vItems[j];
               vRankingCount:=GetElementCount(pItem^.FObject);

               if vListCount > vRankingCount
               then
               begin
                    SetStats(Get(i), vListCount);
                    break;
               end;
          end;
     end;

     WriteLn;
     WriteLn('== Top Items ==');
     for j:=Low(vItems) to High(vItems) do
     begin
          WriteLn(vItems[j].FString  + ' -> ' + IntToStr(GetElementCount(vItems[j].FObject)));
     end;

end;    }

{ TEventsContainer }

procedure TOdfSpecProcessor.OnElement(
  AProcessor: TRelaxNgProcessor; Element: TRelaxNGElement;
  var ProcessChildren: boolean);
var
   s: string;
begin
     {$IFDEF OnElement_DumpPath}
     DumpPath;
     {$ENDIF}

     s:=TRngNamedElement(Element).ReferenceName;

     if s<>''
     then
     begin
          ElementList.Add(s);


{     if s='office:document-content'
     then
         write('Element found: ' + s);

          if (ElementList.Count mod 10 = 0)
          then
              ElementList.PrintTopItems;
}
     end;
end;

function TOdfSpecProcessor.GetDefineDetails(DefineName: String): TDefineDetails;
var
   i: integer;
begin
     result:=nil;
     i:=ProcessedDefines.IndexOf(DefineName);

     if i>=0
     then
         result:=TDefineDetails(ProcessedDefines.Objects[i]);
end;

procedure TOdfSpecProcessor.DumpPath;
var
   j: integer;
   re: TRelaxNGElement;
   vParentName: String;
begin
     WriteLn('== Path ==');
     for j:=0 to Pred(Path.Count) do
     begin
          re:=Path[j];
          if re.RngType in [retElement. retAttribute, retDefine, retRef]
          then
              vParentName:=TRngNamedElement(re).ReferenceName
          else
              vParentName:='';

          WriteLn(' => ' + re.NodeName);
          WriteLn(' => ' + QuotedStr(vParentName));
          WriteLn;

     end;
end;


procedure TOdfSpecProcessor.AfterElement(
  AProcessor: TRelaxNgProcessor; Element: TRelaxNGElement;
  ChildrenProcessed: boolean; APathIndex: integer);
var
   re: TRelaxNGElement;


   vParentName, vElementName: string;
   dd: TDefineDetails;

begin
     re:=FindLastElement([retElement, retDefine], APathIndex);

     if not assigned(re)
     then
         exit;

     vParentName:=TRngNamedElement(re).ReferenceName;

     //Dicard Nameless Atts and Elements
     //Example: Odf Spec, under '<define name="anyElements">'
     if vParentName=''
     then
         exit;

     vElementName:=TRngNamedElement(Element).ReferenceName;

     if vElementName=''
     then
         exit;

     case re.RngType of
          retElement: begin
                           ElementList.AddChildElement(vParentName, vElementName);
                      end;
          retDefine:  begin
                           dd:=GetDefineDetails(vParentName);
                           dd.Elements.Add(vElementName);
                      end;
     end;
end;

procedure TOdfSpecProcessor.OnDefine(
  AProcessor: TRelaxNgProcessor; Element: TRelaxNGElement;
  var ProcessChildren: boolean);

var
   index: integer;
   vName: string;
   dd: TDefineDetails;
begin
     ProcessChildren:=true;

     vName:=TRngNamedElement(Element).ReferenceName;
     index:=ProcessedDefines.IndexOf(vName);

     if index>=0
     then
     begin
          WriteLn('Define Already Processed: ' + vName);

          ProcessChildren:=false;
          exit;
     end
     else
     begin
          dd:=TDefineDetails.create(vName);

          //ProcessedDefines.Add(Element);
          ProcessedDefines.AddObject(dd.Name,  dd);
     end;

end;

procedure TOdfSpecProcessor.OnReference(AProcessor: TRelaxNgProcessor;
  Element: TRelaxNGElement; var AProcessChildren: boolean);
var
   vDefineName: string;
begin
     vDefineName:=TRngNamedElement(Element).ReferenceName;

     AProcessChildren:=(ProcessedDefines.IndexOf(vDefineName) < 0);
end;

procedure TOdfSpecProcessor.AfterReference(AProcessor: TRelaxNgProcessor;
  Element: TRelaxNGElement; ChildrenProcessed: boolean; APathIndex: integer);
var
  vReferenceName, vParentElement: String;
  dd: TDefineDetails;
  re: TRelaxNGElement;
begin
     vReferenceName:=TRngNamedElement(Element).ReferenceName;

     dd:=GetDefineDetails(vReferenceName);

     if Assigned(dd)
     then
     begin
          re:=FindLastElement([retElement], APathIndex);

         if Assigned(re)
         then
         begin
              vParentElement:=TRngNamedElement(re).ReferenceName;

              ElementList.AddChildrenElements(vParentElement, dd.Elements);
              ElementList.AddAtts(vParentElement, dd.Atts);
         end;
    end;
end;

procedure TOdfSpecProcessor.AfterAttribute(AProcessor: TRelaxNgProcessor;
                                           Element: TRelaxNGElement;
                                           ChildrenProcessed: boolean;
                                           APathIndex: integer);
var
   re: TRelaxNGElement;

   vParentName, vAttName: string;
   dd: TDefineDetails;
begin
     {$IFDEF AfterAttribute_DumpPath}
     DumpPath;
     {$ENDIF}

     re:=FindLastElement([retElement, retDefine]);

     if not assigned(re)
     then
         exit;

     vParentName:=TRngNamedElement(re).ReferenceName;

     //Nameless Atts and Elements are discarded
     //Example: Odf Spec, under '<define name="anyElements">'
     if vParentName=''
     then
         exit;

     vAttName:=TRngNamedElement(Element).ReferenceName;
     if vAttName=''
     then
         exit;

     //debug
     //if vAttName = 'style:family'
     //then
     //    WriteLn('Attribute found: ' + vAttName);

     case re.RngType of
          retElement: begin
                           ElementList.AddAtt(vParentName, vAttName);
                      end;
          retDefine:  begin
                           dd:=GetDefineDetails(vParentName);
                           dd.AddAtt(vAttName);
                      end;
     end;
end;

function TOdfSpecProcessor.FindLastElement(Types: TRelaxNgElementTypes;
  SkipIndex: integer = -1): TRelaxNGElement;
var
  i: integer;
begin
     Result:=nil;
     for i:=Pred(Path.Count) downto 0 do
     begin
          if (SkipIndex=i)
          then
              continue;

          if Path[i].RngType in Types
          then
          begin
               Result:=Path[i];
               break;
          end;
      end;
end;

destructor TOdfSpecProcessor.Destroy;
begin
     ElementList.Free;
     ProcessedDefines.Free;

     inherited Destroy;
end;

constructor TOdfSpecProcessor.Create;
begin
     inherited create;
     ElementList:=TEnhancedElementList.Create;
     ProcessedDefines:=TStringList.Create;

     Register_OnEvent(retDefine, @OnDefine);

     Register_OnEvent(retElement, @OnElement);
     Register_AfterEvent(retElement, @AfterElement);

     Register_AfterEvent(retAttribute, @AfterAttribute);

     Register_OnEvent(retRef, @OnReference);
     Register_AfterEvent(retRef, @AfterReference);
end;

procedure TOdfSpecProcessor.WriteToFile(DestFile: string);
var
   doc: TXMLDocument;
   i: integer;
   s: string;
   ec: TElementChildren;
   slAtts, slElements: TStrings;

   e: TDOMElement;
begin
     doc:=TXMLDocument.Create;
     doc.AppendChild(doc.CreateElement('odf-elements'));

     doc.DocumentElement.SetAttribute('spec-version', '1.2');

     for i:=0 to Pred(ElementList.Count) do
     begin
          s:=ElementList[i];

          if s=''
          then
              continue;

          ec:=ElementList.GetChildren(s);
          if Assigned(ec)
          then
          begin
               slAtts:=ec.GetAtts;
               slElements:=ec.GetChildrenElements;
          end
          else
          begin
               slAtts:=nil;
               slElements:=nil;
          end;

          e:=doc.CreateElement('element');
          e['name']:=s;

          if Assigned(slAtts)
          then
              e['attList']:=slAtts.CommaText;

          if Assigned(slElements)
          then
              e['elementList']:=slElements.CommaText;

          doc.DocumentElement.AppendChild(e);
     end;

     WriteXML(doc, DestFile);
end;

begin
     if Paramcount<>2
     then
     begin
          WriteLn('Use: ' + ExtractFileName(ParamStr(0)) +
                  ' <RelaxNG file> <output file>');
          WriteLn('Example: ' + ExtractFileName(ParamStr(0)) +
                            ' specs-dir/OpenDocument-v1.2-os-schema.rng /tmp/output.xml');
          exit;
     end;

     with TOdfSpecProcessor.Create do
     begin
          WriteLn(' == Loading RelaxNG Model: ' + ExtractFileName(ParamStr(1)) + ' ==');
          LoadModelFromFile(ParamStr(1));

          Execute;
          WriteLn(' == End of processing ==');

          WriteLn(' == Writing to file:' + ExtractFileName(ParamStr(2)) + ' ==');
          WriteToFile(ParamStr(2));

          Free;
     end;

end.

