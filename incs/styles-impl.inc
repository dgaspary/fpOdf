function OdfGetStyleFamilyValue(AFamily: TStyleFamilyValue): string;
begin
     result:=StyleFamilyValues[AFamily];
end;

function OdfGetStyleFamilyValueFromString(AFamily: string): TStyleFamilyValue;
var
   f: TStyleFamilyValue;
   s: string;
begin
     result:=sfvNone;

     s:=LowerCase(AFamily);

     for f in TStyleFamilyValue do
         if s = StyleFamilyValues[f]
         then
         begin
              result:=f;
              break;
         end;
end;

function OdfGetFontStyleValue(AFontStyle: TOdfFontStyle): string;
begin
     result:=OdfFontStyleValues[AFontStyle];
end;

function OdfGetFontStyleFromString(AFontStyle: string): TOdfFontStyle;
var
   fs: TOdfFontStyle;
begin
     result:=ofsNone;
     AFontStyle:=LowerCase(AFontStyle);
     for fs in TOdfFontStyle do
         if AFontStyle = OdfFontStyleValues[fs]
         then
         begin
              result:=fs;
              break;
         end;
end;

procedure OdfSetFontStyleAttribute(AElement: TDOMElement;
  FontStyle: TOdfFontStyle);
const
     fsAtts : array[1..3] of TAttributeType = (oatFoFontStyle,
                    oatStyleFontStyleAsian,
                    oatStyleFontStyleComplex);
begin
     TOdfElement(AElement).SetOrDeleteAttributes(fsAtts,
         OdfGetFontStyleValue(FontStyle));
end;

procedure OdfDeleteFontStyleAttribute(AElement: TDOMElement);
begin
     OdfSetFontStyleAttribute(AElement, ofsNone);
end;


function OdfGetFontWeightValue(AFontWeight: TOdfFontWeight): string;
begin
     result:=OdfFontWeightValues[AFontWeight];
end;

function OdfGetFontWeightFromString(AFontWeight: string): TOdfFontWeight;
var
   fw: TOdfFontWeight;
begin
     result:=fwNone;
     AFontWeight:=LowerCase(AFontWeight);
     for fw in TOdfFontWeight do
         if AFontWeight = OdfFontWeightValues[fw]
         then
         begin
              result:=fw;
              break;
         end;
end;

procedure OdfSetFontWeightAttribute(AElement: TDOMElement;
  FontWeight: TOdfFontWeight);
const
     fwAtts : array[1..3] of TAttributeType = (oatFoFontWeight,
                    oatStyleFontWeightAsian,
                    oatStyleFontWeightComplex);
begin
     TOdfElement(AElement).SetOrDeleteAttributes(fwAtts,
              OdfGetFontWeightValue(FontWeight));
end;

procedure OdfDeleteFontWeightAttribute(AElement: TDOMElement);
begin
     OdfSetFontWeightAttribute(AElement, fwNone);
end;


procedure TOdfCustomStyle.SetStyleFamily(family: TStyleFamilyValue);
begin
     self.OdfStyleFamily:=OdfGetStyleFamilyValue(family);
end;

function TOdfCustomStyle.GetStyleFamily: TStyleFamilyValue;
begin
     result:=OdfGetStyleFamilyValueFromString(self.OdfStyleFamily);
end;
