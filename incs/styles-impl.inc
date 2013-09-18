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
var
   s: string;
begin
     s:=OdfGetFontStyleValue(FontStyle);

     with TOdfElement(AElement) do
          if s=''
          then
              DeleteAttributes(fsAtts)
          else
              SetAttributes(fsAtts, [s, s, s]);
end;

procedure OdfDeleteFontStyleAttribute(AElement: TDOMElement);
begin
     OdfSetFontStyleAttribute(AElement, ofsNone);
end;

