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


