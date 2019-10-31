{ fpOdf "Search And Replace" Example

  Copyright (c) 2013-2019 Daniel F.Gaspary

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

program OdfSearchAndReplace;

uses
    odf_types;
const
     cStyle = 'Standard';
     cUndefined = '<undefined>';
     cName = 'Name';
     cAge = 'Age';

     { TODO : Windows Path }
     cOutputDir = '/tmp/';

     cOutputFile = cOutputDir + 'ReplaceFieldsExample.fodt';

var
   doc: TOdfTextDocument;
   p: TOdfParagraph;

   e: TOdfElement;

procedure AddField(AFieldName, ADescription: string);
var
   e: TOdfElement;
begin
     p:=doc.AddParagraph(cStyle);
     p.AddSpan(AFieldName + ':', cStyle);
     p.AppendText(#9);
     e:=p.AddTextInput(ADescription);
     TOdfContent(e).TextContent:=cUndefined;
end;

procedure ReplaceValues(AName, AnAge: string);
var
   vResults: TOdfNodeSet;
   p: Pointer;
   AttValue: string;

begin
     if doc.XPathSearch('//text:text-input', doc.XmlDocument.DocumentElement, [],
        vResults)
     then
     begin
         for p in vResults do
         begin
              with TOdfContent(p) do
              begin
                   AttValue:=GetAttributeString(oatTextDescription);

                   if AttValue=cName
                   then
                       TextContent:=AName
                   else if AttValue=cAge
                   then
                       TextContent:=AnAge;
              end;
         end;

         vResults.Free;
     end;
end;

begin
     doc:=TOdfTextDocument.Create;
     AddField('Name', cName);
     AddField('Age', cAge);

     ReplaceValues('Arthur', '35');

     try
        doc.SaveToSingleXml(cOutputFile);
     finally
            doc.Free;
     end;

end.

