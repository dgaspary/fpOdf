{ fpOdf "Hello World" Example

  Copyright (c) 2013 Daniel F.Gaspary

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

program OdtHelloWorld;

{$mode objfpc}{$H+}

uses
    odf_types;

const
     cOutput = '/tmp/HelloWorld.odt';
     cStyleName = 'Standard';

var
   doc: TOdfTextDocument;
   vStyle: TOdfStyleStyle;
begin
     doc:=TOdfTextDocument.Create;

     //Styles
     vStyle:=doc.CreateStyle(cStyleName, sfvParagraph);
     with vStyle do
     begin
          OdfStyleDisplayName:=OdfStyleName;
          AppendOdfElement(oetStyleTextProperties, oatFoFontSize, '12.00pt');
     end;
     doc.Styles.AppendChild(vStyle);

     //Automatic Styles (Not automatic yet :)
     vStyle:=doc.CreateStyle('P1', vStyle);
     vStyle.AppendOdfElement(oetStyleTextProperties).
             SetAttributes([oatFoColor, oatFoFontFamily],
                           ['#000000', 'Sans Serif']);
     doc.AutomaticStyles.AppendChild(vStyle);

     //Paragraph
     doc.AddParagraph(cStyleName).TextContent:='Hello World!';

     doc.SaveToZipFile(cOutput);
     doc.Free;
end.

