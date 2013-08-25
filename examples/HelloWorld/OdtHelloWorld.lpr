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

var
   doc: TOdfTextDocument;
begin
     doc:=TOdfTextDocument.Create;

     //Styles
     with TOdfElement(doc.Styles).AppendOdfElement(oetStyleStyle) do
     begin
          SetAttribute(oatStyleName, 'Standard');
          SetAttribute(oatStyleDisplayName, 'Standard');
          SetAttribute(oatStyleFamily, 'paragraph');
          AppendOdfElement(oetStyleTextProperties, oatFoFontSize, '12.00pt');
     end;

     //Font Face Decls
     TFontFaceDecls(doc.FontFaceDecls).AddFontFace('Liberation Serif',
         '''Liberation Serif''', 'roman', 'variable');

     //Automatic Styles (Not automatic yet :)
     with TOdfElement(doc.AutomaticStyles).AppendOdfElement(oetStyleStyle) do
     begin
          SetAttributes([oatStyleName, oatStyleParentStyleName, oatStyleFamily],
                        ['P1', 'Standard', 'paragraph']);

          AppendOdfElement(oetStyleTextProperties).
             SetAttributes([oatFoColor, oatFoFontFamily],
                           ['#000000', 'Sans Serif']);
     end;

     //Paragraph
     doc.AddParagraph('Standard').TextContent:='Hello World!';

     TOdfDocument.SaveToZipFile(doc, cOutput);
     doc.Free;
end.

