{ fpOdf "Image Insert" Example

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

program ImageInsert;

uses
    classes, odf_types, base64;

const
     cStyle = 'Standard';

     cInputFile = '/tmp/image1.png';
     cOutputFile = '/tmp/output.fodt';

function EncodeStreamBase64(AInputStream: TStream):String;
var
  Outstream : TStringStream;
  Encoder   : TBase64EncodingStream;
begin
  Outstream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.create(outstream);
    try
      Encoder.CopyFrom(AInputStream, AInputStream.Size);
    finally
      Encoder.Free;
      end;
    Result:=Outstream.DataString;
  finally
    Outstream.free;
  end;
end;

var
   doc: TOdfTextDocument;
   p: TOdfParagraph;
   eDrawFrame, eDrawImage, eBinaryData: TOdfElement;

   fs: TFileStream;
   s: string;
begin
     doc:=TOdfTextDocument.Create;

     doc.AddParagraph(cStyle).TextContent:='p1';
     p:=doc.AddParagraph(cStyle);

     eDrawFrame:=p.AppendOdfElement(oetDrawFrame);

     eDrawFrame.SetAttributes(
       [oatDrawStyleName, oatDrawName, oatTextAnchorType, oatSvgWidth, oatSvgHeight, oatDrawZIndex],
       ['fr1', 'Image1', 'paragraph', '5.80in', '3.6in', '0']);

     eDrawImage:=eDrawFrame.AppendOdfElement(oetDrawImage);

     eBinaryData:=eDrawImage.AppendOdfElement(oetOfficeBinaryData);

     try
        fs:=TFileStream.Create(cInputFile, fmOpenRead);
        s:=EncodeStreamBase64(fs);
     finally
            fs.Free;
     end;

     eBinaryData.TextContent:=s;

     doc.AddParagraph(cStyle).TextContent:='p2';

     try
        doc.SaveToSingleXml(cOutputFile);

     finally
            doc.Free;
     end;
end.

