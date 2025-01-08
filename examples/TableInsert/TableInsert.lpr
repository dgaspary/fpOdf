{ fpOdf "Table Insert" Example

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

program TableInsert;

uses
    Classes, SysUtils, odf_types;

const
     cStyle = 'Standard';
     cOutputFile = 'ShowTable.fodt';
     cOutPut = 'output';

     cTableStyle = 'Table1';
     cTableColumnStyle = 'Table1.A';
     cTableCellStyle = 'Table1.A';

     cTableCellStyle2 = 'CellStyle2';

     cRowCount = 3;
     cColCount = 3;


var
       FDatapath: string;

var
   doc: TOdfTextDocument;
   p: TOdfParagraph;
   e, vTable, vRow, vCell: TOdfElement;

   s, t: string;

   i, j: integer;

procedure Init;

var
    i: integer;
begin
    if (ParamStr(1) <> '') and DirectoryExists(ParamStr(1)) then
        FDatapath := ParamStr(1)
    else
      begin
        FDatapath := cOutPut;
        for i := 0 to 3 do
            if DirectoryExists(FDatapath) then
                break
            else
                FDatapath := '..' + DirectorySeparator + FDatapath;
      end;
    Randomize;
end;

procedure CreateStyles;
begin
     e:=doc.CreateStyle(cTableStyle, sfvTble);
     doc.AutomaticStyles.AppendChild(e);
     e:=e.AppendOdfElement(oetStyleTableProperties, oatStyleWidth, '6.6931in');
     e.SetAttribute(oatTableAlign, 'margins');

     e:=doc.CreateStyle(cTableColumnStyle, sfvTableColumn);
     doc.AutomaticStyles.AppendChild(e);
     e:=e.AppendOdfElement(oetStyleTableColumnProperties, oatStyleWidth, '3.346in');
     e.SetAttribute(oatStyleRelColumnWidth, '32767*');

     e:=doc.CreateOdfElement(oetStyleDefaultStyle, oatStyleFamily, 'table');
     doc.Styles.AppendChild(e);
     e:=e.AppendOdfElement(oetStyleTableProperties, oatTableBorderModel, 'collapsing');

     e:=doc.CreateOdfElement(oetStyleDefaultStyle, oatStyleFamily, 'table-row');
     doc.Styles.AppendChild(e);
     e:=e.AppendOdfElement(oetStyleTableRowProperties, oatFoKeepTogether, 'auto');

     e:=doc.CreateStyle(cTableCellStyle2, sfvParagraph);
     e.SetAttribute(oatStyleParentStyleName, cStyle);
     doc.Styles.AppendChild(e);
     e:=e.AppendOdfElement(oetStyleTextProperties, oatFoFontWeight, 'bold');
     e.SetAttributes([oatStyleFontWeightAsian, oatStyleFontWeightComplex], ['bold', 'bold']);
end;

begin
     Init();
     doc:=TOdfTextDocument.Create;

     CreateStyles;

     doc.AddParagraph(cStyle).TextContent:='p1';
     p:=doc.AddParagraph(cStyle);

     //Create Table
     vTable:=doc.CreateOdfElement(oetTableTable);
     vTable.SetAttributes([oatTableName, oatTableStyleName], ['Table1', cTableStyle]);

     //Create table column description.
     e:=vTable.AppendOdfElement(oetTableTableColumn, oatTableStyleName, cTableColumnStyle);
     e.SetAttribute(oatTableNumberColumnsRepeated, IntToStr(cColCount));

     //create rows and cells
     for i:=1 to cRowCount do
     begin
          vRow:=vTable.AppendOdfElement(oetTableTableRow);

          for j:=1 to cColCount do
          begin
               vCell:=vRow.AppendOdfElement(oetTableTableCell, oatTableStyleName, cTableCellStyle);
               vCell.SetAttribute(oatOfficeValueType, 'string');

               if i=j
               then
                   s:=cTableCellStyle2
               else
                   s:=cStyle;

               t:=Format('%d%d', [i,j]);
               vCell.AppendOdfElement(oetTextP, oatTextStyleName, s).TextContent:=t;
          end;
     end;

     doc.Text.AppendChild(vTable);

     doc.AddParagraph(cStyle).TextContent:='p2';

     try
        doc.SaveToSingleXml(FDatapath + DirectorySeparator + cOutputFile);

     finally
            doc.Free;
     end;
end.

