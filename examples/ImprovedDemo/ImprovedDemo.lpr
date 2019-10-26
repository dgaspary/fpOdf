program ImprovedDemo;

{$R *.res}

uses Interfaces, sysutils, graphics, forms, odf_types;

var FOdfTextDocument:TOdftextDocument;
    FDatapath:String;

const COutout1='ShowStyles.fodt';
      COutout2='ShowLinks.odf';
      COutout3='ShowFonts.odf';

procedure Init;

var
  i: Integer;
begin
  FDatapath:='output';
  for i := 0 to 2 do
     if DirectoryExists(FDatapath) then
       break
     else
       FDatapath:='..'+DirectorySeparator+FDatapath;
  FOdfTextDocument := TOdfTextDocument.Create;
end;

procedure Execute;

const  cStyleName = 'Standard';
      cTextStyles :array[TFontStyle] of string =
        ('bold','italic','underlined','striked out');
var
  lPara: TOdfParagraph;
  lFS: TFontStyles;
  lFsi: TFontStyle;
  lText, lFont: String;
  i: Integer;
  aFont: TFont;
  lSpan: TSpan;

begin


  FOdfTextDocument.Clear;
  lPara := FOdfTextDocument.AddParagraph(cStyleName);
  lpara.AddSpan('Here (bold)',[fsBold]);
  lpara.AppendOdfElement(oetTextLineBreak);
  lpara.AddSpan('are (italic)',[fsItalic]);
  lPara := FOdfTextDocument.AddParagraph(cStyleName);
  lpara.AddSpan('some (underline)',[fsUnderline]);
  lpara.AppendOdfElement(oetTextLineBreak);
  lpara.AddSpan('Styles (Strikeout)',[fsStrikeOut]);
  FOdfTextDocument.SaveToSingleXml(FDatapath+DirectorySeparator+COutout1);
  FOdfTextDocument.Clear;


  for i := 0 to 15 do
    begin
      lFS:=[];
      lText := '';
      for lFsi in TFontStyle do
        if (i and (1 shl ord(lFsi))) <>0 then
          begin
            lfs += [lfsi];
            if ltext='' then
              lText := cTextStyles[lfsi]
            else
              if ltext.IndexOf(' and ')>=0 then
                ltext :=cTextStyles[lfsi]+', '+lText
              else
                ltext :=cTextStyles[lfsi]+' and '+lText
          end;
      if ltext='' then
        lText := 'This text is normal.'
      else
        lText := 'This text is '+lText+'.';
      lPara := FOdfTextDocument.AddParagraph(cStyleName);
      lPara.AddBookmark(lText,lFS,'F'+inttostr(i));
    end;

  for i := 0 to 15 do
    begin
      lPara := FOdfTextDocument.AddParagraph(cStyleName);
      lPara.AddLink('Go to '+Inttostr(i),[],'F'+inttostr(i))
    end;

  FOdfTextDocument.SaveToZipFile(FDatapath+DirectorySeparator+COutout2);
  FOdfTextDocument.Clear;

  aFont := TFont.Create;
  try
  for lFont in Screen.Fonts do
     begin
       lPara := FOdfTextDocument.AddParagraph(cStyleName);
       aFont.Name:=lFont;
       lpara.AddSpan('This is Font: "'+lFont+'"',[]);
       lPara.AppendOdfElement(oetTextLineBreak);
       lSpan:= lpara.AddSpan('ABCDEF abcdef 12345',aFont,FOdfTextDocument);
       lSpan.AppendOdfElement(oetTextLineBreak);
       lSpan.AppendText('The quick, brown fox jumps over the lazy dog.');
       lSpan.AppendOdfElement(oetTextLineBreak);
     end;
  finally
    freeandnil(aFont)
  end;
  FOdfTextDocument.SaveToZipFile(FDatapath+DirectorySeparator+COutout3);
end;

Procedure Done;

begin
  FreeAndNil(FOdfTextDocument);
end;

begin
  Init;
  Execute;
  Done;
end.

