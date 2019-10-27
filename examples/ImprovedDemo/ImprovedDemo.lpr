program ImprovedDemo;

{$R *.res}

uses Interfaces, sysutils, graphics, forms, odf_types;

var FOdfTextDocument:TOdftextDocument;
    FDatapath:String;

const COutout1='ShowStyles.fodt';
      COutout2='ShowLinks.odt';
      COutout3='ShowFonts.odt';
      COutout4='ShowColors.fodt';

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
  lFirstChar: string;
  w: ValReal;

begin
  FOdfTextDocument.Clear;
  FOdfTextDocument.AddHeadline(1).AppendText('Demonstration of fpOdf');
  FOdfTextDocument.AddHeadline(2).AppendText('Headlines');
    for i := 3 to 8 do
        FOdfTextDocument.AddHeadline(i).AppendText('Headline ' + IntToStr(i));
  FOdfTextDocument.AddHeadline(2).AppendText('Some Textstyles');
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

  FOdfTextDocument.AddHeadline(1).AppendText('Demonstration of fpOdf');
  FOdfTextDocument.AddHeadline(2).AppendText('All Textstyles');
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
  FOdfTextDocument.AddHeadline(2).AppendText('Hyperlinks ...');
  for i := 0 to 15 do
    begin
      lPara := FOdfTextDocument.AddParagraph(cStyleName);
      lPara.AddLink('Go to '+Inttostr(i),[],'F'+inttostr(i))
    end;
  FOdfTextDocument.SaveToZipFile(FDatapath+DirectorySeparator+COutout2);

  FOdfTextDocument.Clear;
  FOdfTextDocument.AddHeadline(1).AppendText('Demonstration of fpOdf');
  FOdfTextDocument.AddHeadline(2).AppendText('All Fonts');
 lFirstChar := '!';
    aFont := TFont.Create;
      try
        for lFont in Screen.Fonts do
          begin
            if copy(lfont, 1, 1) <> lFirstChar then
              begin
                FOdfTextDocument.AddHeadline(4).AppendText(copy(lfont, 1, 1));
                lFirstChar := copy(lfont, 1, 1);
              end;
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

  FOdfTextDocument.Clear;
  FOdfTextDocument.AddHeadline(1).AppendText('Demonstration of fpOdf');
  FOdfTextDocument.AddHeadline(2).AppendText('Textcolors');
  lText := 'Bring more color into your life, because it brightens your life, and touches your soul.';
   lPara := FOdfTextDocument.AddParagraph(cStyleName);
   aFont := TFont.Create;
   aFont.Name := 'default';
     try
       i := 1;
       while i <= length(lText) do
         begin
           w := i / length(lText) * pi * 2;
           afont.Color :=
               RGBToColor(96 + trunc(cos(w) * 96), 96 + trunc(sin(w - pi / 3) * 96), 96 + trunc(sin(w + 4 * pi / 3) * 96));
           if lText[i] <> 'ü'[1] then
               lpara.AddSpan(lText[i], aFont, FOdfTextDocument)
           else
             begin
               lpara.AddSpan(copy(lText, i, 2), aFont, FOdfTextDocument);
               Inc(i);
             end;
           Inc(i);
         end;
     finally;
       FreeAndNil(aFont)
     end;
   FOdfTextDocument.SaveToSingleXml(FDatapath+DirectorySeparator+COutout4);
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

