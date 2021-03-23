program ImprovedDemo;

{$R *.res}

uses
    Interfaces,
    SysUtils,
    Graphics,
    Forms,
    odf_types;

var
    FOdfTextDocument: TOdftextDocument;
    FDatapath: string;

const
    COutout1 = 'ShowStyles.fodt';
    COutout2 = 'ShowLinks.odt';
    COutout3 = 'ShowFonts.odt';
    COutout4 = 'ShowColors.fodt';
    COutout5 = 'ShowTabs.odt';
    COutoutAll = 'AllParts.odt';
    cStyleName = 'Standard';
    COutPut = 'output';

    procedure Init;

    var
        i: integer;
    begin
        if (ParamStr(1) <> '') and DirectoryExists(ParamStr(1)) then
            FDatapath := ParamStr(1)
        else
          begin
            FDatapath := COutPut;
            for i := 0 to 2 do
                if DirectoryExists(FDatapath) then
                    break
                else
                    FDatapath := '..' + DirectorySeparator + FDatapath;
          end;
        Randomize;
        FOdfTextDocument := TOdfTextDocument.Create;
    end;

    procedure HeadingandStyles;
    var
        i: integer;
        lPara: TOdfParagraph;

    begin
        FOdfTextDocument.AddHeadline(2).AppendText('Headlines');
        for i := 3 to 8 do
            FOdfTextDocument.AddHeadline(i).AppendText('Headline ' + IntToStr(i));
        FOdfTextDocument.AddHeadline(2).AppendText('Some Textstyles');
        lPara := FOdfTextDocument.AddParagraph(cStyleName);
        lpara.AddSpan('Here (bold)', [fsBold]);
        lpara.AppendOdfElement(oetTextLineBreak);
        lpara.AddSpan('are (italic)', [fsItalic]);
        lPara := FOdfTextDocument.AddParagraph(cStyleName);
        lpara.AddSpan('some (underline)', [fsUnderline]);
        lpara.AppendOdfElement(oetTextLineBreak);
        lpara.AddSpan('Styles (Strikeout)', [fsStrikeOut]);
    end;

    procedure FontsDisplay;
    var
        lFirstChar: string;
        lSpan: TSpan;
        lFont: string;
        aFont: TOdfFont;
        lPara: TOdfParagraph;

    begin
        FOdfTextDocument.AddHeadline(2).AppendText('All Fonts');
        lFirstChar := '!';
        aFont := TOdfFont.Create;
          try
            for lFont in Screen.Fonts do
              begin
                if copy(lfont, 1, 1) <> lFirstChar then
                  begin
                    FOdfTextDocument.AddHeadline(4).AppendText(copy(lfont, 1, 1));
                    lFirstChar := copy(lfont, 1, 1);
                  end;
                lPara := FOdfTextDocument.AddParagraph(cStyleName);
                aFont.Name := lFont;
                lpara.AddSpan('This is Font: "' + lFont + '"', []);
                lPara.AppendOdfElement(oetTextLineBreak);
                lSpan := lpara.AddSpan('ABCDEF abcdef 12345', aFont, FOdfTextDocument);
                lSpan.AppendOdfElement(oetTextLineBreak);
                lSpan.AppendText('The quick, brown fox jumps over the lazy dog.');
                lSpan.AppendOdfElement(oetTextLineBreak);
              end;
          finally
            FreeAndNil(aFont)
          end;
    end;

    procedure StylesBookmarkAndLinks;

    const
        cTextStyles: array[TFontStyle] of string =
            ('bold', 'italic', 'underlined', 'striked out');

    var
        i: integer;
        lText: string;
        lPara: TOdfParagraph;
        lFsi: TFontStyle;
        lFS: TFontStyles;
    begin
        FOdfTextDocument.AddHeadline(2).AppendText('All Textstyles');
        for i := 0 to 15 do
          begin
            lFS := [];
            lText := '';
            for lFsi in TFontStyle do
                if (i and (1 shl Ord(lFsi))) <> 0 then
                  begin
                    lfs += [lfsi];
                    if ltext = '' then
                        lText := cTextStyles[lfsi]
                    else
                    if ltext.IndexOf(' and ') >= 0 then
                        ltext := cTextStyles[lfsi] + ', ' + lText
                    else
                        ltext := cTextStyles[lfsi] + ' and ' + lText;
                  end;
            if ltext = '' then
                lText := 'This text is normal.'
            else
                lText := 'This text is ' + lText + '.';
            lPara := FOdfTextDocument.AddParagraph(cStyleName);
            lPara.AddBookmark(lText, lFS, 'F' + IntToStr(i));
          end;
        FOdfTextDocument.AddHeadline(2).AppendText('Hyperlinks ...');
        for i := 0 to 15 do
          begin
            lPara := FOdfTextDocument.AddParagraph(cStyleName);
            lPara.AddLink('Go to ', [], 'F' + IntToStr(i)).AddSpan(IntToStr(i),[fsBold]);
          end;
    end;

    procedure TextColors;
    var
        i: integer;
        lText: string;
        lPara: TOdfParagraph;
        aFont: TFont;
        w: ValReal;
    begin
        FOdfTextDocument.AddHeadline(2).AppendText('Textcolors');
        lText :=
            'Bring more color into your life, because it brightens your life, and touches your soul.';
        lPara := FOdfTextDocument.AddParagraph(cStyleName);
        aFont := TFont.Create;
        aFont.Name := 'default';
          try
            i := 1;
            while i <= length(lText) do
              begin
                w := i / length(lText) * pi * 2;
                afont.Color :=
                    RGBToColor(96 + trunc(cos(w) * 96), 96 +
                    trunc(sin(w - pi / 3) * 96), 96 + trunc(sin(w + 4 * pi / 3) * 96));
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
    end;

    procedure TextWithTabulators;


        function RandomText(aMaxLen: integer): string;

        var
            lLen, i: integer;

        begin
            Result := '';
            lLen := trunc(sqrt(Random + 1e-30) * aMaxlen);
            for i := 1 to lLen do
                if Random < 0.5 then
                    Result += 'aeiouy'[1 + Random(6)]
                else
                    Result += char(Ord('a') + Random(26));
        end;

    var
        i, j: integer;
        lPara: TOdfParagraph;

    begin
        FOdfTextDocument.AddHeadline(2).AppendText('Tabulators');
        // A Table-Headline
        lPara := FOdfTextDocument.AddParagraph(cStyleName);
        for i := 0 to 5 do
            if i = 0 then
                lpara.AddSpan('No.', [fsBold])
            else
              begin
                lpara.AddTab([]);
                lpara.AddSpan(RandomText(10), [fsBold]);
              end;
        for j := 1 to 20 do
          begin
            lPara := FOdfTextDocument.AddParagraph(cStyleName);
            for i := 0 to 5 do
                if i = 0 then
                    lpara.AddSpan(IntToStr(j) + '.', [fsItalic])
                else
                  begin
                    lpara.AddTab([]);
                    lpara.AddSpan(RandomText(10), []);
                  end;
          end;
    end;


    procedure Execute;

    begin
        FOdfTextDocument.Clear;
        FOdfTextDocument.AddHeadline(1).AppendText('Demonstration of fpOdf');
        HeadingandStyles;
        FOdfTextDocument.SaveToSingleXml(FDatapath + DirectorySeparator + COutout1);
        FOdfTextDocument.Clear;

        FOdfTextDocument.AddHeadline(1).AppendText('Demonstration of fpOdf');
        StylesBookmarkAndLinks;
        FOdfTextDocument.SaveToZipFile(FDatapath + DirectorySeparator + COutout2);

        FOdfTextDocument.Clear;
        FOdfTextDocument.AddHeadline(1).AppendText('Demonstration of fpOdf');
        FontsDisplay;
        FOdfTextDocument.SaveToZipFile(FDatapath + DirectorySeparator + COutout3);

        FOdfTextDocument.Clear;
        FOdfTextDocument.AddHeadline(1).AppendText('Demonstration of fpOdf');
        TextColors;
        FOdfTextDocument.SaveToSingleXml(FDatapath + DirectorySeparator + COutout4);

        FOdfTextDocument.Clear;
        FOdfTextDocument.AddHeadline(1).AppendText('Demonstration of fpOdf');
        TextWithTabulators;
        FOdfTextDocument.SaveToZipFile(FDatapath + DirectorySeparator + COutout5);

        FOdfTextDocument.Clear;
        {Do all}
        FOdfTextDocument.AddHeadline(1).AppendText('Demonstration of fpOdf');
        HeadingandStyles;
        StylesBookmarkAndLinks;
        TextColors;
        TextWithTabulators;
        FontsDisplay;
        FOdfTextDocument.SaveToZipFile(FDatapath + DirectorySeparator + COutoutAll);

    end;

    procedure Done;

    begin
        FreeAndNil(FOdfTextDocument);
    end;

begin
    Init;
    Execute;
    Done;
end.
