{
Writes an ODT Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

ORIGINAL AUTHOR: Felipe Monteiro de Carvalho

This file is a version of the fpvectorial's odtvectorialwriter.pas file
(Subversion: r42440).

Adapted to fpOdf by Daniel Gaspary

}

unit odtxmlvectorialwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpvectorial,
  odf_types;

type
  { TvOdtXmlWriter }

  TvOdtXmlWriter = class(TvCustomVectorialWriter)
  private
         FOdt: TOdfTextDocument;

    FPointSeparator: TFormatSettings;
    // Strings with the contents of files
    FMeta, FSettings, FStyles, FContent, FMimetype: string;
    FMetaInfManifest, FManifestRDF: string;
    // helper routines
    function StyleNameToODTStyleName(AData: TvVectorialDocument; AStyleIndex: Integer; AToContentAutoStyle: Boolean = False): string; overload;
    function StyleNameToODTStyleName(AData: TvVectorialDocument; AStyle: TvStyle; AToContentAutoStyle: Boolean = False): string; overload;
    function FloatToODTText(AFloat: Double): string;
    // Routines to write those files
    procedure WriteSettings;
    procedure WriteStyles(AData: TvVectorialDocument);
    procedure WriteDocument(AData: TvVectorialDocument);
    procedure WritePage(ACurPage: TvTextPageSequence; AData: TvVectorialDocument;
               AParent: TOdfElement);

    procedure WriteParagraph(AEntity: TvParagraph; ACurPage: TvTextPageSequence;
               AData: TvVectorialDocument; AParent: TOdfElement);
    procedure WriteTextSpan(AEntity: TvText; AParagraph: TvParagraph;
      ACurPage: TvTextPageSequence; AData: TvVectorialDocument; AParent: TOdfElement);
    procedure WriteBulletList(AEntity: TvBulletList;
      ACurPage: TvTextPageSequence; AData: TvVectorialDocument;
             AParent: TOdfElement);
    // Routines to write parts of those files
    function WriteStylesXMLAsString: string;
    //
  public
    { General reading methods }
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFile(AFileName: string; AData: TvVectorialDocument); override;
    procedure WriteToStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

const
  // SVG requires hardcoding a DPI value

  // The Opera Browser and Inkscape use 90 DPI, so we follow that

  // 1 Inch = 25.4 milimiters
  // 90 inches per pixel = (1 / 90) * 25.4 = 0.2822
  // FLOAT_MILIMETERS_PER_PIXEL = 0.3528; // DPI 72 = 1 / 72 inches per pixel

  FLOAT_MILIMETERS_PER_PIXEL = 0.2822; // DPI 90 = 1 / 90 inches per pixel
  FLOAT_PIXELS_PER_MILIMETER = 3.5433; // DPI 90 = 1 / 90 inches per pixel

function TvOdtXmlWriter.StyleNameToODTStyleName(
  AData: TvVectorialDocument; AStyleIndex: Integer; AToContentAutoStyle: Boolean): string;
var
  lStyle: TvStyle;
begin
  lStyle := AData.GetStyle(AStyleIndex);
  if AToContentAutoStyle then
  begin
    Result := 'P' + IntToStr(AStyleIndex);
  end
  else
  begin
    Result := StringReplace(lStyle.Name, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TvOdtXmlWriter.StyleNameToODTStyleName(
  AData: TvVectorialDocument; AStyle: TvStyle; AToContentAutoStyle: Boolean
  ): string;
var
  lStyleIndex: Integer;
begin
  lStyleIndex := AData.FindStyleIndex(AStyle);
  Result := StyleNameToODTStyleName(AData, lStyleIndex, AToContentAutoStyle);
end;

function TvOdtXmlWriter.FloatToODTText(AFloat: Double): string;
begin
  Result := FloatToStr(AFloat, FPointSeparator);
end;

procedure TvOdtXmlWriter.WriteSettings;
var
   ConfigSet: TConfigConfigItemSet;
begin
     ConfigSet:=TConfigConfigItemSet(FOdt.CreateOdfElement(oetConfigConfigItemSet));
     with ConfigSet do
     begin
          ConfigName:='ooo:view-settings';
          AddConfigItem('ViewAreaTop', 'int', '0');
          AddConfigItem('ViewAreaLeft', 'int', '0');
          AddConfigItem('ViewAreaWidth', 'int', '25534');
          AddConfigItem('ViewAreaHeight', 'int', '9289');
     end;
     FOdt.Settings.AppendChild(ConfigSet);

     ConfigSet:=TConfigConfigItemSet(FOdt.CreateOdfElement(oetConfigConfigItemSet));
     ConfigSet.ConfigName:='ooo:configuration-settings';
     ConfigSet.AddConfigItem('ChartAutoUpdate', 'boolean', 'true');
     FOdt.Settings.AppendChild(ConfigSet);
end;

procedure TvOdtXmlWriter.WriteStyles(AData: TvVectorialDocument);
var
   oe1, oe2,
   oe3, oe4: TOdfElement;

  i: Integer;
  CurStyle: TvStyle;
  lTextPropsStr, lParagraphPropsStr, lCurStyleTmpStr, CurStyleParent: string;
  xTextProp, xParagraphProp: TOdfElement;

  function CreateOutlineLevelStyle(TextLevel, TabStopPosition, TextIndent,
              MarginLeft: string): TOdfElement;
  var
     llProps: TOdfElement;
  begin
       result:=FOdt.CreateOdfElement(oetTextOutlineLevelStyle);
       result.SetAttributes([oatTextLevel, oatStyleNumFormat],
                 [TextLevel, '']);

       llProps:=result.AppendOdfElement(oetStyleListLevelProperties,
                  oatTextListLevelPositionAndSpaceMode, 'label-alignment');

        llProps.AppendOdfElement(oetStyleListLevelLabelAlignment).
           SetAttributes([oatTextLabelFollowedBy, oatTextListTabStopPosition,
             oatFoTextIndent, oatFoMarginLeft],
             ['listtab', TabStopPosition, TextIndent, MarginLeft]);
  end;

begin
     //Font Face Decls
     with TFontFaceDecls(FOdt.FontFaceDecls) do
     begin
          AddFontFace('Mangal1', 'Mangal');
          AddFontFace('OpenSymbol', 'OpenSymbol');
          AddFontFace('Times New Roman', 'Times New Roman', 'roman', 'variable');
          AddFontFace('Arial', 'Arial');
          AddFontFace('Mangal', 'Mangal', 'system', 'variable');
          AddFontFace('Microsoft YaHei', '''Microsoft YaHei''', 'system',
                 'variable');
          AddFontFace('SimSun', 'SimSun', 'system', 'variable');
     end;

     //Styles
     oe1:=FOdt.CreateOdfElement(oetStyleDefaultStyle, oatStyleFamily, 'graphic');
     FOdt.Styles.AppendChild(oe1);
     oe3:=TOdfElement(oe1);

     oe3.SetAttributes([oatSvgStrokeColor, oatDrawFillColor,
         oatFoWrapOption, oatDrawShadowOffsetX, oatDrawShadowOffsetY,
         oatDrawStartLineSpacingHorizontal, oatDrawStartLineSpacingVertical,
         oatDrawEndLineSpacingHorizontal, oatDrawEndLineSpacingVertical,
         oatStyleFlowWithText],
         ['#3465af', '#729fcf', 'no-wrap', '0.3cm', '0.3cm', '0.283cm', '0.283cm',
          '0.283cm', '0.283cm', 'false']);
     oe3:=FOdt.CreateOdfElement(oetStyleGraphicProperties);

     oe1.AppendChild(oe3);

     oe3:=FOdt.CreateOdfElement(oetStyleParagraphProperties);
     oe3.SetAttributes([oatStyleTextAutospace, oatStyleLineBreak,
         oatStyleWritingMode, oatStyleFontIndependentLineSpacing],
         ['ideograph-alpha', 'strict', 'lr-tb', 'false']);
     oe3.AppendOdfElement(oetStyleTabStops);
     oe1.AppendChild(oe3);

     oe3:=FOdt.CreateOdfElement(oetStyleTextProperties);
     oe3.SetAttributes([oatStyleUseWindowFontColor, oatFoFontSize,
         oatFoLanguage, oatFoCountry, oatStyleLetterKerning,
         oatStyleFontSizeAsian, oatStyleLanguageAsian,
         oatStyleCountryAsian, oatStyleFontSizeComplex,
         oatStyleLanguageComplex, oatStyleCountryComplex],
         ['true', '12pt', 'fi', 'FI', 'true', '10.5pt', 'zh', 'CN', '12pt',
          'hi', 'IN']);
     oe1.AppendChild(oe3);

     oe1:=FOdt.CreateOdfElement(oetStyleDefaultStyle, oatStyleFamily, 'paragraph');
     FOdt.Styles.AppendChild(oe1);

     oe3:=FOdt.CreateOdfElement(oetStyleParagraphProperties);
     oe3.SetAttributes([oatFoHyphenationLadderCount, oatStyleTextAutospace,
         oatStylePunctuationWrap, oatStyleLineBreak, oatStyleTabStopDistance,
         oatStyleWritingMode],
         ['no-limit', 'ideograph-alpha', 'hanging', 'strict', '1.251cm',
          'page']);
     oe1.AppendChild(oe3);
     //'style:font-size-complex="12pt" style:language-complex="hi" style:country-complex="IN" fo:hyphenate="false" fo:hyphenation-remain-char-count="2" fo:hyphenation-push-char-count="2" />' + LineEnding +
     oe3:=FOdt.CreateOdfElement(oetStyleTextProperties);
     oe3.SetAttributes([oatStyleUseWindowFontColor, oatStyleFontName,
         oatFoFontSize, oatFoLanguage, oatFoCountry,
         oatStyleLetterKerning, oatStyleFontNameAsian,
         oatStyleFontSizeAsian, oatStyleLanguageAsian,
         oatStyleCountryAsian, oatStyleFontNameComplex,
         oatStyleFontSizeComplex, oatStyleLanguageComplex,
         oatStyleCountryComplex, oatFoHyphenate,
         oatFoHyphenationRemainCharCount, oatFoHyphenationPushCharCount],
         ['true', 'Times New Roman', '12pt', 'fi', 'FI', 'true', 'SimSun',
          '10.5pt', 'zh', 'CN', 'Mangal', '12pt', 'hi', 'IN', 'false', '2',
          '2']);
     oe1.AppendChild(oe3);

     oe1:=FOdt.CreateOdfElement(oetStyleDefaultStyle, oatStyleFamily, 'table');
     FOdt.Styles.AppendChild(oe1);

     oe3:=FOdt.CreateOdfElement(oetStyleTableProperties, oatTableBorderModel,
     'collapsing');
     oe1.AppendChild(oe3);

     oe1:=FOdt.CreateOdfElement(oetStyleDefaultStyle, oatStyleFamily, 'table-row');
     FOdt.Styles.AppendChild(oe1);

     oe3:=FOdt.CreateOdfElement(oetStyleTableRowProperties, oatFoKeepTogether,
     'auto');
     oe1.AppendChild(oe3);


     oe3:=FOdt.CreateOdfElement(oetStyleStyle);
     oe3.SetAttributes([oatStyleName, oatStyleFamily, oatStyleClass],
            ['Standard', 'paragraph', 'text']);
     FOdt.Styles.AppendChild(oe3);

     for i := 0 to AData.GetStyleCount() - 1 do
     begin
       lTextPropsStr := '';
       lParagraphPropsStr := '';
       xTextProp:=FOdt.CreateOdfElement(oetStyleTextProperties);

       CurStyle := AData.GetStyle(i);

       if CurStyle.Parent = nil then CurStyleParent := 'Standard'
       else CurStyleParent := StyleNameToODTStyleName(AData, AData.FindStyleIndex(CurStyle.Parent), False);

       if spbfFontSize in CurStyle.SetElements then
       begin
            xTextProp.SetAttributes([oatFoFontSize, oatStyleFontSizeAsian,
                                     oatStyleFontSizeComplex],
                [IntToStr(CurStyle.Font.Size)+'pt',
                 IntToStr(CurStyle.Font.Size)+'pt',
                 IntToStr(CurStyle.Font.Size)+'pt']);
       end;
       if spbfFontName in CurStyle.SetElements then
       begin
            xTextProp.SetAttributes([oatStyleFontName, oatStyleFontNameAsian,
                                  oatStyleFontNameComplex],
             [CurStyle.Font.Name, 'Microsoft YaHei', 'Mangal']);
       end;
       if (spbfFontBold in CurStyle.SetElements) then
       begin
         if CurStyle.Font.Bold then
         begin
              xTextProp.SetAttributes([oatFoFontWeight, oatStyleFontWeightAsian,
                                 oatStyleFontWeightComplex],
            ['bold', 'bold', 'bold']);
         end
         else
         begin
              xTextProp.SetAttributes([oatFoFontWeight, oatStyleFontWeightAsian,
                                      oatStyleFontWeightComplex],
                    ['normal', 'normal', 'normal']);
         end;
       end;
       if (spbfFontItalic in CurStyle.SetElements) then
       begin
         if CurStyle.Font.Italic then
         begin
              xTextProp.SetAttributes([oatFoFontStyle, oatStyleFontStyleAsian,
                                   oatStyleFontStyleComplex],
                 ['italic', 'italic', 'italic']);
         end
         else
         begin
           // ToDo
         end;
       end;

       if CurStyle.GetKind() = vskTextSpan then
       begin
            oe3:=FOdt.CreateOdfElement(oetStyleStyle);
            oe3.SetAttributes([oatStyleName, oatStyleDisplayName,
                oatStyleFamily, oatStyleParentStyleName],
                [StyleNameToODTStyleName(AData, i, False), CurStyle.Name,
                 'text', CurStyleParent]);
            oe3.AppendChild(xTextProp);

            FOdt.Styles.AppendChild(oe3);
       end
       // Paragraph kind
       else
       begin
         xParagraphProp:=FOdt.CreateOdfElement(oetStyleParagraphProperties);

         if sseMarginTop in CurStyle.SetElements then
            xParagraphProp.SetAttribute(oatFoMarginTop,
               FloatToODTText(CurStyle.MarginTop)+'mm');
         if sseMarginBottom in CurStyle.SetElements then
            xParagraphProp.SetAttribute(oatFoMarginBottom,
               FloatToODTText(CurStyle.MarginTop)+'mm');

         oe3:=FOdt.CreateOdfElement(oetStyleStyle);
         oe3.SetAttributes([oatStyleName, oatStyleDisplayName,
             oatStyleFamily, oatStyleParentStyleName, oatStyleClass],
             [StyleNameToODTStyleName(AData, i, False), CurStyle.Name,
              'paragraph', CurStyleParent, 'text']);

         //Attribute missing on enum: Contextual-Spacing???
         xParagraphProp.SetAttributeNS(GetURI(onsStyle),
                'style:contextual-spacing', 'false');

         oe3.AppendChild(xTextProp);
         oe3.AppendChild(xParagraphProp);

         FOdt.Styles.AppendChild(oe3);

       end;

       oe3:=FOdt.CreateOdfElement(oetStyleStyle);
       oe3.SetAttributes([oatStyleName, oatStyleDisplayName, oatStyleFamily],
            ['Bullet_20_Symbols', 'Bullet Symbols', 'text']);
       FOdt.Styles.AppendChild(oe3);
       oe3:=oe3.AppendOdfElement(oetStyleTextProperties);
       oe3.SetAttributes([oatStyleFontName, oatStyleFontNameAsian,
             oatStyleFontNameComplex],
             ['OpenSymbol', 'OpenSymbol', 'OpenSymbol']);
     end;

     oe3:=FOdt.CreateOdfElement(oetTextOutlineStyle, oatStyleName, 'Outline');

     oe3.AppendChild(CreateOutlineLevelStyle('1', '0.762cm', '-0.762cm', '0.762cm'));
     oe3.AppendChild(CreateOutlineLevelStyle('2', '1.016cm', '-1.016cm', '1.016cm'));
     oe3.AppendChild(CreateOutlineLevelStyle('3', '1.27cm', '-1.27cm', '1.27cm'));
     oe3.AppendChild(CreateOutlineLevelStyle('4', '1.524cm', '-1.524cm', '1.524cm'));
     oe3.AppendChild(CreateOutlineLevelStyle('5', '1.778cm', '-1.778cm', '1.778cm'));
     oe3.AppendChild(CreateOutlineLevelStyle('6', '2.032cm', '-2.032cm', '2.032cm'));
     oe3.AppendChild(CreateOutlineLevelStyle('7', '2.286cm', '-2.286cm', '2.286cm'));
     oe3.AppendChild(CreateOutlineLevelStyle('8', '2.54cm', '-2.54cm', '2.54cm'));
     oe3.AppendChild(CreateOutlineLevelStyle('9', '2.794cm', '-2.794cm', '2.794cm'));
     oe3.AppendChild(CreateOutlineLevelStyle('10', '3.048cm', '-3.048cm', '3.048cm'));

     FOdt.Styles.AppendChild(oe3);

     oe4:=oe3.AppendOdfElement(oetTextNotesConfiguration);
     oe4.SetAttributes([oatTextNoteClass, oatStyleNumFormat, oatTextStartValue,
          oatTextFootnotesPosition, oatTextStartNumberingAt ],
          ['footnote', '1', '0', 'page', 'document']);

     oe4:=oe3.AppendOdfElement(oetTextNotesConfiguration);
     oe4.SetAttributes([oatTextNoteClass, oatStyleNumFormat,oatTextStartValue],
          ['endnote', 'i', '0']);

     oe4:=oe3.AppendOdfElement(oetTextLinenumberingConfiguration);
     oe4.SetAttributes([oatTextNumberLines, oatTextOffset, oatStyleNumFormat,
        oatTextNumberPosition, oatTextIncrement],
          ['false', '0.499cm', '1', 'left', '5']);


     // ----------------------------
     // Automatic Styles
     // ----------------------------

     oe4:=TOdfElement(FOdt.AutomaticStyles).AppendOdfElement(oetStylePageLayout,
             oatStyleName, 'Mpm1');
     oe4:=oe4.AppendOdfElement(oetStylePageLayoutProperties);
     oe4.SetAttributes([oatFoPageWidth, oatFoPageHeight, oatStyleNumFormat,
           oatStylePrintOrientation, oatFoMarginTop, oatFoMarginBottom,
           oatFoMarginLeft, oatFoMarginRight, oatStyleWritingMode,
           oatStyleFootnoteMaxHeight],
           ['21.001cm', '29.7cm', '1', 'portrait', '2cm', '2cm', '2cm', '2cm',
           'lr-tb', '0cm']);

     oe4:=oe3.AppendOdfElement(oetStyleFootnoteSep);
     oe4.SetAttributes([oatStyleWidth, oatStyleDistanceBeforeSep,
          oatStyleDistanceAfterSep, oatStyleLineStyle, oatStyleAdjustment,
          oatStyleRelWidth, oatStyleColor],
          ['0.018cm', '0.101cm', '0.101cm', 'solid', 'left', '25%', '#000000']);

     oe4:=oe3.AppendOdfElement(oetStyleStyle);
     oe4.SetAttributes([oatStyleName, oatStyleFamily, oatStyleParentStyleName,
         oatStyleListStyleName], ['List_0', 'paragraph', 'Standard', 'L1']);


     oe3:=FOdt.CreateOdfElement(oetStyleMasterPage, oatStyleName, 'Standard');
     oe3.SetAttribute(oatStylePageLayoutName, 'Mpm1');
     FOdt.MasterStyles.AppendChild(oe3);
end;


procedure TvOdtXmlWriter.WriteDocument(AData: TvVectorialDocument);
var
  i: Integer;
  CurPage: TvPage;
  CurTextPage: TvTextPageSequence absolute CurPage;
  vStyle: TStyleStyle;
  e: TOdfElement;
begin
{     vStyle:=TStyleStyle(FOdt.CreateOdfElement(oetStyleStyle));
     with vStyle do
     begin
          StyleName:='P1';
          StyleFamily:='paragraph';
          ParentStyleName:='Heading_20_2';
          SetAttributeNS(SCHEMAS_XMLNS_OFFICEOOO, 'officeooo:rsid', '00072f3e');
          SetAttributeNS(SCHEMAS_XMLNS_OFFICEOOO, 'officeooo:paragraph-rsid',
               '00072f3e');
     end;
     FOdt.AutomaticStyles.AppendChild(vStyle);

     vStyle:=TStyleStyle(FOdt.CreateOdfElement(oetStyleStyle));
     with vStyle do
     begin
          StyleName:='P2';
          StyleFamily:='paragraph';
          ParentStyleName:='Heading_20_1';
          SetAttributeNS(SCHEMAS_XMLNS_OFFICEOOO, 'officeooo:rsid', '00072f3e');
          SetAttributeNS(SCHEMAS_XMLNS_OFFICEOOO, 'officeooo:paragraph-rsid',
               '00072f3e');
     end;
     FOdt.AutomaticStyles.AppendChild(vStyle);

     vStyle:=TStyleStyle(FOdt.CreateOdfElement(oetStyleStyle));
     with vStyle do
     begin
          StyleName:='P3';
          StyleFamily:='paragraph';
          ParentStyleName:='Standard';
          SetAttributeNS(SCHEMAS_XMLNS_OFFICEOOO, 'officeooo:rsid', '00072f3e');
          SetAttributeNS(SCHEMAS_XMLNS_OFFICEOOO, 'officeooo:paragraph-rsid',
               '00072f3e');
     end;
     FOdt.AutomaticStyles.AppendChild(vStyle);

     vStyle:=TStyleStyle(FOdt.CreateOdfElement(oetStyleStyle));
     with vStyle do
     begin
          StyleName:='P4';
          StyleFamily:='paragraph';
          ParentStyleName:='Standard';
          OdfSetAttribute(oatStyleListStyleName,'L1');
          SetAttributeNS(SCHEMAS_XMLNS_OFFICEOOO, 'officeooo:rsid', '00072f3e');
          SetAttributeNS(SCHEMAS_XMLNS_OFFICEOOO, 'officeooo:paragraph-rsid',
               '00072f3e');
     end;
     FOdt.AutomaticStyles.AppendChild(vStyle);

     vStyle:=TStyleStyle(FOdt.CreateOdfElement(oetStyleStyle));
     with vStyle do
     begin
          StyleName:='P5';
          StyleFamily:='paragraph';
          ParentStyleName:='Text_20_body';
          SetAttributeNS(SCHEMAS_XMLNS_OFFICEOOO, 'officeooo:rsid', '00072f3e');
     end;
     FOdt.AutomaticStyles.AppendChild(vStyle);
}
     e:=FOdt.CreateOdfElement(oetTextListStyle, oatStyleName, 'L1');
     FOdt.AutomaticStyles.AppendChild(e);

     e:=e.AppendOdfElement(oetTextListLevelStyleBullet);
     e.SetAttributes([oatTextLevel, oatTextStyleName, oatTextBulletChar],
             ['1', 'Bullet_20_Symbols', '•']);

     e:=e.AppendOdfElement(oetStyleListLevelProperties,
           oatTextListLevelPositionAndSpaceMode, 'label-alignment');

     e:=e.AppendOdfElement(oetStyleListLevelLabelAlignment);
     e.SetAttributes([oatTextLabelFollowedBy, oatTextListTabStopPosition,
       oatFoTextIndent,  oatFoMarginLeft],
       ['listtab', '1.667cm', '-0.635cm', '1.667cm']);

  FContent := FContent +
     '  <office:body>' + LineEnding;

  for i := 0 to AData.GetPageCount()-1 do
  begin
    CurPage := AData.GetPage(i);
    if CurPage is TvTextPageSequence then
    begin
         WritePage(CurTextPage, AData, TOdfElement(FOdt.Text));
    end;
  end;
end;

procedure TvOdtXmlWriter.WritePage(ACurPage: TvTextPageSequence; AData: TvVectorialDocument;
             AParent: TOdfElement);
var
  i: Integer;
  lCurEntity: TvEntity;
   oe1: TOdfElement;
begin
     oe1:=FOdt.CreateOdfElement(oetTextSequenceDecls);
     with TTextSequenceDecls(oe1) do
     begin
          AddSequenceDecl('0', 'Illustration');
          AddSequenceDecl('0', 'Table');
          AddSequenceDecl('0', 'Text');
          AddSequenceDecl('0', 'Drawing');
     end;

     AParent.AppendChild(oe1);


  for i := 0 to ACurPage.GetEntitiesCount()-1 do
  begin
    lCurEntity := ACurPage.GetEntity(i);

    if (lCurEntity is TvParagraph) then
       WriteParagraph(TvParagraph(lCurEntity), ACurPage, AData, AParent);
    if (lCurEntity is TvBulletList) then
      WriteBulletList(TvBulletList(lCurEntity), ACurPage, AData, AParent);
  end;
end;

procedure TvOdtXmlWriter.WriteParagraph(AEntity: TvParagraph;
  ACurPage: TvTextPageSequence; AData: TvVectorialDocument;
  AParent: TOdfElement);
var
   EntityKind: TElementType;
   lOutlineLevel: string;
   e: TOdfElement;

   AEntityStyleName: string;
   i: Integer;
   lCurEntity: TvEntity;
begin
     lOutlineLevel:=''
     ;
  if AEntity.Style = nil then
  begin
       EntityKind:=oetTextP;
       AEntityStyleName := 'Standard';
  end
  else
  begin
    case AEntity.Style.GetKind() of
    vskHeading: begin
                     EntityKind:=oetTextH;
                     lOutlineLevel:=IntToStr(AEntity.Style.HeadingLevel);
                end
    else // vskTextBody;
        EntityKind:=oetTextP;
    end;

    AEntityStyleName := StyleNameToODTStyleName(AData, AEntity.Style, False);
  end;

  e:=AParent.AppendOdfElement(EntityKind, oatTextStyleName, AEntityStyleName);

  if lOutlineLevel<>''
  then
      e.SetAttribute(oatTextOutlineLevel, lOutlineLevel);

  for i := 0 to AEntity.GetEntitiesCount()-1 do
  begin
    lCurEntity := AEntity.GetEntity(i);

    if not (lCurEntity is TvText) then Continue;

    WriteTextSpan(TvText(lCurEntity), AEntity, ACurPage, AData, e);
  end;
end;

procedure TvOdtXmlWriter.WriteTextSpan(AEntity: TvText; AParagraph: TvParagraph;
  ACurPage: TvTextPageSequence; AData: TvVectorialDocument; AParent: TOdfElement);
var
   e: TOdfElement;
  AEntityStyleName: string;
  lStyle: TvStyle;
begin
  lStyle := AEntity.GetCombinedStyle(AParagraph);
  if lStyle = nil then
  begin
    AEntityStyleName := 'Standard';
  end
  else
  begin
    AEntityStyleName := StyleNameToODTStyleName(AData, lStyle, False);
  end;

  // Note that here we write only text spans!
  e:=AParent.AppendOdfElement(oetTextSpan, oatTextStyleName, AEntityStyleName);
  e.TextContent:=AEntity.Value.Text;
end;

procedure TvOdtXmlWriter.WriteBulletList(AEntity: TvBulletList;
  ACurPage: TvTextPageSequence; AData: TvVectorialDocument;
  AParent: TOdfElement);
var
   i, j: Integer;
   lCurEntity, lCurSubEntity: TvEntity;
   lCurParagraph: TvParagraph;
   vList, vPara: TOdfElement;
begin
     vList:=AParent.AppendOdfElement(oetTextList, oatTextStyleName, 'L1');

    for i := 0 to AEntity.GetEntitiesCount()-1 do
    begin
      lCurEntity := AEntity.GetEntity(i);

      if (lCurEntity is TvParagraph) then
      begin
        lCurParagraph := lCurEntity as TvParagraph;
        if lCurParagraph.Style <> nil then
        begin
             vPara:=vList.AppendOdfElement(oetTextListItem).
                       AppendOdfElement(oetTextP, oatTextStyleName,
                       'List_'+IntToStr(lCurParagraph.Style.ListLevel));
        end
        else
        begin
             vPara:=vList.AppendOdfElement(oetTextListItem).
                       AppendOdfElement(oetTextP, oatTextStyleName,
                       'List_0');
        end;

        for j := 0 to lCurParagraph.GetEntitiesCount()-1 do
        begin
          lCurSubEntity := lCurParagraph.GetEntity(j);

          if (lCurSubEntity is TvText) then
            WriteTextSpan(TvText(lCurSubEntity), lCurParagraph, ACurPage, AData,
            vPara);
        end;
      end;
    end;
end;

function TvOdtXmlWriter.WriteStylesXMLAsString: string;
begin

end;

constructor TvOdtXmlWriter.Create;
begin
  inherited Create;

  FOdt:=TOdfTextDocument.Create;

  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator
end;

destructor TvOdtXmlWriter.Destroy;
begin
     FOdt.Free;

  inherited Destroy;
end;

procedure TvOdtXmlWriter.WriteToFile(AFileName: string;
  AData: TvVectorialDocument);
begin
  WriteSettings();
  WriteStyles(AData);
  WriteDocument(AData);
  TOdfDocument.SaveToZipFile(FOdt, AFileName);
end;

procedure TvOdtXmlWriter.WriteToStream(AStream: TStream;
  AData: TvVectorialDocument);
begin
  // Not supported at the moment
  raise Exception.Create('TvOdtXmlWriter.WriteToStream not supported');
end;

initialization

//RegisterVectorialWriter(TvOdtXmlWriter, vfODT);

end.

