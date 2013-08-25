{ Atts-Proc-Implementation.inc is part of the fpOdf.

  fpOdf is a library used to help users to create and to modify OpenDocument
  Files(ODF)

  Copyright (C) 2013 Daniel F. Gaspary https://github.com/dgaspary

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{This file was generated automatically.
 Any modification will be overridden.}

procedure OdfAttributeGetDetails(at: TAttributeType;
                                 out Namespace: TOdfNamespace;
                                 out Localname: String);
begin
     case at of
          oatNone :     begin
                             Namespace:=onsNone;
                             LocalName:='none';
                        end;
          oatManifestVersion : begin
                                    Namespace:=onsManifest;
                                    LocalName:='version';
                               end;
          oatManifestFullPath : begin
                                     Namespace:=onsManifest;
                                     LocalName:='full-path';
                                end;
          oatManifestSize : begin
                                 Namespace:=onsManifest;
                                 LocalName:='size';
                            end;
          oatManifestMediaType : begin
                                      Namespace:=onsManifest;
                                      LocalName:='media-type';
                                 end;
          oatManifestPreferredViewMode : begin
                                              Namespace:=onsManifest;
                                              LocalName:='preferred-view-mode';
                                         end;
          oatManifestChecksumType : begin
                                         Namespace:=onsManifest;
                                         LocalName:='checksum-type';
                                    end;
          oatManifestChecksum : begin
                                     Namespace:=onsManifest;
                                     LocalName:='checksum';
                                end;
          oatManifestAlgorithmName : begin
                                          Namespace:=onsManifest;
                                          LocalName:='algorithm-name';
                                     end;
          oatManifestInitialisationVector : begin
                                                 Namespace:=onsManifest;
                                                 LocalName:='initialisation-vector';
                                            end;
          oatManifestStartKeyGenerationName : begin
                                                   Namespace:=onsManifest;
                                                   LocalName:='start-key-generation-name';
                                              end;
          oatManifestKeySize : begin
                                    Namespace:=onsManifest;
                                    LocalName:='key-size';
                               end;
          oatManifestKeyDerivationName : begin
                                              Namespace:=onsManifest;
                                              LocalName:='key-derivation-name';
                                         end;
          oatManifestSalt : begin
                                 Namespace:=onsManifest;
                                 LocalName:='salt';
                            end;
          oatManifestIterationCount : begin
                                           Namespace:=onsManifest;
                                           LocalName:='iteration-count';
                                      end;
          oatOfficeMimetype : begin
                                   Namespace:=onsOffice;
                                   LocalName:='mimetype';
                              end;
          oatOfficeVersion : begin
                                  Namespace:=onsOffice;
                                  LocalName:='version';
                             end;
          oatGrddlTransformation : begin
                                        Namespace:=onsGrddl;
                                        LocalName:='transformation';
                                   end;
          oatXlinkType : begin
                              Namespace:=onsXlink;
                              LocalName:='type';
                         end;
          oatXlinkHref : begin
                              Namespace:=onsXlink;
                              LocalName:='href';
                         end;
          oatXlinkActuate : begin
                                 Namespace:=onsXlink;
                                 LocalName:='actuate';
                            end;
          oatXlinkTitle : begin
                               Namespace:=onsXlink;
                               LocalName:='title';
                          end;
          oatMetaDate : begin
                             Namespace:=onsMeta;
                             LocalName:='date';
                        end;
          oatXlinkShow : begin
                              Namespace:=onsXlink;
                              LocalName:='show';
                         end;
          oatMetaDelay : begin
                              Namespace:=onsMeta;
                              LocalName:='delay';
                         end;
          oatOfficeTargetFrameName : begin
                                          Namespace:=onsOffice;
                                          LocalName:='target-frame-name';
                                     end;
          oatMetaPageCount : begin
                                  Namespace:=onsMeta;
                                  LocalName:='page-count';
                             end;
          oatMetaTableCount : begin
                                   Namespace:=onsMeta;
                                   LocalName:='table-count';
                              end;
          oatMetaDrawCount : begin
                                  Namespace:=onsMeta;
                                  LocalName:='draw-count';
                             end;
          oatMetaImageCount : begin
                                   Namespace:=onsMeta;
                                   LocalName:='image-count';
                              end;
          oatMetaOleObjectCount : begin
                                       Namespace:=onsMeta;
                                       LocalName:='ole-object-count';
                                  end;
          oatMetaObjectCount : begin
                                    Namespace:=onsMeta;
                                    LocalName:='object-count';
                               end;
          oatMetaParagraphCount : begin
                                       Namespace:=onsMeta;
                                       LocalName:='paragraph-count';
                                  end;
          oatMetaWordCount : begin
                                  Namespace:=onsMeta;
                                  LocalName:='word-count';
                             end;
          oatMetaCharacterCount : begin
                                       Namespace:=onsMeta;
                                       LocalName:='character-count';
                                  end;
          oatMetaFrameCount : begin
                                   Namespace:=onsMeta;
                                   LocalName:='frame-count';
                              end;
          oatMetaSentenceCount : begin
                                      Namespace:=onsMeta;
                                      LocalName:='sentence-count';
                                 end;
          oatMetaSyllableCount : begin
                                      Namespace:=onsMeta;
                                      LocalName:='syllable-count';
                                 end;
          oatMetaNonWhitespaceCharacterCount : begin
                                                    Namespace:=onsMeta;
                                                    LocalName:='non-whitespace-character-count';
                                               end;
          oatMetaRowCount : begin
                                 Namespace:=onsMeta;
                                 LocalName:='row-count';
                            end;
          oatMetaCellCount : begin
                                  Namespace:=onsMeta;
                                  LocalName:='cell-count';
                             end;
          oatMetaName : begin
                             Namespace:=onsMeta;
                             LocalName:='name';
                        end;
          oatMetaValueType : begin
                                  Namespace:=onsMeta;
                                  LocalName:='value-type';
                             end;
          oatConfigName : begin
                               Namespace:=onsConfig;
                               LocalName:='name';
                          end;
          oatConfigType : begin
                               Namespace:=onsConfig;
                               LocalName:='type';
                          end;
          oatScriptLanguage : begin
                                   Namespace:=onsScript;
                                   LocalName:='language';
                              end;
          oatScriptEventName : begin
                                    Namespace:=onsScript;
                                    LocalName:='event-name';
                               end;
          oatScriptMacroName : begin
                                    Namespace:=onsScript;
                                    LocalName:='macro-name';
                               end;
          oatPresentationAction : begin
                                       Namespace:=onsPresentation;
                                       LocalName:='action';
                                  end;
          oatPresentationEffect : begin
                                       Namespace:=onsPresentation;
                                       LocalName:='effect';
                                  end;
          oatPresentationDirection : begin
                                          Namespace:=onsPresentation;
                                          LocalName:='direction';
                                     end;
          oatPresentationSpeed : begin
                                      Namespace:=onsPresentation;
                                      LocalName:='speed';
                                 end;
          oatPresentationStartScale : begin
                                           Namespace:=onsPresentation;
                                           LocalName:='start-scale';
                                      end;
          oatPresentationVerb : begin
                                     Namespace:=onsPresentation;
                                     LocalName:='verb';
                                end;
          oatXmlId : begin
                          Namespace:=onsXml;
                          LocalName:='id';
                     end;
          oatPresentationPlayFull : begin
                                         Namespace:=onsPresentation;
                                         LocalName:='play-full';
                                    end;
          oatSvgFontFamily : begin
                                  Namespace:=onsSvg;
                                  LocalName:='font-family';
                             end;
          oatSvgFontStyle : begin
                                 Namespace:=onsSvg;
                                 LocalName:='font-style';
                            end;
          oatSvgFontVariant : begin
                                   Namespace:=onsSvg;
                                   LocalName:='font-variant';
                              end;
          oatSvgFontWeight : begin
                                  Namespace:=onsSvg;
                                  LocalName:='font-weight';
                             end;
          oatSvgFontStretch : begin
                                   Namespace:=onsSvg;
                                   LocalName:='font-stretch';
                              end;
          oatSvgFontSize : begin
                                Namespace:=onsSvg;
                                LocalName:='font-size';
                           end;
          oatSvgUnicodeRange : begin
                                    Namespace:=onsSvg;
                                    LocalName:='unicode-range';
                               end;
          oatSvgUnitsPerEm : begin
                                  Namespace:=onsSvg;
                                  LocalName:='units-per-em';
                             end;
          oatSvgPanose1 : begin
                               Namespace:=onsSvg;
                               LocalName:='panose-1';
                          end;
          oatSvgStemv : begin
                             Namespace:=onsSvg;
                             LocalName:='stemv';
                        end;
          oatSvgStemh : begin
                             Namespace:=onsSvg;
                             LocalName:='stemh';
                        end;
          oatSvgSlope : begin
                             Namespace:=onsSvg;
                             LocalName:='slope';
                        end;
          oatSvgCapHeight : begin
                                 Namespace:=onsSvg;
                                 LocalName:='cap-height';
                            end;
          oatSvgXHeight : begin
                               Namespace:=onsSvg;
                               LocalName:='x-height';
                          end;
          oatSvgAccentHeight : begin
                                    Namespace:=onsSvg;
                                    LocalName:='accent-height';
                               end;
          oatSvgAscent : begin
                              Namespace:=onsSvg;
                              LocalName:='ascent';
                         end;
          oatSvgDescent : begin
                               Namespace:=onsSvg;
                               LocalName:='descent';
                          end;
          oatSvgWidths : begin
                              Namespace:=onsSvg;
                              LocalName:='widths';
                         end;
          oatSvgBbox : begin
                            Namespace:=onsSvg;
                            LocalName:='bbox';
                       end;
          oatSvgIdeographic : begin
                                   Namespace:=onsSvg;
                                   LocalName:='ideographic';
                              end;
          oatSvgAlphabetic : begin
                                  Namespace:=onsSvg;
                                  LocalName:='alphabetic';
                             end;
          oatSvgMathematical : begin
                                    Namespace:=onsSvg;
                                    LocalName:='mathematical';
                               end;
          oatSvgHanging : begin
                               Namespace:=onsSvg;
                               LocalName:='hanging';
                          end;
          oatSvgVIdeographic : begin
                                    Namespace:=onsSvg;
                                    LocalName:='v-ideographic';
                               end;
          oatSvgVAlphabetic : begin
                                   Namespace:=onsSvg;
                                   LocalName:='v-alphabetic';
                              end;
          oatSvgVMathematical : begin
                                     Namespace:=onsSvg;
                                     LocalName:='v-mathematical';
                                end;
          oatSvgVHanging : begin
                                Namespace:=onsSvg;
                                LocalName:='v-hanging';
                           end;
          oatSvgUnderlinePosition : begin
                                         Namespace:=onsSvg;
                                         LocalName:='underline-position';
                                    end;
          oatSvgUnderlineThickness : begin
                                          Namespace:=onsSvg;
                                          LocalName:='underline-thickness';
                                     end;
          oatSvgStrikethroughPosition : begin
                                             Namespace:=onsSvg;
                                             LocalName:='strikethrough-position';
                                        end;
          oatSvgStrikethroughThickness : begin
                                              Namespace:=onsSvg;
                                              LocalName:='strikethrough-thickness';
                                         end;
          oatSvgOverlinePosition : begin
                                        Namespace:=onsSvg;
                                        LocalName:='overline-position';
                                   end;
          oatSvgOverlineThickness : begin
                                         Namespace:=onsSvg;
                                         LocalName:='overline-thickness';
                                    end;
          oatStyleName : begin
                              Namespace:=onsStyle;
                              LocalName:='name';
                         end;
          oatStyleFontAdornments : begin
                                        Namespace:=onsStyle;
                                        LocalName:='font-adornments';
                                   end;
          oatStyleFontFamilyGeneric : begin
                                           Namespace:=onsStyle;
                                           LocalName:='font-family-generic';
                                      end;
          oatStyleFontPitch : begin
                                   Namespace:=onsStyle;
                                   LocalName:='font-pitch';
                              end;
          oatStyleFontCharset : begin
                                     Namespace:=onsStyle;
                                     LocalName:='font-charset';
                                end;
          oatSvgString : begin
                              Namespace:=onsSvg;
                              LocalName:='string';
                         end;
          oatSvgName : begin
                            Namespace:=onsSvg;
                            LocalName:='name';
                       end;
          oatStyleDisplayName : begin
                                     Namespace:=onsStyle;
                                     LocalName:='display-name';
                                end;
          oatStyleParentStyleName : begin
                                         Namespace:=onsStyle;
                                         LocalName:='parent-style-name';
                                    end;
          oatStyleNextStyleName : begin
                                       Namespace:=onsStyle;
                                       LocalName:='next-style-name';
                                  end;
          oatStyleListLevel : begin
                                   Namespace:=onsStyle;
                                   LocalName:='list-level';
                              end;
          oatStyleListStyleName : begin
                                       Namespace:=onsStyle;
                                       LocalName:='list-style-name';
                                  end;
          oatStyleMasterPageName : begin
                                        Namespace:=onsStyle;
                                        LocalName:='master-page-name';
                                   end;
          oatStyleAutoUpdate : begin
                                    Namespace:=onsStyle;
                                    LocalName:='auto-update';
                               end;
          oatStyleDataStyleName : begin
                                       Namespace:=onsStyle;
                                       LocalName:='data-style-name';
                                  end;
          oatStylePercentageDataStyleName : begin
                                                 Namespace:=onsStyle;
                                                 LocalName:='percentage-data-style-name';
                                            end;
          oatStyleClass : begin
                               Namespace:=onsStyle;
                               LocalName:='class';
                          end;
          oatStyleDefaultOutlineLevel : begin
                                             Namespace:=onsStyle;
                                             LocalName:='default-outline-level';
                                        end;
          oatStyleFamily : begin
                                Namespace:=onsStyle;
                                LocalName:='family';
                           end;
          oatFoBackgroundColor : begin
                                      Namespace:=onsFo;
                                      LocalName:='background-color';
                                 end;
          oatFoFontVariant : begin
                                  Namespace:=onsFo;
                                  LocalName:='font-variant';
                             end;
          oatFoTextTransform : begin
                                    Namespace:=onsFo;
                                    LocalName:='text-transform';
                               end;
          oatFoColor : begin
                            Namespace:=onsFo;
                            LocalName:='color';
                       end;
          oatStyleUseWindowFontColor : begin
                                            Namespace:=onsStyle;
                                            LocalName:='use-window-font-color';
                                       end;
          oatStyleTextOutline : begin
                                     Namespace:=onsStyle;
                                     LocalName:='text-outline';
                                end;
          oatStyleTextLineThroughType : begin
                                             Namespace:=onsStyle;
                                             LocalName:='text-line-through-type';
                                        end;
          oatStyleTextLineThroughStyle : begin
                                              Namespace:=onsStyle;
                                              LocalName:='text-line-through-style';
                                         end;
          oatStyleTextLineThroughWidth : begin
                                              Namespace:=onsStyle;
                                              LocalName:='text-line-through-width';
                                         end;
          oatStyleTextLineThroughColor : begin
                                              Namespace:=onsStyle;
                                              LocalName:='text-line-through-color';
                                         end;
          oatStyleTextLineThroughText : begin
                                             Namespace:=onsStyle;
                                             LocalName:='text-line-through-text';
                                        end;
          oatStyleTextLineThroughTextStyle : begin
                                                  Namespace:=onsStyle;
                                                  LocalName:='text-line-through-text-style';
                                             end;
          oatStyleTextPosition : begin
                                      Namespace:=onsStyle;
                                      LocalName:='text-position';
                                 end;
          oatStyleFontName : begin
                                  Namespace:=onsStyle;
                                  LocalName:='font-name';
                             end;
          oatStyleFontNameAsian : begin
                                       Namespace:=onsStyle;
                                       LocalName:='font-name-asian';
                                  end;
          oatStyleFontNameComplex : begin
                                         Namespace:=onsStyle;
                                         LocalName:='font-name-complex';
                                    end;
          oatFoFontFamily : begin
                                 Namespace:=onsFo;
                                 LocalName:='font-family';
                            end;
          oatStyleFontFamilyAsian : begin
                                         Namespace:=onsStyle;
                                         LocalName:='font-family-asian';
                                    end;
          oatStyleFontFamilyComplex : begin
                                           Namespace:=onsStyle;
                                           LocalName:='font-family-complex';
                                      end;
          oatStyleFontFamilyGenericAsian : begin
                                                Namespace:=onsStyle;
                                                LocalName:='font-family-generic-asian';
                                           end;
          oatStyleFontFamilyGenericComplex : begin
                                                  Namespace:=onsStyle;
                                                  LocalName:='font-family-generic-complex';
                                             end;
          oatStyleFontStyleName : begin
                                       Namespace:=onsStyle;
                                       LocalName:='font-style-name';
                                  end;
          oatStyleFontStyleNameAsian : begin
                                            Namespace:=onsStyle;
                                            LocalName:='font-style-name-asian';
                                       end;
          oatStyleFontStyleNameComplex : begin
                                              Namespace:=onsStyle;
                                              LocalName:='font-style-name-complex';
                                         end;
          oatStyleFontPitchAsian : begin
                                        Namespace:=onsStyle;
                                        LocalName:='font-pitch-asian';
                                   end;
          oatStyleFontPitchComplex : begin
                                          Namespace:=onsStyle;
                                          LocalName:='font-pitch-complex';
                                     end;
          oatStyleFontCharsetAsian : begin
                                          Namespace:=onsStyle;
                                          LocalName:='font-charset-asian';
                                     end;
          oatStyleFontCharsetComplex : begin
                                            Namespace:=onsStyle;
                                            LocalName:='font-charset-complex';
                                       end;
          oatFoFontSize : begin
                               Namespace:=onsFo;
                               LocalName:='font-size';
                          end;
          oatStyleFontSizeAsian : begin
                                       Namespace:=onsStyle;
                                       LocalName:='font-size-asian';
                                  end;
          oatStyleFontSizeComplex : begin
                                         Namespace:=onsStyle;
                                         LocalName:='font-size-complex';
                                    end;
          oatStyleFontSizeRel : begin
                                     Namespace:=onsStyle;
                                     LocalName:='font-size-rel';
                                end;
          oatStyleFontSizeRelAsian : begin
                                          Namespace:=onsStyle;
                                          LocalName:='font-size-rel-asian';
                                     end;
          oatStyleFontSizeRelComplex : begin
                                            Namespace:=onsStyle;
                                            LocalName:='font-size-rel-complex';
                                       end;
          oatStyleScriptType : begin
                                    Namespace:=onsStyle;
                                    LocalName:='script-type';
                               end;
          oatFoLetterSpacing : begin
                                    Namespace:=onsFo;
                                    LocalName:='letter-spacing';
                               end;
          oatFoLanguage : begin
                               Namespace:=onsFo;
                               LocalName:='language';
                          end;
          oatStyleLanguageAsian : begin
                                       Namespace:=onsStyle;
                                       LocalName:='language-asian';
                                  end;
          oatStyleLanguageComplex : begin
                                         Namespace:=onsStyle;
                                         LocalName:='language-complex';
                                    end;
          oatFoCountry : begin
                              Namespace:=onsFo;
                              LocalName:='country';
                         end;
          oatStyleCountryAsian : begin
                                      Namespace:=onsStyle;
                                      LocalName:='country-asian';
                                 end;
          oatStyleCountryComplex : begin
                                        Namespace:=onsStyle;
                                        LocalName:='country-complex';
                                   end;
          oatFoScript : begin
                             Namespace:=onsFo;
                             LocalName:='script';
                        end;
          oatStyleScriptAsian : begin
                                     Namespace:=onsStyle;
                                     LocalName:='script-asian';
                                end;
          oatStyleScriptComplex : begin
                                       Namespace:=onsStyle;
                                       LocalName:='script-complex';
                                  end;
          oatStyleRfcLanguageTag : begin
                                        Namespace:=onsStyle;
                                        LocalName:='rfc-language-tag';
                                   end;
          oatStyleRfcLanguageTagAsian : begin
                                             Namespace:=onsStyle;
                                             LocalName:='rfc-language-tag-asian';
                                        end;
          oatStyleRfcLanguageTagComplex : begin
                                               Namespace:=onsStyle;
                                               LocalName:='rfc-language-tag-complex';
                                          end;
          oatFoFontStyle : begin
                                Namespace:=onsFo;
                                LocalName:='font-style';
                           end;
          oatStyleFontStyleAsian : begin
                                        Namespace:=onsStyle;
                                        LocalName:='font-style-asian';
                                   end;
          oatStyleFontStyleComplex : begin
                                          Namespace:=onsStyle;
                                          LocalName:='font-style-complex';
                                     end;
          oatStyleFontRelief : begin
                                    Namespace:=onsStyle;
                                    LocalName:='font-relief';
                               end;
          oatFoTextShadow : begin
                                 Namespace:=onsFo;
                                 LocalName:='text-shadow';
                            end;
          oatStyleTextUnderlineType : begin
                                           Namespace:=onsStyle;
                                           LocalName:='text-underline-type';
                                      end;
          oatStyleTextUnderlineStyle : begin
                                            Namespace:=onsStyle;
                                            LocalName:='text-underline-style';
                                       end;
          oatStyleTextUnderlineWidth : begin
                                            Namespace:=onsStyle;
                                            LocalName:='text-underline-width';
                                       end;
          oatStyleTextUnderlineColor : begin
                                            Namespace:=onsStyle;
                                            LocalName:='text-underline-color';
                                       end;
          oatStyleTextOverlineType : begin
                                          Namespace:=onsStyle;
                                          LocalName:='text-overline-type';
                                     end;
          oatStyleTextOverlineStyle : begin
                                           Namespace:=onsStyle;
                                           LocalName:='text-overline-style';
                                      end;
          oatStyleTextOverlineWidth : begin
                                           Namespace:=onsStyle;
                                           LocalName:='text-overline-width';
                                      end;
          oatStyleTextOverlineColor : begin
                                           Namespace:=onsStyle;
                                           LocalName:='text-overline-color';
                                      end;
          oatStyleTextOverlineMode : begin
                                          Namespace:=onsStyle;
                                          LocalName:='text-overline-mode';
                                     end;
          oatFoFontWeight : begin
                                 Namespace:=onsFo;
                                 LocalName:='font-weight';
                            end;
          oatStyleFontWeightAsian : begin
                                         Namespace:=onsStyle;
                                         LocalName:='font-weight-asian';
                                    end;
          oatStyleFontWeightComplex : begin
                                           Namespace:=onsStyle;
                                           LocalName:='font-weight-complex';
                                      end;
          oatStyleTextUnderlineMode : begin
                                           Namespace:=onsStyle;
                                           LocalName:='text-underline-mode';
                                      end;
          oatStyleTextLineThroughMode : begin
                                             Namespace:=onsStyle;
                                             LocalName:='text-line-through-mode';
                                        end;
          oatStyleLetterKerning : begin
                                       Namespace:=onsStyle;
                                       LocalName:='letter-kerning';
                                  end;
          oatStyleTextBlinking : begin
                                      Namespace:=onsStyle;
                                      LocalName:='text-blinking';
                                 end;
          oatStyleTextCombine : begin
                                     Namespace:=onsStyle;
                                     LocalName:='text-combine';
                                end;
          oatStyleTextCombineStartChar : begin
                                              Namespace:=onsStyle;
                                              LocalName:='text-combine-start-char';
                                         end;
          oatStyleTextCombineEndChar : begin
                                            Namespace:=onsStyle;
                                            LocalName:='text-combine-end-char';
                                       end;
          oatStyleTextEmphasize : begin
                                       Namespace:=onsStyle;
                                       LocalName:='text-emphasize';
                                  end;
          oatStyleTextScale : begin
                                   Namespace:=onsStyle;
                                   LocalName:='text-scale';
                              end;
          oatStyleTextRotationAngle : begin
                                           Namespace:=onsStyle;
                                           LocalName:='text-rotation-angle';
                                      end;
          oatStyleTextRotationScale : begin
                                           Namespace:=onsStyle;
                                           LocalName:='text-rotation-scale';
                                      end;
          oatFoHyphenate : begin
                                Namespace:=onsFo;
                                LocalName:='hyphenate';
                           end;
          oatFoHyphenationRemainCharCount : begin
                                                 Namespace:=onsFo;
                                                 LocalName:='hyphenation-remain-char-count';
                                            end;
          oatFoHyphenationPushCharCount : begin
                                               Namespace:=onsFo;
                                               LocalName:='hyphenation-push-char-count';
                                          end;
          oatTextDisplay : begin
                                Namespace:=onsText;
                                LocalName:='display';
                           end;
          oatTextCondition : begin
                                  Namespace:=onsText;
                                  LocalName:='condition';
                             end;
          oatFoTextAlign : begin
                                Namespace:=onsFo;
                                LocalName:='text-align';
                           end;
          oatFoMarginLeft : begin
                                 Namespace:=onsFo;
                                 LocalName:='margin-left';
                            end;
          oatFoMarginRight : begin
                                  Namespace:=onsFo;
                                  LocalName:='margin-right';
                             end;
          oatFoMarginTop : begin
                                Namespace:=onsFo;
                                LocalName:='margin-top';
                           end;
          oatFoMarginBottom : begin
                                   Namespace:=onsFo;
                                   LocalName:='margin-bottom';
                              end;
          oatFoMargin : begin
                             Namespace:=onsFo;
                             LocalName:='margin';
                        end;
          oatFoBreakBefore : begin
                                  Namespace:=onsFo;
                                  LocalName:='break-before';
                             end;
          oatFoBreakAfter : begin
                                 Namespace:=onsFo;
                                 LocalName:='break-after';
                            end;
          oatFoBorder : begin
                             Namespace:=onsFo;
                             LocalName:='border';
                        end;
          oatFoBorderTop : begin
                                Namespace:=onsFo;
                                LocalName:='border-top';
                           end;
          oatFoBorderBottom : begin
                                   Namespace:=onsFo;
                                   LocalName:='border-bottom';
                              end;
          oatFoBorderLeft : begin
                                 Namespace:=onsFo;
                                 LocalName:='border-left';
                            end;
          oatFoBorderRight : begin
                                  Namespace:=onsFo;
                                  LocalName:='border-right';
                             end;
          oatStyleBorderLineWidth : begin
                                         Namespace:=onsStyle;
                                         LocalName:='border-line-width';
                                    end;
          oatStyleBorderLineWidthTop : begin
                                            Namespace:=onsStyle;
                                            LocalName:='border-line-width-top';
                                       end;
          oatStyleBorderLineWidthBottom : begin
                                               Namespace:=onsStyle;
                                               LocalName:='border-line-width-bottom';
                                          end;
          oatStyleBorderLineWidthLeft : begin
                                             Namespace:=onsStyle;
                                             LocalName:='border-line-width-left';
                                        end;
          oatStyleBorderLineWidthRight : begin
                                              Namespace:=onsStyle;
                                              LocalName:='border-line-width-right';
                                         end;
          oatFoPadding : begin
                              Namespace:=onsFo;
                              LocalName:='padding';
                         end;
          oatFoPaddingTop : begin
                                 Namespace:=onsFo;
                                 LocalName:='padding-top';
                            end;
          oatFoPaddingBottom : begin
                                    Namespace:=onsFo;
                                    LocalName:='padding-bottom';
                               end;
          oatFoPaddingLeft : begin
                                  Namespace:=onsFo;
                                  LocalName:='padding-left';
                             end;
          oatFoPaddingRight : begin
                                   Namespace:=onsFo;
                                   LocalName:='padding-right';
                              end;
          oatStyleShadow : begin
                                Namespace:=onsStyle;
                                LocalName:='shadow';
                           end;
          oatFoKeepWithNext : begin
                                   Namespace:=onsFo;
                                   LocalName:='keep-with-next';
                              end;
          oatStyleWritingMode : begin
                                     Namespace:=onsStyle;
                                     LocalName:='writing-mode';
                                end;
          oatStylePageNumber : begin
                                    Namespace:=onsStyle;
                                    LocalName:='page-number';
                               end;
          oatStyleBackgroundTransparency : begin
                                                Namespace:=onsStyle;
                                                LocalName:='background-transparency';
                                           end;
          oatFoLineHeight : begin
                                 Namespace:=onsFo;
                                 LocalName:='line-height';
                            end;
          oatStyleLineHeightAtLeast : begin
                                           Namespace:=onsStyle;
                                           LocalName:='line-height-at-least';
                                      end;
          oatStyleLineSpacing : begin
                                     Namespace:=onsStyle;
                                     LocalName:='line-spacing';
                                end;
          oatStyleFontIndependentLineSpacing : begin
                                                    Namespace:=onsStyle;
                                                    LocalName:='font-independent-line-spacing';
                                               end;
          oatFoTextAlignLast : begin
                                    Namespace:=onsFo;
                                    LocalName:='text-align-last';
                               end;
          oatStyleJustifySingleWord : begin
                                           Namespace:=onsStyle;
                                           LocalName:='justify-single-word';
                                      end;
          oatFoKeepTogether : begin
                                   Namespace:=onsFo;
                                   LocalName:='keep-together';
                              end;
          oatFoWidows : begin
                             Namespace:=onsFo;
                             LocalName:='widows';
                        end;
          oatFoOrphans : begin
                              Namespace:=onsFo;
                              LocalName:='orphans';
                         end;
          oatStyleTabStopDistance : begin
                                         Namespace:=onsStyle;
                                         LocalName:='tab-stop-distance';
                                    end;
          oatFoHyphenationKeep : begin
                                      Namespace:=onsFo;
                                      LocalName:='hyphenation-keep';
                                 end;
          oatFoHyphenationLadderCount : begin
                                             Namespace:=onsFo;
                                             LocalName:='hyphenation-ladder-count';
                                        end;
          oatStyleRegisterTrue : begin
                                      Namespace:=onsStyle;
                                      LocalName:='register-true';
                                 end;
          oatFoTextIndent : begin
                                 Namespace:=onsFo;
                                 LocalName:='text-indent';
                            end;
          oatStyleAutoTextIndent : begin
                                        Namespace:=onsStyle;
                                        LocalName:='auto-text-indent';
                                   end;
          oatStyleJoinBorder : begin
                                    Namespace:=onsStyle;
                                    LocalName:='join-border';
                               end;
          oatTextNumberLines : begin
                                    Namespace:=onsText;
                                    LocalName:='number-lines';
                               end;
          oatTextLineNumber : begin
                                   Namespace:=onsText;
                                   LocalName:='line-number';
                              end;
          oatStyleTextAutospace : begin
                                       Namespace:=onsStyle;
                                       LocalName:='text-autospace';
                                  end;
          oatStylePunctuationWrap : begin
                                         Namespace:=onsStyle;
                                         LocalName:='punctuation-wrap';
                                    end;
          oatStyleLineBreak : begin
                                   Namespace:=onsStyle;
                                   LocalName:='line-break';
                              end;
          oatStyleVerticalAlign : begin
                                       Namespace:=onsStyle;
                                       LocalName:='vertical-align';
                                  end;
          oatStyleWritingModeAutomatic : begin
                                              Namespace:=onsStyle;
                                              LocalName:='writing-mode-automatic';
                                         end;
          oatStyleSnapToLayoutGrid : begin
                                          Namespace:=onsStyle;
                                          LocalName:='snap-to-layout-grid';
                                     end;
          oatStyleChar : begin
                              Namespace:=onsStyle;
                              LocalName:='char';
                         end;
          oatStylePosition : begin
                                  Namespace:=onsStyle;
                                  LocalName:='position';
                             end;
          oatStyleType : begin
                              Namespace:=onsStyle;
                              LocalName:='type';
                         end;
          oatStyleLeaderType : begin
                                    Namespace:=onsStyle;
                                    LocalName:='leader-type';
                               end;
          oatStyleLeaderStyle : begin
                                     Namespace:=onsStyle;
                                     LocalName:='leader-style';
                                end;
          oatStyleLeaderWidth : begin
                                     Namespace:=onsStyle;
                                     LocalName:='leader-width';
                                end;
          oatStyleLeaderColor : begin
                                     Namespace:=onsStyle;
                                     LocalName:='leader-color';
                                end;
          oatStyleLeaderText : begin
                                    Namespace:=onsStyle;
                                    LocalName:='leader-text';
                               end;
          oatStyleLeaderTextStyle : begin
                                         Namespace:=onsStyle;
                                         LocalName:='leader-text-style';
                                    end;
          oatStyleLength : begin
                                Namespace:=onsStyle;
                                LocalName:='length';
                           end;
          oatStyleLines : begin
                               Namespace:=onsStyle;
                               LocalName:='lines';
                          end;
          oatStyleDistance : begin
                                  Namespace:=onsStyle;
                                  LocalName:='distance';
                             end;
          oatStyleStyleName : begin
                                   Namespace:=onsStyle;
                                   LocalName:='style-name';
                              end;
          oatStyleRepeat : begin
                                Namespace:=onsStyle;
                                LocalName:='repeat';
                           end;
          oatStyleFilterName : begin
                                    Namespace:=onsStyle;
                                    LocalName:='filter-name';
                               end;
          oatDrawOpacity : begin
                                Namespace:=onsDraw;
                                LocalName:='opacity';
                           end;
          oatStyleEditable : begin
                                  Namespace:=onsStyle;
                                  LocalName:='editable';
                             end;
          oatStyleProtect : begin
                                 Namespace:=onsStyle;
                                 LocalName:='protect';
                            end;
          oatTextDontBalanceTextColumns : begin
                                               Namespace:=onsText;
                                               LocalName:='dont-balance-text-columns';
                                          end;
          oatFoColumnCount : begin
                                  Namespace:=onsFo;
                                  LocalName:='column-count';
                             end;
          oatFoColumnGap : begin
                                Namespace:=onsFo;
                                LocalName:='column-gap';
                           end;
          oatStyleStyle : begin
                               Namespace:=onsStyle;
                               LocalName:='style';
                          end;
          oatStyleWidth : begin
                               Namespace:=onsStyle;
                               LocalName:='width';
                          end;
          oatStyleHeight : begin
                                Namespace:=onsStyle;
                                LocalName:='height';
                           end;
          oatStyleColor : begin
                               Namespace:=onsStyle;
                               LocalName:='color';
                          end;
          oatStyleRelWidth : begin
                                  Namespace:=onsStyle;
                                  LocalName:='rel-width';
                             end;
          oatFoStartIndent : begin
                                  Namespace:=onsFo;
                                  LocalName:='start-indent';
                             end;
          oatFoEndIndent : begin
                                Namespace:=onsFo;
                                LocalName:='end-indent';
                           end;
          oatFoSpaceBefore : begin
                                  Namespace:=onsFo;
                                  LocalName:='space-before';
                             end;
          oatFoSpaceAfter : begin
                                 Namespace:=onsFo;
                                 LocalName:='space-after';
                            end;
          oatTextNoteClass : begin
                                  Namespace:=onsText;
                                  LocalName:='note-class';
                             end;
          oatStyleNumPrefix : begin
                                   Namespace:=onsStyle;
                                   LocalName:='num-prefix';
                              end;
          oatStyleNumSuffix : begin
                                   Namespace:=onsStyle;
                                   LocalName:='num-suffix';
                              end;
          oatStyleNumLetterSync : begin
                                       Namespace:=onsStyle;
                                       LocalName:='num-letter-sync';
                                  end;
          oatStyleNumFormat : begin
                                   Namespace:=onsStyle;
                                   LocalName:='num-format';
                              end;
          oatTextCitationStyleName : begin
                                          Namespace:=onsText;
                                          LocalName:='citation-style-name';
                                     end;
          oatTextCitationBodyStyleName : begin
                                              Namespace:=onsText;
                                              LocalName:='citation-body-style-name';
                                         end;
          oatTextDefaultStyleName : begin
                                         Namespace:=onsText;
                                         LocalName:='default-style-name';
                                    end;
          oatTextMasterPageName : begin
                                       Namespace:=onsText;
                                       LocalName:='master-page-name';
                                  end;
          oatTextStartValue : begin
                                   Namespace:=onsText;
                                   LocalName:='start-value';
                              end;
          oatTextStartNumberingAt : begin
                                         Namespace:=onsText;
                                         LocalName:='start-numbering-at';
                                    end;
          oatTextFootnotesPosition : begin
                                          Namespace:=onsText;
                                          LocalName:='footnotes-position';
                                     end;
          oatStyleRubyPosition : begin
                                      Namespace:=onsStyle;
                                      LocalName:='ruby-position';
                                 end;
          oatStyleRubyAlign : begin
                                   Namespace:=onsStyle;
                                   LocalName:='ruby-align';
                              end;
          oatTableAlign : begin
                               Namespace:=onsTable;
                               LocalName:='align';
                          end;
          oatStyleMayBreakBetweenRows : begin
                                             Namespace:=onsStyle;
                                             LocalName:='may-break-between-rows';
                                        end;
          oatTableBorderModel : begin
                                     Namespace:=onsTable;
                                     LocalName:='border-model';
                                end;
          oatTableDisplay : begin
                                 Namespace:=onsTable;
                                 LocalName:='display';
                            end;
          oatStyleColumnWidth : begin
                                     Namespace:=onsStyle;
                                     LocalName:='column-width';
                                end;
          oatStyleRelColumnWidth : begin
                                        Namespace:=onsStyle;
                                        LocalName:='rel-column-width';
                                   end;
          oatStyleUseOptimalColumnWidth : begin
                                               Namespace:=onsStyle;
                                               LocalName:='use-optimal-column-width';
                                          end;
          oatStyleRowHeight : begin
                                   Namespace:=onsStyle;
                                   LocalName:='row-height';
                              end;
          oatStyleMinRowHeight : begin
                                      Namespace:=onsStyle;
                                      LocalName:='min-row-height';
                                 end;
          oatStyleUseOptimalRowHeight : begin
                                             Namespace:=onsStyle;
                                             LocalName:='use-optimal-row-height';
                                        end;
          oatStyleDirection : begin
                                   Namespace:=onsStyle;
                                   LocalName:='direction';
                              end;
          oatStyleRotationAngle : begin
                                       Namespace:=onsStyle;
                                       LocalName:='rotation-angle';
                                  end;
          oatStyleTextAlignSource : begin
                                         Namespace:=onsStyle;
                                         LocalName:='text-align-source';
                                    end;
          oatStyleGlyphOrientationVertical : begin
                                                  Namespace:=onsStyle;
                                                  LocalName:='glyph-orientation-vertical';
                                             end;
          oatStyleDiagonalTlBr : begin
                                      Namespace:=onsStyle;
                                      LocalName:='diagonal-tl-br';
                                 end;
          oatStyleDiagonalTlBrWidths : begin
                                            Namespace:=onsStyle;
                                            LocalName:='diagonal-tl-br-widths';
                                       end;
          oatStyleDiagonalBlTr : begin
                                      Namespace:=onsStyle;
                                      LocalName:='diagonal-bl-tr';
                                 end;
          oatStyleDiagonalBlTrWidths : begin
                                            Namespace:=onsStyle;
                                            LocalName:='diagonal-bl-tr-widths';
                                       end;
          oatFoWrapOption : begin
                                 Namespace:=onsFo;
                                 LocalName:='wrap-option';
                            end;
          oatStyleRotationAlign : begin
                                       Namespace:=onsStyle;
                                       LocalName:='rotation-align';
                                  end;
          oatStyleCellProtect : begin
                                     Namespace:=onsStyle;
                                     LocalName:='cell-protect';
                                end;
          oatStylePrintContent : begin
                                      Namespace:=onsStyle;
                                      LocalName:='print-content';
                                 end;
          oatStyleDecimalPlaces : begin
                                       Namespace:=onsStyle;
                                       LocalName:='decimal-places';
                                  end;
          oatStyleRepeatContent : begin
                                       Namespace:=onsStyle;
                                       LocalName:='repeat-content';
                                  end;
          oatStyleShrinkToFit : begin
                                     Namespace:=onsStyle;
                                     LocalName:='shrink-to-fit';
                                end;
          oatSvgWidth : begin
                             Namespace:=onsSvg;
                             LocalName:='width';
                        end;
          oatSvgHeight : begin
                              Namespace:=onsSvg;
                              LocalName:='height';
                         end;
          oatStyleRelHeight : begin
                                   Namespace:=onsStyle;
                                   LocalName:='rel-height';
                              end;
          oatStyleVerticalPos : begin
                                     Namespace:=onsStyle;
                                     LocalName:='vertical-pos';
                                end;
          oatSvgY : begin
                         Namespace:=onsSvg;
                         LocalName:='y';
                    end;
          oatStyleVerticalRel : begin
                                     Namespace:=onsStyle;
                                     LocalName:='vertical-rel';
                                end;
          oatTextAnchorType : begin
                                   Namespace:=onsText;
                                   LocalName:='anchor-type';
                              end;
          oatTextAnchorPageNumber : begin
                                         Namespace:=onsText;
                                         LocalName:='anchor-page-number';
                                    end;
          oatDrawStroke : begin
                               Namespace:=onsDraw;
                               LocalName:='stroke';
                          end;
          oatDrawStrokeDash : begin
                                   Namespace:=onsDraw;
                                   LocalName:='stroke-dash';
                              end;
          oatDrawStrokeDashNames : begin
                                        Namespace:=onsDraw;
                                        LocalName:='stroke-dash-names';
                                   end;
          oatSvgStrokeWidth : begin
                                   Namespace:=onsSvg;
                                   LocalName:='stroke-width';
                              end;
          oatSvgStrokeColor : begin
                                   Namespace:=onsSvg;
                                   LocalName:='stroke-color';
                              end;
          oatDrawMarkerStart : begin
                                    Namespace:=onsDraw;
                                    LocalName:='marker-start';
                               end;
          oatDrawMarkerEnd : begin
                                  Namespace:=onsDraw;
                                  LocalName:='marker-end';
                             end;
          oatDrawMarkerStartWidth : begin
                                         Namespace:=onsDraw;
                                         LocalName:='marker-start-width';
                                    end;
          oatDrawMarkerEndWidth : begin
                                       Namespace:=onsDraw;
                                       LocalName:='marker-end-width';
                                  end;
          oatDrawMarkerStartCenter : begin
                                          Namespace:=onsDraw;
                                          LocalName:='marker-start-center';
                                     end;
          oatDrawMarkerEndCenter : begin
                                        Namespace:=onsDraw;
                                        LocalName:='marker-end-center';
                                   end;
          oatSvgStrokeOpacity : begin
                                     Namespace:=onsSvg;
                                     LocalName:='stroke-opacity';
                                end;
          oatDrawStrokeLinejoin : begin
                                       Namespace:=onsDraw;
                                       LocalName:='stroke-linejoin';
                                  end;
          oatSvgStrokeLinecap : begin
                                     Namespace:=onsSvg;
                                     LocalName:='stroke-linecap';
                                end;
          oatDrawSymbolColor : begin
                                    Namespace:=onsDraw;
                                    LocalName:='symbol-color';
                               end;
          oatTextAnimation : begin
                                  Namespace:=onsText;
                                  LocalName:='animation';
                             end;
          oatTextAnimationDirection : begin
                                           Namespace:=onsText;
                                           LocalName:='animation-direction';
                                      end;
          oatTextAnimationStartInside : begin
                                             Namespace:=onsText;
                                             LocalName:='animation-start-inside';
                                        end;
          oatTextAnimationStopInside : begin
                                            Namespace:=onsText;
                                            LocalName:='animation-stop-inside';
                                       end;
          oatTextAnimationRepeat : begin
                                        Namespace:=onsText;
                                        LocalName:='animation-repeat';
                                   end;
          oatTextAnimationDelay : begin
                                       Namespace:=onsText;
                                       LocalName:='animation-delay';
                                  end;
          oatTextAnimationSteps : begin
                                       Namespace:=onsText;
                                       LocalName:='animation-steps';
                                  end;
          oatDrawAutoGrowWidth : begin
                                      Namespace:=onsDraw;
                                      LocalName:='auto-grow-width';
                                 end;
          oatDrawAutoGrowHeight : begin
                                       Namespace:=onsDraw;
                                       LocalName:='auto-grow-height';
                                  end;
          oatDrawFitToSize : begin
                                  Namespace:=onsDraw;
                                  LocalName:='fit-to-size';
                             end;
          oatDrawFitToContour : begin
                                     Namespace:=onsDraw;
                                     LocalName:='fit-to-contour';
                                end;
          oatDrawTextareaVerticalAlign : begin
                                              Namespace:=onsDraw;
                                              LocalName:='textarea-vertical-align';
                                         end;
          oatDrawTextareaHorizontalAlign : begin
                                                Namespace:=onsDraw;
                                                LocalName:='textarea-horizontal-align';
                                           end;
          oatDrawColorMode : begin
                                  Namespace:=onsDraw;
                                  LocalName:='color-mode';
                             end;
          oatDrawColorInversion : begin
                                       Namespace:=onsDraw;
                                       LocalName:='color-inversion';
                                  end;
          oatDrawLuminance : begin
                                  Namespace:=onsDraw;
                                  LocalName:='luminance';
                             end;
          oatDrawContrast : begin
                                 Namespace:=onsDraw;
                                 LocalName:='contrast';
                            end;
          oatDrawGamma : begin
                              Namespace:=onsDraw;
                              LocalName:='gamma';
                         end;
          oatDrawRed : begin
                            Namespace:=onsDraw;
                            LocalName:='red';
                       end;
          oatDrawGreen : begin
                              Namespace:=onsDraw;
                              LocalName:='green';
                         end;
          oatDrawBlue : begin
                             Namespace:=onsDraw;
                             LocalName:='blue';
                        end;
          oatDrawImageOpacity : begin
                                     Namespace:=onsDraw;
                                     LocalName:='image-opacity';
                                end;
          oatDrawShadow : begin
                               Namespace:=onsDraw;
                               LocalName:='shadow';
                          end;
          oatDrawShadowOffsetX : begin
                                      Namespace:=onsDraw;
                                      LocalName:='shadow-offset-x';
                                 end;
          oatDrawShadowOffsetY : begin
                                      Namespace:=onsDraw;
                                      LocalName:='shadow-offset-y';
                                 end;
          oatDrawShadowColor : begin
                                    Namespace:=onsDraw;
                                    LocalName:='shadow-color';
                               end;
          oatDrawShadowOpacity : begin
                                      Namespace:=onsDraw;
                                      LocalName:='shadow-opacity';
                                 end;
          oatDrawStartLineSpacingHorizontal : begin
                                                   Namespace:=onsDraw;
                                                   LocalName:='start-line-spacing-horizontal';
                                              end;
          oatDrawStartLineSpacingVertical : begin
                                                 Namespace:=onsDraw;
                                                 LocalName:='start-line-spacing-vertical';
                                            end;
          oatDrawEndLineSpacingHorizontal : begin
                                                 Namespace:=onsDraw;
                                                 LocalName:='end-line-spacing-horizontal';
                                            end;
          oatDrawEndLineSpacingVertical : begin
                                               Namespace:=onsDraw;
                                               LocalName:='end-line-spacing-vertical';
                                          end;
          oatDrawLineDistance : begin
                                     Namespace:=onsDraw;
                                     LocalName:='line-distance';
                                end;
          oatDrawGuideOverhang : begin
                                      Namespace:=onsDraw;
                                      LocalName:='guide-overhang';
                                 end;
          oatDrawGuideDistance : begin
                                      Namespace:=onsDraw;
                                      LocalName:='guide-distance';
                                 end;
          oatDrawStartGuide : begin
                                   Namespace:=onsDraw;
                                   LocalName:='start-guide';
                              end;
          oatDrawEndGuide : begin
                                 Namespace:=onsDraw;
                                 LocalName:='end-guide';
                            end;
          oatDrawPlacing : begin
                                Namespace:=onsDraw;
                                LocalName:='placing';
                           end;
          oatDrawParallel : begin
                                 Namespace:=onsDraw;
                                 LocalName:='parallel';
                            end;
          oatDrawMeasureAlign : begin
                                     Namespace:=onsDraw;
                                     LocalName:='measure-align';
                                end;
          oatDrawMeasureVerticalAlign : begin
                                             Namespace:=onsDraw;
                                             LocalName:='measure-vertical-align';
                                        end;
          oatDrawUnit : begin
                             Namespace:=onsDraw;
                             LocalName:='unit';
                        end;
          oatDrawShowUnit : begin
                                 Namespace:=onsDraw;
                                 LocalName:='show-unit';
                            end;
          oatDrawDecimalPlaces : begin
                                      Namespace:=onsDraw;
                                      LocalName:='decimal-places';
                                 end;
          oatDrawCaptionType : begin
                                    Namespace:=onsDraw;
                                    LocalName:='caption-type';
                               end;
          oatDrawCaptionAngleType : begin
                                         Namespace:=onsDraw;
                                         LocalName:='caption-angle-type';
                                    end;
          oatDrawCaptionAngle : begin
                                     Namespace:=onsDraw;
                                     LocalName:='caption-angle';
                                end;
          oatDrawCaptionGap : begin
                                   Namespace:=onsDraw;
                                   LocalName:='caption-gap';
                              end;
          oatDrawCaptionEscapeDirection : begin
                                               Namespace:=onsDraw;
                                               LocalName:='caption-escape-direction';
                                          end;
          oatDrawCaptionEscape : begin
                                      Namespace:=onsDraw;
                                      LocalName:='caption-escape';
                                 end;
          oatDrawCaptionLineLength : begin
                                          Namespace:=onsDraw;
                                          LocalName:='caption-line-length';
                                     end;
          oatDrawCaptionFitLineLength : begin
                                             Namespace:=onsDraw;
                                             LocalName:='caption-fit-line-length';
                                        end;
          oatDr3dHorizontalSegments : begin
                                           Namespace:=onsDr3d;
                                           LocalName:='horizontal-segments';
                                      end;
          oatDr3dVerticalSegments : begin
                                         Namespace:=onsDr3d;
                                         LocalName:='vertical-segments';
                                    end;
          oatDr3dEdgeRounding : begin
                                     Namespace:=onsDr3d;
                                     LocalName:='edge-rounding';
                                end;
          oatDr3dEdgeRoundingMode : begin
                                         Namespace:=onsDr3d;
                                         LocalName:='edge-rounding-mode';
                                    end;
          oatDr3dBackScale : begin
                                  Namespace:=onsDr3d;
                                  LocalName:='back-scale';
                             end;
          oatDr3dDepth : begin
                              Namespace:=onsDr3d;
                              LocalName:='depth';
                         end;
          oatDr3dBackfaceCulling : begin
                                        Namespace:=onsDr3d;
                                        LocalName:='backface-culling';
                                   end;
          oatDr3dEndAngle : begin
                                 Namespace:=onsDr3d;
                                 LocalName:='end-angle';
                            end;
          oatDr3dCloseFront : begin
                                   Namespace:=onsDr3d;
                                   LocalName:='close-front';
                              end;
          oatDr3dCloseBack : begin
                                  Namespace:=onsDr3d;
                                  LocalName:='close-back';
                             end;
          oatDr3dLightingMode : begin
                                     Namespace:=onsDr3d;
                                     LocalName:='lighting-mode';
                                end;
          oatDr3dNormalsKind : begin
                                    Namespace:=onsDr3d;
                                    LocalName:='normals-kind';
                               end;
          oatDr3dNormalsDirection : begin
                                         Namespace:=onsDr3d;
                                         LocalName:='normals-direction';
                                    end;
          oatDr3dTextureGenerationModeX : begin
                                               Namespace:=onsDr3d;
                                               LocalName:='texture-generation-mode-x';
                                          end;
          oatDr3dTextureGenerationModeY : begin
                                               Namespace:=onsDr3d;
                                               LocalName:='texture-generation-mode-y';
                                          end;
          oatDr3dTextureKind : begin
                                    Namespace:=onsDr3d;
                                    LocalName:='texture-kind';
                               end;
          oatDr3dTextureFilter : begin
                                      Namespace:=onsDr3d;
                                      LocalName:='texture-filter';
                                 end;
          oatDr3dTextureMode : begin
                                    Namespace:=onsDr3d;
                                    LocalName:='texture-mode';
                               end;
          oatDr3dAmbientColor : begin
                                     Namespace:=onsDr3d;
                                     LocalName:='ambient-color';
                                end;
          oatDr3dEmissiveColor : begin
                                      Namespace:=onsDr3d;
                                      LocalName:='emissive-color';
                                 end;
          oatDr3dSpecularColor : begin
                                      Namespace:=onsDr3d;
                                      LocalName:='specular-color';
                                 end;
          oatDr3dDiffuseColor : begin
                                     Namespace:=onsDr3d;
                                     LocalName:='diffuse-color';
                                end;
          oatDr3dShininess : begin
                                  Namespace:=onsDr3d;
                                  LocalName:='shininess';
                             end;
          oatDr3dShadow : begin
                               Namespace:=onsDr3d;
                               LocalName:='shadow';
                          end;
          oatFoMinWidth : begin
                               Namespace:=onsFo;
                               LocalName:='min-width';
                          end;
          oatFoMinHeight : begin
                                Namespace:=onsFo;
                                LocalName:='min-height';
                           end;
          oatFoMaxHeight : begin
                                Namespace:=onsFo;
                                LocalName:='max-height';
                           end;
          oatFoMaxWidth : begin
                               Namespace:=onsFo;
                               LocalName:='max-width';
                          end;
          oatStyleHorizontalPos : begin
                                       Namespace:=onsStyle;
                                       LocalName:='horizontal-pos';
                                  end;
          oatSvgX : begin
                         Namespace:=onsSvg;
                         LocalName:='x';
                    end;
          oatStyleHorizontalRel : begin
                                       Namespace:=onsStyle;
                                       LocalName:='horizontal-rel';
                                  end;
          oatStyleWrap : begin
                              Namespace:=onsStyle;
                              LocalName:='wrap';
                         end;
          oatStyleWrapDynamicThreshold : begin
                                              Namespace:=onsStyle;
                                              LocalName:='wrap-dynamic-threshold';
                                         end;
          oatStyleNumberWrappedParagraphs : begin
                                                 Namespace:=onsStyle;
                                                 LocalName:='number-wrapped-paragraphs';
                                            end;
          oatStyleWrapContour : begin
                                     Namespace:=onsStyle;
                                     LocalName:='wrap-contour';
                                end;
          oatStyleWrapContourMode : begin
                                         Namespace:=onsStyle;
                                         LocalName:='wrap-contour-mode';
                                    end;
          oatStyleRunThrough : begin
                                    Namespace:=onsStyle;
                                    LocalName:='run-through';
                               end;
          oatStyleFlowWithText : begin
                                      Namespace:=onsStyle;
                                      LocalName:='flow-with-text';
                                 end;
          oatStyleOverflowBehavior : begin
                                          Namespace:=onsStyle;
                                          LocalName:='overflow-behavior';
                                     end;
          oatStyleMirror : begin
                                Namespace:=onsStyle;
                                LocalName:='mirror';
                           end;
          oatFoClip : begin
                           Namespace:=onsFo;
                           LocalName:='clip';
                      end;
          oatDrawWrapInfluenceOnPosition : begin
                                                Namespace:=onsDraw;
                                                LocalName:='wrap-influence-on-position';
                                           end;
          oatDrawFrameDisplayScrollbar : begin
                                              Namespace:=onsDraw;
                                              LocalName:='frame-display-scrollbar';
                                         end;
          oatDrawFrameDisplayBorder : begin
                                           Namespace:=onsDraw;
                                           LocalName:='frame-display-border';
                                      end;
          oatDrawFrameMarginHorizontal : begin
                                              Namespace:=onsDraw;
                                              LocalName:='frame-margin-horizontal';
                                         end;
          oatDrawFrameMarginVertical : begin
                                            Namespace:=onsDraw;
                                            LocalName:='frame-margin-vertical';
                                       end;
          oatDrawVisibleAreaLeft : begin
                                        Namespace:=onsDraw;
                                        LocalName:='visible-area-left';
                                   end;
          oatDrawVisibleAreaTop : begin
                                       Namespace:=onsDraw;
                                       LocalName:='visible-area-top';
                                  end;
          oatDrawVisibleAreaWidth : begin
                                         Namespace:=onsDraw;
                                         LocalName:='visible-area-width';
                                    end;
          oatDrawVisibleAreaHeight : begin
                                          Namespace:=onsDraw;
                                          LocalName:='visible-area-height';
                                     end;
          oatDrawDrawAspect : begin
                                   Namespace:=onsDraw;
                                   LocalName:='draw-aspect';
                              end;
          oatDrawOleDrawAspect : begin
                                      Namespace:=onsDraw;
                                      LocalName:='ole-draw-aspect';
                                 end;
          oatDrawFill : begin
                             Namespace:=onsDraw;
                             LocalName:='fill';
                        end;
          oatDrawFillColor : begin
                                  Namespace:=onsDraw;
                                  LocalName:='fill-color';
                             end;
          oatDrawSecondaryFillColor : begin
                                           Namespace:=onsDraw;
                                           LocalName:='secondary-fill-color';
                                      end;
          oatDrawFillGradientName : begin
                                         Namespace:=onsDraw;
                                         LocalName:='fill-gradient-name';
                                    end;
          oatDrawGradientStepCount : begin
                                          Namespace:=onsDraw;
                                          LocalName:='gradient-step-count';
                                     end;
          oatDrawFillHatchName : begin
                                      Namespace:=onsDraw;
                                      LocalName:='fill-hatch-name';
                                 end;
          oatDrawFillHatchSolid : begin
                                       Namespace:=onsDraw;
                                       LocalName:='fill-hatch-solid';
                                  end;
          oatDrawFillImageName : begin
                                      Namespace:=onsDraw;
                                      LocalName:='fill-image-name';
                                 end;
          oatDrawFillImageWidth : begin
                                       Namespace:=onsDraw;
                                       LocalName:='fill-image-width';
                                  end;
          oatDrawFillImageHeight : begin
                                        Namespace:=onsDraw;
                                        LocalName:='fill-image-height';
                                   end;
          oatDrawFillImageRefPointX : begin
                                           Namespace:=onsDraw;
                                           LocalName:='fill-image-ref-point-x';
                                      end;
          oatDrawFillImageRefPointY : begin
                                           Namespace:=onsDraw;
                                           LocalName:='fill-image-ref-point-y';
                                      end;
          oatDrawFillImageRefPoint : begin
                                          Namespace:=onsDraw;
                                          LocalName:='fill-image-ref-point';
                                     end;
          oatDrawTileRepeatOffset : begin
                                         Namespace:=onsDraw;
                                         LocalName:='tile-repeat-offset';
                                    end;
          oatDrawOpacityName : begin
                                    Namespace:=onsDraw;
                                    LocalName:='opacity-name';
                               end;
          oatSvgFillRule : begin
                                Namespace:=onsSvg;
                                LocalName:='fill-rule';
                           end;
          oatTextConsecutiveNumbering : begin
                                             Namespace:=onsText;
                                             LocalName:='consecutive-numbering';
                                        end;
          oatTextLevel : begin
                              Namespace:=onsText;
                              LocalName:='level';
                         end;
          oatTextStyleName : begin
                                  Namespace:=onsText;
                                  LocalName:='style-name';
                             end;
          oatTextDisplayLevels : begin
                                      Namespace:=onsText;
                                      LocalName:='display-levels';
                                 end;
          oatTextSpaceBefore : begin
                                    Namespace:=onsText;
                                    LocalName:='space-before';
                               end;
          oatTextMinLabelWidth : begin
                                      Namespace:=onsText;
                                      LocalName:='min-label-width';
                                 end;
          oatTextMinLabelDistance : begin
                                         Namespace:=onsText;
                                         LocalName:='min-label-distance';
                                    end;
          oatFoWidth : begin
                            Namespace:=onsFo;
                            LocalName:='width';
                       end;
          oatFoHeight : begin
                             Namespace:=onsFo;
                             LocalName:='height';
                        end;
          oatTextListLevelPositionAndSpaceMode : begin
                                                      Namespace:=onsText;
                                                      LocalName:='list-level-position-and-space-mode';
                                                 end;
          oatTextLabelFollowedBy : begin
                                        Namespace:=onsText;
                                        LocalName:='label-followed-by';
                                   end;
          oatTextListTabStopPosition : begin
                                            Namespace:=onsText;
                                            LocalName:='list-tab-stop-position';
                                       end;
          oatTextBulletChar : begin
                                   Namespace:=onsText;
                                   LocalName:='bullet-char';
                              end;
          oatTextBulletRelativeSize : begin
                                           Namespace:=onsText;
                                           LocalName:='bullet-relative-size';
                                      end;
          oatPresentationTransitionType : begin
                                               Namespace:=onsPresentation;
                                               LocalName:='transition-type';
                                          end;
          oatPresentationTransitionStyle : begin
                                                Namespace:=onsPresentation;
                                                LocalName:='transition-style';
                                           end;
          oatPresentationTransitionSpeed : begin
                                                Namespace:=onsPresentation;
                                                LocalName:='transition-speed';
                                           end;
          oatSmilType : begin
                             Namespace:=onsSmil;
                             LocalName:='type';
                        end;
          oatSmilSubtype : begin
                                Namespace:=onsSmil;
                                LocalName:='subtype';
                           end;
          oatSmilDirection : begin
                                  Namespace:=onsSmil;
                                  LocalName:='direction';
                             end;
          oatSmilFadecolor : begin
                                  Namespace:=onsSmil;
                                  LocalName:='fadeColor';
                             end;
          oatPresentationDuration : begin
                                         Namespace:=onsPresentation;
                                         LocalName:='duration';
                                    end;
          oatPresentationVisibility : begin
                                           Namespace:=onsPresentation;
                                           LocalName:='visibility';
                                      end;
          oatDrawBackgroundSize : begin
                                       Namespace:=onsDraw;
                                       LocalName:='background-size';
                                  end;
          oatPresentationBackgroundObjectsVisible : begin
                                                         Namespace:=onsPresentation;
                                                         LocalName:='background-objects-visible';
                                                    end;
          oatPresentationBackgroundVisible : begin
                                                  Namespace:=onsPresentation;
                                                  LocalName:='background-visible';
                                             end;
          oatPresentationDisplayHeader : begin
                                              Namespace:=onsPresentation;
                                              LocalName:='display-header';
                                         end;
          oatPresentationDisplayFooter : begin
                                              Namespace:=onsPresentation;
                                              LocalName:='display-footer';
                                         end;
          oatPresentationDisplayPageNumber : begin
                                                  Namespace:=onsPresentation;
                                                  LocalName:='display-page-number';
                                             end;
          oatPresentationDisplayDateTime : begin
                                                Namespace:=onsPresentation;
                                                LocalName:='display-date-time';
                                           end;
          oatChartScaleText : begin
                                   Namespace:=onsChart;
                                   LocalName:='scale-text';
                              end;
          oatChartThreeDimensional : begin
                                          Namespace:=onsChart;
                                          LocalName:='three-dimensional';
                                     end;
          oatChartDeep : begin
                              Namespace:=onsChart;
                              LocalName:='deep';
                         end;
          oatChartRightAngledAxes : begin
                                         Namespace:=onsChart;
                                         LocalName:='right-angled-axes';
                                    end;
          oatChartSymbolType : begin
                                    Namespace:=onsChart;
                                    LocalName:='symbol-type';
                               end;
          oatChartSymbolName : begin
                                    Namespace:=onsChart;
                                    LocalName:='symbol-name';
                               end;
          oatChartSymbolWidth : begin
                                     Namespace:=onsChart;
                                     LocalName:='symbol-width';
                                end;
          oatChartSymbolHeight : begin
                                      Namespace:=onsChart;
                                      LocalName:='symbol-height';
                                 end;
          oatChartSortByXValues : begin
                                       Namespace:=onsChart;
                                       LocalName:='sort-by-x-values';
                                  end;
          oatChartVertical : begin
                                  Namespace:=onsChart;
                                  LocalName:='vertical';
                             end;
          oatChartConnectBars : begin
                                     Namespace:=onsChart;
                                     LocalName:='connect-bars';
                                end;
          oatChartGapWidth : begin
                                  Namespace:=onsChart;
                                  LocalName:='gap-width';
                             end;
          oatChartOverlap : begin
                                 Namespace:=onsChart;
                                 LocalName:='overlap';
                            end;
          oatChartGroupBarsPerAxis : begin
                                          Namespace:=onsChart;
                                          LocalName:='group-bars-per-axis';
                                     end;
          oatChartJapaneseCandleStick : begin
                                             Namespace:=onsChart;
                                             LocalName:='japanese-candle-stick';
                                        end;
          oatChartInterpolation : begin
                                       Namespace:=onsChart;
                                       LocalName:='interpolation';
                                  end;
          oatChartSplineOrder : begin
                                     Namespace:=onsChart;
                                     LocalName:='spline-order';
                                end;
          oatChartSplineResolution : begin
                                          Namespace:=onsChart;
                                          LocalName:='spline-resolution';
                                     end;
          oatChartPieOffset : begin
                                   Namespace:=onsChart;
                                   LocalName:='pie-offset';
                              end;
          oatChartAngleOffset : begin
                                     Namespace:=onsChart;
                                     LocalName:='angle-offset';
                                end;
          oatChartHoleSize : begin
                                  Namespace:=onsChart;
                                  LocalName:='hole-size';
                             end;
          oatChartLines : begin
                               Namespace:=onsChart;
                               LocalName:='lines';
                          end;
          oatChartSolidType : begin
                                   Namespace:=onsChart;
                                   LocalName:='solid-type';
                              end;
          oatChartStacked : begin
                                 Namespace:=onsChart;
                                 LocalName:='stacked';
                            end;
          oatChartPercentage : begin
                                    Namespace:=onsChart;
                                    LocalName:='percentage';
                               end;
          oatChartTreatEmptyCells : begin
                                         Namespace:=onsChart;
                                         LocalName:='treat-empty-cells';
                                    end;
          oatChartLinkDataStyleToSource : begin
                                               Namespace:=onsChart;
                                               LocalName:='link-data-style-to-source';
                                          end;
          oatChartLogarithmic : begin
                                     Namespace:=onsChart;
                                     LocalName:='logarithmic';
                                end;
          oatChartMaximum : begin
                                 Namespace:=onsChart;
                                 LocalName:='maximum';
                            end;
          oatChartMinimum : begin
                                 Namespace:=onsChart;
                                 LocalName:='minimum';
                            end;
          oatChartOrigin : begin
                                Namespace:=onsChart;
                                LocalName:='origin';
                           end;
          oatChartIntervalMajor : begin
                                       Namespace:=onsChart;
                                       LocalName:='interval-major';
                                  end;
          oatChartIntervalMinorDivisor : begin
                                              Namespace:=onsChart;
                                              LocalName:='interval-minor-divisor';
                                         end;
          oatChartTickMarksMajorInner : begin
                                             Namespace:=onsChart;
                                             LocalName:='tick-marks-major-inner';
                                        end;
          oatChartTickMarksMajorOuter : begin
                                             Namespace:=onsChart;
                                             LocalName:='tick-marks-major-outer';
                                        end;
          oatChartTickMarksMinorInner : begin
                                             Namespace:=onsChart;
                                             LocalName:='tick-marks-minor-inner';
                                        end;
          oatChartTickMarksMinorOuter : begin
                                             Namespace:=onsChart;
                                             LocalName:='tick-marks-minor-outer';
                                        end;
          oatChartReverseDirection : begin
                                          Namespace:=onsChart;
                                          LocalName:='reverse-direction';
                                     end;
          oatChartDisplayLabel : begin
                                      Namespace:=onsChart;
                                      LocalName:='display-label';
                                 end;
          oatChartTextOverlap : begin
                                     Namespace:=onsChart;
                                     LocalName:='text-overlap';
                                end;
          oatTextLineBreak : begin
                                  Namespace:=onsText;
                                  LocalName:='line-break';
                             end;
          oatChartLabelArrangement : begin
                                          Namespace:=onsChart;
                                          LocalName:='label-arrangement';
                                     end;
          oatChartDataLabelNumber : begin
                                         Namespace:=onsChart;
                                         LocalName:='data-label-number';
                                    end;
          oatChartDataLabelText : begin
                                       Namespace:=onsChart;
                                       LocalName:='data-label-text';
                                  end;
          oatChartDataLabelSymbol : begin
                                         Namespace:=onsChart;
                                         LocalName:='data-label-symbol';
                                    end;
          oatChartLabelPosition : begin
                                       Namespace:=onsChart;
                                       LocalName:='label-position';
                                  end;
          oatChartLabelPositionNegative : begin
                                               Namespace:=onsChart;
                                               LocalName:='label-position-negative';
                                          end;
          oatChartVisible : begin
                                 Namespace:=onsChart;
                                 LocalName:='visible';
                            end;
          oatChartAutoPosition : begin
                                      Namespace:=onsChart;
                                      LocalName:='auto-position';
                                 end;
          oatChartAutoSize : begin
                                  Namespace:=onsChart;
                                  LocalName:='auto-size';
                             end;
          oatChartMeanValue : begin
                                   Namespace:=onsChart;
                                   LocalName:='mean-value';
                              end;
          oatChartErrorCategory : begin
                                       Namespace:=onsChart;
                                       LocalName:='error-category';
                                  end;
          oatChartErrorPercentage : begin
                                         Namespace:=onsChart;
                                         LocalName:='error-percentage';
                                    end;
          oatChartErrorMargin : begin
                                     Namespace:=onsChart;
                                     LocalName:='error-margin';
                                end;
          oatChartErrorLowerLimit : begin
                                         Namespace:=onsChart;
                                         LocalName:='error-lower-limit';
                                    end;
          oatChartErrorUpperLimit : begin
                                         Namespace:=onsChart;
                                         LocalName:='error-upper-limit';
                                    end;
          oatChartErrorUpperIndicator : begin
                                             Namespace:=onsChart;
                                             LocalName:='error-upper-indicator';
                                        end;
          oatChartErrorLowerIndicator : begin
                                             Namespace:=onsChart;
                                             LocalName:='error-lower-indicator';
                                        end;
          oatChartErrorLowerRange : begin
                                         Namespace:=onsChart;
                                         LocalName:='error-lower-range';
                                    end;
          oatChartErrorUpperRange : begin
                                         Namespace:=onsChart;
                                         LocalName:='error-upper-range';
                                    end;
          oatChartSeriesSource : begin
                                      Namespace:=onsChart;
                                      LocalName:='series-source';
                                 end;
          oatChartRegressionType : begin
                                        Namespace:=onsChart;
                                        LocalName:='regression-type';
                                   end;
          oatChartAxisPosition : begin
                                      Namespace:=onsChart;
                                      LocalName:='axis-position';
                                 end;
          oatChartAxisLabelPosition : begin
                                           Namespace:=onsChart;
                                           LocalName:='axis-label-position';
                                      end;
          oatChartTickMarkPosition : begin
                                          Namespace:=onsChart;
                                          LocalName:='tick-mark-position';
                                     end;
          oatChartIncludeHiddenCells : begin
                                            Namespace:=onsChart;
                                            LocalName:='include-hidden-cells';
                                       end;
          oatXhtmlDatatype : begin
                                  Namespace:=onsXhtml;
                                  LocalName:='datatype';
                             end;
          oatXhtmlContent : begin
                                 Namespace:=onsXhtml;
                                 LocalName:='content';
                            end;
          oatXhtmlAbout : begin
                               Namespace:=onsXhtml;
                               LocalName:='about';
                          end;
          oatXhtmlProperty : begin
                                  Namespace:=onsXhtml;
                                  LocalName:='property';
                             end;
          oatTextClassNames : begin
                                   Namespace:=onsText;
                                   LocalName:='class-names';
                              end;
          oatTextCondStyleName : begin
                                      Namespace:=onsText;
                                      LocalName:='cond-style-name';
                                 end;
          oatTextId : begin
                           Namespace:=onsText;
                           LocalName:='id';
                      end;
          oatTextC : begin
                          Namespace:=onsText;
                          LocalName:='c';
                     end;
          oatTextTabRef : begin
                               Namespace:=onsText;
                               LocalName:='tab-ref';
                          end;
          oatTextName : begin
                             Namespace:=onsText;
                             LocalName:='name';
                        end;
          oatTextLabel : begin
                              Namespace:=onsText;
                              LocalName:='label';
                         end;
          oatTextOutlineLevel : begin
                                     Namespace:=onsText;
                                     LocalName:='outline-level';
                                end;
          oatTextRestartNumbering : begin
                                         Namespace:=onsText;
                                         LocalName:='restart-numbering';
                                    end;
          oatTextIsListHeader : begin
                                     Namespace:=onsText;
                                     LocalName:='is-list-header';
                                end;
          oatTextContinueNumbering : begin
                                          Namespace:=onsText;
                                          LocalName:='continue-numbering';
                                     end;
          oatTextContinueList : begin
                                     Namespace:=onsText;
                                     LocalName:='continue-list';
                                end;
          oatTextStyleOverride : begin
                                      Namespace:=onsText;
                                      LocalName:='style-override';
                                 end;
          oatTextListId : begin
                               Namespace:=onsText;
                               LocalName:='list-id';
                          end;
          oatTableName : begin
                              Namespace:=onsTable;
                              LocalName:='name';
                         end;
          oatTableStyleName : begin
                                   Namespace:=onsTable;
                                   LocalName:='style-name';
                              end;
          oatTableTemplateName : begin
                                      Namespace:=onsTable;
                                      LocalName:='template-name';
                                 end;
          oatTableUseFirstRowStyles : begin
                                           Namespace:=onsTable;
                                           LocalName:='use-first-row-styles';
                                      end;
          oatTableUseLastRowStyles : begin
                                          Namespace:=onsTable;
                                          LocalName:='use-last-row-styles';
                                     end;
          oatTableUseFirstColumnStyles : begin
                                              Namespace:=onsTable;
                                              LocalName:='use-first-column-styles';
                                         end;
          oatTableUseLastColumnStyles : begin
                                             Namespace:=onsTable;
                                             LocalName:='use-last-column-styles';
                                        end;
          oatTableUseBandingRowsStyles : begin
                                              Namespace:=onsTable;
                                              LocalName:='use-banding-rows-styles';
                                         end;
          oatTableUseBandingColumnsStyles : begin
                                                 Namespace:=onsTable;
                                                 LocalName:='use-banding-columns-styles';
                                            end;
          oatTableProtected : begin
                                   Namespace:=onsTable;
                                   LocalName:='protected';
                              end;
          oatTableProtectionKey : begin
                                       Namespace:=onsTable;
                                       LocalName:='protection-key';
                                  end;
          oatTableProtectionKeyDigestAlgorithm : begin
                                                      Namespace:=onsTable;
                                                      LocalName:='protection-key-digest-algorithm';
                                                 end;
          oatTablePrint : begin
                               Namespace:=onsTable;
                               LocalName:='print';
                          end;
          oatTablePrintRanges : begin
                                     Namespace:=onsTable;
                                     LocalName:='print-ranges';
                                end;
          oatTableIsSubTable : begin
                                    Namespace:=onsTable;
                                    LocalName:='is-sub-table';
                               end;
          oatTableMode : begin
                              Namespace:=onsTable;
                              LocalName:='mode';
                         end;
          oatTableTableName : begin
                                   Namespace:=onsTable;
                                   LocalName:='table-name';
                              end;
          oatTableFilterName : begin
                                    Namespace:=onsTable;
                                    LocalName:='filter-name';
                               end;
          oatTableFilterOptions : begin
                                       Namespace:=onsTable;
                                       LocalName:='filter-options';
                                  end;
          oatTableRefreshDelay : begin
                                      Namespace:=onsTable;
                                      LocalName:='refresh-delay';
                                 end;
          oatOfficeName : begin
                               Namespace:=onsOffice;
                               LocalName:='name';
                          end;
          oatOfficeConversionMode : begin
                                         Namespace:=onsOffice;
                                         LocalName:='conversion-mode';
                                    end;
          oatOfficeDdeApplication : begin
                                         Namespace:=onsOffice;
                                         LocalName:='dde-application';
                                    end;
          oatOfficeDdeTopic : begin
                                   Namespace:=onsOffice;
                                   LocalName:='dde-topic';
                              end;
          oatOfficeDdeItem : begin
                                  Namespace:=onsOffice;
                                  LocalName:='dde-item';
                             end;
          oatOfficeAutomaticUpdate : begin
                                          Namespace:=onsOffice;
                                          LocalName:='automatic-update';
                                     end;
          oatTableScenarioRanges : begin
                                        Namespace:=onsTable;
                                        LocalName:='scenario-ranges';
                                   end;
          oatTableIsActive : begin
                                  Namespace:=onsTable;
                                  LocalName:='is-active';
                             end;
          oatTableDisplayBorder : begin
                                       Namespace:=onsTable;
                                       LocalName:='display-border';
                                  end;
          oatTableBorderColor : begin
                                     Namespace:=onsTable;
                                     LocalName:='border-color';
                                end;
          oatTableCopyBack : begin
                                  Namespace:=onsTable;
                                  LocalName:='copy-back';
                             end;
          oatTableCopyStyles : begin
                                    Namespace:=onsTable;
                                    LocalName:='copy-styles';
                               end;
          oatTableCopyFormulas : begin
                                      Namespace:=onsTable;
                                      LocalName:='copy-formulas';
                                 end;
          oatTableComment : begin
                                 Namespace:=onsTable;
                                 LocalName:='comment';
                            end;
          oatFormAutomaticFocus : begin
                                       Namespace:=onsForm;
                                       LocalName:='automatic-focus';
                                  end;
          oatFormApplyDesignMode : begin
                                        Namespace:=onsForm;
                                        LocalName:='apply-design-mode';
                                   end;
          oatFormName : begin
                             Namespace:=onsForm;
                             LocalName:='name';
                        end;
          oatFormControlImplementation : begin
                                              Namespace:=onsForm;
                                              LocalName:='control-implementation';
                                         end;
          oatOfficeTargetFrame : begin
                                      Namespace:=onsOffice;
                                      LocalName:='target-frame';
                                 end;
          oatFormMethod : begin
                               Namespace:=onsForm;
                               LocalName:='method';
                          end;
          oatFormEnctype : begin
                                Namespace:=onsForm;
                                LocalName:='enctype';
                           end;
          oatFormAllowDeletes : begin
                                     Namespace:=onsForm;
                                     LocalName:='allow-deletes';
                                end;
          oatFormAllowInserts : begin
                                     Namespace:=onsForm;
                                     LocalName:='allow-inserts';
                                end;
          oatFormAllowUpdates : begin
                                     Namespace:=onsForm;
                                     LocalName:='allow-updates';
                                end;
          oatFormApplyFilter : begin
                                    Namespace:=onsForm;
                                    LocalName:='apply-filter';
                               end;
          oatFormCommandType : begin
                                    Namespace:=onsForm;
                                    LocalName:='command-type';
                               end;
          oatFormCommand : begin
                                Namespace:=onsForm;
                                LocalName:='command';
                           end;
          oatFormDatasource : begin
                                   Namespace:=onsForm;
                                   LocalName:='datasource';
                              end;
          oatFormMasterFields : begin
                                     Namespace:=onsForm;
                                     LocalName:='master-fields';
                                end;
          oatFormDetailFields : begin
                                     Namespace:=onsForm;
                                     LocalName:='detail-fields';
                                end;
          oatFormEscapeProcessing : begin
                                         Namespace:=onsForm;
                                         LocalName:='escape-processing';
                                    end;
          oatFormFilter : begin
                               Namespace:=onsForm;
                               LocalName:='filter';
                          end;
          oatFormIgnoreResult : begin
                                     Namespace:=onsForm;
                                     LocalName:='ignore-result';
                                end;
          oatFormNavigationMode : begin
                                       Namespace:=onsForm;
                                       LocalName:='navigation-mode';
                                  end;
          oatFormOrder : begin
                              Namespace:=onsForm;
                              LocalName:='order';
                         end;
          oatFormTabCycle : begin
                                 Namespace:=onsForm;
                                 LocalName:='tab-cycle';
                            end;
          oatFormPropertyName : begin
                                     Namespace:=onsForm;
                                     LocalName:='property-name';
                                end;
          oatOfficeValueType : begin
                                    Namespace:=onsOffice;
                                    LocalName:='value-type';
                               end;
          oatOfficeValue : begin
                                Namespace:=onsOffice;
                                LocalName:='value';
                           end;
          oatOfficeCurrency : begin
                                   Namespace:=onsOffice;
                                   LocalName:='currency';
                              end;
          oatOfficeDateValue : begin
                                    Namespace:=onsOffice;
                                    LocalName:='date-value';
                               end;
          oatOfficeTimeValue : begin
                                    Namespace:=onsOffice;
                                    LocalName:='time-value';
                               end;
          oatOfficeBooleanValue : begin
                                       Namespace:=onsOffice;
                                       LocalName:='boolean-value';
                                  end;
          oatOfficeStringValue : begin
                                      Namespace:=onsOffice;
                                      LocalName:='string-value';
                                 end;
          oatFormId : begin
                           Namespace:=onsForm;
                           LocalName:='id';
                      end;
          oatXformsBind : begin
                               Namespace:=onsXforms;
                               LocalName:='bind';
                          end;
          oatFormCurrentValue : begin
                                     Namespace:=onsForm;
                                     LocalName:='current-value';
                                end;
          oatFormDisabled : begin
                                 Namespace:=onsForm;
                                 LocalName:='disabled';
                            end;
          oatFormMaxLength : begin
                                  Namespace:=onsForm;
                                  LocalName:='max-length';
                             end;
          oatFormPrintable : begin
                                  Namespace:=onsForm;
                                  LocalName:='printable';
                             end;
          oatFormReadonly : begin
                                 Namespace:=onsForm;
                                 LocalName:='readonly';
                            end;
          oatFormTabIndex : begin
                                 Namespace:=onsForm;
                                 LocalName:='tab-index';
                            end;
          oatFormTabStop : begin
                                Namespace:=onsForm;
                                LocalName:='tab-stop';
                           end;
          oatFormTitle : begin
                              Namespace:=onsForm;
                              LocalName:='title';
                         end;
          oatFormValue : begin
                              Namespace:=onsForm;
                              LocalName:='value';
                         end;
          oatFormConvertEmptyToNull : begin
                                           Namespace:=onsForm;
                                           LocalName:='convert-empty-to-null';
                                      end;
          oatFormDataField : begin
                                  Namespace:=onsForm;
                                  LocalName:='data-field';
                             end;
          oatFormLinkedCell : begin
                                   Namespace:=onsForm;
                                   LocalName:='linked-cell';
                              end;
          oatFormSpinButton : begin
                                   Namespace:=onsForm;
                                   LocalName:='spin-button';
                              end;
          oatFormRepeat : begin
                               Namespace:=onsForm;
                               LocalName:='repeat';
                          end;
          oatFormDelayForRepeat : begin
                                       Namespace:=onsForm;
                                       LocalName:='delay-for-repeat';
                                  end;
          oatFormMaxValue : begin
                                 Namespace:=onsForm;
                                 LocalName:='max-value';
                            end;
          oatFormMinValue : begin
                                 Namespace:=onsForm;
                                 LocalName:='min-value';
                            end;
          oatFormValidation : begin
                                   Namespace:=onsForm;
                                   LocalName:='validation';
                              end;
          oatFormDropdown : begin
                                 Namespace:=onsForm;
                                 LocalName:='dropdown';
                            end;
          oatFormSize : begin
                             Namespace:=onsForm;
                             LocalName:='size';
                        end;
          oatFormListSource : begin
                                   Namespace:=onsForm;
                                   LocalName:='list-source';
                              end;
          oatFormListSourceType : begin
                                       Namespace:=onsForm;
                                       LocalName:='list-source-type';
                                  end;
          oatFormSourceCellRange : begin
                                        Namespace:=onsForm;
                                        LocalName:='source-cell-range';
                                   end;
          oatFormAutoComplete : begin
                                     Namespace:=onsForm;
                                     LocalName:='auto-complete';
                                end;
          oatFormLabel : begin
                              Namespace:=onsForm;
                              LocalName:='label';
                         end;
          oatFormBoundColumn : begin
                                    Namespace:=onsForm;
                                    LocalName:='bound-column';
                               end;
          oatFormListLinkageType : begin
                                        Namespace:=onsForm;
                                        LocalName:='list-linkage-type';
                                   end;
          oatFormMultiple : begin
                                 Namespace:=onsForm;
                                 LocalName:='multiple';
                            end;
          oatFormXformsListSource : begin
                                         Namespace:=onsForm;
                                         LocalName:='xforms-list-source';
                                    end;
          oatFormCurrentSelected : begin
                                        Namespace:=onsForm;
                                        LocalName:='current-selected';
                                   end;
          oatFormSelected : begin
                                 Namespace:=onsForm;
                                 LocalName:='selected';
                            end;
          oatFormVisualEffect : begin
                                     Namespace:=onsForm;
                                     LocalName:='visual-effect';
                                end;
          oatFormImagePosition : begin
                                      Namespace:=onsForm;
                                      LocalName:='image-position';
                                 end;
          oatFormImageAlign : begin
                                   Namespace:=onsForm;
                                   LocalName:='image-align';
                              end;
          oatFormCurrentState : begin
                                     Namespace:=onsForm;
                                     LocalName:='current-state';
                                end;
          oatFormIsTristate : begin
                                   Namespace:=onsForm;
                                   LocalName:='is-tristate';
                              end;
          oatFormState : begin
                              Namespace:=onsForm;
                              LocalName:='state';
                         end;
          oatFormEchoChar : begin
                                 Namespace:=onsForm;
                                 LocalName:='echo-char';
                            end;
          oatFormFor : begin
                            Namespace:=onsForm;
                            LocalName:='for';
                       end;
          oatFormMultiLine : begin
                                  Namespace:=onsForm;
                                  LocalName:='multi-line';
                             end;
          oatFormButtonType : begin
                                   Namespace:=onsForm;
                                   LocalName:='button-type';
                              end;
          oatFormImageData : begin
                                  Namespace:=onsForm;
                                  LocalName:='image-data';
                             end;
          oatFormDefaultButton : begin
                                      Namespace:=onsForm;
                                      LocalName:='default-button';
                                 end;
          oatFormToggle : begin
                               Namespace:=onsForm;
                               LocalName:='toggle';
                          end;
          oatFormFocusOnClick : begin
                                     Namespace:=onsForm;
                                     LocalName:='focus-on-click';
                                end;
          oatFormXformsSubmission : begin
                                         Namespace:=onsForm;
                                         LocalName:='xforms-submission';
                                    end;
          oatFormTextStyleName : begin
                                      Namespace:=onsForm;
                                      LocalName:='text-style-name';
                                 end;
          oatFormStepSize : begin
                                 Namespace:=onsForm;
                                 LocalName:='step-size';
                            end;
          oatFormPageStepSize : begin
                                     Namespace:=onsForm;
                                     LocalName:='page-step-size';
                                end;
          oatFormOrientation : begin
                                    Namespace:=onsForm;
                                    LocalName:='orientation';
                               end;
          oatDrawCornerRadius : begin
                                     Namespace:=onsDraw;
                                     LocalName:='corner-radius';
                                end;
          oatSvgRx : begin
                          Namespace:=onsSvg;
                          LocalName:='rx';
                     end;
          oatSvgRy : begin
                          Namespace:=onsSvg;
                          LocalName:='ry';
                     end;
          oatDrawZIndex : begin
                               Namespace:=onsDraw;
                               LocalName:='z-index';
                          end;
          oatDrawId : begin
                           Namespace:=onsDraw;
                           LocalName:='id';
                      end;
          oatDrawLayer : begin
                              Namespace:=onsDraw;
                              LocalName:='layer';
                         end;
          oatDrawStyleName : begin
                                  Namespace:=onsDraw;
                                  LocalName:='style-name';
                             end;
          oatDrawClassNames : begin
                                   Namespace:=onsDraw;
                                   LocalName:='class-names';
                              end;
          oatPresentationStyleName : begin
                                          Namespace:=onsPresentation;
                                          LocalName:='style-name';
                                     end;
          oatPresentationClassNames : begin
                                           Namespace:=onsPresentation;
                                           LocalName:='class-names';
                                      end;
          oatDrawTransform : begin
                                  Namespace:=onsDraw;
                                  LocalName:='transform';
                             end;
          oatDrawName : begin
                             Namespace:=onsDraw;
                             LocalName:='name';
                        end;
          oatTableEndCellAddress : begin
                                        Namespace:=onsTable;
                                        LocalName:='end-cell-address';
                                   end;
          oatTableEndX : begin
                              Namespace:=onsTable;
                              LocalName:='end-x';
                         end;
          oatTableEndY : begin
                              Namespace:=onsTable;
                              LocalName:='end-y';
                         end;
          oatTableTableBackground : begin
                                         Namespace:=onsTable;
                                         LocalName:='table-background';
                                    end;
          oatDrawTextStyleName : begin
                                      Namespace:=onsDraw;
                                      LocalName:='text-style-name';
                                 end;
          oatDrawCaptionId : begin
                                  Namespace:=onsDraw;
                                  LocalName:='caption-id';
                             end;
          oatDrawAlign : begin
                              Namespace:=onsDraw;
                              LocalName:='align';
                         end;
          oatDrawEscapeDirection : begin
                                        Namespace:=onsDraw;
                                        LocalName:='escape-direction';
                                   end;
          oatSvgX1 : begin
                          Namespace:=onsSvg;
                          LocalName:='x1';
                     end;
          oatSvgY1 : begin
                          Namespace:=onsSvg;
                          LocalName:='y1';
                     end;
          oatSvgX2 : begin
                          Namespace:=onsSvg;
                          LocalName:='x2';
                     end;
          oatSvgY2 : begin
                          Namespace:=onsSvg;
                          LocalName:='y2';
                     end;
          oatDrawPoints : begin
                               Namespace:=onsDraw;
                               LocalName:='points';
                          end;
          oatSvgViewbox : begin
                               Namespace:=onsSvg;
                               LocalName:='viewBox';
                          end;
          oatDrawSharpness : begin
                                  Namespace:=onsDraw;
                                  LocalName:='sharpness';
                             end;
          oatDrawConcave : begin
                                Namespace:=onsDraw;
                                LocalName:='concave';
                           end;
          oatDrawCorners : begin
                                Namespace:=onsDraw;
                                LocalName:='corners';
                           end;
          oatSvgD : begin
                         Namespace:=onsSvg;
                         LocalName:='d';
                    end;
          oatSvgR : begin
                         Namespace:=onsSvg;
                         LocalName:='r';
                    end;
          oatSvgCx : begin
                          Namespace:=onsSvg;
                          LocalName:='cx';
                     end;
          oatSvgCy : begin
                          Namespace:=onsSvg;
                          LocalName:='cy';
                     end;
          oatDrawKind : begin
                             Namespace:=onsDraw;
                             LocalName:='kind';
                        end;
          oatDrawStartAngle : begin
                                   Namespace:=onsDraw;
                                   LocalName:='start-angle';
                              end;
          oatDrawEndAngle : begin
                                 Namespace:=onsDraw;
                                 LocalName:='end-angle';
                            end;
          oatDrawPageNumber : begin
                                   Namespace:=onsDraw;
                                   LocalName:='page-number';
                              end;
          oatPresentationClass : begin
                                      Namespace:=onsPresentation;
                                      LocalName:='class';
                                 end;
          oatPresentationPlaceholder : begin
                                            Namespace:=onsPresentation;
                                            LocalName:='placeholder';
                                       end;
          oatPresentationUserTransformed : begin
                                                Namespace:=onsPresentation;
                                                LocalName:='user-transformed';
                                           end;
          oatDrawCopyOf : begin
                               Namespace:=onsDraw;
                               LocalName:='copy-of';
                          end;
          oatDrawChainNextName : begin
                                      Namespace:=onsDraw;
                                      LocalName:='chain-next-name';
                                 end;
          oatDrawFilterName : begin
                                   Namespace:=onsDraw;
                                   LocalName:='filter-name';
                              end;
          oatDrawNotifyOnUpdateOfRanges : begin
                                               Namespace:=onsDraw;
                                               LocalName:='notify-on-update-of-ranges';
                                          end;
          oatDrawClassId : begin
                                Namespace:=onsDraw;
                                LocalName:='class-id';
                           end;
          oatDrawCode : begin
                             Namespace:=onsDraw;
                             LocalName:='code';
                        end;
          oatDrawObject : begin
                               Namespace:=onsDraw;
                               LocalName:='object';
                          end;
          oatDrawArchive : begin
                                Namespace:=onsDraw;
                                LocalName:='archive';
                           end;
          oatDrawMayScript : begin
                                  Namespace:=onsDraw;
                                  LocalName:='may-script';
                             end;
          oatDrawValue : begin
                              Namespace:=onsDraw;
                              LocalName:='value';
                         end;
          oatDrawFrameName : begin
                                  Namespace:=onsDraw;
                                  LocalName:='frame-name';
                             end;
          oatDrawMimeType : begin
                                 Namespace:=onsDraw;
                                 LocalName:='mime-type';
                            end;
          oatDrawNohref : begin
                               Namespace:=onsDraw;
                               LocalName:='nohref';
                          end;
          oatDrawRecreateOnEdit : begin
                                       Namespace:=onsDraw;
                                       LocalName:='recreate-on-edit';
                                  end;
          oatDrawCaptionPointX : begin
                                      Namespace:=onsDraw;
                                      LocalName:='caption-point-x';
                                 end;
          oatDrawCaptionPointY : begin
                                      Namespace:=onsDraw;
                                      LocalName:='caption-point-y';
                                 end;
          oatDrawType : begin
                             Namespace:=onsDraw;
                             LocalName:='type';
                        end;
          oatDrawStartShape : begin
                                   Namespace:=onsDraw;
                                   LocalName:='start-shape';
                              end;
          oatDrawStartGluePoint : begin
                                       Namespace:=onsDraw;
                                       LocalName:='start-glue-point';
                                  end;
          oatDrawEndShape : begin
                                 Namespace:=onsDraw;
                                 LocalName:='end-shape';
                            end;
          oatDrawEndGluePoint : begin
                                     Namespace:=onsDraw;
                                     LocalName:='end-glue-point';
                                end;
          oatDrawLineSkew : begin
                                 Namespace:=onsDraw;
                                 LocalName:='line-skew';
                            end;
          oatDrawControl : begin
                                Namespace:=onsDraw;
                                LocalName:='control';
                           end;
          oatDr3dVrp : begin
                            Namespace:=onsDr3d;
                            LocalName:='vrp';
                       end;
          oatDr3dVpn : begin
                            Namespace:=onsDr3d;
                            LocalName:='vpn';
                       end;
          oatDr3dVup : begin
                            Namespace:=onsDr3d;
                            LocalName:='vup';
                       end;
          oatDr3dProjection : begin
                                   Namespace:=onsDr3d;
                                   LocalName:='projection';
                              end;
          oatDr3dDistance : begin
                                 Namespace:=onsDr3d;
                                 LocalName:='distance';
                            end;
          oatDr3dFocalLength : begin
                                    Namespace:=onsDr3d;
                                    LocalName:='focal-length';
                               end;
          oatDr3dShadowSlant : begin
                                    Namespace:=onsDr3d;
                                    LocalName:='shadow-slant';
                               end;
          oatDr3dShadeMode : begin
                                  Namespace:=onsDr3d;
                                  LocalName:='shade-mode';
                             end;
          oatDr3dTransform : begin
                                  Namespace:=onsDr3d;
                                  LocalName:='transform';
                             end;
          oatDr3dDirection : begin
                                  Namespace:=onsDr3d;
                                  LocalName:='direction';
                             end;
          oatDr3dEnabled : begin
                                Namespace:=onsDr3d;
                                LocalName:='enabled';
                           end;
          oatDr3dSpecular : begin
                                 Namespace:=onsDr3d;
                                 LocalName:='specular';
                            end;
          oatDr3dCenter : begin
                               Namespace:=onsDr3d;
                               LocalName:='center';
                          end;
          oatDr3dSize : begin
                             Namespace:=onsDr3d;
                             LocalName:='size';
                        end;
          oatDr3dMinEdge : begin
                                Namespace:=onsDr3d;
                                LocalName:='min-edge';
                           end;
          oatDr3dMaxEdge : begin
                                Namespace:=onsDr3d;
                                LocalName:='max-edge';
                           end;
          oatDrawEngine : begin
                               Namespace:=onsDraw;
                               LocalName:='engine';
                          end;
          oatDrawData : begin
                             Namespace:=onsDraw;
                             LocalName:='data';
                        end;
          oatDrawMirrorVertical : begin
                                       Namespace:=onsDraw;
                                       LocalName:='mirror-vertical';
                                  end;
          oatDrawMirrorHorizontal : begin
                                         Namespace:=onsDraw;
                                         LocalName:='mirror-horizontal';
                                    end;
          oatDrawTextRotateAngle : begin
                                        Namespace:=onsDraw;
                                        LocalName:='text-rotate-angle';
                                   end;
          oatDrawExtrusionAllowed : begin
                                         Namespace:=onsDraw;
                                         LocalName:='extrusion-allowed';
                                    end;
          oatDrawTextPathAllowed : begin
                                        Namespace:=onsDraw;
                                        LocalName:='text-path-allowed';
                                   end;
          oatDrawConcentricGradientFillAllowed : begin
                                                      Namespace:=onsDraw;
                                                      LocalName:='concentric-gradient-fill-allowed';
                                                 end;
          oatDrawExtrusion : begin
                                  Namespace:=onsDraw;
                                  LocalName:='extrusion';
                             end;
          oatDrawExtrusionBrightness : begin
                                            Namespace:=onsDraw;
                                            LocalName:='extrusion-brightness';
                                       end;
          oatDrawExtrusionDepth : begin
                                       Namespace:=onsDraw;
                                       LocalName:='extrusion-depth';
                                  end;
          oatDrawExtrusionDiffusion : begin
                                           Namespace:=onsDraw;
                                           LocalName:='extrusion-diffusion';
                                      end;
          oatDrawExtrusionNumberOfLineSegments : begin
                                                      Namespace:=onsDraw;
                                                      LocalName:='extrusion-number-of-line-segments';
                                                 end;
          oatDrawExtrusionLightFace : begin
                                           Namespace:=onsDraw;
                                           LocalName:='extrusion-light-face';
                                      end;
          oatDrawExtrusionFirstLightHarsh : begin
                                                 Namespace:=onsDraw;
                                                 LocalName:='extrusion-first-light-harsh';
                                            end;
          oatDrawExtrusionSecondLightHarsh : begin
                                                  Namespace:=onsDraw;
                                                  LocalName:='extrusion-second-light-harsh';
                                             end;
          oatDrawExtrusionFirstLightLevel : begin
                                                 Namespace:=onsDraw;
                                                 LocalName:='extrusion-first-light-level';
                                            end;
          oatDrawExtrusionSecondLightLevel : begin
                                                  Namespace:=onsDraw;
                                                  LocalName:='extrusion-second-light-level';
                                             end;
          oatDrawExtrusionFirstLightDirection : begin
                                                     Namespace:=onsDraw;
                                                     LocalName:='extrusion-first-light-direction';
                                                end;
          oatDrawExtrusionSecondLightDirection : begin
                                                      Namespace:=onsDraw;
                                                      LocalName:='extrusion-second-light-direction';
                                                 end;
          oatDrawExtrusionMetal : begin
                                       Namespace:=onsDraw;
                                       LocalName:='extrusion-metal';
                                  end;
          oatDrawExtrusionRotationAngle : begin
                                               Namespace:=onsDraw;
                                               LocalName:='extrusion-rotation-angle';
                                          end;
          oatDrawExtrusionRotationCenter : begin
                                                Namespace:=onsDraw;
                                                LocalName:='extrusion-rotation-center';
                                           end;
          oatDrawExtrusionShininess : begin
                                           Namespace:=onsDraw;
                                           LocalName:='extrusion-shininess';
                                      end;
          oatDrawExtrusionSkew : begin
                                      Namespace:=onsDraw;
                                      LocalName:='extrusion-skew';
                                 end;
          oatDrawExtrusionSpecularity : begin
                                             Namespace:=onsDraw;
                                             LocalName:='extrusion-specularity';
                                        end;
          oatDrawExtrusionViewpoint : begin
                                           Namespace:=onsDraw;
                                           LocalName:='extrusion-viewpoint';
                                      end;
          oatDrawExtrusionOrigin : begin
                                        Namespace:=onsDraw;
                                        LocalName:='extrusion-origin';
                                   end;
          oatDrawExtrusionColor : begin
                                       Namespace:=onsDraw;
                                       LocalName:='extrusion-color';
                                  end;
          oatDrawEnhancedPath : begin
                                     Namespace:=onsDraw;
                                     LocalName:='enhanced-path';
                                end;
          oatDrawPathStretchpointX : begin
                                          Namespace:=onsDraw;
                                          LocalName:='path-stretchpoint-x';
                                     end;
          oatDrawPathStretchpointY : begin
                                          Namespace:=onsDraw;
                                          LocalName:='path-stretchpoint-y';
                                     end;
          oatDrawTextAreas : begin
                                  Namespace:=onsDraw;
                                  LocalName:='text-areas';
                             end;
          oatDrawGluePoints : begin
                                   Namespace:=onsDraw;
                                   LocalName:='glue-points';
                              end;
          oatDrawGluePointType : begin
                                      Namespace:=onsDraw;
                                      LocalName:='glue-point-type';
                                 end;
          oatDrawGluePointLeavingDirections : begin
                                                   Namespace:=onsDraw;
                                                   LocalName:='glue-point-leaving-directions';
                                              end;
          oatDrawTextPath : begin
                                 Namespace:=onsDraw;
                                 LocalName:='text-path';
                            end;
          oatDrawTextPathMode : begin
                                     Namespace:=onsDraw;
                                     LocalName:='text-path-mode';
                                end;
          oatDrawTextPathScale : begin
                                      Namespace:=onsDraw;
                                      LocalName:='text-path-scale';
                                 end;
          oatDrawTextPathSameLetterHeights : begin
                                                  Namespace:=onsDraw;
                                                  LocalName:='text-path-same-letter-heights';
                                             end;
          oatDrawModifiers : begin
                                  Namespace:=onsDraw;
                                  LocalName:='modifiers';
                             end;
          oatDrawFormula : begin
                                Namespace:=onsDraw;
                                LocalName:='formula';
                           end;
          oatDrawHandleMirrorVertical : begin
                                             Namespace:=onsDraw;
                                             LocalName:='handle-mirror-vertical';
                                        end;
          oatDrawHandleMirrorHorizontal : begin
                                               Namespace:=onsDraw;
                                               LocalName:='handle-mirror-horizontal';
                                          end;
          oatDrawHandleSwitched : begin
                                       Namespace:=onsDraw;
                                       LocalName:='handle-switched';
                                  end;
          oatDrawHandlePosition : begin
                                       Namespace:=onsDraw;
                                       LocalName:='handle-position';
                                  end;
          oatDrawHandleRangeXMinimum : begin
                                            Namespace:=onsDraw;
                                            LocalName:='handle-range-x-minimum';
                                       end;
          oatDrawHandleRangeXMaximum : begin
                                            Namespace:=onsDraw;
                                            LocalName:='handle-range-x-maximum';
                                       end;
          oatDrawHandleRangeYMinimum : begin
                                            Namespace:=onsDraw;
                                            LocalName:='handle-range-y-minimum';
                                       end;
          oatDrawHandleRangeYMaximum : begin
                                            Namespace:=onsDraw;
                                            LocalName:='handle-range-y-maximum';
                                       end;
          oatDrawHandlePolar : begin
                                    Namespace:=onsDraw;
                                    LocalName:='handle-polar';
                               end;
          oatDrawHandleRadiusRangeMinimum : begin
                                                 Namespace:=onsDraw;
                                                 LocalName:='handle-radius-range-minimum';
                                            end;
          oatDrawHandleRadiusRangeMaximum : begin
                                                 Namespace:=onsDraw;
                                                 LocalName:='handle-radius-range-maximum';
                                            end;
          oatOfficeTitle : begin
                                Namespace:=onsOffice;
                                LocalName:='title';
                           end;
          oatOfficeServerMap : begin
                                    Namespace:=onsOffice;
                                    LocalName:='server-map';
                               end;
          oatTableNumberColumnsRepeated : begin
                                               Namespace:=onsTable;
                                               LocalName:='number-columns-repeated';
                                          end;
          oatTableVisibility : begin
                                    Namespace:=onsTable;
                                    LocalName:='visibility';
                               end;
          oatTableDefaultCellStyleName : begin
                                              Namespace:=onsTable;
                                              LocalName:='default-cell-style-name';
                                         end;
          oatTableNumberRowsRepeated : begin
                                            Namespace:=onsTable;
                                            LocalName:='number-rows-repeated';
                                       end;
          oatTableContentValidationName : begin
                                               Namespace:=onsTable;
                                               LocalName:='content-validation-name';
                                          end;
          oatTableFormula : begin
                                 Namespace:=onsTable;
                                 LocalName:='formula';
                            end;
          oatTableProtect : begin
                                 Namespace:=onsTable;
                                 LocalName:='protect';
                            end;
          oatTableNumberColumnsSpanned : begin
                                              Namespace:=onsTable;
                                              LocalName:='number-columns-spanned';
                                         end;
          oatTableNumberRowsSpanned : begin
                                           Namespace:=onsTable;
                                           LocalName:='number-rows-spanned';
                                      end;
          oatTableNumberMatrixColumnsSpanned : begin
                                                    Namespace:=onsTable;
                                                    LocalName:='number-matrix-columns-spanned';
                                               end;
          oatTableNumberMatrixRowsSpanned : begin
                                                 Namespace:=onsTable;
                                                 LocalName:='number-matrix-rows-spanned';
                                            end;
          oatTableLastColumnSpanned : begin
                                           Namespace:=onsTable;
                                           LocalName:='last-column-spanned';
                                      end;
          oatTableLastRowSpanned : begin
                                        Namespace:=onsTable;
                                        LocalName:='last-row-spanned';
                                   end;
          oatOfficeDisplay : begin
                                  Namespace:=onsOffice;
                                  LocalName:='display';
                             end;
          oatTableCellRangeAddress : begin
                                          Namespace:=onsTable;
                                          LocalName:='cell-range-address';
                                     end;
          oatTableDirection : begin
                                   Namespace:=onsTable;
                                   LocalName:='direction';
                              end;
          oatTableContainsError : begin
                                       Namespace:=onsTable;
                                       LocalName:='contains-error';
                                  end;
          oatTableMarkedInvalid : begin
                                       Namespace:=onsTable;
                                       LocalName:='marked-invalid';
                                  end;
          oatTableIndex : begin
                               Namespace:=onsTable;
                               LocalName:='index';
                          end;
          oatTableBaseCellAddress : begin
                                         Namespace:=onsTable;
                                         LocalName:='base-cell-address';
                                    end;
          oatTableRangeUsableAs : begin
                                       Namespace:=onsTable;
                                       LocalName:='range-usable-as';
                                  end;
          oatTableExpression : begin
                                    Namespace:=onsTable;
                                    LocalName:='expression';
                               end;
          oatTextProtected : begin
                                  Namespace:=onsText;
                                  LocalName:='protected';
                             end;
          oatTextProtectionKey : begin
                                      Namespace:=onsText;
                                      LocalName:='protection-key';
                                 end;
          oatTextProtectionKeyDigestAlgorithm : begin
                                                     Namespace:=onsText;
                                                     LocalName:='protection-key-digest-algorithm';
                                                end;
          oatTextSectionName : begin
                                    Namespace:=onsText;
                                    LocalName:='section-name';
                               end;
          oatTextFilterName : begin
                                   Namespace:=onsText;
                                   LocalName:='filter-name';
                              end;
          oatTextUseOutlineLevel : begin
                                        Namespace:=onsText;
                                        LocalName:='use-outline-level';
                                   end;
          oatTextUseIndexMarks : begin
                                      Namespace:=onsText;
                                      LocalName:='use-index-marks';
                                 end;
          oatTextUseIndexSourceStyles : begin
                                             Namespace:=onsText;
                                             LocalName:='use-index-source-styles';
                                        end;
          oatTextIndexScope : begin
                                   Namespace:=onsText;
                                   LocalName:='index-scope';
                              end;
          oatTextRelativeTabStopPosition : begin
                                                Namespace:=onsText;
                                                LocalName:='relative-tab-stop-position';
                                           end;
          oatStyleLeaderChar : begin
                                    Namespace:=onsStyle;
                                    LocalName:='leader-char';
                               end;
          oatTextUseCaption : begin
                                   Namespace:=onsText;
                                   LocalName:='use-caption';
                              end;
          oatTextCaptionSequenceName : begin
                                            Namespace:=onsText;
                                            LocalName:='caption-sequence-name';
                                       end;
          oatTextCaptionSequenceFormat : begin
                                              Namespace:=onsText;
                                              LocalName:='caption-sequence-format';
                                         end;
          oatTextUseSpreadsheetObjects : begin
                                              Namespace:=onsText;
                                              LocalName:='use-spreadsheet-objects';
                                         end;
          oatTextUseMathObjects : begin
                                       Namespace:=onsText;
                                       LocalName:='use-math-objects';
                                  end;
          oatTextUseDrawObjects : begin
                                       Namespace:=onsText;
                                       LocalName:='use-draw-objects';
                                  end;
          oatTextUseChartObjects : begin
                                        Namespace:=onsText;
                                        LocalName:='use-chart-objects';
                                   end;
          oatTextUseOtherObjects : begin
                                        Namespace:=onsText;
                                        LocalName:='use-other-objects';
                                   end;
          oatTextUseGraphics : begin
                                    Namespace:=onsText;
                                    LocalName:='use-graphics';
                               end;
          oatTextUseTables : begin
                                  Namespace:=onsText;
                                  LocalName:='use-tables';
                             end;
          oatTextUseFloatingFrames : begin
                                          Namespace:=onsText;
                                          LocalName:='use-floating-frames';
                                     end;
          oatTextUseObjects : begin
                                   Namespace:=onsText;
                                   LocalName:='use-objects';
                              end;
          oatTextCopyOutlineLevels : begin
                                          Namespace:=onsText;
                                          LocalName:='copy-outline-levels';
                                     end;
          oatTextIndexName : begin
                                  Namespace:=onsText;
                                  LocalName:='index-name';
                             end;
          oatTextIgnoreCase : begin
                                   Namespace:=onsText;
                                   LocalName:='ignore-case';
                              end;
          oatTextMainEntryStyleName : begin
                                           Namespace:=onsText;
                                           LocalName:='main-entry-style-name';
                                      end;
          oatTextAlphabeticalSeparators : begin
                                               Namespace:=onsText;
                                               LocalName:='alphabetical-separators';
                                          end;
          oatTextCombineEntries : begin
                                       Namespace:=onsText;
                                       LocalName:='combine-entries';
                                  end;
          oatTextCombineEntriesWithDash : begin
                                               Namespace:=onsText;
                                               LocalName:='combine-entries-with-dash';
                                          end;
          oatTextCombineEntriesWithPp : begin
                                             Namespace:=onsText;
                                             LocalName:='combine-entries-with-pp';
                                        end;
          oatTextUseKeysAsEntries : begin
                                         Namespace:=onsText;
                                         LocalName:='use-keys-as-entries';
                                    end;
          oatTextCapitalizeEntries : begin
                                          Namespace:=onsText;
                                          LocalName:='capitalize-entries';
                                     end;
          oatTextCommaSeparated : begin
                                       Namespace:=onsText;
                                       LocalName:='comma-separated';
                                  end;
          oatTextSortAlgorithm : begin
                                      Namespace:=onsText;
                                      LocalName:='sort-algorithm';
                                 end;
          oatTextBibliographyType : begin
                                         Namespace:=onsText;
                                         LocalName:='bibliography-type';
                                    end;
          oatTextBibliographyDataField : begin
                                              Namespace:=onsText;
                                              LocalName:='bibliography-data-field';
                                         end;
          oatTextChangeId : begin
                                 Namespace:=onsText;
                                 LocalName:='change-id';
                            end;
          oatTextFixed : begin
                              Namespace:=onsText;
                              LocalName:='fixed';
                         end;
          oatTextDateValue : begin
                                  Namespace:=onsText;
                                  LocalName:='date-value';
                             end;
          oatTextDateAdjust : begin
                                   Namespace:=onsText;
                                   LocalName:='date-adjust';
                              end;
          oatTextTimeValue : begin
                                  Namespace:=onsText;
                                  LocalName:='time-value';
                             end;
          oatTextTimeAdjust : begin
                                   Namespace:=onsText;
                                   LocalName:='time-adjust';
                              end;
          oatTextPageAdjust : begin
                                   Namespace:=onsText;
                                   LocalName:='page-adjust';
                              end;
          oatTextSelectPage : begin
                                   Namespace:=onsText;
                                   LocalName:='select-page';
                              end;
          oatTextStringValue : begin
                                    Namespace:=onsText;
                                    LocalName:='string-value';
                               end;
          oatTextFormula : begin
                                Namespace:=onsText;
                                LocalName:='formula';
                           end;
          oatTextDescription : begin
                                    Namespace:=onsText;
                                    LocalName:='description';
                               end;
          oatTextRefName : begin
                                Namespace:=onsText;
                                LocalName:='ref-name';
                           end;
          oatTextDuration : begin
                                 Namespace:=onsText;
                                 LocalName:='duration';
                            end;
          oatTextTableName : begin
                                  Namespace:=onsText;
                                  LocalName:='table-name';
                             end;
          oatTextTableType : begin
                                  Namespace:=onsText;
                                  LocalName:='table-type';
                             end;
          oatTextDatabaseName : begin
                                     Namespace:=onsText;
                                     LocalName:='database-name';
                                end;
          oatTextColumnName : begin
                                   Namespace:=onsText;
                                   LocalName:='column-name';
                              end;
          oatTextRowNumber : begin
                                  Namespace:=onsText;
                                  LocalName:='row-number';
                             end;
          oatTextValue : begin
                              Namespace:=onsText;
                              LocalName:='value';
                         end;
          oatTextActive : begin
                               Namespace:=onsText;
                               LocalName:='active';
                          end;
          oatTextPlaceholderType : begin
                                        Namespace:=onsText;
                                        LocalName:='placeholder-type';
                                   end;
          oatTextStringValueIfTrue : begin
                                          Namespace:=onsText;
                                          LocalName:='string-value-if-true';
                                     end;
          oatTextStringValueIfFalse : begin
                                           Namespace:=onsText;
                                           LocalName:='string-value-if-false';
                                      end;
          oatTextCurrentValue : begin
                                     Namespace:=onsText;
                                     LocalName:='current-value';
                                end;
          oatTextIsHidden : begin
                                 Namespace:=onsText;
                                 LocalName:='is-hidden';
                            end;
          oatTextReferenceFormat : begin
                                        Namespace:=onsText;
                                        LocalName:='reference-format';
                                   end;
          oatTextConnectionName : begin
                                       Namespace:=onsText;
                                       LocalName:='connection-name';
                                  end;
          oatTextKind : begin
                             Namespace:=onsText;
                             LocalName:='kind';
                        end;
          oatTextKey1 : begin
                             Namespace:=onsText;
                             LocalName:='key1';
                        end;
          oatTextKey2 : begin
                             Namespace:=onsText;
                             LocalName:='key2';
                        end;
          oatTextStringValuePhonetic : begin
                                            Namespace:=onsText;
                                            LocalName:='string-value-phonetic';
                                       end;
          oatTextKey1Phonetic : begin
                                     Namespace:=onsText;
                                     LocalName:='key1-phonetic';
                                end;
          oatTextKey2Phonetic : begin
                                     Namespace:=onsText;
                                     LocalName:='key2-phonetic';
                                end;
          oatTextMainEntry : begin
                                  Namespace:=onsText;
                                  LocalName:='main-entry';
                             end;
          oatTextVisitedStyleName : begin
                                         Namespace:=onsText;
                                         LocalName:='visited-style-name';
                                    end;
          oatStyleCondition : begin
                                   Namespace:=onsStyle;
                                   LocalName:='condition';
                              end;
          oatStyleApplyStyleName : begin
                                        Namespace:=onsStyle;
                                        LocalName:='apply-style-name';
                                   end;
          oatStyleBaseCellAddress : begin
                                         Namespace:=onsStyle;
                                         LocalName:='base-cell-address';
                                    end;
          oatNumberLanguage : begin
                                   Namespace:=onsNumber;
                                   LocalName:='language';
                              end;
          oatNumberCountry : begin
                                  Namespace:=onsNumber;
                                  LocalName:='country';
                             end;
          oatNumberScript : begin
                                 Namespace:=onsNumber;
                                 LocalName:='script';
                            end;
          oatNumberRfcLanguageTag : begin
                                         Namespace:=onsNumber;
                                         LocalName:='rfc-language-tag';
                                    end;
          oatNumberTitle : begin
                                Namespace:=onsNumber;
                                LocalName:='title';
                           end;
          oatStyleVolatile : begin
                                  Namespace:=onsStyle;
                                  LocalName:='volatile';
                             end;
          oatNumberTransliterationFormat : begin
                                                Namespace:=onsNumber;
                                                LocalName:='transliteration-format';
                                           end;
          oatNumberTransliterationLanguage : begin
                                                  Namespace:=onsNumber;
                                                  LocalName:='transliteration-language';
                                             end;
          oatNumberTransliterationCountry : begin
                                                 Namespace:=onsNumber;
                                                 LocalName:='transliteration-country';
                                            end;
          oatNumberTransliterationStyle : begin
                                               Namespace:=onsNumber;
                                               LocalName:='transliteration-style';
                                          end;
          oatNumberDecimalReplacement : begin
                                             Namespace:=onsNumber;
                                             LocalName:='decimal-replacement';
                                        end;
          oatNumberDisplayFactor : begin
                                        Namespace:=onsNumber;
                                        LocalName:='display-factor';
                                   end;
          oatNumberDecimalPlaces : begin
                                        Namespace:=onsNumber;
                                        LocalName:='decimal-places';
                                   end;
          oatNumberMinIntegerDigits : begin
                                           Namespace:=onsNumber;
                                           LocalName:='min-integer-digits';
                                      end;
          oatNumberGrouping : begin
                                   Namespace:=onsNumber;
                                   LocalName:='grouping';
                              end;
          oatNumberPosition : begin
                                   Namespace:=onsNumber;
                                   LocalName:='position';
                              end;
          oatNumberMinExponentDigits : begin
                                            Namespace:=onsNumber;
                                            LocalName:='min-exponent-digits';
                                       end;
          oatNumberMinNumeratorDigits : begin
                                             Namespace:=onsNumber;
                                             LocalName:='min-numerator-digits';
                                        end;
          oatNumberMinDenominatorDigits : begin
                                               Namespace:=onsNumber;
                                               LocalName:='min-denominator-digits';
                                          end;
          oatNumberDenominatorValue : begin
                                           Namespace:=onsNumber;
                                           LocalName:='denominator-value';
                                      end;
          oatNumberAutomaticOrder : begin
                                         Namespace:=onsNumber;
                                         LocalName:='automatic-order';
                                    end;
          oatNumberFormatSource : begin
                                       Namespace:=onsNumber;
                                       LocalName:='format-source';
                                  end;
          oatNumberStyle : begin
                                Namespace:=onsNumber;
                                LocalName:='style';
                           end;
          oatNumberCalendar : begin
                                   Namespace:=onsNumber;
                                   LocalName:='calendar';
                              end;
          oatNumberTextual : begin
                                  Namespace:=onsNumber;
                                  LocalName:='textual';
                             end;
          oatNumberPossessiveForm : begin
                                         Namespace:=onsNumber;
                                         LocalName:='possessive-form';
                                    end;
          oatNumberTruncateOnOverflow : begin
                                             Namespace:=onsNumber;
                                             LocalName:='truncate-on-overflow';
                                        end;
          oatFoPageWidth : begin
                                Namespace:=onsFo;
                                LocalName:='page-width';
                           end;
          oatFoPageHeight : begin
                                 Namespace:=onsFo;
                                 LocalName:='page-height';
                            end;
          oatStylePaperTrayName : begin
                                       Namespace:=onsStyle;
                                       LocalName:='paper-tray-name';
                                  end;
          oatStylePrintOrientation : begin
                                          Namespace:=onsStyle;
                                          LocalName:='print-orientation';
                                     end;
          oatStyleRegisterTruthRefStyleName : begin
                                                   Namespace:=onsStyle;
                                                   LocalName:='register-truth-ref-style-name';
                                              end;
          oatStylePrint : begin
                               Namespace:=onsStyle;
                               LocalName:='print';
                          end;
          oatStylePrintPageOrder : begin
                                        Namespace:=onsStyle;
                                        LocalName:='print-page-order';
                                   end;
          oatStyleFirstPageNumber : begin
                                         Namespace:=onsStyle;
                                         LocalName:='first-page-number';
                                    end;
          oatStyleScaleTo : begin
                                 Namespace:=onsStyle;
                                 LocalName:='scale-to';
                            end;
          oatStyleScaleToPages : begin
                                      Namespace:=onsStyle;
                                      LocalName:='scale-to-pages';
                                 end;
          oatStyleTableCentering : begin
                                        Namespace:=onsStyle;
                                        LocalName:='table-centering';
                                   end;
          oatStyleFootnoteMaxHeight : begin
                                           Namespace:=onsStyle;
                                           LocalName:='footnote-max-height';
                                      end;
          oatStyleLayoutGridMode : begin
                                        Namespace:=onsStyle;
                                        LocalName:='layout-grid-mode';
                                   end;
          oatStyleLayoutGridStandardMode : begin
                                                Namespace:=onsStyle;
                                                LocalName:='layout-grid-standard-mode';
                                           end;
          oatStyleLayoutGridBaseHeight : begin
                                              Namespace:=onsStyle;
                                              LocalName:='layout-grid-base-height';
                                         end;
          oatStyleLayoutGridRubyHeight : begin
                                              Namespace:=onsStyle;
                                              LocalName:='layout-grid-ruby-height';
                                         end;
          oatStyleLayoutGridLines : begin
                                         Namespace:=onsStyle;
                                         LocalName:='layout-grid-lines';
                                    end;
          oatStyleLayoutGridBaseWidth : begin
                                             Namespace:=onsStyle;
                                             LocalName:='layout-grid-base-width';
                                        end;
          oatStyleLayoutGridColor : begin
                                         Namespace:=onsStyle;
                                         LocalName:='layout-grid-color';
                                    end;
          oatStyleLayoutGridRubyBelow : begin
                                             Namespace:=onsStyle;
                                             LocalName:='layout-grid-ruby-below';
                                        end;
          oatStyleLayoutGridPrint : begin
                                         Namespace:=onsStyle;
                                         LocalName:='layout-grid-print';
                                    end;
          oatStyleLayoutGridDisplay : begin
                                           Namespace:=onsStyle;
                                           LocalName:='layout-grid-display';
                                      end;
          oatStyleLayoutGridSnapTo : begin
                                          Namespace:=onsStyle;
                                          LocalName:='layout-grid-snap-to';
                                     end;
          oatStyleLineStyle : begin
                                   Namespace:=onsStyle;
                                   LocalName:='line-style';
                              end;
          oatStyleAdjustment : begin
                                    Namespace:=onsStyle;
                                    LocalName:='adjustment';
                               end;
          oatStyleDistanceBeforeSep : begin
                                           Namespace:=onsStyle;
                                           LocalName:='distance-before-sep';
                                      end;
          oatStyleDistanceAfterSep : begin
                                          Namespace:=onsStyle;
                                          LocalName:='distance-after-sep';
                                     end;
          oatStyleDynamicSpacing : begin
                                        Namespace:=onsStyle;
                                        LocalName:='dynamic-spacing';
                                   end;
          oatTextPrefix : begin
                               Namespace:=onsText;
                               LocalName:='prefix';
                          end;
          oatTextSuffix : begin
                               Namespace:=onsText;
                               LocalName:='suffix';
                          end;
          oatTextNumberedEntries : begin
                                        Namespace:=onsText;
                                        LocalName:='numbered-entries';
                                   end;
          oatTextSortByPosition : begin
                                       Namespace:=onsText;
                                       LocalName:='sort-by-position';
                                  end;
          oatTextKey : begin
                            Namespace:=onsText;
                            LocalName:='key';
                       end;
          oatTextSortAscending : begin
                                      Namespace:=onsText;
                                      LocalName:='sort-ascending';
                                 end;
          oatTextIncrement : begin
                                  Namespace:=onsText;
                                  LocalName:='increment';
                             end;
          oatTextNumberPosition : begin
                                       Namespace:=onsText;
                                       LocalName:='number-position';
                                  end;
          oatTextOffset : begin
                               Namespace:=onsText;
                               LocalName:='offset';
                          end;
          oatTextCountEmptyLines : begin
                                        Namespace:=onsText;
                                        LocalName:='count-empty-lines';
                                   end;
          oatTextCountInTextBoxes : begin
                                         Namespace:=onsText;
                                         LocalName:='count-in-text-boxes';
                                    end;
          oatTextRestartOnPage : begin
                                      Namespace:=onsText;
                                      LocalName:='restart-on-page';
                                 end;
          oatDrawDisplayName : begin
                                    Namespace:=onsDraw;
                                    LocalName:='display-name';
                               end;
          oatDrawStyle : begin
                              Namespace:=onsDraw;
                              LocalName:='style';
                         end;
          oatDrawCx : begin
                           Namespace:=onsDraw;
                           LocalName:='cx';
                      end;
          oatDrawCy : begin
                           Namespace:=onsDraw;
                           LocalName:='cy';
                      end;
          oatDrawAngle : begin
                              Namespace:=onsDraw;
                              LocalName:='angle';
                         end;
          oatDrawBorder : begin
                               Namespace:=onsDraw;
                               LocalName:='border';
                          end;
          oatDrawStartColor : begin
                                   Namespace:=onsDraw;
                                   LocalName:='start-color';
                              end;
          oatDrawEndColor : begin
                                 Namespace:=onsDraw;
                                 LocalName:='end-color';
                            end;
          oatDrawStartIntensity : begin
                                       Namespace:=onsDraw;
                                       LocalName:='start-intensity';
                                  end;
          oatDrawEndIntensity : begin
                                     Namespace:=onsDraw;
                                     LocalName:='end-intensity';
                                end;
          oatSvgGradientunits : begin
                                     Namespace:=onsSvg;
                                     LocalName:='gradientUnits';
                                end;
          oatSvgGradienttransform : begin
                                         Namespace:=onsSvg;
                                         LocalName:='gradientTransform';
                                    end;
          oatSvgSpreadmethod : begin
                                    Namespace:=onsSvg;
                                    LocalName:='spreadMethod';
                               end;
          oatSvgOffset : begin
                              Namespace:=onsSvg;
                              LocalName:='offset';
                         end;
          oatSvgStopColor : begin
                                 Namespace:=onsSvg;
                                 LocalName:='stop-color';
                            end;
          oatSvgStopOpacity : begin
                                   Namespace:=onsSvg;
                                   LocalName:='stop-opacity';
                              end;
          oatSvgFx : begin
                          Namespace:=onsSvg;
                          LocalName:='fx';
                     end;
          oatSvgFy : begin
                          Namespace:=onsSvg;
                          LocalName:='fy';
                     end;
          oatDrawColor : begin
                              Namespace:=onsDraw;
                              LocalName:='color';
                         end;
          oatDrawDistance : begin
                                 Namespace:=onsDraw;
                                 LocalName:='distance';
                            end;
          oatDrawRotation : begin
                                 Namespace:=onsDraw;
                                 LocalName:='rotation';
                            end;
          oatDrawDots1 : begin
                              Namespace:=onsDraw;
                              LocalName:='dots1';
                         end;
          oatDrawDots1Length : begin
                                    Namespace:=onsDraw;
                                    LocalName:='dots1-length';
                               end;
          oatDrawDots2 : begin
                              Namespace:=onsDraw;
                              LocalName:='dots2';
                         end;
          oatDrawDots2Length : begin
                                    Namespace:=onsDraw;
                                    LocalName:='dots2-length';
                               end;
          oatDrawStart : begin
                              Namespace:=onsDraw;
                              LocalName:='start';
                         end;
          oatDrawEnd : begin
                            Namespace:=onsDraw;
                            LocalName:='end';
                       end;
          oatPresentationObject : begin
                                       Namespace:=onsPresentation;
                                       LocalName:='object';
                                  end;
          oatTableFirstRowStartColumn : begin
                                             Namespace:=onsTable;
                                             LocalName:='first-row-start-column';
                                        end;
          oatTableFirstRowEndColumn : begin
                                           Namespace:=onsTable;
                                           LocalName:='first-row-end-column';
                                      end;
          oatTableLastRowStartColumn : begin
                                            Namespace:=onsTable;
                                            LocalName:='last-row-start-column';
                                       end;
          oatTableLastRowEndColumn : begin
                                          Namespace:=onsTable;
                                          LocalName:='last-row-end-column';
                                     end;
          oatTableParagraphStyleName : begin
                                            Namespace:=onsTable;
                                            LocalName:='paragraph-style-name';
                                       end;
          oatStylePageUsage : begin
                                   Namespace:=onsStyle;
                                   LocalName:='page-usage';
                              end;
          oatStylePageLayoutName : begin
                                        Namespace:=onsStyle;
                                        LocalName:='page-layout-name';
                                   end;
          oatStyleDisplay : begin
                                 Namespace:=onsStyle;
                                 LocalName:='display';
                            end;
          oatTextTrackChanges : begin
                                     Namespace:=onsText;
                                     LocalName:='track-changes';
                                end;
          oatTextDisplayOutlineLevel : begin
                                            Namespace:=onsText;
                                            LocalName:='display-outline-level';
                                       end;
          oatTextSeparationCharacter : begin
                                            Namespace:=onsText;
                                            LocalName:='separation-character';
                                       end;
          oatDrawProtected : begin
                                  Namespace:=onsDraw;
                                  LocalName:='protected';
                             end;
          oatDrawDisplay : begin
                                Namespace:=onsDraw;
                                LocalName:='display';
                           end;
          oatSmilTargetelement : begin
                                      Namespace:=onsSmil;
                                      LocalName:='targetElement';
                                 end;
          oatAnimSubItem : begin
                                Namespace:=onsAnim;
                                LocalName:='sub-item';
                           end;
          oatSmilAttributename : begin
                                      Namespace:=onsSmil;
                                      LocalName:='attributeName';
                                 end;
          oatSmilTo : begin
                           Namespace:=onsSmil;
                           LocalName:='to';
                      end;
          oatSmilValues : begin
                               Namespace:=onsSmil;
                               LocalName:='values';
                          end;
          oatAnimFormula : begin
                                Namespace:=onsAnim;
                                LocalName:='formula';
                           end;
          oatSmilFrom : begin
                             Namespace:=onsSmil;
                             LocalName:='from';
                        end;
          oatSmilBy : begin
                           Namespace:=onsSmil;
                           LocalName:='by';
                      end;
          oatSmilCalcmode : begin
                                 Namespace:=onsSmil;
                                 LocalName:='calcMode';
                            end;
          oatSmilKeytimes : begin
                                 Namespace:=onsSmil;
                                 LocalName:='keyTimes';
                            end;
          oatSmilKeysplines : begin
                                   Namespace:=onsSmil;
                                   LocalName:='keySplines';
                              end;
          oatSmilBegin : begin
                              Namespace:=onsSmil;
                              LocalName:='begin';
                         end;
          oatSmilEnd : begin
                            Namespace:=onsSmil;
                            LocalName:='end';
                       end;
          oatSmilDur : begin
                            Namespace:=onsSmil;
                            LocalName:='dur';
                       end;
          oatSmilRepeatdur : begin
                                  Namespace:=onsSmil;
                                  LocalName:='repeatDur';
                             end;
          oatSmilRepeatcount : begin
                                    Namespace:=onsSmil;
                                    LocalName:='repeatCount';
                               end;
          oatSmilRestart : begin
                                Namespace:=onsSmil;
                                LocalName:='restart';
                           end;
          oatSmilRestartdefault : begin
                                       Namespace:=onsSmil;
                                       LocalName:='restartDefault';
                                  end;
          oatSmilFill : begin
                             Namespace:=onsSmil;
                             LocalName:='fill';
                        end;
          oatSmilFilldefault : begin
                                    Namespace:=onsSmil;
                                    LocalName:='fillDefault';
                               end;
          oatSmilAccelerate : begin
                                   Namespace:=onsSmil;
                                   LocalName:='accelerate';
                              end;
          oatSmilDecelerate : begin
                                   Namespace:=onsSmil;
                                   LocalName:='decelerate';
                              end;
          oatSmilAutoreverse : begin
                                    Namespace:=onsSmil;
                                    LocalName:='autoReverse';
                               end;
          oatSmilAccumulate : begin
                                   Namespace:=onsSmil;
                                   LocalName:='accumulate';
                              end;
          oatSmilAdditive : begin
                                 Namespace:=onsSmil;
                                 LocalName:='additive';
                            end;
          oatSvgPath : begin
                            Namespace:=onsSvg;
                            LocalName:='path';
                       end;
          oatSvgOrigin : begin
                              Namespace:=onsSvg;
                              LocalName:='origin';
                         end;
          oatAnimColorInterpolation : begin
                                           Namespace:=onsAnim;
                                           LocalName:='color-interpolation';
                                      end;
          oatAnimColorInterpolationDirection : begin
                                                    Namespace:=onsAnim;
                                                    LocalName:='color-interpolation-direction';
                                               end;
          oatSvgType : begin
                            Namespace:=onsSvg;
                            LocalName:='type';
                       end;
          oatSmilMode : begin
                             Namespace:=onsSmil;
                             LocalName:='mode';
                        end;
          oatPresentationNodeType : begin
                                         Namespace:=onsPresentation;
                                         LocalName:='node-type';
                                    end;
          oatPresentationPresetId : begin
                                         Namespace:=onsPresentation;
                                         LocalName:='preset-id';
                                    end;
          oatPresentationPresetSubType : begin
                                              Namespace:=onsPresentation;
                                              LocalName:='preset-sub-type';
                                         end;
          oatPresentationPresetClass : begin
                                            Namespace:=onsPresentation;
                                            LocalName:='preset-class';
                                       end;
          oatPresentationMasterElement : begin
                                              Namespace:=onsPresentation;
                                              LocalName:='master-element';
                                         end;
          oatPresentationGroupId : begin
                                        Namespace:=onsPresentation;
                                        LocalName:='group-id';
                                   end;
          oatAnimId : begin
                           Namespace:=onsAnim;
                           LocalName:='id';
                      end;
          oatSmilEndsync : begin
                                Namespace:=onsSmil;
                                LocalName:='endsync';
                           end;
          oatAnimIterateType : begin
                                    Namespace:=onsAnim;
                                    LocalName:='iterate-type';
                               end;
          oatAnimIterateInterval : begin
                                        Namespace:=onsAnim;
                                        LocalName:='iterate-interval';
                                   end;
          oatAnimAudioLevel : begin
                                   Namespace:=onsAnim;
                                   LocalName:='audio-level';
                              end;
          oatAnimCommand : begin
                                Namespace:=onsAnim;
                                LocalName:='command';
                           end;
          oatAnimName : begin
                             Namespace:=onsAnim;
                             LocalName:='name';
                        end;
          oatAnimValue : begin
                              Namespace:=onsAnim;
                              LocalName:='value';
                         end;
          oatPresentationUseHeaderName : begin
                                              Namespace:=onsPresentation;
                                              LocalName:='use-header-name';
                                         end;
          oatPresentationUseFooterName : begin
                                              Namespace:=onsPresentation;
                                              LocalName:='use-footer-name';
                                         end;
          oatPresentationUseDateTimeName : begin
                                                Namespace:=onsPresentation;
                                                LocalName:='use-date-time-name';
                                           end;
          oatPresentationPresentationPageLayoutName : begin
                                                           Namespace:=onsPresentation;
                                                           LocalName:='presentation-page-layout-name';
                                                      end;
          oatTextGlobal : begin
                               Namespace:=onsText;
                               LocalName:='global';
                          end;
          oatTextUseSoftPageBreaks : begin
                                          Namespace:=onsText;
                                          LocalName:='use-soft-page-breaks';
                                     end;
          oatTableCaseSensitive : begin
                                       Namespace:=onsTable;
                                       LocalName:='case-sensitive';
                                  end;
          oatTablePrecisionAsShown : begin
                                          Namespace:=onsTable;
                                          LocalName:='precision-as-shown';
                                     end;
          oatTableSearchCriteriaMustApplyToWholeCell : begin
                                                            Namespace:=onsTable;
                                                            LocalName:='search-criteria-must-apply-to-whole-cell';
                                                       end;
          oatTableAutomaticFindLabels : begin
                                             Namespace:=onsTable;
                                             LocalName:='automatic-find-labels';
                                        end;
          oatTableUseRegularExpressions : begin
                                               Namespace:=onsTable;
                                               LocalName:='use-regular-expressions';
                                          end;
          oatTableUseWildcards : begin
                                      Namespace:=onsTable;
                                      LocalName:='use-wildcards';
                                 end;
          oatTableNullYear : begin
                                  Namespace:=onsTable;
                                  LocalName:='null-year';
                             end;
          oatTableValueType : begin
                                   Namespace:=onsTable;
                                   LocalName:='value-type';
                              end;
          oatTableDateValue : begin
                                   Namespace:=onsTable;
                                   LocalName:='date-value';
                              end;
          oatTableStatus : begin
                                Namespace:=onsTable;
                                LocalName:='status';
                           end;
          oatTableSteps : begin
                               Namespace:=onsTable;
                               LocalName:='steps';
                          end;
          oatTableMaximumDifference : begin
                                           Namespace:=onsTable;
                                           LocalName:='maximum-difference';
                                      end;
          oatTableCondition : begin
                                   Namespace:=onsTable;
                                   LocalName:='condition';
                              end;
          oatTableAllowEmptyCell : begin
                                        Namespace:=onsTable;
                                        LocalName:='allow-empty-cell';
                                   end;
          oatTableDisplayList : begin
                                     Namespace:=onsTable;
                                     LocalName:='display-list';
                                end;
          oatTableTitle : begin
                               Namespace:=onsTable;
                               LocalName:='title';
                          end;
          oatTableMessageType : begin
                                     Namespace:=onsTable;
                                     LocalName:='message-type';
                                end;
          oatTableExecute : begin
                                 Namespace:=onsTable;
                                 LocalName:='execute';
                            end;
          oatTableLabelCellRangeAddress : begin
                                               Namespace:=onsTable;
                                               LocalName:='label-cell-range-address';
                                          end;
          oatTableDataCellRangeAddress : begin
                                              Namespace:=onsTable;
                                              LocalName:='data-cell-range-address';
                                         end;
          oatTableOrientation : begin
                                     Namespace:=onsTable;
                                     LocalName:='orientation';
                                end;
          oatTableIsSelection : begin
                                     Namespace:=onsTable;
                                     LocalName:='is-selection';
                                end;
          oatTableOnUpdateKeepStyles : begin
                                            Namespace:=onsTable;
                                            LocalName:='on-update-keep-styles';
                                       end;
          oatTableOnUpdateKeepSize : begin
                                          Namespace:=onsTable;
                                          LocalName:='on-update-keep-size';
                                     end;
          oatTableHasPersistentData : begin
                                           Namespace:=onsTable;
                                           LocalName:='has-persistent-data';
                                      end;
          oatTableContainsHeader : begin
                                        Namespace:=onsTable;
                                        LocalName:='contains-header';
                                   end;
          oatTableDisplayFilterButtons : begin
                                              Namespace:=onsTable;
                                              LocalName:='display-filter-buttons';
                                         end;
          oatTableTargetRangeAddress : begin
                                            Namespace:=onsTable;
                                            LocalName:='target-range-address';
                                       end;
          oatTableDatabaseName : begin
                                      Namespace:=onsTable;
                                      LocalName:='database-name';
                                 end;
          oatTableSqlStatement : begin
                                      Namespace:=onsTable;
                                      LocalName:='sql-statement';
                                 end;
          oatTableParseSqlStatement : begin
                                           Namespace:=onsTable;
                                           LocalName:='parse-sql-statement';
                                      end;
          oatTableQueryName : begin
                                   Namespace:=onsTable;
                                   LocalName:='query-name';
                              end;
          oatTableDatabaseTableName : begin
                                           Namespace:=onsTable;
                                           LocalName:='database-table-name';
                                      end;
          oatTableConditionSource : begin
                                         Namespace:=onsTable;
                                         LocalName:='condition-source';
                                    end;
          oatTableConditionSourceRangeAddress : begin
                                                     Namespace:=onsTable;
                                                     LocalName:='condition-source-range-address';
                                                end;
          oatTableDisplayDuplicates : begin
                                           Namespace:=onsTable;
                                           LocalName:='display-duplicates';
                                      end;
          oatTableFieldNumber : begin
                                     Namespace:=onsTable;
                                     LocalName:='field-number';
                                end;
          oatTableValue : begin
                               Namespace:=onsTable;
                               LocalName:='value';
                          end;
          oatTableOperator : begin
                                  Namespace:=onsTable;
                                  LocalName:='operator';
                             end;
          oatTableDataType : begin
                                  Namespace:=onsTable;
                                  LocalName:='data-type';
                             end;
          oatTableBindStylesToContent : begin
                                             Namespace:=onsTable;
                                             LocalName:='bind-styles-to-content';
                                        end;
          oatTableLanguage : begin
                                  Namespace:=onsTable;
                                  LocalName:='language';
                             end;
          oatTableCountry : begin
                                 Namespace:=onsTable;
                                 LocalName:='country';
                            end;
          oatTableScript : begin
                                Namespace:=onsTable;
                                LocalName:='script';
                           end;
          oatTableRfcLanguageTag : begin
                                        Namespace:=onsTable;
                                        LocalName:='rfc-language-tag';
                                   end;
          oatTableAlgorithm : begin
                                   Namespace:=onsTable;
                                   LocalName:='algorithm';
                              end;
          oatTableEmbeddedNumberBehavior : begin
                                                Namespace:=onsTable;
                                                LocalName:='embedded-number-behavior';
                                           end;
          oatTableOrder : begin
                               Namespace:=onsTable;
                               LocalName:='order';
                          end;
          oatTablePageBreaksOnGroupChange : begin
                                                 Namespace:=onsTable;
                                                 LocalName:='page-breaks-on-group-change';
                                            end;
          oatTableGroupByFieldNumber : begin
                                            Namespace:=onsTable;
                                            LocalName:='group-by-field-number';
                                       end;
          oatTableFunction : begin
                                  Namespace:=onsTable;
                                  LocalName:='function';
                             end;
          oatTableApplicationData : begin
                                         Namespace:=onsTable;
                                         LocalName:='application-data';
                                    end;
          oatTableGrandTotal : begin
                                    Namespace:=onsTable;
                                    LocalName:='grand-total';
                               end;
          oatTableIgnoreEmptyRows : begin
                                         Namespace:=onsTable;
                                         LocalName:='ignore-empty-rows';
                                    end;
          oatTableIdentifyCategories : begin
                                            Namespace:=onsTable;
                                            LocalName:='identify-categories';
                                       end;
          oatTableButtons : begin
                                 Namespace:=onsTable;
                                 LocalName:='buttons';
                            end;
          oatTableShowFilterButton : begin
                                          Namespace:=onsTable;
                                          LocalName:='show-filter-button';
                                     end;
          oatTableDrillDownOnDoubleClick : begin
                                                Namespace:=onsTable;
                                                LocalName:='drill-down-on-double-click';
                                           end;
          oatTableSourceName : begin
                                    Namespace:=onsTable;
                                    LocalName:='source-name';
                               end;
          oatTableObjectName : begin
                                    Namespace:=onsTable;
                                    LocalName:='object-name';
                               end;
          oatTableUserName : begin
                                  Namespace:=onsTable;
                                  LocalName:='user-name';
                             end;
          oatTablePassword : begin
                                  Namespace:=onsTable;
                                  LocalName:='password';
                             end;
          oatTableSourceFieldName : begin
                                         Namespace:=onsTable;
                                         LocalName:='source-field-name';
                                    end;
          oatTableSelectedPage : begin
                                      Namespace:=onsTable;
                                      LocalName:='selected-page';
                                 end;
          oatTableIsDataLayoutField : begin
                                           Namespace:=onsTable;
                                           LocalName:='is-data-layout-field';
                                      end;
          oatTableUsedHierarchy : begin
                                       Namespace:=onsTable;
                                       LocalName:='used-hierarchy';
                                  end;
          oatTableShowEmpty : begin
                                   Namespace:=onsTable;
                                   LocalName:='show-empty';
                              end;
          oatTableShowDetails : begin
                                     Namespace:=onsTable;
                                     LocalName:='show-details';
                                end;
          oatTableEnabled : begin
                                 Namespace:=onsTable;
                                 LocalName:='enabled';
                            end;
          oatTableDataField : begin
                                   Namespace:=onsTable;
                                   LocalName:='data-field';
                              end;
          oatTableMemberCount : begin
                                     Namespace:=onsTable;
                                     LocalName:='member-count';
                                end;
          oatTableDisplayMemberMode : begin
                                           Namespace:=onsTable;
                                           LocalName:='display-member-mode';
                                      end;
          oatTableSortMode : begin
                                  Namespace:=onsTable;
                                  LocalName:='sort-mode';
                             end;
          oatTableLayoutMode : begin
                                    Namespace:=onsTable;
                                    LocalName:='layout-mode';
                               end;
          oatTableAddEmptyLines : begin
                                       Namespace:=onsTable;
                                       LocalName:='add-empty-lines';
                                  end;
          oatTableFieldName : begin
                                   Namespace:=onsTable;
                                   LocalName:='field-name';
                              end;
          oatTableMemberType : begin
                                    Namespace:=onsTable;
                                    LocalName:='member-type';
                               end;
          oatTableMemberName : begin
                                    Namespace:=onsTable;
                                    LocalName:='member-name';
                               end;
          oatTableType : begin
                              Namespace:=onsTable;
                              LocalName:='type';
                         end;
          oatTableDateStart : begin
                                   Namespace:=onsTable;
                                   LocalName:='date-start';
                              end;
          oatTableStart : begin
                               Namespace:=onsTable;
                               LocalName:='start';
                          end;
          oatTableDateEnd : begin
                                 Namespace:=onsTable;
                                 LocalName:='date-end';
                            end;
          oatTableEnd : begin
                             Namespace:=onsTable;
                             LocalName:='end';
                        end;
          oatTableStep : begin
                              Namespace:=onsTable;
                              LocalName:='step';
                         end;
          oatTableGroupedBy : begin
                                   Namespace:=onsTable;
                                   LocalName:='grouped-by';
                              end;
          oatTableSourceCellRangeAddresses : begin
                                                  Namespace:=onsTable;
                                                  LocalName:='source-cell-range-addresses';
                                             end;
          oatTableTargetCellAddress : begin
                                           Namespace:=onsTable;
                                           LocalName:='target-cell-address';
                                      end;
          oatTableUseLabels : begin
                                   Namespace:=onsTable;
                                   LocalName:='use-labels';
                              end;
          oatTableLinkToSourceData : begin
                                          Namespace:=onsTable;
                                          LocalName:='link-to-source-data';
                                     end;
          oatDrawMasterPageName : begin
                                       Namespace:=onsDraw;
                                       LocalName:='master-page-name';
                                  end;
          oatDrawNavOrder : begin
                                 Namespace:=onsDraw;
                                 LocalName:='nav-order';
                            end;
          oatDrawShapeId : begin
                                Namespace:=onsDraw;
                                LocalName:='shape-id';
                           end;
          oatPresentationDelay : begin
                                      Namespace:=onsPresentation;
                                      LocalName:='delay';
                                 end;
          oatPresentationPathId : begin
                                       Namespace:=onsPresentation;
                                       LocalName:='path-id';
                                  end;
          oatPresentationName : begin
                                     Namespace:=onsPresentation;
                                     LocalName:='name';
                                end;
          oatPresentationSource : begin
                                       Namespace:=onsPresentation;
                                       LocalName:='source';
                                  end;
          oatPresentationStartPage : begin
                                          Namespace:=onsPresentation;
                                          LocalName:='start-page';
                                     end;
          oatPresentationShow : begin
                                     Namespace:=onsPresentation;
                                     LocalName:='show';
                                end;
          oatPresentationFullScreen : begin
                                           Namespace:=onsPresentation;
                                           LocalName:='full-screen';
                                      end;
          oatPresentationEndless : begin
                                        Namespace:=onsPresentation;
                                        LocalName:='endless';
                                   end;
          oatPresentationPause : begin
                                      Namespace:=onsPresentation;
                                      LocalName:='pause';
                                 end;
          oatPresentationShowLogo : begin
                                         Namespace:=onsPresentation;
                                         LocalName:='show-logo';
                                    end;
          oatPresentationForceManual : begin
                                            Namespace:=onsPresentation;
                                            LocalName:='force-manual';
                                       end;
          oatPresentationMouseVisible : begin
                                             Namespace:=onsPresentation;
                                             LocalName:='mouse-visible';
                                        end;
          oatPresentationMouseAsPen : begin
                                           Namespace:=onsPresentation;
                                           LocalName:='mouse-as-pen';
                                      end;
          oatPresentationStartWithNavigator : begin
                                                   Namespace:=onsPresentation;
                                                   LocalName:='start-with-navigator';
                                              end;
          oatPresentationAnimations : begin
                                           Namespace:=onsPresentation;
                                           LocalName:='animations';
                                      end;
          oatPresentationTransitionOnClick : begin
                                                  Namespace:=onsPresentation;
                                                  LocalName:='transition-on-click';
                                             end;
          oatPresentationStayOnTop : begin
                                          Namespace:=onsPresentation;
                                          LocalName:='stay-on-top';
                                     end;
          oatPresentationShowEndOfPresentationSlide : begin
                                                           Namespace:=onsPresentation;
                                                           LocalName:='show-end-of-presentation-slide';
                                                      end;
          oatPresentationPages : begin
                                      Namespace:=onsPresentation;
                                      LocalName:='pages';
                                 end;
          oatTableStructureProtected : begin
                                            Namespace:=onsTable;
                                            LocalName:='structure-protected';
                                       end;
          oatTableTrackChanges : begin
                                      Namespace:=onsTable;
                                      LocalName:='track-changes';
                                 end;
          oatTableId : begin
                            Namespace:=onsTable;
                            LocalName:='id';
                       end;
          oatTableAcceptanceState : begin
                                         Namespace:=onsTable;
                                         LocalName:='acceptance-state';
                                    end;
          oatTableRejectingChangeId : begin
                                           Namespace:=onsTable;
                                           LocalName:='rejecting-change-id';
                                      end;
          oatTableColumn : begin
                                Namespace:=onsTable;
                                LocalName:='column';
                           end;
          oatTableRow : begin
                             Namespace:=onsTable;
                             LocalName:='row';
                        end;
          oatTableTable : begin
                               Namespace:=onsTable;
                               LocalName:='table';
                          end;
          oatTableCellAddress : begin
                                     Namespace:=onsTable;
                                     LocalName:='cell-address';
                                end;
          oatTableMatrixCovered : begin
                                       Namespace:=onsTable;
                                       LocalName:='matrix-covered';
                                  end;
          oatTablePosition : begin
                                  Namespace:=onsTable;
                                  LocalName:='position';
                             end;
          oatTableCount : begin
                               Namespace:=onsTable;
                               LocalName:='count';
                          end;
          oatTableMultiDeletionSpanned : begin
                                              Namespace:=onsTable;
                                              LocalName:='multi-deletion-spanned';
                                         end;
          oatTableStartPosition : begin
                                       Namespace:=onsTable;
                                       LocalName:='start-position';
                                  end;
          oatTableEndPosition : begin
                                     Namespace:=onsTable;
                                     LocalName:='end-position';
                                end;
          oatTableStartColumn : begin
                                     Namespace:=onsTable;
                                     LocalName:='start-column';
                                end;
          oatTableStartRow : begin
                                  Namespace:=onsTable;
                                  LocalName:='start-row';
                             end;
          oatTableStartTable : begin
                                    Namespace:=onsTable;
                                    LocalName:='start-table';
                               end;
          oatTableEndColumn : begin
                                   Namespace:=onsTable;
                                   LocalName:='end-column';
                              end;
          oatTableEndRow : begin
                                Namespace:=onsTable;
                                LocalName:='end-row';
                           end;
          oatTableEndTable : begin
                                  Namespace:=onsTable;
                                  LocalName:='end-table';
                             end;
          oatChartClass : begin
                               Namespace:=onsChart;
                               LocalName:='class';
                          end;
          oatChartColumnMapping : begin
                                       Namespace:=onsChart;
                                       LocalName:='column-mapping';
                                  end;
          oatChartRowMapping : begin
                                    Namespace:=onsChart;
                                    LocalName:='row-mapping';
                               end;
          oatChartStyleName : begin
                                   Namespace:=onsChart;
                                   LocalName:='style-name';
                              end;
          oatTableCellRange : begin
                                   Namespace:=onsTable;
                                   LocalName:='cell-range';
                              end;
          oatChartLegendPosition : begin
                                        Namespace:=onsChart;
                                        LocalName:='legend-position';
                                   end;
          oatChartLegendAlign : begin
                                     Namespace:=onsChart;
                                     LocalName:='legend-align';
                                end;
          oatStyleLegendExpansion : begin
                                         Namespace:=onsStyle;
                                         LocalName:='legend-expansion';
                                    end;
          oatStyleLegendExpansionAspectRatio : begin
                                                    Namespace:=onsStyle;
                                                    LocalName:='legend-expansion-aspect-ratio';
                                               end;
          oatChartDataSourceHasLabels : begin
                                             Namespace:=onsChart;
                                             LocalName:='data-source-has-labels';
                                        end;
          oatChartDimension : begin
                                   Namespace:=onsChart;
                                   LocalName:='dimension';
                              end;
          oatChartName : begin
                              Namespace:=onsChart;
                              LocalName:='name';
                         end;
          oatChartValuesCellRangeAddress : begin
                                                Namespace:=onsChart;
                                                LocalName:='values-cell-range-address';
                                           end;
          oatChartLabelCellAddress : begin
                                          Namespace:=onsChart;
                                          LocalName:='label-cell-address';
                                     end;
          oatChartAttachedAxis : begin
                                      Namespace:=onsChart;
                                      LocalName:='attached-axis';
                                 end;
          oatChartAutomaticContent : begin
                                          Namespace:=onsChart;
                                          LocalName:='automatic-content';
                                     end;
          oatChartDisplayRSquare : begin
                                        Namespace:=onsChart;
                                        LocalName:='display-r-square';
                                   end;
          oatChartDisplayEquation : begin
                                         Namespace:=onsChart;
                                         LocalName:='display-equation';
                                    end;
          oatChartRepeated : begin
                                  Namespace:=onsChart;
                                  LocalName:='repeated';
                             end;
          oatDbMediaType : begin
                                Namespace:=onsDb;
                                LocalName:='media-type';
                           end;
          oatDbExtension : begin
                                Namespace:=onsDb;
                                LocalName:='extension';
                           end;
          oatDbHostname : begin
                               Namespace:=onsDb;
                               LocalName:='hostname';
                          end;
          oatDbPort : begin
                           Namespace:=onsDb;
                           LocalName:='port';
                      end;
          oatDbLocalSocket : begin
                                  Namespace:=onsDb;
                                  LocalName:='local-socket';
                             end;
          oatDbType : begin
                           Namespace:=onsDb;
                           LocalName:='type';
                      end;
          oatDbDatabaseName : begin
                                   Namespace:=onsDb;
                                   LocalName:='database-name';
                              end;
          oatDbUserName : begin
                               Namespace:=onsDb;
                               LocalName:='user-name';
                          end;
          oatDbUseSystemUser : begin
                                    Namespace:=onsDb;
                                    LocalName:='use-system-user';
                               end;
          oatDbIsPasswordRequired : begin
                                         Namespace:=onsDb;
                                         LocalName:='is-password-required';
                                    end;
          oatDbLoginTimeout : begin
                                   Namespace:=onsDb;
                                   LocalName:='login-timeout';
                              end;
          oatDbShowDeleted : begin
                                  Namespace:=onsDb;
                                  LocalName:='show-deleted';
                             end;
          oatDbIsFirstRowHeaderLine : begin
                                           Namespace:=onsDb;
                                           LocalName:='is-first-row-header-line';
                                      end;
          oatDbSystemDriverSettings : begin
                                           Namespace:=onsDb;
                                           LocalName:='system-driver-settings';
                                      end;
          oatDbBaseDn : begin
                             Namespace:=onsDb;
                             LocalName:='base-dn';
                        end;
          oatDbParameterNameSubstitution : begin
                                                Namespace:=onsDb;
                                                LocalName:='parameter-name-substitution';
                                           end;
          oatDbAdditionalColumnStatement : begin
                                                Namespace:=onsDb;
                                                LocalName:='additional-column-statement';
                                           end;
          oatDbRowRetrievingStatement : begin
                                             Namespace:=onsDb;
                                             LocalName:='row-retrieving-statement';
                                        end;
          oatDbField : begin
                            Namespace:=onsDb;
                            LocalName:='field';
                       end;
          oatDbString : begin
                             Namespace:=onsDb;
                             LocalName:='string';
                        end;
          oatDbDecimal : begin
                              Namespace:=onsDb;
                              LocalName:='decimal';
                         end;
          oatDbThousand : begin
                               Namespace:=onsDb;
                               LocalName:='thousand';
                          end;
          oatDbEncoding : begin
                               Namespace:=onsDb;
                               LocalName:='encoding';
                          end;
          oatDbIsTableNameLengthLimited : begin
                                               Namespace:=onsDb;
                                               LocalName:='is-table-name-length-limited';
                                          end;
          oatDbEnableSql92Check : begin
                                       Namespace:=onsDb;
                                       LocalName:='enable-sql92-check';
                                  end;
          oatDbAppendTableAliasName : begin
                                           Namespace:=onsDb;
                                           LocalName:='append-table-alias-name';
                                      end;
          oatDbIgnoreDriverPrivileges : begin
                                             Namespace:=onsDb;
                                             LocalName:='ignore-driver-privileges';
                                        end;
          oatDbBooleanComparisonMode : begin
                                            Namespace:=onsDb;
                                            LocalName:='boolean-comparison-mode';
                                       end;
          oatDbUseCatalog : begin
                                 Namespace:=onsDb;
                                 LocalName:='use-catalog';
                            end;
          oatDbMaxRowCount : begin
                                  Namespace:=onsDb;
                                  LocalName:='max-row-count';
                             end;
          oatDbSuppressVersionColumns : begin
                                             Namespace:=onsDb;
                                             LocalName:='suppress-version-columns';
                                        end;
          oatDbDataSourceSettingIsList : begin
                                              Namespace:=onsDb;
                                              LocalName:='data-source-setting-is-list';
                                         end;
          oatDbDataSourceSettingName : begin
                                            Namespace:=onsDb;
                                            LocalName:='data-source-setting-name';
                                       end;
          oatDbDataSourceSettingType : begin
                                            Namespace:=onsDb;
                                            LocalName:='data-source-setting-type';
                                       end;
          oatDbAsTemplate : begin
                                 Namespace:=onsDb;
                                 LocalName:='as-template';
                            end;
          oatDbName : begin
                           Namespace:=onsDb;
                           LocalName:='name';
                      end;
          oatDbTitle : begin
                            Namespace:=onsDb;
                            LocalName:='title';
                       end;
          oatDbDescription : begin
                                  Namespace:=onsDb;
                                  LocalName:='description';
                             end;
          oatDbCommand : begin
                              Namespace:=onsDb;
                              LocalName:='command';
                         end;
          oatDbEscapeProcessing : begin
                                       Namespace:=onsDb;
                                       LocalName:='escape-processing';
                                  end;
          oatDbStyleName : begin
                                Namespace:=onsDb;
                                LocalName:='style-name';
                           end;
          oatDbDefaultRowStyleName : begin
                                          Namespace:=onsDb;
                                          LocalName:='default-row-style-name';
                                     end;
          oatDbApplyCommand : begin
                                   Namespace:=onsDb;
                                   LocalName:='apply-command';
                              end;
          oatDbVisible : begin
                              Namespace:=onsDb;
                              LocalName:='visible';
                         end;
          oatDbDefaultCellStyleName : begin
                                           Namespace:=onsDb;
                                           LocalName:='default-cell-style-name';
                                      end;
          oatDbCatalogName : begin
                                  Namespace:=onsDb;
                                  LocalName:='catalog-name';
                             end;
          oatDbSchemaName : begin
                                 Namespace:=onsDb;
                                 LocalName:='schema-name';
                            end;
          oatDbDataType : begin
                               Namespace:=onsDb;
                               LocalName:='data-type';
                          end;
          oatDbTypeName : begin
                               Namespace:=onsDb;
                               LocalName:='type-name';
                          end;
          oatDbPrecision : begin
                                Namespace:=onsDb;
                                LocalName:='precision';
                           end;
          oatDbScale : begin
                            Namespace:=onsDb;
                            LocalName:='scale';
                       end;
          oatDbIsNullable : begin
                                 Namespace:=onsDb;
                                 LocalName:='is-nullable';
                            end;
          oatDbIsEmptyAllowed : begin
                                     Namespace:=onsDb;
                                     LocalName:='is-empty-allowed';
                                end;
          oatDbIsAutoincrement : begin
                                      Namespace:=onsDb;
                                      LocalName:='is-autoincrement';
                                 end;
          oatDbReferencedTableName : begin
                                          Namespace:=onsDb;
                                          LocalName:='referenced-table-name';
                                     end;
          oatDbUpdateRule : begin
                                 Namespace:=onsDb;
                                 LocalName:='update-rule';
                            end;
          oatDbDeleteRule : begin
                                 Namespace:=onsDb;
                                 LocalName:='delete-rule';
                            end;
          oatDbRelatedColumnName : begin
                                        Namespace:=onsDb;
                                        LocalName:='related-column-name';
                                   end;
          oatDbIsUnique : begin
                               Namespace:=onsDb;
                               LocalName:='is-unique';
                          end;
          oatDbIsClustered : begin
                                  Namespace:=onsDb;
                                  LocalName:='is-clustered';
                             end;
          oatDbIsAscending : begin
                                  Namespace:=onsDb;
                                  LocalName:='is-ascending';
                             end;
     end;
end;

