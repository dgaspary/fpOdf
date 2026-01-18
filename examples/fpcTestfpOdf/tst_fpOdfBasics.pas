unit tst_fpOdfBasics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, odf_types;

type

  { TTestfpOdf }

  TTestfpOdf= class(TTestCase)
  private
    FOdfTextDocument:TOdfTextDocument;
    FDatapath:String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetUp;
  public
    constructor Create; override;
    property OdfTextDocument:TOdfTextDocument read FOdfTextDocument;
  end;

implementation

const CDataPath = 'output';

procedure TTestfpOdf.TestSetUp;
begin
  CheckNotNull(OdfTextDocument,'OdfDocument is assigned');
  CheckNotNull(OdfTextDocument.Text,'OdfDocument.Text is assigned');
  CheckNotNull(OdfTextDocument.Styles,'OdfDocument.Styles is assigned');
  CheckNotNull(OdfTextDocument.Body ,'OdfDocument.Body is assigned');
  CheckNotNull(OdfTextDocument.AutomaticStyles,'OdfDocument.AutomaticStyles is assigned');
  CheckNotNull(OdfTextDocument.FontFaceDecls,'OdfDocument.FontFaceDecls is assigned');
  Checktrue(DirectoryExists(FDatapath),'Datapath exists');
end;

procedure TTestfpOdf.SetUp;
begin
  FOdfTextDocument:= TOdfTextDocument.Create;
end;

procedure TTestfpOdf.TearDown;
begin
  freeandnil(FOdfTextDocument)
end;

constructor TTestfpOdf.Create;
var
  i: Integer;
begin
  inherited Create;
  FDatapath := CDataPath;
  for i := 0 to 2 do
    if DirectoryExists(FDatapath)
      then break
      else
        FDatapath:='..'+DirectorySeparator+FDatapath;
end;

initialization

  RegisterTest(TTestfpOdf);
end.

