unit xmlErrorReport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, JNCCXMLDoc, MarkupDocs, ExceptionForm;
                                               
type
  TfrmXMLReport = class(TForm)
    pnlReport: TPanel;
    Panel1: TPanel;
    bbOk: TBitBtn;
    pnlErrors: TPanel;
    lblInfo: TLabel;
    pnlFile: TPanel;
    Label1: TLabel;
    reXmlFile: TRichEdit;
    mmErrorPath: TMemo;
    Splitter1: TSplitter;
  private
    { Private declarations }
    procedure PopulateExceptionPath( iException : TExceptionPath );
  public
    { Public declarations }
    constructor Create( AOwner : TComponent; iException : TMarkupException;
                        iDocument : TXMLDoc ); reintroduce;
  end;

var
  frmXMLReport: TfrmXMLReport;

implementation

uses
  XMLData;

{$R *.DFM}

//==============================================================================
{ TfrmXMLReport }
//==============================================================================


{ New constructor - parses required information from the document and error
     class }
constructor TfrmXMLReport.Create(AOwner: TComponent;
    iException: TMarkupException; iDocument : TXMLDoc);
begin
  inherited Create(AOwner);
  iDocument.PopulateErrorReport( reXMLFile, iException.Row, iException.BlockPos );
  PopulateExceptionPath( iException );
end;



procedure TfrmXMLReport.PopulateExceptionPath( iException : TExceptionPath );
begin
  { Insert the current exception (most recent) at the end }
  mmErrorPath.lines.Add( iException.ClassName + ' : ' +
                                iException.Message );

  { Save the string list to a file for support }
  mmErrorPath.lines.SaveToFile(madExceptErrorPath + 'ImportErrors.txt');
end;

end.
