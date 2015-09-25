//==============================================================================
//  Unit:        ReportExport
//
//  Implements:  TBaseExport
//               TWizardExport
//               TExcelExport
//
//  Description: Classes for exporting from a database grid
//
//  Author:      John van Breda
//  Created:     22/3/2006
//
//  Last Revision Details:
//    $Revision: 9 $
//    $Date: 1/10/09 10:05 $
//    $Author: Simonlewis $
//
//==============================================================================
unit ReportExport;

interface

uses
  Sysutils, Classes, DBGrids, Forms, Dialogs, ComCtrls, Controls, DB, Graphics,
  ExceptionForm, ExportDS, SMEEngine, SME2Cell, SME2XLS, SMEWiz;

resourcestring
  ResStr_ExportAborted = 'The export was cancelled.';

type
  TExportException = class(TExceptionPath);

  //============================================================================
  TBaseExport = class(TObject)
  private
    FReportOutputPath: string;
    FRtfConvertor: TRichEdit;
    FWinControl: TWinControl;
    procedure SetReportOutputPath(const Value: string);
    procedure ParseDelimited(sl: TStringList; stringtoparse,
      delimiter: string);
  protected
    FNeedFilePath: Boolean;
    FFilter: string;
    function GetOutputLocation: string;
    procedure GetCellParams(Sender: TObject;
      Field: TField; var Text: String; AFont: TFont; var Alignment: TAlignment;
      var Background: TColor; var CellType: TCellType); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function DoExport(ADBGrid: TDBGrid): String; virtual; abstract;
    property ReportOutputPath: string read FReportOutputPath write
        SetReportOutputPath;
    property WinControl: TWinControl read FWinControl write FWinControl;
  end;

  //============================================================================
  TBaseExportClass = class of TBaseExport;

  //============================================================================
  // Class to export any format via SMExport component suite's wizard
  TWizardExport = class(TBaseExport)
  public
    function DoExport(ADBGrid: TDBGrid): String; override;
  end;

  //============================================================================
  TExcelExport = class(TBaseExport)
  protected
    procedure GetCellParams(Sender: TObject;
      Field: TField; var Text: String; AFont: TFont; var Alignment: TAlignment;
      var Background: TColor; var CellType: TCellType); override;
  public
    constructor Create; override;
    function DoExport(ADBGrid: TDBGrid): String; override;
  end;

//==============================================================================
implementation

uses
  DefaultPaths, ApplicationSettings;

//==============================================================================
{ TWizardExport }

{-------------------------------------------------------------------------------
  Export using the wizard
  Returns the full path of the export file, or an empty string if cancelled.
}
function TWizardExport.DoExport(ADBGrid: TDBGrid): String;
begin
  inherited;
  with TSMEWizardDlg.Create(nil) do begin
    try
      SpecificationDir := AppSettings.ExportTemplatePath;
      OnGetCellParams := GetCellParams;
      DBGrid := ADBGrid;
      DataSet := ADBGrid.DataSource.DataSet;
      InitialDir := GetOutputLocation;
      AddTitle := True;
      FileName := GetOutputLocation + 'Export';
      TableName := 'Export';
      Formats :=
          [teParadox, teDBase, teText, teHTML, teExcel2007, teXLS, teWord,
           teWKS, teQuattro, teXML, teAccess, teRTF, tePDF, teADO];
      Picture.LoadFromFile(ExtractFilePath(Application.Exename) + 'Images\Export.jpg');
      ActionAfterExport := aeNone;
      ConfirmFileOverwrite := True;
      if Execute = mrOk then
        Result := FileName
      else
        Result := '';
    finally
      Free;
    end;
  end;
end;

//==============================================================================
{ TBaseExport }

{-------------------------------------------------------------------------------
  Constructor
}
constructor TBaseExport.Create;
begin
  inherited;
  FNeedFilePath := false;
  FFilter := '';
end;

{-------------------------------------------------------------------------------
  Accessor
}
procedure TBaseExport.SetReportOutputPath(const Value: string);
begin
  FReportOutputPath := Value;
end;

{-------------------------------------------------------------------------------
  Return the path or file name required for output
}
function TBaseExport.GetOutputLocation: string;
var
  dlg: TSaveDialog;
  filterlist: TStringList;
begin
  if not DirectoryExists(ExtractFilePath(ReportOutputPath)) then
    try
      ForceDirectories(ExtractFilePath(ReportOutputPath))
    finally ; // don't worry if the user can't create this dir for any reason
    end;

  if FNeedFilePath then begin
    dlg := TSaveDialog.Create(nil);
    with dlg do try
      Filter := FFilter;
      InitialDir := ReportOutputPath;
      if Execute then begin
        // Auto-add the extension. Thought the dialog could do this itself?
        if ExtractFileExt(FileName)='' then begin
          filterlist := TStringList.Create;
          try
            ParseDelimited(filterlist, Filter, '|');
            // The list returns 2 entries for each filter, the name followed by the extension.
            // We also skip the first char of the extension which will be *
            FileName := FileName + Copy(filterlist[FilterIndex*2-1], 2, 255);
          finally
            filterlist.Free;
          end;
          // 'NBN Data|*.xml|All files|*.*'
        end;
        Result := FileName;
      end
      else
        raise TExportException.CreateNonCritical(ResStr_ExportAborted);
    finally
      Free;
    end;
  end
  else
    Result := ReportOutputPath;
end;

procedure TBaseExport.ParseDelimited(sl: TStringList; stringtoparse, delimiter: string);
var
  current: string;
  i: integer;
begin
  current := '';
  for i:=1 to length(stringtoparse) do begin
    if stringtoparse[i]=delimiter then begin
      sl.Add(current);
      current := '';
    end else
      current := current+stringtoparse[i];
  end;
  if current<>'' then
    sl.Add(current);
end;

{-------------------------------------------------------------------------------
  Constructor
}
constructor TExcelExport.Create;
begin
  inherited;
  FNeedFilePath := true;
  FFilter := 'Excel file (*.xlsx)|*.xlsx|Excel 97 - 2003 file (*.xls)|*.xls|';
end;

{-------------------------------------------------------------------------------
  Export to XLS file
  Returns the full path of the export file, or an empty string if cancelled.
}
function TExcelExport.DoExport(ADBGrid: TDBGrid): String;
var exporter: TSMExportToCustomXLS;
  filepath: string;
begin
  filepath := GetOutputLocation;
  if SameText(ExtractFileExt(filepath), '.xls') then
    exporter := TSMExportToXLS.Create(nil)
  else
    exporter := TSMExportToXLSX.Create(nil);
  with exporter do
    try
      DBGrid := ADBGrid;
      FileName := filepath;
      AddTitle := true;
      OnGetCellParams := GetCellParams;
      Execute;
      Result := FileName;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
  Convert any rtf cells to plain text as most export formats won't handle it
      properly.  Also convert vague dates from numbers to text
}
procedure TBaseExport.GetCellParams(Sender: TObject; Field: TField;
  var Text: String; AFont: TFont; var Alignment: TAlignment;
  var Background: TColor; var CellType: TCellType);

  function IsVagueDateColumnTitle: Boolean;
  var
    i: Integer;
  begin
    Result := False; // default
    if not Assigned(Field) then
      with TDBGrid(TSMExportBaseComponent(Sender).DBGrid) do begin
        // look through the columns for the one that matches our title
        if Assigned(Columns) then
          for i := 0 to Columns.Count - 1 do
            if Columns[i].Title.Caption = Text then begin
              // got the right column, so return if its a vague date
              Result := Pos('Vague_Date_Start', Columns[i].FieldName) > 0;
              Break;
            end;
      end;
  end;

begin
  if SameText(Copy(Text, 1, 5), '{\rtf') then begin
    if not Assigned(FRtfConvertor) then begin
      FRtfConvertor := TRichEdit.Create(nil);
      FRtfConvertor.PlainText := True;
      FRtfConvertor.Visible := False;
      FRtfConvertor.Parent := FWinControl;
    end;
    FrtfConvertor.Lines.Clear;
    FrtfConvertor.SelStart := 0;
    FrtfConvertor.SelLength := 0;
    FrtfConvertor.SetSelTextBuf(PChar(Text));
    Text := Trim(FRtfConvertor.Lines.Text);
  end else
  if IsVagueDateColumnTitle then
    CellType := ctString
  else
  if Assigned(Field) then
    if Pos('Vague_Date_Start', Field.FieldName) > 0 then begin
      // vague date fields, so use field's text convertor to get vague date string
      CellType := ctString;
      Text := Field.Text;
    end;
end;

{-------------------------------------------------------------------------------
  Destructor - cleanup
}
destructor TBaseExport.Destroy;
begin
  FRtfConvertor.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  For excel output, CRLF just needs to be LF
}
procedure TExcelExport.GetCellParams(Sender: TObject; Field: TField;
  var Text: String; AFont: TFont; var Alignment: TAlignment;
  var Background: TColor; var CellType: TCellType);
begin
  inherited;
  Text := StringReplace(Text, #13#10, #10, [rfReplaceAll]);
end;

end.
