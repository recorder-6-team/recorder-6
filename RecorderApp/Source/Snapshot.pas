//==============================================================================
//  Unit:        Snapshot
//
//  Implements:  TfrmSnapshot
//
//  Description:
//  Author:      Polly Shaw
//  Created:     23 December 2002
//
//  Last Revision Details:
//    $Revision: 18 $
//    $Date: 4/01/08 15:03 $
//    $Author: Rickyshrestha $
//
//  $History: Snapshot.pas $
//  
//  *****************  Version 18  *****************
//  User: Rickyshrestha Date: 4/01/08    Time: 15:03
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 17  *****************
//  User: Rickyshrestha Date: 27/12/07   Time: 17:27
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  
//  *****************  Version 16  *****************
//  User: Rickyshrestha Date: 19/12/07   Time: 11:32
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestring
//  ResStr_TaxonObservTable
//    ResStr_TaxonDictTable
//    ResStr_ObservationTable
//    ResStr_DictionaryTable
//    ResStr_SnapshotCannotRun
//    ResStr_SnapshotCannotRunDB
//    ResStr_UsernameBlank
//    ResStr_ExistOrDenied
//    ResStr_NoPermissionToDB
//    ResStr_DBNotFound
//    ResStr_CannotCreateDB
//    ResStr_UserCancelled
//    ResStr_InvalidSnapshotFile
//    ResStr_CannotFindFile
//    ResStr_BlankTableName
//    ResStr_NoPermissionToDelete
//    ResStr_CreatingTable
//    ResStr_ListDeleteTable
//    ResStr_CannotDropTable
//    ResStr_CannotDeleteTableRef
//    ResStrCannotDeleteTable
//    ResStr_SaveSnapshotChanges
//    ResStr_SnapshotAffectReport
//  
//  *****************  Version 15  *****************
//  User: Johnvanbreda Date: 16/12/04   Time: 16:03
//  Updated in $/JNCC/Development/Build/Source
//  Improved error messages
//  
//  *****************  Version 14  *****************
//  User: Johnvanbreda Date: 16/12/04   Time: 13:39
//  Updated in $/JNCC/Development/Build/Source
//  IR8161
//  Fixed close query
//  
//  *****************  Version 13  *****************
//  User: Ericsalmon   Date: 16/11/04   Time: 9:32
//  Updated in $/JNCC/Development/Build/Source
//  ID 7521/CCN114. Reporting interface improvements.
//  
//  *****************  Version 12  *****************
//  User: Ericsalmon   Date: 10/02/04   Time: 11:46
//  Updated in $/JNCC/Development/Build/Source
//  Development.
//  
//  *****************  Version 11  *****************
//  User: Pollyshaw    Date: 14/02/03   Time: 12:37
//  Updated in $/JNCC/Source
//  IR 494: now prompt for save when changed.
//  
//  *****************  Version 10  *****************
//  User: Pollyshaw    Date: 12/02/03   Time: 10:58
//  Updated in $/JNCC/Source
//  Made it a bit faster by getting source tables from the attributes
//  rather than re-querying the database.
//
//  *****************  Version 8  *****************
//  User: Pollyshaw    Date: 11/02/03   Time: 18:07
//  Updated in $/JNCC/Source
//  IR 487: made it work faster. Also put in progress bars.
//  
//  *****************  Version 7  *****************
//  User: Ericsalmon   Date: 21/01/03   Time: 18:02
//  Updated in $/JNCC/Source
//  Cleanup.
//  
//  *****************  Version 6  *****************
//  User: Pollyshaw    Date: 14/01/03   Time: 10:42
//  Updated in $/JNCC/Source
//  Fixed some minor incidents.
//  
//  *****************  Version 4  *****************
//  User: Pollyshaw    Date: 8/01/03    Time: 15:18
//  Updated in $/JNCC/Source
//  Made the form not close after running the snapshot, and removed
//  duplicated keys.
//  
//  *****************  Version 2  *****************
//  User: Pollyshaw    Date: 23/12/02   Time: 17:25
//  Updated in $/JNCC/Source
//  In progress doing Report
//
//  Copyright © Dorset Software Services Ltd, 2002
//==============================================================================

unit Snapshot;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, SQLList, ADODB, Db, ExceptionForm,
  comobj, GeneralFunctions, ApplicationSettings, xmldom, XMLIntf, msxmldom,
  XMLDoc, ReportGenerator, Contnrs, Password, BaseChildUnit, FormActions,
  ImageListButton;

type
  TSnapshotException = class(Exception);

  TfrmSnapshot = class(TBaseChild)
    pcSnapshot: TPageControl;
    tsServer: TTabSheet;
    tsModel: TTabSheet;
    chkSeparateObs: TCheckBox;
    chkSamples: TCheckBox;
    chkEvents: TCheckBox;
    chkSurveys: TCheckBox;
    cmbServer: TComboBox;
    cmbDatabase: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    tsTableNames: TTabSheet;
    Label3: TLabel;
    pnlButtons: TPanel;
    chkSeparateDict: TCheckBox;
    chkObsData: TCheckBox;
    chkSampleData: TCheckBox;
    pnlDiagram: TPanel;
    pntDataDiagram: TPaintBox;
    Label4: TLabel;
    lblSurveyName: TLabel;
    lblEventName: TLabel;
    lblSampleName: TLabel;
    lblSampleDataName: TLabel;
    lblTaxObsName: TLabel;
    lblBioObsName: TLabel;
    lblTaxObsDataName: TLabel;
    lblBioObsDataName: TLabel;
    lblTaxDictName: TLabel;
    lblBioDictName: TLabel;
    eBioObsName: TEdit;
    eBioDictName: TEdit;
    eBioObsDataName: TEdit;
    eTaxObsDataName: TEdit;
    eTaxDictName: TEdit;
    eSurveyName: TEdit;
    eEventName: TEdit;
    eSampleName: TEdit;
    eSampleDataName: TEdit;
    eTaxObsName: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    chkDatabase: TCheckBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    eUserName: TEdit;
    lblUserName: TLabel;
    ePassword: TEdit;
    lblPassword: TLabel;
    chkTrustedSecurity: TCheckBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    dlgOpen: TOpenDialog;
    chkOverwriteTables: TCheckBox;
    cnnNewDatabase: TADOConnection;
    qryAllPurpose: TADOQuery;
    qryForeignKeys: TADOQuery;
    dlgSave: TSaveDialog;
    btnCancel: TButton;
    btnPrevious: TImageListButton;
    btnNext: TImageListButton;
    procedure pntDataDiagramPaint(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure cmbDatabaseChange(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure eTableNameChange(Sender: TObject);
    procedure chkTrustedSecurityClick(Sender: TObject);
    procedure cmbServerExit(Sender: TObject);
    procedure cmbServerEnter(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FiLastBoxTop: Integer;
    FstServer: String;
    FtfOpeningDoc: Boolean; // indicates that a snapshot spec is being loaded.
    FAttributes: TStringList;
    FcnnOldConnection: TADOConnection;
    FstSnapshotFile: String;
    FstShortFileName: String;
    FDirty: Boolean;
    function AddSpacesToString(AstText: String; ALength: Integer): String;
    procedure CheckDatabasePermissions;
    procedure CreateSnapshot;
    procedure DrawConnector(iLastConn: TPoint; iiXEndOffset: Integer);
    function DrawLevelTable(const istName: String; iiXOffset: Integer): TPoint;
    function DrawTable(const istName: String; iiXOffset: Integer): TPoint;
    function GetDiagramRows: Integer;
    function IsCharType(AValue: String): Boolean;
    procedure OpenSnapshot(AstSnapshotFile: String);overload;
    procedure SaveSnapshot(AstFileName: String); overload;
    procedure SetSnapshotFile(Value: String);
  public
    property SnapshotFile: String read FstSnapshotFile write SetSnapshotFile;
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AConn :TADOConnection; AAttributes: TStringList); reintroduce; overload;
    constructor CreateAndLoad(AOwner: TComponent; AConn: TADOConnection; Attributes: TStringList; AstSnapshotFile: String);
    constructor CreateAndRun(AOwner: TComponent; AConn: TADOConnection; Attributes: TStringList; AstSnapshotFile: String);
    procedure OpenSnapshot;overload;
    procedure SaveSnapshot; overload;
    procedure SaveSnapshotAs;
    property Dirty: Boolean read FDirty;
  end;

//==============================================================================
implementation

uses
  Math, Variants, ADOInt, StrUtils, FilterResult, Maintbar;

{$R *.DFM}

const
  BOX_WIDTH = 72;
  BOX_HEIGHT = 50;
  SPACING = 30;
  REPORT_TABLE = '#REPORT_OUTPUT';

resourcestring
  ResStr_TaxonObservTable = 'Taxon Observations Table:';
  ResStr_TaxonDictTable = 'Taxon Dictionary Table';
  ResStr_ObservationTable = 'Observations Table:';
  ResStr_DictionaryTable =  'Dictionary Table';
  ResStr_SnapshotCannotRun =  'The snapshot cannot be run as the server name has not been specified.';
  ResStr_SnapshotCannotRunDB =  'The snapshot cannot be run as the database name has not been specified.';
  ResStr_UsernameBlank =  'User name can''t be blank';
  ResStr_ExistOrDenied = ''' does not exist or access is denied.';

  ResStr_NoPermissionToDB = 'This user does not have permission to ' +
                            'create tables in the database ''%s''';

  ResStr_DBNotFound = 'The database ''%s'' was not found';

  ResStr_CannotCreateDB = 'The database ''%s'' was not found and the server does not permit this user to ' +
                          'create databases.';

  ResStr_UserCancelled = 'User Cancelled';
  ResStr_InvalidSnapshotFile =  '''%s'' is not a valid snapshot file';
  ResStr_CannotFindFile = 'Can''t find the file ''%s''';
  ResStr_BlankTableName = 'Table names can''t be blank.';

  ResStr_NoPermissionToDelete =  'The database ''%s'' already contains the table ''%s''' +
                                 ' and you do not have permission to delete it.';

  ResStr_CreatingTable =  'Creating table ''%s''';
  ResStr_ListDeleteTable = 'The following tables will be deleted:';
  ResStr_CannotDropTable = 'Could not drop the table ''%s''. There may be a foreign key referencing this table.';

  ResStr_CannotDeleteTableRef = 'Could not delete the table ''%s'' because it has a foreign key referencing it ' +
                                'which you do not have permission to delete';

  ResStrCannotDeleteTable = 'Could not drop the table ''%s''';
  ResStr_SaveSnapshotChanges =  'Would you like to save your changes to this snapshot?';
  ResStr_SnapshotAffectReport = 'Please note that any report using this same snapshot will be affected.';
  ResStr_Cap_Run = 'R&un';
  ResStr_Cap_Next = '&Next';
  ResStr_RptingSnapshotTool = 'Reporting Snapshot Tool - %s';


function FormatWithSquareBrackets(AstText: String): String;
begin
  Result := '[' + DuplicateCharacters(AstText, ']') + ']';
end;

function FormatForConnectionString(AstText: String): String;
begin
  if Pos(';',AstText) <> 0 then
    Result := '"' + DuplicateCharacters(AstText, '"') + '"'
  else
    Result := AstText;
end;

//==============================================================================
{-------------------------------------------------------------------------------
}
constructor TfrmSnapshot.Create(AOwner: TComponent);
begin
  inherited;
  PopulateServerList(cmbServer.Items);
  pcSnapshot.ActivePage := tsServer;
  FstServer := '';
  FstShortFileName := 'Untitled';
  Caption := Format(ResStr_RptingSnapshotTool, [FstShortFileName]);
  FtfOpeningDoc :=False;
end;

{-------------------------------------------------------------------------------
}
constructor TfrmSnapshot.Create(AOwner: TComponent; AConn: TADOConnection;
  AAttributes: TStringList);
begin
  Create(AOwner);
  FAttributes := AAttributes;
  FcnnOldConnection := AConn;
  FDirty := False;
end;

{-------------------------------------------------------------------------------
}
constructor TfrmSnapshot.CreateAndLoad(AOwner: TComponent;
  AConn: TADOConnection; Attributes: TStringList; AstSnapshotFile: String);
begin
  Create(AOwner, AConn, Attributes);
  openSnapshot(AstSnapshotFile);
end;

{-------------------------------------------------------------------------------
}
constructor TfrmSnapshot.CreateAndRun(AOwner: TComponent;
  AConn: TADOConnection; Attributes: TStringList; AstSnapshotFile: String);
begin
  Create(AOwner, AConn, Attributes);
  try
    try
      openSnapshot(AstSnapshotFile);
      CheckDatabasePermissions;
      CreateSnapshot;
    except
      on TSnapshotException do //nothing
    end
  finally
    close;
  end;
end;

{-------------------------------------------------------------------------------
  This function adds a space to a String if a word is too long to fit in a space
  of Alength pixels, so that the word wraps.
}
function TfrmSnapshot.AddSpacesToString(AstText: String; ALength: Integer): String;
var lrect: TRECT;
  i: Integer;
  lstWord: String;
begin
  lRect.Top := 0;
  lrect.Left := 0;
  lrect.Bottom := 0;
  lrect.Right := 0;
  lstWord := '';
  Result := '';
  for i := 1 to length(AstText) do
  begin
    if AstText[i] = ' ' then
    begin
      Result := Result + lstWord + ' ';
      lstWord := '';
      continue;
    end;
    lstWord := lstWord + AstText[i];
    DrawText(pntDataDiagram.Canvas.Handle, PChar(lstWord), length(lstWord),
        lRect, dt_calcrect);
    if lRect.Right > Alength then
    begin
      Result := Result + LeftStr(lstWord, Length(lstWord) - 1) + ' ';
      lstWord := RightStr(lstWord, 1);
    end;
  end;
  Result := Result + lstWord;
end;

{-------------------------------------------------------------------------------
}
function TfrmSnapshot.DrawLevelTable(const istName: String; iiXOffset: Integer): TPoint;
var
  lTextSize: TSize;
  lRect: TRect;
  lNewRect: TRect;
  lstStringWithWrapSpaces: String;
begin
  lRect := Rect((pntDataDiagram.Width - BOX_WIDTH) div 2 + iiXOffset, FiLastBoxTop,
                (pntDataDiagram.Width + BOX_WIDTH) div 2 + iiXOffset, FiLastBoxTop + BOX_HEIGHT);
  with pntDataDiagram.Canvas do begin
    // drop shadow
    Brush.Color := clDkGray;
    FillRect(Rect(lRect.Left+3, lRect.Top+3, lRect.Right+3, lRect.Bottom+3));
    // fill box white
    Brush.Color := clWhite;
    FillRect(lRect);
    Rectangle(lRect.Left, lRect.Top, lRect.Right, lRect.Bottom);
    // centred text
    lTextSize := TextExtent(istName);
    lNewRect.Left :=lRect.Left + 3;
    lNewRect.Top := lRect.Top + 3;
    lNewRect.Right :=lRect.Right - 3;
    lNEwRect.Bottom :=lRect.Top + 3;
    lstStringWithWrapSpaces := AddSpacesToString(istName, lRect.Right - lRect.Left - 6);
    DrawText(pntDataDiagram.Canvas.Handle,
              PChar(lstStringWithWrapSpaces), Length(lstStringWithWrapSpaces), lNewRect,
              dt_calcrect or dt_Center OR  DT_WORDBREAK);
    //TextOut((pntDataDiagram.Width - lNewRect.Right) div 2 + iiXOffset,
    //         FiLastBoxTop + (BOX_HEIGHT - lNewRect.Bottom) div 2, istName);
    lNewRect.Right := lRect.Right -3; //Centre within the box
    lNewRect.Left := lRect.Left + 3;
    if lNewRect.Bottom > lRect.Bottom -3 then
      lNewRect.Bottom := lRect.Bottom -3
    else
    begin
      lNewRect.Top := lNewRect.Top + (lRect.Bottom - lNewRect.Bottom -3) Div 2;
      lNewRect.Bottom := lNewRect.Bottom + (lRect.Bottom - lNewRect.Bottom -3) Div 2;
    end;

    DrawText(pntDataDiagram.canvas.handle,
             PChar(lstStringWithWrapSpaces), Length(lstStringWithWrapSpaces), lNewRect,
             dt_Center or DT_WordBreak);
  end;
  Result := Point((lRect.Left + lRect.Right) div 2, lRect.Bottom);
end;

{-------------------------------------------------------------------------------
}
function TfrmSnapshot.GetDiagramRows: Integer;
begin
  Result := 1;  // always obs
  if chkSurveys.Checked then
    Inc(Result);
  if chkEvents.Checked then
    Inc(Result);
  if chkSamples.Checked then
    Inc(Result);
  if chkSeparateDict.Checked then
    Inc(Result);
end;

{-------------------------------------------------------------------------------
}
function TfrmSnapshot.DrawTable(const istName: String; iiXOffset: Integer): TPoint;
begin
  FiLastBoxTop := FiLastBoxTop + BOX_HEIGHT + SPACING;
  Result := DrawLevelTable(istName, iiXOffset);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.pntDataDiagramPaint(Sender: TObject);
var
  lLastBoxConnector: TPoint;
  lTaxOccConnector, lBioOccConnector: TPoint;
  ltfLockWindowResult: LongBool;
begin
  ltfLockWindowResult := LockWindowUpdate(pntDataDiagram.Canvas.Handle);
  try
    with pntDataDiagram.Canvas do begin
      Brush.Color := $BBFFFF;     // cream
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, pntDataDiagram.Width, pntDataDiagram.Height));
      Font.Size := 12;
      Font.Color := clSilver;
      Font.Style := [fsBold];
      if FstShortFileName = '' then
        TextOut(4, 4, 'Data model preview')
      else
        TextOut(4, 4, 'Data model preview - ' + FstShortFilename);
      Font.Size := 8;
      Font.Color := clBlack;
      Font.Style := [];
    end;
    lLastBoxConnector.X := 0;
    FiLastBoxTop := (pntDataDiagram.Height - (GetDiagramRows + 1) * (BOX_HEIGHT + SPACING) -
                     BOX_HEIGHT) div 2;

    if chkSurveys.Checked then
      lLastBoxConnector := DrawTable(eSurveyName.Text, 0);
    if chkEvents.Checked then begin
      DrawConnector(lLastBoxConnector, 0);
      lLastBoxConnector := DrawTable(eEventName.Text, 0);
    end;
    if chkSamples.Checked then begin
      DrawConnector(lLastBoxConnector, 0);
      lLastBoxConnector := DrawTable(eSampleName.Text, 0);
      if chkSampleData.Checked then begin
        DrawLevelTable(eSampleDataName.Text, SPACING + BOX_WIDTH);
        pntDataDiagram.Canvas.MoveTo((pntDataDiagram.width + BOX_WIDTH) div 2,
                                     lLastBoxConnector.y - BOX_HEIGHT div 2);
        pntDataDiagram.Canvas.LineTo((pntDataDiagram.width + BOX_WIDTH) div 2 + SPACING,
                                     lLastBoxConnector.y - BOX_HEIGHT div 2);
      end;
    end;
    if chkSeparateObs.Checked then
    begin
      if chkObsData.Checked then
      begin
        DrawConnector(lLastBoxConnector, -60);
        lTaxOccConnector := DrawTable(eTaxObsName.Text, -60);

        DrawLevelTable(eTaxObsDataName.Text, 0 - (SPACING + BOX_WIDTH + 60));
        pntDataDiagram.Canvas.MoveTo((pntDataDiagram.width + BOX_WIDTH) div 2 - 60 - BOX_WIDTH - SPACING,
                                     lTaxOccConnector.y - BOX_HEIGHT div 2);
        pntDataDiagram.Canvas.LineTo((pntDataDiagram.width + BOX_WIDTH) div 2 - 60 - BOX_WIDTH,
                                     lTaxOccConnector.y - BOX_HEIGHT div 2);
        DrawConnector(lLastBoxConnector, 60);
        lBioOccConnector := DrawLevelTable(eBioObsName.Text, 60);
        DrawLevelTable(eBioObsDataName.Text, SPACING + BOX_WIDTH + 60);
        pntDataDiagram.Canvas.MoveTo((pntDataDiagram.width + BOX_WIDTH) div 2 + 60,
                                     lBioOccConnector.y - BOX_HEIGHT div 2);
        pntDataDiagram.Canvas.LineTo((pntDataDiagram.width + BOX_WIDTH) div 2 + SPACING + 60,
                                     lBioOccConnector.y - BOX_HEIGHT div 2);
      end
      else
      begin
        DrawConnector(lLastBoxConnector, -60);
        lTaxOccConnector := DrawTable(eTaxObsName.Text, -60);
        DrawConnector(lLastBoxConnector, 60);
        lBioOccConnector := DrawLevelTable(eBioObsName.Text, 60);
        pntDataDiagram.Canvas.MoveTo((pntDataDiagram.width + BOX_WIDTH) div 2 - 60 - BOX_WIDTH - SPACING,
                                     lTaxOccConnector.y - BOX_HEIGHT div 2);
      end;
    end
    else
    begin
      DrawConnector(lLastBoxConnector, 0);
      lLastBoxConnector := DrawTable(eTaxObsName.Text, 0);
    end;
    if chkSeparateDict.Checked then begin
      if chkSeparateObs.Checked then begin
        DrawConnector(lTaxOccConnector, 0);
        DrawTable(eTaxDictName.Text, -60);
        DrawConnector(lBioOccConnector, 0);
        DrawLevelTable(eBioDictName.Text, 60);
      end else begin
        DrawConnector(lLastBoxConnector, 0);
        lLastBoxConnector := DrawTable(eTaxDictName.Text, 0);
      end;
    end;
  finally
    if ltfLockWindowResult then LockWindowUpdate(0);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.DrawConnector(iLastConn: TPoint; iiXEndOffset: Integer);
begin
  if iLastConn.x<> 0 then begin
    pntDataDiagram.Canvas.MoveTo(iLastConn.X, iLastConn.Y);
    pntDataDiagram.Canvas.LineTo(iLastConn.x + iiXEndOffset, iLastConn.Y + Round(0.75 * BOX_HEIGHT));
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.CheckBoxClick(Sender: TObject);
begin
  pntDataDiagram.Invalidate;
  eSurveyName.Enabled := chkSurveys.Checked;
  lblSurveyName.Enabled := chkSurveys.Checked;
  eEventName.Enabled := chkEvents.Checked;
  lblEventName.Enabled := chkEvents.Checked;
  eSampleName.Enabled := chkSamples.Checked;
  lblSampleName.Enabled := chkSamples.Checked;
  chkSampleData.Enabled :=chkSamples.Checked;
  eSampleDataName.Enabled := chkSampleData.Checked and chkSamples.Checked;
  lblSampleDataName.Enabled := chkSampleData.Checked and chkSamples.Checked;

  eTaxDictName.Enabled := chkSeparateDict.Checked;
  lblTaxDictName.Enabled := chkSeparateDict.Checked;
  eBioDictName.Enabled := chkSeparateDict.Checked and chkSeparateObs.Checked;
  lblBioDictName.Enabled := chkSeparateDict.Checked and chkSeparateObs.Checked;
  if chkSeparateObs.Checked then
  begin
    if not FtfOpeningDoc then eTaxObsName.Text := 'TaxObs';
    lblTaxObsName.Caption := ResStr_TaxonObservTable;
    if not FtfOpeningDoc then eTaxDictName.Text :='TaxDict';
    lblTaxDictName.Caption :=ResStr_TaxonDictTable;
    eBioObsName.Enabled := True;
    lblBioObsName.Enabled := True;

    chkObsData.Enabled := True;
    if chkObsData.Checked then
    begin
      eBioObsDataName.Enabled := True;
      lblBioObsDataName.Enabled := True;
      eTaxObsDataName.Enabled := True;
      lblTaxObsDataName.Enabled := True;
    end
    else
    begin
      eBioObsDataName.Enabled := False;
      lblBioObsDataName.Enabled := False;
      eTaxObsDataName.Enabled := False;
      lblTaxObsDataName.Enabled := False;
    end;
  end
  else
  begin
    if not FtfOpeningDoc then eTaxObsName.Text := 'Observations';
    lblTaxObsName.Caption := ResStr_ObservationTable;
    if not FtfOpeningDoc then eTaxDictName.Text :='Dictionary';
    lblTaxDictName.Caption :=ResStr_DictionaryTable;

    eBioObsName.Enabled := False;
    lblBioObsName.Enabled := False;

    chkObsData.Enabled := False;

    eBioObsDataName.Enabled := False;
    lblBioObsDataName.Enabled := False;
    eTaxObsDataName.Enabled := False;
    lblTaxObsDataName.Enabled := False;
  end;
  FDirty := True;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.btnNextClick(Sender: TObject);
var
lCursor: TCursor;
begin
  if pcSnapshot.ActivePage = tsServer then
  begin
    lCursor:=HourglassCursor;
    try
      try
        CheckDatabasePermissions; //Causes an exception if user doesn't have enough
                              //permissions.
        pcSnapshot.ActivePage := tsModel
      except
        on TSnapshotException do
          //nothing
      end;
    finally
      DefaultCursor(lCursor);
    end;

  end
  else if pcSnapshot.ActivePage = tsModel then begin
    pcSnapshot.ActivePage := tsTableNames;
    btnNext.Caption := ResStr_Cap_Run;
  end else if pcSnapshot .ActivePage = tsTableNames then
  begin
    lCursor := HourglassCursor;
    try
      try
        CheckDatabasePermissions;
        CreateSnapshot;
      except
        on TSnapshotException do //nothing
      end
    finally
      DefaultCursor(lCursor);
    end;
  end;
  btnPrevious.Enabled := True;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.cmbDatabaseChange(Sender: TObject);
begin
  FstServer := cmbServer.Text;
  FDirty := True;
  pntDataDiagram.Invalidate;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.btnPreviousClick(Sender: TObject);
begin
  btnNext.Caption := ResStr_Cap_Next;
  if  pcSnapshot.ActivePage = tsModel then begin
    pcSnapshot.ActivePage := tsServer;
    btnPrevious.Enabled := False;
  end else if pcSnapshot.ActivePage = tsTableNames then
    pcSnapshot.ActivePage := tsModel;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.eTableNameChange(Sender: TObject);
begin
  pntDataDiagramPaint(pntDataDiagram);
  FDirty := True;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.chkTrustedSecurityClick(Sender: TObject);
begin
  ePassword.Enabled := not chkTrustedSecurity.Checked;
  eUserName.Enabled := not chkTrustedSecurity.Checked;
  lblPassword.Enabled := not chkTrustedSecurity.Checked;
  lblUserName.Enabled := not chkTrustedSecurity.Checked;
  FDirty := True;
end;

{-------------------------------------------------------------------------------
  This function checks that hte user specified on tsServer
  has adequate permissions to do what they want to do.
}
procedure TfrmSnapshot.CheckDatabasePermissions;
  var lstConnection: String;
  lrec: _Recordset;
  liPermissions: Cardinal;
begin
//Do preliminary checks of validity
  if (Trim(cmbServer.Text) = '') or (Trim(cmbServer.Text) = 'Select SQL Server or MSDE') then
  begin
    pcSnapshot.ActivePage := tsServer;
    ActiveControl := cmbServer;
    raise TExceptionPath.CreateNonCritical(ResStr_SnapshotCannotRun);
  end
  else if (Trim(cmbDatabase.Text) = '') or (Trim(cmbDatabase.Text) = 'Select Database on Server') then
  begin
    pcSnapshot.ActivePage := tsServer;
    ActiveControl := cmbDatabase;
    raise TSnapshotException.Create(ResStr_SnapshotCannotRunDB);
  end
  else if (Trim(eUserName.Text)= '') and not chkTrustedSecurity.Checked then
  begin
    pcSnapshot.ActivePage := tsServer;
    ActiveControl := eUsername;
    raise TExceptionPath.CreateNonCritical(ResStr_UsernameBlank);
  end;

  with cnnNewDatabase do
    try
    begin
      lstConnection := 'Data Source=' + FormatForConnectionString(cmbServer.Text) + ';Initial Catalog=master;';
      if chkTrustedSecurity.Checked then
        lstConnection := lstConnection + 'Trusted_Connection=Yes;Integrated Security=SSPI'
      else
        lstConnection := lstConnection + 'User ID=' + FormatForConnectionString(eUserName.Text)
            + ';Password=' + FormatForConnectionString(ePassword.Text) + ';';
      ConnectionString:=lstConnection;
      try
        Open;
      except on E: EOLEException do
        begin
          if Pos('Login failed', E.Message)=1 then
            raise TExceptionPath.CreateNonCritical(E.Message)
          else
            raise TExceptionPath.CreateNonCritical('SQL Server ''' + cmbServer.Text +
                ResStr_ExistOrDenied);
        end;
      end;
    end; //with cnnNewDatabase
    with qryAllPurpose do
    begin
      //find out whether the database exists
      try
        //switch to the new database
        cnnNewDatabase.Execute('Use ' + FormatWithSquareBrackets(cmbDatabase.Text));
        //Check that the user has create table permission.
        lrec :=cnnNewDatabase.Execute('Select Permissions()');
        //Permissions() returns a 32 bit Integer with the bits signifying
        //various permissions.
        liPermissions:=lrec.Fields[0].Value;
        if (liPermissions and 2)=0 then // 2 is create table.
          raise TExceptionPath.CreateNonCritical(Format(ResStr_NoPermissionToDB, [cmbDatabase.Text]));
      except on E: EOLEException do
        //failed to use the new database.
        //Could be because it doesn't exist
        //or because it permission is denied.
        if Pos('Could not locate', E.Message) = 1 then
        begin
          if not chkDatabase.Checked then
            raise TExceptionPath.CreateNonCritical(Format(ResStr_DBNotFound, [cmbDatabase.Text]))
          else
          begin
            //check that we have create database permission
            lrec :=cnnNewDatabase.Execute('select Permissions()');
            //Permissions() returns a 32 bit Integer with the bits signifying
            //various permissions.
            liPermissions:=lrec.Fields[0].Value;
            if (liPermissions and 1)=0 then
              raise TExceptionPath.CreateNonCritical(Format(ResStr_CannotCreateDB, [cmbDatabase.Text]));
          end;
        end
        else
        begin
          if Pos('Server user', E.Message) = 1 then
          begin
            raise TExceptionPath.CreateNonCritical(E.Message);
          end
          else
            raise E;
        end;
      end;
    end; //with qryAllPurpose
  finally
    cnnNewDatabase.Close;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.OpenSnapshot(AstSnapshotFile: String);
var
  lXMLDoc: IXMLDocument;
  lXMLNode: IXMLNode;
  ldlgPassword: TdlgPassword;
  ltfLockWindowResult: LongBool;

  // Relies on lXMLNode to be assigned.
  procedure CheckXMLNodeValue(const ASource: String; ACheckBox: TCheckBox; AEditBox: TEdit);
  begin
    if lXMLNode.Attributes['source'] = ASource then
    begin
      if Assigned(ACheckBox) then ACheckBox.Checked := True;
      AEditBox.Text := lXMLNode.Attributes['dest'];
    end;
  end;

begin
  if FileExists(AstSnapshotFile) then
    try
      lXMLDoc := TXMLDocument.Create(AstSnapshotFile);
      FtfOpeningDoc :=True;
      ltfLockWindowResult := LockWindowUpdate(Handle);
      try
        with lXMLDoc.DocumentElement.ChildNodes['destination'] do
        begin
          if not HasAttribute('trusted') then
          begin
            ldlgPassword := TdlgPassword.Create(nil);
            try
              if ldlgPassword.ShowModal = mrOK then
                ePassword.Text := ldlgPassword.Password
              else
                raise TSnapshotException.Create(ResStr_UserCancelled);
            finally
              ldlgPassword.Free;
            end;
          end;
          eUserName.Text := Attributes['username'];
          cmbServer.Text := Attributes['server'];
          chkTrustedSecurity.Checked := HasAttribute('trusted');
          if fstServer <> cmbServer.Text then
            cmbServerExit(cmbServer);
          fstServer := cmbServer.Text;
          cmbDatabase.Text := Attributes['database'];
          chkDatabase.Checked := HasAttribute('createDB');
          chkOverwriteTables.Checked := HasAttribute('overwrite');
        end;
        //Clear all the check boxes on tsModel
        chkSurveys.Checked      := False;
        chkEvents.Checked       := False;
        chkSamples.Checked      := False;
        chkSeparateObs.Checked  := False;
        chkSeparateDict.Checked := False;
        chkObsData.Checked      := False;
        chkSampleData.Checked   := False;

        if Assigned(lXMLDoc.DocumentElement.ChildNodes['tables']) then
          if lXMLDoc.DocumentElement.ChildNodes['tables'].HasChildNodes then
          begin
            lXMLNode := lXMLDoc.DocumentElement.ChildNodes['tables'].ChildNodes[0];
            while Assigned(lXMLNode) do
            begin
              CheckXMLNodeValue('Survey', chkSurveys, eSurveyName);
              CheckXMLNodeValue('Event', chkEvents, eEventName);
              CheckXMLNodeValue('Sample', chkSamples, eSampleName);
              CheckXMLNodeValue('Sample_Data', chkSampleData, eSampleDataName);
              CheckXMLNodeValue('Observation', nil, eTaxObsName);
              CheckXMLNodeValue('Biotope_Occurrence', chkSeparateObs, eBioObsName);
              CheckXMLNodeValue('Taxon_Occurrence', chkSeparateObs, eTaxObsName);
              CheckXMLNodeValue('Taxon_Occurrence_Data', chkObsData, eTaxObsDataName);
              CheckXMLNodeValue('Biotope_Occurrence_Data', chkObsData, eBioObsDataName);
              CheckXMLNodeValue('Biotope_Dictionary', chkSeparateDict, eBioDictName);
              CheckXMLNodeValue('Taxon_Dictionary', chkSeparateDict, eTaxDictName);
              CheckXMLNodeValue('Dictionary', chkSeparateDict, eTaxDictName);
              lXMLNode := lXMLNode.NextSibling;
            end;//while Assigned(lXMLNode)
          end;//if lXMLDoc.DocumentElement.ChildNodes['tables'].HasChildNodes
      finally
        FtfOpeningDoc := False;
        if ltfLockWindowResult then LockWindowUpdate(0);
      end;
    except
      on EXMLDocError do
        raise TExceptionPath.CreateNonCritical(Format(ResStr_InvalidSnapshotFile, [dlgOpen.FileName]));
      on EDOMParseError do
        raise TExceptionPath.CreateNonCritical(Format(dlgOpen.FileName, [dlgOpen.FileName]));
    end
  else  //if fileExists(dlgOpen.FileName)
    raise TExceptionPath.CreateNonCritical(Format(ResStr_CannotFindFile, [dlgOpen.FileName]));
  // If the FilterResult screen is still up, send the SnapshotName back before closing
  SnapshotFile := AstSnapshotFile;
  FDirty := False;
end;

{-------------------------------------------------------------------------------
  This function takes the data on the page and saves it in an XML document.
}
procedure TfrmSnapshot.SaveSnapshot(AstFileName: String);
var
  lXMLDoc: IXMLDocument;
  lXMLTableNode: IXMLNode;

  procedure AddTable(AParentNode: IXMLNode; AstSource: String; AstDest: String);
  begin
    with AParentNode.AddChild('table') do begin
      Attributes['source'] := AstSource;
      Attributes['dest']   := AstDest;
    end;
  end;

begin
  lXMLDoc := NewXMLDocument;
  lXMLDoc.AddChild('snapshot'); //root node is always 'snapshot'
  with lXMLDoc.DocumentElement do
  begin
    with AddChild('destination') do //first child node
    begin
      Attributes['server']   := cmbServer.Text;
      Attributes['database'] := cmbDatabase.Text;
      Attributes['username'] := eUserName.Text;
      if chkDatabase.Checked        then Attributes['createDB']  := '';
      if chkOverwriteTables.Checked then Attributes['overwrite'] := '';
      if chkTrustedSecurity.Checked then Attributes['trusted']   := '';
    end;
    lXMLTableNode:= AddChild('tables') //second child node
  end;
  if chkSeparateObs.Checked then
  begin
    AddTable(lXMLTableNode, 'Taxon_Occurrence', eTaxObsName.Text);
    AddTable(lXMLTableNode, 'Biotope_Occurrence', eBioObsName.Text);
    if chkObsData.checked then
    begin
      AddTable(lXMLTableNode, 'Taxon_Occurrence_Data', eTaxObsDataName.Text);
      AddTable(lXMLTableNode, 'Biotope_Occurrence_Data', eBioObsDataName.Text);
    end;
    if chkSeparateDict.Checked then
    begin
      AddTable(lXMLTableNode, 'Taxon_Dictionary', eTaxDictName.Text);
      AddTable(lXMLTableNode, 'Biotope_Dictionary', eBioDictName.Text);
    end;
  end
  else
  begin
    AddTable(lXMLTableNode, 'Observation', eTaxObsName.Text);
    if chkSeparateDict.Checked then
      AddTable(lXMLTableNode, 'Dictionary', eTaxDictName.Text);
  end;
  if chkSamples.Checked then
    begin
      AddTable(lXMLTableNode, 'Sample', eSampleName.Text);
      if chkSampleData.Checked then
        AddTable(lXMLTableNode, 'Sample_Data', eSampleDataName.Text);
    end;
  if chkSurveys.Checked then
    AddTable(lXMLTableNode, 'Survey', eSurveyName.Text);
  if chkEvents.Checked then
    AddTable(lXMLTableNode, 'Event', eEventName.Text);
  lXMLDoc.SaveToFile(AstFileName);
  // If the FilterResult screen is still up, send the TemplateName back before closing
  SnapshotFile := AstFileName;
  FDirty := False;
end;

{-------------------------------------------------------------------------------
  Attempt to query the server for a list of databases on leaving the Server
  combo box
}
procedure TfrmSnapshot.cmbServerExit(Sender: TObject);
var
  lCursor: TCursor;
  lstConnection: String;
begin
  if (FstServer <>cmbServer.Text) and ((eUserName.Text<>'') or chkTrustedSecurity.Checked) then
  begin
    cmbDatabase.Items.Clear;
    lCursor := HourglassCursor;
    try
      with cnnNewDatabase do
      begin
        lstConnection := 'Server=' + cmbServer.Text + ';Database=master;';
        if chkTrustedSecurity.Checked then
          lstConnection := lstConnection + 'Trusted_Connection=Yes;Integrated Security=SSPI'
        else
          lstConnection := lstConnection + 'uid=' + eUserName.Text+ ';pwd='+ ePassword.Text;
        ConnectionString:=lstConnection;
        try
          Open;
          try
            qryAllPurpose.SQL.Clear;
            qryAllPurpose.SQL.Add('SELECT Name FROM dbo.SysDatabases ORDER BY 1');
            qryAllPurpose.Open;
            try
              qryAllPurpose.First;
              while not qryAllPurpose.EOF do
              begin
                cmbDatabase.Items.Add(qryAllPurpose.Fields[0].AsString);
                qryAllPurpose.Next;
              end;
            finally
              qryAllPurpose.Close;
            end;
        finally
          Close; //close the connection
        end;
       except on EOLEException do
          //Don't warn the user at this stage
        end;
      end; //with cnnNewDatabase
    finally
      DefaultCursor(lCursor);
    end;
  end; //if FstServer<>cmbServer.Text
  FstServer := cmbServer.Text;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.cmbServerEnter(Sender: TObject);
begin
  FstServer:=cmbServer.Text;
end;

{-------------------------------------------------------------------------------
  This procedure writes the snapshot to the database.
}
procedure TfrmSnapshot.CreateSnapshot;
var
  lTableName: String;
  lstrlDeleteTables: TStringList;
  lrec: _Recordset;
  i: Integer;
  //Strings for building SQL
  lstSQL: String;
  lstColumns: String;
  lstAttributes: String;
  lstObservationColumns, lstObservationSQL: String;
  lstBiotopeDataColumns, lstBiotopeDataSQL: String;
  lstTaxonDataColumns, lstTaxonDataSQL: String;
  lstSampleDataColumns, lstSampleDataSQL: String;
  lstSampleColumns, lstSampleSQL: String;
  lstSurveyColumns, lstSurveySQL: String;
  lstEventColumns, lstEventSQL: String;
  lstBiotopeColumns, lstBiotopeSQL: String;
  lstTaxonColumns, lstTaxonSQL: String;
  lstBiotopeDictColumns, lstBiotopeDictSQL: String;
  lstTaxonDictColumns, lstTaxonDictSQL: String;

  //----------------------------------------------------------------------------
  {This function raises an exception if the table names are blank.}
  procedure CheckTableName(ACon: TEdit);
  begin
    if (Trim(ACon.Text) = '') and Acon.Enabled then
    begin
      pcSnapshot.ActivePage := tsTableNames;
      ActiveControl := ACon;
      raise TExceptionPath.CreateNonCritical(ResStr_BlankTableName);
    end;
  end;

  //----------------------------------------------------------------------------
  {This function says whether a table needs to be deleted (i.e. if it exists.) Raises
  an exception if the user does not have permission to delete the table}
  function MustDeleteTable(AstTableName: String) :Boolean;
  var
    liPermission: Integer;
  begin
    lrec := cnnNewDatabase.Execute('SELECT Permissions(Object_ID(''[dbo].' + FormatWithSquareBrackets(AstTableName) + '''))');
    if lrec.Fields[0].Value = Null then
      Result := False
    else
    begin
      liPermission := lrec.Fields[0].Value;
      if (liPermission and 16) <>0 then
        Result:=True
      else begin
        Result := False;
        TExceptionPath.CreateNonCritical(Format(ResStr_NoPermissionToDelete, [cmbDatabase.Text, AstTableName]));
      end;
    end;
  end;

  //----------------------------------------------------------------------------
  {This function puts the contents of a cursor into a database table}
  function PopulateTableFromCursor(Arec: _Recordset; AstTableName: String;
      AtfCheckPrimaryKey: Boolean=False; AstPrimaryKeyField1: String = '';
       AstPrimaryKeyField2: String = ''): Boolean;
  var
    i, j: Integer;
    lADOTable: TADOTable;
    lstOldPrimaryKey1: String;
    lstOldPrimaryKey2: String;
    liNumRecords: Integer;
    liNumRecordsOver100: Integer;
    liStepSize: Integer;

  begin
    if (Arec.Fields.Count >0) and not(ARec.EOF and ARec.BOF) then
    begin
      frmMain.SetStatus(Format(ResStr_CreatingTable, [AstTableName]));
      lADOTable := TADOTable.Create(nil);
      try
        lADOTable.TableName := AstTableName;
        lADOTable.CursorLocation := clUseServer;
        lADOTable.Connection := cnnNewDatabase;
        lADOTable.Open;
        liNumRecords := ARec.RecordCount;
        liNumRecordsOver100 := Trunc(liNumRecords/100);
        //Find the
        liStepSize :=1;
        while liNumRecordsOver100 > 0 do
        begin
          liNumRecordsOver100 := liNumRecordsOver100 shr 1;
          liStepSize := liStepSize shl 1;
        end;
        liStepSize := (liStepSize shr 1) - 1;

        i :=0;
        if not AtfCheckPrimaryKey then
          with Arec do
          begin
            MoveFirst;
            while not Eof do
            begin
              inc(i);
              if (i and liStepSize) = 0 then
                frmMain.SetProgress(Trunc(i/liNumRecords * 100));
              lADOTable.Append;
              for j:=0 to Fields.Count -1 do
              begin
                lADOTable.Fields[j].Value := Fields[j].Value;
              end;
              try
                lAdoTable.Post;
              except on EDatabaseError do
                //do nothing, just try the next row.
                //Most likely because inserted row was violation of
                //primary key or some other constraint. Can't do SELECT DISTINCT when
                //forming Arec if it has TEXT fields in it.
                lAdoTable.Cancel;
              end;
              MoveNext;
            end;
          end
        //otherwise check for duplicates in primary keys first
        else if AstPrimaryKeyField2 = '' then
          with Arec do
          begin
            Sort := AstPrimaryKeyField1;
            MoveFirst;
            lstOldPrimaryKey1 := '';
            while not Eof do
            begin
              inc(i);
              if (i and liStepSize) = 0 then
                frmMain.SetProgress(Trunc(i/liNumRecords * 100));
              if lstOldPrimaryKey1 <> Fields[AstPrimaryKeyField1].Value then
              begin
                lADOTable.Append;
                for j:=0 to Fields.Count -1 do
                  lADOTable.Fields[j].Value := Fields[j].Value;
                lstOldPrimaryKey1 := Fields[AstPrimaryKeyField1].Value;
                lAdoTable.Post;
              end;
              MoveNext;
            end;
          end
        else
          with Arec do
          begin
            Sort := AstPrimaryKeyField1 + ', ' + AstPrimaryKeyField2;
            MoveFirst;
            lstOldPrimaryKey1 := '';
            lstOldPrimaryKey2 := '';
            while not Eof do
            begin
              inc(i);
              if (i and liStepSize) = 0 then
                frmMain.SetProgress(Trunc(i/liNumRecords * 100));
              if (lstOldPrimaryKey1 <> Fields[AstPrimaryKeyField1].Value)
                  or (lstOldPrimaryKey2 <> Fields[AstPrimaryKeyField2].Value) then
              begin
                lADOTable.Append;
                for j:=0 to Fields.Count -1 do
                   lADOTable.Fields[j].Value := Fields[j].Value;
                lstOldPrimaryKey1 := Fields[AstPrimaryKeyField1].Value;
                lstOldPrimaryKey2 := Fields[AstPrimaryKeyField2].Value;
                lAdoTable.Post;
              end;
              MoveNext;
            end;
          end;
      finally
        lAdoTable.Close;
        lAdoTable.Free;
      end;
      Result:=True;
    end
    else
      Result := False;
  end;

  procedure PutAttributeInStrings(AAttribute: TAttribute; var AstColumns, AstSQL: String);
    var j: Integer;
  begin
    with AAttribute do
      for j := 0 to AttributeFields.Count -1 do
        with TAttributeField(AttributeFields[j]) do begin
          AddToCommaSeparatedList(AstColumns, FormatWithSquareBrackets(FieldName));
          AddToCommaSeparatedList(AstSQL, FormatWithSquareBrackets(FieldName) + FieldType);
          if IsCharType(FieldType) then
            AstSQL := AstSQL + '(' + FieldSize  + ')';
        end;
  end;
  //----------------------------------------------------------------------------
begin
  //Check that the table names aren't blank
  CheckTableName(eSurveyName);
  CheckTableName(eEventName);
  CheckTableName(eSampleName);
  CheckTableName(eSampleDataName);
  CheckTableName(eTaxObsName);
  CheckTableName(eBioObsName);
  CheckTableName(eTaxObsDataName);
  CheckTableName(eBioObsDataName);
  CheckTableName(eTaxDictName);
  CheckTableName(eBioDictName);

  //Switch to the database, create it if it doesn't exist  
  try
    try
      cnnNewDatabase.Execute('Use ' + FormatWithSquareBrackets(cmbDatabase.Text));
      //find any existing tables. Use Permissions rather than querying sysobjects
      // because the user may be denied select rights from sysobjects.
      lstrlDeleteTables := TStringList.Create();
      if eTaxObsName.Enabled then
        if MustDeleteTable(eTaxObsName.Text) then
          lstrlDeleteTables.Add(eTaxObsName.Text);
      if eBioObsName.Enabled then
        if MustDeleteTable(eBioObsName.Text) then
          lstrlDeleteTables.Add(eBioObsName.Text);
      if eTaxObsDataName.Enabled then
        if MustDeleteTable(eTaxObsDataName.Text) then
          lstrlDeleteTables.Add(eTaxObsDataName.Text);
      if eBioObsDataName.Enabled then
        if MustDeleteTable(eBioObsDataName.Text) then
          lstrlDeleteTables.Add(eBioObsDataName.Text);
      if eTaxDictName.Enabled then
        if MustDeleteTable(eTaxDictName.Text) then
          lstrlDeleteTables.Add(eTaxDictName.Text);
      if eBioDictName.Enabled then
        if MustDeleteTable(eBioDictName.Text) then
          lstrlDeleteTables.Add(eBioDictName.Text);
      if eSurveyName.Enabled then
        if MustDeleteTable(eSurveyName.Text) then
          lstrlDeleteTables.Add(eSurveyName.Text);
      if eSampleName.Enabled then
        if MustDeleteTable(eSampleName.Text) then
          lstrlDeleteTables.Add(eSampleName.Text);
      if eSampleDataName.Enabled then
        if MustDeleteTable(eSampleDataName.Text) then
          lstrlDeleteTables.Add(eSampleDataName.Text);
      if eEventName.Enabled then
        if MustDeleteTable(eEventName.Text) then
          lstrlDeleteTables.Add(eEventName.Text);
      if not chkOverwriteTables.Checked then
        if lstrlDeleteTables.Count >0 then
          if CreateMessageDialog(ResStr_ListDeleteTable + #13#10 +
              lstrlDeleteTables.CommaText, mtWarning, [mbOK, mbCancel]).ShowModal = mrCancel then
            Raise TSnapshotException.Create(ResStr_UserCancelled);// abort snapshot

      //Try to delete all
      //Check to see whether have permission to select from sysobjects & sysconstriants

      lrec := cnnNewDatabase.Execute('SELECT Permissions(object_ID(''dbo.sysobjects'')) & Permissions(object_ID(''dbo.sysforeignkeys'')) &1');
      if lrec.Fields[0].Value =0 then
      begin
        try
          for i := 0 to lstrlDeleteTables.Count -1 do begin
            lTableName := lstrlDeleteTables[i];
            cnnNewDatabase.Execute('DROP TABLE [dbo].' + FormatWithSquareBrackets(lTableName));
          end;
        except on EOLEException do
          TExceptionPath.Create(Format(ResStr_CannotDropTable, [lTableName]));
        end;
      end
      else
      //Delete all foreign keys pointing to the tables.
      //qryForeignKeys produces SQL which deletes the foreign keys.
        for i := 0 to lstrlDeleteTables.Count -1 do
        begin
          lTableName := lstrlDeleteTables[i];
          with qryForeignKeys do
          begin
            Parameters.ParamValues['TableName']:='[dbo].'+ FormatWithSquareBrackets(lTableName);
            Open;
            try
              First;
              while not Eof do
              begin
                try
                  cnnNewDatabase.Execute(Fields[0].Value);
                except
                  on EOLEException do
                    TExceptionPath.CreateNonCritical(Format(ResStr_CannotDeleteTableRef, [lTableName]));
                end;
                Next;
              end;//while not eof
            finally
              Close;
            end;
          end;//with qryForeignKeys
          //Attempt to drop the table
          try
            cnnNewDatabase.Execute('DROP TABLE [dbo].' + FormatWithSquareBrackets(lTableName));
          except
            on EOLEException do
              TExceptionPath.Create(Format(ResStrCannotDeleteTable, [lTableName]));
          end;
        end;//for i := 0 to lstrlDeleteTables.Count -1
    except
      on EOLEException do
      begin
        cnnNewDatabase.Execute('CREATE DATABASE ' + FormatWithSquareBrackets(cmbDatabase.Text));
        cnnNewDatabase.Execute('USE ' + FormatWithSquareBrackets(cmbDatabase.Text));
      end;
    end;
    //Initialise strings to be used in the building of Create and Select
    //SQL statements
    lstAttributes :='';
    lstBiotopeDataColumns := ''; lstBiotopeDataSQL := '';
    lstTaxonDataColumns   := ''; lstTaxonDataSQL   := '';
    lstSampleDataColumns  := ''; lstSampleDataSQL  := '';
    lstSampleColumns      := ''; lstSampleSQL      := '';
    lstSurveyColumns      := ''; lstSurveySQL      := '';
    lstEventColumns       := ''; lstEventSQL       := '';
    lstBiotopeColumns     := ''; lstBiotopeSQL     := '';
    lstTaxonColumns       := ''; lstTaxonSQL       := '';
    lstBiotopeDictColumns := ''; lstBiotopeDictSQL := '';
    lstTaxonDictColumns   := ''; lstTaxonDictSQL   := '';
    lstObservationColumns := ''; lstObservationSQL := '';

    //Create a list of the attributes that the user has selected to be included in
    //the report. Form an IN clause with it.
    //Where AttrType is atAttribute the field is one of the predefined columns.
    //Where AttrType is atMeasurement the field is a measurement and is not in the
    //REPORT_ATTRIBUTE table.
    for i := 0 to FAttributes.Count - 1 do
      with TAttribute(FAttributes.Objects[i]) do
        if Selected or (Sort <>stNone) then
          if AttrType = atAttribute  then
          begin
            if LowerCase(SourceTable) = 'survey' then
              PutAttributeInStrings(TAttribute(FAttributes.Objects[i]), lstSurveyColumns, lstSurveySQL)
            else if LowerCase(SourceTable) = 'survey_event' then
              PutAttributeInStrings(TAttribute(FAttributes.Objects[i]), lstEventColumns, lstEventSQL)
            else if LowerCase(SourceTable) = 'sample' then
              PutAttributeInStrings(TAttribute(FAttributes.Objects[i]), lstSampleColumns, lstSampleSQL)
            else if LowerCase(SourceTable) = 'observation' then
              PutAttributeInStrings(TAttribute(FAttributes.Objects[i]), lstObservationColumns, lstObservationSQL)
            else if LowerCase(SourceTable) = 'taxon' then
              PutAttributeInStrings(TAttribute(FAttributes.Objects[i]), lstTaxonColumns, lstTaxonSQL)
            else if LowerCase(SourceTable) = 'biotope' then
              PutAttributeInStrings(TAttribute(FAttributes.Objects[i]), lstBiotopeColumns, lstBiotopeSQL)
            else if LowerCase(SourceTable) = 'taxon_list' then
              PutAttributeInStrings(TAttribute(FAttributes.Objects[i]), lstTaxonDictColumns, lstTaxonDictSQL)
            else if LowerCase(SourceTable) = 'biotope_list' then
              PutAttributeInStrings(TAttribute(FAttributes.Objects[i]), lstBiotopeDictColumns, lstBiotopeDictSQL);
          end
          else if MeasurementContextTable = 'TAXON_OCCURRENCE_DATA' then
          begin
            AddToCommaSeparatedList(lstTaxonDataColumns, FormatWithSquareBrackets(Name));
            AddToCommaSeparatedList(lstTaxonDataSQL, FormatWithSquareBrackets(Name) + ' Varchar(10)');
          end
          else if MeasurementContextTable = 'BIOTOPE_OCCURRENCE_DATA' then
          begin
            AddToCommaSeparatedList(lstBiotopeDataColumns , FormatWithSquareBrackets(Name));
            AddToCommaSeparatedList(lstBiotopeDataSQL, FormatWithSquareBrackets(Name) + ' Varchar(10)');
          end
          else if (MeasurementContextTable = 'SAMPLE_DATA') or (MeasurementContextTable = 'LOCATION_DATA') then
          begin
            AddToCommaSeparatedList(lstSampleDataColumns, FormatWithSquareBrackets(Name));
            AddToCommaSeparatedList(lstSampleDataSQL, FormatWithSquareBrackets(Name) + ' Varchar(10)');
          end;

    //Now create the tables.
    if eSurveyName.Enabled then
    begin
    //Survey
      lstSQL := 'CREATE TABLE [dbo].' + FormatWithSquareBrackets(eSurveyName.Text) + ' ([SURVEY_KEY] Char(16) Primary Key';
      lstColumns := '[SURVEY_KEY]';
      AddToCommaSeparatedList(lstSQL, lstSurveySQL);
      AddToCommaSeparatedList(lstColumns, lstSurveyColumns);
      lstSQL := lstSQL + ')';
      cnnNewDatabase.Execute(lstSQL);
      lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns + ' FROM ' + REPORT_TABLE);
      PopulateTableFromCursor(lrec, eSurveyName.Text, True, 'SURVEY_KEY');
    end;// if eSurveyName.Enabled

    if eEventName.Enabled then
    begin
      //Survey Event
      lstSQL := 'CREATE TABLE [dbo].' + FormatWithSquareBrackets(eEventName.Text) + ' ([SURVEY_EVENT_KEY] Char(16) Primary Key, ' +
            '[SURVEY_KEY] Char(16) ';
      lstColumns := '[SURVEY_EVENT_KEY], [SURVEY_KEY] ';
      AddToCommaSeparatedList(lstSQL, lstEventSQL);
      AddToCommaSeparatedList(lstColumns, lstEventColumns);
      if not eSurveyName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, lstSurveySQL);
        AddToCommaSeparatedList(lstColumns, lstSurveyColumns);
      end;
      lstSQL := lstSQL + ')';
      cnnNewDatabase.Execute(lstSQL);
      lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns + ' FROM ' + REPORT_TABLE);
      PopulateTableFromCursor(lrec, eEventName.Text, True, 'SURVEY_EVENT_KEY');
    end;//if eEventName.Enabled

    if eSampleName.Enabled then
    begin
      //Sample
      lstSQL := 'CREATE TABLE [dbo].' + FormatWithSquareBrackets(eSampleName.Text) +
            ' ([SAMPLE_KEY] Char(16) Primary Key, ' +
            '[SURVEY_EVENT_KEY] Char(16) ';
      lstColumns := '[SAMPLE_KEY], [SURVEY_EVENT_KEY]';
      if not EEventName.Enabled then //have to put the survey keys in as well
      begin
        AddToCommaSeparatedList(lstSQL, '[SURVEY_KEY] Char(16) ');
        AddToCommaSeparatedList(lstColumns, '[SURVEY_KEY]');
      end;
      AddToCommaSeparatedList(lstSQL, lstSampleSQL);
      AddToCommaSeparatedList(lstColumns, lstSampleColumns);
      if not eEventName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, lstEventSQL);
        AddToCommaSeparatedList(lstColumns, lstEventColumns);
        if not eSurveyName.Enabled then
        begin
          AddToCommaSeparatedList(lstSQL, lstSurveySQL);
          AddToCommaSeparatedList(lstColumns, lstSurveyColumns);
        end;
      end;
      if not eSampleDataName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL , lstSampleDataSQL);
        AddToCommaSeparatedList(lstColumns, lstSampleDataColumns);
      end;
      lstSQL := lstSQL + ')';
      cnnNewDatabase.Execute(lstSQL);
      lrec := FCnnOldConnection.Execute('SELECT ' + lstColumns + ' FROM ' + REPORT_TABLE);
      PopulateTableFromCursor(lrec,eSampleName.Text, True, 'SAMPLE_KEY');
      if eSampleDataName.Enabled then
      begin
        lstSQL :='CREATE TABLE [dbo].' + FormatWithSquareBrackets(eSampleDataName.Text) +
              ' ([SAMPLE_KEY] Char(16) Primary Key';
        AddToCommaSeparatedList(lstSQL, lstSampleDataSQL);
        lstColumns := '[SAMPLE_KEY]';
        AddToCommaSeparatedList(lstColumns, lstSampleDataColumns);
        lstSQL :=lstSQL + ')';
        cnnNewDatabase.Execute(lstSQL);
        lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns + ' FROM ' + REPORT_TABLE);
        PopulateTableFromCursor(lrec, eSampleDataName.Text, True, 'SAMPLE_KEY');
      end;
    end;//if eSampleName.Enabled
    //Observations
    if not eBioObsName.Enabled then
    begin
      lstSQL := 'CREATE TABLE [dbo].' + FormatWithSquareBrackets(eTaxObsName.Text) +
            ' ([OCCURRENCE_KEY] Char(16), ' +
            '[TYPE] Char(1), ' +
            '[LIST_ITEM_KEY] Char(16), ' +
            '[SAMPLE_KEY] Char(16) ';
      lstColumns := '[OCCURRENCE_KEY], [TYPE], [LIST_ITEM_KEY], [SAMPLE_KEY]';
      //Might need more information if other tables are absent
      if not eSampleName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, '[SURVEY_EVENT_KEY] Char(16)');
        AddToCommaSeparatedList(lstColumns, '[SURVEY_EVENT_KEY]');
        if not eEventName.Enabled then
        begin
          AddToCommaSeparatedList(lstSQL, '[SURVEY_KEY] Char(16)');
          AddToCommaSeparatedList(lstColumns, ' [SURVEY_KEY]');
        end;
      end;
      AddToCommaSeparatedList(lstSQL, lstObservationSQL);
      AddToCommaSeparatedList(lstColumns, lstObservationColumns);
      AddToCommaSeparatedList(lstSQL, lstTaxonSQL);
      AddToCommaSeparatedList(lstColumns, lstTaxonColumns);
      AddToCommaSeparatedList(lstSQL, lstBiotopeSQL);
      AddToCommaSeparatedList(lstColumns, lstBiotopeColumns);

      //When taxa and biotopes observations are bunched together, always
      //include occurrence data.
      AddToCommaSeparatedList(lstSQL, lstTaxonDataSQL);
      AddToCommaSeparatedList(lstColumns, lstTaxonDataColumns);
      AddToCommaSeparatedList(lstSQL, lstBiotopeDataSQL);
      AddToCommaSeparatedList(lstColumns, lstBiotopeDataColumns);
      if not eTaxDictName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, lstTaxonDictSQL);
        AddToCommaSeparatedList(lstColumns, lstTaxonDictColumns);
        AddToCommaSeparatedList(lstSQL, lstBiotopeDictSQL);
        AddToCommaSeparatedList(lstColumns, lstBiotopeDictColumns);
      end;
      if not eSampleName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, lstSampleSQL);
        AddToCommaSeparatedList(lstColumns, lstSampleColumns);
        if not eEventName.Enabled then
        begin
          AddToCommaSeparatedList(lstSQL, lstEventSQL);
          AddToCommaSeparatedList(lstColumns, lstEventColumns);
          if not eSurveyName.Enabled then
          begin
            AddToCommaSeparatedList(lstSQL, lstSurveySQL);
            AddToCommaSeparatedList(lstColumns, lstSurveyColumns);
          end;
        end;
      end;
      lstSQL := lstSQL + ' PRIMARY KEY ([TYPE],[OCCURRENCE_KEY]))';
      cnnNewDatabase.Execute(lstSQL);
      lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns + ' FROM ' + REPORT_TABLE);
      try
        PopulateTableFromCursor(lrec,eTaxObsName.Text);
      finally
        lrec.Close;
      end;

      if eTaxDictName.Enabled then
      begin
         //Observation dictionary
        lstSQL := 'CREATE TABLE [dbo].' + FormatWithSquareBrackets(eTaxDictName.Text) +
              ' ([TYPE] Char(1) , ' +
              '[LIST_ITEM_KEY] Char(16) ';
        lstColumns := '[TYPE], [LIST_ITEM_KEY]';
        AddToCommaSeparatedList(lstSQL, lstTaxonDictSQL);
        AddToCommaSeparatedList(lstColumns, lstTaxonDictColumns);
        AddToCommaSeparatedList(lstSQL, lstBiotopeDictSQL);
        AddToCommaSeparatedList(lstColumns, lstBiotopeDictColumns);
        lstSQL := lstSQL + ' PRIMARY KEY ([TYPE],[LIST_ITEM_KEY]))';
        cnnNewDatabase.Execute(lstSQL);
        lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns + ' FROM ' + REPORT_TABLE);
        try
          PopulateTableFromCursor(lrec,eTaxDictName.Text, True, 'LIST_ITEM_KEY', 'TYPE');
        finally
          lrec.Close;
        end;
      end;//if eTaxDictName.Enabled
    end //if not eBioObsName.Enabled
    else
    //Separate taxon and biotope observations
    begin
      //taxon occurrence
      lstSQL := 'CREATE TABLE [dbo].' + FormatWithSquareBrackets(eTaxObsName.Text) +
            ' ([OCCURRENCE_KEY] Char(16) Primary Key, ' +
            '[LIST_ITEM_KEY] Char(16), ' +
            '[SAMPLE_KEY] Char(16)';
      lstColumns := '[OCCURRENCE_KEY], [LIST_ITEM_KEY], [SAMPLE_KEY]';
      //Might need more information if other tables are absent
      if not eSampleName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, '[SURVEY_EVENT_KEY] Char(16)');
        AddToCommaSeparatedList(lstColumns, '[SURVEY_EVENT_KEY]');
        if not eEventName.Enabled then
        begin
          AddToCommaSeparatedList(lstSQL, '[SURVEY_KEY] Char(16)');
          AddToCommaSeparatedList(lstColumns, ' [SURVEY_KEY]');
        end;
      end;
      AddToCommaSeparatedList(lstSQL, lstObservationSQL);
      AddToCommaSeparatedList(lstColumns, lstObservationColumns);
      AddToCommaSeparatedList(lstSQL, lstTaxonSQL);
      AddToCommaSeparatedList(lstColumns, lstTaxonColumns);
      if not eTaxObsDataName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, lstTaxonDataSQL);
        AddToCommaSeparatedList(lstColumns, lstTaxonDataColumns);
      end;
      if not eTaxDictName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, lstTaxonDictSQL);
        AddToCommaSeparatedList(lstColumns, lstTaxonDictColumns);
      end;
      if not eSampleName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, lstSampleSQL);
        AddToCommaSeparatedList(lstColumns, lstSampleColumns);
        if not eEventName.Enabled then
        begin
          AddToCommaSeparatedList(lstSQL, lstEventSQL);
          AddToCommaSeparatedList(lstColumns, lstEventColumns);
          if not eSurveyName.Enabled then
          begin
            AddToCommaSeparatedList(lstSQL, lstSurveySQL);
            AddToCommaSeparatedList(lstColumns, lstSurveyColumns);
          end;
        end;
      end;
      lstSQL := lstSQL + ')';
      cnnNewDatabase.Execute(lstSQL);
      lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns + ' FROM '
        + REPORT_TABLE + ' WHERE [Type] = ''T''');
      PopulateTableFromCursor(lrec,eTaxObsName.Text);

      //Biotope Occurrence
      lstSQL := 'Create table [dbo].' + FormatWithSquareBrackets(eBioObsName.Text) +
            ' ([OCCURRENCE_KEY] Char(16) Primary Key, ' +
            '[LIST_ITEM_KEY] Char(16), ' +
            '[SAMPLE_KEY] Char(16)';
      lstColumns := '[OCCURRENCE_KEY], [LIST_ITEM_KEY], [SAMPLE_KEY]';
      //Might need more information if other tables are absent
      if not eSampleName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, '[SURVEY_EVENT_KEY] Char(16)');
        AddToCommaSeparatedList(lstColumns, '[SURVEY_EVENT_KEY]');
        if not eEventName.Enabled then
        begin
          AddToCommaSeparatedList(lstSQL, '[SURVEY_KEY] Char(16)');
          AddToCommaSeparatedList(lstColumns, ' [SURVEY_KEY]');
        end;
      end;AddToCommaSeparatedList(lstSQL, lstObservationSQL);
      AddToCommaSeparatedList(lstColumns, lstObservationColumns);
      AddToCommaSeparatedList(lstSQL, lstBiotopeSQL);
      AddToCommaSeparatedList(lstColumns, lstBiotopeColumns);
      if not eTaxObsDataName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, lstBiotopeDataSQL);
        AddToCommaSeparatedList(lstColumns, lstBiotopeDataColumns);
      end;
      if not eTaxDictName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, lstBiotopeDictSQL);
        AddToCommaSeparatedList(lstColumns, lstBiotopeDictColumns);
      end;
      if not eSampleName.Enabled then
      begin
        AddToCommaSeparatedList(lstSQL, lstSampleSQL);
        AddToCommaSeparatedList(lstColumns, lstSampleColumns);
        if not eEventName.Enabled then
        begin
          AddToCommaSeparatedList(lstSQL, lstEventSQL);
          AddToCommaSeparatedList(lstColumns, lstEventColumns);
          if not eSurveyName.Enabled then
          begin
            AddToCommaSeparatedList(lstSQL, lstSurveySQL);
            AddToCommaSeparatedList(lstColumns, lstSurveyColumns);
          end;
        end;
      end;
      lstSQL := lstSQL + ')';
      cnnNewDatabase.Execute(lstSQL);
      lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns + ' FROM '
        + REPORT_TABLE + ' WHERE [Type] = ''B''');
      PopulateTableFromCursor(lrec,eBioObsName.Text);

      //Do the taxon and Biotope occurrence data tables
      if eTaxObsDataName.enabled then
      begin
        //Taxon occurrence data
        lstSQL :='CREATE TABLE [dbo].' + FormatWithSquareBrackets(eTaxObsDataName.Text) +
              ' ([OCCURRENCE_KEY] Char(16) Primary Key';
        lstColumns := '[OCCURRENCE_KEY]';
        AddToCommaSeparatedList(lstSQL, lstTaxonDataSQL);
        AddToCommaSeparatedList(lstColumns, lstTaxonDataColumns);
        lstSQL :=lstSQL + ')';
        cnnNewDatabase.Execute(lstSQL);
        lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns +
            ' FROM ' + REPORT_TABLE + ' WHERE [TYPE] = ''T''');
        PopulateTableFromCursor(lrec, eTaxObsDataName.Text);

        //Biotope occurrence data
        lstSQL :='CREATE TABLE [dbo].' + FormatWithSquareBrackets(eBioObsDataName.Text) +
              ' ([OCCURRENCE_KEY] Char(16) PRIMARY KEY';
        AddToCommaSeparatedList(lstSQL, lstBiotopeDataSQL);
        lstColumns := '[OCCURRENCE_KEY]';
        AddToCommaSeparatedList(lstColumns, lstBiotopeDataColumns);
        lstSQL :=lstSQL + ')';
        cnnNewDatabase.Execute(lstSQL);
        lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns +
            ' FROM ' + REPORT_TABLE + ' WHERE [TYPE] = ''B''');
        PopulateTableFromCursor(lrec, eBioObsDataName.Text);
      end;
      //do the dictionaries
      if eTaxDictName.Enabled then
      begin
         //Taxon dictionary
        lstSQL := 'CREATE TABLE [dbo].' + FormatWithSquareBrackets(eTaxDictName.Text) +
              ' ([LIST_ITEM_KEY] Char(16) PRIMARY KEY';
        lstColumns := '[LIST_ITEM_KEY]';
        AddToCommaSeparatedList(lstSQL, lstTaxonDictSQL);
        AddToCommaSeparatedList(lstColumns, lstTaxonDictColumns);
        lstSQL := lstSQL + ')';
        cnnNewDatabase.Execute(lstSQL);
        lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns + ' FROM '
             + REPORT_TABLE + ' WHERE [type] = ''T''');
        PopulateTableFromCursor(lrec,eTaxDictName.Text, True, 'LIST_ITEM_KEY');

        //Biotope Dictionary
        lstSQL := 'CREATE TABLE [dbo].' + FormatWithSquareBrackets(eBioDictName.Text) +
              ' ([LIST_ITEM_KEY] Char(16) PRIMARY KEY';
        lstColumns := '[LIST_ITEM_KEY]';
        AddToCommaSeparatedList(lstSQL, lstBiotopeDictSQL);
        AddToCommaSeparatedList(lstColumns, lstBiotopeDictColumns);
        lstSQL := lstSQL + ')';
        cnnNewDatabase.Execute(lstSQL);
        lrec := FcnnOldConnection.Execute('SELECT ' + lstColumns + ' FROM '
             + REPORT_TABLE + ' WHERE [Type] = ''B''');
        PopulateTableFromCursor(lrec,eBioDictName.Text, True, 'LIST_ITEM_KEY');
      end;
    end;//if not eBioObsName.Enabled
    ShowInformation('Snapshot created in ' + cmbServer.Text + '.' + cmbDatabase.Text);
  finally
    cnnNewDatabase.Close;
    frmMain.SetStatus('');
    frmMain.SetProgress(0);
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  // Disassociate the report designer from the filter Result screen.
  dmFormActions.actPrint.Enabled   := False;
  dmFormActions.actPreview.Enabled := False;
  frmMain.mnuFileSave.Enabled      := False;
  frmMain.mnuFileSaveAs.Enabled    := False;

  // If the FilterResult screen is still up, send the TemplateName back before closing
  if Owner<>nil then
    TfrmFilterResult(Owner).Snapshot := nil;

  Action := caFree;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.FormActivate(Sender: TObject);
begin
  inherited;
  frmMain.ClearContextToolbar(True);
  { Need to unlock window as clear context toolbar locks it }
  LockWindowUpdate(0);
  dmFormActions.actPrint.Enabled  := True;
  dmFormActions.actPreview.Enabled:= True;
  frmMain.mnuFileOpen.Visible     := True;
  frmMain.mnuFileSave.Enabled     := True;
  frmMain.mnuFileSaveAs.Enabled   := True
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.FormDeactivate(Sender: TObject);
begin
  inherited;
  dmFormActions.actPrint.Enabled  := False;
  dmFormActions.actPreview.Enabled:= False;
  frmMain.mnuFileOpen.Visible     := False;
  frmMain.mnuFileSave.Enabled     := False;
  frmMain.mnuFileSaveAs.Enabled   := False;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.SetSnapshotFile(Value: String);
  var lstShortPathName: String;
begin
  FstSnapshotFile := Value;

  if Value <> '' then
  begin
    lstShortPathName := ExtractShortPathName(IncludeTrailingPathDelimiter(Appsettings.SnapshotPath));
    if (lstShortPathName <> '') and (ExtractShortPathName(IncludeTrailingPathDelimiter(ExtractFileDir(Value))) = lstShortPathname) then
      FstShortFileName := ExtractFileName(Value)
    else
      FstShortFileName := Value
  end else
    FstShortFileName := 'Untitled';

  Caption := Format(ResStr_RptingSnapshotTool, [FstShortFileName]);
  if Owner <> nil then
    TfrmFilterResult(Owner).SnapshotFile := FstSnapshotFile;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.SaveSnapshot;
begin
  if (FstSnapshotFile <>'') and (FstShortFileName <> 'Untitled') then
    SaveSnapshot(FstSnapshotFile)
  else
    SaveSnapshotAs;
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.SaveSnapshotAs;
begin
  if FileExists(SnapshotFile) then
    dlgSave.FileName := SnapshotFile
  else
  begin
    dlgSave.InitialDir := AppSettings.SnapshotPath;
    dlgSave.FileName := 'Snapshot.snp';
  end;
  if dlgSave.Execute then
    SaveSnapshot(dlgSave.FileName);
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.OpenSnapshot;
begin
  try
    dlgOpen.InitialDir := AppSettings.SnapshotPath;
    if dlgOpen.Execute then OpenSnapshot(dlgOpen.FileName);
  except
    on TSnapshotException do //nothing
  end;
end;

{-------------------------------------------------------------------------------
}
function TfrmSnapshot.IsCharType(AValue: String): Boolean;
begin
  AValue := LowerCase(AValue);
  Result := (AValue = 'char') or
            (AValue = 'nchar') or
            (AValue = 'varchar') or
            (AValue = 'nvarchar');
end;

{-------------------------------------------------------------------------------
}
procedure TfrmSnapshot.ControlChanged(Sender: TObject);
begin
  inherited;
  FDirty := True;
end;

{-------------------------------------------------------------------------------
  Allow the user to save the snapshot if changes have been made to it.
}
procedure TfrmSnapshot.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  lMessage: string;
begin
  inherited;
  if Dirty then begin
    lMessage :=  ResStr_SaveSnapshotChanges;
    if FstSnapshotFile<>'' then
      lMessage := lMessage + #13#10 + ResStr_SnapshotAffectReport;
    case MessageDlg(lMessage, mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes:    SaveSnapshot;
      mrCancel: CanClose := False;
    end;
  end;
end;


end.
