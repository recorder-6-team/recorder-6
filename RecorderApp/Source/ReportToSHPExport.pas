//==============================================================================
//  Unit:        ReportToSHPExport
//
//  Implements:  TdlgReportToSHPExport
//
//  Description: Allows users to select which data from the report will be
//                included in the exported SHP file.
//
//  Author:      John Durman
//  Created:     13 May 2008
//
//  Last Revision Details:
//    $Revision: 9 $
//    $Date: 1/10/09 9:56 $
//    $Author: Simonlewis $
//
//  $History: ReportToSHPExport.pas $
//  
//  *****************  Version 9  *****************
//  User: Simonlewis   Date: 1/10/09    Time: 9:56
//  Updated in $/JNCC/Development/Build/Source
//  Changed the DoExport routine from a procedure to a function; it now
//  returns the file name of the export file, which is used in FilterResult
//  to save metadata.
//
//  *****************  Version 8  *****************
//  User: Pauldavies   Date: 19/02/09   Time: 14:15
//  Updated in $/JNCC/Development/Build/Source
//  Incident 17992
//  
//  Now re-adds the first point when creating a polygon, to ensure that the
//  shape is completed.
//  
//  Incident 18625
//  CCN 315
//  
//  Now passes the selected spatial reference system to the WriteSHPFiles
//  procedure, so that it can create a projection file.
//  
//  *****************  Version 7  *****************
//  User: Johnvanbreda Date: 19/06/08   Time: 15:01
//  Updated in $/JNCC/Development/Build/Source
//  17131
//  Fixed export message if some records omitted.
//  
//  *****************  Version 6  *****************
//  User: Johnvanbreda Date: 18/06/08   Time: 11:56
//  Updated in $/JNCC/Development/Build/Source
//  VI17131
//  fixed export of memo attributes
//  
//  *****************  Version 5  *****************
//  User: Qingsun      Date: 29/05/08   Time: 15:43
//  Updated in $/JNCC/Development/Build/Source
//  VI 17131
//  
//  *****************  Version 3  *****************
//  User: Johnvanbreda Date: 21/05/08   Time: 16:41
//  Updated in $/JNCC/Development/Build/Source
//  17131
//  Fixed handling of non-named spatial references
//  
//  *****************  Version 2  *****************
//  User: Johndurman   Date: 15/05/08   Time: 10:35
//  Updated in $/JNCC/Development/Build/Source
//  VI 17131 - CCN255 - Export to .shp to include attributes
//  
//  *****************  Version 1  *****************
//  User: Johndurman   Date: 15/05/08   Time: 9:49
//  Created in $/JNCC/Development/Build/Source
//  VI 17131 - CCN255 - Export to .shp to include attributes
//==============================================================================

unit ReportToSHPExport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, CheckLst, DBGrids, ApplicationSettings,
  xmldom, XMLIntf, msxmldom, XMLDoc, Grids, ImageListButton, ExceptionForm,
  VagueDate, DB, GISShape, DataFields;

resourcestring
  ResStr_SelectSpatialRef = 'Please select a spacial reference system';
  ResStr_NullSpatialRef='The spatial reference column in the report contains no data, so the export has been aborted.';
  ResStr_SomeNullSpatialRef='Some of the rows were ommitted from the export because the spatial reference column contains no data for the row.';
  ResStr_SpatialFieldsNotAvailable = 'There are no spatial reference fields in the dataset to export';
  ResStr_CreatingKmlFile = 'Creating SHP file...';
  ResStr_Squares  = 'Squares';
  ResStr_Points   = 'Points';

type
  EReportToSHP = class(TExceptionPath);

  TdlgReportToSHPExport = class(TForm)
    cmbReferenceSystem: TComboBox;
    lbAttributeFields: TCheckListBox;
    btnOK: TImageListButton;
    btnCancel: TImageListButton;
    Label1: TLabel;
    Label2: TLabel;
    dlgSaveSHP: TSaveDialog;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FColumns: TStringList;
    FFields: TStringList;
    procedure PopulateStandardSpatialReferences;
    procedure PopulateComAddInSpatialReferences;
    procedure AddFieldToShapeList(AShapeList: TSVOShapeList; const AFieldName:
        string; const AColumnType: TColumnTypes; ASize: Word=0);
    function ConvertFieldTypeToSVOColumnType(const AFieldType: TFieldType): TColumnTypes;
    function CanExport(DataSet: TDataSet):boolean;
    procedure AddSelectedFieldsToShapeLists(squaresList, pointsList: TSVOShapeList;
      DataSet: TDataSet; FieldNames: TStringList);
    procedure ConvertRecordsToShapeObjects(squaresList, pointsList: TSVOShapeList;
      DataSet: TDataSet; FieldNames: TStringList);
    function CreateSVOPointShape(ShapeList: TSVOShapeList;
      const SpatialReference, IncomingSRSystem, RequiredSRSystem: string): TSVOPointShape;
    function CreateSVOPolygonShape(ShapeList: TSVOShapeList;
      const SpatialReference, IncomingSRSystem, RequiredSRSystem: string): TSVOPolygonShape;
    procedure AddAttributesToShapeObject(svoShapeObject: TSVOShapeObject;
      DataSet: TDataSet; FieldNames: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Populate(ADBGrid: TDBGrid);
    function DoExport(AGrid: TDBGrid): String;
  end;

implementation

uses FormActions, JNCCDatasets, MainTBar, SpatialRefFuncs, Options,
  Recorder2000_TLB, DynamicArrayUnit, GISReadWrite, StrUtils, GeneralData,
  GeneralFunctions;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Constructor
}
constructor TdlgReportToSHPExport.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TStringList.Create;
  FFields := TStringList.Create;
end;    // TdlgReportToSHPExport.Create

{-------------------------------------------------------------------------------
  Destructor
}
destructor TdlgReportToSHPExport.Destroy;
var
  i: integer;
begin
  for i := 0 to Pred(cmbReferenceSystem.Items.Count) do
    cmbReferenceSystem.Items.Objects[i].Free;
  FColumns.Free;
  FFields.Free;
  inherited;
end;    // TdlgReportToSHPExport.Destroy

{-------------------------------------------------------------------------------
  Populate the combobox, checklistbox, and hidden stringlists
}
procedure TdlgReportToSHPExport.Populate(ADBGrid: TDBGrid);
var
  i: Integer;
begin
  with ADBGrid do begin
    for i := 0 to Columns.Count - 1 do begin
      FColumns.Add(Columns[i].Title.Caption);
      FFields.Add(Columns[i].FieldName);
      if Columns[i].Visible then begin
        lbAttributeFields.Items.AddObject(
            Columns[i].Title.Caption, Columns[i].Field);
      end;
    end;
  end;

  PopulateStandardSpatialReferences;
  PopulateComAddInSpatialReferences;
end;    // TdlgReportToSHPExport.Populate

{-------------------------------------------------------------------------------
  Populate the dropdown list with the standard four Recorder spatial reference systems.
}
procedure TdlgReportToSHPExport.PopulateStandardSpatialReferences;
var
  lSpatialSystem: TSpatialSystemName;
  NewIndex: integer;
begin
  // OS_GB
  lSpatialSystem      := TSpatialSystemName.Create;
  lSpatialSystem.Name := OS_GB;
  NewIndex := cmbReferenceSystem.Items.AddObject(ResStr_GBTitle, lSpatialSystem);
  if (lSpatialSystem.Name = AppSettings.SpatialRefSystem) then
    cmbReferenceSystem.ItemIndex := NewIndex;

  // OS_NI
  lSpatialSystem      := TSpatialSystemName.Create;
  lSpatialSystem.Name := OS_NI;
  NewIndex := cmbReferenceSystem.Items.AddObject(ResStr_NITitle, lSpatialSystem);
  if (lSpatialSystem.Name = AppSettings.SpatialRefSystem) then
    cmbReferenceSystem.ItemIndex := NewIndex;

  // LAT_LONG
  lSpatialSystem      := TSpatialSystemName.Create;
  lSpatialSystem.Name := LAT_LONG;
  NewIndex := cmbReferenceSystem.Items.AddObject(ResStr_LatLongTitle, lSpatialSystem);
  if (lSpatialSystem.Name = AppSettings.SpatialRefSystem) then
    cmbReferenceSystem.ItemIndex := NewIndex;

  // UTM
  lSpatialSystem      := TSpatialSystemName.Create;
  lSpatialSystem.Name := UTM;
  NewIndex := cmbReferenceSystem.Items.AddObject(ResStr_UTMTitle, lSpatialSystem);
  if (lSpatialSystem.Name = AppSettings.SpatialRefSystem) then
    cmbReferenceSystem.ItemIndex := NewIndex;
end;    // TdlgReportToSHPExport.PopulateStandardSpatialReferences

{-------------------------------------------------------------------------------
  Populate the dropdown list with any COM add-in spatial reference systems.
}
procedure TdlgReportToSHPExport.PopulateComAddInSpatialReferences;
var
  i: Integer;
  lSpatialSystem : TSpatialSystemName;
  lBaseMapFormat: IBaseMapFormat;
  lSpatialRefAddin: ISpatialReference;
  NewIndex: integer;
begin
  for i := 0 to AppSettings.ComAddins.SpatialSystemInterfaces.Count - 1 do begin
    if Supports(AppSettings.ComAddins.SpatialSystemInterfaces[i],
                  IBaseMapFormat, lBaseMapFormat) then
    begin
      lSpatialSystem := TSpatialSystemName.Create;
      lSpatialRefAddin := AppSettings.ComAddins.SpatialSystemInterfaces[i]
                           as ISpatialReference;
      lSpatialSystem.Name := lSpatialRefAddin.SpatialRefSystem;
      if Supports(lSpatialRefAddin, INamedSpatialReference) then
        NewIndex := cmbReferenceSystem.Items.AddObject(
            (lSpatialRefAddin as INamedSpatialReference).SpatialSystemName, lSpatialSystem)
      else
        NewIndex := cmbReferenceSystem.Items.AddObject(
            lSpatialRefAddin.SpatialRefSystem, lSpatialSystem);
      if (lSpatialSystem.Name = AppSettings.SpatialRefSystem) then
        cmbReferenceSystem.ItemIndex := NewIndex;
    end;
  end;
end;    // TdlgReportToSHPExport.PopulateComAddInSpatialReferences

{-------------------------------------------------------------------------------
  Show save dialog. Initial directory and filename are standard export names
}
procedure TdlgReportToSHPExport.btnOKClick(Sender: TObject);
var
  ltfSave: Boolean;
begin
  if cmbReferenceSystem.Text = '' then
    ShowInformation(ResStr_SelectSpatialRef)
  else begin
    with dlgSaveSHP do begin
      InitialDir := AppSettings.ReportPath + 'Output\';
      Filter := 'SHP Files | *.shp';
      FileName := AppSettings.ReportPath + 'Output\Export.shp';
      ltfSave := Execute;
    end;
    if ltfSave then begin
      // Ensure folders exist.
      if ExtractFilePath(dlgSaveSHP.FileName) <> '' then
        ForceDirectories(ExtractFilePath(dlgSaveSHP.FileName));
      // Ensure extension is SHP.
      if not SameText(ExtractFileExt(dlgSaveSHP.FileName), '.shp') then
        dlgSaveSHP.FileName := dlgSaveSHP.FileName + '.shp';
      ModalResult := mrOK;
    end;
  end;
end;    // TdlgReportToSHPExport.btnOKClick

{-------------------------------------------------------------------------------
}
procedure TdlgReportToSHPExport.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;    // TdlgReportToSHPExport.btnCancelClick

{-------------------------------------------------------------------------------
   // method
 }
function TdlgReportToSHPExport.CanExport(DataSet: TDataSet): boolean;
var
  i,j:Integer;
  SpatialReferenceField, SpatialReferenceSystemField: string;
  IncomingSpatialReferenceSystem,  SpatialReference:string;
begin
  SpatialReferenceField := gAssistor.GetSpatialFieldName(DataSet, srfDefault);
  SpatialReferenceSystemField := gAssistor.GetSpatialFieldName(DataSet, srfSystem);
  Result:=true;
  j:=0;
  DataSet.First;
  for i := 0 to DataSet.RecordCount - 1 do begin
    SpatialReference:=  DataSet.FieldByName(SpatialReferenceField).AsString;
    IncomingSpatialReferenceSystem := Trim(DataSet.FieldByName(
        SpatialReferenceSystemField).AsString);
    if( (IncomingSpatialReferenceSystem = '') or (SpatialReference= '')) then
      inc(j) ;

  DataSet.Next;
 end;
   if j > 0 then begin
    if(j= DataSet.RecordCount) then begin
      Result:=false;
      MessageDlg(ResStr_NullSpatialRef, mtWarning, [mbOK], 0);
    end else  begin
      Result:=true;
      MessageDlg(ResStr_SomeNullSpatialRef, mtInformation, [mbOK], 0);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Write the data to a GIS shape file. Gets the full path of the exported file if
  saved successfully, or an empty string otherwise.
}
function TdlgReportToSHPExport.DoExport(AGrid: TDBGrid): String;
var
  squaresList, pointsList: TSVOShapeList;
  FieldNames: TStringList;
  lSuccess: boolean;
begin
  Result := '';
  if not gAssistor.HasSpatialFields(AGrid.DataSource.DataSet) then
    raise EReportToSHP.Create(ResStr_SpatialFieldsNotAvailable);

  squaresList := TSVOShapeList.Create;
  pointsList  := TSVOShapeList.Create;
  try
    AddFieldToShapeList(squaresList, 'ID', ctInteger);
    AddFieldToShapeList(pointsList, 'ID', ctInteger);
    FieldNames := TStringList.Create;
    lSuccess:= CanExport(AGrid.DataSource.DataSet);
    if(lSuccess) then begin
      try
        AddSelectedFieldsToShapeLists(squaresList, pointsList,
            AGrid.DataSource.DataSet, FieldNames);


        ConvertRecordsToShapeObjects(squaresList, pointsList,
            AGrid.DataSource.DataSet, FieldNames);
        dmGeneralData.WriteSHPFiles(squaresList, pointsList,
            dlgSaveSHP.FileName, ResStr_Squares, ResStr_Points,
            TSpatialSystemName(
            cmbReferenceSystem.Items.Objects[cmbReferenceSystem.ItemIndex]).Name);
        Result := dlgSaveSHP.FileName;
      finally
        FieldNames.Free;
        frmMain.TaskFinished;
      end;
    end;
  finally
    squaresList.Free;
    pointsList.Free;
  end;
end;    // TdlgReportToSHPExport.DoExport

{-------------------------------------------------------------------------------
  Add a field to a shape list
}
procedure TdlgReportToSHPExport.AddFieldToShapeList(AShapeList: TSVOShapeList;
    const AFieldName: string; const AColumnType: TColumnTypes; ASize: Word=0);
begin
  with AShapeList.DataFields.AddField do begin
    FieldName := AFieldName;
    FieldType := AColumnType;
    if ASize<>0 then
      Size := ASize;
  end;    // with ShapeList.DataFields.AddField
end;    // TdlgReportToSHPExport.AddFieldToShapeList

{-------------------------------------------------------------------------------
  Converts a Delphi TFieldType enum value to an SVO TColumnTypes enum, if possible
}
function TdlgReportToSHPExport.ConvertFieldTypeToSVOColumnType(
  const AFieldType: TFieldType): TColumnTypes;
begin
  case AFieldType of
    ftUnknown:    Result := DataFields.ctUnknown;
    ftString:     Result := ctString;
    ftSmallint:   Result := ctSmallint;
    ftInteger:    Result := ctInteger;
    ftWord:       Result := ctWord;
    ftBoolean:    Result := ctBoolean;
    ftFloat:      Result := ctFloat;
    ftCurrency:   Result := ctCurrency;
    ftBCD:        Result := ctBCD;
    ftDate:       Result := ctDate;
    ftTime:       Result := ctTime;
    ftDateTime:   Result := ctDateTime;
    ftBytes:      Result := ctBytes;
    ftVarBytes:   Result := ctVarBytes;
    ftAutoInc:    Result := ctAutoInc;
    ftBlob:       Result := ctBlob;
    ftMemo:       Result := ctString; // note that SVO export cannot export a memo!
    ftGraphic:    Result := ctGraphic;
    ftFmtMemo:    Result := ctFmtMemo;
    ftParadoxOle: Result := ctParadoxOle;
    ftDBaseOle:   Result := ctDBaseOle;
    ftTypedBinary:Result := ctTypedBinary;
    ftCursor:     Result := ctCursor;
  else
    Result := DataFields.ctUnknown;
  end;
end;    // TdlgReportToSHPExport.ConvertFieldTypeToSVOColumnType

{-------------------------------------------------------------------------------
  Adds each selected field to the shape list
}
procedure TdlgReportToSHPExport.AddSelectedFieldsToShapeLists(
  squaresList, pointsList: TSVOShapeList; DataSet: TDataSet;
  FieldNames: TStringList);
var
  i: Integer;
  SelectedFieldName: string;
  DelphiFieldType: TFieldType;
  lSize: integer;
begin
  for i := 0 to Pred(lbAttributeFields.Items.Count) do begin
    if lbAttributeFields.Checked[i] then begin
      SelectedFieldName := TField(lbAttributeFields.Items.Objects[i]).FieldName;
      FieldNames.Add(SelectedFieldName);
      DelphiFieldType := DataSet.FieldDefList.FieldByName(
          SelectedFieldName).DataType;
      if DelphiFieldType = ftString then
        lSize := TField(lbAttributeFields.Items.Objects[i]).Size
      else if DelphiFieldType = ftMemo then
        lSize := 255 // can't export a memo as there is a bug in SVO export, so export 255 chars instead
      else
        lSize := 0; // other field types don't need to set the size
      AddFieldToShapeList(squaresList, SelectedFieldName,
          ConvertFieldTypeToSVOColumnType(DelphiFieldType), lSize);
      AddFieldToShapeList(pointsList, SelectedFieldName,
          ConvertFieldTypeToSVOColumnType(DelphiFieldType), lSize);
    end;    // if lbAttributeFields.Checked[i]
  end;    // for i := 0 to Pred(lbAttributeFields.Items.Count)
end;    // TdlgReportToSHPExport.AddSelectedFieldsToShapeList



{-------------------------------------------------------------------------------
  Steps through the records, adding each one to the shape list
}
procedure TdlgReportToSHPExport.ConvertRecordsToShapeObjects(
  squaresList, pointsList: TSVOShapeList; DataSet: TDataSet; FieldNames: TStringList);
var
  i: Integer;
  svoShapeObject: TSVOShapeObject;
  SpatialReferenceField, SpatialReferenceSystemField: string;
  IncomingSpatialReferenceSystem, RequiredSpatialReferenceSystem,SpatialReference: string;
begin
  RequiredSpatialReferenceSystem := TSpatialSystemName(
      cmbReferenceSystem.Items.Objects[cmbReferenceSystem.ItemIndex]).Name;
  SpatialReferenceField := gAssistor.GetSpatialFieldName(DataSet, srfDefault);
  SpatialReferenceSystemField := gAssistor.GetSpatialFieldName(DataSet, srfSystem);
  // Make sure we start at the top!
  DataSet.First;
  for i := 0 to DataSet.RecordCount - 1 do begin
    frmMain.SetProgress(i*100 div DataSet.RecordCount);
    SpatialReference:=    DataSet.FieldByName(SpatialReferenceField).AsString;
    IncomingSpatialReferenceSystem := Trim(DataSet.FieldByName(
        SpatialReferenceSystemField).AsString);
    if  ((SpatialReference<>'') and (IncomingSpatialReferenceSystem<>'')) then  begin
      if (IncomingSpatialReferenceSystem = OS_GB)
            or (IncomingSpatialReferenceSystem = OS_NI) then
          svoShapeObject := CreateSVOPolygonShape(squaresList,
              SpatialReference,
              IncomingSpatialReferenceSystem,
              RequiredSpatialReferenceSystem)
      else
          svoShapeObject := CreateSVOPointShape(pointsList,
              SpatialReference,
              IncomingSpatialReferenceSystem,
              RequiredSpatialReferenceSystem);

      svoShapeObject.FieldByName('ID').AsInteger := i;
      AddAttributesToShapeObject(svoShapeObject, DataSet, FieldNames);
    end;
    DataSet.Next;
  end;
end;    // TdlgReportToSHPExport.ConvertRecordsToShapeObjects

{-------------------------------------------------------------------------------
  Creates a TSVOPointShape, adding it to the list of shapes
}
function TdlgReportToSHPExport.CreateSVOPointShape(ShapeList: TSVOShapeList;
  const SpatialReference, IncomingSRSystem, RequiredSRSystem: string): TSVOPointShape;
var
  LatLong: TLatLong;
begin
  LatLong := ConvertToLatLong(SpatialReference, IncomingSRSystem);
  Result := TSVOPointShape.Create(ShapeList);
  Result.SetShape(TDoublePoint(LatLongToSpecificEN(LatLong, RequiredSRSystem)));
end;    // TdlgReportToSHPExport.CreateSVOPointShape

{-------------------------------------------------------------------------------
  Creates a TSVOPolygonShape, adding it to the list of shapes
}
function TdlgReportToSHPExport.CreateSVOPolygonShape(ShapeList: TSVOShapeList;
  const SpatialReference, IncomingSRSystem, RequiredSRSystem: string): TSVOPolygonShape;
var
  svoSquare: TSVOShapePointArray;
  GridSquare: TGridSquare;
begin
  GridSquare := SpatialRefToGridSquare(SpatialReference, IncomingSRSystem, -1);
  svoSquare := TSVOShapePointArray.Create(0);
  svoSquare.AddPoint(TDoublePoint(LatLongToSpecificEN(GridSquare.BottomLeft, RequiredSRSystem)));
  svoSquare.AddPoint(TDoublePoint(LatLongToSpecificEN(GridSquare.TopLeft, RequiredSRSystem)));
  svoSquare.AddPoint(TDoublePoint(LatLongToSpecificEN(GridSquare.TopRight, RequiredSRSystem)));
  svoSquare.AddPoint(TDoublePoint(LatLongToSpecificEN(GridSquare.BottomRight, RequiredSRSystem)));
  // VI 17992 Re-add the first point again to ensure that the shape is completed.
  svoSquare.AddPoint(TDoublePoint(LatLongToSpecificEN(GridSquare.BottomLeft, RequiredSRSystem)));
  Result := TSVOPolygonShape.Create(ShapeList, svoSquare);
end;    // TdlgReportToSHPExport.CreateSVOPolygonShape

{-------------------------------------------------------------------------------
  Adds each of the attributes listed in FieldNames to the TSVOShapeObject
}
procedure TdlgReportToSHPExport.AddAttributesToShapeObject(
  svoShapeObject: TSVOShapeObject; DataSet: TDataSet;
  FieldNames: TStringList);
var
  i: integer;
begin
  for i := 0 to Pred(FieldNames.Count) do begin
    svoShapeObject.FieldByName(FieldNames[i]).Value :=
        DataSet.FieldByName(FieldNames[i]).Value;
  end;    // for i := 0 to Pred(FieldNames.Count)
end;    // TdlgReportToSHPExport.AddAttributesToShapeObject



end.
