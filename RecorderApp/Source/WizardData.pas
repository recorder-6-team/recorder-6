//==============================================================================
//  Unit:        WizardData
//
//  Implements:  TdmWizard
//               TTableLinkInfo
//
//  Description: Contains data access components and functions, to slightly
//               alleviate the complexity of the Wizard unit.
//
//  Author:      Andrew Smith
//  Created:     7 June 1999
//
//  Changes:     Restored the dsResults datasource component, required by the
//               ReportDesigner unit and used by QuickReport. DO NOT REMOVE.
//
//               Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 52 $
//    $Date: 18/12/09 11:37 $
//    $Author: Simonlewis $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit WizardData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseData, HierarchyNodes, DataClasses, StdCtrls, JNCCDatasets, ADODB,
  DatabaseAccessADO, ReportGenerator, ReportWizardSettings;

type
  TTableLinkInfo = class(TObject)
  private
    FTableName: string;
    FLinkTable: string;
    FLinkSQL: string;
    FJoinOrder: integer;
    FRequired: boolean;
    FWhereSQL: string;
  public
    property TableName: string read FTableName write FTableName;
    property LinkTable: string read FLinkTable write FLinkTable;
    property LinkSQL: string read FLinkSQL write FLinkSQL;
    property WhereSQL: string read FWhereSQL write FWhereSQL;
    property Required: boolean read FRequired write FRequired;
    property JoinOrder: integer read FJoinOrder write FJoinOrder;
  end;

  //----------------------------------------------------------------------------
  TdmWizard = class(TDataModule)
    qrySurveyTypes: TJNCCQuery;
    qrySurveys: TJNCCQuery;
    qrySearch: TJNCCQuery;
    tblInfoFinder: TJNCCTable;
    qryFieldFinder: TJNCCQuery;
    qryUsableTables: TJNCCQuery;
    qryOccSamples: TJNCCQuery;
    qrySampleRec: TJNCCQuery;
    qryResults: TJNCCQuery;
    qryMapSheets: TJNCCQuery;
    qryMapObjects: TJNCCQuery;
    dsResults: TDataSource;
    connReport: TADOConnection;
    qryTaxonDesignationSets: TJNCCQuery;
    qryTaxonDesignationSetItems: TJNCCQuery;  // Required for ReportDesigner, DO NOT REMOVE
  private
    FTaxonTables  :TStringList;
    FBiotopeTables:TStringList;
    FPlaceTables  :TStringList;
    FReportGenerator       : TReportGenerator;
    { Private declarations }
    procedure PopulateTaxonTablesList;
    procedure PopulateBiotopeTablesList;
    procedure ClearList(AList:TStringList);
    procedure AddTables(AList:TStringList; const iApply:char);
    procedure PopulateDesignationSets (const items: TStrings);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareOutput(ASettings: TReportWizardSettings;
        AReportType : Char;
        ACriteria: TStrings);
    procedure PopulatePlaceTaxonTablesList;
    procedure PopulatePlaceBiotopeTablesList;
    procedure PopulateSurveyTypes(const ASurveyCombo: TComboBox);
    procedure PopulateTaxonDesignationSets(const ATaxDesSetCombo: TComboBox);
    procedure PopulateSurveys(const ASurveyCombo: TComboBox;
      const ASurveyList: TListBox);
    procedure PopulateTaxonDesignationSetItems(const ATaxDesSetCombo: TComboBox;
      const ATaxDesList: TListBox);
    property TaxonTables: TStringList read FTaxonTables write FTaxonTables;
    property BiotopeTables: TStringList read FBiotopeTables write FBiotopeTables;
    property PlaceTables: TStringList read FPlaceTables write FPlaceTables;
    property ReportGenerator : TReportGenerator read FReportGenerator;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralData, Maintbar, ApplicationSettings;

//==============================================================================
constructor TdmWizard.Create(AOwner: TComponent);
var
  liIdx: Integer;
  designationSets: TStrings;
begin
  inherited Create(AOwner);
  // Set database and session properties
  for liIdx := 0 to Self.ComponentCount - 1 do
    if Components[liIdx] is TCustomADODataset then
      dmDatabase.SetDatabaseLocal([TCustomADODataset(Components[liIdx])]);
  FTaxonTables  :=TStringList.Create;
  FBiotopeTables:=TStringList.Create;
  FPlaceTables  :=TStringList.Create;
  PopulateTaxonTablesList;
  PopulateBiotopeTablesList;
  // Setup connection to build temp table on
  dmDatabase.SetConnectionToMainDB(connReport);
  connReport.Open;
  dmDatabase.SetApplicationRole(connReport);
  designationSets := TStringList.Create;
  PopulateDesignationSets(designationSets);
  FReportGenerator := TReportGenerator.Create(connReport, designationSets);

  qrySurveys.Parameters.ParamByName('UserID').Value := AppSettings.UserID;
end;  // Create

//==============================================================================
destructor TdmWizard.Destroy;
begin
  // Taxon Tables
  ClearList(FTaxonTables);
  FTaxonTables.Free;
  FTaxonTables := nil;
  // Biotope Tables
  ClearList(FBiotopeTables);
  FBiotopeTables.Free;
  FBiotopeTables := nil;
  // Place Tables
  ClearList(FPlaceTables);
  FPlaceTables.Free;
  FPlaceTables := nil;
  connReport.Close;  // make sure it doesn't hang around in ADO connection pool
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TdmWizard.ClearList(AList:TStringList);
var lCount:integer;
begin
  with AList do begin
    for lCount:=0 to Count-1 do
      if Assigned(Objects[lCount]) then TTableLinkInfo(Objects[lCount]).Free;
    Clear;
  end;
end;  // ClearList

//==============================================================================
procedure TdmWizard.AddTables(AList:TStringList; const iApply:char);
var lNewLinkInfo:TTableLinkInfo;
begin
  with qryUsableTables do begin
    SQL[SQL.Count - 1] := '(''A'',''' + iApply + ''')';
    Open;
    while not Eof do begin
      lNewLinkInfo := TTableLinkInfo.Create;
      with lNewLinkInfo do begin
        Required := false;                  
        TableName := FieldByName('Table_Name').AsString;
        LinkTable := FieldByName('Link_Table').AsString;
        LinkSQL   := FieldByName('Link').AsString;
        WhereSQL  := FieldByName('Additional_Link').AsString;
        JoinOrder := FieldByName('Join_Order').AsInteger;
      end;
      AList.AddObject(UpperCase(lNewLinkInfo.TableName), lNewLinkInfo);
      Next;
    end;
    Close;
  end;
end;  // AddTables

//==============================================================================
procedure TdmWizard.PopulateTaxonTablesList;
begin
  ClearList(FTaxonTables);
  AddTables(FTaxonTables,'T');
end;  // PopulateTaxonTablesList

//==============================================================================
procedure TdmWizard.PopulateBiotopeTablesList;
begin
  ClearList(FBiotopeTables);
  AddTables(FBiotopeTables,'B');
end;  // PopulateBiotopeTablesList

//==============================================================================
procedure TdmWizard.PopulatePlaceTaxonTablesList;
begin
  ClearList(FPlaceTables);
  AddTables(FPlaceTables,'T');
end;  // PopulatePlaceTaxonTablesList

//==============================================================================
procedure TdmWizard.PopulatePlaceBiotopeTablesList;
begin
  ClearList(FPlaceTables);
  AddTables(FPlaceTables,'B');
end;  // PopulatePlaceBiotopeTablesList

//==============================================================================
procedure TdmWizard.PopulateSurveyTypes(const ASurveyCombo: TComboBox);
var lsList       : String;
    lNewKey      : TKeyData;
  //----------------------------------------------------------------------------
  procedure ClearList;
  var liCount: integer;
  begin
    with ASurveyCombo.Items do begin
      for liCount := 0 to Count-1 do
        TKeyData(Objects[liCount]).Free;
      Clear;
    end;
  end;  // ClearList
  //----------------------------------------------------------------------------
begin
  if Assigned(ASurveyCombo) then begin
    ClearList;
    with qrySurveyTypes do begin
      if not Active then Open;
      First;
      while not Eof do begin
        lNewKey := TKeyData.Create;
        lNewKey.ItemKey := FieldByName('KeyField').AsString; // Store list key
        lNewKey.ItemAdditional := 'LOCAL';
        lsList := Fieldbyname('DisplayField').AsString;
        ASurveyCombo.Items.AddObject(lsList, lNewKey);
        Next;
      end; //while
      Close;
      ASurveyCombo.ItemIndex := -1;
    end; //with
  end;
end;  // PopulateSurveyTypes  

//==============================================================================
procedure TdmWizard.PopulateTaxonDesignationSets(const ATaxDesSetCombo: TComboBox);
begin
  if Assigned(ATaxDesSetCombo) then begin
    PopulateDesignationSets(ATaxDesSetCombo.Items);
    ATaxDesSetCombo.ItemIndex := -1;
  end;
end;  // PopulateTaxonDesignationSets

{-------------------------------------------------------------------------------
  Fills a list of strings with a list of designation sets found from the database.
}
procedure TdmWizard.PopulateDesignationSets (const items: TStrings);
var lsList       : String;
    lNewKey      : TKeyData;
  //----------------------------------------------------------------------------
  procedure ClearList;
  var liCount: integer;
  begin
    with items do begin
      for liCount := 0 to Count-1 do
        TKeyData(Objects[liCount]).Free;
      Clear;
    end;
  end;  // ClearList
  //----------------------------------------------------------------------------
begin
  ClearList;
  with qryTaxonDesignationSets do begin
    if not Active then Open;
    First;
    while not Eof do begin
      lNewKey := TKeyData.Create;
      lNewKey.ItemKey := FieldByName('KeyField').AsString; // Store list key
      lNewKey.ItemAdditional := 'LOCAL';
      lsList := Fieldbyname('DisplayField').AsString;
      items.AddObject(lsList, lNewKey);
      Next;
    end; //while
    Close;
  end; //with
end;  // PopulateDesignationSets

//------------------------------------------------------------------------------
procedure TdmWizard.PopulateSurveys(const ASurveyCombo: TComboBox;
  const ASurveyList: TListBox);
var lListKeyData: TKeyData;
    lsItem      : String;
    loSurveyType: TSurveyNode;
  //----------------------------------------------------------------------------
  procedure ClearList;
  var liCount: integer;
  begin
    with ASurveyList.Items do begin
      for liCount := 0 to Count-1 do
        TSurveyNode(Objects[liCount]).Free;
      Clear;
    end;
  end;  // ClearList
  //----------------------------------------------------------------------------
begin
  ClearList; 
  if ASurveyCombo.ItemIndex >= 0 then begin
    lListKeyData := TKeyData(ASurveyCombo.Items.Objects[ASurveyCombo.ItemIndex]);
    with qrySurveys do begin
      if Active then Close;
      dmDatabase.SetDatabaseLocal([qrySurveys]);
      Parameters.ParamByName('SelectedListKey').Value:= lListKeyData.ItemKey;
      try
        Open;
        First;
        //Call type specific function;
        while not Eof do begin
          lsItem := Fieldbyname('DisplayField').AsString;
          loSurveyType := TSurveyNode.Create;
          loSurveyType.Text              := Fieldbyname('DisplayField').AsString;
          loSurveyType.ItemKey           := Fieldbyname('ListKey').AsString;
          loSurveyType.Selected          := false;
          loSurveyType.ChildrenPopulated := false;
          loSurveyType.ParentKey         := lListKeyData.ItemKey;

          ASurveyList.ItemIndex := ASurveyList.Items.AddObject(lsItem, loSurveyType);
          Next;
        end; // while
      finally
        Close;
      end;
    end;
  end;
end;  // PopulateSurveys

//------------------------------------------------------------------------------
procedure TdmWizard.PopulateTaxonDesignationSetItems(const ATaxDesSetCombo: TComboBox;
  const ATaxDesList: TListBox);
var lListKeyData: TKeyData;
    lsItem      : String;
    loSurveyType: TSurveyNode;
  //----------------------------------------------------------------------------
  procedure ClearList;
  var liCount: integer;
  begin
    with ATaxDesList.Items do begin
      for liCount := 0 to Count-1 do
        TSurveyNode(Objects[liCount]).Free;
      Clear;
    end;
  end;  // ClearList
  //----------------------------------------------------------------------------
begin
  ClearList; 
  if ATaxDesSetCombo.ItemIndex >= 0 then begin
    lListKeyData := TKeyData(ATaxDesSetCombo.Items.Objects[ATaxDesSetCombo.ItemIndex]);
    with qryTaxonDesignationSetItems do begin
      if Active then Close;
      dmDatabase.SetDatabaseLocal([qryTaxonDesignationSetItems]);
      Parameters.ParamByName('SelectedSetKey').Value:= lListKeyData.ItemKey;
      try
        Open;
        First;
        //Call type specific function;
        while not Eof do begin
          lsItem := Fieldbyname('DisplayField').AsString;
          loSurveyType := TSurveyNode.Create;
          loSurveyType.Text              := Fieldbyname('DisplayField').AsString;
          loSurveyType.ItemKey           := Fieldbyname('ListKey').AsString;
          loSurveyType.Selected          := false;
          loSurveyType.ChildrenPopulated := false;
          loSurveyType.ParentKey         := lListKeyData.ItemKey;

          ATaxDesList.ItemIndex := ATaxDesList.Items.AddObject(lsItem, loSurveyType);
          Next;
        end; // while
      finally
        Close;
      end;
    end;
  end;
end;  // PopulateSurveys


//==============================================================================
{ Description : Prepare the template report output table, put the occurrences
    we have found into it, and ask the report generator to populate the
    attributes
  Created : 5/12/2002 }
procedure TdmWizard.PrepareOutput(ASettings: TReportWizardSettings;
  AReportType : Char; ACriteria: TStrings);
var
  lstSortFields : string;
  liAttrIndex, liColIndex : integer;
const
  SQL_READ_REPORT = 'Select %s from %s';
    procedure FindNextDisplayCol(var ColIndex : integer);
    begin
      repeat
        Inc(ColIndex);
        if ColIndex < qryResults.FieldCount then begin
          if qryResults.Fields[ColIndex].Visible then
            break; // from repeat
        end;
      until ColIndex >= qryResults.FieldCount;
    end;
begin
  FReportGenerator.PrepareTable(AReportType, ASettings);
  qryResults.Connection := connReport;
  FReportGenerator.PrepareData;
  qryResults.SQL.Text := Format(SQL_READ_REPORT, [FReportGenerator.SelectAttributes, FReportGenerator.TableName]);
  if Assigned(ACriteria) then
    qryResults.SQL.AddStrings(ACriteria);
  lstSortFields := FReportGenerator.SortAttributes;
  if lstSortFields <> '' then
    qryResults.SQL.Add('ORDER BY '+lstSortFields);
  qryResults.Open;
  frmMain.SetProgress(100);
  // Set column display titles - walk through attributes alongside columns
  liColIndex := 5;
  FindNextDisplayCol(liColIndex);
  for liAttrIndex := 0 to FReportGenerator.Attributes.Count-1 do
    if TAttribute(FReportGenerator.Attributes.Objects[liAttrIndex]).Selected then begin
      qryResults.Fields[liColIndex].DisplayLabel :=
                          TAttribute(FReportGenerator.Attributes.Objects[liAttrIndex]).Name;
      FindNextDisplayCol(liColIndex);
    end;
end;

end.
