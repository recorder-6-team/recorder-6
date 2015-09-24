//==============================================================================
//  Unit:        NameData
//
//  Implements:  TdmName
//               TAddressItem
//               TAddressList
//               TAssocItem
//               TAssocList
//               TCommsItem                  
//               TCommsList
//               TContactItem
//               TContactList
//
//  Description: Implements data access functionality for the Individual and
//               Organisation screen.
//
//               TAddressItem and TAddressList
//               Helper classes used on the Address tab of the input screen.
//
//               TAssocItem and TAssocList
//               Helper classes used on the Associations tab of the input screen.
//
//               TCommsItem and TCommsList
//               Helper classes used on the Communications tab of the input screen.
//
//               TContactItem and TContactList
//               Helper classes used on the Contacts tab of the input screen.
//
//  Author:      Paul Thomas
//  Created:     26 April 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 62 $
//    $Date: 30/01/08 11:04 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit NameData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, ADODB, DatabaseAccessADO, JNCCDatasets, BaseData, DataClasses, Grids,
  GeneralData, ExceptionForm, Constants, GeneralFunctions;

type
  ENameError = class(TExceptionPath);

  //Data module for IndOrg screen and other name related forms
  TdmName = class(TBaseDataModule)
    qryPopulate         : TJNCCQuery;
    qryOrganisation     : TJNCCQuery;
    dsIndividual        : TDataSource;
    dsOrganisation      : TDataSource;
    qryPrefAddress      : TJNCCQuery;
    qryPrefContact      : TJNCCQuery;
    dsOrganisationType  : TDataSource;
    qryAddresses        : TJNCCQuery;
    qryContacts         : TJNCCQuery;
    qryComms            : TJNCCQuery;
    qryAssocs           : TJNCCQuery;
    qryIndividual       : TJNCCQuery;
    qryAddDelete        : TJNCCQuery;
    qryOrganisationType : TJNCCQuery;
    qryDepartments: TJNCCQuery;
  private
    { Private declarations }
    FLastTableChecked : string;
  public
    { Public declarations }
    function CreateChildSQL(const iType: Integer; const iSort: String;
      const iFilter: String = ''): TStringList;
    function CreateChildSQLForValidation(parentType: Integer; const parentKey: TKeyString): String;
    function CreateTopLevelSQL(const iType: Integer; const iSQLWhere, iSort: String): TStringList;
    procedure AddName(const AKey: TKeyString; const Organisation: Boolean);
    procedure AddRelation(const Key1, Key2: TKeyString);
    function HasFilteredChildNodes(const parentType: Integer; const parentKey: TKeyString;
      childKeys: String): Boolean;
    function DelIndividual(const AKey: TKeyString):boolean;
    function DelOrganisation(const AKey: TKeyString):boolean;
    function ReferencedInTable(const AKey, ATable: string; Fields: array of string): boolean;
  end;  // TdmName

  //============================================================================
  //TAddressItem - Single address item for addresses tab in IndOrg
  TAddressItem = class(TGridDataItem)
  private
    FAddress1    : string;
    FAddress2    : string;
    FAddress3    : string;
    FAddress4    : string;
    FCountry     : string;
    FPostcode    : string;
    FDateFrom    : string;
    FDateTo      : string;
    FComment     : TMemoryStream;
    FWorkAddress : Boolean;
    FPreferred   : Boolean;
    procedure SetAddress1(const Value: string);
    procedure SetAddress2(const Value: string);
    procedure SetAddress3(const Value: string);
    procedure SetAddress4(const Value: string);
    procedure SetCountry(const Value: string);
    procedure SetPostcode(const Value: string);
    procedure SetDateFrom(const Value: string);
    procedure SetDateTo(const Value: string);
    procedure SetPreferred(const Value: Boolean);
    procedure SetWorkAddress(const Value: Boolean);
  protected
    procedure InitFromRecord( iDataset : TDataset); override;
    procedure WriteToRecord( iDataset : TDataset); override;
    function GetCell(const iX: integer): string; override;
  public
    constructor CreateNew(AOwner: TCacheDataList); override;
    destructor Destroy; override;
    property Address1: string read FAddress1 write SetAddress1;
    property Address2: string read FAddress2 write SetAddress2;
    property Address3: string read FAddress3 write SetAddress3;
    property Address4: string read FAddress4 write SetAddress4;
    property Country: string read FCountry write SetCountry;
    property Postcode: string read FPostcode write SetPostcode;
    property DateFrom: string read FDateFrom write SetDateFrom;
    property DateTo: string read FDateTo write SetDateTo;
    property Comment: TMemoryStream read FComment;
    property WorkAddress: Boolean read FWorkAddress write SetWorkAddress;
    property Preferred: Boolean read FPreferred write SetPreferred;
  end;  // TAddressItem

  //----------------------------------------------------------------------------
  //TAddressList - List of addresses for addresses tab in IndOrg
  TAddressList = class(TGridDataList)
  private
    FKeyString : TKeyString;
    procedure SetKeyString(const Value: TKeyString);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    constructor Create(iDataset: TDataset; iKeyFieldName: string; iStringGrid: TStringGrid);
    property NameKey: TKeyString read FKeyString write SetKeyString;
  end;  // TAddressList

  //============================================================================
  //TContactItem - Single contact item for Contact Numbers tab in IndOrg
  TContactItem = class(TGridDataItem)
  private
    FPrefix      : string;
    FNumber      : string;
    FContactType : string;
    FConstraints : TMemoryStream;
    FPreferred   : Boolean;
    procedure SetPrefix(const Value: string);
    procedure SetNumber(const Value: string);
    procedure SetContactType(const Value: string);
    procedure SetPreferred(const Value: Boolean);
  protected
    procedure InitFromRecord(iDataset : TDataset); override;
    procedure WriteToRecord(iDataset : TDataset); override;
    function GetCell(const iX : integer ) : string; override;
  public
    constructor CreateNew(aOwner : TCacheDataList); override;
    destructor Destroy; override;
    property Prefix: string read FPrefix write SetPrefix;
    property Number: string read FNumber write SetNumber;
    property ContactType: string read FContactType write SetContactType;
    property Constraints: TMemoryStream read FConstraints;
    property Preferred: Boolean read FPreferred write SetPreferred;
  end;  // TContactItem

  //----------------------------------------------------------------------------
  //TContactList - Contacts list for Contact Numbers tab in IndOrg
  TContactList = class(TGridDataList)
  private
    FKeyString : TKeyString;
    procedure SetKeyString(const Value: TKeyString);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    constructor Create(iDataset: TDataset; iKeyFieldName: string; iStringGrid: TStringGrid);
    property NameKey: TKeyString read FKeyString write SetKeyString;
  end;  // TContactList

  //============================================================================
  //TCommsItem - Single comms item for Comms tab in IndOrg
  TCommsItem = class(TGridDataItem)
  private
    FWithID     : TKeyString;
    FWithString : string;
    FCommsType  : string;
    FDate       : string;
    FContent    : TMemoryStream;
    FFileRef    : string;
    procedure SetWithID(const Value: TKeyString);
    procedure SetWithString(const Value: string);
    procedure SetCommsType(const Value: string);
    procedure SetDate(const Value: string);
    procedure SetFileRef(const Value: string);
  protected
    procedure InitFromRecord(iDataset : TDataset); override;
    procedure WriteToRecord(iDataset : TDataset); override;
    function GetCell(const iX : integer ) : string; override;
  public
    constructor CreateNew(aOwner : TCacheDataList); override;
    destructor Destroy; override;
    property WithID: TKeyString read FWithID write SetWithID;
    property WithString: string read FWithString write SetWithString;
    property CommsType: string read FCommsType write SetCommsType;
    property Date: string read FDate write SetDate;
    property Content: TMemoryStream read FContent;
    property FileRef: string read FFileRef write SetFileRef;
  end;  // TCommsItem

  //----------------------------------------------------------------------------
  //TCommsList - Comms list for Comms tab in IndOrg
  TCommsList = class(TGridDataList)
  private
    FKeyString : TKeyString;
    procedure SetKeyString(const Value: TKeyString);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    constructor Create(iDataset: TDataset; iKeyFieldName: string; iStringGrid: TStringGrid);
    property NameKey: TKeyString read FKeyString write SetKeyString;
  end;  // TCommsList

  //============================================================================
  //TAssocItem - Single assoc item for Assoc tab in IndOrg
  TAssocItem = class(TGridDataItem)
  private
    FWithID     : TKeyString;
    FWithString : string;
    FRole       : string;
    FDateFrom   : string;
    FDateTo     : string;
    FNameCode   : string;
    FComment    : TMemoryStream;
    procedure SetWithID(const Value: TKeyString);
    procedure SetWithString(const Value: string);
    procedure SetRole(const Value: string);
    procedure SetDateFrom(const Value: string);
    procedure SetDateTo(const Value: string);
    procedure SetNameCode(const Value: string);
  protected
    procedure InitFromRecord(iDataset : TDataset); override;
    procedure WriteToRecord(iDataset : TDataset); override;
    function GetCell(const iX : integer ) : string; override;
  public
    constructor CreateNew(aOwner : TCacheDataList); override;
    destructor Destroy; override;
    property WithID: TKeyString read FWithID write SetWithID;
    property WithString: string read FWithString write SetWithString;
    property Role: string read FRole write SetRole;
    property DateFrom: string read FDateFrom write SetDateFrom;
    property DateTo: string read FDateTo write SetDateTo;
    property NameCode: string read FNameCode write SetNameCode;
    property Comment: TMemoryStream read FComment;
  end;  // TAssocItem

  //----------------------------------------------------------------------------
  //TAssocList - Assoc list for Assoc tab in IndOrg
  TAssocList = class(TGridDataList)
  private
    FKeyString : TKeyString;
    procedure SetKeyString(const Value: TKeyString);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    constructor Create(iDataset: TDataset; iKeyFieldName: string; iStringGrid: TStringGrid);
    property NameKey: TKeyString read FKeyString write SetKeyString;
  end;  // TAssocList

  //============================================================================

const
  IND_TYPE = 0;
  ORG_TYPE = 1;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings;

const
  SQL_INDIVIDUAL_NAME =
          ' CASE WHEN I.Forename IS NULL THEN ' +
          '  CASE WHEN I.Initials IS NULL THEN ' +
          '   CASE WHEN I.Title IS NULL THEN I.Surname ' +
          '   ELSE I.Title + '' '' + I.Surname END ' +
          '  ELSE I.Initials + '' '' + I.Surname END ' +
          ' ELSE I.Forename + '' '' + I.Surname END AS Full_Name, ' +
          'ISNULL(I.Forename, '''') + '';'' + ISNULL(I.Initials, '''') + '';'' + I.Surname AS Node_Text ';

  SQL_ORGANISATION_NAME =
          ' CASE WHEN O.Acronym IS NOT NULL THEN O.Acronym + '', '' + O.Full_Name ' +
          ' ELSE O.Full_Name END AS Full_Name, ' +
          'ISNULL(O.Acronym, '''') + '';'' + ISNULL(O.Full_Name, '''') AS Node_Text ';

  SQL_NAME_RELATION =
      'FROM Name_Relation NR '
      + 'JOIN Individual I ON I.Name_Key IN (NR.Name_Key_1, NR.Name_Key_2) '
      + 'JOIN Organisation O ON O.Name_Key IN (NR.Name_Key_1, NR.Name_Key_2)';

resourcestring
  ResStr_CannotRemoveIndividual = 'The Individual is referenced in table ''%s'' and ' +
                                  'cannot be removed.';

  ResStr_CannotRemoveOrganisation = 'The Organisation is referenced in table ''%s'' and ' +
                                    'cannot be removed.';

//==============================================================================
{ TdmName }
function TdmName.HasFilteredChildNodes(const parentType: Integer; const parentKey: TKeyString;
  childKeys: String): Boolean;
var
  tableName: String;
  rs: _Recordset;
begin
  Result := False;
  if parentType = IND_TYPE then
    tableName := TN_ORGANISATION
  else
    tableName := TN_INDIVIDUAL;

  rs := dmDatabase.ExecuteSQL(Format(
      'SELECT DISTINCT Name_Key_2 FROM %s JOIN Name_Relation ON Name_Key = Name_Key_1 %s',
      [tableName, childKeys]), True);

  while not rs.Eof do begin
    if SameText(rs.Fields['Name_Key_2'].Value, parentKey) then begin
      Result := True;
      Break;
    end;
    rs.MoveNext;
  end;
  rs.Close;
end;

//==============================================================================
function TdmName.CreateChildSQL(const iType: Integer; const iSort: String;
  const iFilter: String = ''): TStringList;
var
  lSQL : TStringList;
begin
  lSQL:= TStringList.Create;
  with lSQL do begin
    Clear;
    if iType = IND_TYPE then begin
      Add('SELECT DISTINCT O.Name_Key, O.System_Supplied_Data, '
          + SQL_ORGANISATION_NAME
          + StringReplace(iSort, 'ORDER BY', ', ', [rfIgnoreCase]));    // Because of DISTINCT
      Add(SQL_NAME_RELATION);
      if iFilter <> '' then
        Add(StringReplace(iFilter, 'Name_Key', 'O.Name_Key', [rfIgnoreCase]) + ' AND I.Name_Key = :Parent ')
      else
        Add('WHERE I.Name_Key = :Parent ');
      Add(iSort);
    end else begin
      Add('SELECT DISTINCT I.Name_Key, I.System_Supplied_Data, '
          + SQL_INDIVIDUAL_NAME
          + StringReplace(iSort, 'ORDER BY', ', ', [rfIgnoreCase]));  // Because of DISTINCT
      Add(SQL_NAME_RELATION);
      if iFilter <> '' then
        Add(StringReplace(iFilter, 'Name_Key', 'I.Name_Key', [rfIgnoreCase]) + ' AND O.Name_Key = :Parent ')
      else
        Add('WHERE O.Name_Key = :Parent ');
      Add(iSort);
    end;
  end;
  Result:= lSQL;
end;  // CreateChildSQL

{-------------------------------------------------------------------------------
  Returns the query to run to get the organisations under an individual, and vice versa.
}
function TdmName.CreateChildSQLForValidation(parentType: Integer; const parentKey: TKeyString): String;
begin
  if parentType = IND_TYPE then
    Result := Format(
        'SELECT DISTINCT ''%s'' AS TableName, O.Name_Key AS ItemKey '
        + SQL_NAME_RELATION
        + 'WHERE I.Name_Key = ''%s'' ',
        [TN_ORGANISATION, parentKey])
  else
    Result := Format(
        'SELECT DISTINCT ''%s'' AS TableName, I.Name_Key AS ItemKey '
        + SQL_NAME_RELATION
        + 'WHERE O.Name_Key = ''%s'' ',
        [TN_INDIVIDUAL, parentKey]);
end;

//==============================================================================
function TdmName.CreateTopLevelSQL(const iType: Integer; const iSQLWhere, iSort: String): TStringList;
var lSQL: TStringList;
begin
  lSQL := TStringList.Create;
  with lSQL do begin
    Clear;
    if iType = ORG_TYPE then begin
      Add('SELECT O.Name_Key, O.System_Supplied_Data, ' + SQL_ORGANISATION_NAME);
      Add('FROM Organisation AS O');
      Add(iSQLWhere);
      Add(iSort);
    end else begin
      Add('SELECT I.Name_Key, I.System_Supplied_Data, ' + SQL_INDIVIDUAL_NAME);
      Add('FROM Individual AS I');
      Add(iSQLWhere);
      Add(iSort);
    end;
  end;
  Result := lSQL;
end;  // CreateTopLevelSQL

//==============================================================================
procedure TdmName.AddName(const AKey: TKeyString; const Organisation: Boolean);
begin
  dmDatabase.ExecuteSQL(Format('INSERT INTO Name (Name_Key, Organisation, Entered_By) ' +
                               'VALUES(''%s'', %d, ''%s'')',
                               [AKey, Ord(Organisation), AppSettings.UserID]),
                        False, Format(ResStr_AddFail, ['NAME']));
end;  // AddName

//==============================================================================
procedure TdmName.AddRelation(const Key1, Key2: TKeyString);
begin
  dmDatabase.ExecuteSQL(Format(
      'INSERT INTO Name_Relation (Name_Relation_Key, Name_Key_1, Name_Key_2, Role, Entered_By) ' +
      'VALUES (''%s'', ''%s'', ''%s'', ''Unknown'', ''%s'')',
      [dmGeneralData.GetNextKey('NAME_RELATION','Name_Relation_Key'), Key1, Key2, AppSettings.UserID]),
      False, Format(ResStr_AddFail, ['NAME_RELATION']));
end;  // AddRelation

//==============================================================================
// Checks if keys for a field in a table exist or not.
function TdmName.ReferencedInTable(const AKey, ATable:string; Fields:array of string): boolean;
var
  lWhere : string;
  lIdx   : integer;
begin
  FLastTableChecked := ATable;
  lWhere := '';
  if High(Fields) = -1 then
    Result := false
  else begin
    lWhere := Fields[0] + '=''' + AKey + '''';
    for lIdx := 1 to High(Fields) do
      lWhere := lWhere + ' OR ' + Fields[lIdx] + '=''' + AKey + '''';

    with qryAddDelete do begin
      SQL.Text := 'SELECT TOP 1 1 FROM ' + ATable + ' WHERE ' + lWhere;
      Open;
      Result := not Eof;
      Close;
    end;
  end;
end;  // ReferencedInTable

//==============================================================================
function TdmName.DelIndividual(const AKey: TKeyString):Boolean;
const
  // SQL to retrieve all relationships with the name table that aren't supported
  // by proper referential integrity (because Access couldn't cope with the
  // number of relationships!)
  SQL_ALL_NAME_SOFT_RELATIONSHIPS =
    'SELECT DISTINCT OBJECT_NAME(S.ID) AS TableName, Name '+
    'FROM SysColumns S '+
    'INNER JOIN INFORMATION_SCHEMA.TABLE_PRIVILEGES TP '+
    '      ON TP.Table_Name=OBJECT_NAME(ID) '+
    '      AND TP.Grantee=''R2k_FullEdit'' '+
    'WHERE S.Name IN (''Entered_By'', ''Changed_by'', ''Checked_By'')';

    //----------------------------------------------------------------------------
    function CheckCrossReference:boolean;
    begin
      // Assume not all is fine
      Result:=false;
      with dmDatabase.ExecuteSQL(SQL_ALL_NAME_SOFT_RELATIONSHIPS, true) do begin
        while not EOF do begin
          if dmGeneralData.CheckKeyExists(Fields['TableName'].Value, Fields['Name'].Value, AKey) then
            Exit;
          MoveNext;
        end;
      end;
      // found no instances of the name, so exit
      Result := true;
    end;  // CheckCrossReference

//----------------------------------------------------------------------------
begin
  // Set return value to false here, and set it to true if all deletions have
  // been correctly performed.
  Result:=false;  // Ignore compilation hint on this line
  if CheckCrossReference then begin
    dmDatabase.Connection.BeginTrans;
    try
      with dmGeneralData do begin
        ExecuteSQL('DELETE FROM Address WHERE Name_Key=''' + AKey + '''',
                   ResStr_DelFail + ' - ADDRESS table');
        ExecuteSQL('DELETE FROM Communication WHERE Name_Key_1=''' + AKey + ''' OR Name_Key_2=''' + AKey + '''',
                   ResStr_DelFail + ' - COMMUNICATION table');
        ExecuteSQL('DELETE FROM Contact_Number WHERE Name_Key=''' + AKey + '''',
                   ResStr_DelFail + ' - CONTACT_NUMBER table');
        ExecuteSQL('DELETE FROM Name_Relation WHERE Name_Key_1=''' + AKey + ''' OR Name_Key_2=''' + AKey + '''',
                   ResStr_DelFail + ' - NAME_RELATION table');
        DelSources('Name_Sources', 'Name_Key', AKey);
        // Delete from Individual first
        ExecuteSQL('DELETE FROM Individual WHERE Name_Key = ''' + AKey + '''',
                   ResStr_DelFail + ' - INDIVIDUAL table');
        // Then Name
        ExecuteSQL('DELETE FROM Name WHERE Name_Key=''' + AKey + '''',
                   ResStr_DelFail + ' - NAME table');
      end;
    except
      begin
        dmDatabase.Connection.RollbackTrans;
        raise;
      end;
    end; // try
    dmDatabase.Connection.CommitTrans;
    // If we reached this bit, everything went well.
    Result:= True;
  end else
    MessageDlg(Format(ResStr_CannotRemoveIndividual, [FLastTableChecked]), mtInformation, [mbOk], 0);
  FLastTableChecked:='';
end;  // DelIndividual

//==============================================================================
function TdmName.DelOrganisation(const AKey: TKeyString):boolean;
//----------------------------------------------------------------------------
  function CheckCrossReference:boolean;
  begin
    // Assume not all is fine
    Result:=false;
    // If any of the following calls is true, exit with Result still set to false
    if ReferencedInTable(AKey,'Biotope_Occurrence',['Checked_By']) then Exit;
    if ReferencedInTable(AKey,'Location_Designation',['Authority']) then Exit;
    if ReferencedInTable(AKey,'Management_Aim',['Authority']) then Exit;
    if ReferencedInTable(AKey,'Survey',['Run_By']) then Exit;
    if ReferencedInTable(AKey,'Taxon_Occurrence',['Checked_By']) then Exit;
    if ReferencedInTable(AKey,'Tenure',['Owned_By']) then Exit;
    // If we reach this stage, all is fine and set the result to true
    Result:=true;
  end;  // CheckCrossReference
//----------------------------------------------------------------------------
begin
  // Set return value to false here, and set it to true if all deletions have
  // been correctly performed.
  Result:=false;  // Ignore compilation hint on this line
  if CheckCrossReference then begin
    with dmGeneralData do begin
      ExecuteSQL('DELETE FROM Address WHERE Name_Key=''' + AKey + '''',
                 ResStr_DelFail + ' - ADDRESS table');
      ExecuteSQL('DELETE FROM Communication WHERE Name_Key_1=''' + AKey + ''' OR Name_Key_2=''' + AKey + '''',
                 ResStr_DelFail + ' - COMMUNICATION table');
      ExecuteSQL('DELETE FROM Contact_Number WHERE Name_Key=''' + AKey + '''',
                 ResStr_DelFail + ' - CONTACT_NUMBER table');
      ExecuteSQL('DELETE FROM Name_Relation WHERE Name_Key_1=''' + AKey + ''' OR Name_Key_2=''' + AKey + '''',
                 ResStr_DelFail + ' - NAME_RELATION table');
      DelSources('Name_Sources', 'Name_Key', AKey);
      // Delete from Organisation first
      ExecuteSQL('DELETE FROM Organisation WHERE Name_Key=''' + AKey + '''',
                 ResStr_DelFail + ' - ORGANISATION table');
      // Then Name
      ExecuteSQL('DELETE FROM Name WHERE Name_Key=''' + AKey + '''',
                 ResStr_DelFail + ' - NAME table');
    end;
    // If we reached this bit, everything went well.
    Result:= True;
  end else
    MessageDlg(ResStr_CannotRemoveOrganisation, mtInformation, [mbOk], 0);
  FLastTableChecked:='';
end;  // DelOrganisation


//==============================================================================
{ TAddressItem }
//==============================================================================
function TAddressItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: if FPreferred then //Use + to signal check box
        Result:= '+'
      else
        Result:= '';
    1: begin
      Result:= FAddress1;  //Only include commas if item is not blank
      AddToCommaSeparatedList(Result, FAddress2);
      AddToCommaSeparatedList(Result, FAddress3);
      AddToCommaSeparatedList(Result, FAddress4);
    end;
    2: Result:= FPostcode;
  else
    Result := ResStr_NotImplemented;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TAddressItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FItemKey := FieldByName('ADDRESS_KEY').AsString;
      FAddress1:= FieldByName('ADDRESS_1').AsString;
      FAddress2:= FieldByName('ADDRESS_2').AsString;
      FAddress3:= FieldByName('ADDRESS_3').AsString;
      FAddress4:= FieldByName('ADDRESS_4').AsString;
      FCountry := FieldByName('ADDRESS_COUNTRY').AsString;
      FPostcode:= FieldByName('ADDRESS_POSTCODE').AsString;
      FDateFrom:= FieldByName('FROM_VAGUE_DATE_START').Text;
      FDateTo  := FieldByName('TO_VAGUE_DATE_START').Text;

      FComment := TMemoryStream.Create;
      TMemoField(FieldByName('COMMENT')).SaveToStream(FComment);

      FWorkAddress:= FieldByName('WORK_ADDRESS').AsBoolean;
      FPreferred  := FieldByName('PREFERRED').AsBoolean;
    except on E:Exception do
      raise ENameError.Create(ResStr_InitRecFail + ' - ADDRESS', E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TAddressItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('ADDRESS_1').AsString        := FAddress1;
      FieldByName('ADDRESS_2').AsString        := FAddress2;
      FieldByName('ADDRESS_3').AsString        := FAddress3;
      FieldByName('ADDRESS_4').AsString        := FAddress4;
      FieldByName('ADDRESS_COUNTRY').AsString  := FCountry;
      FieldByName('ADDRESS_POSTCODE').AsString := FPostcode;
      FieldByName('FROM_VAGUE_DATE_START').Text:= FDateFrom;
      FieldByName('TO_VAGUE_DATE_START').Text  := FDateTo;

      FComment.Position:= 0;
      TMemoField(FieldByName('COMMENT')).LoadFromStream(FComment);

      FieldByName('WORK_ADDRESS').AsBoolean := FWorkAddress;
      FieldByName('PREFERRED').AsBoolean    := FPreferred;
      FieldByName('CHANGED_BY').AsString    := AppSettings.UserID;
      FieldByName('CHANGED_DATE').AsDateTime:= Now;
    except on E:Exception do
      raise ENameError.Create(ResStr_WriteRecFail + ' - ADDRESS', E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TAddressItem.SetAddress1(const Value: string);
begin
  FAddress1 := Value;
  SetModified;
end;

procedure TAddressItem.SetAddress2(const Value: string);
begin
  FAddress2 := Value;
  SetModified;
end;

procedure TAddressItem.SetAddress3(const Value: string);
begin
  FAddress3 := Value;
end;

procedure TAddressItem.SetAddress4(const Value: string);
begin
  FAddress4 := Value;
  SetModified;
end;

procedure TAddressItem.SetCountry(const Value: string);
begin
  FCountry := Value;
  SetModified;
end;

procedure TAddressItem.SetPostcode(const Value: string);
begin
  FPostcode := Value;
  SetModified;
end;

procedure TAddressItem.SetDateFrom(const Value: string);
begin
  FDateFrom := Value;
  SetModified;
end;

procedure TAddressItem.SetDateTo(const Value: string);
begin
  FDateTo := Value;
  SetModified;
end;

procedure TAddressItem.SetWorkAddress(const Value: Boolean);
begin
  FWorkAddress := Value;
  SetModified;
end;

procedure TAddressItem.SetPreferred(const Value: Boolean);
begin
  FPreferred := Value;
  SetModified;
end;

constructor TAddressItem.CreateNew(aOwner: TCacheDataList);
begin
  inherited CreateNew(aOwner);
  FComment:= TMemoryStream.Create;
end;

destructor TAddressItem.Destroy;
begin
  FComment.Free;
  inherited;
end;

//==============================================================================
{ TAddressList }
constructor TAddressList.Create(iDataset: TDataset; iKeyFieldName: string; iStringGrid: TStringGrid);
begin
  inherited Create(iDataset, iKeyFieldName, iStringGrid, TAddressItem);
end;  // Create

//------------------------------------------------------------------------------
function TAddressList.ItemsDisplayName: String;
begin
  Result := ResStr_Address;
end;

//------------------------------------------------------------------------------
procedure TAddressList.SetKeyString(const Value: TKeyString);
begin
  FKeyString := Value;
end;  // SetKeyString

//------------------------------------------------------------------------------
procedure TAddressList.DoAdditions;
var lqryAddition: TJnccQuery;
    i           : integer;
    lCurrentItem: TAddressItem;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Address WHERE Address_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lCurrentItem:= ItemsToAdd.Items[i];
          Append;
          FieldByName('Address_Key').AsString      :=
              dmGeneralData.GetNextKey('Address', 'Address_Key');
          FieldByName('Address_1').AsString        := lCurrentItem.FAddress1;
          FieldByName('Address_2').AsString        := lCurrentItem.FAddress2;
          FieldByName('Address_3').AsString        := lCurrentItem.FAddress3;
          FieldByName('Address_4').AsString        := lCurrentItem.FAddress4;
          FieldByName('Address_Country').AsString  := lCurrentItem.FCountry;
          FieldByName('Address_Postcode').AsString := lCurrentItem.FPostcode;
          FieldByName('From_Vague_Date_Start').Text:= lCurrentItem.FDateFrom;
          FieldByName('To_Vague_Date_Start').Text  := lCurrentItem.FDateTo;

          lCurrentItem.FComment.Position:= 0;
          TMemoField(FieldByName('Comment')).LoadFromStream(lCurrentItem.FComment);

          FieldByName('Work_Address').AsBoolean:= lCurrentItem.FWorkAddress;
          FieldByName('Preferred').AsBoolean   := lCurrentItem.FPreferred;
          FieldByName('Name_Key').AsString     := FKeyString;
          FieldByName('Entered_By').AsString   := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ENameError.Create(Format(ResStr_AddFail, ['ADDRESS']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TAddressList.ProcessUpdate;
var lUpdateQuery: TJNCCQuery;
begin
  DoAdditions;
  DoModifications;

  {Create a temporary query for doing deletions}
  lUpdateQuery := TJNCCQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lUpdateQuery]);
    DeleteFromTable(lUpdateQuery, 'ADDRESS', 'ADDRESS_KEY');
  finally
    lUpdateQuery.Free;
  end;
end;  // ProcessUpdate

//==============================================================================
{ TContactItem }
function TContactItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: if FPreferred then //Use + to signal check box
        Result:= '+'
      else
        Result:= '';
    1: Result:= FContactType;
    2: Result:= FPrefix;
    3: Result:= FNumber;
  else
    Result := ResStr_NotImplemented;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TContactItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do
  begin
    try
      FItemKey:= FieldByName('CONTACT_NUMBER_KEY').AsString;
      FPrefix:= FieldByName('PREFIX').AsString;
      FNumber:= FieldByName('NUMBER').AsString;
      FPreferred:= FieldByName('PREFERRED').AsBoolean;

      FConstraints:= TMemoryStream.Create;
      TMemoField(FieldByName('CONSTRAINTS')).SaveToStream(FConstraints);

      FContactType:= FieldByName('CONTACT_NUMBER_TYPE').AsString;
    except on E:Exception do
      raise ENameError.Create(ResStr_InitRecFail + ' - CONTACT_NUMBER', E);
    end;
  end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TContactItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataSet do
    try
      FieldByName('PREFIX').AsString    := FPrefix;
      FieldByName('NUMBER').AsString    := FNumber;
      FieldByName('PREFERRED').AsBoolean:= FPreferred;

      FConstraints.Position:= 0;
      TMemoField(FieldByName('CONSTRAINTS')).LoadFromStream(FConstraints);

      FieldByName('CONTACT_NUMBER_TYPE').AsString:= FContactType;
      FieldByName('CHANGED_BY').AsString         := AppSettings.UserID;
      FieldByName('CHANGED_DATE').AsDateTime     := Now;
    except on E:Exception do
      raise ENameError.Create(ResStr_WriteRecFail + ' - CONTACT_NUMBER', E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TContactItem.SetContactType(const Value: string);
begin
  FContactType := Value;
  SetModified;
end;

procedure TContactItem.SetNumber(const Value: string);
begin
  FNumber := Value;
  SetModified;
end;

procedure TContactItem.SetPreferred(const Value: Boolean);
begin
  FPreferred := Value;
  SetModified;
end;

procedure TContactItem.SetPrefix(const Value: string);
begin
  FPrefix := Value;
  SetModified;
end;

constructor TContactItem.CreateNew(aOwner: TCacheDataList);
begin
  inherited CreateNew(aOwner);
  FConstraints:= TMemoryStream.Create;
end;

destructor TContactItem.Destroy;
begin
  FConstraints.Free;
  inherited;
end;

//==============================================================================
{ TContactList }
constructor TContactList.Create(iDataset: TDataset; iKeyFieldName: string;
  iStringGrid: TStringGrid);
begin
  inherited Create(iDataset, iKeyFieldName, iStringGrid, TContactItem);
end;  // Create

//------------------------------------------------------------------------------
function TContactList.ItemsDisplayName: String;
begin
  Result := ResStr_ContactNumbers;
end;

//------------------------------------------------------------------------------
procedure TContactList.DoAdditions;
var lqryAddition: TJnccQuery;
    i           : integer;
    lCurrentItem: TContactItem;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Contact_Number WHERE Contact_Number_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lCurrentItem := ItemsToAdd.Items[i];
          Append;
          FieldByName('Contact_Number_Key').AsString :=
              dmGeneralData.GetNextKey('Contact_Number','Contact_Number_Key');
          FieldByName('Prefix').AsString             := lCurrentItem.FPrefix;
          FieldByName('Number').AsString             := lCurrentItem.FNumber;
          FieldByName('Contact_Number_Type').AsString:= lCurrentItem.FContactType;

          lCurrentItem.FConstraints.Position:= 0;
          TMemoField(FieldByName('Constraints')).LoadFromStream(lCurrentItem.FConstraints);

          FieldByName('Preferred').AsBoolean  := lCurrentItem.FPreferred;
          FieldByName('Name_Key').AsString    := FKeyString;
          FieldByName('Entered_By').AsString  := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ENameError.Create(Format(ResStr_AddFail, ['CONTACT_NUMBER']), E);
      end;
  finally
    lqryAddition.Free;
  end; // with
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TContactList.ProcessUpdate;
var lUpdateQuery: TJNCCQuery;
begin
  DoAdditions;
  DoModifications;

  {Create a temporary query for doing deletions}
  lUpdateQuery := TJNCCQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lUpdateQuery]);
    DeleteFromTable(lUpdateQuery, 'CONTACT_NUMBER', 'CONTACT_NUMBER_KEY');
  finally
    lUpdateQuery.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TContactList.SetKeyString(const Value: TKeyString);
begin
  FKeyString := Value;
end;  // SetKeyString

//==============================================================================
{ TCommsItem }
constructor TCommsItem.CreateNew(aOwner: TCacheDataList);
begin
  inherited CreateNew(aOwner);
  FContent:= TMemoryStream.Create;
end;  // CreateNew

//------------------------------------------------------------------------------
destructor TCommsItem.Destroy;
begin
  FContent.Free;
  inherited;
end;  // Destroy

//------------------------------------------------------------------------------
function TCommsItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: Result:= FDate;
    1: Result:= FWithString;
    2: Result:= FCommsType;
  else
    Result := ResStr_NotImplemented;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TCommsItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FItemKey:= FieldByName('COMMUNICATION_KEY').AsString;

      FWithID    := FieldByName('Name_Key_2').AsString;
      FWithString:= dmGeneralData.GetName(FWithId);
      FCommsType := FieldByName('COMMUNICATION_TYPE').AsString;
      FDate      := FieldByName('VAGUE_DATE_START').Text;

      FContent:= TMemoryStream.Create;
      TMemoField(FieldByName('CONTENT')).SaveToStream(FContent);

      FFileRef:= FieldByName('FILE_REF').AsString;
    except on E:Exception do
      raise ENameError.Create(ResStr_InitRecFail + ' - COMMUNICATION', E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TCommsItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FieldByName('Name_Key_2').AsString        := FWithID;
      FieldByName('COMMUNICATION_TYPE').AsString:= FCommsType;
      FieldByName('VAGUE_DATE_START').Text      := FDate;

      FContent.Position:= 0;
      TMemoField(FieldByName('CONTENT')).LoadFromStream(FContent);

      FieldByName('FILE_REF').AsString:= FFileRef;
    except on E:Exception do
      raise ENameError.Create(ResStr_WriteRecFail + ' - COMMUNICATION', E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TCommsItem.SetCommsType(const Value: string);
begin
  FCommsType := Value;
  SetModified;
end;

procedure TCommsItem.SetDate(const Value: string);
begin
  FDate := Value;
  SetModified;
end;

procedure TCommsItem.SetFileRef(const Value: string);
begin
  FFileRef := Value;
  SetModified;
end;

procedure TCommsItem.SetWithID(const Value: TKeyString);
begin
  FWithID := Value;
  SetModified;
end;

procedure TCommsItem.SetWithString(const Value: string);
begin
  FWithString := Value;
  SetModified;
end;

//==============================================================================
{ TCommsList }
constructor TCommsList.Create(iDataset: TDataset; iKeyFieldName: string;
  iStringGrid: TStringGrid);
begin
  inherited Create(iDataset, iKeyFieldName, iStringGrid, TCommsItem);
end;  // Create

//------------------------------------------------------------------------------
function TCommsList.ItemsDisplayName: String;
begin
  Result := ResStr_Communication;
end;

//------------------------------------------------------------------------------
procedure TCommsList.DoAdditions;
var lqryAddition: TJnccQuery;
    i           : integer;
    lCurrentItem: TCommsItem;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Communication WHERE Communication_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do begin
          lCurrentItem := ItemsToAdd.Items[i];
          Append;
          FieldByName('Communication_Key').AsString :=
              dmGeneralData.GetNextKey('Communication','Communication_Key');
          FieldByName('Name_Key_2').AsString        := lCurrentItem.FWithID;
          FieldByName('Communication_Type').AsString:= lCurrentItem.FCommsType;
          FieldByName('Vague_Date_Start').Text      := lCurrentItem.FDate;

          lCurrentItem.FContent.Position:= 0;
          TMemoField(FieldByName('Content')).LoadFromStream(lCurrentItem.FContent);

          FieldByName('File_Ref').AsString    := lCurrentItem.FFileRef;
          FieldByName('Name_Key_1').AsString  := FKeyString;
          FieldByName('Entered_By').AsString  := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise ENameError.Create(Format(ResStr_AddFail, ['COMMUNICATION']), E);
      end;
  finally
    lqryAddition.Free;
  end; // with
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TCommsList.ProcessUpdate;
var lUpdateQuery: TJNCCQuery;
begin
  DoAdditions;
  DoModifications;

  {Create a temporary query for doing deletions}
  lUpdateQuery := TJNCCQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lUpdateQuery]);
    DeleteFromTable(lUpdateQuery, 'COMMUNICATION', 'COMMUNICATION_KEY');
  finally
    lUpdateQuery.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TCommsList.SetKeyString(const Value: TKeyString);
begin
  FKeyString:= Value;
end;  // SetKeyString

//==============================================================================
{ TAssocItem }
constructor TAssocItem.CreateNew(aOwner: TCacheDataList);
begin
  inherited CreateNew(aOwner);
  FComment:= TMemoryStream.Create;
end;  // CreateNew

//------------------------------------------------------------------------------
destructor TAssocItem.Destroy;
begin
  FComment.Free;
  inherited;
end;  // Destroy

//------------------------------------------------------------------------------
function TAssocItem.GetCell(const iX: integer): string;
begin
  case iX of
    0: Result:= FWithString;
    1: Result:= FRole;
    2: Result:= FNameCode;
    3: Result:= FDateFrom;
    4: Result:= FDateTo;
  else
    Result := ResStr_NotImplemented;
  end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TAssocItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FItemKey:= FieldByName('NAME_RELATION_KEY').AsString;

      FWithID    := FieldByName('Name_Key_2').AsString;
      FWithString:= dmGeneralData.GetName(FWithId);
      FRole      := FieldByName('ROLE').AsString;
      FNameCode  := FieldByName('NAME_CODE').AsString;

      FComment:= TMemoryStream.Create;
      TMemoField(FieldByName('COMMENT')).SaveToStream(FComment);

      FDateFrom:= FieldByName('FROM_VAGUE_DATE_START').Text;
      FDateTo  := FieldByName('TO_VAGUE_DATE_START').Text;
    except on E:Exception do
      raise ENameError.Create(ResStr_InitRecFail + ' - NAME_RELATION', E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TAssocItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FieldByName('Name_Key_2').AsString:= FWithID;
      FieldByName('ROLE').AsString      := FRole;
      FieldByName('NAME_CODE').AsString := FNameCode;

      FComment.Position:= 0;
      TMemoField(FieldByName('COMMENT')).LoadFromStream(FComment);

      FieldByName('FROM_VAGUE_DATE_START').Text:= FDateFrom;
      FieldByName('TO_VAGUE_DATE_START').Text  := FDateTo;
      Fieldbyname('Changed_By').AsString       := AppSettings.UserID;
      Fieldbyname('Changed_Date').AsDateTime   := Now;
    except on E:Exception do
      raise ENameError.Create(ResStr_WriteRecFail + ' - NAME_RELATION', E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TAssocItem.SetDateFrom(const Value: string);
begin
  FDateFrom := Value;
  SetModified;
end;

procedure TAssocItem.SetDateTo(const Value: string);
begin
  FDateTo := Value;
  SetModified;
end;

procedure TAssocItem.SetNameCode(const Value: string);
begin
  FNameCode := Value;
  SetModified;
end;

procedure TAssocItem.SetRole(const Value: string);
begin
  FRole := Value;
  SetModified;
end;

procedure TAssocItem.SetWithID(const Value: TKeyString);
begin
  FWithID := Value;      
  SetModified;
end;

procedure TAssocItem.SetWithString(const Value: string);
begin
  FWithString := Value;
  SetModified;
end;

//==============================================================================
{ TAssocList }
constructor TAssocList.Create(iDataset: TDataset; iKeyFieldName: string;
  iStringGrid: TStringGrid);
begin
  inherited Create(iDataset, iKeyFieldName, iStringGrid, TAssocItem);
end;  // Create

//------------------------------------------------------------------------------
function TAssocList.ItemsDisplayName: String;
begin
  Result := ResStr_Association;
end;

//------------------------------------------------------------------------------
procedure TAssocList.SetKeyString(const Value: TKeyString);
begin
  FKeyString:= Value;
end;  // SetKeyString

//------------------------------------------------------------------------------
procedure TAssocList.DoAdditions;
var lqryAddition: TJnccQuery;
    i           : integer;
    lCurrentItem: TAssocItem;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
   dmDatabase.SetDatabaseLocal([lqryAddition]);
   with lqryAddition do
     try
       SQL.Text := 'SELECT * FROM Name_Relation WHERE Name_Relation_Key = ''''';
       Open;
       for i := 0 to ItemsToAdd.Count - 1 do begin
          lCurrentItem := ItemsToAdd.Items[i];
          Append;
          FieldByName('Name_Relation_key').AsString:=
              dmGeneralData.GetNextKey('Name_Relation','Name_Relation_Key');
          FieldByName('Name_Key_1').AsString       := FKeyString;
          FieldByName('Name_Key_2').AsString       := lCurrentItem.FWithID;

          lCurrentItem.FComment.Position:= 0;
          TMemoField(FieldByName('Comment')).LoadFromStream(lCurrentItem.FComment);

          FieldByName('From_Vague_Date_Start').Text:= lCurrentItem.DateFrom;
          FieldByName('To_Vague_Date_Start').Text  := lCurrentItem.DateTo;
          FieldByName('Role').AsString             := lCurrentItem.Role;
          FieldByName('Name_Code').AsString        := lCurrentItem.NameCode;
          FieldByName('Entered_By').AsString       := AppSettings.UserID;
          Post;
       end;
       Close;
      except on E:Exception do
        raise ENameError.Create(Format(ResStr_AddFail, ['NAME_RELATION']), E);
      end;
  finally
    lqryAddition.Free;
  end; // with
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TAssocList.ProcessUpdate;
var lUpdateQuery: TJNCCQuery;
begin
  DoAdditions;
  DoModifications;

  {Create a temporary query for doing deletions}
  lUpdateQuery := TJNCCQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lUpdateQuery]);
    DeleteFromTable(lUpdateQuery, 'NAME_RELATION', 'NAME_RELATION_KEY');
  finally
    lUpdateQuery.Free;
  end;
end;  // ProcessUpdate

//==============================================================================
end.
