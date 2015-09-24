//==============================================================================
//  Unit:        ReferencesData
//
//  Implements:  TdmReferences
//               TAuthorItem
//               TAuthorList
//               TEditorItem
//               TEditorList
//               TISBNItem
//               TISBNList
//               TPeopleItem
//               TPeopleList
//
//  Description: Implements data access functionality for the reference details
//               screen.
//
//               TAuthorItem and TAuthorList
//               Helper classes used on the General tab of the details screen.
//
//               TEditorItem and TEditorList
//               Helper classes used on the Details tab of the details screen.
//
//               TISBNItem and TISBNList
//               Helper classes used on the Other tab of the details screen.
//
//               TPeopleItem and TPeopleList
//               Helper classes used as common ancestor classes for the Author
//               and Editor classes.
//
//  Author:      Ian Addis
//  Created:     26 May 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 47 $
//    $Date: 27/02/08 11:10 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit ReferencesData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, Db, ADODB, DatabaseAccessADO, JNCCDatasets, DataClasses, Grids,
  Constants, ExceptionForm;

type
  EReferenceError=class(TExceptionPath);

  TdmReferences = class(TBaseDataModule)
    qryReference: TJNCCQuery;
    qryPopulate: TJNCCQuery;
    dsReference: TDataSource;
    tblReferenceType: TJNCCTable;
    dsReferenceType: TDataSource;
    qryAuthors: TJNCCQuery;
    dsJournal: TDataSource;
    qryEditors: TJNCCQuery;
    qryISBNs: TJNCCQuery;
    qrySource: TJNCCQuery;
    qryJournal: TJNCCQuery;
    qryReferenceWithCustodian: TJNCCQuery;
    qryKeywords: TJNCCQuery;
    qryKeywordsConcept_Key: TStringField;
    qryKeywordsPlaintext: TWideStringField;
    qryKeywordsReference_Keyword_Key: TStringField;
    qryKeywordsCustodian: TStringField;
    qryKeywordsEntered_By: TStringField;
  protected
  public
    procedure DeleteReference(const AKey: TKeyString);
  end;  // TdmReference

  //============================================================================
  { TPeopleItem - Abstract class for creating author and editor strings }
  TPeopleItem = class(TStringDataItem)
  private
    FName: String;
    FSortOrder : Integer;
    procedure SetName(const Value: String);
  protected
    FCanDelete: boolean;
    FCanEdit: boolean;
    procedure SetEditability(ADataset: TDataset);
  public
    constructor CreateNew(aOwner: TCacheDataList); override;
    property Name: String read FName write SetName;
    property SortOrder : Integer read FSortOrder write FSortOrder;
    property CanEdit: boolean read FCanEdit;
    property CanDelete: boolean read FCanDelete;
  end;  // TPeopleItem

  //============================================================================
  { TPeopleList - Abstract class for creating author and editor strings }
  TPeopleList = class(TStringDataList)
  private
    FRestrictFullEdit: boolean;
    FAddOnly: boolean;
    FUserID: string;
    FSiteID: string;
    procedure SetAddOnly(const Value: boolean);
    procedure SetRestrictFullEdit(const Value: boolean);
    procedure SetSiteID(const Value: string);
    procedure SetUserID(const Value: string);
  protected
    procedure ProcessUpdate; override;
  public
		function CreatePeopleString: String;
    procedure SetSortOrder;
    property SiteID: string read FSiteID write SetSiteID;
    property UserID: string read FUserID write SetUserID;
    property RestrictFullEdit: boolean read FRestrictFullEdit write SetRestrictFullEdit;
    property AddOnly: boolean read FAddOnly write SetAddOnly;
  end;  // TPeopleList

  //============================================================================
  { TAuthorItem - Single author item for general tab in References }
  TAuthorItem = class(TPeopleItem)
  private
    FInitials: String;
    procedure SetInitials(const Value: String);
	protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
	public
    property Initials: String read FInitials write SetInitials;
  end;  // TAuthorItem

  //============================================================================
  { TAuthorList - List of authors for general tab in References }
  TAuthorList = class(TPeopleList)
  private
    FNameKey: TKeyString;
    procedure SetNameKey(const Value: TKeyString);
	protected
    function ItemsDisplayName: String; override;
    function GetText(iDataItem: TDataItem): string; override;
    procedure DoAdditions(iQuery: TJNCCQuery);
    procedure ProcessUpdate; override;
  public
    constructor Create(iDataSet : TDataSet; const iKeyFieldName : string;
                iStrings : TStrings); reintroduce; overload;
    property NameKey: TKeyString read FNameKey write SetNameKey;
  end;  // TAuthorList

  //============================================================================
  { TEditorItem - Single editor item for details tab in References }
  TEditorItem = class(TPeopleItem)
  private
    FInitials: String;
    procedure SetInitials(const Value: String);
	protected
    procedure InitFromRecord(iDataset: TDataset); override;
    procedure WriteToRecord(iDataset: TDataset); override;
	public
    property Initials: String read FInitials write SetInitials;
  end;  // TEditorItem

  //----------------------------------------------------------------------------
  //TEditorList - List of editors for general tab in References
	TEditorList = class(TPeopleList)
  private
    FNameKey: TKeyString;
    procedure SetNameKey(const Value: TKeyString);
	protected
    function ItemsDisplayName: String; override;
    function GetText(iDataItem: TDataItem): string; override;
    procedure DoAdditions(iQuery: TJNCCQuery);
    procedure ProcessUpdate; override;
  public
    constructor Create( iDataSet : TDataSet; const iKeyFieldName : string;
                iStrings : TStrings);  reintroduce; overload;
    property NameKey: TKeyString read FNameKey write SetNameKey;
	end;  // TEditorList

	//============================================================================
  //TISBNItem - Single reference number item for other tab in References
	TISBNItem = class(TGridDataItem)
	private
    FNumber: String;
    FTypeString: String;
    FCanEdit: Boolean;
    FCanDelete: Boolean;
    procedure SetNumber(const Value: String);
    procedure SetTypeString(const Value: String);
	protected
		procedure InitFromRecord( iDataset : TDataset ); override;
		procedure WriteToRecord( iDataset : TDataset ); override;
		function GetCell(const iX: integer): string; override;
	public
    constructor CreateNew(aOwner: TCacheDataList); override;
    property Number: String read FNumber write SetNumber;
    property TypeString: String read FTypeString write SetTypeString;
    property CanEdit: Boolean read FCanEdit;
    property CanDelete: Boolean read FCanDelete;
	end;  // TISBNItem

  //----------------------------------------------------------------------------
  //TISBNList - List of reference numbers for Other tab in References
	TISBNList = class(TGridDataList)
  private
    FNameKey: TKeyString;
    FAddOnly: boolean;
    FRestrictFullEdit: boolean;
    FSiteID: string;
    FUserID: string;
    procedure SetNameKey(const Value: TKeyString);
    procedure SetAddOnly(const Value: boolean);
    procedure SetRestrictFullEdit(const Value: boolean);
    procedure SetSiteID(const Value: string);
    procedure SetUserID(const Value: string);
	protected
    function ItemsDisplayName: String; override;
		procedure DoAdditions(iQuery: TJNCCQuery);
		procedure ProcessUpdate; override;
  public
		constructor Create(iDataset: TDataset; iKeyFieldName: String; iStringGrid: TStringGrid);
    property NameKey: TKeyString read FNameKey write SetNameKey;
    property AddOnly: boolean read FAddOnly write SetAddOnly;
    property RestrictFullEdit: boolean read FRestrictFullEdit write 
        SetRestrictFullEdit;
    property SiteID: string read FSiteID write SetSiteID;
    property UserID: string read FUserID write SetUserID;
	end;  // TISBNList

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralData, ApplicationSettings, GeneralFunctions, DateUtils;

//==============================================================================
{ TdmReferences }
procedure TdmReferences.DeleteReference(const AKey: TKeyString);
begin
  with dmGeneralData do begin
    qryAllPurpose.Connection.Execute ('set xact_abort on begin tran');
    ExecuteSQL('DELETE FROM Reference_Author WHERE Source_Key = '''+AKey+'''',
               ResStr_DelFail+' - REFERENCE_AUTHOR');
    //Delete reference_editor record(s)
    ExecuteSQL('DELETE FROM Reference_Editor WHERE Source_Key = '''+AKey+'''',
               ResStr_DelFail+'REFERENCE_EDITOR');
    //Delete reference_number record(s)
    ExecuteSQL('DELETE FROM Reference_Number WHERE Source_Key = '''+AKey+'''',
               ResStr_DelFail+'REFERENCE_NUMBER');
    //Delete reference record
    ExecuteSQL('DELETE FROM Reference WHERE Source_Key = '''+AKey+'''',
               ResStr_DelFail+'REFERENCE');
    //Delete source record
    ExecuteSQL('DELETE FROM Source WHERE Source_Key = '''+AKey+'''',
               ResStr_DelFail+'SOURCE');
    qryAllPurpose.Connection.Execute('Commit tran');
  end;
end;  // DeleteReference

//==============================================================================
{ TPeopleItem }

{-------------------------------------------------------------------------------
  Initialise the CanEdit and CanDelete for new items to true
}
constructor TPeopleItem.CreateNew(aOwner: TCacheDataList);
begin
  inherited;
  FCanEdit := true;
  FCanDelete := true;
end;

{-------------------------------------------------------------------------------
  Initialise the CanEdit and CanDelete properties according to user access
     rights
}
procedure TPeopleItem.SetEditability(ADataset: TDataset);
begin
  FCanDelete := ((not TPeopleList(OwnerList).RestrictFullEdit)
           or (ADataset.FieldByName('Entered_By').AsString=TPeopleList(OwnerList).UserID))
           and (not TPeopleList(OwnerList).AddOnly);
  FCanEdit := FCanDelete
           and (ADataset.FieldByName('Custodian').AsString=TPeopleList(OwnerList).SiteID);
end;

{-------------------------------------------------------------------------------
}
procedure TPeopleItem.SetName(const Value: String);
begin
  FName := Value;
  SetModified;
end;  // SetName

//==============================================================================
{ TPeopleList }
function TPeopleList.CreatePeopleString: String;
begin
  case Count of
    0: Result:= '';
    1: Result:= TPeopleItem(Items[0]).Name;
    2: Result:= TPeopleItem(Items[0]).Name + ' & ' + TPeopleItem(Items[1]).Name;
  else
    Result:= TPeopleItem(Items[0]).Name + ' et al';
  end;
end;  // CreatePeopleString

//------------------------------------------------------------------------------
procedure TPeopleList.SetSortOrder;
var
  iSortCode,iCount :integer;
  iSetModified : Boolean;
begin
  iSortCode:=0;

  //if we have any deleted items then we need to modify all items in the list so that their sort order is updated
  iSetModified := (ItemsToDelete.Count > 0);

  for iCount:=0 to Count-1 do
  begin
    if not TPeopleItem(Items[iCount]).Deleted then
    begin
      Inc(iSortCode);
      TPeopleItem(Items[iCount]).SortOrder := iSortCode;
      if iSetModified then TPeopleItem(Items[iCount]).SetModified;
    end
    else
      TPeopleItem(Items[iCount]).SortOrder := -1;
  end;

  if iSetModified then SetLists;
end;  // SetSortOrder

//------------------------------------------------------------------------------
procedure TPeopleList.ProcessUpdate;
begin
  SetSortOrder;
end;  // ProcessUpdate

{-------------------------------------------------------------------------------
}
procedure TPeopleList.SetAddOnly(const Value: boolean);
begin
  FAddOnly := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TPeopleList.SetRestrictFullEdit(const Value: boolean);
begin
  FRestrictFullEdit := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TPeopleList.SetSiteID(const Value: string);
begin
  FSiteID := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TPeopleList.SetUserID(const Value: string);
begin
  FUserID := Value;
end;

{ TAuthorItem }
procedure TAuthorItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FItemKey := FieldByName('AUTHOR_KEY').AsString;
      FName    := FieldByName('ITEM_NAME').AsString;
      FInitials:= FieldByName('INITIALS').AsString;
      SetEditability(iDataset);
    except on E:Exception do
      raise EReferenceError.Create(ResStr_InitRecFail+' - REFERENCE_AUTHOR',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TAuthorItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FieldByName('ITEM_NAME').AsString     := FName;
      FieldByName('INITIALS').AsString      := FInitials;
      FieldByName('CHANGED_BY').AsString    := AppSettings.UserID;
      FieldByName('CHANGED_DATE').AsDateTime:= Now;
      FieldByName('SORT_ORDER').AsInteger   := FSortOrder;
    except on E:Exception do
      raise EReferenceError.Create(ResStr_WriteRecFail+' - REFERENCE_AUTHOR',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TAuthorItem.SetInitials(const Value: String);
begin
  FInitials := Value;
  SetModified;
end;  // SetInitials

//==============================================================================
{ TAuthorList }
constructor TAuthorList.Create( iDataSet : TDataSet; const iKeyFieldName : string; iStrings : TStrings);
begin
  inherited Create(iDataSet, iKeyFieldName, iStrings, TAuthorItem);
end;  // Create

//------------------------------------------------------------------------------
function TAuthorList.ItemsDisplayName: String;
begin
  Result := ResStr_Author;
end;

//------------------------------------------------------------------------------
procedure TAuthorList.DoAdditions(iQuery: TJNCCQuery);
var iCount   :Integer;
    lDataItem:TAuthorItem;
begin
  with iQuery do
    try
      for iCount:= 0 to ItemsToAdd.Count - 1 do begin
        lDataItem:=TAuthorItem(ItemsToAdd[iCount]);
        SQL.Clear;
        SQL.Add('INSERT INTO Reference_Author (Author_Key, Item_Name, Initials, ' +
                'Source_Key, Entered_By, Sort_Order)');
        SQL.Add('VALUES (');
        SQL.Add('''' + dmGeneralData.GetNextKey('REFERENCE_AUTHOR','Author_Key') + ''', ');
        if lDataItem.Name = '' then
          SQL.Add('NULL, ')
        else
          SQL.Add(QuotedStr(lDataItem.Name) + ', ');
        if lDataItem.Initials = '' then
          SQL.Add('NULL, ')
        else
          SQL.Add(QuotedStr(lDataItem.Initials) + ', ');
        SQL.Add(QuotedStr(NameKey) + ', ''' + AppSettings.UserID + ''', ' + IntToStr(lDataItem.SortOrder) + ')');
        ExecSQL;
      end;
    except on E:Exception do
      raise EReferenceError.Create(Format(ResStr_AddFail, ['REFERENCE_AUTHOR']), E);
    end;
end;  // DoAdditions

//------------------------------------------------------------------------------
function TAuthorList.GetText(iDataItem: TDataItem): string;
var lAuthorItem: TAuthorItem;
begin
  lAuthorItem:= TAuthorItem(iDataItem);
  if lAuthorItem.FInitials='' then
    Result:=lAuthorItem.FName
  else
    Result:=lAuthorItem.FName+', '+lAuthorItem.FInitials;
end;  // GetText

//------------------------------------------------------------------------------
procedure TAuthorList.ProcessUpdate;
var
  lUpdateQuery: TJNCCQuery;
begin
  inherited ProcessUpdate;
  { Create a temporary query for doing updates }
  lUpdateQuery:= TJNCCQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lUpdateQuery]);
    DoAdditions(lUpdateQuery);
    DoModifications;
    DeleteFromTable(lUpdateQuery, 'REFERENCE_AUTHOR', 'AUTHOR_KEY');
  finally
    lUpdateQuery.Free;
  end;
end;  // Processupdate

//------------------------------------------------------------------------------
procedure TAuthorList.SetNameKey(const Value: TKeyString);
begin
  FNameKey:= Value;
end;  // SetNameKey

//==============================================================================
{ TEditorItem }
procedure TEditorItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FItemKey := FieldByName('EDITOR_KEY').AsString;
      FName    := FieldByName('ITEM_NAME').AsString;
      FInitials:= FieldByName('INITIALS').AsString;
      SetEditability(iDataset);
    except on E:Exception do
      raise EReferenceError.Create(ResStr_InitRecFail+' - REFERENCE_EDITOR',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TEditorItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FieldByName('ITEM_NAME').AsString     := FName;
      FieldByName('INITIALS').AsString      := FInitials;
      FieldByName('CHANGED_BY').AsString    := AppSettings.UserID;
      FieldByName('CHANGED_DATE').AsDateTime:= Now;
      FieldByName('SORT_ORDER').AsInteger   := FSortOrder;
    except on E:Exception do
      raise EReferenceError.Create(ResStr_WriteRecFail+' - REFERENCE_EDITOR',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TEditorItem.SetInitials(const Value: String);
begin
  FInitials := Value;
  SetModified;
end;  // SetInitials

//==============================================================================
{ TEditorList }
constructor TEditorList.Create( iDataSet : TDataSet; const iKeyFieldName : string; iStrings : TStrings);
begin
  inherited Create(iDataSet, iKeyFieldName, iStrings, TEditorItem);
end;  // Create

//------------------------------------------------------------------------------
function TEditorList.ItemsDisplayName: String;
begin
  Result := ResStr_Editor;;
end;

//------------------------------------------------------------------------------
procedure TEditorList.DoAdditions(iQuery: TJNCCQuery);
var iCount   :Integer;
    lDataItem:TEditorItem;
begin
  with iQuery do
    try
      for iCount:= 0 to ItemsToAdd.Count - 1 do begin
        lDataItem:=TEditorItem(ItemsToAdd[iCount]);
        SQL.Clear;
        SQL.Add('INSERT INTO REFERENCE_EDITOR (EDITOR_KEY, ITEM_NAME, INITIALS, ' +
                'SOURCE_KEY, ENTERED_BY, SORT_ORDER)');
        SQL.Add('VALUES (');
        SQL.Add('''' + dmGeneralData.GetNextKey('REFERENCE_EDITOR','Editor_Key') + ''', ');
        if lDataItem.Name = '' then
          SQL.Add('NULL, ')
        else
          SQL.Add(QuotedStr(lDataItem.Name) + ', ');
        if lDataItem.Initials = '' then
          SQL.Add('NULL, ')
        else
          SQL.Add(QuotedStr(lDataItem.Initials) + ', ');
        SQL.Add(QuotedStr(NameKey) + ', ''' + AppSettings.UserID + ''', ' + IntToStr(lDataItem.SortOrder) + ')');
        ExecSQL;
      end;
    except on E:Exception do
      raise EReferenceError.Create(FormaT(ResStr_AddFail, ['REFERENCE_EDITOR']), E);
    end;
end;  // DoAdditions

//------------------------------------------------------------------------------
function TEditorList.GetText(iDataItem: TDataItem): string;
var
  lEditorItem: TEditorItem;
begin
  lEditorItem:= TEditorItem(iDataItem);
  if lEditorItem.FInitials = '' then
    Result := lEditorItem.FName
  else
    Result:= lEditorItem.FName + ', ' + lEditorItem.FInitials;
end;  // GetText

//------------------------------------------------------------------------------
procedure TEditorList.ProcessUpdate;
var
  lUpdateQuery: TJNCCQuery;
begin
  inherited ProcessUpdate;
  { Create a temporary query for doing updates }
  lUpdateQuery:= TJNCCQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lUpdateQuery]);
    DoAdditions(lUpdateQuery);
    DoModifications;
    DeleteFromTable(lUpdateQuery, 'REFERENCE_EDITOR', 'EDITOR_KEY');
  finally
    lUpdateQuery.Free;
  end;
end;  // ProcessUpdate

//------------------------------------------------------------------------------
procedure TEditorList.SetNameKey(const Value: TKeyString);
begin
  FNameKey:= Value;
end;  // SetNameKey

//==============================================================================
{ TISBNItem }
procedure TISBNItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FItemKey   := FieldByName('NUMBER_KEY').AsString;
      FNumber    := FieldByName('NUMBER').AsString;
      FTypeString:= FieldByName('REFERENCE_NUMBER_TYPE').AsString;
      FCanDelete := ((not TISBNList(OwnerList).RestrictFullEdit)
           or (iDataset.FieldByName('Entered_By').AsString=TISBNList(OwnerList).UserID))
           and (not TISBNList(OwnerList).AddOnly);
      FCanEdit := FCanDelete
           and (iDataset.FieldByName('Custodian').AsString=TISBNList(OwnerList).SiteID);
    except on E:Exception do
      raise EReferenceError.Create(ResStr_InitRecFail+' - REFERENCE_NUMBER',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TISBNItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FieldByName('NUMBER').AsString               := FNumber;
      FieldByName('REFERENCE_NUMBER_TYPE').AsString:= FTypeString;
      FieldByName('CHANGED_BY').AsString           := AppSettings.UserID;
      FieldByName('CHANGED_DATE').AsDateTime       := Now;
    except on E:Exception do
      raise EReferenceError.Create(ResStr_WriteRecFail+' - REFERENCE_NUMBER',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
function TISBNItem.GetCell(const iX: integer): string;
begin
	case iX of
		0: Result:= FNumber;
		1: Result:= FTypeString;
	else
		Result := ResStr_NotImplemented;
	end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TISBNItem.SetNumber(const Value: String);
begin
  FNumber:= Value;
  SetModified;
end;  // SetNumber

//------------------------------------------------------------------------------
procedure TISBNItem.SetTypeString(const Value: String);
begin
  FTypeString:= Value;
  SetModified;
end;  // SetTypeString

{-------------------------------------------------------------------------------
  New items can always be edited and deleted
}
constructor TISBNItem.CreateNew(aOwner: TCacheDataList);
begin
  inherited;
  FCanEdit := true;
  FCanDelete := true;
end;


//==============================================================================
{ TISBNList }
constructor TISBNList.Create(iDataset: TDataset; iKeyFieldName: String; iStringGrid: TStringGrid);
begin
	inherited Create(iDataset, iKeyFieldName, iStringGrid, TISBNItem);
end;  // Create

//------------------------------------------------------------------------------
function TISBNList.ItemsDisplayName: String;
begin
  Result := ResStr_ReferenceNumber;
end;

//------------------------------------------------------------------------------
procedure TISBNList.DoAdditions(iQuery: TJNCCQuery);
var iCount   :Integer;
    lDataItem:TISBNItem;
begin
  with iQuery do
    try
      for iCount:= 0 to ItemsToAdd.Count - 1 do begin
        lDataItem:=TISBNItem(ItemsToAdd[iCount]);
        SQL.Clear;
        SQL.Add('INSERT INTO Reference_Number (Number_Key, [Number], ' +
                'Reference_Number_Type, Source_Key, Entered_By)');
        SQL.Add('VALUES (');
        SQL.Add('''' + dmGeneralData.GetNextKey('REFERENCE_NUMBER','Number_Key') + ''', ');
        SQL.Add(QuotedStr(lDataItem.Number) + ', ');
        SQL.Add(QuotedStr(lDataItem.TypeString) + ', ');
        SQL.Add('''' + NameKey + ''', ''' + AppSettings.UserID + ''')');
        ExecSQL;
      end;
    except on E:Exception do
      raise EReferenceError.Create(Format(ResStr_AddFail, ['REFERENCE_NUMBER']), E);
    end;
end;  // DoAdditions

//------------------------------------------------------------------------------
procedure TISBNList.ProcessUpdate;
var
  lUpdateQuery: TJNCCQuery;
begin
  { Create a temporary query for doing updates }
  lUpdateQuery:= TJNCCQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lUpdateQuery]);
    DoAdditions(lUpdateQuery);
    DoModifications;
    DeleteFromTable(lUpdateQuery, 'REFERENCE_NUMBER', 'NUMBER_KEY');
  finally
    lUpdateQuery.Free;
  end;
end;  // ProcessUpdate

{-------------------------------------------------------------------------------
}
procedure TISBNList.SetNameKey(const Value: TKeyString);
begin
  FNameKey:= Value;
end;  // SetNameKey

{-------------------------------------------------------------------------------
}
procedure TISBNList.SetAddOnly(const Value: boolean);
begin
  FAddOnly := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TISBNList.SetRestrictFullEdit(const Value: boolean);
begin
  FRestrictFullEdit := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TISBNList.SetSiteID(const Value: string);
begin
  FSiteID := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TISBNList.SetUserID(const Value: string);
begin
  FUserID := Value;
end;

end.

