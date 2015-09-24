//==============================================================================
//  Unit:        DeterminationData
//
//  Implements:  TdmDetermination
//               TDeterminationItem
//               TDeterminationList
//
//  Description: Implements functionalities available on the determination tab
//               of the biotope and taxon occurrence detail screens.
//
//               TDeterminationItem and TDeterminationList
//               Helper classes used on the Determination tab of both the
//               biotope and taxon details screens
//
//  Author:      Eric Salmon                       
//  Created:     25 May 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 34 $
//    $Date: 7/04/10 13:46 $
//    $Author: Robertjohnson $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

{$I '..\..\Third Party\Dorset Software Services\DssVcl32\trunk\DelphiVersions.Inc'}

unit DeterminationData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  BaseData, DataClasses, JNCCDatasets, Grids, ComCtrls, Constants, ExceptionForm,
  ADODB, DatabaseAccessADO, VagueDate {$IFDEF DELPHI7UP}, Variants {$ENDIF};

type
  EDeterminationError = class(TExceptionPath);

  TdmDetermination = class(TBaseDataModule)
    qryTaxonDet: TJNCCQuery;
    qryDetRole: TJNCCQuery;
    qryDetType: TJNCCQuery;
    dsDetRole: TDataSource;
    dsDetType: TDataSource;
    qryBiotopeDet: TJNCCQuery;
    qrySampleDate: TJNCCQuery;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TDeterminationItem = class(TGridDataItem)
  private
    FNameKey   :TKeyString;
    FName      :string;
    FDeterminerKey:TKeyString;
    FDeterminer:string;
    FRoleKey   :TKeyString;
    FRole      :string;
    FTypeKey   :TKeyString;
    FType      :string;
    FDate      :string;
    FPreferred :boolean;
    FWorkKey   :TKeyString;
    FWork      :string;
    FComment   :TMemoryStream;
    FVerified  :Integer;
    FCustodian : String;
    procedure SetNameKey(const Value: TKeyString);
    procedure SetName(const Value: string);
    procedure SetPreferred(const Value: boolean);
    procedure SetRole(const Value: string);
    procedure SetRoleKey(const Value: TKeyString);
    procedure SetType(const Value: string);
    procedure SetTypeKey(const Value: TKeyString);
    procedure SetWork(const Value: string);
    procedure SetDeterminerKey(const Value: TKeyString);
    procedure SetWorkKey(const Value: TKeyString);
    procedure SetDeterminer(const Value: string);
    procedure SetDate(const Value: string);
    procedure SetVerified(const Value: Integer);
    procedure SetCustodian(const Value: String);
  protected
    procedure InitFromRecord(ADataset: TDataset); override;
    procedure WriteToRecord(ADataset: TDataset); override;
    function GetCell( const iX : integer ): string; override;
  public
    constructor CreateNew(AOwner:TCacheDataList); override;
    destructor Destroy; override;
    property ItemNameKey:TKeyString read FNameKey write SetNameKey;
    property ItemName:string read FName write SetName;
    property DeterminerKey:TKeyString read FDeterminerKey write SetDeterminerKey;
    property Determiner:string read FDeterminer write SetDeterminer;
    property RoleKey:TKeyString read FRoleKey write SetRoleKey;
    property Role:string read FRole write SetRole;
    property DetTypeKey:TKeyString read FTypeKey write SetTypeKey;
    property DetType:string read FType write SetType;
    property Preferred:boolean read FPreferred write SetPreferred;
    property WorkKey:TKeyString read FWorkKey write SetWorkKey;
    property Work:string read FWork write SetWork;
    property Comment:TMemoryStream read FComment;
    property Date:string read FDate write SetDate;
    property Verified:Integer read FVerified write SetVerified;
    property Custodian: String read FCustodian write SetCustodian;
  end;

  TDeterminationList = class(TGridDataList)
  private
    FOccKey:TKeyString;
    FTaxon :boolean;
    function GetCommentString(ADeterminationItem: TDeterminationItem): string;
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure DoModifications; override;
    procedure ProcessUpdate; override;
  public
    constructor Create(iDataSet: TDataset; const iKeyFieldName: string;
      iStringGrid: TStringGrid; iItemClass: TItemClass);
    property OccKey:TKeyString read FOccKey write FOccKey;
    property Taxon:boolean read FTaxon write FTaxon;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses GeneralData, ApplicationSettings;

//==============================================================================
{ TDeterminationItem }

constructor TDeterminationItem.CreateNew(AOwner: TCacheDataList);
begin
  inherited CreateNew(AOwner);
  FComment:=TMemoryStream.Create;
end;  // CreateNew

//------------------------------------------------------------------------------
destructor TDeterminationItem.Destroy;
begin
  FComment.Free;
  inherited Destroy;
end;  // Destroy

//------------------------------------------------------------------------------
function TDeterminationItem.GetCell(const iX: integer): string;
begin
  case iX of
    1 : Result:=ItemName;
    2 : Result:=Determiner;
    3 : Result:=Role;
    4 : Result:=DetType;
    5 : Result:=Date;
  end;
end;  // GetCell

//------------------------------------------------------------------------------
procedure TDeterminationItem.InitFromRecord(ADataset: TDataset);
begin
  with ADataset do
    try
      if TDeterminationList(OwnerList).Taxon then begin
        FItemKey:=FieldByName('Taxon_Determination_Key').AsString;
        FNameKey:=FieldByName('Taxon_List_Item_Key').AsString;
        FName   :=FieldByName('Item_Name').AsString;
      end else begin
        FItemKey:=FieldByName('Biotope_Determination_Key').AsString;
        FNameKey:=FieldByName('Biotope_List_Item_Key').AsString;
        FName   :=FieldByName('Short_Term').AsString;
        if FieldByName('Original_Code').AsString<>'' then
          FName:=FieldByName('Original_Code').AsString+', '+FName;
      end;
      FDeterminerKey:=FieldByName('Determiner').AsString;
      FDeterminer:=dmGeneralData.GetIndividualName(FDeterminerKey);
      FRoleKey   :=FieldByName('Determiner_Role_Key').AsString;
      FRole      :=FieldByName('Det_Role').AsString;
      FTypeKey   :=FieldByName('Determination_Type_Key').AsString;
      FType      :=FieldByName('Det_Type').AsString;
      FPreferred :=FieldByName('Preferred').AsBoolean;
      FDate      :=FieldByName('Vague_Date_Start').Text;
      FWorkKey   :=FieldByName('Source_Key').AsString;
      FWork      :=dmGeneralData.GetReferenceText(FWorkKey);
      FComment   :=TMemoryStream.Create;
      FVerified  :=FieldByName('Verified').Value;
      FCustodian :=FieldByName('Custodian').AsString;
      TMemoField(FieldByName('Comment')).SaveToStream(FComment);
    except on E:Exception do
      raise EDeterminationError.Create(ResStr_InitRecFail+' - DETERMINATION',E);
    end;
end;  // InitFromRecord

//------------------------------------------------------------------------------
procedure TDeterminationItem.WriteToRecord(ADataset: TDataset);
begin
  with ADataset do
    try
      // can only save the preferred flag if not the custodian
      if Custodian=AppSettings.SiteID then begin
        if TDeterminationList(OwnerList).Taxon then
          FieldByName('Taxon_List_Item_Key').AsString:=FNameKey
        else
          FieldByName('Biotope_List_Item_Key').AsString:=FNameKey;
        FieldByName('Determiner').AsString            :=FDeterminerKey;
        FieldByName('Determiner_Role_Key').AsString   :=FRoleKey;
        FieldByName('Determination_Type_Key').AsString:=FTypeKey;
        FieldByName('Vague_Date_Start').Text          :=FDate;
        if FWorkKey = '' then
          FieldByName('Source_Key').Value := Null
        else
          FieldByName('Source_Key').AsString :=FWorkKey;

        FComment.Position:= 0;
        TMemoField(FieldByName('Comment')).LoadFromStream(FComment);
        FieldByName('Changed_By').AsString    :=AppSettings.UserID;
        FieldByName('Changed_Date').AsDateTime:=Now;
      end;
      FieldByName('Preferred').AsBoolean            :=FPreferred;
    except on E:Exception do
      raise EDeterminationError.Create(ResStr_WriteRecFail+' - DETERMINATION',E);
    end;
end;  // WriteToRecord

//------------------------------------------------------------------------------
procedure TDeterminationItem.SetDeterminerKey(const Value: TKeyString);
begin
  FDeterminerKey := Value;
  SetModified;
end;

procedure TDeterminationItem.SetPreferred(const Value: boolean);
begin
  FPreferred := Value;
  SetModified;
end;

procedure TDeterminationItem.SetRole(const Value: string);
begin
  FRole := Value;
  SetModified;
end;

procedure TDeterminationItem.SetRoleKey(const Value: TKeyString);
begin
  FRoleKey := Value;
  SetModified;
end;

procedure TDeterminationItem.SetType(const Value: string);
begin
  FType := Value;
  SetModified;
end;

procedure TDeterminationItem.SetTypeKey(const Value: TKeyString);
begin
  FTypeKey := Value;
  SetModified;
end;

procedure TDeterminationItem.SetWork(const Value: string);
begin
  FWork := Value;
  SetModified;
end;

procedure TDeterminationItem.SetWorkKey(const Value: TKeyString);
begin
  FWorkKey := Value;
  SetModified;
end;

procedure TDeterminationItem.SetDeterminer(const Value: string);
begin
  FDeterminer := Value;
  Setmodified;
end;

procedure TDeterminationItem.SetName(const Value: string);
begin
  FName := Value;
  SetModified;
end;

procedure TDeterminationItem.SetNameKey(const Value: TKeyString);
begin
  FNameKey := Value;
  SetModified;
end;

procedure TDeterminationItem.SetDate(const Value: string);
begin
  FDate := Value;
  SetModified;
end;

procedure TDeterminationItem.SetVerified(const Value: Integer);
begin
  FVerified := Value;
  SetModified;
end;

procedure TDeterminationItem.SetCustodian(const Value: string);
begin
  FCustodian := Value;
  SetModified;
end;

//==============================================================================
{ TDeterminationList }
function TDeterminationList.ItemsDisplayName: String;
begin
  if Taxon then Result := ResStr_TaxonDetermination
           else Result := ResStr_BiotopeDetermination;
end;

//------------------------------------------------------------------------------
procedure TDeterminationList.DoAdditions;
var i           : integer;
    lItem   : TDeterminationItem;
    lTableName  : string;
    lKey: String;
    lSourceKey: String;
    lComment: String;
    lDate: TVagueDate;
    lSQL: String;
begin
  try
    if Taxon then lTableName := 'Taxon' else lTableName := 'Biotope';

    for i := 0 to ItemsToAdd.Count - 1 do begin
      lItem := TDeterminationItem(ItemsToAdd[i]);
      lKey := dmGeneralData.GetNextKey(lTableName + '_Determination',
                                       lTableName + '_Determination_Key');

      lDate := StringToVagueDate(lItem.Date);
      lSourceKey := 'Null';
      if lItem.WorkKey <> '' then lSourceKey := '''' + lItem.WorkKey + '''';
      lComment := GetCommentString(lItem);

      lSQL := Format(
          'INSERT INTO %s_Determination (%s_Determination_Key, %s_Occurrence_Key, ' +
          '%s_List_Item_Key, Determiner, Determiner_Role_Key, Determination_Type_Key, ' +
          'Preferred, Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Source_Key, ' +
          '[Comment], Entered_By) VALUES ( ' +
          '''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %d, %d, %d, ''%s'', %s, %s, ''%s'')',
          [lTableName, lTableName, lTableName, lTableName, lKey, OccKey, lItem.ItemNameKey,
           lItem.DeterminerKey, lItem.RoleKey, lItem.DetTypeKey, Ord(lItem.Preferred),
           Trunc(lDate.StartDate), Trunc(lDate.EndDate), lDate.DateTypeString, lSourceKey,
           lComment, AppSettings.UserID]);

      dmDatabase.ExecuteSQL(lSQL);
    end;
  except on E:Exception do
    raise EDeterminationError.Create(Format(ResStr_AddFail, [UpperCase(lTableName)]), E);
  end;
end;  // DoAdditions

{-------------------------------------------------------------------------------
}
procedure TDeterminationList.DoModifications;
var
  i: Integer;
  lItem: TDeterminationItem;
  lTableName: String;
  lSourceKey: String;
  lComment: String;
  lDate: TVagueDate;
  lSQL: String;
begin
  // Do NOT call inherited!!!!!

  if Taxon then lTableName := 'Taxon' else lTableName := 'Biotope';

  for i := 0 to ItemsToModify.Count - 1 do begin
    lItem := TDeterminationItem(ItemsToModify[i]);

    lDate := StringToVagueDate(lItem.Date);
    lSourceKey := 'Null';
    if lItem.WorkKey <> '' then lSourceKey := '''' + lItem.WorkKey + '''';
    lComment := GetCommentString(lItem);

    lSQL := Format(
      'UPDATE %s_Determination SET %s_List_Item_Key = ''%s'', Vague_Date_Start = %d, ' +
      'Vague_Date_End = %d, Vague_Date_Type = ''%s'', Comment = %s, Preferred = %d, ' +
      'Determiner = ''%s'', Determination_Type_Key = ''%s'', Determiner_Role_Key = ''%s'', ' +
      'Changed_By = ''%s'', Changed_Date = GetDate(), Source_Key = %s ' +
      'WHERE %s_Determination_Key = ''%s''',
      [lTableName, lTableName, lItem.ItemNameKey, Trunc(lDate.StartDate), Trunc(lDate.EndDate),
       lDate.DateTypeString, lComment, Ord(lItem.Preferred), lItem.DeterminerKey,
       lItem.DetTypeKey, lItem.RoleKey, AppSettings.UserID, lSourceKey, lTableName, lItem.ItemKey]);

    dmDatabase.ExecuteSQL(lSQL);
  end;
end;  // DoModifications

{-------------------------------------------------------------------------------
  Gets the comment of the given determination item as a formatted string
}
function TDeterminationList.GetCommentString(
  ADeterminationItem: TDeterminationItem): String;
begin
  if ADeterminationItem.Comment.Size = 0 then
    Result := 'Null'
  else
  begin
    // Transfer content of Comment memory stream
    ADeterminationItem.Comment.Position := 0;
    SetLength(Result, ADeterminationItem.Comment.Size);
    ADeterminationItem.Comment.Read(Result[1], ADeterminationItem.Comment.Size);
    // #0 marks the end.
    if Pos(#0, Result) > 0 then SetLength(Result, Pos(#0, Result) - 1);
    // Get rid of CR/LF
    Result := QuotedStr(StringReplace(Result, #13#10, '', [rfReplaceAll]));
  end;
end;

//------------------------------------------------------------------------------
procedure TDeterminationList.ProcessUpdate;
var qryDel:TJnccQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    if Taxon then
      DeleteFromTable(qryDel, 'Taxon_Determination', 'Taxon_Determination_Key')
    else
      DeleteFromTable(qryDel, 'Biotope_Determination', 'Biotope_Determination_key');
  finally
    qryDel.Free;
  end;
end;  // ProcessUpdate

//==============================================================================
constructor TDeterminationList.Create(iDataSet: TDataset;
  const iKeyFieldName: string; iStringGrid: TStringGrid;
  iItemClass: TItemClass);
begin
  inherited;
  FOverrideCustodyControl := True;
end;

end.
