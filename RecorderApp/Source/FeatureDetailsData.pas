//==============================================================================
//  Unit:        FeatureDetailsData
//                                                     
//  Implements:  TdmFeatureDetails
//               TAimItem
//               TAimList
//               TDamageItem
//               TDamageList
//               TThreatItem
//               TThreatList
//
//  Description: Implements data access functionality for the feature details
//               screen.
//
//               TAimItem and TAimList
//               Helper classes used on the Management Aims tab of the details
//               screen.
//
//               TDamageItem and TDamageList
//               Helper classes used on the Damage Occurrences tab of the
//               details screen.
//
//               TThreatItem and TThreatList
//               Helper classes used on the Potential Threats tab of the details
//               screen.
//
//  Author:      Eric Salmon
//  Created:     3 June 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 26 $
//    $Date: 27/12/07 15:07 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit FeatureDetailsData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, Db, JNCCDatasets, DataClasses, Constants, ExceptionForm,
  ADODB, DatabaseAccessADO;

type
  EFeatureError=class(TExceptionPath);

  TdmFeatureDetails = class(TBaseDataModule)
    qryFeature: TJNCCQuery;
    dsFeature: TDataSource;
    tblType: TJNCCTable;
    dsType: TDataSource;
    qryGrading: TJNCCQuery;
    dsGrading: TDataSource;
    qryTypeLocate: TJNCCQuery;
    qryManagementAim: TJNCCQuery;
    tblThreatType: TJNCCTable;
    dsThreatType: TDataSource;
    qryThreats: TJNCCQuery;
    qryDamages: TJNCCQuery;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    function DeleteRecord(const AFeatDetKey:TKeyString): Boolean;
  end;  // TdmFeatureDetails

  //============================================================================
  TAimItem = class(TGridDataItem)
  private
    FDescription: TMemoryStream;
    FItemName: String;
    FAppraisalDate: String;
    FAgreementDate: TDateTime;
    FAuthority: TKeyString;
    procedure SetAgreementDate(const Value: TDateTime);
    procedure SetAppraisalDate(const Value: String);
    procedure SetAuthority(const Value: TKeyString);
    procedure SetItemName(const Value: String);
  protected
    procedure InitFromRecord( iDataset : TDataset ); override;
    procedure WriteToRecord( iDataset : TDataset ); override;
    function GetCell( const iX : integer ): string; override;
  public
    constructor CreateNew(AOwner:TCacheDataList); override;
    property ItemName: String read FItemName write SetItemName;
    property Authority: TKeyString read FAuthority write SetAuthority;
    property Description: TMemoryStream read FDescription;
    property AppraisalDate: String read FAppraisalDate write SetAppraisalDate;
    property AgreementDate: TDateTime read FAgreementDate write SetAgreementDate;
  end;  // TAimItem

  //----------------------------------------------------------------------------
  TAimList = class(TGridDataList)
  private
    FFeatureKey: TKeyString;
    procedure SetFeatureKey(const Value: TKeyString);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property FeatureKey: TKeyString read FFeatureKey write SetFeatureKey;
  end;  // TAimList

  //============================================================================
  TThreatItem = class(TGridDataItem)
  private
    FTypeName: String;
    FThreat: String;
    FTypeKey: TKeyString;
    FComments: TMemoryStream;
    procedure SetThreat(const Value: String);
    procedure SetTypeKey(const Value: TKeyString);
    procedure SetTypeName(const Value: String);
  protected
    procedure InitFromRecord( iDataset : TDataset ); override;
    procedure WriteToRecord( iDataset : TDataset ); override;
    function GetCell( const iX : integer ): string; override;
  public
    constructor CreateNew(AOwner:TCacheDataList); override;
    property TypeKey: TKeyString read FTypeKey write SetTypeKey;
    property TypeName: String read FTypeName write SetTypeName;
    property Threat: String read FThreat write SetThreat;
    property Comments: TMemoryStream read FComments;
  end;  // TThreatItem

  //----------------------------------------------------------------------------
  TThreatList = class(TGridDataList)
  private
    FFeatureKey: TKeyString;
    procedure SetFeatureKey(const Value: TKeyString);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property FeatureKey: TKeyString read FFeatureKey write SetFeatureKey;
  end;  // TThreatList

  //============================================================================
  TDamageItem = class(TGridDataItem)
  private
    FDate: String;
    FComments: TMemoryStream;
    FCommentString: String;
    procedure SetDate(const Value: String);
    procedure SetCommentString(const Value: String);
  protected
    procedure InitFromRecord( iDataset : TDataset ); override;
    procedure WriteToRecord( iDataset : TDataset ); override;
    function GetCell( const iX : integer ): string; override;
  public
    constructor CreateNew(AOwner:TCacheDataList); override;
    property Date: String read FDate write SetDate;
    property Comments: TMemoryStream read FComments;
    property CommentString: String read FCommentString write SetCommentString;
  end;  // TDamageItem

  //----------------------------------------------------------------------------
  TDamageList = class(TGridDataList)
  private
    FFeatureKey: TKeyString;
    procedure SetFeatureKey(const Value: TKeyString);
  protected
    function ItemsDisplayName: String; override;
    procedure DoAdditions;
    procedure ProcessUpdate; override;
  public
    property FeatureKey: TKeyString read FFeatureKey write SetFeatureKey;
  end;  // TDamageList

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralData, ApplicationSettings;

//==============================================================================
{ TdmFeatureDetails }

constructor TdmFeatureDetails.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
end;  // Create

//------------------------------------------------------------------------------
function TdmFeatureDetails.DeleteRecord(const AFeatDetKey: TKeyString): Boolean;
begin
  with dmGeneralData do begin
    DelSources('Location_Feature_Sources','Location_Feature_Key',AFeatDetKey);

    ExecuteSQL('DELETE FROM Management_Aim WHERE Location_Feature_Key = ''' + AFeatDetKey + '''',
               ResStr_DelFail+' - MANAGEMENT_AIM table');
    ExecuteSQL('DELETE FROM Potential_Threat WHERE Location_Feature_Key = ''' + AFeatDetKey + '''',
               ResStr_DelFail+' - POTENTIAL_THREAT table');
    ExecuteSQL('DELETE FROM Damage_Occurrence WHERE Location_Feature_Key = '''+AFeatDetKey+'''',
               ResStr_DelFail+' - DAMAGE_OCCURRENCE table');
    ExecuteSQL('DELETE FROM Location_Feature WHERE Location_Feature_Key =  '''+AFeatDetKey+'''',
               ResStr_DelFail+' - LOCATION_FEATURE table');
  end;
  Result:= True;
end;  // DeleteRecord

//==============================================================================
{ TAimItem }
constructor TAimItem.CreateNew(AOwner: TCacheDataList);
begin
  inherited CreateNew(AOwner);
  FDescription:= TMemoryStream.Create;
end;

//------------------------------------------------------------------------------
function TAimItem.GetCell(const iX: integer): string;
begin
  case iX of
    0 : Result:= ItemName;
    1 : Result:= DateTimeToStr(AgreementDate);
  end;
end;

//------------------------------------------------------------------------------
procedure TAimItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FItemKey  := FieldByName('MANAGEMENT_AIM_KEY').AsString;
      FItemName := FieldByName('ITEM_NAME').AsString;
      FAuthority:= FieldByName('AUTHORITY').AsString;

      //Description - RichEdit
      FDescription:= TMemoryStream.Create;
      TMemoField(FieldByName('DESCRIPTION')).SaveToStream(FDescription);

      FAppraisalDate:= FieldByName('NEXT_APPRAISAL_DATE').AsString;
      FAgreementDate:= FieldByName('AGREEMENT_DATE').AsDateTime;
    except on E:Exception do
      raise EFeatureError.Create(ResStr_InitRecFail+' - MANAGEMENT_AIM',E);
    end;
end;

//------------------------------------------------------------------------------
procedure TAimItem.SetAgreementDate(const Value: TDateTime);
begin
  FAgreementDate := Value;
  SetModified;
end;

//------------------------------------------------------------------------------
procedure TAimItem.SetAppraisalDate(const Value: String);
begin
  FAppraisalDate := Value;
  SetModified;
end;

//------------------------------------------------------------------------------
procedure TAimItem.SetAuthority(const Value: TKeyString);
begin
  FAuthority := Value;
  SetModified;
end;

//------------------------------------------------------------------------------
procedure TAimItem.SetItemName(const Value: String);
begin
  FItemName := Value;
  SetModified;
end;

//------------------------------------------------------------------------------
procedure TAimItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FieldByName('ITEM_NAME').AsString:= FItemName;
      FieldByName('AUTHORITY').AsString:= FAuthority;

      //Description - RichEdit
      FDescription.Position:=0;
      TMemoField(FieldByName('DESCRIPTION')).LoadFromStream(FDescription);

      FieldByName('NEXT_APPRAISAL_DATE').AsString:= FAppraisalDate;
      FieldByName('AGREEMENT_DATE').AsDateTime   := FAgreementDate;
      FieldByName('CHANGED_BY').AsString         := AppSettings.UserId;
      FieldByName('CHANGED_DATE').AsDateTime     := Now;
    except on E:Exception do
      raise EFeatureError.Create(ResStr_WriteRecFail+' - MANAGEMENT_AIM',E);
    end;
end;

//==============================================================================
{ TAimList }
function TAimList.ItemsDisplayName: String;
begin
  Result := ResStr_ManagementAim;
end;

//------------------------------------------------------------------------------
procedure TAimList.DoAdditions;
var lqryAddition: TJnccQuery;
    lDataItem   : TAimItem;
    i           : Integer;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT *  FROM Management_Aim WHERE Management_Aim_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do
        begin
          lDataItem:= TAimItem(ItemsToAdd[i]);
          Append;
          FieldByName('Management_Aim_Key').AsString:=
              dmGeneralData.GetNextKey('Management_Aim','Management_Aim_Key');
          FieldByName('Location_Feature_Key').AsString:= FFeatureKey;
          FieldByName('Item_Name').AsString:= lDataItem.ItemName;
          FieldByName('Authority').AsString:= lDataItem.Authority;

          //Description - RichEdit
          lDataItem.Description.Position := 0;
          TMemoField(FieldByName('Description')).LoadFromStream(lDataItem.Description);

          FieldByName('Next_Appraisal_Date').AsString:= lDataItem.AppraisalDate;
          FieldByName('Agreement_Date').AsDateTime   := lDataItem.AgreementDate;
          FieldByName('Entered_By').AsString         := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise EFeatureError.Create(Format(ResStr_AddFail, ['MANAGEMENT_AIM']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TAimList.ProcessUpdate;
var
  qryDel: TJNCCQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel,'MANAGEMENT_AIM','MANAGEMENT_AIM_KEY');
  finally
    qryDel.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TAimList.SetFeatureKey(const Value: TKeyString);
begin
  FFeatureKey := Value;
end;

//==============================================================================
{ TThreatItem }
constructor TThreatItem.CreateNew(AOwner: TCacheDataList);
begin
  inherited CreateNew(AOwner);
  FComments:= TMemoryStream.Create;
end;

//------------------------------------------------------------------------------
function TThreatItem.GetCell(const iX: integer): string;
begin
  case iX of
    0 : Result:= FTypeName;
    1 : Result:= FThreat;
  end;
end;

//------------------------------------------------------------------------------
procedure TThreatItem.InitFromRecord(iDataset: TDataset);
var lTempList: TStringList;
begin
  with iDataset do
    try
      FItemKey:= FieldByName('POTENTIAL_THREAT_KEY').AsString;
      FTypeKey:= FieldByName('THREAT_TYPE_KEY').AsString;

      //Get type string
      lTempList:= TStringList.Create;
      try
        dmGeneralData.GetRecordStrings(lTempList, 'THREAT_TYPE', FTypeKey);
        FTypeName:= lTempList.Values['SHORT_NAME'];
      finally
        lTempList.Free;
      end;

      FThreat:= FieldByName('THREAT').AsString;

      //Comment - RichEdit
      FComments:= TMemoryStream.Create;
      TMemoField(FieldByName('COMMENT')).SaveToStream(FComments);
    except on E:Exception do
      raise EFeatureError.Create(ResStr_WriteRecFail+' - POTENTIAL_THREAT',E);
    end;
end;

//------------------------------------------------------------------------------
procedure TThreatItem.SetThreat(const Value: String);
begin
  FThreat := Value;
  SetModified;
end;

//------------------------------------------------------------------------------
procedure TThreatItem.SetTypeKey(const Value: TKeyString);
begin
  FTypeKey := Value;
  SetModified;
end;

//------------------------------------------------------------------------------
procedure TThreatItem.SetTypeName(const Value: String);
begin
  FTypeName := Value;
  SetModified;
end;

//------------------------------------------------------------------------------
procedure TThreatItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FieldByName('THREAT_TYPE_KEY').AsString:= FTypeKey;
      FieldByName('THREAT').AsString:= FThreat;

      //Comments - RichEdit
      FComments.Position:=0;
      TMemoField(FieldByName('COMMENT')).LoadFromStream(FComments);

      FieldByName('CHANGED_BY').AsString:= AppSettings.UserID;
      FieldByName('CHANGED_DATE').AsDateTime:= Now;
    except on E:Exception do
      raise EFeatureError.Create(ResStr_WriteRecFail+' - POTENTIAL_THREAT',E);
    end;
end;

//==============================================================================
{ TThreatList }
function TThreatList.ItemsDisplayName: String;
begin
  Result := ResStr_PotentialThreat;
end;

//------------------------------------------------------------------------------
procedure TThreatList.DoAdditions;
var lqryAddition: TJnccQuery;
    lDataItem   : TThreatItem;
    i           : Integer;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Potential_Threat WHERE Potential_Threat_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do
        begin
          lDataItem:= TThreatItem(ItemsToAdd[i]);
          Append;
          FieldByName('Potential_threat_Key').AsString:=
              dmGeneralData.GetNextKey('Potential_Threat','Potential_Threat_Key');
          FieldByName('Threat_Type_Key').AsString:= lDataItem.TypeKey;
          FieldByName('Threat').AsString:= lDataItem.Threat;

          //Comments - RichEdit
          lDataItem.Comments.Position:=0;
          TMemoField(FieldByName('Comment')).LoadFromStream(lDataItem.Comments);

          FieldByName('Location_Feature_Key').AsString:= FeatureKey;
          FieldByName('Entered_By').AsString:= AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise EFeatureError.Create(Format(ResStr_AddFail, ['POTENTIAL_THREAT']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TThreatList.ProcessUpdate;
var
  qryDel: TJNCCQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel,'POTENTIAL_THREAT','POTENTIAL_THREAT_KEY');
  finally
    qryDel.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TThreatList.SetFeatureKey(const Value: TKeyString);
begin
  FFeatureKey := Value;
end;

//==============================================================================
{ TDamageItem }
constructor TDamageItem.CreateNew(AOwner: TCacheDataList);
begin
  inherited CreateNew(AOwner);
  FComments:= TMemoryStream.Create;
end;

//------------------------------------------------------------------------------
function TDamageItem.GetCell(const iX: integer): string;
begin
  case iX of
    0 : Result:= FDate;
    1 : Result:= FCommentString;
  end;
end;

//------------------------------------------------------------------------------
procedure TDamageItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FItemKey:= FieldByName('DAMAGE_OCCURRENCE_KEY').AsString;
      FDate:= FieldByName('VAGUE_DATE_START').Text;

      //Comment - RichEdit
      FComments:= TMemoryStream.Create;
      TMemoField(FieldByName('COMMENT')).SaveToStream(FComments);
      FCommentString:= dmGeneralData.ConvertRtfFieldToText(FieldByName('COMMENT'));
    except on E:Exception do
      raise EFeatureError.Create(ResStr_InitRecFail+' - DAMAGE_OCCURRENCE',E);
    end;
end;

//------------------------------------------------------------------------------
procedure TDamageItem.SetCommentString(const Value: String);
begin
  FCommentString := Value;
  SetModified;
end;

//------------------------------------------------------------------------------
procedure TDamageItem.SetDate(const Value: String);
begin
  FDate := Value;
  SetModified;
end;

//------------------------------------------------------------------------------
procedure TDamageItem.WriteToRecord(iDataset: TDataset);
begin
  with iDataset do
    try
      FieldByName('VAGUE_DATE_START').Text:= FDate;

      //Comments - RichEdit
      FComments.Position:=0;
      TMemoField(FieldByName('COMMENT')).LoadFromStream(FComments);

      FieldByName('CHANGED_BY').AsString:= AppSettings.UserID;
      FieldByName('CHANGED_DATE').AsDateTime:= Now;
    except on E:Exception do
      raise EFeatureError.Create(ResStr_WriteRecFail+' - DAMAGE_OCCURRENCE',E);
    end;
end;

//==============================================================================
{ TDamageList }
function TDamageList.ItemsDisplayName: String; 
begin
  Result := ResStr_DamageOccurence;
end;

//------------------------------------------------------------------------------
procedure TDamageList.DoAdditions;
var lqryAddition: TJnccQuery;
    lDataItem   : TDamageItem;
    i           : Integer;
begin
  lqryAddition := TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([lqryAddition]);
    with lqryAddition do
      try
        SQL.Text := 'SELECT * FROM Damage_Occurrence WHERE Damage_Occurrence_Key = ''''';
        Open;
        for i := 0 to ItemsToAdd.Count - 1 do
        begin
          lDataItem:= TDamageItem(ItemsToAdd[i]);
          Append;
          FieldByName('Damage_Occurrence_Key').AsString :=
              dmGeneralData.GetNextKey('Damage_Occurrence','Damage_Occurrence_Key');
          FieldByName('Vague_Date_Start').Text:= lDataItem.Date;

          //Comments - RichEdit
          lDataItem.Comments.Position:=0;
          TMemoField(FieldByName('Comment')).LoadFromStream(lDataItem.Comments);

          FieldByName('Location_Feature_Key').AsString := FeatureKey;
          FieldByName('Entered_By').AsString := AppSettings.UserID;
          Post;
        end;
        Close;
      except on E:Exception do
        raise EFeatureError.Create(Format(ResStr_AddFail, ['DAMAGE_OCCURRENCE']), E);
      end;
  finally
    lqryAddition.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TDamageList.ProcessUpdate;
var
  qryDel: TJNCCQuery;
begin
  DoAdditions;
  DoModifications;
  qryDel:=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryDel]);
    DeleteFromTable(qryDel,'DAMAGE_OCCURRENCE','DAMAGE_OCCURRENCE_KEY');
  finally
    qryDel.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TDamageList.SetFeatureKey(const Value: TKeyString);
begin
  FFeatureKey := Value;
end;

//==============================================================================
end.
 