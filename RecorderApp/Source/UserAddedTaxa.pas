unit UserAddedTaxa;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,Dataclasses, ApplicationSettings,Constants;

resourcestring
   ResStr_No_User_Added      = 'There are no user added taxa in the system.';
   ResStr_Accept_Update      = '%s (%s) will be replaced by %s (%s). Continue ?';
   ResStr_Updated            = '%s has been replaced by %s.';
   ResStr_Not_Updated        = 'No changes have been made ';
   ResStr_Failed_Update      = 'Update failed. No changes have been made';
   ResStr_Nothing_To_Update  = 'No replacement selected - no changes have been made ';
   ResStr_Attempted_Auto     = 'Auto Match found %s matche(s). %s were accepted ';
   ResStr_Comment            = 'Originally recorded as user added taxa - ';
   ResStr_Species            = '%s. ' + #13#10;
   ResStr_Taxon_Private_Type = 'R6TEAM1800000003';

const
  SQL_GET_LISTS =
      'Select DISTINCT TL.Taxon_List_Key,TL.Item_Name from ' +
      'Taxon_List TL INNER JOIN Taxon_List_Version TLV ' +
      'ON TLV.Taxon_LIst_Key = TL.Taxon_List_Key ' +
      'INNER JOIN Taxon_List_Item  TLI ' +
      'ON TLI.Taxon_list_Version_Key = TLV.Taxon_List_Version_Key ' +
      'WHERE TLI.SYSTEM_SUPPLIED_DATA = 0 ';
  SQL_GET_TAXA =
      'SELECT DISTINCT [dbo].[LCPrefNameWithAttribute](TLI.TAXON_LIST_ITEM_KEY,1) + ' +
      ''' - ''' + ' + TL.ITEM_NAME AS FUll_NAME,' +
      'TLI.TAXON_LIST_ITEM_KEY ' +
      'FROM TAXON_LIST_ITEM TLI INNER JOIN TAXON_VERSION TV ' +
      'ON TLI.TAXON_VERSION_KEY = TV.TAXON_VERSION_KEY ' +
      'INNER JOIN TAXON_LIST_VERSION TLV ON ' +
      'TLV.TAXON_LIST_VERSION_KEY = TLI.TAXON_LIST_VERSION_KEY ' +
      'INNER JOIN TAXON_LIST TL ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY ' +
      'LEFT JOIN TAXON_DETERMINATION TDET ON TDET.TAXON_LIST_ITEM_KEY = ' +
      'TLI.TAXON_LIST_ITEM_KEY ' +
      'WHERE TLI.SYSTEM_SUPPLIED_DATA = 0 ';
   SQL_EXTRA_WHERE_TDET =
      'AND TDET.TAXON_LIST_ITEM_KEY IS NOT NULL ';
   SQL_EXTRA_WHERE_TL =
      'AND TLV.TAXON_LIST_KEY = ''%s'' ';
   SQL_ORDER_BY =
      'ORDER BY FULL_NAME ';
   SQL_BASIC_SEARCH =
      'SELECT COUNT(ITN.TAXON_LIST_ITEM_KEY) AS MATCHES FROM ' +
      'INDEX_TAXON_NAME ITN ' +
      'INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ' +
      'ITN.TAXON_LIST_ITEM_KEY AND TLI.SYSTEM_SUPPLIED_DATA = 1 ' +
      'WHERE ITN.PREFERRED_TAXA = 1 AND ITN.OUTPUT_TAXON_NAME = ''%s''';
   SQL_GET_TAXON_NAME =
      'SELECT OUTPUT_TAXON_NAME,TAXON_LIST_ITEM_KEY FROM INDEX_TAXON_NAME ITN ' +
      'WHERE ITN.TAXON_LIST_ITEM_KEY = ''%s''';
   SQL_GET_NEW_TAXON =
      'SELECT ITN.TAXON_LIST_ITEM_KEY, ' +
      '[dbo].[LCPrefNameWithAttribute](ITN.TAXON_LIST_ITEM_KEY,1) AS FULL_NAME FROM ' +
      'INDEX_TAXON_NAME ITN ' +
      'INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = ' +
      'ITN.TAXON_LIST_ITEM_KEY AND TLI.SYSTEM_SUPPLIED_DATA = 1 ' +
      'WHERE ITN.PREFERRED_TAXA = 1 AND ITN.OUTPUT_TAXON_NAME = ''%s''';
    SQL_GET_NEW_FULL_NAME =
       'SELECT ITN.TAXON_LIST_ITEM_KEY, ' +
       '[dbo].[LCPrefNameWithAttribute](ITN.TAXON_LIST_ITEM_KEY,1) AS FULL_NAME FROM ' +
       'INDEX_TAXON_NAME ITN ' +
       'WHERE ITN.TAXON_LIST_ITEM_KEY = ''%s''';
    SQL_UPDATE_TDET =
       'UPDATE TAXON_DETERMINATION SET TAXON_LIST_ITEM_KEY = ''%s'',' +
       'CHANGED_DATE = Getdate(), ' +
       'COMMENT = ''%s %s. '' +  [dbo].[ufn_RtfToPlaintext](COMMENT) ' +
       'WHERE TAXON_LIST_ITEM_KEY = ''%s''';

  type TdlgUserAddedTaxa = class(TForm)
    rgProcess: TRadioGroup;
    btnAction: TButton;
    btnCancel: TButton;
    cbUsedOnly: TCheckBox;
    cmbLists: TComboBox;
    lstTaxa: TListBox;
    cbAuto: TCheckBox;
    lblListName: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbUsedOnlyClick(Sender: TObject);
    procedure btnActionClick(Sender: TObject);
    procedure cmbListsClick(Sender: TObject);
  private
    { Private declarations }
    fChangedRecords: integer;
    function ClearcmbLists: integer;
    function ClearlstTaxa: integer;
    function PopulateLstTaxa(AKey:string):integer;
    function PopulatecmbLists: integer;
    function ConfirmAcceptance(AKey, ANewKey,ATaxon,ANewTaxon: string ):integer;
    function PerformFind(ATaxa,Akey :string): string;
    function UpdateTaxonDetermination(AKey,ANewKey,ATaxa: string):boolean;
    function GetNewTaxonName(ANewKey :string): string;
    Procedure GetNewTaxonDetails(Var ANewKey,ANewName: string; ASearchName:string);
  public
    { Public declarations }
  end;

var
  dlgUserAddedTaxa: TdlgUserAddedTaxa;

implementation
  uses DatabaseAccessADO, ADODB, GeneralFunctions,GeneralData,Find;

{$R *.dfm}

procedure TdlgUserAddedTaxa.btnCancelClick(Sender: TObject);
begin
 modalresult := mrCancel;
end;

function TdlgUserAddedTaxa.PopulateLstTaxa(AKey:string):integer;
var
  rs: _Recordset;
  lKey: TKey;
  lExtraWhere: string;
begin
  Screen.Cursor := crHourGlass;
  ClearLstTaxa;
  lExtraWhere := '';
  if cbUsedOnly.state = cbChecked then
    lExtraWhere := SQL_EXTRA_WHERE_TDET;
  if AKey <> 'All' then
    lExtraWhere := lExtraWhere + Format(SQL_EXTRA_WHERE_TL,[Akey]);
  lExtraWhere := lExtraWhere + SQL_ORDER_BY;
  rs := dmDatabase.ExecuteSQL(SQL_GET_TAXA + lExtraWhere,true);
  while not rs.eof do begin
    lKey := TKey.Create;
    lKey.Key :=  rs.Fields['Taxon_List_Item_Key'].Value;
    lstTaxa.Items.AddObject(rs.Fields['FUll_NAME'].Value,lkey);
    rs.MoveNext;
  end;
  if lstTaxa.Items.count > 0 then lstTaxa.ItemIndex := 0;
  Screen.Cursor := crdefault;
  Result := lstTaxa.Count;
end;

function TdlgUserAddedTaxa.ClearLstTaxa: integer;

begin
  lstTaxa.Clear;
  Result := lstTaxa.count;
end;

function TdlgUserAddedTaxa.PopulatecmbLists(): integer;
var
  rs: _Recordset;
  lKey: TKey;
begin
  ClearcmbLists;
  lKey := TKey.Create;
  lKey.Key :=  'All';
  cmbLists.Items.AddObject('All Lists', lkey);
  rs := dmDatabase.ExecuteSQL(SQL_GET_LISTS,true);
  while not rs.eof do begin
    lKey := TKey.Create;
    lKey.Key :=  rs.Fields['Taxon_List_Key'].Value;
    cmbLists.Items.AddObject(rs.Fields['Item_Name'].Value,lKey);
    rs.MoveNext;
  end;
  cmbLists.ItemIndex := -1;
  if cmbLists.items.count = 1 then
    ClearcmbLists;
  Result := cmbLists.Items.count;
end;

function TdlgUserAddedTaxa.ClearcmbLists: integer;
begin
  cmbLists.Clear;
  Result := cmbLists.Items.count;
end;
procedure TdlgUserAddedTaxa.FormActivate(Sender: TObject);
begin
  if PopulatecmbLists() = 0 then begin
    MessageDlg(ResStr_No_User_Added, mtInformation, [mbOk], 0);
    modalresult := mrCancel;
   end;
end;

procedure TdlgUserAddedTaxa.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
begin
  i := 0;
  for i := 0 to cmbLists.Items.Count - 1 do begin
    cmbLists.Items.Objects[i].Free;
  end;
  for i := 0 to lstTaxa.Items.Count - 1 do
    lstTaxa.Items.Objects[i].Free;
end;

procedure TdlgUserAddedTaxa.cbUsedOnlyClick(Sender: TObject);
begin
  lstTaxa.Clear;
  if cmbLists.ItemIndex > -1 then
    cmbListsClick(Sender);
end;

procedure TdlgUserAddedTaxa.btnActionClick(Sender: TObject);
var
  i: integer;
  rs: _recordset;
  lkey: Tkey;
  lSearchName: string;
  lFoundTaxonkey: string;
  lMatchCount: integer;
  lAttempted: integer;
  lNewTaxonKey: string;
  lNewTaxonName: string;
begin
  lSearchName := '';
  for i := 0 to lstTaxa.Items.Count - 1 do begin
    if (lstTaxa.Selected[i])or (rgProcess.ItemIndex = 1) then begin
      lkey := lstTaxa.Items.Objects[i] as TKey;
      rs := dmDatabase.ExecuteSQL(Format(SQL_GET_TAXON_NAME,[lkey.key]),true);
      if not rs.eof then
        lSearchName := rs.Fields['OUTPUT_TAXON_NAME'].value;
      if cbAuto.state = cbChecked then begin
        rs := dmDatabase.ExecuteSQL(Format(SQL_BASIC_SEARCH,[lSearchName]),true);
        if not rs.eof then begin
          lMatchCount := rs.Fields['MATCHES'].Value;
          if lMatchCount = 1 then begin
            GetNewTaxonDetails(lNewTaxonKey, lNewTaxonName,lSearchName);
            lAttempted := lAttempted + ConfirmAcceptance(lkey.key,lNewTaxonKey,
                lSearchName,lNewTaxonName);
          end else
          begin
            if lMatchCount > 1 then begin
              lFoundTaxonKey := PerformFind(lSearchName,lkey.key);
              lAttempted := lAttempted + ConfirmAcceptance(lkey.key,lFoundTaxonKey,
                  lSearchName,GetNewTaxonName(lFoundTaxonKey));
            end;
          end;
        end; //end of eof
      end else begin
        // auto is not set
        lFoundTaxonKey := PerformFind(lSearchName,lkey.key);
        lAttempted := lAttempted +  ConfirmAcceptance(lkey.key,lFoundTaxonKey,lSearchName,GetNewTaxonName(lFoundTaxonKey))
      end;
    end;//end of one selected
  end; //end of for do
  if cbAuto.Checked then
    MessageDlg(Format(ResStr_Attempted_Auto,[inttostr(lAttempted),inttostr(FChangedRecords)]), mtInformation, [mbOK], 0)
end;

function TdlgUserAddedTaxa.ConfirmAcceptance(AKey, ANewKey,ATaxon,ANewTaxon: string): integer;
var
  lMessage: string;
begin
  Result:=0;
  if ANewKey <> '' then begin
    lMessage := Format(Resstr_Species,[ATaxon]);
    Result:= 1;
    if MessageDlg(Format(ResStr_Accept_Update,[ATaxon,Akey,ANewTaxon,ANewKey]),mtInformation, [mbYes, mbNo], 0) = mrYes then begin
      if UpdateTaxonDetermination(AKey, ANewKey,ATaxon) then begin
        FChangedRecords := FChangedRecords + 1;
        MessageDlg(lMessage + Format(ResStr_Updated,[AKey,ANewKey]),mtInformation,[mbOK], 0);
      end else
        MessageDlg(lMessage + ResStr_Failed_Update, mtInformation, [mbOK], 0)
    end else
      MessageDlg(lMessage + ResStr_Not_Updated, mtInformation, [mbOK], 0);
  end else
     MessageDlg(ResStr_Nothing_To_Update, mtInformation, [mbOK], 0);
end;

function TdlgUserAddedTaxa.PerformFind(ATaxa, AKey: string): string;
var ldlgFind: TdlgFind;
begin
  //Finds are best done on preferred taxa.
  AppSettings.SessionTaxonomicSearchRestriction := ResStr_Preferred_Taxa;
  ldlgFind := TdlgFind.CreateDialog(nil, True, 'Find Species', ftTaxon);
  with ldlgFind do begin
    try
      if ATaxa<> '' then StoreSearchText(ATaxa)
                          else StoreSearchText('');
      // If there is a unique item, get it, otherwise, we have to show the dialog
      if (FindUnique(ATaxa)) AND (Akey <> ItemKey) then
        Result := ItemKey
      else
      if ShowModal = mrOK then Result := ItemKey
                          else Result := '';
    finally
      Release;
    end;
  end; // with lFind;
end;

function TdlgUserAddedTaxa.UpdateTaxonDetermination(AKey, ANewKey,
  ATaxa: string): boolean;
begin
  Result := true;
  Try
    dmDatabase.ExecuteSQL(Format(SQL_UPDATE_TDET,[ANewKey,Resstr_Comment,ATaxa,AKey]),true);
  except
    Result:= false;
  end;
end;

function TdlgUserAddedTaxa.GetNewTaxonName(ANewKey: string): string;
var
  rs: _Recordset;
begin
  Result:= '';
  rs := dmDatabase.ExecuteSQL(Format(SQL_GET_NEW_FULL_NAME,[ANewKey]),true);
  if not rs.eof then
    Result:=  rs.Fields['FULL_NAME'].Value;
end;

procedure TdlgUserAddedTaxa.GetNewTaxonDetails(var ANewKey,
  ANewName: string; ASearchName: string);
var
  rs: _Recordset;
begin
  ANewName := '';
  ANewKey := '';
  rs := dmDatabase.ExecuteSQL(Format(SQL_GET_NEW_TAXON,[ASearchName]),true);
  if not rs.eof then begin
    ANewName :=  rs.Fields['FULL_NAME'].Value;
    AnewKey :=  rs.Fields['TAXON_LIST_ITEM_KEY'].Value;
  end;
end;

procedure TdlgUserAddedTaxa.cmbListsClick(Sender: TObject);
var
  lkey: Tkey;
begin
  lkey := cmbLists.Items.Objects[cmbLists.itemindex] as TKey;
  PopulatelstTaxa(lKey.key);
end;

end.
