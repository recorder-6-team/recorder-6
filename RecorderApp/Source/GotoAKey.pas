unit GotoAKey;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImageListButton, StdCtrls, ExtCtrls, ComObj,Recorder2000_TLB;

resourcestring
  Restr_NotValidKey = 'Not a valid key';

type
  TdlgGotoKey = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edKey: TEdit;
    cmbTable: TComboBox;
    cbExpand: TCheckBox;
    Bevel1: TBevel;
    btnAction: TButton;
    bbCancel: TImageListButton;
    procedure bbCancelClick(Sender: TObject);
    procedure btnActionClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbExpandClick(Sender: TObject);
    procedure edKeyDblClick(Sender: TObject);
  private
    { MY Private declarations }
    FSQL:  widestring;
    FDataType: string;
    FExpand : wordbool;
    FIsExternalRef  : Boolean;
    Function RowNotExists(NewQuery : string   ) : boolean;
  public
    { Public declarations }
  end;

var
  dlgGotoKey: TdlgGotoKey;

implementation
  uses DatabaseAccessADO, ADODB, Maintbar, GeneralFunctions,GeneralData;

{$R *.dfm}

procedure TdlgGotoKey.bbCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TdlgGotoKey.btnActionClick(Sender: TObject);
var lFullSQL :string;
begin
  FIsExternalRef := false;
  FExpand := false;
  edKey.MaxLength := 16;
  With cmbTable do
    Case ItemIndex of
    0: begin
         FSQL  := 'SELECT SURVEY_KEY FROM SURVEY WHERE SURVEY_KEY = ';
         FDataType  := 'Survey';
       end;
    1: begin
         FSQL  := 'SELECT SURVEY_EVENT_KEY FROM SURVEY_EVENT WHERE SURVEY_EVENT_KEY = ';
         FDataType   := 'Event';
       end;
    2: begin
         FSQL  := 'SELECT SAMPLE_KEY FROM SAMPLE WHERE SAMPLE_KEY = ';
         FDataType   := 'Sample';
       end;
    3: begin
         FSQL :=  'SELECT TAXON_OCCURRENCE_KEY FROM TAXON_OCCURRENCE  WHERE TAXON_OCCURRENCE_KEY  = ';
         FDataType   := 'Taxon_Occurrence';
       end;
    4: begin
         FSQL :=  'SELECT BIOTOPE_OCCURRENCE_KEY FROM BIOTOPE_OCCURRENCE  WHERE BIOTOPE_OCCURRENCE_KEY  = ';
         FDataType   := 'Biotope_Occurrence';
       end;
    5: begin
         FSQL :=  'SELECT LOCATION_KEY FROM LOCATION  WHERE LOCATION_KEY  = ';
         FDataType   := 'Location';
       end;
    6: begin
         FSQL :=  'SELECT LOCATION_FEATURE_KEY FROM LOCATION_FEATURE WHERE LOCATION_FEATURE_KEY   = ';
         FDataType   := 'Feature';
       end;
    7: begin
         FSQL :=  'SELECT NAME_KEY FROM NAME WHERE NAME_KEY  = ';
         FDataType   := 'Name';
       end;
    8: begin
         FSQL :=  'SELECT SOURCE_KEY FROM REFERENCE WHERE SOURCE_KEY   = ';
         FDataType   := 'Document';
       end;
    9: begin
         FSQL :=  'SELECT ITN.TAXON_LIST_ITEM_KEY AS TAXON_LIST_ITEM_KEY ' +
           ' FROM INDEX_TAXON_NAME ITN  WHERE ' +
           ' ITN.PREFERRED_TAXA = 1 AND ITN.TAXON_VERSION_KEY = ';
         FDataType   := 'Taxon';
       end;
    10: begin
          FSQL :=  'SELECT TAXON_LIST_ITEM_KEY FROM TAXON_LIST_ITEM WHERE TAXON_LIST_ITEM_KEY   = ';
          FDataType   := 'Taxon';
        end;
     11: begin
           FSQL :=  'SELECT BIOTOPE_LIST_ITEM_KEY FROM BIOTOPE_LIST_ITEM WHERE BIOTOPE_LIST_ITEM_KEY   = ';
           FDataType   := 'Biotope';
         end;
     12:  begin
            FSQL :=  'SELECT ADMIN_AREA_KEY FROM ADMIN_AREA WHERE ADMIN_AREA_KEY  = ';
            FDataType   := 'Admin';;
           end;
     13:  begin
            FSQL := ' Select min(TPD.TAXON_OCCURRENCE_KEY) AS TAXON_OCCURRENCE_KEY ' +
              ' FROM Taxon_Private_Data TPD ' +
              ' WHERE TPD.TAXON_PRIVATE_TYPE_KEY = ''R6TEAM1800000001''  AND ' +
              ' TPD.ITEM_NAME + '' '' + TPD.Detail  = ';
            FDataType   := 'Taxon_Occurrence';;
            FIsExternalRef := true;
            edKey.MaxLength := 100;
          end;
   end; {case}
   if (length(edKey.Text) = 16) OR ((FIsExternalRef = true) AND (length(edKey.Text) > 1)) then
   begin
     if (pos(' ',edKey.Text) = 0) AND (FIsExternalRef) then
       lFullSQl := FSQL + 'TPD.ITEM_NAME + '' '' + ' + ''''  + edKey.Text + ''''
     else
       lFullSQl := FSQL + ''''  + edKey.Text + '''';

    If RowNotExists(lFullSQL) = false  then
    begin
      with CreateOlEobject('Recorder2000.AutoapplicationSettings') as IRecorder6 do
      displaytab(FDataType,lFullSQl,'','',FExpand,false);
      ModalResult := mrOk;
    end
    else begin
       MessageDlg  (Restr_NotValidKey,mtCustom,[mbOK],0);
       ModalResult := mrNone;
    end;
   end;
end;

procedure TdlgGotoKey.FormActivate(Sender: TObject);
begin
 If FDataType = '' then
  begin
    {Initialise values for global variables - for Survey}
    FDataType  := 'SURVEY';
    FSQL   := 'Select SURVEY_KEY FROM Survey WHERE Survey_key = ';
    FDataType   := 'Survey';
    FExpand := false;
    cmbTable.ItemIndex := 0; // set combo for Survey
    edKey.Text := ''; // blank out key
    edKey.MaxLength := 16;  
  end;
end;

procedure TdlgGotoKey.cbExpandClick(Sender: TObject);
begin
  FExpand := cbExpand.checked;
end;

 //------------------------------------------------------------------------
// Check that the row sKey exists in table sTable
// Note that the name of the Key field is put in FKeyField at
// initialisation or when the ComboBox is clicked
//------------------------------------------------------------------------
Function TdlgGotoKey.RowNotExists(NewQuery : string   ) : boolean;
var
rs : _Recordset;
begin
  Result := true;
  rs := dmDatabase.ExecuteSQL(NewQuery, true);
  if not rs.eof then
    if vartostr(rs.Fields[0].Value) <> '' then Result := false;
end;

procedure TdlgGotoKey.edKeyDblClick(Sender: TObject);
var
rs : _Recordset;
begin
  rs := dmDatabase.ExecuteSQL('Select * from Setting where [Name] = ''SiteId''', true);
  edKey.Text := vartostr(rs.Fields[1].Value);
end;

end.
