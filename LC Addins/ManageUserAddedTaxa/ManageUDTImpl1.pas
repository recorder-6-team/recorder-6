unit ManageUDTImpl1;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, ProjectManageUserTaxa_TLB, StdVcl, StdCtrls,Recorder2000_TLB,Variants,
  ExtCtrls,strUtils,ADODB;


type

  TManageUDT = class(TActiveForm, IManageUDT,IRecorderAddin,INewAction, IRequestor,IKeyListSupplier)

    ListBox1: TListBox;
    Button2: TButton;
    Button3: TButton;
    Shape1: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Button1: TButton;
    Shape2: TShape;
    Label5: TLabel;
    Button4: TButton;
    Button5: TButton;
    ListBox2: TListBox;
    Label9: TLabel;
    Shape3: TShape;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Shape4: TShape;
    Button6: TButton;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Label12: TLabel;
    Label13: TLabel;
    Shape5: TShape;
    Label14: TLabel;
    Edit1: TEdit;
    Label15: TLabel;
    ComboBox1: TComboBox;
    Label16: TLabel;
    Button10: TButton;
    Edit2: TEdit;
    Label17: TLabel;
    procedure ActiveFormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ActiveFormCreate(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);

  private
   { My declarations }
    FkeyList : IKeyList;
    FEvents: IManageUDTEvents;
    FCurrentAction : integer;
    FUserId : widestring;
    FSiteId : string;
    // Flist is the info from the currently selected item on the list
    FListTaxonRank : string;
    FListTVKey : string;
    FListTaxonName :string ;
    FListOrganismKey :string;
    FListParentOrganismKey :string;
    //The taxon list item key of the parent organism
    FListParentOrgTLIKey :string;
    FListParentName : string ;
    FListOrganismRedundantFlag : string;
    FListOrganismLevel :integer;
    FListOrganismLineage:  string;
    FListOutputGroupKey  :string;
    FListDictionary  :string;
    FListTLIKey : string;
    FListTaxonListKey :string;
    FListRecords : integer;
    FConn: TADOConnection;
  //  FRecorder: IRecorder2000;

    //FR6 is the info from the currently selected item from the Dictionary

    FR6TaxonRank : string;
    FR6TVKey : string;
    FR6TaxonName :string;
    FR6OrganismKey :string;
    FR6ParentOrganismKey :string;
    FR6ParentOrgTLIKey :string;
    FR6ParentName : string ;
    FR6OrganismRedundantFlag : string;
    FR6OrganismLevel :integer;
    FR6OrganismLineage:  string;
    FR6OutputGroupKey : string;
    FR6Dictionary  :string;
    FR6TLIKey : string;
    FR6TaxonListKey :string;
    FR6Records : integer;
    { Private declarations }
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);

    { My declarations }
    function PopulateListBox1(itemindex : integer) : integer;
    function ClearListBox1() : integer;
    procedure PopulateInfoOnTaxa(R6Key, TableShortName,Populate:  string);
    function Get_Current_TV_Key () : string;
    function Execute_SQL(SQLString:widestring ) : integer;
    function Populate_Organism_Parent() :integer;
    function Populate_Lineage_Children(StartOrganismKey, StartParentKey, ParentLineage : string;  ParentLevel: integer) :integer;
    function Update_Info() :integer;
    function CheckProposedParent(): string;
    function IncrementKey(LastKey : string): string;
    Function GenerateSortOrder(NewLineage : string) : string;
    Function CheckUserId () : integer;
    Function CheckTVK(CurrentTVK :string) : boolean;
    function AutoProcess(R6Key : string) : integer;
    function WorkOutGenus(Taxon :string) : string;
    function ConnectToR6 : string;
    procedure ProcessChangedOrganism(CurrentKey : string);
    procedure ProcessReplaceTaxa(CurrentKey : string );
    function Delete_Taxon(CurrentKey : string ): boolean;
    function AutoReplaced(TLIkey : string ) : integer;
    function RePopulateBox1(): integer;
    function UpdateSingleITH(OrganismKey,ParentOrganismKey: string): integer;
    function UpdateTaxonDetermination (TLIKeyOld,TLIKeyNew : string) : boolean;
    function AddNewTaxon(CurrentKey : string) : boolean;
    function GenerateAbbreviation( ItemName : string ) :string;
    function Populate_Index_Taxon_Group : integer;

    protected

    { Protected declarations }
     function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function DoOk: WordBool; safecall;
    procedure DoCancel; safecall;
    function Get_ItemCount: Integer; safecall;
    function Get_TableName: WideString; safecall;
    function GetKeyItem(iIndex: Integer): IKeyItem; safecall;
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_KeyList: IKeyList; safecall;
    function Get_Exportable: WordBool; safecall;

    {IRecorderAddin}
    procedure Update(const KeyList: IKeyList); safecall;


  public
    { Public declarations }
    procedure Initialize; override;
    destructor destroy; override;
  end;

implementation

uses ComObj, ComServ;

{$R *.DFM}

{ TManageUDT }
const
     CRLF = chr(13) + chr(10);
     Button8Message = 'Works on all taxa listed. ' +
     ' Deletes the dictionary entries for taxa which are not used in records ';
     Button10Message = 'Taxon will be added to the "A List  to hold User  Added  Taxa". Continue to select a ' +
     ' parent Organism for the taxa. Any list may be used. ';
     NewTaxonMessage = 'To create the new taxon click OK, and then choose a parent for the Organism';
procedure TManageUDT.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_ManageUDTPage); }
end;

procedure TManageUDT.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IManageUDTEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TManageUDT.Initialize;
begin
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnPaint := PaintEvent;
end;

function TManageUDT.Get_Active: WordBool;
begin
  Result := Active;
end;

function TManageUDT.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TManageUDT.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TManageUDT.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TManageUDT.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TManageUDT.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TManageUDT.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TManageUDT.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TManageUDT.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TManageUDT.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TManageUDT.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TManageUDT.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TManageUDT.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TManageUDT.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TManageUDT.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TManageUDT.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TManageUDT.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TManageUDT.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TManageUDT.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TManageUDT.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TManageUDT._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TManageUDT.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TManageUDT.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TManageUDT.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TManageUDT.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TManageUDT.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TManageUDT.DestroyEvent(Sender: TObject);
begin

  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TManageUDT.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TManageUDT.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TManageUDT.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TManageUDT.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TManageUDT.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TManageUDT.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TManageUDT.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TManageUDT.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TManageUDT.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TManageUDT.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TManageUDT.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TManageUDT.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TManageUDT.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TManageUDT.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TManageUDT.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TManageUDT.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TManageUDT.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TManageUDT.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TManageUDT.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

function TManageUDT.Get_Description: WideString;
begin
  result:='Version 6.26';
end;

function TManageUDT.Get_ImageFileName: WideString;
begin
  result:='default.bmp';
end;

function TManageUDT.Get_Name: WideString;
begin
  result:='Manage User Added Taxa v6.22';
end;

procedure TManageUDT.Install(const iInstalledFilePath: WideString);
begin

end;

function TManageUDT.Get_ActionCaption: WideString;
begin
  result:= 'Manage User Added Taxa' ;
end;

function TManageUDT.Get_CanAddToToolbar: WordBool;
begin
   result:= false;
end;

function TManageUDT.Get_DimmedImageFilename: WideString;
begin
      result:= 'default.bmp';
end;

function TManageUDT.Get_DisabledImageFileName: WideString;
begin
      result:= 'default.bmp';
end;

function TManageUDT.Get_Hint: WideString;
begin
      result:= 'Manage the taxa you have added';
end;

function TManageUDT.Get_ParentMenu: WideString;
begin
     result:= 'Dictionaries';
end;



function TManageUDT.PopulateListBox1(itemindex : integer): integer;
var

  IResult: _Recordset;

  ExtraWhere : string;

begin
   Screen.Cursor := crHourGlass;
   ClearListBox1;

   if itemindex = 1  then ExtraWhere := ' TLI.Taxon_List_version_Key = ''LC00000100000001'' AND ';
   if itemindex = 2  then ExtraWhere := ' Org.Organism_Key is  null AND  ';
    if itemindex = 3  then ExtraWhere := ' Org.Organism_Key is not null AND  ';
   IResult := FConn.Execute('Select TV.Taxon_Version_Key,Taxon.Item_Name FROM  Taxon '  +
        ' INNER JOIN Taxon_Version TV ON TV.Taxon_Key = Taxon.Taxon_Key ' +
        ' LEFT JOIN Organism Org On Org.Organism_Key = TV.Taxon_Version_Key ' +
        ' INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_version_Key = TV.Taxon_version_Key ' +
        ' WHERE ' + ExtraWhere +  ' TV.System_supplied_data = 0 AND TV.Custodian =  ''' + FSiteId + ''' Order BY Item_Name');

   WHILE IResult.eof = false DO

   BEGIN
       ListBox1.items.add(vartostr(Iresult.Fields.Item[0].value) +  '        '  +  vartostr(Iresult.Fields.Item[1].value));
       Iresult.MoveNext
   END;

   Screen.Cursor := crdefault;
   Result := ListBox1.Count;

end;

function TManageUDT.ClearListBox1: integer;

begin
  listbox1.Clear;
  label2.Caption := '';
  label4.Caption := 'Not Available' ;
  label3.Caption := '';
  label6.Caption := '';
  label11.Caption := '';
  Result := ListBox1.count;
end;

procedure TManageUDT.ActiveFormDestroy(Sender: TObject);
begin
     If FConn <> nil then
     begin
          FConn.Close;
          FConn.Free;
     end;
        Screen.Cursor := crdefault;
        ClearListBox1;
        ListBox1.Free;
        ListBox2.Free;
end;


procedure TManageUDT.Button1Click(Sender: TObject);
Var
IRecorder : IRecorder2000;

begin
    
     FCurrentAction := 1;
     IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
     IRecorder.RequestData(TManageUDT(self),'Taxon');



end;


function TManageUDT.Get_ItemCount: Integer;
begin

end;

function TManageUDT.Get_TableName: WideString;
begin

end;

function TManageUDT.GetKeyItem(iIndex: Integer): IKeyItem;


begin

end;

// Given a key and the short name of the table will return various information on the key provided
// Adapt this procedure to returm additional information if required.

procedure TManageUDT.PopulateInfoOnTaxa(R6Key, TableShortName,Populate : string);

Var
  IResult: _Recordset;
  sSQL : string;
  tvKey : string;
  taxonName :string;
  taxonrank : string;
  organismkey : string;
  ParentOrganismKey : string;
  OrganismLineage : string ;
  OrganismLevel : integer;
  OrganismRedundantFlag : string ;
  ParentName : string;
  ParentTLIKey : string;
  DictionaryName : string;
  TaxonListKey : string;
  OutputGroupKey : string;
  TLIKey: string;
  NoOfDets : integer;
begin
   NoOfDets := 0;
   OrganismLevel := 0;
   // Converts the key into a Taxon_Version Key then gets the information in one query
   // option are TV,TLI,ORG - add others if required
   if  TableShortName = 'TV' then
       TVkey := R6Key
   else
   Begin
       if TableShortName = 'TLI' then
          sSQL := 'Select Taxon_Version_Key from Taxon_List_Item ' +
          ' WHERE Taxon_List_Item_Key = ''' + R6Key + ''''
       Else
          sSQL := 'Select Taxon_Version_Key from Organism  ' +
          ' WHERE Organism_Key  = ''' + R6Key + '''';
       IResult :=   FConn.Execute (sSQL);
       TVkey := Iresult.Fields.Item[0].value;

    End;

   if tvKey <> '' then
   begin
      // gets the details for the TV
      sSQl  := 'Select T.Item_Name,TLI.Taxon_Rank_Key,TL.Item_Name, TV.Output_Group_Key,TLI.Taxon_List_Item_Key, TL.Taxon_List_Key From Taxon T Inner Join Taxon_Version TV '  +
      'ON TV.Taxon_Key = T.taxon_Key INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_Version_Key = TV.Taxon_Version_Key ' +
      ' INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key ' +
      ' INNER JOIN Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
      ' WHERE  TV.Taxon_Version_Key = ''' + TVkey + ''' ';
       IResult :=   FConn.Execute (sSQL);
       if IResult.EOF = false then
       begin
           TaxonName  :=   Iresult.Fields.Item[0].value;
           TaxonRank := vartostr(Iresult.Fields.Item[1].value);
           DictionaryName :=  vartostr(Iresult.Fields.Item[2].value);
           OutPutGroupKey  :=  vartostr(Iresult.Fields.Item[3].value);
           TLIKey :=  vartostr(Iresult.Fields.Item[4].value);
           TaxonListKey :=  vartostr(Iresult.Fields.Item[5].value);
       end;
       ssQL := 'Select count(*) From Taxon_Determination WHERE Taxon_List_Item_Key = ''' + TLIKey + '''';
       IResult :=   FConn.Execute (sSQL);
       NoOfDets := Iresult.Fields[0].Value;

       // gets the details for the Organism key
       sSql := 'Select ORG.Organism_Key, Org.Parent_Key, Org.Lineage, Org.Sort_Level,Org.Redundant_Flag' +
      ' FROM Taxon_Version TV ' +
      ' INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_Version_Key = TV.Taxon_Version_Key '    +
      ' INNER JOIN Index_Taxon_Name ITN ON ITN.TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY ' +
      ' INNER JOIN Taxon_List_Item TLI2 ON TLI2.Taxon_List_Item_Key = ITN.Recommended_Taxon_List_Item_Key ' +
      ' INNER JOIN Organism Org ON Org.Taxon_Version_Key = TLI2.Taxon_Version_Key ' +
      ' WHERE  TV.Taxon_Version_Key = ''' + TVkey + ''' ';

       IResult :=   FConn.Execute (sSQL);
       if IResult.EOF = false then
       begin
          OrganismKey  :=   vartostr(Iresult.Fields.Item[0].value);
          ParentOrganismKey :=      vartostr(Iresult.Fields.Item[1].value);
          OrganismLineage :=   vartostr(Iresult.Fields.Item[2].value);
          OrganismLevel :=   Iresult.Fields.Item[3].value;
          OrganismRedundantFlag :=   vartostr(Iresult.Fields.Item[4].value);
       end;
       // The following returns details of the parent

       sSQl  := 'SELECT T.Item_Name,ITN.Taxon_LIst_ITem_Key  ' +
       ' From Organism ORG ' +
       ' INNER JOIN Taxon_Version TV ' +
       ' ON TV.Taxon_Version_Key = ORG.Taxon_Version_Key ' +
       ' INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_Version_Key = TV.Taxon_Version_Key  ' +
       ' INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key ' +
       ' = TLI.Taxon_List_Item_Key AND ITN.Taxon_LIst_ITem_Key = ITN.Recommended_Taxon_List_Item_Key ' +
       ' INNER JOIN Taxon T ON T.Taxon_Key = TV.Taxon_Key ' +
       ' WHERE  Org.Organism_Key = ''' + ParentOrganismKey + ''' ';
       IResult :=   FConn.Execute (sSQL);
       if IResult.EOF = false then
       begin
        ParentName  :=  Iresult.Fields.Item[0].value;
        ParentTLIKey  :=  Iresult.Fields.Item[1].value;
       end;
 end;
      if populate = 'FList' then
      Begin
          FListTaxonRank := TaxonRank ;
          FListTVKey := TVKey ;
          FListTaxonName := TaxonName;
          FListOrganismKey := OrganismKey;
          FListParentOrganismKey := ParentOrganismKey;
          FListParentName :=  ParentName;
          FListOrganismRedundantFlag := OrganismRedundantFlag;
          FListOrganismLevel := OrganismLevel;
          FListOrganismLineage:= OrganismLineage ;
          FListDictionary := DictionaryName;
          FListOutputGroupKey := OutputGroupKey;
          FListTLIKey := TLIKey;
          FlistRecords := NoOfDets;
          FListTaxonListKey := TaxonListKey;
          FListParentOrgTLIKey := ParentTLiKey
      end
      else
      Begin
          FR6TaxonRank := TaxonRank ;
          FR6TVKey := TVKey ;
          FR6TaxonName := TaxonName;
          FR6OrganismKey := OrganismKey;
          FR6ParentOrganismKey := ParentOrganismKey;
          FR6ParentName :=  ParentName;
          FR6OrganismRedundantFlag := OrganismRedundantFlag;
          FR6OrganismLevel := OrganismLevel;
          FR6OrganismLineage:= OrganismLineage ;
          FR6Dictionary := DictionaryName;
          FR6OutputGroupKey := OutputGroupKey;
          FR6TLIKey := TLIKey;
          FR6Records := NoOfDets;
          FR6TaxonListKey := TaxonListKey;
          FR6ParentOrgTLIKey := ParentTLiKey
      end;




end;

function TManageUDT.Get_Current_TV_Key: string;
var
 nitem :integer;
begin
    nitem:= ListBox1.ItemIndex;
    if nItem >= 0 then
      Result := ansileftstr(ListBox1.Items [nitem],16)
    else
    begin
      MessageDlg ('Please select a Taxon', MtInformation,[mbOk],0);
      Result := '';

    end;
end;

// Processes the returned taxa from R6
procedure TManageUDT.Update(const KeyList: IKeyList);

var CurrentKey :string;


begin
FKeyList := KeyList;
CurrentKey :=   FkeyList.GetKeyItem(0).KeyField1;
PopulateInfoOnTaxa(CurrentKey, 'TLI','R6' ) ;
Case FCurrenTAction of
 1 :  ProcessChangedOrganism(CurrentKey);
 2 :  ProcessReplaceTaxa(CurrentKey);
 3 :  AddNewTaxon(CurrentKey);
end;

end;

procedure TManageUDT.ListBox1Click(Sender: TObject);

begin
    // Updates the details for the selected item
    Update_Info;
end;



function TManageUDT.Execute_SQL(SQLstring: widestring): integer;


begin
  FConn.Execute(SQLString);
  Result := 1;

end;

function TManageUDT.Populate_Organism_Parent(): integer;
var
SQLString : Widestring;
begin
   if FListOrganismkey = ''  then
     Begin
      SQLString := 'INSERT INTO ORGANISM (Organism_key,Parent_Key,Taxon_Version_key, ' +
    ' Organism_Rank_Key,Entered_By,Entry_Date,System_Supplied_Data,Sort_Level) ' +
      ' Values (''' + FListTVkey + ''',''' + FR6OrganismKey + ''',''' + FListTVkey + ''',''' + FListTaxonRank + ''',''' +
      'TESTDATA00000001'',GetDate(),0 , ' + inttostr(FR6OrganismLevel+ 1) + ')';
       Execute_SQL(SQLString);
     end;

     SQLString := 'UPDATE Organism Set Sort_Level = ''' + inttostr(FR6OrganismLevel+ 1) + ''', Parent_key = ''' + FR6OrganismKey + '''' +
     ' WHERE Organism_Key = ''' + FListTVkey + '''';
      Execute_SQL(SQLString);

      SQLString := 'UPDATE Taxon_Version  Set Output_Group_Key = ''' + FR6OutputGroupKey + '''' +
     ' WHERE Taxon_Version_Key = ''' + FListTVkey + '''';
      Execute_SQL(SQLString);

     // run the option here to update the lineage from the parent down

     Populate_Lineage_Children(FListTVKey, FR6OrganismKey,  FR6OrganismLineage, FR6OrganismLevel );
     UpdateSingleITH(FListTVkey,FR6OrganismKey);

     // Update the info on the selected taxon

       // Now need to update index_taxon_Group

      //5Mar2014


      Populate_Index_Taxon_Group;


     Result:= 1;
end;

function TManageUDT.Update_Info: integer;
var
R6Key :string;

begin
    R6Key := Get_Current_TV_Key;    //tihs is the key from listbox1
    PopulateInfoOnTaxa(R6Key, 'TV', 'FList') ;
    label4.caption :=  FListParentOrganismKey;
    if FListparentname = '' then label2.caption :=  'Not allocated'
    else  label2.caption := FListParentname;
    label3.Caption := FListTaxonName;
    label6.Caption := FListDictionary;
    label11.Caption := inttostr(FListRecords);
    Result := 1;
end;

procedure TManageUDT.Button4Click(Sender: TObject);
 Var
  I: Integer;
  TVKey : string;
  Processed : integer;
begin
  Processed := 0;
  Screen.Cursor := crHourglass;
  For I := 0 to ListBox1.count -1 do
  Begin
     TVKey := ansileftstr(ListBox1.Items [I],16);
     PopulateInfoOnTaxa(TVKey, 'TV', 'FList');
     If FListParentOrganismKey = '' then
        Processed := Processed + AutoProcess(FListTVkey);
  end;
  Screen.Cursor := crdefault;
  MessageDlg ('Records Auto Processed = ' + inttostr(Processed), MtInformation,[mbOk],0);
  RePopulateBox1
end;

function TManageUDT.Populate_Lineage_Children(
  StartOrganismKey, StartParentKey,  ParentLineage : string; ParentLevel : integer ): integer;
var

IResult: _Recordset;
SQLString : Widestring;
CurrentLevel : integer;
LastKey : string;
J : integer;
NewLineage : string;
BaseLineage : string;
PrevBaseLineage : String;
UpDateSQL : String;
NewSortOrder :string;
begin


CurrentLevel := ParentLevel ;
J := 1;
While J <> 0 Do
begin
  Screen.Cursor := crHourglass;
  currentLevel := CurrentLevel + 1;
  SQLString := 'Select Org.Organism_Key,Org2.Lineage,ORG.Taxon_version_Key from Organism ORG ' +
  ' INNER JOIN ORGANISM ORG2 ON ORG2.Organism_key = ORG.Parent_Key ' +
  ' INNER JOIN Taxon_Rank TR ON TR.Taxon_rank_Key = ORG.Organism_Rank_Key ' +
  ' INNER JOIN Taxon_version TV ON TV.Taxon_Version_Key = ORG.Taxon_Version_Key ' +
  ' INNER JOIN Taxon T ON T.Taxon_Key = TV.Taxon_Key ' +
  ' WHERE ORG.Sort_Level = ' + inttostr(CurrentLevel) +
  ' AND  (ORG.PARENT_KEY = ''' + StartparentKey + ''' OR ORG.Lineage LIKE ''' + ParentLineage  + '%'')' +
  ' ORDER BY  Org2.Lineage,Org.WEIGHT, TR.Sequence, T.Item_Name ';

  IResult := FConn.Execute(SQLString);
  j := 0;
  Lastkey := '00';
  PrevBaseLineage  := '';
  WHILE IResult.eof = false DO
     begin
       j:= j + 1;

       BaseLineage :=   Iresult.Fields.Item[1].value;
       If BaseLineage <> PrevBaseLineage then  Lastkey := '00';
       Lastkey :=  IncrementKey(LastKey);
       NewLineage := Iresult.Fields.Item[1].value +  LastKey + '\';
       NewSortOrder :=  GenerateSortOrder(NewLineage);
       UpdateSQL := 'Update Organism Set Lineage = ''' + NewLineage + ''', Sort_Order = ''' + newSortOrder + '''' +
       ' FROM Organism WHERE Organism_Key = ''' +  Iresult.Fields.Item[0].value  + '''';
       Execute_Sql(UpdateSql);

       UpdateSQl := ' Update Index_Taxon_name set sort_order = '''  +  NewSortOrder + '''' +
       ' FROM Index_Taxon_Name ITN INNER JOIN Taxon_List_Item TLI  ' +
       ' ON TLI.Taxon_List_Item_Key = ITN.Recommended_taxon_List_Item_Key ' +
       ' WHERE TLI.Taxon_Version_Key = ''' + Iresult.Fields.Item[2].value + '''';
       Execute_Sql(UpdateSql);

       PrevBaseLineage := BaseLineage;
       IResult.MoveNext
     end;
  Screen.Cursor := crdefault;

end;
end;






function TManageUDT.Get_Exportable: WordBool;
begin

end;

function TManageUDT.Get_KeyList: IKeyList;

begin


end;



procedure TManageUDT.ActiveFormCreate(Sender: TObject);
begin
  ListBox2.items.add('All Lists');
  ListBox2.items.add('User Added List');
  ListBox2.items.add('Taxa with no parents');
   ListBox2.items.add('Taxa with parents');
  ComboBox1.items.add('28 - Species');
  ComboBox1.items.add('22 - Genus');
  ComboBox1.items.add('18 - Family');

end;

procedure TManageUDT.ListBox2Click(Sender: TObject);
var
 nitem :integer;

begin
    ConnectToR6;
    ClearListBox1();
    checkUserId;
    nitem:= ListBox2.ItemIndex;
    if nItem >= 0 then
       label13.Caption := inttostr(PopulateListBox1(nitem))
    else
    begin
      MessageDlg ('Please select a list to use ', MtInformation,[mbOk],0);
    end;
end;


function TManageUDT.CheckProposedParent: string;
var
FailedReason : string;
begin
  //cant be redundant, can't be null and can't be same as organism
  //If (FR6OrganismRedundantFlag = 'Y') then FailedReason := 'Proposed parent is redundant - ';
  If  (FR6OrganismKey = '') then  FailedReason := FailedReason + 'Does not have Organism key - ' ;
  If  (FR6OrganismKey= FListOrganismKey) then  FailedReason := FailedReason + 'Parent can not be same key. ';
  if FailedReason <> '' then
     Result := 'Not a suitable parent.' +  FailedReason
  else
    Result:= '';

end;

function TManageUDT.IncrementKey(LastKey: string): string;
Var

NewAscLeft : integer;
NewAscRight   : integer;


begin

if length(LastKey)  = 1 then LastKey := '0' + LastKey;

NewAscLeft := Ord(LastKey[1]);
NewAscRight :=  Ord(LastKey[2]) + 1;


if NewAscRight =  91 then
begin
 NewAscRight := 48;
 NewAscLeft := NewAscLeft + 1
end;
if NewAscRight = 58 then NewAscRight := 65;
if NewAscLeft = 58 then NewAscLeft := 65;
if NewAscLeft <> 48 then
       Result := chr(NewAscLeft) + chr(NewAscRight)
  else
       Result :=  chr(NewAscRight);
end;

function TManageUDT.GenerateSortOrder(NewLineage: string): string;
var
s : integer;
LineageElement : string;
Newsort : string;
Remainder : string;
begin

   Remainder := NewLineage;
   s := pos('\',Remainder);
   WHILE S > 0  DO
   Begin
     LineageElement := '00' + ansileftstr(Remainder,s-1);
     NewSort := NewSort + ansirightstr(LineageElement,2);
     Remainder := ansirightstr(Remainder,length(remainder)-s);
     s := pos('\',Remainder);
   end;
   Result := ansileftstr(NewSort + '000000000000000000000000000000000000',36);

end;
function TManageUDT.CheckTVK(CurrentTvk :string) : boolean;
var
  IResult: _Recordset;
  IRecorder: IRecorder2000;
begin

  IResult := FConn.Execute('Select * from Taxon_Version Where Taxon_Version_Key  = ''' + CurrentTvk + '''');
  if IResult.EOF = false then begin
     MessageDlg ('The TVK entered already exists', MtInformation,[mbOk],0);
     Result := false
  end
  else result := true;
end;

function TManageUDT.CheckUserId : integer;
var
  IResult: _Recordset;

  IRecorder: IRecorder2000;
begin
   IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
   FuserID := IRecorder.CurrentSettings.UserIDKey;

   IResult := FConn.Execute('Select Security_Level from [user] Where Name_Key = ''' + Fuserid + '''');
   If IResult.Fields[0].value = 5   then
    Begin
      Button1.Enabled := true;
      Button2.Enabled := true;
      Button3.Enabled := true;
      Button4.Enabled := true;
      Button5.Enabled := true;
      Button6.Enabled := true;
      Button7.Enabled := true;
      Button8.Enabled := true;
      Button9.Enabled := true;
      Button10.enabled := true;
      Result := 5;
      IResult :=   FConn.Execute('Select [Data] from Setting WHERE [Name] = ''' + 'SiteID' + '''');
      FSiteId := iResult.Fields[0].Value;

    End
    else
    begin
      MessageDlg ('You need to be an administrator to ues most of the available features ', MtInformation,[mbOk],0);
      Result := IResult.Fields[0].value;
    end;
 end;

function TManageUDT.AutoProcess(R6Key: string): integer;
var
  IResult: _Recordset;
  SQLText : Widestring;
  ParentTVKey  : string;
  Genus : string;
begin
   SQLText := 'Select TLI2.Taxon_Version_Key FROM Taxon_List_Item TLI   ' +
   ' INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI.Parent ' +
   ' INNER JOIN Taxon_List_item TLI2 ON TLI2.Taxon_List_item_Key = ITN.Recommended_taxon_List_Item_Key ' +
   ' INNER JOIN Organism Org on ORg.Taxon_version_Key = TLI2.Taxon_version_Key ' +
   ' WHERE TLI.Taxon_version_key = ''' +  r6key + '''';
   IResult := FConn.Execute(SQLText);
   If IResult.eof = false then ParentTvKey := Iresult.fields[0].value
   Else
   Begin
      Genus := WorkoutGenus(FListTaxonName);
      SQLText := 'Select Org.Taxon_Version_Key FROM Index_taxon_name ITN ' +
      ' INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Recommended_taxon_List_Item_Key ' +
      ' INNER JOIN Organism ORG ON ORG.taxon_Version_Key = TLI.Taxon_Version_Key ' +
      ' WHERE ITN.actual_Name = ''' + Genus + '''';
      IResult := FConn.Execute(SQLText);
      If IResult.eof = false then ParentTvKey := Iresult.fields[0].value
   end;
   If ParentTvKey <> '' then
   begin
        PopulateInfoOnTaxa(ParentTVKey, 'TV', 'R6');
        If CheckProposedParent  = '' then
        begin
          Result := Populate_Organism_Parent;
        end;
   end
   else
       Result := 0;
end;

function TManageUDT.WorkOutGenus(Taxon: string): string;
var
S : integer;
begin
  s := Pos(' ',Taxon);
  if s <> 0 then  Result:= ansiLeftStr(Taxon,s-1)
  else Result := '';
end;

procedure TManageUDT.Button2Click(Sender: TObject);
var
IResult: _Recordset;
SQLString : Widestring;
currentmessage : string;
buttonselected : Integer;
begin
If FListTVKey <> '' then
Begin
 CurrentMessage := 'Entry for ' + FListTaxonName  + ' in Organism Table will be deleted - does not affect any other table' ;
 buttonselected := MessageDlg (CurrentMessage, MtInformation,[mbok,mbCancel],0);
 if buttonselected = 1 then
  begin
   SQLString := 'Delete from Organism where Taxon_version_Key = ''' + FLISTTVkey + '''';
   FConn.Execute (SQLString);
   SQLString := 'Update Index_Taxon_Name set Sort_Order = ''000000000000000000000000000'' ' +
   ' FROM Index_Taxon_Name ITN INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_key = ITN.Taxon_List_Item_Key ' +
   ' WHERE TLI.Taxon_version_Key = ''' +  FLISTTVkey + '''';
   FConn.Execute (SQLString);

   RePopulateBox1;

  end;
end
else
  MessageDlg ('Select a taxon first ', MtInformation,[mbok],0)

end;

function TManageUDT.ConnectToR6: string;
Var IRecorder : IRecorder2000;
begin
if FConn =  nil then
Begin
 FConn:=TADOConnection.Create(nil);
 IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
 FConn.ConnectionString:=IRecorder.ConnectionString;
 FConn.open;
 IRecorder.SetApplicationSecurity(FConn.ConnectionObject);
End;
end;

procedure TManageUDT.Button3Click(Sender: TObject);
var
Irecorder : IRECORDER2000;
begin
  if FListTLIKey <> '' then
  begin
     FCurrentAction := 2;
     IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
     IRecorder.RequestData(TManageUDT(self),'Taxon')
  end
  else
     MessageDlg ('You must select a Taxon before proceeding', MtInformation,[mbOk],0)




end;

Procedure TManageUDT.ProcessChangedOrganism(CurrentKey : string);
var
CurrentMessage : string;
buttonSelected : integer;

begin
   CurrentMessage := CheckProposedParent ;
   If CurrentMessage <> '' then
          MessageDlg (CurrentMessage, MtInformation,[mbOk],0)
    else
      begin
        CurrentMessage := 'Parent will allocated to ' +  FR6TaxonName ;
        buttonselected := MessageDlg (CurrentMessage, MtInformation,[mbok,mbCancel],0);

        If buttonSelected = 1 then
         begin

            Populate_Organism_Parent;
            Update_Info;
          end;
      end;
end;

procedure TManageUDT.ProcessReplaceTaxa(CurrentKey: string);
Var
SQlString : widestring;
buttonSelected : integer;
UpdateOK : boolean;
begin
  Screen.Cursor := crHourGlass;
  if FR6TLIKey <> '' then
  begin
     buttonselected := MessageDlg (FListTaxonName + ' will be replaced with ' + FR6TaxonName  , MtInformation,[mbyes,mbcancel],0);

     UpdateOk := UpdateTaxonDetermination(FListTLIKey, FR6TLIKey );

     buttonselected := MessageDlg ('Do you wish to delete the selected taxon', MtInformation,[mbyes,mbno],0);
     If buttonSelected = 6 then
         begin
            Delete_Taxon(FListTLIKey);
            RePopulateBox1;
         end
         else
           Update_Info;
 end;
  Screen.Cursor := crDefault;
end;

function TManageUDT.Delete_Taxon(CurrentKey: string): boolean;
var
IResult : _Recordset;
SQLString : widestring;
TaxonKey : string;
TVKey : string;
CantDeleteFlag : boolean;
begin

   CantDeleteFlag := false;
   //check to see if determination exists - if so then can't do
   // Delete from all other places in order
   SQLstring := 'Select * From Taxon_Determination where Taxon_List_Item_Key = ''' + CurrentKey +  '''';
   IResult := Fconn.Execute(SQLString);
   If IResult.eof = false then  CantDeleteFlag := true;
   SQLstring := 'Select * From Taxon_List_Item WHERE Parent = ''' + CurrentKey +  '''';
   IResult := Fconn.Execute(SQLString);
   If IResult.eof = false then  CantDeleteFlag := true;

   If CantDeleteFlag = false then
   begin
        Screen.Cursor := crHourglass;
        SQLstring := 'Select TV.Taxon_version_Key,TV.Taxon_Key ' +
        ' FROM taxon_List_Item TLI INNER JOIN Taxon_Version TV ON TV.Taxon_version_Key = TLi.Taxon_Version_Key '+
        ' WHERE TLI.Taxon_List_Item_Key =  ''' + CurrentKey + '''';
         IResult := Fconn.Execute(SQLString);
         if Iresult.EOF = false then
         begin
           TaxonKey := IResult.Fields[0].value;
           TVKey := IResult.Fields[1].value;
         end;

        SQLstring := 'Delete FROM Taxon_Designation WHERE Taxon_List_Item_key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM IW_Matched_Species  WHERE Matched_key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Index_Taxon_Name   WHERE Recommended_Taxon_List_Item_Key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Index_Taxon_Name   WHERE Taxon_List_Item_Key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Index_Taxon_Hierarchy   WHERE Recommended_Taxon_Version_Key = ''' + TVKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Taxon_Fact WHERE Taxon_Version_Key = ''' + TVKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Index_Taxon_Synonym   WHERE Taxon_List_Item_Key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Index_Taxon_Group  WHERE Taxon_List_Item_Key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Index_Taxon_Group  WHERE Contained_List_Item_Key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Index_Taxon_Designation  WHERE Taxon_List_Item_Key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Taxon_Common_name  WHERE Taxon_List_Item_Key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Taxon_User_name  WHERE Taxon_List_Item_Key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Export_Filter_Taxon WHERE Taxon_List_Item_Key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Organism  ' +
        ' FROM Organism  WHERE Taxon_Version_Key = ''' + TVKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Taxon_Fact ' +
        ' FROM Taxon_Fact WHERE Taxon_Version_Key = ''' + TVkey + '''';
        Execute_SQl (SQLString);
         SQLstring := 'Delete FROM Taxon_Sources ' +
        ' FROM Taxon_Sources ' +
        ' WHERE Taxon_Key  = ''' + TaxonKey + '''';
        Execute_SQl (SQLString);
         SQLstring := 'Delete FROM Taxon_List_item WHERE Parent = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Taxon_List_item WHERE Taxon_List_Item_Key = ''' + CurrentKey + '''';
        Execute_SQl (SQLString);

        SQLstring := 'Delete FROM Taxon_Version ' +
        ' FROM Taxon_Version WHERE Taxon_Version_Key = ''' + TVkey + '''';
        Execute_SQl (SQLString);
        SQLstring := 'Delete FROM Taxon ' +
        ' FROM Taxon ' +
        ' WHERE Taxon_Key = ''' + TaxonKey + '''';
        Execute_SQl (SQLString);

        Screen.Cursor := crdefault;
        Result:= true;
    end
    else   Result := false;
end;

procedure TManageUDT.Button6Click(Sender: TObject);
begin
  if FListTLIKey <> '' then
   begin
      if Delete_Taxon(FListTLIKey)= true then
       begin
          RePopulateBox1;
          MessageDlg ('Taxon deleted ', MtInformation,[mbOk],0);

       end
       else
          MessageDlg ('The Taxon is in use - can not delete ', MtInformation,[mbOk],0);
   end

  else
     MessageDlg ('You must select a Taxon before proceeding', MtInformation,[mbOk],0);



end;

procedure TManageUDT.Button5Click(Sender: TObject);
Var
  I: Integer;
  TVKey : string;
  ProcessedMove : integer;
  AutoReplaceCount : integer;
  CurrentMessage : widestring;
  buttonselected : integer;
begin
  Screen.Cursor := crHourglass;
  CurrentMessage := 'This process will attempt to replace all taxa listed with an acceptable taxa ' +
    ' from the current dictionary based on name. Only taxon where just one version of the name exists will be used. ' +
    ' Where the change is made the user defined taxon will be deleted. Take a backup before using this option and check the results carefully.';


  buttonselected := MessageDlg (CurrentMessage, MtInformation,[mbOk,Mbcancel],0);


  if buttonselected = 1 then
  begin
    For I := 0 to ListBox1.count -1 do
    Begin
     TVKey := ansileftstr(ListBox1.Items [I],16);
     PopulateInfoOnTaxa(TVKey, 'TV', 'FList');
     AutoReplaceCount := AutoReplaced(FListTLIkey);
     ProcessedMove := ProcessedMove + AutoReplaceCount;


    end;
  end;
  RepopulateBox1;
  MessageDlg ('Records Auto Replaced = ' + inttostr(Processedmove), MtInformation,[mbOk],0);
  Screen.Cursor := crdefault;
end;

function TManageUDT.AutoReplaced(TLIkey: string): integer;
var
CurrentMessage : widestring;
buttonselected : integer;
IResult : _Recordset;
SQLString : string;
UpdateOk : boolean;
begin
  SQLString := 'Select count(Distinct ITN.Recommended_Taxon_List_Item_Key)  FROM  Index_taxon_name ITN ' +
  ' WHERE  ITN.System_Supplied_data = 1 AND ITN.Allow_data_Entry = 1 AND ITN.ACTUAL_NAME = ''' + FListTaxonName + ''' AND '  +
  ' ITN.Taxon_List_Item_Key <> ''' + FListTLIKey + '''';

  IResult := Fconn.execute(SQLString);

  if Iresult.Fields[0].value  = 1 then
    begin
      SQLString := 'Select ITN.Recommended_Taxon_List_Item_Key FROM  Index_taxon_name ITN ' +
     ' WHERE  ITN.System_Supplied_data = 1 AND ITN.Allow_data_Entry = 1 AND ITN.ACTUAL_NAME = ''' + FListTaxonName + ''' AND '  +
     ' ITN.Taxon_List_Item_Key <> ''' + FListTLIKey + '''';
      IResult := Fconn.execute(SQLString);
      if Iresult.eof = false then
        begin
          UpdateOk := UpdateTaxonDetermination(FListTLIKey, IResult.fields[0].value );
          Delete_Taxon(FListTLIKey);
          Result := 1;
        end
        else
          Result := 0;
    end
    else
          Result := 0;


end;

function TManageUDT.RePopulateBox1: integer;
var
nitem : integer;
begin
  nitem:= ListBox2.ItemIndex;
  PopulateListBox1(nitem);
  label13.Caption := inttostr(PopulateListBox1(nitem));

end;


function TManageUDT.UpdateSingleITH(OrganismKey,ParentOrganismKey: string): integer;
var
 IResult : _Recordset;
 SQLString :widestring;
 TLevelKeyF : string;
 TLevelKeyO : string;
 TLevelKeyC : string;
 TLevelKeyP : string;
 TLevelKeyK : string;
 StartSequence : integer;
 SelectedSequence : integer;
 OrgTVKey : string;
 begin
  SQLString := 'Select TR.Sequence FROM Organism ORG INNER JOIN Taxon_rank TR ON TR.Taxon_rank_Key = ' +
  ' ORG.Organism_Rank_Key WHERE Organism_Key = ''' + OrganismKey  + '''';
   IResult := FConn.execute(SQLString);
   StartSequence := IResult.Fields[0].Value;

  SQLString := 'Select Org.Taxon_version_Key,Org1.Taxon_Version_key , isnull(TR.Sequence,0)  ' +
  ' FROM Organism Org INNER JOIN Organism  ORG1 ON ORG.Lineage like Org1.Lineage + ''%''' +
  ' INNER JOIN Taxon_Rank TR ON TR.Taxon_rank_Key = ORG1.Organism_rank_Key '  +
  ' WHERE Org.Organism_Key = ''' + OrganismKey + '''' +
  ' ORDER BY TR.Sequence DESC ';
  IResult := FConn.execute(SQLString);
  While IResult.EOF = false DO
  begin
    OrgTVKey := Iresult.fields[0].value;
     SelectedSequence := Iresult.fields[2].value;
     If (SelectedSequence < 181) AND (TLevelKeyF = '') AND (StartSequence > 179) then TLevelKeyF := IResult.Fields[1].value;
     If (SelectedSequence < 101) AND (TLevelKeyO = '') AND (StartSequence > 99) then TLevelKeyO := IResult.Fields[1].value;
     If (SelectedSequence < 61) AND (TLevelKeyC = '')  AND (StartSequence > 59) then TLevelKeyC := IResult.Fields[1].value;
     If (SelectedSequence < 31) AND (TLevelKeyP = '')  AND (StartSequence > 29)then TLevelKeyP := IResult.Fields[1].value;
     If (SelectedSequence < 11)  AND (TLevelKeyK = '') then TLevelKeyK := IResult.Fields[1].value;
     IResult.movenext;
   end;

   SQLString := 'Delete from Index_Taxon_Hierarchy  '  +
   ' WHERE Recommended_Taxon_version_Key  = ''' +  OrgTVKey + '''';
   Execute_SQL(SQLstring);

   SQLString := 'INSERT INTO Index_taxon_Hierarchy (Recommended_taxon_version_key,Hierarchy_taxon_Version_Key,Hierarchy_Type) ' +
  ' Values (''' + OrgTVKey + ''',''' + TlevelKeyF + ''',''F'') ';
   If TlevelKeyF <> '' then Execute_SQL(SQLstring);

   SQLString := 'INSERT INTO Index_taxon_Hierarchy (Recommended_taxon_version_key,Hierarchy_taxon_Version_Key,Hierarchy_Type) ' +
  ' Values (''' + OrgTVKey + ''',''' + TlevelKeyO + ''',''O'')';
   If TlevelKeyO <> '' then Execute_SQL(SQLstring);

   SQLString := 'INSERT INTO Index_taxon_Hierarchy (Recommended_taxon_version_key,Hierarchy_taxon_Version_Key,Hierarchy_Type) ' +
  ' Values (''' + OrgTVKey + ''',''' + TlevelKeyC + ''',''C'') ';
   If TlevelKeyC <> '' then Execute_SQL(SQLstring);

    SQLString := 'INSERT INTO Index_taxon_Hierarchy (Recommended_taxon_version_key,Hierarchy_taxon_Version_Key,Hierarchy_Type) ' +
  ' Values (''' + OrgTVKey + ''',''' + TlevelKeyP + ''',''P'') ';
   If TlevelKeyP <> '' then Execute_SQL(SQLstring);

    SQLString := 'INSERT INTO Index_taxon_Hierarchy (Recommended_taxon_version_key,Hierarchy_taxon_Version_Key,Hierarchy_Type) ' +
  ' Values (''' + OrgTVKey + ''',''' + TlevelKeyK + ''',''K'') ';
   If TlevelKeyK <> '' then Execute_SQL(SQLstring);


end;


procedure TManageUDT.DoCancel;
begin

end;

function TManageUDT.DoOk: WordBool;
begin

end;

function TManageUDT.Get_Height: Integer;
begin
     Result := 656;
end;

function TManageUDT.Get_Width: Integer;
begin
    Result := 871;
end;

procedure TManageUDT.Button7Click(Sender: TObject);
Var
TVKey : string;
AutoReplaceCount : integer;
CurrentMessage : widestring;
buttonselected : integer;
begin
  TVkey :=  Get_Current_TV_Key ;
  if TVkey <> '' then
   begin

    CurrentMessage := 'This process will attempt to automatically replace the selected taxon with an acceptable taxon ' +
    ' from the current dictionary based on name. Only taxon where just one version of the name exists will be used. ' +
    ' Where the change is made the user defined taxon will be deleted. Take a backup before using this option and check the results carefully.';
    buttonselected := MessageDlg (CurrentMessage, MtInformation,[mbOk,Mbcancel],0);
    if buttonselected = 1 then
    begin
     PopulateInfoOnTaxa(TVKey, 'TV', 'FList');
     AutoReplaceCount := AutoReplaced(FListTLIkey);
    end;
    if  AutoReplaceCount = 1 then
     begin
       MessageDlg ('Records Auto Replaced = ' + inttostr(AutoReplaceCount), MtInformation,[mbOk],0);
       RepopulateBox1;
     end
     else
        MessageDlg ('No records replaced'  , MtInformation,[mbOk],0);
   end;

end;

function TManageUDT.UpdateTaxonDetermination(TLIKeyOld, TLIKeyNew : string): boolean;
var SQLString : widestring;
begin
   SQLString := 'Update Taxon_Determination SET Taxon_List_Item_Key = ''' + TLIKeyNew + '''' +
   ', Changed_Date = Getdate(), Changed_By = ''' + FUserID + '''' +
   ' WHERE Taxon_List_item_Key = ''' + TLIKeyOld + '''';
   Execute_SQL (SQLstring);
   Result := true;
end;

procedure TManageUDT.Button9Click(Sender: TObject);
begin
  RePopulateBox1;
end;

procedure TManageUDT.Button8Click(Sender: TObject);
Var
  buttonselected :integer;
  I: Integer;
  TVKey : string;
  Processed : integer;
begin
  buttonselected := MessageDlg (Button8Message, MtInformation,[mbOk,MBCancel],0);
  if buttonselected =  1 then
  begin
    Screen.Cursor := crHourglass;
    For I := 0 to ListBox1.count -1 do
      Begin
        TVKey := ansileftstr(ListBox1.Items [I],16);
        PopulateInfoOnTaxa(TVKey, 'TV', 'FList');
        if  delete_taxon(FListTLIKey)  = true then
        begin
           Processed :=  Processed + 1;
        end;
      end;
        Screen.Cursor := crdefault;
        MessageDlg ('Taxa Deleted = ' + inttostr(Processed), MtInformation,[mbOk],0);
   end;
  RePopulateBox1();

end;
destructor TManageUDT.destroy;
begin
   If FConn <> nil then
     begin
          FConn.Close;
          FConn.Free;
     end;
  inherited;
end;

procedure TManageUDT.Button10Click(Sender: TObject);
var
 nitem : integer;
  IRecorder :  IRECORDER2000;
begin
 nitem := combobox1.ItemIndex;
 ConnectToR6;
 if (CheckUserid = 5) and (checkTVK(edit2.Text)) then
 Begin
  if (Edit1.text = '') Or (nitem < 0)  then
     MessageDlg ('Taxon and Rank must be completed', MtInformation,[mbOk],0)
  else
     if (pos('''',Edit1.text) > 0) OR (length(Edit1.text) < 3) OR (length(Edit1.text) >100 ) then
      MessageDlg ('Not a valid taxon name', MtInformation,[mbOk],0)
     else
     Begin
         FCurrentAction := 3;
         If   MessageDlg (NewTaxonMessage, MtInformation,[mbOk,mbCancel],0) = 1 then
         begin
           IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
           IRecorder.RequestData(TManageUDT(self),'Taxon');
         end;
     End;
 end;
end;

function TManageUDT.AddNewTaxon(CurrentKey: string): boolean;
var
NewTVkey :string;
NewTLIKey :string;
NewTaxonKey :string;
SQLText : widestring;
IRecorder : IRecorder2000;
Abbreviation : string;
TRankKey : string;
CurrentMessage : string;
ParentKeyString : string;
IRecordset : _Recordset;
nitem : integer;
begin
   nItem := ComboBox1.ItemIndex;
   CurrentMessage := CheckProposedParent ;
   If CurrentMessage <> '' then
          MessageDlg (CurrentMessage, MtInformation,[mbOk],0)
   else


      begin
       if FR6TaxonListKey = 'LC00000100000001' then

       Begin
         ParenTKeyString := '''' + FR6TLIKey + '''';
       end
       else
        ParenTKeyString := 'NULL' ;
        Screen.Cursor := crHourglass;
       IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
       NewTaxonKey := IRecorder.GetNextKey('Taxon');
       NewTVKey   :=  IRecorder.GetNextKey('Taxon_Version');
       NewTLIKey := IRecorder.GetNextKey('Taxon_List_Item');
       Abbreviation := GenerateAbbreviation(Edit1.Text);

       TRankKey :=  'NBNSYS00000000' + ansileftstr(ComboBox1.Items[nItem],2);
       SQLText := 'INSERT INTO Taxon(Taxon_Key,Item_Name,Language,Taxon_name_type_key, Abbreviation, ' +
       ' Entered_By, Entry_Date,System_supplied_data, Custodian) ' +
       ' VALUES (''' + NewTaxonKey + ''',''' + Edit1.text + ''',''la'',''NBNSYS0000000000'',''' +  Abbreviation + ''',''' + FUSERID + ''', GetDate(),0,''' + FSiteId + ''')';
       Execute_SQL(SQlText);
       SQLText := 'INSERT INTO Taxon_version (Taxon_version_Key,Taxon_Key,Entered_By,Entry_Date,System_Supplied_Data,Output_Group_Key,Custodian) '+
       ' VALUES(''' + NewTVKey + ''','''  + NewTaxonKey + ''',''' + FUserId + ''',GetDate(),0,''' + FR6OutputGroupKey + ''',''' + FsiteId + ''')';
       Execute_SQL(SQlText);
       SQLText :=  'INSERT INTO Taxon_List_Item(Taxon_List_Item_Key,Taxon_version_Key,Taxon_List_Version_Key,Preferred_Name,Parent,Taxon_rank_key,Lst_Itm_Code, Entered_By,Entry_date,System_supplied_data,Custodian ) ' +
       ' VALUES(''' + NewTLIKey + ''',''' + NewTVKey + ''',''LC00000100000001'',''' + NewTLiKey + ''',' + ParentKeyString + ',''' + TRankKey + ''',''' + Fuserid + ''',''' + Edit2.text + ''', GetDate(),0,''' + FsiteId + ''')';
       Execute_SQL(SQlText);
       SqLText := 'INSERT INTO INDEX_TAXON_NAME (Taxon_List_item_Key,Taxon_List_Version_Key,Actual_name,Common_Name,' +
       ' Preferred_Name,Abbreviation,Preferred_Taxa,System_Supplied_Data,Preferred_List,Allow_data_entry,Actual_Name_Italic,Common_Name_Italic, Preferred_Name_Italic,' +
       ' Recommended_Taxon_List_Item_Key) ' +
       'VALUES(''' + NEWTLIkey + ''',''LC00000100000001'',''' +  Edit1.text + ''',''' +  Edit1.text +  ''',''' +
       Edit1.text + ''',''' + abbreviation + ''',1,1,0,1,0,0,0, '   +
       '''' + NewTLIKey + ''')';
       Execute_SQL(SQlText);

       PopulateInfoOnTaxa(NewTLIKey, 'TLI','FList');

       Populate_Organism_Parent;

       SQLtext := 'UPDATE Index_taxon_name set Sort_Order = (SELECT Sort_Order ' +
        ' FROM Organism Where Organism.taxon_version_key = ''' + NewTVKey + ''') WHERE Taxon_List_Item_Key = ''' + NewTLIKey + '''';
        Execute_SQL(SQlText);

         SQLText := ' INSERT INTO Index_Taxon_Synonym (Taxon_List_Item_Key,Synonym_List_Item_Key)' +
         ' VALUES (''' + NewTLIKey + ''',''' + NewTLIKey + ''')';
         Execute_SQL(SQlText);


       If Parentkeystring <> '' then
         Begin
           SQLtext := 'UPDATE Index_taxon_name set Has_Children = 1 where Taxon_List_item_Key = ''' +
           FR6TLIKey + '''' ;
           Execute_SQL(SQlText);


          // Changed to allow the pick up of the recommended TLI key

          
        end;
         MessageDlg ('Taxon Created', MtInformation,[mbOk],0);
         edit1.Text := '';
    end;
    Screen.Cursor := crDefault;
end;

function TManageUDT.Populate_Index_Taxon_Group : Integer;
var
IRecordset : _Recordset;
SQLText : widestring;
ITG_Added : integer;
begin
         SQLText  := ' Delete From Index_Taxon_Group Where Contained_List_Item_Key = ''' +   FListTLIKey + '''' ;
         Execute_SQL(SQlText);

         SQLtext := 'Select ITG.taxon_List_Item_key,ITG.Contained_List_Item_key,ITG.System_Supplied_Data, ITG.Item_Level ' +
         ' FROM Index_Taxon_Group ITG '  +
         ' WHERE ITG.Contained_List_Item_key = ''' + FListParentOrgTLIKey + '''';
          ITG_Added := 1;
          IRecordset  := FConn.Execute(SQLText);
           while Irecordset.eof = false Do
           Begin
             ITG_Added := ITG_Added + 1;
             SqlText := 'Insert INTO Index_Taxon_Group (Contained_List_Item_Key,Taxon_List_Item_Key,Item_Level,System_Supplied_data)' +
            ' VALUES (''' + FListTLIKey + ''',''' + IRecordset.Fields[0].value + ''',' + (inttostr(IRecordset.Fields[3].value+1))  + ',1)';
             Execute_SQL(SQlText);
             Irecordset.movenext
           end;

         SQLText := ' INSERT INTO Index_Taxon_Group (Taxon_List_Item_Key,Contained_List_Item_Key,System_Supplied_data,Item_Level)' +
         ' VALUES (''' + FListTLIKey + ''',''' + FListTLIKey + ''',1,0)';
         Execute_SQL(SQlText);
        



         Result := ITG_Added;


end;


function TManageUDT.GenerateAbbreviation(ItemName: string): string;
var
S :integer  ;
B :integer ;

begin
  B := pos(')',ItemName) + 1 ;
  S :=  PosEx(' ',ItemName, B );
  if S <> 0 then
   Result := ansileftstr(ItemName,2) +  AnsiMidStr(ItemName,S+2,3)
  else
   Result :=  ansileftstr(ItemName,2);

end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TManageUDT,
    Class_ManageUDT,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmSingle);

end.
