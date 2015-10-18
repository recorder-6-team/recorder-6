unit ManageDesignationSetsImpl1;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, ManageDesignationSets_TLB, StdVcl,  Recorder2000_TLB,
  StdCtrls, ExtCtrls, ADODB, strUtils, Variants;
             
type
  TManageDesignationSetsX = class(TActiveForm, IManageDesignationSetsX,IRecorderAddin,INewAction)
    ListBox2: TListBox;
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    ListBox3: TListBox;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Shape1: TShape;
    Button6: TButton;
    Button7: TButton;
    Label5: TLabel;
    Label7: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Button8: TButton;
    Shape2: TShape;
    Shape3: TShape;
    procedure ActiveFormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private

    { Private declarations created}
     FConn: TADOConnection;
     FUserId : widestring;
     FSiteId : string;
     FLastSetType : integer;
    { Private declarations systems}
    FEvents: IManageDesignationSetsXEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);

   { Private declarations created}

    function Execute_SQL(SQLstring: widestring): integer;
    function ConnectToR6: string;
    function ClearListBox1: integer;
    function ClearListBox2: integer;
    function PopulateListBox1: integer;
    function PopulateListBox2(itemindex : integer): integer;
    function PopulateListBox3: integer;

    function Get_Current_Type_Key: string;
    function Get_Current_Set_Key: string;

    function Get_Current_System_Supplied() : integer;
    function ClearListBox3: integer;
    function HideSHowButtons(SystemSupplied, AddClone :integer): integer;
    function CheckUserId : integer;
    function AddNewDesignationSetItem(TaxonDesTypekey: string; TaxonSetkey : string ): integer;
    function CheckTDTypeExists(TaxonSetKey, TaxonDesTypeKey :string): boolean;
    function CheckTDSetNamevalid(TaxonSetName:string): boolean;
    function AddNewSetItem(TaxonSetName: string): boolean;
    function CloneNewSetItem(TaxonSetName: string): boolean;

  protected
    { Protected declarations }
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
   { IRecorder_Addin Public declarations }
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    { INewAction Public declarations }
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;

  public
    { Public declarations }
    procedure Initialize; override;
  end;

implementation

uses ComObj, ComServ;

{$R *.DFM}

{ TManageDesignationSetsX }

procedure TManageDesignationSetsX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_ManageDesignationSetsXPage); }
end;

procedure TManageDesignationSetsX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IManageDesignationSetsXEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TManageDesignationSetsX.Initialize;
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

function TManageDesignationSetsX.Get_Active: WordBool;
begin
  Result := Active;
end;

function TManageDesignationSetsX.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TManageDesignationSetsX.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TManageDesignationSetsX.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TManageDesignationSetsX.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TManageDesignationSetsX.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TManageDesignationSetsX.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TManageDesignationSetsX.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TManageDesignationSetsX.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TManageDesignationSetsX.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TManageDesignationSetsX.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TManageDesignationSetsX.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TManageDesignationSetsX.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TManageDesignationSetsX.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TManageDesignationSetsX.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TManageDesignationSetsX.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TManageDesignationSetsX.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TManageDesignationSetsX.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TManageDesignationSetsX.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TManageDesignationSetsX.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TManageDesignationSetsX._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TManageDesignationSetsX.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TManageDesignationSetsX.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TManageDesignationSetsX.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TManageDesignationSetsX.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TManageDesignationSetsX.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TManageDesignationSetsX.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TManageDesignationSetsX.KeyPressEvent(Sender: TObject;
  var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TManageDesignationSetsX.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TManageDesignationSetsX.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TManageDesignationSetsX.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TManageDesignationSetsX.Set_AxBorderStyle(
  Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TManageDesignationSetsX.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TManageDesignationSetsX.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TManageDesignationSetsX.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TManageDesignationSetsX.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TManageDesignationSetsX.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TManageDesignationSetsX.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TManageDesignationSetsX.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TManageDesignationSetsX.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TManageDesignationSetsX.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TManageDesignationSetsX.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TManageDesignationSetsX.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TManageDesignationSetsX.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TManageDesignationSetsX.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TManageDesignationSetsX.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

function TManageDesignationSetsX.Get_ActionCaption: WideString;
begin
     result:= 'Manage Designation Sets' ;
end;

function TManageDesignationSetsX.Get_CanAddToToolbar: WordBool;
begin
      result:= false;
end;

function TManageDesignationSetsX.Get_Description: WideString;
begin
     result:= 'Manage Taxon Designation Sets' ;
end;

function TManageDesignationSetsX.Get_DimmedImageFilename: WideString;
begin
      result:= 'default.bmp';
end;

function TManageDesignationSetsX.Get_DisabledImageFileName: WideString;
begin
     result:= 'default.bmp';
end;

function TManageDesignationSetsX.Get_Hint: WideString;
begin
       result:= 'Manage locally created Taxon Designation Sets' ;
end;

function TManageDesignationSetsX.Get_ImageFileName: WideString;
begin
     result:= 'default.bmp';
end;

function TManageDesignationSetsX.Get_Name: WideString;
begin
    result:='Manage Designation Sets V6.24';
end;

function TManageDesignationSetsX.Get_ParentMenu: WideString;
begin
        result:= 'Tools';
end;

procedure TManageDesignationSetsX.Install(
  const iInstalledFilePath: WideString);
begin

end;

//procedure TManageDesignationSetsX.ActiveFormDestroy(Sender: TObject);
//begin

//end;

function TManageDesignationSetsX.Execute_SQL(SQLstring: widestring): integer;


begin
  FConn.Execute(SQLString);
  Result := 0;

end;

function TManageDesignationSetsX.ConnectToR6: string;
Var IRecorder : IRecorder2000;
begin
if FConn =  nil then
Begin
 FConn:=TADOConnection.Create(nil);
 IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
 FConn.ConnectionString:=IRecorder.ConnectionString;
 FConn.open;
 IRecorder.SetApplicationSecurity(FConn.ConnectionObject);
end;
end;

procedure TManageDesignationSetsX.ActiveFormDestroy(Sender: TObject);
begin
    If FConn <> nil then
     begin
         FConn.Close;
          FConn.Free;
     end;
     Screen.Cursor := crdefault;
     ClearListBox1;
     ClearListBox2;
     ClearListBox3;
     ListBox1.Free;
     ListBox2.Free;
     ListBox3.Free;

end;
function  TManageDesignationSetsX.Get_Current_System_Supplied(): integer;
var
  TaxonSetKey : string;
  IResult: _Recordset;

 Begin
   Result := 0;
   TaxonSetKey :=  Get_Current_Set_Key;
   IResult := FConn.Execute ('Select system_supplied_data from Taxon_Designation_Set ' +
              ' Where Taxon_Designation_Set_Key = ''' + TaxonSetKey + '''');
   If  IResult.eof = true then
       Result :=  1
   else
       if Iresult.Fields.Item[0].value = true then result:= 1;





   //showmessage (inttostr(result));

end;


function  TManageDesignationSetsX.PopulateListBox1(): integer;
var

  IResult: _Recordset;
  TaxonSetKey : string;


begin
   Screen.Cursor := crHourGlass;
   ClearListBox1;
   TaxonSetKey :=  Get_Current_Set_Key();

   IResult := FConn.Execute ('SELECT TAXON_DESIGNATION_TYPE.TAXON_DESIGNATION_TYPE_KEY,  ' +
              ' TAXON_DESIGNATION_TYPE.KIND, ' +
              ' TAXON_DESIGNATION_TYPE.SHORT_NAME ' +
              ' FROM Taxon_Designation_Set_Item ' +
              ' INNER JOIN TAXON_DESIGNATION_TYPE ON ' +
              ' Taxon_Designation_Set_Item.Taxon_Designation_Type_Key = TAXON_DESIGNATION_TYPE.TAXON_DESIGNATION_TYPE_KEY ' +
              ' WHERE Taxon_Designation_Set_Key = ''' + TaxonSetKey + ''' ORDER BY TAXON_DESIGNATION_TYPE.KIND,TAXON_DESIGNATION_TYPE.SHORT_NAME ');
   WHILE IResult.eof = false DO

   BEGIN
       ListBox1.items.add(vartostr(Iresult.Fields.Item[0].value) +  '        '  +  vartostr(Iresult.Fields.Item[1].value) + '/' + vartostr(Iresult.Fields.Item[2].value));
       Iresult.MoveNext
   END;

   Screen.Cursor := crdefault;
   Result := ListBox1.Count;

end;
 function  TManageDesignationSetsX.PopulateListBox3(): integer;
var

  IResult: _Recordset;
  TaxonSetKey : string;


begin
   Screen.Cursor := crHourGlass;
   ClearListBox3;
   TaxonSetKey :=  Get_Current_Set_Key();
   IResult :=   FConn.Execute( 'SELECT DISTINCT TAXON_DESIGNATION_TYPE.TAXON_DESIGNATION_TYPE_KEY, ' +
               ' TAXON_DESIGNATION_TYPE.KIND, ' +
                ' TAXON_DESIGNATION_TYPE.SHORT_NAME FROM TAXON_DESIGNATION_TYPE  ' +
               ' ORDER BY TAXON_DESIGNATION_TYPE.KIND,TAXON_DESIGNATION_TYPE.SHORT_NAME ');


    WHILE IResult.eof = false DO

    BEGIN
       ListBox3.items.add(vartostr(Iresult.Fields.Item[0].value) +  '        '  + vartostr(Iresult.Fields.Item[1].value) + '/' + vartostr(Iresult.Fields.Item[2].value));
       Iresult.MoveNext
    END;

    Screen.Cursor := crdefault;
    Result := ListBox3.Count;

end;



function  TManageDesignationSetsX.PopulateListBox2(itemindex : integer): integer;
var

  IResult: _Recordset;

  ExtraWhere : string;

begin
   Screen.Cursor := crHourGlass;
   ClearListBox1;
   ClearListBox2;
   if itemindex = 1  then ExtraWhere := '';
   if itemindex = 2  then ExtraWhere := 'WHERE System_Supplied_Data = 0';
   IResult := FConn.Execute('SELECT Taxon_Designation_Set.Taxon_Designation_Set_Key, Taxon_Designation_Set.Title, ' +
              ' System_Supplied_Data ' +
              ' FROM Taxon_Designation_Set ' + ExtraWhere + ' ORDER BY TITLE ' );
   WHILE IResult.eof = false DO

   BEGIN
       ListBox2.items.add(vartostr(Iresult.Fields.Item[0].value) +  '        '  +  vartostr(Iresult.Fields.Item[1].value)) ;
       Iresult.MoveNext
   END;
   button5.Enabled := true;
   Screen.Cursor := crdefault;
   Result := ListBox2.Count;

end;
function  TManageDesignationSetsX.ClearListBox1: integer;

begin
  listbox1.Clear;
  //label2.Caption := '';
  //label4.Caption := 'Not Available' ;
   label5.Caption := '';
   
  //label11.Caption := '';
  Result := ListBox2.count;
end;

procedure TManageDesignationSetsX.Button1Click(Sender: TObject);
 var
 nitem :integer;

begin
    ConnectToR6;
    ClearListBox1();
    CheckUserId ;
   
    nitem:= 1;
    if nItem >= 0 then
      Begin
       label7.Caption := inttostr(PopulateListBox2(nitem));
       FLastSetType := 1;
      end
    else
    begin
      MessageDlg ('No valid lists', MtInformation,[mbOk],0);
    end;
end;

procedure TManageDesignationSetsX.Button2Click(Sender: TObject);

   var
 nitem :integer;

begin
    ConnectToR6;
    CheckUserId;
    ClearListBox1();
    ClearListBox2();

    nitem:= 2;
    if nItem >= 0 then
      Begin
       label7.Caption := inttostr(PopulateListBox2(nitem));
       FLastSetType := 2;
      end
    else
    begin
      MessageDlg ('No valid lists', MtInformation,[mbOk],0);
    end;
end;

function TManageDesignationSetsX.ClearListBox2: integer;


begin
  HideSHowButtons(1,0);
  listbox2.Clear;
  label7.Caption := '';
  label5.Caption := '';
  Result := ListBox2.count;
end;

function TManageDesignationSetsX.ClearListBox3: integer;

begin
  listbox3.Clear;
   Result := ListBox3.count;
end;


procedure TManageDesignationSetsX.ListBox2Click(Sender: TObject);
var NoOfTypes : integer;
    CurrentSystemSupplied :Integer ;
begin
    CurrentSystemSupplied := Get_Current_System_Supplied;
    ConnectToR6;
    ClearListBox1();
    PopulateListbox3;
    NoOfTypes := PopulateListBox1();
    If NoOfTypes >0 then
    begin
       HideSHowButtons (CurrentSystemSupplied,2);
      label5.caption := inttostr(NoOfTypes)
   end
    else
    begin
       HideSHowButtons (CurrentSystemSupplied,1);
      MessageDlg ('No Designation Types allocated to this set', MtInformation,[mbOk],0);
    end;
end;

Function TManageDesignationSetsX.Get_Current_Set_Key: string;
var
 nitem :integer;
begin
    nitem:= ListBox2.ItemIndex;
    if nItem >= 0 then
      Result := ansileftstr(ListBox2.Items [nitem],16)
    else
    begin
      MessageDlg ('Please select a set', MtInformation,[mbOk],0);
      Result := '';

    end;
end;

Function TManageDesignationSetsX.Get_Current_Type_Key: string;
var
 nitem :integer;
begin
    nitem:= ListBox1.ItemIndex;
    if nItem >= 0 then
      Result := ansileftstr(ListBox1.Items [nitem],16)
    else
      Result := '';

  
end;



procedure TManageDesignationSetsX.Button7Click(Sender: TObject);
var

SQLString : Widestring;
currentmessage : string;
buttonselected : Integer;
CurrentSetKey : string;
CurrentSystemSupplied : integer;
begin
CurrentSetKey := Get_Current_Set_Key;
If CurrentSetKey <> '' then
 Begin
    CurrentSystemSupplied := Get_Current_System_Supplied;
    If CurrentSystemSupplied = 0  then
      Begin
        CurrentMessage := 'Are you sure you wish to delete the selected set' ;
         buttonselected := MessageDlg (CurrentMessage, MtInformation,[mbok,mbCancel],0);
         if buttonselected = 1 then
              begin

                 SQLString := 'Delete from Taxon_Designation_Set_Item WHERE Taxon_Designation_Set_key = ''' + CurrentSetKey + '''';
                 FConn.Execute (SQLString);
                 SQLString := 'Delete from Taxon_Designation_Set WHERE Taxon_Designation_Set_key = ''' + CurrentSetKey + '''';
                 FConn.Execute (SQLString);

                 Clearlistbox1();
                 Clearlistbox2();
                 Clearlistbox3();
                 CurrentMessage := 'Set deleted ' ;
                 populatelistbox2(FLastSetType);
                 MessageDlg (CurrentMessage, MtInformation,[mbok],0);
              end;
      end
      else
         Begin
          CurrentMessage := 'System supplied sets can not be deleted ' ;
          MessageDlg (CurrentMessage, MtInformation,[mbok],0);
         End;

 end;
end;

procedure TManageDesignationSetsX.Button8Click(Sender: TObject);
var

SQLString : Widestring;
currentmessage : string;
buttonselected : Integer;
CurrentSetKey : string;
begin
   CurrentSetKey :=  Get_Current_Set_Key;
   If CurrentSetKey <> ''  then
     Begin
        CurrentMessage := 'Are you sure you wish to remove all entries from the set ?' ;
        buttonselected := MessageDlg (CurrentMessage, MtInformation,[mbok,mbCancel],0);
        if buttonselected = 1 then
              begin
                 SQLString := 'Delete from Taxon_Designation_Set_Item WHERE Taxon_Designation_Set_key = ''' + CurrentSetKey + '''';
                 FConn.Execute (SQLString);
                 Clearlistbox1();
              end;
    end
    else
    begin
      CurrentMessage := ' Please select set ' ;
      MessageDlg (CurrentMessage, MtInformation,[mbok],0);
    end;


end;

procedure TManageDesignationSetsX.Button3Click(Sender: TObject);
var
CurrentSetKey : string;
CurrentTypeKey : string;
SqlString : string;
CurrentMessage : string;
begin
  CurrentSetKey :=  Get_Current_Set_Key;
  CurrentTypeKey :=  Get_Current_Type_Key;
  If  CurrentTypeKey <> '' then
    Begin
      SQLString := 'Delete from Taxon_Designation_Set_Item WHERE Taxon_Designation_Type_Key = ''' + CurrentTypeKey  + ''' AND Taxon_Designation_Set_key = ''' + CurrentSetKey + '''';
      FConn.Execute (SQLString);
      PopulateListBox1();
    end
    else
    begin
      CurrentMessage := ' Please select a type for deletion ' ;
      MessageDlg (CurrentMessage, MtInformation,[mbok],0);
    end;


end;

function TManageDesignationSetsX.HideSHowButtons(
  SystemSupplied, AddClone : integer): integer;
var
  buttonvisible : boolean;
begin
   buttonvisible := true;
   if SystemSupplied = 1 then buttonvisible := false;
   button3.enabled := buttonvisible;
   button4.enabled := buttonvisible;
   button8.enabled := buttonvisible;

   button6.enabled := false;
  
   If AddClone = 2 then button6.Enabled := true;
   Result := 0;
end;

procedure TManageDesignationSetsX.Button4Click(Sender: TObject);
Var
  I: Integer;
  TDTKey : string;
  TaxonSetKey :string;
  Processed : integer;
begin
  Processed := 0;
  Screen.Cursor := crHourglass;
  TaxonSetKey := Get_Current_Set_Key;
  For I := 0 to ListBox3.count -1 do
  Begin
     if listbox3.selected[I]  = true then
       Begin
          TDTKey := ansileftstr(ListBox3.Items [I],16);
          Processed := Processed + AddNewDesignationSetItem(TDTKey,TaxonSetKey);
       end;
  end;
  Screen.Cursor := crdefault;
  if Processed > 1 then  MessageDlg ('Taxon Determination Types Added = ' + inttostr(Processed), MtInformation,[mbOk],0);
  PopulateListBox1();
end;


function TManageDesignationSetsX.AddNewDesignationSetItem(TaxonDesTypekey: string; TaxonSetkey : string ): integer;
var
NewTSIKey :string;
SQLText : widestring;
IRecorder : IRecorder2000;

begin
   result := 0;
   if CheckTDTypeExists(TaxonSetKey,TaxonDesTypeKey) = true then
   Begin
     IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
     NewTSIKey := IRecorder.GetNextKey('Taxon_Designation_Set_Item');
     SQLText := 'INSERT INTO Taxon_Designation_Set_Item(Taxon_Designation_Set_Item_key,Taxon_designation_Set_Key,' +
       ' Taxon_Designation_Type_Key,Entered_By, Entry_Date,System_supplied_data, Custodian) ' +
       ' VALUES (''' + NewTSIKey + ''',''' + TaxonSetKey + ''', ''' + TaxonDesTypeKey + ''',''' + FuserId   + ''', GetDate(),0, ''' + FSiteId + ''')';
      Execute_SQL(SQlText);
      Result := 1;
    end;
end;

function TManageDesignationSetsX.CheckTDTypeExists(TaxonSetKey, TaxonDesTypeKey :string): boolean;
Var
IResult : _Recordset;
Begin

  IResult := FConn.Execute('SELECT TAXON_DESIGNATION_TYPE_Key ' +
              ' FROM Taxon_Designation_Set_Item ' +
              ' WHERE Taxon_Designation_Set_Key = ''' + TaxonSetKey + ''' AND TAXON_DESIGNATION_TYPE_KEY = ''' + TaxonDesTypeKey + '''');

  Result := IResult.eof;

end;
function TManageDesignationSetsX.CheckUserId : integer;
var
  IResult: _Recordset;

  IRecorder: IRecorder2000;
begin
   IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
   FuserID := IRecorder.CurrentSettings.UserIDKey;
   IResult :=   FConn.Execute('Select [Data] from Setting WHERE [Name] = ''' + 'SiteID' + '''');
   FSiteId := iResult.Fields[0].Value;
   Result := 0;
end;

function TManageDesignationSetsX.CheckTDSetNameValid(
  TaxonSetName: string): boolean;

Var
IResult : _Recordset;
Begin
      Result := false;
      If leftstr(TaxonSetName,1) <> '[' then
      begin
       IResult := FConn.Execute('SELECT TAXON_Designation_Set_KEY ' +
              ' FROM Taxon_Designation_Set ' +
              ' WHERE Title = ''' + TaxonSetName + '''');

        If Iresult.eof = true then  Result := True;
     end;
end;



function TManageDesignationSetsX.AddNewSetItem(
  TaxonSetName: string): boolean;
 var
 NewSEtKey: string;
 IRecorder : IRecorder2000;
 SqlText : Widestring;
 begin
   if (Edit1.text = '') and (length(Edit1.text) > 10) then
      Begin
       MessageDlg ('Enter name for new set. Name must be between 10 and 100 characters in length ', MtInformation,[mbOk],0);
      end
      else
      begin
         if CheckTDSetNamevalid(Edit1.Text) = true   then
          Begin
               clearlistbox2;
               clearlistbox1;
               clearlistbox3;

                IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
                NewSetKey := IRecorder.GetNextKey('Taxon_Designation_Set');
              
                SQLText := 'INSERT INTO Taxon_Designation_Set(Taxon_designation_Set_Key,' +
                 ' Title,Entered_By, Entry_Date,System_supplied_data, Custodian) ' +
                  ' VALUES (''' + NewSetKey + ''',''' + edit1.text + ''',''' + fuserid  + ''', GetDate(),0, ''' + FSiteId + ''')';
                 Execute_SQL(SQlText);

                 ClearListBox1();
                 ClearListBox2();
                 label7.Caption := inttostr(PopulateListBox2(FLastSetType));
                 MessageDlg ('New set created.' + Edit1.Text + ' Use All or Local to view the sets', MtInformation,[mbOk],0);
                 Edit1.Text := '[Name of New Set]';


          end
          else
          begin
            MessageDlg ('Name already in use. Enter alternative ', MtInformation,[mbOk],0);
          end;
      end;
      Result := true;
end;

function TManageDesignationSetsX.CloneNewSetItem(
  TaxonSetName: string): boolean;
 var
 NewSetKey: string;
 NewSetItemKey: string;
 TDTKey : string;
 IRecorder : IRecorder2000;
 SqlText : Widestring;
 I : integer;
 begin
   if (Edit1.text = '') and (length(Edit1.text) > 10) AND (ListBox2.Count > 0) then
     Begin
       MessageDlg ('Check that you have a set selected and that a name is entered for new set. Name must be between 10 and 100 characters in length ', MtInformation,[mbOk],0);
     end
     else
     begin
       if CheckTDSetNamevalid(Edit1.Text) = true   then
       Begin
         clearlistbox3;
         IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
         NewSetKey := IRecorder.GetNextKey('Taxon_Designation_Set');
         SQLText := 'INSERT INTO Taxon_Designation_Set(Taxon_designation_Set_Key,' +
         ' Title,Entered_By, Entry_Date,System_supplied_data, Custodian) ' +
         ' VALUES (''' + NewSetKey + ''',''' + edit1.text + ''',''' + Fuserid  + ''', GetDate(),0, ''' + FSiteId + ''')';
         Execute_SQL(SQlText);
         For I := 0 to ListBox1.count -1 do
           Begin
             NewSetItemKey := IRecorder.GetNextKey('Taxon_Designation_Set_Item');
             TDTKey := ansileftstr(ListBox1.Items [I],16);
             SQLText := 'INSERT INTO Taxon_Designation_Set_Item(Taxon_Designation_Set_Item_key,Taxon_designation_Set_Key,' +
             ' Taxon_Designation_Type_Key,Entered_By, Entry_Date,System_supplied_data, Custodian) ' +
             ' VALUES (''' + NewSetItemKey + ''',''' + NewSetKey + ''', ''' + TDTKey + ''',''' + FuserId      + ''', GetDate(),0, ''' + FSiteId + ''')';
              Execute_SQL(SQlText);
           end;
             ClearListBox1();
             ClearListBox2();
             clearlistbox3;
             label7.Caption := inttostr(PopulateListBox2(FLastSetType));
             MessageDlg ('New set created. ' + Edit1.Text , MtInformation,[mbOk],0);
             Edit1.Text := '[Name of New Set]';
         end
         else
         begin
           MessageDlg ('Name already in use. Enter alternative ', MtInformation,[mbOk],0);
         end;
      end;
      Result := true;
end;




procedure TManageDesignationSetsX.Button5Click(Sender: TObject);
begin
  
   AddNewSetItem(Edit1.Text);

end;

procedure TManageDesignationSetsX.Button6Click(Sender: TObject);
begin
   CloneNewSetItem(edit1.text);
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TManageDesignationSetsX,
    Class_ManageDesignationSetsX,
    2,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
