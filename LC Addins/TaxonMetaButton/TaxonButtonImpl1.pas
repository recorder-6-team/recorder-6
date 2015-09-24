unit TaxonButtonImpl1;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, Controls, Graphics, Menus, Forms, StdCtrls,
  ComServ, StdVCL, AXCtrls, ProjectTaxonMetaButton_TLB,RECORDER2000_TLB,Messages,SysUtils,Dialogs,strUtils,Variants;

 // , SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
 // ActiveX, AxCtrls, ProjectManageUserTaxa_TLB, StdVcl, StdCtrls,Recorder2000_TLB,Variants,
 // ExtCtrls,strUtils,ADODB;


type
  TTaxonButton = class(TActiveXControl, ITaxonButton,IRecorderAddin,IRecorderFormEvents)
  private
    { Private declarations }
    FDelphiControl: TButton;
    FEvents: ITaxonButtonEvents;
    FTableName : string;
    FkeyValue : string;

    procedure ClickEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    function Get_MetaDataForTable() : widestring ;
    { my declarations }

  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    procedure InitializeControl; override;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_Cancel: WordBool; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Default: WordBool; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DragCursor: Smallint; safecall;
    function Get_DragMode: TxDragMode; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function Get_WordWrap: WordBool; safecall;
    function IsRightToLeft: WordBool; safecall;
    function UseRightToLeftReading: WordBool; safecall;
    function UseRightToLeftScrollBar: WordBool; safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    procedure InitiateAction; safecall;
    procedure Set_Cancel(Value: WordBool); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Default(Value: WordBool); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DragCursor(Value: Smallint); safecall;
    procedure Set_DragMode(Value: TxDragMode); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    procedure Set_WordWrap(Value: WordBool); safecall;
    procedure SetSubComponent(IsSubComponent: WordBool); safecall;

    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;

    function Get_FormName: WideString; safecall;
    procedure DoItemChange(const iTableName: WideString; const iKeyValue: WideString); safecall;
    procedure DoSave; safecall;
    procedure DoCancel; safecall;
    procedure SetForm(const iForm: IRecorderForm); safecall;
    procedure DoEditMode; safecall;
    procedure DoAdd; safecall;
    procedure DoDelete; safecall;
    function CheckCanSave: WordBool; safecall;
  end;

implementation

uses ComObj,ADODB;

{ TTaxonButton }
 const
     CRLF = chr(13) + chr(10);

procedure TTaxonButton.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  {TODO: Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_TaxonButtonPage); }
end;

procedure TTaxonButton.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ITaxonButtonEvents;
end;

procedure TTaxonButton.InitializeControl;
begin
  FDelphiControl := Control as TButton;
  FDelphiControl.OnClick := ClickEvent;
  FDelphiControl.OnKeyPress := KeyPressEvent;
end;

function TTaxonButton.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  Result := FDelphiControl.DrawTextBiDiModeFlagsReadingOnly;
end;

function TTaxonButton.Get_AlignDisabled: WordBool;
begin
  Result := FDelphiControl.AlignDisabled;
end;

function TTaxonButton.Get_Cancel: WordBool;
begin
  Result := FDelphiControl.Cancel;
end;

function TTaxonButton.Get_Caption: WideString;
begin
  Result := WideString(FDelphiControl.Caption);
end;

function TTaxonButton.Get_Default: WordBool;
begin
  Result := FDelphiControl.Default;
end;

function TTaxonButton.Get_DoubleBuffered: WordBool;
begin
  Result := FDelphiControl.DoubleBuffered;
end;

function TTaxonButton.Get_DragCursor: Smallint;
begin
  Result := Smallint(FDelphiControl.DragCursor);
end;

function TTaxonButton.Get_DragMode: TxDragMode;
begin
  Result := Ord(FDelphiControl.DragMode);
end;

function TTaxonButton.Get_Enabled: WordBool;
begin
  Result := FDelphiControl.Enabled;
end;

function TTaxonButton.Get_Font: IFontDisp;
begin
  GetOleFont(FDelphiControl.Font, Result);
end;

function TTaxonButton.Get_Visible: WordBool;
begin
  Result := FDelphiControl.Visible;
end;

function TTaxonButton.Get_VisibleDockClientCount: Integer;
begin
  Result := FDelphiControl.VisibleDockClientCount;
end;

function TTaxonButton.Get_WordWrap: WordBool;
begin
  Result := FDelphiControl.WordWrap;
end;

function TTaxonButton.IsRightToLeft: WordBool;
begin
  Result := FDelphiControl.IsRightToLeft;
end;

function TTaxonButton.UseRightToLeftReading: WordBool;
begin
  Result := FDelphiControl.UseRightToLeftReading;
end;

function TTaxonButton.UseRightToLeftScrollBar: WordBool;
begin
  Result := FDelphiControl.UseRightToLeftScrollBar;
end;

procedure TTaxonButton._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(FDelphiControl.Font, Value);
end;

procedure TTaxonButton.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
   MessageDlg (Get_MetaDataForTable, MtInformation,[mbOk],0);


end;

procedure TTaxonButton.InitiateAction;
begin
  FDelphiControl.InitiateAction;
end;

procedure TTaxonButton.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TTaxonButton.Set_Cancel(Value: WordBool);
begin
  FDelphiControl.Cancel := Value;
end;

procedure TTaxonButton.Set_Caption(const Value: WideString);
begin
  FDelphiControl.Caption := TCaption(Value);
end;

procedure TTaxonButton.Set_Default(Value: WordBool);
begin
  FDelphiControl.Default := Value;
end;

procedure TTaxonButton.Set_DoubleBuffered(Value: WordBool);
begin
  FDelphiControl.DoubleBuffered := Value;
end;

procedure TTaxonButton.Set_DragCursor(Value: Smallint);
begin
  FDelphiControl.DragCursor := TCursor(Value);
end;

procedure TTaxonButton.Set_DragMode(Value: TxDragMode);
begin
  FDelphiControl.DragMode := TDragMode(Value);
end;

procedure TTaxonButton.Set_Enabled(Value: WordBool);
begin
  FDelphiControl.Enabled := Value;
end;

procedure TTaxonButton.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(FDelphiControl.Font, Value);
end;

procedure TTaxonButton.Set_Visible(Value: WordBool);
begin
  FDelphiControl.Visible := Value;
end;

procedure TTaxonButton.Set_WordWrap(Value: WordBool);
begin
  FDelphiControl.WordWrap := Value;
end;

procedure TTaxonButton.SetSubComponent(IsSubComponent: WordBool);
begin
  FDelphiControl.SetSubComponent(IsSubComponent);
end;

function TTaxonButton.CheckCanSave: WordBool;
begin
  result:= true;
end;

procedure TTaxonButton.DoAdd;
begin

end;

procedure TTaxonButton.DoCancel;
begin

end;

procedure TTaxonButton.DoDelete;
begin

end;

procedure TTaxonButton.DoEditMode;
begin

end;

procedure TTaxonButton.DoItemChange(const iTableName,
  iKeyValue: WideString);
begin
  FTablename := iTableName;
  FKeyValue := iKeyValue;

end;

procedure TTaxonButton.DoSave;
begin

end;

function TTaxonButton.Get_Description: WideString;
begin
   result := 'Add a button to the Taxon Dictionary window which when clicked provides addtional information on the selected taxa';
end;

function TTaxonButton.Get_FormName: WideString;
begin
   result := 'TfrmTaxonDictbrowser';
end;

function TTaxonButton.Get_ImageFileName: WideString;
begin

end;

function TTaxonButton.Get_Name: WideString;
begin
  result:='Taxon Dictionary Extra Info v 6.22'
end;

procedure TTaxonButton.Install(const iInstalledFilePath: WideString);
begin

end;

procedure TTaxonButton.SetForm(const iForm: IRecorderForm);
var
parent : IrecorderControl;
begin
   self.Set_Caption('Extra Info');
 //   parent := iForm.Control['pnlselection'] as IRecorderControl;
   iForm.EmbedActiveX(self as IUnknown,CLASS_TaxonButton,parent,150,32,80,20);

end;

function TTaxonButton.Get_MetaDataForTable: widestring;
var
 IConn: TADOConnection;
 IRecorder : IRECORDER2000;
 IResult  : _Recordset;
 SQlString : string;
 I : integer;
 RTvk : string;
 Orgparent : string;
 MessageLine : array [0..25] of string;
 begin
  IConn:=TADOConnection.Create(nil);
  IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  IConn.ConnectionString:=IRecorder.ConnectionString;
  IConn.open;
  IRecorder.SetApplicationSecurity(IConn.ConnectionObject);
  Screen.Cursor := crHourGlass;
  If FkeyValue <> '' then

  begin

  SQlString := 'Select T.Taxon_key,TV.Taxon_Version_Key,TLI.Taxon_List_Item_Key,TLT.Short_name,TL.Taxon_List_Key, TG.Taxon_Group_Name,T.Item_Name,  TV.attribute, T.Authority,TV.System_Supplied_Data ' +
  ' FROM Taxon T INNER JOIN Taxon_version TV ON TV.Taxon_Key = T.Taxon_Key INNER JOIN Taxon_List_Item TLI ' +
  ' ON TLI.Taxon_Version_Key = TV.Taxon_Version_Key INNER JOIN Taxon_List_Version TLV ON TLV.taxon_List_Version_Key = TLI.Taxon_List_Version_Key ' +
  ' INNER JOIN Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
  ' INNER JOIN Taxon_List_Type TLT ON TLT.Taxon_List_Type_Key = TL.Taxon_List_Type_Key ' +

  ' LEFT JOIN Taxon_Group TG ON TG.Taxon_Group_Key = TV.Output_group_Key ' +
  ' WHERE TLI.Taxon_List_Item_Key = ''' + FKeyValue + '''';
   IResult := IConn.Execute(SqlString);

  IF Iresult.fields[9].value = false then
     MessageLine[1] := CRLF + '!!! User Added Taxa !!! ' + CRLF + CRLF;
  MessageLine[0] :=  Iresult.fields[6].value + ' ' + vartostr(Iresult.fields[7].value)  + ' ' +   vartostr(Iresult.fields[8].value) + CRLF +  CRLF;
  MessageLine[3] := 'Taxon Key = ' + Iresult.fields[0].value + CRLF;
  MessageLine[4] := 'Taxon Version Key = ' + Iresult.fields[1].value + CRLF;
  MessageLine[5] := 'Taxon List Item Key = ' + Iresult.fields[2].value + CRLF;
  MessageLine[6] := 'Taxon List Type = ' + Iresult.fields[3].value + CRLF;
  MessageLine[7] := 'Taxon List Key = ' + Iresult.fields[4].value + CRLF;
  MessageLine[8] := 'Taxon Group = ' + vartostr(Iresult.fields[5].value)  + CRLF;

  SQlString := ' Select ITN.Preferred_Name, ITN.Recommended_taxon_List_Item_Key, ITN2.Actual_Name, ITN2.Common_Name, ' +
  ' TLI.Taxon_Version_Key, TL.Item_Name,ITN.Sort_Order ' +
  ' FROM Index_Taxon_Name ITN INNER JOIN INdex_taxon_Name ITN2 ON ITN2.Taxon_List_Item_Key =  ITN.Recommended_Taxon_List_Item_Key ' +
  ' INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Recommended_taxon_List_Item_Key ' +
  ' INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_key = ITN2.Taxon_List_Version_key ' +
  ' INNER JOIN Taxon_List  TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
  ' WHERE ITN.Taxon_List_Item_Key = ''' + FKeyValue + '''';
    IResult := IConn.Execute(SqlString);
  MessageLine[9] := 'Preferred Name (This list)  = ' + Iresult.fields[0].value + CRLF;
  MessageLine[10] := 'Recommended Taxon List Item Key  = ' + Iresult.fields[1].value + CRLF;
  MessageLine[11] := 'Recommened Taxon Version Key  = ' + Iresult.fields[4].value + CRLF;
  MessageLine[12] := 'Recommended Name  = ' + Iresult.fields[2].value + CRLF;
  MessageLine[13] := 'Recommended Common Name  = ' + Iresult.fields[3].value + CRLF;
  MessageLine[14] := 'Recommended Sort Order = ' +   Iresult.fields[6].value + CRLF;
  MessageLine[15] := 'Recommended List = ' + Iresult.fields[5].value + CRLF;
  Rtvk :=   Iresult.fields[4].value;

  SQLString  := 'Select ORG.Organism_Key,Org.Taxon_Version_Key, T.Item_Name, Org.Parent_Key, Org.Redundant_Flag ' +
  ' FROM ORGANISM ORG ' +
  ' INNER JOIN Taxon_Version TV ON TV.taxon_Version_Key = ORG.Taxon_Version_Key ' +
  ' INNER JOIN Taxon T On T.Taxon_Key = TV.Taxon_Key ' +
  ' WHERE ORG.taxon_Version_Key = ''' + RTvk + '''';

    IResult := IConn.Execute(SqlString);
   If Iresult.EOF = false then
    begin
       If vartostr(Iresult.fields[4].value) = 'Y' then
            MessageLine[1] := CRLF + '!!! The NHM consider this Organism REDUNDANT !!! ' + CRLF + CRLF;

      MessageLine[16] := 'Organism Key = ' + Iresult.fields[0].value + CRLF;
      MessageLine[17] := 'Organism Taxon Version Key = ' + Iresult.fields[1].value + CRLF;
      MessageLine[18] := 'Organism Name = ' + vartostr(Iresult.fields[2].value) + CRLF;



      OrgParent :=  vartostr(Iresult.fields[3].value);

    end
    else
        MessageLine[16] := 'No linked Organism '+ CRLF;

    SQLString :=  'Select Org.Organism_Key,Org.Taxon_Version_Key, T.Item_Name ' +
       ' FROM ORGANISM ORG ' +
       ' INNER JOIN Taxon_Version TV ON TV.taxon_Version_Key = ORG.Taxon_Version_Key ' +
       ' INNER JOIN Taxon T On T.Taxon_Key = TV.Taxon_Key ' +
       ' WHERE ORG.Organism_Key  = '''+ OrgParent + '''';
       IResult := IConn.Execute(SqlString);
        If Iresult.EOF = false then
         begin
           MessageLine[19] := 'Organism Parent Key = ' + Orgparent + CRLF;
          MessageLine[20] := 'Organism Parent Taxon Version Key = ' + Iresult.fields[1].value + CRLF;
          MessageLine[21] := 'Organism Parent Name = ' + Iresult.fields[2].value + CRLF;
          MessageLine[22] := ' ' + CRLF;
         end;

    SQLString := ' Select count (Taxon_List_Item_key) FROM Index_taxon_designation ITD ' +
   ' WHERE ITD.Taxon_List_Item_Key = ''' + FKeyValue + '''';
   IResult := IConn.Execute(SqlString);
     If Iresult.fields[0].value <> 0  then
        MessageLine[23] := 'This taxa has designations ' + CRLF + CRLF
     else
        MessageLine[24] := 'This taxa has no designations '  ;



  // SQLString := ' Select count (Taxon_Determination_key) FROM Taxon_Determination TDET ' +
  // ' WHERE TDET.Taxon_List_Item_Key = ''' + FKeyValue + '''';
  // IResult := IConn.Execute(SqlString);
  // MessageLine[24] := 'Number of Records using = ' + inttostr(Iresult.fields[0].value) + CRLF +CRLF ;
   //MessageLine[25] := 'Hierarchy = '  ;
   //SQLString := 'Select ITN2.Actual_Name from Index_Taxon_Group ITG ' +
   //' INNER JOIN Index_Taxon_Name ITN ON ITN.Recommended_Taxon_List_Item_Key = ITG.Taxon_List_Item_Key ' +
   //' INNER JOIN Index_Taxon_name ITN2 ON ITN2.Taxon_List_Item_Key = ITG.Contained_List_Item_Key ' +
   //' WHERE ITN.Taxon_List_Item_Key = ''' + FKeyValue + ''' ORDER BY ITN.Sort_Order';
   //  IResult := IConn.Execute(SqlString);

   //  while IResult.eof = false Do

   //  Begin
   //      MessageLine[25]  := MessageLine[25]  +  IResult.Fields[0].Value + ',' ;
   //      IResult.movenext;
   //  end;





   for I := 0 to 24 Do
      Result := Result + MessageLine[I];


  end;
  If IConn <> nil then
     begin
          IConn.Close;
          IConn.Free;

     end;
  Screen.Cursor := crDefault;
end;

initialization
  TActiveXControlFactory.Create(
    ComServer,
    TTaxonButton,
    TButton,
    Class_TaxonButton,
    1,
    '',
    0,
    tmApartment);
end.
