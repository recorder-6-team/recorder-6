unit MetaButtonImpl1;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, Controls, Graphics, Menus, Forms, StdCtrls,
  ComServ, StdVCL, AXCtrls, ObsAdditionalInfo_TLB,Recorder2000_TLB,Variants;

type
  TMetaButton = class(TActiveXControl, IMetaButton,IRecorderAddin,IRecorderFormEvents)
  private
    { Private declarations }
    FTableName : string;
    FkeyValue : string;
    FDelphiControl: TButton;
    FEvents: IMetaButtonEvents;
    procedure ClickEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    function Get_ToccMetaData: WideString;
    function Get_SampleInfo: Widestring;
    function Get_MetaDataForTable: WideString;
    function Get_EventInfo: Widestring;
    function Get_BioInfo: Widestring;
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
    {IRecorderAddin}
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    {IrecorderFormEvents}
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

uses ComObj,dialogs,ADODB,sysutils;

{ TMetaButton }

procedure TMetaButton.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  {TODO: Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_MetaButtonPage); }
end;

procedure TMetaButton.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IMetaButtonEvents;
end;

procedure TMetaButton.InitializeControl;
begin
  FDelphiControl := Control as TButton;
  FDelphiControl.OnClick := ClickEvent;
  FDelphiControl.OnKeyPress := KeyPressEvent;
end;

function TMetaButton.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  Result := FDelphiControl.DrawTextBiDiModeFlagsReadingOnly;
end;

function TMetaButton.Get_AlignDisabled: WordBool;
begin
  Result := FDelphiControl.AlignDisabled;
end;

function TMetaButton.Get_Cancel: WordBool;
begin
  Result := FDelphiControl.Cancel;
end;

function TMetaButton.Get_Caption: WideString;
begin
  Result := WideString(FDelphiControl.Caption);
end;

function TMetaButton.Get_Default: WordBool;
begin
  Result := FDelphiControl.Default;
end;

function TMetaButton.Get_DoubleBuffered: WordBool;
begin
  Result := FDelphiControl.DoubleBuffered;
end;

function TMetaButton.Get_DragCursor: Smallint;
begin
  Result := Smallint(FDelphiControl.DragCursor);
end;

function TMetaButton.Get_DragMode: TxDragMode;
begin
  Result := Ord(FDelphiControl.DragMode);
end;

function TMetaButton.Get_Enabled: WordBool;
begin
  Result := FDelphiControl.Enabled;
end;

function TMetaButton.Get_Font: IFontDisp;
begin
  GetOleFont(FDelphiControl.Font, Result);
end;

function TMetaButton.Get_Visible: WordBool;
begin
  Result := FDelphiControl.Visible;
end;

function TMetaButton.Get_VisibleDockClientCount: Integer;
begin
  Result := FDelphiControl.VisibleDockClientCount;
end;

function TMetaButton.Get_WordWrap: WordBool;
begin
  Result := FDelphiControl.WordWrap;
end;

function TMetaButton.IsRightToLeft: WordBool;
begin
  Result := FDelphiControl.IsRightToLeft;
end;

function TMetaButton.UseRightToLeftReading: WordBool;
begin
  Result := FDelphiControl.UseRightToLeftReading;
end;

function TMetaButton.UseRightToLeftScrollBar: WordBool;
begin
  Result := FDelphiControl.UseRightToLeftScrollBar;
end;

procedure TMetaButton._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(FDelphiControl.Font, Value);
end;

procedure TMetaButton.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
  MessageDlg (Get_MetaDataForTable, MtInformation,[mbOk],0);

end;

procedure TMetaButton.InitiateAction;
begin
  FDelphiControl.InitiateAction;
end;

procedure TMetaButton.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TMetaButton.Set_Cancel(Value: WordBool);
begin
  FDelphiControl.Cancel := Value;
end;

procedure TMetaButton.Set_Caption(const Value: WideString);
begin
  FDelphiControl.Caption := TCaption(Value);
end;

procedure TMetaButton.Set_Default(Value: WordBool);
begin
  FDelphiControl.Default := Value;
end;

procedure TMetaButton.Set_DoubleBuffered(Value: WordBool);
begin
  FDelphiControl.DoubleBuffered := Value;
end;

procedure TMetaButton.Set_DragCursor(Value: Smallint);
begin
  FDelphiControl.DragCursor := TCursor(Value);
end;

procedure TMetaButton.Set_DragMode(Value: TxDragMode);
begin
  FDelphiControl.DragMode := TDragMode(Value);
end;

procedure TMetaButton.Set_Enabled(Value: WordBool);
begin
  FDelphiControl.Enabled := Value;
end;

procedure TMetaButton.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(FDelphiControl.Font, Value);
end;

procedure TMetaButton.Set_Visible(Value: WordBool);
begin
  FDelphiControl.Visible := Value;
end;

procedure TMetaButton.Set_WordWrap(Value: WordBool);
begin
  FDelphiControl.WordWrap := Value;
end;

procedure TMetaButton.SetSubComponent(IsSubComponent: WordBool);
begin
  FDelphiControl.SetSubComponent(IsSubComponent);
end;

procedure TMetaButton.DoAdd;
begin

end;

procedure TMetaButton.DoCancel;
begin

end;

procedure TMetaButton.DoDelete;
begin

end;

procedure TMetaButton.DoEditMode;
begin

end;

procedure TMetaButton.DoItemChange(const iTableName,
  iKeyValue: WideString);
begin
  FTablename := iTableName;
  FKeyValue := iKeyValue;
end;

procedure TMetaButton.DoSave;
begin

end;

function TMetaButton.Get_Description: WideString;
begin
  result:= 'Metadata addin';
end;

function TMetaButton.Get_FormName: WideString;
begin
  result:= 'TFrmObservations';
end;

function TMetaButton.Get_ImageFileName: WideString;
begin

end;

function TMetaButton.Get_Name: WideString;
begin
  result:= 'Extra metadata';
end;

procedure TMetaButton.Install(const iInstalledFilePath: WideString);

begin

end;         {IRecorderAddin}
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    {IrecorderFormEvents}
    function Get_FormName: WideString; safecall;
    procedure DoItemChange(const iTableName: WideString; const iKeyValue: WideString); safecall;
    procedure DoSave; safecall;
    procedure DoCancel; safecall;
    procedure SetForm(const iForm: IRecorderForm); safecall;
    procedure DoEditMode; safecall;
    procedure DoAdd; safecall;
    procedure DoDelete; safecall;
    function CheckCanSave: WordBool; safecall;

procedure TMetaButton.SetForm(const iForm: IRecorderForm);
var
parent : IrecorderControl;
begin
  self.Set_Caption('Extra Info.');
  parent := iform.control['pnlbuttons'] as irecordercontrol;
  iform.EmbedActiveX(self as iunknown,class_metabutton,parent, 400,5,80,28);
end;


function TMetaButton.CheckCanSave: WordBool;
begin
    result := true
end;


function TMetaButton.Get_ToccMetaData: WideString;
var
IConn: TADOConnection;
IRecorder: IRecorder2000;
IResult: _Recordset;

ActualName: string;
ActualAuthor: string;
ActualAttribute: string;
ActualKey: string;
ActualDictionary: string;
RecommendedName: string;
RecommendedAuthor: string;
RecommendedAttribute: string;
RecommendedKey: string;
TaxonGroup: string;
ZeroAbundance: string;
TaxonRank: string;
Recorder3Key :string;
ExternalKey : string;
begin

    IConn:=TADOConnection.Create(nil);
    IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
    IConn.ConnectionString:=IRecorder.ConnectionString;
    IConn.open;
    IRecorder.SetApplicationSecurity(IConn.ConnectionObject);

    IResult := IConn.Execute('Select ITN.Actual_Name as ActualName,ITN.Authority as ActualAuth, ' +
      ' TL.Item_Name as Dict , ITN2.Actual_Name as RecName,ITN2.Authority as RecAuth, '  +
      ' ITN.Taxon_List_Item_Key as ActualKey, ITN2.Taxon_List_Item_Key as RecKey, Tocc.Zero_Abundance ' +
      ' FROM Taxon_Occurrence TOCC' +
      ' INNER JOIN Taxon_Determination TDET ' +
      ' ON TDET.Taxon_Occurrence_Key = TOCC.Taxon_Occurrence_Key ' +
      ' AND TDET.Preferred = 1 ' +
      ' INNER JOIN Index_Taxon_Name ITN ' +
      ' ON ITN.Taxon_List_Item_Key = TDET.Taxon_List_Item_Key ' +
      ' INNER JOIN Index_Taxon_Name ITN2 ' +
      ' ON ITN2.Taxon_List_Item_Key = ITN.Recommended_Taxon_List_Item_Key ' +
      ' INNER JOIN Taxon_List_Version TLV ' +
      ' ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_key ' +
      ' INNER JOIN Taxon_List TL ' +
      ' ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
      ' WHERE TOCC.Taxon_Occurrence_Key = ''' + FKeyValue + '''' , cmdText,[]);
      ActualName := Iresult.Fields.Item[0].value;
      ActualAuthor := vartostr(Iresult.Fields.Item[1].value);
      ActualDictionary := Iresult.Fields.Item[2].value;
      RecommendedName := Iresult.Fields.Item[3].value;
      RecommendedAuthor  := vartostr(Iresult.Fields.Item[4].value);
      ActualKey := Iresult.Fields.Item[5].value;
      RecommendedKey :=  Iresult.Fields.Item[6].value;

      if   Iresult.Fields.Item[7].value = true then
         zeroAbundance := 'Occurrence IS held as having zero abundance '
      else
         zeroabundance := 'Occurrence IS NOT held as having zero abundance ';

      IResult:= IConn.Execute('Select Attribute, Taxon_Group_Name, Long_Name FROM  '  +
        ' Taxon_Version TV ' +
        ' INNER JOIN Taxon_List_Item TLI ' +
        ' ON TLI.Taxon_version_key = TV.Taxon_Version_Key' +
        ' LEFT JOIN Taxon_Group TG ' +
        ' ON TG.Taxon_Group_Key = TV.Output_Group_Key ' +
        ' INNER JOIN  Taxon_Rank TRK ' +
        ' ON TRK.Taxon_rank_Key = TLI.Taxon_Rank_Key ' +
        ' where TLI.Taxon_List_Item_Key = ''' + ActualKey + '''' , cmdText,[]);

      ActualAttribute := vartostr(Iresult.Fields.Item[0].value);
      if ActualAttribute = 'unavailable' then ActualAttribute := '';
      TaxonGroup :=  vartostr(Iresult.Fields.Item[1].value);
      TaxonRank :=  vartostr(Iresult.Fields.Item[2].value);

      IResult:= IConn.Execute('Select Attribute FROM  '  +
        ' Taxon_Version TV ' +
        ' INNER JOIN Taxon_List_Item TLI ' +
        ' ON TLI.Taxon_version_key = TV.Taxon_Version_Key' +
        ' where  TLI.Taxon_List_Item_Key = ''' + RecommendedKey + '''' , cmdText,[]);

      RecommendedAttribute := vartostr(Iresult.Fields.Item[0].value);
      if RecommendedAttribute = 'unavailable' then RecommendedAttribute := '';

      Try
         IResult:= IConn.Execute('Select R3_RecordNumber FROM  '  +
        ' R6Translate ' +
        ' WHERE R6Translate.Taxon_Occurrence_Key = ''' + FKeyValue + '''' , cmdText,[]);
        if Iresult.EOF = false then
          Recorder3Key :=  'Recorder3 Record Number = ' + inttostr(Iresult.Fields.Item[0].value) +  #13#10
        else
          Recorder3Key := 'Recorder3 Record Number = ' + 'No key recorded' +  #13#10 ;
      except
          Recorder3Key := '';
      end;

      Try
         IResult:= IConn.Execute('Select External_GUI FROM  '  +
        ' External_GUI_Link ' +
        ' WHERE External_GUI_Link.Taxon_Occurrence_Key = ''' + FKeyValue + '''' , cmdText,[]);
        if Iresult.EOF = false then
          ExternalKey :=  'External GUI = ' + inttostr(Iresult.Fields.Item[0].value) +  #13#10
        else
          ExternalKey := 'External GUI = ' + 'No key recorded' +  #13#10 ;
      except
          ExternalKey := '';
      end;
      result := 'TAXON_OCCURRENCE_KEY = ' + Fkeyvalue +   #13#10 + #13#10 +   Recorder3Key
        + ExternalKey +
        'Taxon as recorded = ' + ActualName + ' ' + ActualAttribute + ' ' + ActualAuthor  +  #13#10  +
        'Taxon Rank = '  + TaxonRank + #13#10+
        'Actual Dictionary = ' + ActualDictionary + #13#10+
        'Recommended taxon =  ' + RecommendedName + ' ' + RecommendedAttribute + ' ' + ActualAuthor +  #13#10  +
        'Taxon Group = ' + TaxonGroup +  #13#10  +
        'Actual Taxon_List_Item_Key  = ' + ActualKey +  #13#10  +
        'Recommended Taxon_List_Item_Key  = ' + RecommendedKey + #13#10 + ZeroAbundance;





     If IConn <> nil then
     begin
          IConn.Close;
          IConn.Free;
     end;


end;

function TMetaButton.Get_MetaDataForTable: WideString;
begin

 if FTableName = 'TAXON_OCCURRENCE' then
    begin
        result :=  Get_ToccMetaData;
    end
   else if
    FTableName = 'SAMPLE' then  result :=  Get_SampleInfo
   else if
     FTableName = 'SURVEY_EVENT' then  result :=  Get_EventInfo
   else if
     FTableName = 'BIOTOPE_OCCURRENCE' then  result :=  Get_BioInfo
  else
    result:= 'No additional information available for ' + FTablename;
end;

function TMetaButton.Get_BioInfo: Widestring;
var
IConn : TADOConnection;
IRecorder : IRecorder2000;
IResult : _Recordset;
BioTopeClass: string;
BioTopeShort: string;
BioTopeFull: string;
BioOriginalCode: string;

begin
  IConn:=TADOConnection.Create(nil);
  IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  IConn.ConnectionString:=IRecorder.ConnectionString;
  IConn.open;
  IRecorder.SetApplicationSecurity(IConn.ConnectionObject);
  IResult:= IConn.Execute('Select BC.Long_Name,B.Short_Term,B.Full_Term, B.Original_Code ' +
    ' FROM BIOTOPE_CLASSIFICATION BC ' +
    ' INNER JOIN BIOTOPE_CLASSIFICATION_VERSION BCV ' +
    ' ON BCV.BIOTOPE_CLASSIFICATION_KEY = BC.BIOTOPE_CLASSIFICATION_KEY ' +
    ' INNER JOIN BIOTOPE_LIST_ITEM BLI ' +
    ' ON BLI.BT_CL_VERSION_KEY = BCV.BT_CL_VERSION_KEY ' +
    ' INNER JOIN BIOTOPE B ' +
    ' ON B.BIOTOPE_KEY = BLI.BIOTOPE_KEY ' +
    ' INNER JOIN BIOTOPE_DETERMINATION BDET ' +
    ' ON  BDET.BIOTOPE_LIST_ITEM_KEY = BLI.BIOTOPE_LIST_ITEM_KEY ' +
    ' INNER JOIN BIOTOPE_OCCURRENCE BOCC ' +
    ' ON BOCC.BIOTOPE_OCCURRENCE_KEY = BDET.BIOTOPE_OCCURRENCE_KEY ' +
    ' AND BDET.PREFERRED = 1 ' +
    ' WHERE BOCC.BIOTOPE_OCCURRENCE_KEY  = ''' + FKeyValue + '''' , cmdText,[]);

     BiotopeClass:= vartostr(IResult.Fields.Item[0].value);
     BiotopeShort:= vartostr(IResult.Fields.Item[1].value);
     BioTopeFull:= vartostr(IResult.Fields.Item[2].value);
     BioOriginalCode:= vartostr(Iresult.Fields.Item[3].value);
     result := 'BIOTOPE_OCCURRENCE_KEY = ' + Fkeyvalue +   #13#10 + #13#10 +
      'Biotope Short Name = ' + BiotopeShort  + #13#10  +
      'Biotope Full Name = ' + BiotopeFull +  #13#10  +
      'Original Code = ' + BioOriginalCode  +  #13#10  +
      'Classification  = ' + BioTopeClass;
     If IConn <> nil then
     begin
          IConn.Close;
          IConn.Free;
     end;

end;

function TMetaButton.Get_SampleInfo: Widestring;
var
IConn : TADOConnection;
IRecorder : IRecorder2000;
IResult : _Recordset;
SpatialRef: string;
SpatialRefQual: string;
TempRecorders: string;
Latitude:  real;
Longitude: real;

begin
  IConn:=TADOConnection.Create(nil);
  IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  IConn.ConnectionString:=IRecorder.ConnectionString;
  IConn.open;
  IRecorder.SetApplicationSecurity(IConn.ConnectionObject);
  IResult:= IConn.Execute('Select Spatial_Ref, Spatial_Ref_Qualifier, LAT, LONG, Recorders FROM SAMPLE S ' +
    ' WHERE S.Sample_Key = ''' + FKeyValue + '''' , cmdText,[]);
    SpatialRef := vartostr(IResult.Fields.Item[0].value);
    SpatialRefQual := vartostr(IResult.Fields.Item[1].value);
    Latitude := IResult.Fields.Item[2].value;
    Longitude := Iresult.Fields.Item[3].value;
    TempRecorders :=  vartostr(Iresult.Fields.Item[4].value);
    if TempRecorders = ''  then TempRecorders := 'None recorded';
    result := 'SAMPLE_KEY = ' + Fkeyvalue +   #13#10 + #13#10 +
      'Recorders (temp) = ' +   TempRecorders + #13#10  +
      'Spatial Ref as entered = ' + SpatialRef + ' (' +  SpatialRefQual + ')' + #13#10  +
      'Latitude as held = ' + FloattoStr(Latitude) +  #13#10  +
      'Longitude as held = ' + FloatToStr(Longitude);
    If IConn <> nil then
     begin
          IConn.Close;
          IConn.Free;
     end;

end;

function TMetaButton.Get_EventInfo: Widestring;
var
IConn : TADOConnection;
IRecorder : IRecorder2000;
IResult : _Recordset;
SpatialRef: string;
SpatialRefQual: string;
Latitude:  real;
Longitude: real;

begin
  IConn:=TADOConnection.Create(nil);
  IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  IConn.ConnectionString:=IRecorder.ConnectionString;
  IConn.open;
  IRecorder.SetApplicationSecurity(IConn.ConnectionObject);
  IResult:= IConn.Execute('Select Spatial_Ref, Spatial_Ref_Qualifier, LAT, LONG FROM SURVEY_EVENT SE ' +
    ' WHERE SE.Survey_Event_Key = ''' + FKeyValue + '''' , cmdText,[]);
    SpatialRef := vartostr(IResult.Fields.Item[0].value);
    SpatialRefQual := vartostr(IResult.Fields.Item[1].value);
    Latitude := IResult.Fields.Item[2].value;
    Longitude := Iresult.Fields.Item[3].value;
    result := 'SURVEY_EVENT_KEY  = ' + Fkeyvalue +   #13#10 + #13#10 +
      'Spatial Ref as entered = ' + SpatialRef + ' (' +  SpatialRefQual + ')' + #13#10  +
      'Latitude as held = ' + FloattoStr(Latitude) +  #13#10  +
      'Longitude as held = ' + FloatToStr(Longitude);

   If IConn <> nil then
     begin
          IConn.Close;
          IConn.Free;
     end;

end;

initialization
  TActiveXControlFactory.Create(
    ComServer,
    TMetaButton,
    TButton,
    Class_MetaButton,
    1,
    '',
    0,
    tmApartment);


end.
