unit ObsAddInfoButtonImpl1;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, Controls, Graphics, Menus, Forms, StdCtrls,
  ComServ, StdVCL, AXCtrls, ObsAdditionalInfo_TLB,Recorder2000_TLB,Variants;

type
  TObsAddInfoButton = class(TActiveXControl, IObsAddInfoButton,IRecorderAddin,IRecorderFormEvents)
  private
    { Private declarations }
    FDelphiControl: TButton;
    FEvents: IObsAddInfoButtonEvents;
    FTableName : string;
    FkeyValue : string;
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

{ TObsAddInfoButton }

procedure TObsAddInfoButton.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  {TODO: Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_ObsAddInfoButtonPage); }
end;

procedure TObsAddInfoButton.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IObsAddInfoButtonEvents;
end;

procedure TObsAddInfoButton.InitializeControl;
begin
  FDelphiControl := Control as TButton;
  FDelphiControl.OnClick := ClickEvent;
  FDelphiControl.OnKeyPress := KeyPressEvent;
end;

function TObsAddInfoButton.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  Result := FDelphiControl.DrawTextBiDiModeFlagsReadingOnly;
end;

function TObsAddInfoButton.Get_AlignDisabled: WordBool;
begin
  Result := FDelphiControl.AlignDisabled;
end;

function TObsAddInfoButton.Get_Cancel: WordBool;
begin
  Result := FDelphiControl.Cancel;
end;

function TObsAddInfoButton.Get_Caption: WideString;
begin
  Result := WideString(FDelphiControl.Caption);
end;

function TObsAddInfoButton.Get_Default: WordBool;
begin
  Result := FDelphiControl.Default;
end;

function TObsAddInfoButton.Get_DoubleBuffered: WordBool;
begin
  Result := FDelphiControl.DoubleBuffered;
end;

function TObsAddInfoButton.Get_DragCursor: Smallint;
begin
  Result := Smallint(FDelphiControl.DragCursor);
end;

function TObsAddInfoButton.Get_DragMode: TxDragMode;
begin
  Result := Ord(FDelphiControl.DragMode);
end;

function TObsAddInfoButton.Get_Enabled: WordBool;
begin
  Result := FDelphiControl.Enabled;
end;

function TObsAddInfoButton.Get_Font: IFontDisp;
begin
  GetOleFont(FDelphiControl.Font, Result);
end;

function TObsAddInfoButton.Get_Visible: WordBool;
begin
  Result := FDelphiControl.Visible;
end;

function TObsAddInfoButton.Get_VisibleDockClientCount: Integer;
begin
  Result := FDelphiControl.VisibleDockClientCount;
end;

function TObsAddInfoButton.Get_WordWrap: WordBool;
begin
  Result := FDelphiControl.WordWrap;
end;

function TObsAddInfoButton.IsRightToLeft: WordBool;
begin
  Result := FDelphiControl.IsRightToLeft;
end;

function TObsAddInfoButton.UseRightToLeftReading: WordBool;
begin
  Result := FDelphiControl.UseRightToLeftReading;
end;

function TObsAddInfoButton.UseRightToLeftScrollBar: WordBool;
begin
  Result := FDelphiControl.UseRightToLeftScrollBar;
end;

procedure TObsAddInfoButton._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(FDelphiControl.Font, Value);
end;

procedure TObsAddInfoButton.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
  MessageDlg (Get_MetaDataForTable, MtInformation,[mbOk],0);
end;

procedure TObsAddInfoButton.InitiateAction;
begin
  FDelphiControl.InitiateAction;
end;

procedure TObsAddInfoButton.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TObsAddInfoButton.Set_Cancel(Value: WordBool);
begin
  FDelphiControl.Cancel := Value;
end;

procedure TObsAddInfoButton.Set_Caption(const Value: WideString);
begin
  FDelphiControl.Caption := TCaption(Value);
end;

procedure TObsAddInfoButton.Set_Default(Value: WordBool);
begin
  FDelphiControl.Default := Value;
end;

procedure TObsAddInfoButton.Set_DoubleBuffered(Value: WordBool);
begin
  FDelphiControl.DoubleBuffered := Value;
end;

procedure TObsAddInfoButton.Set_DragCursor(Value: Smallint);
begin
  FDelphiControl.DragCursor := TCursor(Value);
end;

procedure TObsAddInfoButton.Set_DragMode(Value: TxDragMode);
begin
  FDelphiControl.DragMode := TDragMode(Value);
end;

procedure TObsAddInfoButton.Set_Enabled(Value: WordBool);
begin
  FDelphiControl.Enabled := Value;
end;

procedure TObsAddInfoButton.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(FDelphiControl.Font, Value);
end;

procedure TObsAddInfoButton.Set_Visible(Value: WordBool);
begin
  FDelphiControl.Visible := Value;
end;

procedure TObsAddInfoButton.Set_WordWrap(Value: WordBool);
begin
  FDelphiControl.WordWrap := Value;
end;

procedure TObsAddInfoButton.SetSubComponent(IsSubComponent: WordBool);
begin
  FDelphiControl.SetSubComponent(IsSubComponent);
end;

function TObsAddInfoButton.Get_Description: WideString;
begin
  result:= 'Adds a button to the Observation window which when clicked provides extra information on the selected item. This version is for R6 6.20 and above';
end;

function TObsAddInfoButton.Get_FormName: WideString;
begin
  result:= 'TFrmObservations';
end;

function TObsAddInfoButton.Get_ImageFileName: WideString;
begin

end;

function TObsAddInfoButton.Get_Name: WideString;
begin
  result:= 'Observation Extra Info. Button v 6.22';
end;

procedure TObsAddInfoButton.Install(const iInstalledFilePath: WideString);

begin

end;

procedure TObsAddInfoButton.DoAdd;
begin

end;

procedure TObsAddInfoButton.DoCancel;
begin

end;

procedure TObsAddInfoButton.DoDelete;
begin

end;

procedure TObsAddInfoButton.DoEditMode;
begin

end;

procedure TObsAddInfoButton.DoItemChange(const iTableName,
  iKeyValue: WideString);
begin
  FTablename := iTableName;
  FKeyValue := iKeyValue;
end;

procedure TObsAddInfoButton.DoSave;
begin

end;

procedure TObsAddInfoButton.SetForm(const iForm: IRecorderForm);
var
parent : IrecorderControl;
begin
  self.Set_Caption('Extra Info.');
  parent := iform.control['pnlbuttons'] as irecordercontrol;
  iform.EmbedActiveX(self as iunknown,class_ObsAddInfoButton,parent, 400,5,80,28);
end;

function TObsAddInfoButton.Get_ToccMetaData: WideString;
var
IConn: TADOConnection;
IRecorder: IRecorder2000;
IResult: _Recordset;

EnteredName: string;
EnteredAuthor: string;
EnteredAttribute: string;
EnteredKey: string;
EnteredDictionary: string;
RecommendedName: string;
RecommendedAuthor: string;
RecommendedAttribute: string;
RecommendedKey: string;
RecommendedDictionary: string;
PreferredName: string;
PreferredAuthor: string;
PreferredAttribute: string;
PreferredKey: string;
PreferredDictionary: string;


TaxonGroup: string;
ZeroAbundance: string;
TaxonRank: string;
Recorder3Key :string;
ExternalKey : string;
SRecorder : String;
begin

    IConn:=TADOConnection.Create(nil);
    IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
    IConn.ConnectionString:=IRecorder.ConnectionString;
    IConn.open;
    IRecorder.SetApplicationSecurity(IConn.ConnectionObject);

    IResult := IConn.Execute('Select ITN.Actual_Name as EnteredName,ITN.Authority as EnteredAuth, ' +
      ' ITN.Actual_Name_Attribute as EnteredAttribute,TL.Item_Name as EnteredDict, ITN.Taxon_List_Item_Key,Tocc.Zero_Abundance, S.Recorders,S.Recorders' +
      ' FROM Sample S INNER JOIN  Taxon_Occurrence TOCC' +
      ' ON TOCC.Sample_key = S.Sample_Key ' +
      ' INNER JOIN Taxon_Determination TDET ' +
      ' ON TDET.Taxon_Occurrence_Key = TOCC.Taxon_Occurrence_Key ' +
      ' AND TDET.Preferred = 1 ' +
      ' INNER JOIN Index_Taxon_Name ITN ' +
      ' ON ITN.Taxon_List_Item_Key = TDET.Taxon_List_Item_Key ' +
      ' INNER JOIN Taxon_List_Version TLV ' +
      ' ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_key ' +
      ' INNER JOIN Taxon_List TL ' +
      ' ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
      ' WHERE TOCC.Taxon_Occurrence_Key = ''' + FKeyValue + '''' , cmdText,[]);

      EnteredName := Iresult.Fields.Item[0].value;
      EnteredAuthor := vartostr(Iresult.Fields.Item[1].value);
      EnteredAttribute :=  vartostr(Iresult.Fields.Item[2].value);
      EnteredDictionary := Iresult.Fields.Item[3].value;
      EnteredKey :=  Iresult.Fields.Item[4].value;
      if   Iresult.Fields.Item[5].value = true then
         zeroAbundance := 'Occurrence IS held as having zero abundance '
      else
         zeroabundance := 'Occurrence IS NOT held as having zero abundance ';

      SRecorder :=  vartostr(Iresult.Fields.Item[6].value);
      If SRecorder = '' then SRecorder := 'None Recorded';

       IResult := IConn.Execute('Select ITN.Actual_Name as PreferredName,ITN.Authority as PreferredAuth, ' +
      ' ITN.Actual_Name_Attribute as PreferredAttribute,TL.Item_Name as PreferredDict, TLI2.Taxon_List_Item_Key, TR.SHORT_NAME, TG.TAXON_GROUP_NAME' +
      ' FROM Sample S INNER JOIN  Taxon_Occurrence TOCC' +
      ' ON TOCC.Sample_key = S.Sample_Key ' +
      ' INNER JOIN Taxon_Determination TDET ' +
      ' ON TDET.Taxon_Occurrence_Key = TOCC.Taxon_Occurrence_Key ' +
      ' AND TDET.Preferred = 1 ' +
      ' INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY = TDET.Taxon_List_Item_Key  ' +
      ' INNER JOIN TAXON_LIST_ITEM TLI2 ON TLI2.Taxon_List_ITem_Key = TLI.Preferred_Name   ' +
      ' INNER JOIN Index_Taxon_Name ITN ' +
      ' ON ITN.Taxon_List_Item_Key = TLI2.TAXON_LIST_ITEM_KEY ' +
      ' INNER JOIN Taxon_List_Version TLV ' +
      ' ON TLV.Taxon_List_Version_Key = TLI2.Taxon_List_Version_key ' +
      ' INNER JOIN Taxon_List TL ' +
      ' ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
      ' INNER JOIN TAXON_RANK TR ON TR.TAXON_RANK_KEY = TLI.TAXON_RANK_KEY ' +
      ' INNER JOIN TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY ' +
      ' INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY ' +
      ' WHERE TOCC.Taxon_Occurrence_Key = ''' + FKeyValue + '''' , cmdText,[]);

       PreferredName := Iresult.Fields.Item[0].value;
       PreferredAuthor := vartostr(Iresult.Fields.Item[1].value);
       PreferredAttribute :=  vartostr(Iresult.Fields.Item[2].value);
       PreferredDictionary := Iresult.Fields.Item[3].value;
       PreferredKey :=  Iresult.Fields.Item[4].value;
       TaxonRank :=   vartostr(Iresult.Fields.Item[5].value);
       TaxonGroup :=   vartostr(Iresult.Fields.Item[6].value);


       IResult := IConn.Execute('Select ITN2.Actual_Name as RecommendeName,ITN2.Authority as RecommendedAuth, ' +
      ' ITN2.Actual_Name_Attribute as RecommendedAttribute,TL.Item_Name as RecommendedDict, ITN2.Taxon_List_Item_Key ' +
      ' FROM Taxon_Occurrence TOCC INNER JOIN     ' +
      ' Taxon_Determination TDET  ON TDET.TAXON_OCCURRENCE_KEY = TOCC.TAXON_OCCURRENCE_KEY   AND TDET.PREFERRED = 1' +
      ' INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY = TDET.TAXON_LIST_ITEM_KEY  ' +
      ' INNER JOIN INDEX_TAXON_NAME ITN2 ON ITN2.TAXON_LIST_ITEM_KEY = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY ' +
      ' INNER JOIN TAXON_LIST_VERSION TLV ON TLV.TAXON_LIST_VERSION_KEY = ITN2.TAXON_LIST_VERSION_KEY ' +
      ' INNER JOIN TAXON_LIST TL ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY '     +
      ' WHERE TOCC.Taxon_Occurrence_Key = ''' + FKeyValue + '''' , cmdText,[]);

       RecommendedName := Iresult.Fields.Item[0].value;
       RecommendedAuthor := vartostr(Iresult.Fields.Item[1].value);
       RecommendedAttribute :=  vartostr(Iresult.Fields.Item[2].value);
       RecommendedDictionary := Iresult.Fields.Item[3].value;
       RecommendedKey :=  Iresult.Fields.Item[4].value;

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
         IResult:= IConn.Execute('Select Detail FROM  '  +
        ' Taxon_Private_Data ' +
        ' WHERE Taxon_Private_Data.Taxon_Private_Type_Key = ' +
        '''R6TEAM1800000001''' +
        ' AND Taxon_Private_Data.Taxon_Occurrence_Key = ''' + FKeyValue + '''' , cmdText,[]);
        if Iresult.EOF = false then
          ExternalKey :=  'External Key = ' + vartostr(Iresult.Fields.Item[0].value) +  #13#10
        else
          ExternalKey := 'External Key = ' + 'No key recorded' +  #13#10 ;
      except
          ExternalKey := '';
      end;
      result := 'TAXON_OCCURRENCE_KEY = ' + Fkeyvalue +   #13#10 + #13#10 +   Recorder3Key
        + ExternalKey +
        'Taxon Rank = '  + TaxonRank + #13#10+
        'Taxon Group = ' + TaxonGroup +  #13#10  +    #13#10  +
        'AS ENTERED '   +    #13#10  +
        'Taxon List Item Key = ' + ' ' + EnteredKey   +  #13#10  +
        'Taxon  = ' + EnteredName + ' ' + EnteredAttribute + ' ' + EnteredAuthor  +  #13#10  +
        'Taxon list = ' + EnteredDictionary + #13#10+    #13#10  +
        'PREFERRED (refers to the specific taxon list) ' +    #13#10  +
        'Taxon List Item Key ' + ' ' + PreferredKey   +  #13#10  +
        'Taxon =  ' + PreferredName + ' ' + PreferredAttribute + ' ' + PreferredAuthor +  #13#10  +
        'Taxon list = ' + PreferredDictionary + #13#10   +    #13#10  +
        'RECOMMENDED' +    #13#10  +
        'Taxon List Item Key ' + ' ' + RecommendedKey   +  #13#10  +
        'Taxon =  ' + RecommendedName + ' ' + RecommendedAttribute + ' ' + RecommendedAuthor +  #13#10  +
        'Taxon list = ' + RecommendedDictionary + #13#10 +   #13#10   +

         ZeroAbundance +   #13#10  +   #13#10  +
        'Recorders (Temp) Name = ' + SRecorder ;





     If IConn <> nil then
     begin
          IConn.Close;
          IConn.Free;
     end;


end;

function TObsAddInfoButton.Get_MetaDataForTable: WideString;
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

function TObsAddInfoButton.Get_BioInfo: Widestring;
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

function TObsAddInfoButton.Get_SampleInfo: Widestring;
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

function TObsAddInfoButton.Get_EventInfo: Widestring;
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
function TObsAddInfoButton.CheckCanSave: WordBool;
begin
    result := true
end;

initialization
  TActiveXControlFactory.Create(
    ComServer,
    TObsAddInfoButton,
    TButton,
    Class_ObsAddInfoButton,
    1,
    '',
    0,
    tmApartment);
end.
