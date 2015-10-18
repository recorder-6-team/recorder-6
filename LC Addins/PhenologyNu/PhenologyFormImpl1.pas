unit PhenologyFormImpl1;
//--------------------------------------------------------------------------------------------
// Author:    Mike Weideli
// Date:      August 2015
// Version:   1
// Type:      Recorder addin
// Purpose:   Phenology plot. Generate a histogram showing the number of observations of a
//            taxon falling in periods of a week, fortnight or month. Result can be printed
//            or saved in a variety of formats.
// Based on tthe original Phenolgy Addin by Stuart Ball 2002
//
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, PhenologyNu_TLB, StdVcl,Recorder2000_TLB, ExtCtrls,Printers,OptionDialog,
  TeeProcs, TeEngine, Chart, Series, DragDropComponent,AddinClasses,DropTarget, ActnList, Menus,ADODB;


type
  TPeriod = (prWeek, prFortnight, prMonth);
  TPhenologyFormX = class(TActiveForm, IPhenologyFormX,IRecorderAddin,INewAction)
    Chart: TChart;
    Series1: TBarSeries;
    PopupMenu: TPopupMenu;
    mPeriod: TMenuItem;
    mWeek: TMenuItem;
    mFortnight: TMenuItem;
    mMonth: TMenuItem;
    mColour: TMenuItem;
    N1: TMenuItem;
    mPrint: TMenuItem;
    mSave: TMenuItem;
    N2: TMenuItem;
    ColorDialog: TColorDialog;
    PrintDialog: TPrintDialog;
    SaveDialog: TSaveDialog;
    ActionList: TActionList;
    aSave: TAction;
    aPrint: TAction;
    aApply: TAction;
    SetOptiions: TMenuItem;
    SetOptions: TMenuItem;
    ApplyOptions: TMenuItem;
    procedure mWeekClick(Sender: TObject);
    procedure mFortnightClick(Sender: TObject);
    procedure mMonthClick(Sender: TObject);
    procedure mColourClick(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure SetOptionsClick(Sender: TObject);
    procedure mApplyClick(Sender: TObject);
    procedure aApplyUpdate(Sender: TObject);
  private
    { Private declarations }

    {My declarations}
    FTLIKey:       TKeyString;
    FPeriod:       TPeriod;
    FColor:        TColor;
    FMin, FMax:    integer;
    FCounts:       Array [0..53] of integer;
    FNBNData: TADOConnection;     // the connection to NBNData
    FRecorder2000: IRecorder2000; // interface to Rec2002
    FOptions:  TFormpOption;


    {System declarations}
    FEvents: IPhenologyFormXEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);

    {My declarations }

    procedure SetUpForm;
    procedure DoChart;
    function GetCount(const TLIKey: string; const svKey :string): boolean;
    function GetAxisLabel(const iPeriod: integer): string;
    function Get_Recorder2000: IRecorder2000;
    function GetTaxonName: string;
    function GetTaxonListItemKey(iKey: TKeyList): string;
    function GetSQL(const TLIKey: string; svKey : string): string;
    function GetOptions: TformpOption;
    function GetSurveyName(aKey : string) : string;
    function GetSurveyKey: string;
    function GetDateWhere : string;
    function GetFormTitle(svKey : string) : string;
    function CheckForChildren : string;
  protected
   { Protected declarations }
   {Drag and drop}
    FSpeciesDropper : TJNCCDragDrop;
    procedure DropSpecies(const Sender: TObject;
          const iFormat : integer; const iSourceData: TKeyList;
          const iTextStrings : TstringList; var ioHandled : boolean);

    { IrecorderAddin }
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    function Get_NBNData: TADOConnection;
     {INewAction}
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
    {IFormCaption}
    function Get_FormCaption: WideString; safecall;
    { System Supplied }
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
  public
    { Public declarations }
    destructor Destroy; override;
    procedure Initialize; override;
    property Recorder2000: IRecorder2000 read Get_Recorder2000;
    property NBNData: TADOConnection read Get_NBNData;
    property Options: TformpOption read GetOptions;

  end;

implementation

uses ComObj, ComServ;

Const Quote = Char(39);


{$R *.DFM}

{ TPhenologyFormX }

procedure TPhenologyFormX.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_PhenologyFormXPage); }
end;

procedure TPhenologyFormX.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IPhenologyFormXEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TPhenologyFormX.Initialize;
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
  SetUpForm; // do out initialisation


end;

function TPhenologyFormX.Get_Active: WordBool;
begin
  Result := Active;
end;

function TPhenologyFormX.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TPhenologyFormX.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TPhenologyFormX.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TPhenologyFormX.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TPhenologyFormX.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TPhenologyFormX.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TPhenologyFormX.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TPhenologyFormX.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TPhenologyFormX.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TPhenologyFormX.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TPhenologyFormX.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TPhenologyFormX.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TPhenologyFormX.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TPhenologyFormX.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TPhenologyFormX.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TPhenologyFormX.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TPhenologyFormX.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TPhenologyFormX.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TPhenologyFormX.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TPhenologyFormX._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TPhenologyFormX.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TPhenologyFormX.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TPhenologyFormX.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TPhenologyFormX.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TPhenologyFormX.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TPhenologyFormX.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TPhenologyFormX.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TPhenologyFormX.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TPhenologyFormX.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TPhenologyFormX.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TPhenologyFormX.Set_AxBorderStyle(
  Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TPhenologyFormX.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TPhenologyFormX.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TPhenologyFormX.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TPhenologyFormX.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TPhenologyFormX.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TPhenologyFormX.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TPhenologyFormX.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TPhenologyFormX.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TPhenologyFormX.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TPhenologyFormX.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TPhenologyFormX.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TPhenologyFormX.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TPhenologyFormX.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TPhenologyFormX.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

//=============================================================================
// Recorder 2000 interface implementations
//=============================================================================
function TPhenologyFormX.Get_ActionCaption: WideString;
begin
     Result := 'Phenology';
end;

function TPhenologyFormX.Get_CanAddToToolbar: WordBool;
begin
     Result := True;
end;

function TPhenologyFormX.Get_Description: WideString;
begin
     Result := 'Phenology chart for a species dragged on to the window by Mike Weideli for JNCC';
end;

function TPhenologyFormX.Get_DimmedImageFilename: WideString;
begin
     Result := 'Bars.bmp';
end;

function TPhenologyFormX.Get_DisabledImageFileName: WideString;
begin
     Result := 'Bars.bmp';
end;

function TPhenologyFormX.Get_FormCaption: WideString;
begin
     Result := 'Phenology';
end;

function TPhenologyFormX.Get_Hint: WideString;
begin
     Result := 'Phenology chart for a species dragged on to the window';
end;

function TPhenologyFormX.Get_ImageFileName: WideString;
begin
     Result := 'Bars.bmp';
end;

function TPhenologyFormX.Get_Name: WideString;
begin
     Result := 'Phenology';
end;

function TPhenologyFormX.Get_ParentMenu: WideString;
begin
     Result := 'Reports';
end;

procedure TPhenologyFormX.Install(const iInstalledFilePath: WideString);
begin
     // nothing yet
end;

procedure TPhenologyFormX.SetUpForm;
begin
     // set up drag-and-drop
     FSpeciesDropper := TJNCCDragDrop.Create(Self);
     FSpeciesDropper.RegisterDropComponent(Chart, DropSpecies,
               ['TAXON_LIST_ITEM','TAXON_OCCURRENCE'],[CF_JNCCDATA]);
     // set up chart
     Chart.Series[0].Clear;
     FPeriod := prWeek;
     FColor  := Chart.Series[0].SeriesColor;
     FTLIKey := '';
     FMin := -1;
     FMax := -1;
end;

//-------------------------------------------------------------------------------
// respond to a species being dropped onto the chart
//-------------------------------------------------------------------------------
procedure TPhenologyFormX.DropSpecies(const Sender: TObject;
  const iFormat: integer; const iSourceData: TKeyList;
  const iTextStrings: TstringList; var ioHandled: boolean);
begin
  if iSourceData.Header.ItemCount > 0 then
  begin
       FTLIKey := GetTaxonListItemKey(iSourceData);
       DoChart;
  end;
end;

procedure TPhenologyFormX.mWeekClick(Sender: TObject);
begin
   FPeriod := prWeek;
   mWeek.Checked := True;
   Chart.BottomAxis.Title.Caption := 'Week';
   Chart.LeftAxis.Title.Caption := 'Number of records per week';
   DoChart;
end;

procedure TPhenologyFormX.mFortnightClick(Sender: TObject);
begin
    FPeriod := prFortnight;
    mFortnight.Checked := True;
    Chart.BottomAxis.Title.Caption := 'Fortnight';
    Chart.LeftAxis.Title.Caption := 'Number of records per fortnight';
    DoChart;
end;

procedure TPhenologyFormX.mMonthClick(Sender: TObject);
begin
    FPeriod := prMonth;
    mFortnight.Checked := True;
    Chart.BottomAxis.Title.Caption := 'Month';
    Chart.LeftAxis.Title.Caption := 'Number of records per month';
    DoChart;
end;

procedure TPhenologyFormX.mColourClick(Sender: TObject);
begin
     with ColorDialog do
     begin
          Color := FColor;
          If Execute then
          begin
             FColor := Color;
             DoChart;
          end;
     end;
end;

//-------------------------------------------------------------------------------
// this is the central driving routine that actually generates the chart
//-------------------------------------------------------------------------------
procedure TPhenologyFormX.DoChart;
var i: integer;
var SVkey : string;
begin
  // we cannot do anything until we have a taxon
  // Take out as slow FTLIKey := CheckForChildren;
  If FTLIKey = '' then exit;
    //Get Survey Key in case we need it
    sVkey := GetSurveyKey;
    // get the title
    with Chart.Title.Text do
    begin
      Clear;
      Add(GetFormTitle(svKey));
    end;
    // Clear the data
    Chart.SeriesList[0].Clear;
    // zero the array of counts
    For i:= 0 to 53 do
      FCounts[i] := 0;
      FMin := -1;
      FMax := -1;
      // get the information from the database
      If GetCount(FTLIKey,SvKey) then
      begin
          // find first period with data
          i := 0;
          While (FCounts[i] = 0) and (i<53) do
                inc(i);
          if FCounts[i] > 0 then FMin := i;

          // find last period with data
          i := 53;
          While (FCounts[i]=0) and (i>0) do
                dec(i);
          if FCounts[i] > 0 then FMax := i;

          // update the series on the chart providing we found something
          If (FMin >= 0) and (FMax >= 0) then
             For i := FMin to FMax do
                 With Chart.SeriesList[0] do
                      Add(FCounts[i], GetAxisLabel(i), FColor);
       // no observations were found
       end else ShowMessage('There are no observations of the chosen options');

       // repaint the chart to show the results
       Chart.Repaint;
end;
 //-------------------------------------------------------------------------------
// get the count of sample rows per period from the database assumes only one of each taxa in a sample
//-------------------------------------------------------------------------------
function TPhenologyFormX.GetCount(const TLIKey: string; const svKey : string  ): boolean;
var w,y: integer;
    iCursor: longint;
var oQuery: TADOQuery;
begin
     Result := False;
     iCursor := Screen.Cursor;
     Screen.Cursor := crHourGlass;
     oQuery := TADOQuery.Create(nil);
     oQuery.Connection := NBNData;
     oQuery.SQL.ADD(GetSQL(TLIKey,svKey));
     oQuery.CommandTimeout := 0;
     oQuery.Open;
     While not  oQuery.eof do
     Begin
       y := OQuery.FieldByName('nRecs').AsInteger;
       w := OQuery.FieldByName('Wk').AsInteger;
       If (w>=0) and (w<=53) then
         FCounts[w] := y;

       OQuery.Next;

       Result := True;

     end;
      oQuery.Close;
       oQuery.Free;
     Screen.Cursor := iCursor;

end;
//-------------------------------------------------------------------------------
// label weeks with "day month"
//-------------------------------------------------------------------------------
function TPhenologyFormX.GetAxisLabel(const iPeriod: integer): string;
var d: TDateTime;
    iDays: integer;
begin
  iDays := 7;
  case FPeriod of
    prWeek:       iDays := 7;
    prFortnight:  iDays := 14;
    prMonth:      iDays := 30;
  end;
  d := (iPeriod * iDays) * 1.0; // the last bit ensures the result is a real number
  Result := FormatDateTime('d mmm', d);
end;

//------------------------------------------------------------------------------
// Get the interface to Recorder 2002
//------------------------------------------------------------------------------
function TPhenologyFormX.Get_Recorder2000: IRecorder2000;
begin
  if FRecorder2000 = nil then
  begin
     FRecorder2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  end;
  Result := FRecorder2000;

end;
  //------------------------------------------------------------------------------
// Open the connection to NBNData
//------------------------------------------------------------------------------
function TPhenologyFormX.Get_NBNData: TADOConnection;
begin
  if FNBNData = nil then
  begin
    FNBNData := TADOConnection.Create(nil);
    FNBNData.ConnectionString := Recorder2000.ConnectionString;
    FNBNData.Open;
    Recorder2000.SetApplicationSecurity(FNBNData.ConnectionObject);
  end;
  Result := FNBNData;
end;

destructor TPhenologyFormX.Destroy;
begin
  If FNBNData <> nil then
  begin
    FNBNData.Close;
  end;
  If FOptions <> nil then
        FOptions.Free;
  FRecorder2000 := nil;
  inherited;
end;
//-------------------------------------------------------------------------------
// get taxon_list_item key
// This may have been passed directly (if a species name from taxon dictionary
// was dragged in), of may have to be obtained from a taxon_occurrence
//-------------------------------------------------------------------------------
function TPhenologyFormX.GetTaxonListItemKey(iKey: TKeyList): string;
var lSQL: string;
var oQuery: TADOQuery;
begin
     // if a taxon_list_item was passed, then just return the key
     If CompareText(iKey.Header.TableName, 'TAXON_LIST_ITEM') = 0 then
        Result := iKey.Items[0].KeyField1
     else If CompareText(iKey.Header.TableName, 'TAXON_OCCURRENCE') = 0 then
     // otherwise, we have to query the key from a taxon_occurrence
     begin
          lSQL := 'SELECT TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY ' +
                  'FROM TAXON_OCCURRENCE INNER JOIN TAXON_DETERMINATION ' +
                  'ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY ' +
                  'WHERE (((TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY)=' + Quote + iKey.Items[0].KeyField1 + Quote + ') ' +
                  'AND ((TAXON_DETERMINATION.PREFERRED)=1));';

           oQuery := TADOQuery.Create(nil);
           oQuery.Connection := NBNData;
           oQuery.SQL.ADD(lSQL);
           oQuery.CommandTimeout := 0;
           oQuery.Open;
           if oQuery.RecordCount > 0 then
             Result :=  oQuery.Fields[0].AsString;
           oQuery.Close;
           oQuery.Free;


     end
     else Result := '';





end;

//-------------------------------------------------------------------------------
// get the name of the taxon whose key is in FTLIKey
//-------------------------------------------------------------------------------
function TPhenologyFormX.GetTaxonName: string;
var sSQL, sCommon, sName: string;
var oQuery: TADOQuery;
begin
  Result := '';
  sSQL := 'SELECT ITN2.ACTUAL_NAME, ITN2.COMMON_NAME ' +
          'FROM INDEX_TAXON_NAME ITN' +
          ' INNER JOIN INDEX_TAXON_NAME ITN2 ON ITN2.TAXON_LIST_ITEM_KEY = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY  ' +
          'WHERE (ITN.TAXON_LIST_ITEM_KEY = ' + Quote +  FTLIKey + Quote + ')';
  oQuery := TADOQuery.Create(nil);
  oQuery.Connection := NBNData;
  oQuery.SQL.ADD(sSQL);
  oQuery.CommandTimeout := 0;
  oQuery.Open;
  if oQuery.RecordCount > 0 then
  Begin
    sName :=    oQuery.Fields[0].AsString;
    sCommon :=  oQuery.Fields[1].AsString;
    if sCommon <> sName then
      Result := sCommon + ' (' + sName + ')'
    else
      Result := sName;
  end;
  oQuery.Close;
  oQuery.Free;
end;


//-------------------------------------------------------------------------------
// get the survey key whose TLI key is in FTLIKey
//-------------------------------------------------------------------------------
function TPhenologyFormX.GetSurveyKey: string;
var sSQL  : string;
var oQuery: TADOQuery;
begin
  Result := '';
  sSQL := 'SELECT SV.Survey_Key ' +
          'FROM Survey SV  ' +
          'INNER JOIN Survey_Event SE ON SE.Survey_Key = SV.Survey_Key ' +
          'INNER JOIN Sample S ON S.Survey_Event_key = SE.Survey_event_Key ' +
          'INNER JOIN Taxon_Occurrence TOCC On TOCC.Sample_Key = S.Sample_Key ' +
          'INNER JOIN Taxon_Determination TDET ON TDET.Taxon_Occurrence_Key = TOCC.Taxon_Occurrence_Key ';

  ssQL := ssQL + 'WHERE (TDET.TAXON_LIST_ITEM_KEY = ' + Quote +  FTLIKey + Quote + ')';

  oQuery := TADOQuery.Create(nil);
  oQuery.Connection := NBNData;
  oQuery.SQL.ADD(sSQL);
  oQuery.CommandTimeout := 0;
  oQuery.Open;
  if oQuery.RecordCount > 0 then
    Result := oQuery.Fields[0].AsString;
  oQuery.Close;
  oQuery.Free;
end;
//-------------------------------------------------------------------------------
// get the survey Name whose key is FSVKey
//-------------------------------------------------------------------------------
function TPhenologyFormX.GetSurveyName(aKey :string): string;
var sSQL  : string;
var oQuery: TADOQuery;
begin
  Result := '';
  sSQL := 'SELECT SV.Item_Name ' +
          'FROM Survey SV  ' +
          'WHERE (SV.SURVEY_KEY = ' + Quote +  aKey + Quote + ')';
  oQuery := TADOQuery.Create(nil);
  oQuery.Connection := NBNData;
  oQuery.SQL.ADD(sSQL);
  oQuery.CommandTimeout := 0;
  oQuery.Open;
  if oQuery.RecordCount > 0 then
    Result := oQuery.Fields[0].AsString;
  oQuery.Close;
  oQuery.Free;
end;
 //-------------------------------------------------------------------------------
// get the SQL to query counts from the database
//-------------------------------------------------------------------------------
function TPhenologyFormX.GetSQL(const TLIKey: string; svKey: string ): string;
var sDatePart: string;
var sMainWhere: string;

const
     STANDARD_FROM =   ' FROM Sample S INNER JOIN Taxon_Occurrence TOCC ON TOCC.Sample_Key = S.Sample_Key  ' +
                       ' INNER JOIN Taxon_Determination TDET ON TDET.Taxon_Occurrence_Key = TOCC.Taxon_Occurrence_Key ' +
                       ' INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TDET.Taxon_List_Item_Key  ' +
                       ' INNER JOIN Index_Taxon_Group ITG ON ITG.Contained_List_Item_Key = ITN.Recommended_Taxon_List_Item_Key ' +
                       ' INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key = ITG.Taxon_List_Item_Key ';

const   SURVEY_FROM  = ' FROM Sample S INNER JOIN Taxon_Occurrence TOCC ON TOCC.Sample_Key = S.Sample_Key  ' +
                       ' INNER JOIN Survey_Event SE ON SE.Survey_Event_Key = S.Survey_Event_Key ' +
                       ' INNER JOIN Taxon_Determination TDET ON TDET.Taxon_Occurrence_Key = TOCC.Taxon_Occurrence_Key ' +
                       ' INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TDET.Taxon_List_Item_Key  ' +
                       ' INNER JOIN Index_Taxon_Group ITG ON ITG.Contained_List_Item_Key = ITN.Recommended_Taxon_List_Item_Key ' +
                       ' INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key = ITG.Taxon_List_Item_Key ';

begin
  // the function to calculate period number
  case FPeriod of
    prWeek:      sDatePart := 'DATEPART(ww, S.VAGUE_DATE_START)';
    prFortnight: sDatePart := 'FLOOR(DATEPART(ww, S.VAGUE_DATE_START)/2)';
    prMonth:     sDatePart := 'DATEPART(m, S.VAGUE_DATE_START)';
  end;

  // SELECT clause
  Result := 'SELECT ' + sDatePart + ' AS Wk, COUNT(DISTINCT S.SAMPLE_KEY) AS nRecs ';

  If (options.UseOption = 0) or (aApply.Checked = false) then
  Begin
    Result := Result + STANDARD_FROM;
    sMainWhere :=  'WHERE (ITN2.TAXON_LIST_ITEM_KEY = ' + Quote + TLIKey + Quote + ') ';
  end
  else
  Begin
    Result := Result + SURVEY_FROM;
    sMainWhere :=  'WHERE (SE.Survey_KEY = ' + Quote + sVKey + Quote + ') '
  end;
  //  Main  WHERE and standard conditions
  Result := Result + sMainwhere +  GetDateWhere +
                      'AND (TDET.PREFERRED = 1) ' +
                      'AND (TOCC.ZERO_ABUNDANCE = 0) ' +
                      'AND (TOCC.VERIFIED <> 1) ' +
                      'AND (TOCC.CHECKED = 1) ' +
                      'AND (S.VAGUE_DATE_TYPE = ''D'') ';

  Result := Result + 'GROUP BY ' + sDatePart +  ' ORDER BY Wk';

end;

// ------------------------------------------------------------------------------
// Get the WHERE clause where dates are specified
function   TPhenologyFormX.GetDateWhere : string;
begin
   if (aApply.Checked = true) and Options.UseDate = true then
     Result:= ' AND s.Vague_Date_Start >= dbo.LCToRataDie(' + Quote + '01/01/' + Options.DateFrom + Quote +
              ') AND  s.Vague_Date_Start <= dbo.LCToRataDie(' + Quote + '31/12/' + Options.DateTo + Quote + ')'
     else
     Result:= '';
end;



 //-------------------------------------------------------------------------------
//  Print the chart.  Most of this is to do with scaling it reasonably
//-------------------------------------------------------------------------------
procedure TPhenologyFormX.aPrintExecute(Sender: TObject);
var iScale, xScale, yScale: single;
    iTop, iLeft, iRight, iBottom, iWidth, iHeight, iMargin, h, w: longint;
begin
  If PrintDialog.Execute then
  begin
    // this assumes 300dpi - could do with changing so that it
    // gets the necessary printer info
    iMargin := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
    h := Chart.Height;
    w := Chart.Width;
    iWidth  := GetDeviceCaps(Printer.Handle, HORZRES) - (iMargin * 2);
    iHeight := GetDeviceCaps(Printer.Handle, VERTRES) - (iMargin * 2);
    xScale := (iWidth / w);
    yScale := (iHeight / h);
    // is it best to scale it to maximise x or y direction?
    If xScale < yScale then
      iScale := xScale
    else
      iScale := yScale;
    // in any case don't let it get too big
    //If iScale > 3.5 then iScale := 3.5;
    // calculate dimensions using the chosen scaling
    iTop := iMargin;
    iBottom := iTop + trunc(h * iScale);
    iLeft := iMargin + trunc(iWidth - (w * iScale)) div 2;
    iRight := iLeft + trunc(w * iScale);
    // print it
    Chart.PrintRect(Rect(iLeft, iTop, iRight, iBottom));
  end;
end;
//-------------------------------------------------------------------------------
//  Save as. Mostly, this can be done  using built in procedure of TChart
//  but we have to handle Save to Excel differently
//-------------------------------------------------------------------------------
procedure TPhenologyFormX.aSaveExecute(Sender: TObject);
begin
  With SaveDialog do
  begin
    FileName := Chart.Title.Text[0];
    If Execute then
    case FilterIndex of
      // TChart procedures to save to formats it supports
      1:   Chart.SaveToBitmapFile(FileName);
      2:   Chart.SaveToMetaFile(FileName);
      3:   Chart.SaveToMetaFileEnh(FileName);
      // Our routine to save to Excel
    //  4:   SaveToExcel(FileName);
    end; //case
  end;
end;

//-------------------------------------------------------------------------------
//  This handles the Update event of Print and Save actions
//  Enables or disables them depending on whether we have data in the chart
//-------------------------------------------------------------------------------
procedure TPhenologyFormX.aSaveUpdate(Sender: TObject);
begin
  aPrint.Enabled := (FMin >= 0) and (FMax >= 0);
  aSave.Enabled := (FMin >= 0) and (FMax >= 0);
end;
procedure TPhenologyFormX.SetOptionsClick(Sender: TObject);
begin

  Options.Apply := aApply.Checked;
  // call the options dialog, but only do anything if the OK button was clicked
  If Options.ShowModal = mrOK then
  begin
    aApply.Checked := Options.Apply;
    DoChart;
  end;
end;

function TPhenologyFormX.GetOptions: TformpOption;
begin
  If FOptions = nil then
    FOptions := TformpOption.Create(nil);
  Result := FOptions
end;

function TPhenologyFormX.GetFormTitle(svKey : string): string;
var sDateString : string;
begin
   if (aApply.Checked = true) and (options.usedate = true) then
      sDateString := ' ' + options.datefrom + ' - ' + options.dateto;
   if (aApply.Checked = true) and (options.useoption = 1) then
       Result := GetSurveyName(svKey)
   else
       Result := GetTaxonName;
   Result := Result + sDateString;
end;

function TPhenologyFormX.CheckForChildren: string;
var sSQL  : string;
var oQuery: TADOQuery;
begin
  result:= FTLIKey;
  if (options.useoption = 0) or (aApply.Checked = false) then
  Begin
     sSQL := ' Select ITN.Taxon_List_Item_Key As Children FROM Index_Taxon_Name ITN ' +
            ' INNER JOIN Taxon_List_Item TLI On TLI.Taxon_List_Item_Key = ' +
            ' ITN.Recommended_Taxon_List_Item_Key INNER JOIN ' +
            ' Organism O ON O.Taxon_Version_Key = TlI.Taxon_Version_Key ' +
            ' INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = O.Organism_Rank_Key ';
     sSql := sSql + ' WHERE ITN.TAXON_LIST_ITEM_KEY  = (' + Quote +  FTLIkey + Quote + ')'  +
            ' AND (EXISTS (Select * FROM Organism WHERE Organism.Parent_Key = O.Organism_Key) ' +
            ' OR SEQUENCE > 229)';

     oQuery := TADOQuery.Create(nil);
     oQuery.Connection := NBNData;
     oQuery.SQL.ADD(sSQL);
     oQuery.CommandTimeout := 0;
     oQuery.Open;
     if oQuery.RecordCount = 0 then
     Begin
        Result := '';
       Showmessage ('The starting taxa no children and is not a suitable starting point');
     end;
     oQuery.Close;
     oQuery.Free;
  end;
end;

procedure TPhenologyFormX.mApplyClick(Sender: TObject);
begin
  aApply.Checked := not aApply.Checked;
  DoChart;
end;

//-------------------------------------------------------------------------------
// But Apply can only be active if there are options available to be applied
//-------------------------------------------------------------------------------
procedure TPhenologyFormX.aApplyUpdate(Sender: TObject);
begin
  aApply.Enabled := Options.HasOptions;
end;


Initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TPhenologyFormX,
    Class_PhenologyFormX,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
