unit GotoKey2Impl1;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, GotoKey2_TLB, StdVcl, Recorder2000_TLB, StdCtrls, ExtCtrls,
  ADODB,variants;

type

  TGotoKey2X = class(TActiveForm, IGotoKey2X,IRecorderAddin, INewAction,
                    IDialog, IFormCaption)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    procedure ComboBox1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
   
  private
    { MY Private declarations }
    FSQL:  widestring;
    FDataType: string;
    FConn : TADOConnection;
    FExpand : wordbool;
    FIsExternalRef  : Boolean;
    { Private declarations }
    FEvents: IGotoKey2XEvents;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
   { IRecorderAddin}
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    { INewAction}
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
    { IDialog}
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function DoOk: WordBool; safecall;
    procedure DoCancel; safecall;
    { IFormCaption}
    function Get_FormCaption: WideString; safecall;
    {my procedues  }
    procedure SetUpForm;
    function RowNotExists(NewQuery: string): boolean;
    function GetConnection: Boolean;
    function GetCurrentDBSequence :string;
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
  public
    { Public declarations }
    procedure Initialize; override;
       destructor Destroy; override;

  end;

implementation

uses ComObj, ComServ;

{$R *.DFM}

{ TGotoKey2X }

procedure TGotoKey2X.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_GotoKey2XPage); }
end;

procedure TGotoKey2X.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IGotoKey2XEvents;
  inherited EventSinkChanged(EventSink);
end;

procedure TGotoKey2X.Initialize;
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

function TGotoKey2X.Get_Active: WordBool;
begin
  Result := Active;
end;

function TGotoKey2X.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TGotoKey2X.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TGotoKey2X.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TGotoKey2X.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TGotoKey2X.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TGotoKey2X.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TGotoKey2X.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TGotoKey2X.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TGotoKey2X.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TGotoKey2X.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TGotoKey2X.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TGotoKey2X.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TGotoKey2X.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TGotoKey2X.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TGotoKey2X.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TGotoKey2X.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TGotoKey2X.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TGotoKey2X.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TGotoKey2X.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TGotoKey2X._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TGotoKey2X.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TGotoKey2X.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TGotoKey2X.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TGotoKey2X.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TGotoKey2X.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TGotoKey2X.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TGotoKey2X.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TGotoKey2X.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TGotoKey2X.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TGotoKey2X.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TGotoKey2X.Set_AxBorderStyle(Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TGotoKey2X.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TGotoKey2X.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TGotoKey2X.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TGotoKey2X.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TGotoKey2X.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TGotoKey2X.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TGotoKey2X.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TGotoKey2X.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TGotoKey2X.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TGotoKey2X.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TGotoKey2X.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TGotoKey2X.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TGotoKey2X.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TGotoKey2X.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

procedure TGotoKey2X.DoCancel;
begin
   // don't do anything, just let it close
end;

function TGotoKey2X.DoOk: WordBool;
Var

IMyRecorder6 : Irecorder6;
FullSQL: string;
begin
   if (length(edit1.Text) = 16) OR ((FIsExternalRef = true) AND (length(edit1.Text) > 1))   then
   begin
    if (pos(' ',edit1.Text) = 0) AND (FIsExternalRef) then
      FullSQl := FSQL + 'TPD.ITEM_NAME + '' '' + ' + ''''  + Edit1.Text + ''''
    else
      FullSQl := FSQL + ''''  + Edit1.Text + '''';

    If RowNotExists(FullSQL) = false  then
    begin
      IMyRecorder6 := CreateOlEobject('Recorder2000.AutoapplicationSettings') as IRecorder6;
      IMyrecorder6.displaytab(FDataType,FullSQl,'','',FExpand,false);
      Result := true;
    end else
      Result := false ;


   end
   else begin
     MessageDlg  ('Not a valid key',mtCustom,[mbOK],0);
     Result := false;
   End;


end;

function TGotoKey2X.Get_ActionCaption: WideString;
begin
    Result := '&Go To';
end;

function TGotoKey2X.Get_CanAddToToolbar: WordBool;
begin
    Result := true;
end;

function TGotoKey2X.Get_Description: WideString;
begin
    Result := 'Go to specified entries by picking a table and typing a key.';

end;

function TGotoKey2X.Get_DimmedImageFilename: WideString;
begin
     Result := 'GoToKeyDim.bmp';
end;

function TGotoKey2X.Get_DisabledImageFileName: WideString;
begin
    Result := 'GoToKeyDisabled.bmp';
end;

function TGotoKey2X.Get_FormCaption: WideString;
begin
     Result := 'Go to a key';
     SetUpForm;
end;

function TGotoKey2X.Get_Height: Integer;
begin
     Result := 220;
end;

function TGotoKey2X.Get_Hint: WideString;
begin
    Result := 'Go to particular rows by typing its key.';
end;

function TGotoKey2X.Get_ImageFileName: WideString;
begin
     Result := 'GoToKey.bmp';
end;

function TGotoKey2X.Get_Name: WideString;
begin
     Result := 'GoTo Key v 6.22';
end;

function TGotoKey2X.Get_ParentMenu: WideString;
begin
  Result := 'Tools';
end;

function TGotoKey2X.Get_Width: Integer;
begin
     Result := 240;
end;

procedure TGotoKey2X.Install(const iInstalledFilePath: WideString);
begin
     // don't need to do anything
end;

//------------------------------------------------------------------------
// Check that the row sKey exists in table sTable
// Note that the name of the Key field is put in FKeyField at
// initialisation or when the ComboBox is clicked
//------------------------------------------------------------------------
Function TGotoKey2X.RowNotExists(NewQuery : string   ) : boolean;
var
IRecord : _Recordset;
begin
   If (GetCurrentDBSequence < '00000095') AND  (FIsExternalRef = true)   then
   Begin
       Result := true;
       MessageDlg  ('External Key not supported until after version 6.21',mtCustom,[mbOK],0);
   end
   else
   Begin
      GetConnection;
      Result := false;
      IRecord := Fconn.Execute(NewQuery);
      if IRecord.eof  then
        Begin
           MessageDlg  ('Key not found',mtCustom,[mbOK],0);
           Result := true;
        End
      else begin
        if vartostr(IRecord.Fields[0].Value) = ''  then begin
          MessageDlg  ('Key not found',mtCustom,[mbOK],0);
          Result := true;
        end;
      end
    end;
end;

// Ger current r6 version
//------------------------------------------------------------------------
Function TGotoKey2X.GetCurrentDBSequence: string;
var
IRecord : _Recordset;
begin
   GetConnection;

   IRecord := Fconn.Execute('Select [DATA] FROM setting where [NAMe] = ''DB Seq''');
   Result := IRecord.Fields.Item[0].value;


end;



//------------------------------------------------------------------------
// Initialise the form - set up to search for a SURVEY
//------------------------------------------------------------------------
procedure TGotoKey2X.SetUpForm;
begin
  If FDataType = '' then
  begin
    {Initialise values for global variables - for Survey}
    FDataType  := 'SURVEY';
    FSQL   := 'Select SURVEY_KEY FROM Survey WHERE Survey_key = ';
    FDataType   := 'Survey';
    FExpand := false;
    combobox1.ItemIndex := 0; // set combo for Survey
    edit1.Text := ''; // blank out key
  end;
end;
procedure TGotoKey2X.ComboBox1Click(Sender: TObject);
begin
 FIsExternalRef := false;
 With combobox1 do
    Case ItemIndex of
    0:   begin
             FSQL  := 'SELECT SURVEY_KEY FROM SURVEY WHERE SURVEY_KEY = ';
             FDataType  := 'Survey';

        end;
    1:   begin
             FSQL  := 'SELECT SURVEY_EVENT_KEY FROM SURVEY_EVENT WHERE SURVEY_EVENT_KEY = ';
             FDataType   := 'Event';
         end;
    2:   begin
             FSQL  := 'SELECT SAMPLE_KEY FROM SAMPLE WHERE SAMPLE_KEY = ';
             FDataType   := 'Sample';
         end;
    3:   begin
             FSQL :=  'SELECT TAXON_OCCURRENCE_KEY FROM TAXON_OCCURRENCE  WHERE TAXON_OCCURRENCE_KEY  = ';
             FDataType   := 'Taxon_Occurrence';
          end;
    4:   begin
           FSQL :=  'SELECT BIOTOPE_OCCURRENCE_KEY FROM BIOTOPE_OCCURRENCE  WHERE BIOTOPE_OCCURRENCE_KEY  = ';
           FDataType   := 'Biotope_Occurrence';
        end;
    5:   begin
           FSQL :=  'SELECT LOCATION_KEY FROM LOCATION  WHERE LOCATION_KEY  = ';
           FDataType   := 'Location';
        end;
    6:   begin
           FSQL :=  'SELECT LOCATION_FEATURE_KEY FROM LOCATION_FEATURE WHERE LOCATION_FEATURE_KEY   = ';
           FDataType   := 'Feature';
         end;
    7:   begin
           FSQL :=  'SELECT NAME_KEY FROM NAME WHERE NAME_KEY  = ';
           FDataType   := 'Name';
        end;
    8:  begin
          FSQL :=  'SELECT SOURCE_KEY FROM REFERENCE WHERE SOURCE_KEY   = ';
          FDataType   := 'Document';
        end;
    9:   begin
          FSQL :=  'SELECT ITN.TAXON_LIST_ITEM_KEY AS TAXON_LIST_ITEM_KEY ' +
                   ' FROM INDEX_TAXON_NAME ITN  WHERE ' +
                   ' ITN.PREFERRED_TAXA = 1 AND ITN.TAXON_VERSION_KEY = ';
          FDataType   := 'Taxon';
        end;
    10:   begin
          FSQL :=  'SELECT TAXON_LIST_ITEM_KEY FROM TAXON_LIST_ITEM WHERE TAXON_LIST_ITEM_KEY   = ';
          FDataType   := 'Taxon';
        end;
     11:   begin
           FSQL :=  'SELECT BIOTOPE_LIST_ITEM_KEY FROM BIOTOPE_LIST_ITEM WHERE BIOTOPE_LIST_ITEM_KEY   = ';
          FDataType   := 'Biotope';
        end;
     12: begin
          FSQL :=  'SELECT ADMIN_AREA_KEY FROM ADMIN_AREA WHERE ADMIN_AREA_KEY  = ';
          FDataType   := 'Admin';;
         end;
     13: begin
           FSQL := ' Select min(TPD.TAXON_OCCURRENCE_KEY) AS TAXON_OCCURRENCE_KEY ' +
           ' FROM Taxon_Private_Data TPD ' +
           ' WHERE TPD.TAXON_PRIVATE_TYPE_KEY = ''R6TEAM1800000001''  AND ' +
           ' TPD.ITEM_NAME + '' '' + TPD.Detail  = ';
           FDataType   := 'Taxon_Occurrence';;
           FIsExternalRef := true;
         end;


    end; {case}
end;

procedure TGotoKey2X.CheckBox1Click(Sender: TObject);
begin
  FExpand := Checkbox1.checked;
end;

//------------------------------------------------------------------------
// Get a connection to the database
//------------------------------------------------------------------------
function TGoToKey2X.GetConnection: Boolean;
var
IRecorder : IRECORDER2000;
begin
  If FConn = nil then
  begin
    FConn := TADOConnection.Create(nil);
    IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
    FConn.ConnectionString := IRecorder.ConnectionString;
    FConn.Open;
    IRecorder.SetApplicationSecurity(FConn.ConnectionObject);

  end;
  Result := true;
end;

 //===========================================================================
// Properties and initialisation
//===========================================================================
//------------------------------------------------------------------------
// Clean up - close database connection
//------------------------------------------------------------------------
destructor TGoToKey2X.Destroy;
begin
  If FConn <> nil then
  begin
    FConn.Close;
    FConn.Free;
  end;
  inherited;
end;


initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TGotoKey2X,
    Class_GotoKey2X,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
