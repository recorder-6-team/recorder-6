unit Project3RuckForm1Impl1;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,  Dialogs,
  ActiveX, AxCtrls, ProjectRuck3_TLB, StdVcl,Recorder2000_TLB,Registry,ADODB,StrUtils,Variants,
  StdCtrls, ExtCtrls;

type
  TProject3RuckForm1 = class(TActiveForm, IProject3RuckForm1,IRecorderAddin,INewAction,IDialog)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Label4: TLabel;
    RadioGroup1: TRadioGroup;
    Label5: TLabel;
    Label6: TLabel;
    Edit2: TEdit;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Shape1: TShape;
    CheckBox7: TCheckBox;
    procedure ActiveFormCreate(Sender: TObject);
  private
    { Private declarations }
   {My declarations }
    FStatusFlag : Integer;
    FRucksackPath : string;
    FTextFile : string;
    FRuckFile : string;
    FLogFile : string;
    FColumnContents: array[0..100] of widestring;
    FTaxonNameColumn: Integer;
    FSearchNameColumn: Integer;
    FFullLine : widestring ;
    FDelimiter : string;
    {Built in}
    FEvents: IProject3RuckForm1Events;
    FHasHeading : boolean;
    FHasSearch : boolean;
    FIsFromRucksack : boolean;
    FIgnoreRecord :boolean;
    FStartFlag : boolean;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);

    {My declarations }

    function Get_RucksackPath () : string;
    function Check_FileName () : boolean ;
    function Get_TidyTaxonName(InputTaxonName: string): string;
    function Get_LogFileName() : string;
    function Get_FilesToUse() : boolean;
    function Get_ColumnValues(InputValue :widestring): integer ;
    function Get_NoOFDoubleQuotes(FieldValue:widestring): integer ;
    function Get_FirstlineInfo(TextLine :string) : boolean;
    function Get_TaxonNameFromLine(FileLine : integer) :string;
    function Get_OrderBy() :string;
    function Display_Messages() :boolean;
    function Get_RuckSackOutPutLine( TaxonKey :string; SearchKey :string) :string;
    function Get_SearchNameFromLine(FileLine : integer) :string;
    function Get_SearchInfo(TextLine :string) : boolean;
    function Get_AdditionalTags(IResult : _Recordset; IConn: TADOConnection) : widestring;
    function Get_CheckedAsString(CheckedText :string;  CheckedValue : boolean ) :string;
    function Get_SortOption(RadioGroupIndex: integer ) :string ;
    function Get_ReplaceSingleQuotes(InputText : string): string;
    function Get_KeyToActionFlag(ActionKey : integer ; ExceptionKey :integer): string;
    procedure SplitTaxonName(var Genus,Species,  GenusLength, SpeciesLength,TrueSpecies, TrueSpeciesLength :string ; TaxonName : string);
    function Get_Attribute(TempTaxonName :string ;Attempt :Integer ) : string;

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
    {My protected }
    {IRecorderAddin}
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    {INewAction}
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
    {IDialog}
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function DoOk: WordBool; safecall;
    procedure DoCancel; safecall;
    public
    { Public declarations }
    procedure Initialize; override;

 end;

implementation

uses ComObj, ComServ;

{$R *.DFM}

{ TProject3RuckForm1 }
   const
     CRLF = chr(13) + chr(10);
     StartUpMessage = 'Complete the information above and then click on OK. The input file (.txt,.csv,.ruk or ,crd) and output file (.ruk) can then be chosen. ' +
    ' If you have a file with a column named ''TaxonName'' containing scientific names and you do not wish to have ''Search Codes'' in the Rucksack then ' +
    ' the defaults above will generate the Rucksack. Refer to the documentation for details of other file formats. ' +
    ' Note that the Rucksack file generated will overwrite an existing file of the same name.' ;
    HeadingMessage = 'Rucksack will be created based on the preferred taxon lists. ' +
    ' Additional methods of allocation can be selected from the options below. ' +
    ' If these are used check the log file to ensure that the allocation is appropriate. ';
     BaseQuery =
            {  'SELECT ITN.Taxon_List_Item_Key ' +
              ' From Index_Taxon_Name ITN ' +
              ' Inner Join Taxon_List_Version TLV On ITN.Taxon_List_Version_Key = TLV.Taxon_List_Version_Key '+
              ' Inner Join Taxon_List TL On TLV.Taxon_List_Key = TL.Taxon_List_Key '+
              ' Where ITN.Taxon_List_Item_Key = ITN.Recommended_Taxon_List_Item_Key '+
              ' And ITN.Actual_Name = ITN.Preferred_Name '+
              ' And ITN.Allow_Data_Entry = 1 '+
              ' And TL.Preferred = 1 ';

              'SELECT TLI.Preferred_Name  FROM INDEX_TAXON_NAME ITN ' +
              ' INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key ' +
              'INNER JOIN Taxon_Version TV on TV.Taxon_version_key = TLI.Taxon_Version_Key '+
              ' WHERE ITN.PREFERRED_LIST = 1 AND ITN.ALLOW_DATA_ENTRY = 1  ';   }

              ' SELECT TLI.Preferred_name,TL.item_name FROM Taxon_List_Item TLI ' +
              ' INNER JOIN Taxon_Version  TV  ' +
              ' ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key ' +
              ' INNER JOIN Index_Taxon_Name ITN ' +
              ' ON ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key ' +
              ' INNER JOIN TAXON_List_Version TLV ' +
              ' ON TLV.Taxon_List_Version_Key = TLi.Taxon_List_Version_Key ' +
              ' INNER JOIN Taxon_List TL ' +
              ' ON TL.Taxon_List_Key = TLV.Taxon_List_Key ' +
              ' INNER JOIN Taxon_List_Type TLT ON TLT.Taxon_List_Type_Key = TL.Taxon_List_type_key ' +
              ' WHERE TLT.Allow_Data_Entry = 1 AND TLI.Taxon_List_Item_Key = TLI.Preferred_Name ' ;
     BaseQuerySynonym =
              'SELECT ITN.Recommended_Taxon_List_Item_Key ' +
              ' FROm Index_Taxon_Name ITN ' +
              ' Inner Join Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Recommended_Taxon_List_Item_Key ' +
              ' Inner Join Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key ' +
              ' Inner Join Taxon_List TL On TL.Taxon_List_Key = TLV.Taxon_List_Key '+
              ' INNER JOIN Taxon_Version TV on TV.Taxon_version_key = TLI.Taxon_Version_Key '+
              ' WHERE TV.Attribute IS NULL AND ITN.Allow_Data_Entry = 1 ';

     CreatetableTemptaxon =
              'Create Table #Temptaxa (TaxonName varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS,' +
              'TLIKey varchar(16) COLLATE SQL_Latin1_General_CP1_CI_AS,' +
              'ActualName varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS,' +
              'DictList varchar (200) COLLATE SQL_Latin1_General_CP1_CI_AS, '  +
              'Taxon_Group varchar (60)  COLLATE SQL_Latin1_General_CP1_CI_AS, '  +
              'Action_Flag integer, ' +
              'Sort_Order varchar (60)  COLLATE SQL_Latin1_General_CP1_CI_AS, ' +
              'Original_order integer, Duplicate_Flag integer, ' +
              'SearchName varchar(10) COLLATE SQL_Latin1_General_CP1_CI_AS, ' +
              'OriginalKey  varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS, ' +
              'Attribute varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS )';
      CreatetableTempRucksack =
              'Create Table #temprucksack (EntryType varchar(20) COLLATE SQL_Latin1_General_CP1_CI_AS,' +
              'TLIKey varchar(16) COLLATE SQL_Latin1_General_CP1_CI_AS, ' +
              'SearchCode varchar(10) COLLATE SQL_Latin1_General_CP1_CI_AS )';
      UpdateTempTaxon =
              'Update #Temptaxa Set Duplicate_Flag = 1, ActualName = ITN.Actual_name, DictList = ''"'' +  TL.Item_name + ''"'' , Taxon_Group = TG.Taxon_Group_Name, Sort_Order = ITN.Sort_Order, ' +
              ' Attribute = ''"'' + TV.Attribute + ''"'' ' +
              ' FROM #Temptaxa ' +
              ' INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = #Temptaxa.TliKey' +
              ' INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key ' +
              ' INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key ' +
              ' INNER JOIN Taxon_Group TG ON TG.Taxon_Group_Key = TV.Output_Group_Key ' +
              ' INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key ' +
              ' INNER JOIN Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key ';


procedure TProject3RuckForm1.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_Project3RuckForm1Page); }
end;

procedure TProject3RuckForm1.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IProject3RuckForm1Events;
  inherited EventSinkChanged(EventSink);
end;

procedure TProject3RuckForm1.Initialize;
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

function TProject3RuckForm1.Get_Active: WordBool;
begin
  Result := Active;
end;

function TProject3RuckForm1.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TProject3RuckForm1.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TProject3RuckForm1.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TProject3RuckForm1.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TProject3RuckForm1.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TProject3RuckForm1.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TProject3RuckForm1.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TProject3RuckForm1.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TProject3RuckForm1.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TProject3RuckForm1.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TProject3RuckForm1.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TProject3RuckForm1.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TProject3RuckForm1.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TProject3RuckForm1.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TProject3RuckForm1.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TProject3RuckForm1.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TProject3RuckForm1.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TProject3RuckForm1.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TProject3RuckForm1.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TProject3RuckForm1._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TProject3RuckForm1.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TProject3RuckForm1.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TProject3RuckForm1.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TProject3RuckForm1.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TProject3RuckForm1.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TProject3RuckForm1.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TProject3RuckForm1.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TProject3RuckForm1.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TProject3RuckForm1.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TProject3RuckForm1.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TProject3RuckForm1.Set_AxBorderStyle(
  Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TProject3RuckForm1.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TProject3RuckForm1.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TProject3RuckForm1.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TProject3RuckForm1.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TProject3RuckForm1.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TProject3RuckForm1.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TProject3RuckForm1.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TProject3RuckForm1.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TProject3RuckForm1.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TProject3RuckForm1.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TProject3RuckForm1.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TProject3RuckForm1.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TProject3RuckForm1.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TProject3RuckForm1.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;


function TProject3RuckForm1.Get_ActionCaption: WideString;
begin
  result := 'Generate Rucksack 3';
end;

function TProject3RuckForm1.Get_CanAddToToolbar: WordBool;
begin
  result := false;
end;

function TProject3RuckForm1.Get_Description: WideString;
begin
   result := 'Generate Rucksack from text file and manage existing Rucksacks ';
end;

function TProject3RuckForm1.Get_DimmedImageFilename: WideString;
begin
   result:= 'Default.bmp' ;
end;

function TProject3RuckForm1.Get_DisabledImageFileName: WideString;
begin
   result:= 'Default.bmp' ;
end;

procedure  TProject3RuckForm1.SplitTaxonName(var Genus,Species,  GenusLength, SpeciesLength,TrueSpecies, TrueSpeciesLength :string ; TaxonName : string);
// Note species here is everything after the first space
// It will include attributes sub species etc
// True species is just from first space to second space
var
  FirstSpace : integer;
  SecondSpace : integer;
begin
  begin
    FirstSpace := Ansipos(' ',TaxonName);
    if Firstspace = 0 then
      begin
        Genus := trim(TaxonName);
        Species := '';
       end
      else
        begin
         Genus := Trim(AnsiLeftStr(TaxonName,FirstSpace));
         Species := Trim(AnsirightStr(TaxonName,(Length(TaxonName) - FirstSpace)));
        end;
   end;
   GenusLength := inttostr(Length(Genus));
   SpeciesLength := inttostr(Length(Species));
   SecondSpace := Ansipos(' ',Species);
   TrueSpecies :=  Genus + ' ' + Trim(AnsiLeftStr(Species,SecondSpace));
   TrueSpeciesLength := inttostr(Length(TrueSpecies));


end;

function TProject3RuckForm1.Get_Hint: WideString;
begin
    result := 'Generating Rucksack - Please wait';
end;

function TProject3RuckForm1.Get_ImageFileName: WideString;
begin
     result:= 'Default.bmp' ;
end;

function TProject3RuckForm1.Get_Name: WideString;
begin
      result:= 'Generate Rucksack v6.22' ;
end;

function TProject3RuckForm1.Get_ParentMenu: WideString;
begin
      result := 'Tools';
end;

function TProject3RuckForm1.Get_RucksackPath: string;
 var
R6Registry : TRegistry;
begin
  R6Registry := TRegistry.Create;
  try
    //r6registry.rootkey := 1
    if R6Registry.OpenKey('Software\Dorset Software\Recorder 6\Settings', FALSE) then
      begin
        Result :=  R6Registry.Readstring('Rucksack Path') ;
      end;
  finally
    R6Registry.free;
  end;
end;
function TProject3RuckForm1.Get_ReplaceSingleQuotes(InputText : string) :string;
var
SQ : string;
begin
  SQ :=   ''''''  ;
  Result := stringreplace(InputText,'''',SQ,[rfReplaceAll]);
end;

function TProject3RuckForm1.Get_TidyTaxonName(
  InputTaxonName: string): string;
{Function to get rid of double spacesa and sp Could be used for other tidying up if required }
Var
RevisedTaxonName : string;
StringPosition :integer;
begin
  if FIsFromRucksack = true then
     begin
           Result := ansileftstr(InputTaxonName,27);
           If FStartFlag = false  then result:= '';
           if UpperCase(AnsiLeftStr(InputTaxonName,8)) = '</TAXON>' then FIgnoreRecord := true;
           if UpperCase(AnsiLeftStr(InputTaxonName,7)) = '<TAXON>' then FStartFlag := true;
           if AnsiLeftStr(InputTaxonName,1) = '<' then Result := '';

         end
     else
          begin
            RevisedTaxonName := Trim(AnsiLeftStr(stringreplace(InputTaxonName,'  ',' ',[]),100));

            Result:= RevisedTaxonname;
            StringPosition  := Ansipos ( ' sp.', RevisedTaxonName);
            If StringPosition > 0 then
              begin
                Result := Trim(AnsileftStr(RevisedTaxonName,StringPosition-1));
              end;
              If (AnsiLeftStr(RevisedTaxonName,1) = '*')   then result:= '';
          end;
end;

procedure TProject3RuckForm1.Install(const iInstalledFilePath: WideString);
begin

end;

procedure TProject3RuckForm1.DoCancel;
begin
     screen.Cursor := crDefault;
end;

function TProject3RuckForm1.DoOk: WordBool;
Var
  IConn: TADOConnection;
  IRecorder: IRecorder2000;
  IResult: _Recordset;
  QryNameFromTLI : Widestring;
  FileVar: textfile;
  TaxonName: string;
  SearchName : string;
  InsertText: widestring;
  R6Query: widestring;
  RuckVar: textfile;
  LogVar: textfile;
  Genus : string;
  Species : string;
  GenusLength :string;
  SpeciesLength : string;
  R6TLiKey : string;
  ActionFlag : integer;
  OriginalOrder : integer;
  FileLine : integer;
  ExtractQuery :widestring;
  RuckSackQuery :string;
  RuckSackCurrenttag :string;
  OriginalTaxonName : string;
  TrueSpecies : string;
  TrueSpeciesLength : string;

begin
  OriginalOrder := 0;
  FStartFlag := true;
  If FStatusFlag = 0 then
  begin
   If (Get_FilesToUse  = true) then
    begin
     FiGnoreRecord := false;
     FStatusFlag := 1;
     Display_Messages;
     Application.ProcessMessages;
     IConn:=TADOConnection.Create(nil);
     IRecorder:=CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
     IConn.ConnectionString:=IRecorder.ConnectionString;
     IConn.open;
     IRecorder.SetApplicationSecurity(IConn.ConnectionObject);
     IResult := IConn.Execute (CreateTableTempTaxon);
     IResult := IConn.Execute (CreateTableTempRucksack);
     AssignFile(FileVar, FTextFile);
     Reset (FileVar);
     // If from Rucksack then puts data into a #temprucksacktable
     if FIsFromRucksack = true then
     begin
       While not eof(FileVar) do
         begin
           ReadLN(FileVar, FFullLine );
           if (AnsiLeftStr(FFullLine,1) = '<') and (AnsileftStr(FFullLine,2) <> '</')  then
              RuckSackCurrenttag := FFullLine;
           if ansileftstr(FFullLine,1) <> '<'  then
             begin
                RuckSackQuery := 'Insert Into #temprucksack (EntryType,TLIKey,SearchCode) Values (' +
                '''' + RuckSackCurrentTag + ''',''' + AnsiLeftStr(FFullLine,16) + ''',' +
                '''' + Copy(FFullLine,18,10) + ''')';
                IResult := IConn.Execute ( RuckSackQuery);
             end;
         end;
     end;
     // This is the main routine whih reads the input ile and poulates the main temp table
     Reset (FileVar);
     FileLine := 0;
     While not eof(FileVar) do
        begin
          Fileline := Fileline + 1;
          R6TliKey := '';
          ActionFlag := 99;
          ReadLN(FileVar, FFullLine );
          TaxonName := Get_TaxonNameFromLine(FileLine) ;
          OriginalTaxonName := TaxonName ;
          TaxonName := Get_TidyTaxonName(TaxonName);
          SearchName := Get_SearchNameFromLine(FileLine);

          If (TaxonName <> '') and (FIgnoreRecord = false) then
            begin
              if FIsFromRucksack = true  then
                begin
                  QryNameFromTLI :=  'Select Preferred_Name FROM ' +
                  ' Index_Taxon_Name WHERE Taxon_List_Item_Key = ''' + ansileftstr(TaxonName,16) + '''';
                  IResult :=  IConn.Execute (QryNameFromTLI);
                  If IResult.EOF = false then
                    begin
                      OriginalTaxonName:=  ansileftstr(TaxonName,16);
                      TaxonName := Iresult.Fields[0].Value;

                    end;
                end;
                SplitTaxonName(Genus,Species, GenusLength, SpeciesLength,TrueSpecies,TrueSpeciesLength,TaxonName);
                 R6Query := BaseQuery +
                ' AND TL.Preferred = 1 AND  TV.Attribute IS NULL And ITN.Actual_Name  = ''' + Get_ReplaceSingleQuotes(TaxonName) + '''';
                Iresult :=  IConn.Execute (R6Query);
                If IResult.eof = false then
                  begin
                  R6TliKey := IResult.Fields[0].value;
                  ActionFlag := 1;
                End;

                If ActionFlag = 99 then
                  begin
                    If Species <> '' then
                      begin
                        R6Query := BaseQuery +
                        ' AND TL.Preferred = 1 AND TV.Attribute IS NULL And left(ITN.Actual_Name, ' + GenusLength + ') = ''' +  Genus  + '''' +
                        ' And Right(ITN.Actual_Name, ' + SpeciesLength + ') = ''' + Get_ReplaceSingleQuotes(Species) + ''''  +
                        ' And ITN.Actual_Name <> ''' + Get_ReplaceSingleQuotes(Species) + '''';
                        IResult :=  IConn.Execute (R6Query);
                        If IResult.eof = false then
                          begin
                            R6TliKey := IResult.Fields[0].value;
                            ActionFlag := 2;
                          End;
                    end
                    else
                      begin
                        R6Query := BaseQUery  +
                        ' AND TL.Preferred = 1 AND TV.Attribute IS NULL And ITN.Actual_name = ''' + Get_ReplaceSingleQuotes(Genus) + '''';
                        IResult :=  IConn.Execute (R6Query);
                        If IResult.eof = false then
                          begin
                            R6TliKey := IResult.Fields[0].value;
                            ActionFlag := 3;
                          End;


                     end;

                 end;
                 // This is where we have a go at the attributes
                  If ActionFlag = 99  then
                    begin
                       R6Query := BaseQuery +
                       ' AND (TV.Attribute Like(''%'  + Get_Attribute(TaxonName,1)  + '%'')' +
                       ' OR  TV.Attribute Like(''%'  + Get_Attribute(TaxonName,2)  + '%'')' +
                       ' OR  TV.Attribute Like(''%'  + Get_Attribute(TaxonName,3)  + '%'')' +
                        ' OR  TV.Attribute Like(''%'  + Get_Attribute(TaxonName,4)  + '%'')' +
                       ' OR  TV.Attribute Like(''%'  + Get_Attribute(TaxonName,5)  + '%''))' +
                       ' AND TL.Preferred = 1 And ITN.Actual_Name  = ''' + Get_ReplaceSingleQuotes(TrueSpecies) + '''';

                       Iresult :=  IConn.Execute (R6Query);
                       If IResult.eof = false then
                          begin
                          R6TliKey := IResult.Fields[0].value;
                          ActionFlag := 12;
                       End;
                   end;
                  // From here are the optional searches
                 If (ActionFlag = 99) and (CheckBox1.Checked  = true) then
                  begin

                    R6Query := BaseQuerySynonym +  ' AND TL.Preferred = 1 And  ITN.Actual_Name  = ''' + Get_ReplaceSingleQuotes(TaxonName) + '''';
                    IResult :=  IConn.Execute (R6Query);
                    If IResult.eof = false then
                      begin
                        R6TliKey := IResult.Fields[0].value;
                        ActionFlag := 4;
                      end;
                  end;
                 If (ActionFlag = 99) and (CheckBox2.Checked  = true) then
                 begin

                    R6Query := BaseQuery +  ' AND TV.Attribute IS NULL AND TL.Taxon_List_Key = ''NBNSYS0000000116'' And  ITN.Actual_Name  = ''' + Get_ReplaceSingleQuotes(TaxonName) + '''';
                    IResult :=  IConn.Execute (R6Query);
                    If IResult.eof = false then
                      Begin
                        R6TliKey := IResult.Fields[0].value;
                        ActionFlag := 5;
                      end;
                 end;
                 If (ActionFlag = 99) And (CheckBox3.Checked  = true) then
                 begin

                    R6Query := BaseQuery +  ' AND TV.Attribute IS NULL AND TL.Taxon_List_Key = ''NHMSYS0000355953'' And  ITN.Actual_Name  = ''' + Get_ReplaceSingleQuotes(TaxonName) + '''';
                    IResult :=  IConn.Execute (R6Query);
                    If IResult.eof = false then
                      Begin
                        R6TliKey := IResult.Fields[0].value;
                        ActionFlag := 6;
                      end;
                 end;
                 If (ActionFlag = 99) And (CheckBox4.Checked  = true) then
                 begin

                    R6Query := BaseQuery +  ' AND TV.Attribute IS NULL AND TL.Taxon_List_Key = ''NBNSYS0000000074'' And  ITN.Actual_Name  = ''' + Get_ReplaceSingleQuotes(TaxonName) + '''';
                    IResult :=  IConn.Execute (R6Query);
                    If IResult.eof = false then
                      Begin
                        R6TliKey := IResult.Fields[0].value;
                        ActionFlag := 7;
                      end;
                 end;

                 If (ActionFlag = 99) And (AnsiLeftStr(TaxonName,1)= '#' ) then
                 begin

                    R6Query := 'Select ITN.Taxon_List_Item_Key From Index_Taxon_Name ITN Where ITN.Taxon_List_Item_Key = Right(''' + TaxonName + ''',16)';
                    IResult :=  IConn.Execute (R6Query);
                    If IResult.eof = false then
                      Begin
                        R6TliKey := IResult.Fields[0].value;
                        ActionFlag := 9;
                      end;
                 end;

                 If (ActionFlag = 99) And (AnsiLeftStr(TaxonName,1)= '$' ) then
                 begin
                     R6Query := 'Select TLI.Taxon_List_Item_Key FROM ' +
                     ' Taxon_List_Item TLI ' +
                     ' INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key ' +
                     ' Where ITN.Allow_Data_Entry = 1 AND ITN.Preferred_List =1 AND TLI.Taxon_Version_Key = Right(''' + TaxonName + ''',16)';
                    IResult :=  IConn.Execute (R6Query);
                    If IResult.eof = false then
                      Begin
                        R6TliKey := IResult.Fields[0].value;
                        ActionFlag := 10;
                      end;
                 end;
                 If (ActionFlag = 99) And (AnsiLeftStr(TaxonName,1)= '$' ) then
                 begin
                     R6Query := 'Select TLI.Taxon_List_Item_Key FROM ' +
                     ' Taxon_List_Item TLI ' +
                     ' INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key ' +
                     ' Where ITN.Allow_Data_Entry = 1 AND TLI.Taxon_Version_Key = Right(''' + TaxonName + ''',16)';
                    IResult :=  IConn.Execute (R6Query);
                    If IResult.eof = false then
                      Begin
                        R6TliKey := IResult.Fields[0].value;
                        ActionFlag := 11;
                      end;
                 end;
             OriginalOrder := OriginalOrder + 1;
              InsertText := 'Insert into #Temptaxa (TaxonName,TLiKey, Action_Flag,Original_Order,SearchName,OriginalKey) ' +
             ' values ('''  + Get_ReplaceSingleQuotes(TaxonName) + ''','''  +
             R6TliKey + ''','  +  IntToStr(ActionFlag) + ',' +  IntToStr(OriginalOrder) +
              ',''' + SearchName  + ''',' +
             '''' + Get_ReplaceSingleQuotes(OriginalTaxonName)  + ''')' ;
             IResult := IConn.Execute (InsertText);

             end;


       end;

      CloseFile (FileVar);
      // The file is closed. The dats is then processed
      if (checkbox7.checked = true) AND (FIsFromRucksack = true) then
        begin
          IResult := Iconn.Execute('Update #TempTaxa set TLIKey  = #temprucksack.TLiKey, Action_Flag = 8  ' +
          ' FROM #TempTaxa INNER JOIN #temprucksack on #temprucksack.TLiKey =  #TempTaxa.OriginalKey ' +
          ' INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY = #TempTaxa.OriginalKey ' +
          ' WHERE #temprucksack.EntryType = ''<TAXON>''') ;

        end;
       IResult := Iconn.Execute(UpdateTempTaxon);
       IResult := Iconn.Execute('Update #TempTaxa set SearchName = #temprucksack.SearchCode ' +
       ' From #TempTaxa INNER JOIN #temprucksack on #temprucksack.TLiKey =  #TempTaxa.OriginalKey ' +
       ' WHERE #temprucksack.EntryType = ''<TAXONSEARCHCODE>''') ;

       AssignFile(RuckVar, FRuckFile);
       AssignFile(LogVar, FLogFile);
       Rewrite(RuckVar);
       Rewrite(LogVar);

       IResult := IConn.Execute('Update #TempTaxa set Duplicate_Flag = 0 where TLIKey = ''''');

       ExtractQuery := 'Update #Temptaxa set Duplicate_Flag = 2 ' +
       ' FROM #TEMPTAXA WHERE Original_order = (Select min(Original_order) FROM #TEMPTAXA T WHERE T.TLIKey = #TEMPTAXA.TLIKey AND T.Duplicate_Flag = 1 GROUP BY TLIKey)';
       IResult := IConn.Execute(ExtractQuery);

       ExtractQuery := 'Update #Temptaxa set Duplicate_Flag = Duplicate_Flag + 1  ' +
       ' FROM #TEMPTAXA WHERE Original_order = (Select min(Original_order) FROM #TEMPTAXA T WHERE T.Duplicate_Flag = 2 AND T.ACTUALNAME = #TEMPTAXA.ACTUALNAME GROUP BY Actualname )';

       IResult := IConn.Execute(ExtractQuery);

        ExtractQuery := 'Update #Temptaxa set Duplicate_Flag = 4  ' +
       ' FROM #TEMPTAXA WHERE  SearchName = '''' AND Duplicate_Flag = 3 ';
        IResult := IConn.Execute(ExtractQuery);

        ExtractQuery := 'Update #Temptaxa set Duplicate_Flag = Duplicate_Flag + 1  ' +
       ' FROM #TEMPTAXA WHERE Original_order = (Select min(Original_order) FROM #TEMPTAXA T WHERE T.Duplicate_Flag = 3 AND T.SearchName = #TEMPTAXA.SearchName AnD T.SearchName <> ''''  GROUP BY SearchName )';


        IResult := IConn.Execute(ExtractQuery);


        IResult := IConn.Execute ('Select TLIKey FROM #TempTaxa WHERE Duplicate_Flag = 4 and tlikey <> ''''' + Get_OrderBy );

        {Start writing Rucksack here }
        write (RuckVar,'<TAXON>',CRLF);
        While Not (IResult.eof) do
          begin
           write (RuckVar,IResult.fields[0].value,CRLF);
           Iresult.MoveNext;
        end;
        if Checkbox5.checked = true and  FIsFromRuckSack = true then
          begin

           write (RuckVar,'</TAXON>',CRLF);
           write (RuckVar,Get_AdditionalTags(IResult, IConn));
           write (RuckVar,'<TAXONSEARCHCODE>',CRLF);
         end
         else
          begin

            write (RuckVar,'</TAXON>',CRLF);
            write (RuckVar,'<BIOTOPE>',CRLF);
            write (RuckVar,'</BIOTOPE>',CRLF);
            write (RuckVar,'<LOCATION>',CRLF);
            write (RuckVar,'</LOCATION>',CRLF);
            write (RuckVar,'<PEOPLE>',CRLF);
            write (RuckVar,'</PEOPLE>',CRLF);
            write (RuckVar,'<REFERENCE>',CRLF);
            write (RuckVar,'</REFERENCE>',CRLF);
            write (RuckVar,'<TAXONSEARCHCODE>',CRLF);
          end;
        if checkbox6.Checked = true then
          begin
            IResult := IConn.Execute ('Select TLIKey,  SearchName FROM #TempTaxa WHERE Duplicate_Flag = 4 and SearchName <> '''' and tlikey <> ''''');
              While Not (IResult.eof) do
                begin
                 write (RuckVar,Get_RucksackOutputLine(IResult.fields[0].value,IResult.fields[1].value),CRLF);
                 Iresult.MoveNext;
              end;
          end;
        write (RuckVar,'</TAXONSEARCHCODE>',CRLF);
        Closefile (RuckVar);

       {Doing log file here }
       write (LogVar,'Original_Input',',','TaxonName',',','Taxon_List_item_Key',',','R6Taxon_Name',',','R6Attribute',',','"R6_List"',',','Taxon_Group',',','Search_Code',',','Allocation_Method', CRLF);
       write (LogVar,'Rucksack creation Logfile run at ' + datetimetostr(Now),',*',CRLF ) ;
       write (LogVar,'Input file = ' + FTextFile,',*',CRLF ) ;
       write (LogVar,'Output file = ' + FRuckFile,',*', CRLF) ;
       write (LogVar,'Taxon Column = ' + Edit1.Text ,',*', CRLF) ;
       write (LogVar,'Search Code Column  = ' + Edit2.Text,',*', CRLF ) ;
       write (LogVar, Get_CheckedAsString(CheckBox1.Caption,CheckBox1.checked)   ) ;
       write (LogVar, Get_CheckedAsString(CheckBox2.Caption,CheckBox2.checked)   ) ;
       write (LogVar, Get_CheckedAsString(CheckBox3.Caption,CheckBox3.checked)   ) ;
       write (LogVar, Get_CheckedAsString(CheckBox4.Caption,CheckBox4.checked)   ) ;
       write (LogVar, Get_CheckedAsString(CheckBox7.Caption,CheckBox7.checked)   ) ;
       write (LogVar, Get_CheckedAsString(CheckBox6.Caption,CheckBox6.checked)   ) ;
       write (LogVar, Get_CheckedAsString(CheckBox5.Caption,CheckBox5.checked)   ) ;
       write (LogVar, Get_SortOption(RadioGroup1.ItemIndex ));


       IResult := IConn.Execute ('Select * from #Temptaxa Order By Original_Order');
         while IResult.EOF = false do
           begin
             write (LogVar,IResult.fields[10].value,',', IResult.fields[0].value,',', IResult.fields[1].value, ',', vartostr(IResult.fields[2].value), ',', vartostr(IResult.fields[11].value)    , ',', vartostr(IResult.fields[3].value), ',' , vartostr(IResult.fields[4].value), ',', vartostr(IResult.fields[9].value),',', Get_KeyToActionFlag(IResult.fields[5].value, IResult.fields[8].value), CRLF);
             Iresult.MoveNext;
          end;

       IResult := IConn.Execute ('Drop Table #Temptaxa ');
       IResult := IConn.Execute ('Drop Table #temprucksack ');



       Closefile (LogVar);

       // IResult.Close;
       iconn.free;
       FStatusFlag := 2;
       Result := Display_Messages;

   end
   else
      {Result of checking files}
      Result := false;

 end
   else
      {Result of status flag }
      Result := true
end;

function TProject3RuckForm1.Get_KeyToActionFlag(ActionKey : integer ; ExceptionKey :integer): string;
var
KeytoActionFlag : string;
begin
          case ActionKey of
             1 : KeytoActionFlag := 'Exact match to preferred list';
             2 : KeytoActionFlag := 'Found based on genus/species ignoring sub genus ';
             3 : KeytoActionFlag := 'Match is to genus or a higher taxonomic level';
             4 : KeytoActionFlag := 'Found on a preferred list based on a full synonym check' ;
             5 : KeytoActionFlag := 'Found on the Ulster Museum Marine Dictionary on a full synonym check' ;
             6 : KeytoActionFlag := 'Found on the List of Additional Names  on a full synonym check' ;
             7 : KeytoActionFlag := 'Found on the Recorder 3 List on a full synonym check' ;
             8 : KeytoActionFlag := 'Key retained from original file' ;
             9 : KeytoActionFlag := 'Allocated from provided Taxon_List_Item_Key' ;
             10 : KeytoActionFlag := 'Allocated from provided Taxon_Version_Key - preferred list' ;
             11 : KeytoActionFlag := 'Allocated from provided Taxon_Version_Key - any allowed list' ;
             12 : KeytoActionFlag := 'From preferred list taking into account attribute' ;
          else
             KeytoActionFlag := 'NOT FOUND ON ANY SUITABLE LIST ' ;
          end;
           case ExceptionKey of
             1: KeytoActionFlag := KeytoActionFlag + '. EXCLUDED AS DUPLICATE KEY';
             2 : KeytoActionFlag := KeytoActionFlag + '. EXCLUDED AS DUPLICATE ACTUAL NAME';
             3 : KeytoActionFlag := KeytoActionFlag + '. EXCLUDED AS DUPLICATE SEARCH CODE';
           end;

result := KeyToActionFlag;

end;

function  TProject3RuckForm1.Display_Messages() :boolean;
var
  CurrentMessage : widestring;
  TempStatusFlag : integer;
  ButtonSelected : integer;
Const
   CR = chr(13) + chr(10);
begin
       Application.ProcessMessages;

       TempStatusFlag := FStatusFlag;
       case FStatusFlag of
         1:
          begin
            screen.Cursor := crhourglass;
            label2.Caption := 'PLEASE WAIT WHILE RUCKSACK IS GENERATED'  ;
            label4.Caption := 'WAIT'  ;
            label4.Font.Color := clRed;

          end;
        2 :
          begin
              screen.Cursor := crDefault;
             CurrentMessage := 'Process complete. ' + CR + 'Rucksack  =  ' +
             CR +  FRuckFile + CR + 'Log file = ' + CR + FLogFile;
             label2.Caption := CurrentMessage;
             Label4.caption := 'Click on OK or Cancel to close ';
             label4.Font.color := clGreen;

             MessageDlg  ('Process complete',mtCustom,[mbOK],0);

             TempStatusFlag := 5;

          end;
          3:
          begin
             label2.Caption := 'The input file name can not be the same as the Rucksack file to be generated';
             Label4.caption := 'Click on OK to select files again or Cancel to exit';
             label4.Font.color := clGreen;
             TempStatusFlag := 0;
             Result := false;
          end;

           { Ends proceess }
           5 :
             begin
               screen.Cursor := crDefault;
               Result := true;
             end;
           6:
              begin

                  label2.Caption := 'The specified column name for the taxa can not be found';
                  Label4.caption := 'Change parameters above to try again or Cancel to exit';
                  label4.Font.Color := clGreen;
                  TempStatusFlag := 0;
                  Result := false;
              end;
            7 :
              begin
                label2.Caption := 'File names are not valid, or file may be in use';

                Label4.caption := 'Click on OK to select files again or Cancel to exit';
                label4.Font.Color := clGreen;
                TempStatusFlag := 0;
                Result := false;
              end;
           8 :
              begin
                label2.Caption := 'Both files must be selected';

                Label4.caption := 'Click on OK to select files again or Cancel to exit';
                label4.Font.Color := clGreen;
                TempStatusFlag := 0;
                Result := false;

              end;
            9 :
              begin
                label2.Caption := 'The specified column name for the search can not be found.';
                Label4.caption := 'Change parameters above to try again or Cancel to exit';
                label4.Font.Color := clGreen;
                TempStatusFlag := 0;
                Result := false;
              end;

     end;
       FStatusFlag := TempStatusFlag;

end;



function TProject3RuckForm1.Get_Height: Integer;
begin
  Result := 538;
end;

function TProject3RuckForm1.Get_Width: Integer;
begin
  Result := 378;
end;

function TProject3RuckForm1.Get_LogFileName: string;
begin
  Result := AnsiLeftStr(FRuckFile,Length(FRuckFile)-4) + 'Log.csv';

end;

function TProject3RuckForm1.Get_FilesToUse: boolean;
var
 LengthRucksackPath : integer;
begin
  FRucksackPath :=   Get_RucksackPath;
  LengthRucksackPath := length(FRucksackPath);
  with OpenDialog1 do
    begin
      Title := 'Input Taxon Names file';
      Filter := 'Text file(*.txt)|*.txt|CSV file(*.csv)|*.csv|Rucksack(*.ruk)|*.ruk|Card(*.crd)|*.crd';
      FileName := '';
    end;
  OpenDialog1.Execute;
  FTextFile := OpenDialog1.FileName;



  with SaveDialog1 do
    begin
      Title := 'Rucksack Name';
      Filter := 'Rucksack file (*.ruk)|*.ruk';
      InitialDir :=  AnsiLeftStr(FRucksackPath,(LengthRucksackPath-1));
      Filename := '*.ruk';
    end;
  SaveDialog1.Execute;
  FRuckFile := SaveDialog1.FileName;

  FLogFile :=  Get_LogFileName;
  if  Uppercase(AnsiRightStr(FTextFile,4)) = '.CSV' then FDelimiter := ','
  else
    FDelimiter := chr(9);


  Result := Check_FileName;


end;

function TProject3RuckForm1.Check_FileName: boolean;
var
CheckFileVar : textfile;
TextFileVar : textfile;
FirstLine : widestring;
begin
  If (FTextFile <> '') AND (FRuckfile <> '') then
    begin
      if (UPPERCASE(FTextFile) = UPPERCASE(FRuckFile)) or (UPPERCASE(FLogFile) = UPPERCASE(FTextFile)) then
        begin
          FStatusFlag  := 3;
          Display_Messages;
          Result := false;
        end
        else
            begin   //ok
             Try
               AssignFile(CheckFilevar, FTextFile);
               Reset (CheckFileVar);
               Closefile (CheckFileVar);
               AssignFile(CheckFilevar,FRuckFile);
               Rewrite(CheckFileVar);
               Closefile (CheckFileVar);
               AssignFile(CheckFileVar, FLogFile);
               Rewrite(CheckFileVar);
               Closefile (CheckFileVar);
               AssignFile(TextFilevar, FTextFile);
               Reset (TextFilevar);
               If (Uppercase(AnsiRightStr(FTextFile,3)) = 'RUK') or (Uppercase(AnsiRightStr(FTextFile,3)) = 'CRD') then
                 begin
                    FIsFromRuckSack := true;
                    FHasHeading := false;
                    FTaxonNameColumn := 0;
                    FSearchNameColumn := 0;
                    FStartFlag := false;
                    Result := true;


                 end
                 else
                   begin
                     FIsFromRuckSack := false;
                     If edit1.Text <>'' then
                       FHasHeading := true
                     else
                       begin
                        FHasHeading := false;
                        FHasSearch := false;
                       end;
                     if FHasHeading = true   then
                        begin
                          if edit2.Text <> '' then   FHasSearch := true;
                          ReadLN(TextFileVar, Firstline);
                          Result := Get_FirstLineInfo(FirstLine);

                          If Result = true then
                             Result := Get_SearchInfo(FirstLine);

                        end
                      else
                        begin
                          FTaxonNameColumn := 0;
                          FSearchNameColumn := 0;
                          Result := true;
                      end;
                 end;

              except
                 begin
                   Result := false;
                   FStatusFlag  := 7;
                   Display_Messages;
                 end;
              end;
            end; //ok
            // end of try
    end
    else
      begin
        Result:= false;
        FStatusFlag  := 8;
        Display_Messages;
      end;



end;
function TProject3RuckForm1.Get_ColumnValues(
  InputValue: widestring):integer;
var
  TempInputValue : widestring;
  CommaPosition : integer;
  TempFieldValue : widestring;
  CurrentFieldNumber : integer;
begin
  TempInputValue := InputValue + FDelimiter;
  CommaPosition := Ansipos (FDelimiter,TempInputValue);
  CurrentFieldNumber := 0;
  While CommaPosition <> 0 do
  Begin
     TempFieldValue := ansiLeftstr(TempInputValue,CommaPosition-1);
     While Get_NoOFDoubleQuotes(TempFieldValue) = 1 do
       begin
         TempInputValue := ansiRightStr(TempInputValue,length(TempInputValue)-CommaPosition);
         CommaPosition := Ansipos (FDelimiter,TempInputValue);
         TempFieldValue := TempFieldvalue + FDelimiter + ansiLeftstr(TempInputValue,CommaPosition-1);
       end;
      FColumnContents[CurrentFieldNumber] := StringReplace(TempFieldValue,'"','',[rfReplaceAll]);
      CurrentFieldNumber :=  CurrentFieldNumber + 1;
      TempInputValue := ansiRightStr(TempInputValue,length(TempInputValue)-CommaPosition);
      CommaPosition := Ansipos (FDelimiter,TempInputValue);
  end;
  Result := CurrentFieldNumber - 1;


end;

function TProject3RuckForm1.Get_NoOFDoubleQuotes(FieldValue:widestring ): integer ;
  begin
      Result := 0;
      if (ansiLeftstr(FieldValue,1) = '"') and (ansiRightStr(FieldValue,1) = '"') then Result:= 2;
      if (ansiLeftstr(FieldValue,1) = '"') and (ansiRightStr(FieldValue,1) <> '"') then Result:= 1;


  end;

function TProject3RuckForm1.Get_FirstlineInfo(TextLine :string) : boolean;
var
J : integer;
  begin
       FTaxonNameColumn := -1;
       for J :=  0 to  Get_ColumnValues(TextLine)  do
         begin
           if Uppercase(FColumnContents[J]) = Uppercase(edit1.text) then
            FTaxonNameColumn := J;

         end;
       

         if  FTaxonNameColumn = -1 then
          begin
           
            FStatusFlag := 6;
            Result := Display_MessageS;


           end
         else
         Result := true;


  end;

 function TProject3RuckForm1.Get_SearchInfo(TextLine :string) : boolean;
var
J : integer;
  begin
       if FHasSearch = true then
       begin
          FSearchNameColumn := -1;
          for J :=  0 to  Get_ColumnValues(TextLine)  do
            begin
              if Uppercase(FColumnContents[J]) = Uppercase(edit2.text) then
                  FSearchNameColumn := J;
              end;


         if  FSearchNameColumn = -1 then
          begin
            FStatusFlag := 9;
            Result := Display_Messages;

           end
            else
              Result := true;
       end
       else
         Result :=  true;



  end;
  function TProject3RuckForm1.Get_TaxonNameFromLine(FileLine : integer) :string;
  var

  TempTaxonName : string;
  begin
     if (FileLine = 1) and (Fhasheading = true )   then
       Result:= ''
     else
     begin
        Get_ColumnValues(FFullLine);
        TempTaxonName :=   FColumnContents[FTaxonNameColumn];
        Result :=   TempTaxonName;
     end;


  end;
function TProject3RuckForm1.Get_SearchNameFromLine(FileLine : integer) :string;

  begin

     if (FileLine = 1) and (FHasSearch = true )   then
       Result:= ''
     else
     begin
        if FHasSearch = true then
         begin
           Get_ColumnValues(FFullLine);
           Result :=   FColumnContents[FSearchNameColumn];
         end
         else
           Result := '';
     end;


  end;
  function TProject3RuckForm1.Get_OrderBy() :string;
  Begin
     case RadioGroup1.ItemIndex of
       1 : Result := 'Order By ActualName ';
       2 : Result := 'Order By Taxon_Group, Actualname ';
       3 : Result := 'Order By Sort_order ';
       4 : Result := 'Order By SearchName ';
     else
      Result := ' Order By Original_Order';
     end;

  end;

function  TProject3RuckForm1.Get_RuckSackOutputLine( TaxonKey :string; SearchKey :string) :string;
begin
   if SearchKey = '' then Result := Taxonkey else Result := TaxonKey +  '=' + Searchkey;

end;

function  TProject3RuckForm1.Get_SortOption(RadioGroupIndex: integer ) :string ;
begin
   case RadioGroupIndex of
      0 : Result := 'Sort order = Input File Order,*' + CRLF;
      1 : Result := 'Sort order = Scientific Name,*' +  CRLF;
      2 : Result := 'Sort order = TaxonGroup/Scientific Name,*' + CRLF;
      3 : Result := 'Sort order = Recorder Taxonomic Order,*' + CRLF;
      4 : Result := 'Sort order = Search Code,*' + CRLF;
    end;



end;

function  TProject3RuckForm1.Get_CheckedAsString(CheckedText :string;  CheckedValue : boolean ) :string;
begin
  if CheckedValue = true then
     Result :=  CheckedText +  ' = Chosen,*' + CRLF
  else
     Result := CheckedText + ' = Not chosen,*' + CRLF;
end;


function  TProject3RuckForm1.Get_AdditionalTags(IResult : _Recordset; IConn: TADOConnection) : widestring;
Var
AllOutput : Widestring;

begin
  IResult := IConn.Execute ('Select TLIKey  FROM #temprucksack INNER JOIN BIOTOPE_LIST_ITEM BLI  ' +
  ' ON BLI.BIOTOPE_LIST_ITEM_KEY = #temprucksack.TLIKEY WHERE EntryType = ''<Biotope>''');
  AllOutPut := '<BIOTOPE>' + CRLF;
  While IResult.Eof = false do
   begin
     AllOutput := AllOutput + IResult.Fields[0].value + CRLF;
     IResult.MoveNext;
   end;
     AllOutput := AllOutput +  '</BIOTOPE>' + CRLF;

  IResult := IConn.Execute ('Select TLIKey  FROM #temprucksack ' +
  ' INNER JOIN Location ON Location.Location_Key = #temprucksack.TLIKEY ' +
  ' WHERE EntryType = ''<Location>''');
  AllOutPut := AllOutput + '<LOCATION>' + CRLF;
  While IResult.Eof = false do
   begin
     AllOutput := AllOutput + IResult.Fields[0].value + CRLF;
     IResult.MoveNext;
   end;
     AllOutput := AllOutput +  '</LOCATION>' + CRLF;

  IResult := IConn.Execute ('Select TLIKey  FROM #temprucksack ' +
  ' INNER JOIN Name ON NAME.NAME_Key =  #temprucksack.TLIKEY '+
  ' WHERE EntryType = ''<People>''');
  AllOutPut := AllOutput + '<PEOPLE>' + CRLF;
  While IResult.Eof = false do
   begin
     AllOutput := AllOutput + IResult.Fields[0].value + CRLF;
      IResult.MoveNext;
     end;
     AllOutput := AllOutput +  '</PEOPLE>' + CRLF;

  IResult := IConn.Execute ('Select TLIKey FROM #temprucksack ' +
  ' INNER JOIN REFERENCE ON REFERENCE.SOURCE_KEY =   #temprucksack.TLIKEY '+
  ' WHERE EntryType = ''<Reference>''');
  AllOutPut := AllOutput + '<REFERENCE>' + CRLF;
  While IResult.Eof = false do
   begin
     AllOutput := AllOutput + IResult.Fields[0].value + CRLF;
    IResult.MoveNext;
   end;
     AllOutput := AllOutput +  '</REFERENCE>' + CRLF;

   Result := AllOutput;


end;
function TProject3RuckForm1.Get_Attribute(TempTaxonName : string; Attempt : integer ): string;
begin

  case Attempt of
     1 : begin
           if (Ansipos('sens l',TempTaxonName) > 0) or (Ansipos('sens.l',TempTaxonName) > 0 ) or (Ansipos('s.l',TempTaxonName) > 0) or
            (Ansipos ('sensu l',TempTaxonName) > 0)
            or   (Ansipos('sensu.l',TempTaxonName) > 0)  then Result := 'sens l';

           if (Ansipos('sens s',TempTaxonName) > 0) or (Ansipos('sens.s',TempTaxonName) > 0 ) or (Ansipos('s.s',TempTaxonName) > 0) or
            (Ansipos('sensu s',TempTaxonName) > 0)
            or   (Ansipos('sensu.s',TempTaxonName) > 0)  then Result := 'sens s';

         end;
     2  : begin
              if (Ansipos('sens l',TempTaxonName) > 0) or (Ansipos('sens.l',TempTaxonName) > 0 ) or (Ansipos('s.l',TempTaxonName) > 0) or
            (Ansipos('sensu l',TempTaxonName) > 0)
            or   (Ansipos('sensu.l',TempTaxonName) > 0)  then Result := 'sens.l';

           if (Ansipos('sens s',TempTaxonName) > 0) or (Ansipos('sens.s',TempTaxonName) > 0 ) or (Ansipos('s.s',TempTaxonName) > 0) or
            (Ansipos('sensu s',TempTaxonName) > 0)
            or   (Ansipos('sensu.s',TempTaxonName) > 0)  then Result  := 'sens.s';

         end;
      3 :  begin
             if (Ansipos('sens l',TempTaxonName) > 0) or (Ansipos('sens.l',TempTaxonName) > 0 ) or (Ansipos('s.l',TempTaxonName) > 0) or
            (Ansipos('sensu l',TempTaxonName) > 0)
            or   (Ansipos('sensu.l',TempTaxonName) > 0)  then Result := 's.l';

           if (Ansipos('sens s',TempTaxonName) > 0) or (Ansipos('sens.s',TempTaxonName) > 0 ) or (Ansipos('s.s',TempTaxonName) > 0) or
            (Ansipos('sensu s',TempTaxonName) > 0)
            or   (Ansipos('sensu.s',TempTaxonName) > 0)  then Result  := 's.s';

         end;

     4  : begin
             if (Ansipos('sens l',TempTaxonName) > 0) or (Ansipos('sens.l',TempTaxonName) > 0 ) or (Ansipos('s.l',TempTaxonName) > 0) or
            (Ansipos('sensu l',TempTaxonName) > 0)
            or   (Ansipos('sensu.l',TempTaxonName) > 0)  then Result := 'sensu l';

           if (Ansipos('sens s',TempTaxonName) > 0 ) or (Ansipos('sens.s',TempTaxonName) > 0 ) or (Ansipos('s.s',TempTaxonName) > 0) or
            (Ansipos('sensu s',TempTaxonName) > 0)
            or   (Ansipos('sensu.s',TempTaxonName) > 0)  then Result := 'sensu s';

         end;
     5 : begin
             if (Ansipos('sens l',TempTaxonName) > 0) or (Ansipos('sens.l',TempTaxonName) > 0 ) or (Ansipos('s.l',TempTaxonName) > 0) or
            (Ansipos('sensu l',TempTaxonName) > 0)
            or   (Ansipos('sensu.l',TempTaxonName) > 0)  then Result := 'sensu.l';

           if (Ansipos('sens s',TempTaxonName) > 0 )or (Ansipos('sens.s',TempTaxonName) > 0 ) or (Ansipos('s.s',TempTaxonName) > 0) or
            (Ansipos('sensu s',TempTaxonName) > 0)
            or   (Ansipos('sensu.s',TempTaxonName) > 0)  then Result := 'sensu.s';

         end;

   end;

end;

procedure TProject3RuckForm1.ActiveFormCreate(Sender: TObject);
begin
      label2.Caption := StartUPMessage;
      label1.Caption := HeadingMessage;
end;
initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TProject3RuckForm1,
    Class_Project3RuckForm1,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
