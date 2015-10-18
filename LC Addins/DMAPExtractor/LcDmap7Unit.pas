unit LcDmap7Unit;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, LCDmapExport7_TLB, StdVcl, Recorder2000_Tlb,ADODB,ADoINt,Registry,Sysutils,Variants,StrUtils,Messages;

type
  TTLcDmap7 = class(TAutoObject, ITLcDmap7,Irecorderaddin,InewAction,IExecuteAction)

private

 // Hold the column numbers for the columns which are of use
    FRtbColNumber : integer;
    FSymColNumber : integer;
    FKeyOrderColNumber : integer;
    FFileNameColNumber : integer;
    FRcnColNumber: Integer;
    FTlnColNumber   : integer;
    FTaxonColNumber : integer;
    FCommonColNumber : Integer ;
    FTaxonGroupColNumber : Integer;
    FTaxonListAuthColNumber : Integer ;
    FSsrColNumber : integer ;
    FHeldColumnName : string;
    FSampleLatitude :Integer;
    FSampleLongitude :Integer;
    FColEN : Integer;
    //Connection
    FConnection : TADOConnection;
    FReportRecordset: Recordset;

    //Files
    FDmapDisOutputFile: textfile;
    FDmapNamOutputFile: textfile;
    FDmapMpkOutputFile: Textfile;
    FDmapFileBaseName: string;

    //Records to process
    FNumberOfRecords: integer;

    function ClearUnitvariables: boolean;
    function ConverLatLongToDMAPFormat(ARecorderLatlong: string): string;
    function ConverR6LatLongToDMAPFormat(ARecorderLong: string;ARecorderLat: string): string;
    function ConverR6ENToDMAPFormat(ARecorderEN: string): string;


    function ExportDmapData: boolean;
    function FinaliseWrite: boolean;
    procedure GetColumnUsedForName(var AColumnToUse: Integer; var AColumnNameToUse: String);
    function GetFullPath(DMAPFileType, FDmapFileBaseName: string): string;
    function GetQueryForOutput(sColumnNameToUse: string): string;
    function GetRecorderOutputPath: string;
    function Null2String(AvariantIn: variant; sStringToUse: string): string;
    procedure SetUseableFields(var ANuColumnNameToUse: string);
    function TestFiles: boolean;
    function Updateprogress(ATotalRecords, ADoneSoFar, AAction: Integer; AStatusText: string): boolean;
    function WriteDIsNam(PreviousRTN, CurrentRtn, SpatialRefForDmap, DmapSymbol, TaxonAuthority: string; DmapSpeciesNumber: integer;
      IsNewRTN: Boolean): boolean;
    protected
     {IRecorder Addin}
  function Get_Name: Widestring; safecall;
  function Get_Description: widestring; safecall;
  function Get_ImageFileName: Widestring; safecall;
  procedure setRecorder2000Interface(const iRec2000Intf: Irecorder2000); safecall;
  procedure install( const iInstalledFilepath: widestring); safecall;
  {INewAction}
  function Get_ActionCaption: Widestring; safecall;
  function Get_Hint: Widestring; safecall;
  function Get_DimmedImageFileName: Widestring; safecall;
  function Get_DisabledImageFileName: widestring; safecall;
  function Get_ParentMenu: widestring; safecall;
  function Get_CanAddtoToolbar: WordBool; safecall;

   {Idialog}
   //function Get_width: Integer;Safecall;
   //function Get_height:integer; safecall;
   //function DoOK: WordBool; safecall;
   //procedure DoCancel; safecall;
   {IExecuteAction}
   procedure Execute; Safecall;
  end;

implementation

uses ComServ,Dialogs;

//LC Constants

const
THISVERSION = 'v6.23';
RTN = 'RECOMMENDED TAXON NAME';
RTNANSG = 'RECOMMENDED TAXON NAME/ATTRIBUTE NO SUB GEN.';
RTNA = 'RECOMMENDED TAXON NAME/ATTRIBUTE';
FRTN = 'FULL RECOMMENDED TAXON NAME';
RTNWSG = 'RECOMMENDED TAXON NAME WITHOUT SUB-GENUS';
SSR = 'SAMPLE_SPATIAL_REF';
SYM = 'SYMBOL';
KOR = 'KEYORDER';
RCN = 'RECOMMENDED COMMON NAME';
TLN = 'TAXON LATIN NAME';
TAXON = 'TAXON NAME';
COMMON = 'TAXON COMMON NAME';
COFN = 'DMAPFILENAME';
OBSKEY = 'OBS KEY';
TGROUP = 'TAXON GROUP';
TLNAUTH = 'TAXON LATIN NAME AUTHORITY';
SLAT = 'SAMPLE LATITUDE';
SLONG = 'SAMPLE LONGITUDE';
R6EN = 'SAMPLE EASTINGS AND NORTHINGS';

INACTIVEMESSAGE = ' - Please generate a report before using this option. Minimum fields required for generating the ' +
' DMAP .dis and .nam files are a species name and either sample spatial ref or sample latitude and sample longitude';

CR = Chr(13)+ Chr(10);
BASEQUERY = 'SELECT * FROM #REPORT_OUTPUT';

 //LcCode
//====================================

function TTLcDmap7.ClearUnitvariables(): boolean;
// Not sure if a better way of doiung this
// Necessary if the Report window is not closed, but back used to change the fields.
// In this situation variables are not re-initilaised.

begin
  FRtbColNumber := -1;
  FSsrColNumber := -1;
  FSymColNumber := -1;
  FKeyOrderColNumber := -1;
  FFileNameColNumber := -1;
  FRcnColNumber := -1;
  FTlnColNumber := -1;
  FTaxonColNumber  := -1;
  FCommonColNumber  := -1;
  FTaxonGroupColNumber := -1;
  FTaxonListAuthColNumber := -1;
  FSampleLatitude := -1;
  FSampleLongitude := -1;
  FColEN := -1;
  Result := True;

end;


procedure  TTLcDmap7.GetColumnUsedForName(var AColumnToUse: Integer; var AColumnNameToUse : String);
begin
  AColumnToUse := -1;
  If FCommonColNumber <> -1 then
   begin
     AColumnToUse :=    FCommonColNumber;
     AColumnNameToUse  := COMMON;
   end;

  If FTaxonColNumber <> -1 then
    begin
      AColumnToUse :=     FTaxonColNumber;
      AColumnNameToUse  := TAXON;
    end;

  If FTlnColNumber <> -1 then
    begin
      AColumnToUse :=     FTlnColNumber;
      AColumnNameToUse  := TLN;
    end;

  if FRcnColNumber <> -1 then
    begin
      AColumnToUse  := FRcnColNumber;
      AColumnNameToUse  := RCN;
    end;

  if FRtbColNumber <> -1 then
    begin
      AColumnToUse  := FRtbColNumber;
      AColumnNameToUse := FHeldColumnName;
    end;

end;

procedure TTLcDmap7.SetUseableFields (var ANuColumnNameToUse: string);
var

I : integer;
RecordFieldName : string;
RevisedColumnNumber :integer;
begin
  ClearUnitvariables;
  For i := 0 to FReportRecordset.Fields.count - 1 do
    Begin
      RecordFieldName := UpperCase(FReportRecordset.fields.Get_Item(i).name);
        if (RecordFieldName = RTN) OR  (RecordFieldName = RTNANSG) OR  (RecordFieldName = RTNA)  OR  (RecordFieldName = FRTN)  OR  (RecordFieldName = RTNWSG) then
        Begin
              FRtbColNumber   := i;
              FHeldColumnName :=  RecordFieldName;
        End
        else if RecordFieldname = SSR then
          FSsrColNumber   := i
        else if RecordFieldName = SYM then
          FSymColNumber   := i
        else if RecordFieldName = KOR then
          FKeyOrderColNumber   := i
        else if RecordFieldname = COFN then
          FFileNameColNumber := i
        else if REcordFieldName = RCN then
          FRcnColNumber  := i
        else if REcordFieldName = TLN then
          FTlnColNumber  := i
        else if  REcordFieldName = TAXON then
          FTaxonColNumber  := i
        else if REcordFieldName = COMMON then
          FCommonColNumber  := i
        else if REcordFieldName = TGROUP then
          FTaxonGroupColNumber  := i
        else if REcordFieldName = TLNAUTH then
          FTaxonListAuthColNumber  := i
        else if REcordFieldName = SLAT then
          FSampleLatitude  := i
        else if REcordFieldName = SLONG then
          FSampleLongitude  := i
        else if REcordFieldName = R6EN then
          FColEN  := i;
     end;

  GetColumnUsedForName(RevisedColumnNumber, ANuColumnNameToUse);
  FRtbColNumber  := RevisedColumnNumber;

end;

//====================================

function TTLcDmap7.GetRecorderOutputPath() : string;
var
R6Registry : TRegistry;
begin
  R6Registry := TRegistry.Create;
  try
    //r6registry.rootkey := 1
    if R6Registry.OpenKey('Software\Dorset Software\Recorder 6\Settings', FALSE) then
      begin
        Result :=  R6Registry.Readstring('Report Path') + 'Output\';
      end;
  finally
    R6Registry.free;
  end;
end;

function TTLcDmap7.GetFullPath(DMAPFileType, FDmapFileBaseName: string): string;
begin
  Result:= GetRecorderOutputPath() + FDmapFileBaseName + '.' + DMAPFileType;
end;

//====================================

procedure TTLcDmap7.Execute;

// this is the main procedure which does the export
var
lRec2K : IRecorder2000;
lOldConnection : _Connection;
ColumnNameToUse : string;
begin

  lrec2k :=  CreateOLeObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  if lRec2k.reportResults = nil  then
    MessageDlg(THISVERSION + INACTIVEMESSAGE,mTInformation,[mbOk],0)
  else begin
     // showmessage ('Report available');
    FConnection := TADOConnection.Create(nil);
    lOlDConnection := FConnection.ConnectionObject;
    try
      FConnection.Connectionobject := lrec2k.ReportResults.ReportConnection as ADOINT.Connection;
      FReportRecordset := FConnection.execute('Select count(*) from #Report_Output');
      FNumberOfRecords := FReportRecordset.Fields[0].value;
      FReportRecordset := FConnection.execute('Select * from #Report_Output');
      SetUseableFields (ColumnNameToUse);
      if  (FRtbColNumber = -1) OR (FSsrColNumber = -1) AND ((FColEN = -1) AND ((FSamplelatitude = -1) OR (FSampleLongitude = -1))) then
      begin
          MessageDlg ('A species name field eg. ''Recommended Taxon Name'' and ''Spatial Ref or Lat and Long fields'' are required',mtInformation, [mbOK],0  );
      end
      else begin
        FReportRecordset := FConnection.execute(GetQueryForOutput(ColumnNameToUse));
        if not (FReportRecordset.bof) AND not  (FReportRecordset.eof)  then
          begin
            if exportDmapData = true then
              MessageDlg('DMAP export complete - ' + GetRecorderOutputPath() + FDmapFileBaseName,mtInformation, [mbOK],0  )
            else
              MessageDlg ('DMAP export has failed ', MtInformation,[mbOk],0);
            end
        else
          MessageDlg('DMAP export has no data to export',MtInformation,[Mbok],0);
        end;
    finally
      FReportRecordset.close;
      FConnection.ConnectionObject := lOldConnection;
      FConnection.Free;
    end;
  end;
end;

//====================================

function   TTLcDmap7.GetQueryForOutput(sColumnNameToUse : string): string;
var
OrderByPart: string;
MainQuery: string;
begin
// Allow for possibility of having a differnt base query
  MainQuery  :=  BASEQUERY;
  OrderByPart := ' ORDER BY ';
  if FTaxonGroupColNumber > -1 then
    OrderByPart := OrderByPart + ' [' + TGROUP + '],[' + sColumnNameToUse + ']'
  else
    OrderByPart := OrderByPart + ' [' + sColumnNameToUse + ']';

  if FKeyOrderColNumber > -1 then
    OrderByPart := OrderByPart + ', ' + KOR ;
  If  FSymColNumber > -1 then
    OrderByPart := OrderByPart + ', ' +  SYM;

  Result := MainQuery + OrderByPart;
end;

//====================================

function TTLcDmap7.ExportDmapData() : boolean;
var
//Save_Cursor : TCursor;
CurrentRtn: string;
PreviousRTN: string ;
SpatialRefForDmap: string;
DmapSymbol: string;
TaxonAuthority: string;
DmapSpeciesNumber: integer;
IsNewRTN: boolean;
IsOk: boolean;
NumberDone: integer;

begin
  PreviousRTN := '';
  NumberDone  := 0;
  DmapSpeciesNumber := 0;

  if not    FReportRecordset.eof  then
    begin
      if FFileNameColNumber <> -1 then
        FDmapFileBaseName :=    FReportRecordset.Fields[FFileNameColNumber].value
      else
        FDmapFileBaseName :=  'DMAPOUTPUT';
      end;

    if TestFiles() = true then
      begin
        FReportRecordset.movefirst;
        IsOk  := true;
        Try
         // Save_Cursor := screen.cursor;
         // Screen.cursor := crHourglass;    { Show hourglass cursor }
          UpdateProgress(1,1,1,'');
          UpdateProgress(1,1,4,'Please wait - Exporting data');

          while not (FReportRecordset.eof) and (IsOk = true) do
            begin
              inc(NumberDone,1);
              UpdateProgress(FNumberOfRecords,NumberDone,3,'');
              IsNewRTN := false;
              CurrentRtn :=  FReportRecordset.Fields[FRtbColNumber].value;
              if PreviousRTN <>  CurrentRtn then
                begin
                IsNewRTN := true;
                DmapSpeciesNumber := DmapSpeciesNumber + 1;
              end;
              if FTaxonListAuthColNumber <> -1  then
                TaxonAuthority := ' ' +  Null2String(FReportRecordset.Fields[FTaxonListAuthColNumber].value,'')
              else
                TaxonAuthority := '';

              if FSymColNumber <> -1  then
                DmapSymbol :=  Null2String(FReportRecordset.Fields[FSymColNumber].value,'')
              else
                DmapSymbol := '';
          // Use Lat/Long if fields are there

              If (FSampleLatitude <> -1) AND (FSampleLongitude <> -1) Then
              Begin
                   SpatialRefForDmap :=  ConverR6LatLongToDMAPFormat(Null2String(FReportRecordset.Fields[FSampleLongitude].value,''),Null2String(FReportRecordset.Fields[FSampleLatitude].value,''))
              end
              else if FColEn <> -1 Then
                   SpatialRefForDmap :=  ConverR6ENToDMAPFormat(Null2String(FReportRecordset.Fields[FColEN].value,'') )

              else
                   SpatialRefForDmap := ConverLatLongToDMAPFormat(Null2String(FReportRecordset.Fields[FSsrColNumber].value,''));

              if SpatialRefForDmap <> '' then
                IsOk  :=WriteDIsNam(PreviousRTN, CurrentRtn, SpatialRefForDmap, DmapSymbol, TaxonAuthority,  DmapSpeciesNumber, IsNewRTN);

              PreviousRTN := CurrentRtn;
              FReportRecordset.movenext;

            end;

         finally
           UpdateProgress(1,1,4,'');
           UpdateProgress(1,1,2,'');
         end;
         if IsOk = true then
           Result := FinaliseWrite()
         else
           Result := false;
    end
   else
   Result := false;

end;

//====================================

function    TTLcDmap7.TestFiles: boolean;
begin
  try
    AssignFile(FDmapDisOutputFile,GetFullPath('DIS', FDmapFileBaseName));
    AssignFile(FDmapNamOutputFile,GetFullPath('NAM', FDmapFileBaseName));
     // AssignFile(FDmapMpkOutputFile,FullPath('MPK', FDmapFileBaseName));
    Rewrite(FDmapDisOutputFile);
    closefile(FDmapDisOutputFile);
    rewrite (FDmapNamOutputFile);
    closefile(FDmapNamOutputFile);
    Append(FDmapNamOutputFile);
    Append(FDmapDisOutputFile);

     // Rewrite (FDmapMpkOutputFile);
     // closeFile(FDmapMpkOutputFile);
     Result:= true;
   except
     begin
       MessageDlg ('A file name is invalid or the DMAP files are opened by another application',mtInformation,[MbOK],0);
       Result := false;
     end ;
   end;
end;

//====================================

function TTLcDmap7.WriteDIsNam(PreviousRTN, CurrentRtn, SpatialRefForDmap, DmapSymbol, TaxonAuthority: string; DmapSpeciesNumber: integer; IsNewRTN :Boolean) : boolean;
begin
  Try
    if IsNewRTN = true then
      begin
      write(FDmapNamOutputFile,intToStr(DmapSpeciesNumber), ' ', CurrentRtn + TaxonAuthority ,CR);
      //  Closefile(FDmapNamOutputFile);
      if PreviousRTN <> '' then Write (FDmapDisOutputFile, ' -1',CR);
        Write (FDmapDisOutputFile, DmapSpeciesNumber,CR);
      end;

      Write (FDmapDisOutputFile,  SpatialRefForDmap,' ', DmapSymbol,CR);
      Result := true;

  Except
    Result := false;
  end;
end;

//====================================

function TTLcDmap7.FinaliseWrite(): boolean;
begin
  Result := false;
  Try
     //AssignFile(FDmapDisOutputFile,GetFullPath('DIS', FDmapFileBaseName));
    // AssignFile(FDmapNamOutputFile,GetFullPath('NAM', FDmapFileBaseName));
     //Append(FDmapDisOutputFile);
     //Append(FDmapNamOutputFile);
     Write (FDmapDisOutputFile, ' -1',CR);
     Result := true;
  Finally
    CloseFile(FDmapDisOutputFile);
    CloseFile(FDmapNamOutputFile);
  end;
end;


//end addin

//====================================
function   TTLcDmap7.GET_ActionCaption: Widestring;
begin
  Result := 'Extract data'
end;

//====================================
function   TTLcDmap7.GET_Hint: Widestring;
begin
  Result := 'New DMAP File Exporter'
end;

//====================================
function   TTLcDmap7.Get_DimmedImageFileName: Widestring;
begin
  Result := 'Default.bmp'
end;

//====================================
function   TTLcDmap7.Get_DisabledImageFileName: Widestring;
begin
  Result := 'Default.bmp'
end;

//====================================
function TTLcDmap7.Get_ParentMenu: Widestring;
begin
 Result := 'DMAP'
end;

//====================================
function TTLcDmap7.Get_canAddToToolBar: WordBool;
begin
 Result := false
end;

//====================================
function TTLcDmap7.Get_Description: Widestring;
begin
  Result := 'DMAP File generator v 6.23'
end;
//======================================
function TTLcDmap7.Get_Name: Widestring;
begin
  Result := 'DMAP Extractor V6.23.1'
end;
//======================================
function TTLcDmap7.Get_ImageFilename: Widestring;
begin
  Result := 'Default.bmp'
end;
//======================================
procedure TTLcDmap7.setRecorder2000Interface( const IRec2000Intf: Irecorder2000);
begin

end;
//==========================================
procedure TTLcDmap7.install(const iInstalledFilePath: widestring) ;
begin

end;
//==========================================

function TTLcDmap7.Null2String(AvariantIn: variant; sStringToUse : string): string;
begin
  if varIsNull(AvariantIn)  then Result := sStringToUse
  else
    Result := AvariantIn;
end;

//==========================================

function TTLcDmap7.Updateprogress(ATotalRecords,
  ADoneSoFar, AAction: Integer; AStatusText: string): boolean;
var
lRecorder2000 : IRecorder2000;

begin
   if ATotalRecords = 0 then ATotalRecords := 1;

  lRecorder2000 :=  CreateOLeObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  case AAction of
    1 : lRecorder2000.RecorderMainForm.StartProgressBar();
    2 : lRecorder2000.RecorderMainForm.StopProgressBar();
    3 : if ADoneSoFar mod 100 = 0 then
             lRecorder2000.RecorderMainForm.Progress := ADoneSoFar div ATotalRecords*100;
    4 : lRecorder2000.RecorderMainForm.StatusText := AStatusText;
  end;
  Result := true;

end;

//==========================================
function TTLcDmap7.ConverR6ENToDMAPFormat(ARecorderEN: string): string;
var
Eastings : Double;
Northings : Double;
Separator : integer;

begin

    Result:='';
    Separator := AnsiPos(',',ARecorderEN);
    if Separator > 0 then
    begin
      Eastings := StrToFloat(AnsiLeftStr(ARecorderEN,Separator-1))/1000;
      Northings := StrToFloat(AnsiRightStr(ARecorderEN,(length(ARecorderEN)-Separator)))/1000;
      Result := FloatToStr(Eastings) + ',' +  FloatToStr(Northings)
    end;


end;


//==========================================

function TTLcDmap7.ConverLatLongToDMAPFormat(ARecorderLatlong: string) : string;
var
ANewString : string;
begin

   // because this will only replace ', and N ' this will not affect OSGB or OSNI ref.
   ANewString := stringreplace(ARecorderLatlong, ',','+',[rfReplaceAll]);
   ANewString := stringreplace(ANewSTring, 'N ','+',[rfReplaceAll]);
   ANewString := stringreplace(ANewSTring, ' ','',[rfReplaceAll]);

   Result := ANewstring;
end;
//==========================================
 //==========================================

function TTLcDmap7.ConverR6LatLongToDMAPFormat(ARecorderlong: string; ARecorderlat: string) : string;
Var
LongElement : string;
LatElement: string;

begin

  Result := '';
  if (ARecorderLong <> '0') and  (ARecorderlat <> '0') and (ARecorderLong <> '')  and (ARecorderLat <> '')then
  begin

    If AnsileftStr(Arecorderlong,1) = '-' then
     LongElement := AnsirightStr(ARecorderlong,length(ArecorderLong)-1) + 'W'
    else
     LongElement := ARecorderlong + 'E';
    If AnsileftStr(Arecorderlat,1) = '-' then
      LatElement := Ansirightstr(ArecorderLat,length(ArecorderLat)-1) + 'S+'
    else
    LatElement := ARecorderLat + 'N+';

    Result := LatElement + LongElement;
  end;
end;
//==========================================
initialization
  TAutoObjectFactory.Create(ComServer, TTLcDmap7, Class_TLcDmap7,
    ciMultiInstance, tmApartment);
end.
