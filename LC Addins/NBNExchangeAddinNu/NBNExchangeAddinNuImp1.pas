//==============================================================================
// Author:   Stuart Ball, JNCC    stuart.ball@jncc.gov.uk
// Date:     May 2008
// Purpose:  An export Filter addin for Recorder 6 which outputs data
//           to a file in NBN Gateway Transfer Format. This allows submission
//           of data directly from Recorder to the NBN Gateway.
//------------------------------------------------------------------------------
// Based on Gateway Standard Import Format version 2.0 of 11 Oct 2003
// and ammended XML header 2.1 of September 2004.
// Recorder 6 version takes account of specification written by Andy Brewer
// early in 2007.
//------------------------------------------------------------------------------
// This version uses temporary tables in the SQL Server database, NBNData,
// and stored procedures called "nbn_exchange_..."
// It takes account of the facilities, introduced in Recorder 6.12, to
// revalidate data. In this version, records are revalidated BEFORE export -
// so the Export Filter should exclude records amongst those that the user
// chose to export which failed this validation check. The IExportFilter6
// interface therefore sends both a list of keys to export and a list
// of invalid keys that failed validation.
//------------------------------------------------------------------------------
// Modification history
//------------------------------------------------------------------------------
// Original version for Recorder 2000 (Sept 2004) used an Access97 database
// for temporary storage. Wrote a single output text file with a header section
//  in XML format followed by a tab delinated list for the export rows.
// The output columns were fixed and ZeroAbundance and Confidential rows
// were always excluded.
//
// Recorder 6 version
// July 2007
// Stuart Ball, JNCC
//
// - Used an Access 2000 database to store temporary working tables
// - Handles Sampling method, Comments, Substrate and abundance
// - Provides options for the user to select whether to export Recorder's
//   names, Determiner's name, Sampling method, Substrate and Comment fields
// - Shows measurement types that are in use and allows the user to select
//   which ones to export. Each measurement type is exported to a separate col
//   in the output file
// - Provides options to decide whether to export zero-abundance and sensitive
//   (CONFIDENTIAL) records. The user can only decide to turn on export of
//   sensitive data if that option is enabled in Recorder AND the user doing
//   the export has the necessary security rating. Otherwise records flagged
//   CONFIDENTIAL are NOT exported.
// - Writes XML Header data to a seperate file with the same path and name
//   as the main .txt file, but with an .xml extension. These two files are
//   zipped into a single result file (and the user can decide whether or not
//   to keep the original .txt and .xml file)
//
//------------------------------------------------------------------------------
// Date     Author  Change
//------------------------------------------------------------------------------
// 16/01/2009 SGB   Turned out to be a problem if the name of a measurement
//                  qualifier duplicated one of the existing column names
//                  In the test case, the measurement was called "Substrate"
//                  This is tested for and the duplicate name suffixed with
//                  a digit (starting at 2).
// 07/07/2010 SGB   Bug in exporting "-Y" type dates - they were exporting
//                  yyyy -
//                  Changed defaults so that sample type, comment and abundances
//                  default to off
//                  Truncate strings to 255 characters
// 14/12/2012 SGB   Update NBN Exchange format - truncate Recorder and
//                  Determiner to 140 characters
// 10/12/2013 MDW   Modified to compile under Delphi 7
//                  This version excludes the update of the database files
//                  on install
//                  Designed for use with R6 version 6.22 or above which has the
//                  procedures and functions built in.
//                  Log file goes to the Reports not addin folder.
//                  Removes any temp data from the export  and tells the user if
//                  this is the case
//                  Allows option to Export records which fail validation,
//                  but only if there are some valid records.
//
//==============================================================================

unit NBNExchangeAddinNuImp1;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, NBNExchangeAddinNu_TLB, StdVcl,Recorder2000_TLB,ADODB,Registry,SysUtils,StrUtils,Dialogs,Classes,db,NBNTransferOptions,controls,AbZipper, AbZipTyp;

  // VersionInfo, NBNTransferOptions, VagueDate,

type
  TKeyType = (ktExport, ktInvalid);
  TMeasurement = Record
    Key: ansistring;
    Name: ansistring;
    Selected: boolean;
  end;

  TMeasurements = Array of TMeasurement;

  TParam = Record
    Name: ansistring;
    Val: integer;
  End;

  TParams = Array of TParam;

  TNBNExchangeAddinNuImp = class(TAutoObject, INBNExchangeAddinNuImp,IRecorderAddin, IFilter,IExportFilter,IExportFilter6)

  private
  { Global variables}
  FError: ansistring;               // last error
  FRecorder2000: IRecorder2000; // interface to Rec2002
  // switches to decide what to export - set by option form
  FConfidential: boolean;
  FZeroAbundance: boolean;
  FRecorders: boolean;
  FDeterminer: boolean;
  FSampleType: boolean;
  FComment: boolean;
  FExportInvalid : boolean;
  FSubstrate: Boolean;
  FSampleLocationNameFirst: boolean;
  FAbundance: boolean;   // this is true if we are exporting measurements
  FnRows: Longint; // Number of observations to export
  FnPossibleRows: Longint; // Number of  observations which have been selected for export
  FnExcludedTempRows: Longint;  // Number of  observations which are  excluded because they are temporrary data
  // store measurement types
  FMeasurementTypes: TMeasurements;
  FNBNData: TADOConnection;     // the connection to NBNData
  FReportPath: ansistring;     // path to Report directory where the temporary database is located
     // List of column names
  FColNames: TStrings;

  procedure RetrieveMeasurements(oOpt: TformOptions);
  function GetMeasurementTypes: integer;
  procedure LoadMeasurements(oOpt: TformOptions);
  function OptionsForm: boolean;
  procedure ShowError(const item, msg: ansistring);
  function Get_Recorder2000: IRecorder2000;
  procedure Show(const sFile: ansistring; const iExport, iInvalid: IKeyList);
  function Get_AddinPath: ansistring;
  function WriteKeys(const iKeys: IKeyList; const aType: TKeyType): Longint;
  function CreateTaxonMeasurements: integer;
  procedure WriteLog(const logentry: ansistring; const app: boolean);
  function ExecuteStoredProcedure(const sName, sMsg: ansistring; oParams: TParams): boolean;
  function RowsToExport: Longint;
  procedure DeleteUnzippedFiles(const sFileName: ansistring);
  procedure DoAbundance;
  function DoExport(const sFileName: ansistring): boolean;
  function WriteHeader(const sFileName: ansistring): boolean;
  function InsertIntoTempTable(const sKey, sTable: ansistring; const aType: TKeyType): integer;
  procedure AddMeasurement(const sTypeKey, sName: ansistring; var done, n: Longint);
  function AddStream(fStream: TFileStream; const sLine: ansistring): Longint;
  function WriteField(oQuery: TADOQuery; const iField: integer): boolean;
  function GetItem(const sTable, sField, sKey, sKeyField: ansistring): ansistring;
  function GetField(oQuery: TADOQuery; const iField: integer): ansistring;
  function GetMetadata(const sMetadata: ansistring): ansistring;
 
  function TruncateString(const aString: ansistring; sDelim: TSysCharSet; const iMax: integer): ansistring;
  function ExecuteSQL(const sSQL: ansistring): boolean;
  function CheckColName(const sName: ansistring): ansistring;
  procedure DoZip(const sFileName: ansistring);
  protected
  { IRecorderAddin }
   function Get_Name: WideString; safecall;
   function Get_Description: WideString; safecall;
   function Get_ImageFileName: WideString; safecall;
   procedure Install(const iInstalledFilePath: WideString); safecall;
   function Get_NBNData: TADOConnection;
   {IFilter }

   function Get_DefaultFileExtension: WideString; safecall;

  { IExportFilter }
    function ExportFile(const iItemsToExport: IKeyList; const iDestinationFile: WideString): WordBool; safecall;
    function Get_LastExportError: WideString; safecall;

  { IExportFilter6 }
    function ExportFileWithFilteredInvalids(const iItemsToExport: IKeyList;
                                            const iInvalidItems: IKeyList;
                                            const iDestinationFile: WideString): WordBool; safecall;


  public
    constructor Create;
    destructor Destroy; override;
  { properties}
    property Recorder2000: IRecorder2000 read Get_Recorder2000;
    property NBNData: TADOConnection read Get_NBNData;
    property AddinPath: ansistring read Get_AddinPath;
  end;

implementation

uses ComServ,login;

const RegistryKey = 'Software\Dorset Software\Recorder 6';
      EoL         = Char(13)+Char(10); // Windows end-of-line
      Quote       = Char(39); // single quote
      Tab         = Chr(9);   // tab character

      DESCRIPTION = '%s Write observations to NBN Exchange Format Version 2.3.2, January 2010. ' +
                    'Zipping is carried out by TurboPower Abbrevia version 3.05, released under ' +
                    'Mozilla Public Licence version 1.1.';
      MEASUREMENT_SQL = 'SELECT MEASUREMENT_TYPE.MEASUREMENT_TYPE_KEY, MEASUREMENT_TYPE.SHORT_NAME AS mType ' +
                    'FROM TAXON_OCCURRENCE_DATA INNER JOIN MEASUREMENT_QUALIFIER ON ' +
                    'TAXON_OCCURRENCE_DATA.MEASUREMENT_QUALIFIER_KEY = MEASUREMENT_QUALIFIER.MEASUREMENT_QUALIFIER_KEY INNER JOIN ' +
                    'MEASUREMENT_TYPE ON MEASUREMENT_QUALIFIER.MEASUREMENT_TYPE_KEY = MEASUREMENT_TYPE.MEASUREMENT_TYPE_KEY ' +
                    'GROUP BY MEASUREMENT_TYPE.MEASUREMENT_TYPE_KEY, MEASUREMENT_TYPE.SHORT_NAME ' +
                    'ORDER BY mType';
      OCCURRENCE_DATA_SQL = 'SELECT [##nbn_exchange_obs].RecordKey, TAXON_OCCURRENCE_DATA.DATA, MEASUREMENT_QUALIFIER.SHORT_NAME AS Qualifier, ' +
                    'MEASUREMENT_UNIT.SHORT_NAME AS Unit, TAXON_OCCURRENCE_DATA.ACCURACY ' +
                    'FROM MEASUREMENT_UNIT INNER JOIN [##nbn_exchange_obs] INNER JOIN ' +
                    'MEASUREMENT_QUALIFIER INNER JOIN TAXON_OCCURRENCE_DATA ON ' +
                    'MEASUREMENT_QUALIFIER.MEASUREMENT_QUALIFIER_KEY = TAXON_OCCURRENCE_DATA.MEASUREMENT_QUALIFIER_KEY ON ' +
                    '[##nbn_exchange_obs].RecordKey = TAXON_OCCURRENCE_DATA.TAXON_OCCURRENCE_KEY ON ' +
                    'MEASUREMENT_UNIT.MEASUREMENT_UNIT_KEY = TAXON_OCCURRENCE_DATA.MEASUREMENT_UNIT_KEY ' +
                    'WHERE (MEASUREMENT_UNIT.MEASUREMENT_TYPE_KEY = ''%s'')' +
                    'GROUP BY [##nbn_exchange_obs].RecordKey, TAXON_OCCURRENCE_DATA.DATA, MEASUREMENT_QUALIFIER.SHORT_NAME, ' +
                    'MEASUREMENT_UNIT.SHORT_NAME, TAXON_OCCURRENCE_DATA.ACCURACY';
      SURVEY_SQL =  'SELECT S1.SurveyKey, ITEM_NAME, dbo.ufn_RtfToPlaintext(DESCRIPTION) as Des, dbo.ufn_RtfToPlaintext(GEOGRAPHIC_COVERAGE) as Geog, ' +
                    'FROM_VAGUE_DATE_START, FROM_VAGUE_DATE_END, FROM_VAGUE_DATE_TYPE, TO_VAGUE_DATE_START, TO_VAGUE_DATE_END, ' +
                    'TO_VAGUE_DATE_TYPE FROM SURVEY INNER JOIN ' +
                    '(SELECT SurveyKey FROM [##nbn_exchange_obs] GROUP BY SurveyKey) AS S1 ' +
                    'ON SURVEY.SURVEY_KEY = S1.SurveyKey';
      TEMPSURVEY_SQL = 'Delete  From ##nbn_exchange_obs ' +
                     ' FROM ##nbn_exchange_obs INNER JOIN Survey S ON S.Survey_Key =  ##nbn_exchange_obs.SurveyKey '  +
                     ' WHERE  S.Temporary_Survey = 1 ';


      COL_NAMES: array[1..21] of ansistring =
                    ('RecordKey',
                    'SurveyKey',
                    'SampleKey',
                    'StartDate',
                    'EndDate',
                    'DateType',
                    'TaxonVersionKey',
                    'ZeroAbundance',
                    'Sensitive',
                    'SiteKey',
                    'SiteName',
                    'Projection',
                    'GridReference',
                    'Precision',
                    'East',
                    'North',
                    'Recorder',
                    'Determiner',
                    'SampleMethod',
                    'Comment',
                    'Substrate');

{ TNBNExchangeAddinNuImp }
   //------------------------------------------------------------------------------
// Get the interface to Recorder 2002
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.Get_Recorder2000: IRecorder2000;
begin
  if FRecorder2000 = nil then
  begin
     FRecorder2000 := CreateOLEObject('Recorder2000.AutoApplicationSettings') as IRecorder2000;
  end;
  Result := FRecorder2000;

end;
//------------------------------------------------------------------------------
// If the export fails (ExportFile returns False)  this is used by Recorder to
// get an error message for the last record that failed to export.
//------------------------------------------------------------------------------

function TNBNExchangeAddinNuImp.Get_LastExportError: WideString;
begin
  Result := FError;
end;

function TNBNExchangeAddinNuImp.ExportFile(const iItemsToExport: IKeyList; const iDestinationFile: WideString): WordBool;
Begin
   // nothing happens here

end;

//------------------------------------------------------------------------------
// This is the key routine that is called to export data
// Returns True on success       **********1
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.ExportFileWithFilteredInvalids(const iItemsToExport,
  iInvalidItems: IKeyList; const iDestinationFile: WideString): WordBool;
var i: integer;
    oParams: TParams;
begin
{$IFDEF DEBUG}
  WriteLog('Started', False);
{$ENDIF}
  FColNames := TStringList.Create;
  try
    for i := 1 to High(COL_NAMES) do
      FColNames.Add(COL_NAMES[i]);

 {$IFDEF DEBUG}
    Show(iDestinationFile, iItemsToExport, iInvalidItems);
{$ENDIF}

    Result := False;
    if ExecuteStoredProcedure('nbn_exchange_create_tables', 'Creating temporary tables', nil) then
      if OptionsForm then
      begin
        CreateTaxonMeasurements;
        if WriteKeys(iItemsToExport, ktExport) > 0  then
        begin
          WriteKeys(iInvalidItems, ktInvalid);
          // set up parameters for the nbn_exchange_get_obs stored proceduere

          SetLength(oParams, 2);
          oParams[0].Name := 'ZeroAbundance';
          oParams[0].Val := Abs(Integer(FZeroAbundance));
          oParams[1].Name := 'Sensitive';
          oParams[1].Val := Abs(Integer(FConfidential));;
          if ExecuteStoredProcedure('nbn_exchange_get_obs','Getting keys of rows to export', oParams) then
          begin
            // set up parameters for the nbn_exchange_basic_update stored proceduere
            SetLength(oParams, 1);
            oParams[0].Name := 'SampleLocationNameFirst';
            oParams[0].Val := Abs(Integer(FSampleLocationNameFirst));
            if not ExecuteStoredProcedure('nbn_exchange_basic_update','Getting basic record information', oParams) then
              Exit;

            FnPossibleRows :=  RowsToExport;
            ExecuteSQL(TEMPSURVEY_SQL);


            // the rest of the stored procedured don't have parameters
            oParams := nil;

            if FRecorders then
            begin
              if not ExecuteStoredProcedure('nbn_exchange_add_recorders','Adding recorder names', nil) then
                Exit;
            end;
            if FDeterminer then
            begin
              if not ExecuteStoredProcedure('nbn_exchange_add_determiner','Adding determiners', nil) then
                Exit;
            end;
            if FSampleType then
            begin
              if not ExecuteStoredProcedure('nbn_exchange_add_sample_type','Adding sampling methods', nil)then
                Exit;
            end;
            if FComment then
            begin
              if not ExecuteStoredProcedure('nbn_exchange_add_comment','Adding comments', nil) then
                Exit;
            end;
            if FSubstrate then
            begin
              if not ExecuteStoredProcedure('nbn_exchange_add_substrate','Adding substrate', nil) then
                Exit;
            end;
            // get the number of rows we are going to export
            FnRows := RowsToExport;
            FnExcludedTempRows := FnPossibleRows - FNRows;

            if fNRows <> 0   then
            Begin
              DoAbundance;

              if DoExport(iDestinationFile) then
                begin
                  WriteHeader(iDestinationFile);
                  DoZip(iDestinationFile);
                  DeleteUnzippedFiles(iDestinationFile);
                  Result := True;
                end
                  else

                     FError := 'Cannot write output file';
             end
             else
               FError := 'All data is temporary and not exported!';

         end;
      end
        else  FError := 'No observations to export!';


        if Result = false then
              showmessage ('Export Failed Cancel and Try Again - ' + Get_LastExportError);
        FColNames.Free;
    end;
   except
       if Result = false then
             FError := 'Check path snd file name. Make sure you have write permission on the folder.';
              showmessage ('Export Failed Cancel and Try Again - ' + Get_LastExportError);
        FColNames.Free;
   end;

end;

function TNBNExchangeAddinNuImp.Get_DefaultFileExtension: WideString;
begin
  Result := 'txt';
end;

function TNBNExchangeAddinNuImp.Get_Description: WideString;
begin
   Result := 'Extracts data in the format required by the NBN. This version needs Recorder 6.29 or later to run.';
end;

function TNBNExchangeAddinNuImp.Get_ImageFileName: WideString;
begin
   Result := 'default.bmp';
end;

function TNBNExchangeAddinNuImp.Get_Name: WideString;
begin
   Result := 'NBN Exchange Format - v6.29 ';
end;


procedure TNBNExchangeAddinNuImp.Show(const sFile: ansistring; const iExport, iInvalid: IKeyList);
//var sout: string;
begin

// WriteLog(Format('Write %d items to file %s.',[iExport.ItemCount, sFile]), True);
//  WriteLog(Format('iExport size: %d bytes', [SizeOf(iExport)]), True);
//  if iInvalid.ItemCount > 0 then
//    WriteLog(Format('There are %d invalid items.',[iInvalid.ItemCount]),True)
//  else
 //   WriteLog('No invalid items.', True);

//  sOut := Format('Exort file: %s', [sFile]);
//  sOut := sOut + #13 + #13 + 'Items to export:' + #13 + ShowList(iExport);
//  if iInvalid.ItemCount > 0 then
//    sOut := sOut + #13 + 'Invalid items:' + #13 + ShowList(iInvalid);
//  MessageDlg(sfile, mtWarning, [mbOK], 0);
end;

       //------------------------------------------------------------------------------
// Write logentry to a log file
// The file is opened and closed each time, so should be there even
// if it crashes
//------------------------------------------------------------------------------
procedure TNBNExchangeAddinNuImp.WriteLog(const logentry: ansistring; const app: boolean);
var f: Text;
    work: string;
begin
  work := AddinPath + 'export_log.txt'; // file name
  AssignFile(f, work);
  if FileExists(work) then
  begin
    if app then
      Append(f) // append it to end of file
    else
      Rewrite(f) // start at the beginning of the file
  end
  else
  begin
    Rewrite(f)
  end;
  DateTimeToString(work, 'dd/mm/yyyy hh:nn:ss', now());
  work := format('%s:  %s', [logentry, work]);
  Writeln(f, work);
  CloseFile(f);
end;

  //------------------------------------------------------------------------------
// Get the report path from the registry - only needed for logfile (Still called addin path, but isn't now)
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.Get_AddinPath: ansistring;
var oReg: TRegistry;
begin
  if FReportPath = '' then
  begin
     {Get report path from registry}
     oReg := TRegistry.Create;
     try
        If oReg.Openkey('Software\Dorset Software\Recorder 6\Settings',FALSE) then
        begin
              FReportPath := oReg.ReadString('Report Path');
              WriteLog('Got Report path "' + FReportPath + '"', True);
        end
     finally
        oReg.Free;
     end;
  end;
  Result := FReportPath;
end;
 //------------------------------------------------------------------------------
// Open the connection to NBNData
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.Get_NBNData: TADOConnection;
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
    //==============================================================================
// Property implementation
//==============================================================================
// Clean up when we quit - make sure the database is closed
//------------------------------------------------------------------------------
destructor TNBNExchangeAddinNuImp.Destroy;
begin
  If FNBNData <> nil then
  begin
    // clean up our working tables
     ExecuteStoredProcedure('nbn_exchange_drop_tables','Removing temporary tables', nil);
     FNBNData.Close;
  end;
  FRecorder2000 := nil;
  FMeasurementTypes := nil;
{$IFDEF DEBUG}
  WriteLog('End', True);
{$ENDIF}
  inherited;
end;
 //------------------------------------------------------------------------------
// Initailise value for our key variables
//------------------------------------------------------------------------------
constructor TNBNExchangeAddinNuImp.Create;
begin
  inherited Create;
  FRecorder2000 := nil;
  FNBNData      := nil;
  FReportPath    := '';
end;
  //==============================================================================
// Local routines
//==============================================================================
// Execute a stored procedure that doesn't take parameters or return a dataset
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.ExecuteStoredProcedure(const sName, sMsg: ansistring; oParams: TParams): boolean;
var oProc: TADOStoredProc;
    i: integer;
begin

  Result := False;
{$IFDEF DEBUG}
  WriteLog(Format('Executing stored procedure: %s', [sName]), True);
{$ENDIF}
  if NBNData.Connected then
  begin
    oProc := TADOStoredProc.Create(nil);
    try
      Recorder2000.RecorderMainForm.StatusText := sMsg;
      oProc.Connection := NBNData;
      oProc.ProcedureName := sName;
      oProc.CommandTimeout := 0;
      if oParams <> nil then
      begin
        oProc.Parameters.Clear;
        for i := Low(oParams) to High(oParams) do
          with oProc.Parameters.AddParameter do
          begin
            Name := oParams[i].Name;
            DataType := ftInteger;
            Value := oParams[i].Val;
          end;
        end;
      try
        oProc.ExecProc;
        Result := True;
      except
        on e:Exception do
        begin
          FError := e.Message;
          ShowError('Stored procedure: ' + sName, e.Message);
          Result := False;
        end;
      end;
    finally
      oProc.Free;
      Recorder2000.RecorderMainForm.StatusText := '';
    end;
  end;
end;
    //------------------------------------------------------------------------------
// If an exception occurs, show the message to the user
// This is needed because Rec6.13 doesn't seem to use LastError after
// an exception
//------------------------------------------------------------------------------
procedure TNBNExchangeAddinNuImp.ShowError(const item, msg: ansistring);
var sMsg: ansistring;
begin
  sMsg := Format('An internal exception occurred in the NBN Exchange addin in' + #13 +
                 '%s' + #13 + #13 + 'The exception message is:' + #13 + '%s', [item, msg]);
  ShowMessage(sMsg);
end;
//------------------------------------------------------------------------------
// Displays a form which prompts the user for the columns to export
// and whether to include confidential and zero abundance data
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.OptionsForm: boolean;
var oOpt: TformOptions;
begin
  Result := False;
  oOpt := TformOptions.Create(nil);
  try
    LoadMeasurements(oOpt);
    oOpt.ShowModal;
    if oOpt.ModalResult <> mrCancel then
    begin
      Result:= True;
      FConfidential := oOpt.Confidential;
      FZeroAbundance := oOpt.ZeroAbundance;
      FRecorders := oOpt.RecorderNames;
      FDeterminer := oOpt.DetNames;
      FSampleType := oOpt.SampleType;
      FComment := oOpt.Comment;
      FSubstrate := oOpt.Substrate;
      FSampleLocationNameFirst := oOpt.SampleLocationNameFirst;
      FExportInValid := oOpt.ExportInValid;
      RetrieveMeasurements(oOpt);

    end;
  finally
    oOpt.Free;
{$IFDEF DEBUG}
    WriteLog('Got user input.', True);
{$ENDIF}
  end;
end;
 //------------------------------------------------------------------------------
// Load the labels from FMeasurementTypes into the dialog box
//------------------------------------------------------------------------------
Procedure TNBNExchangeAddinNuImp.LoadMeasurements(oOpt: TformOptions);
var i: integer;
begin
  oOpt.ClearMeasurementTypes;
  i := GetMeasurementTypes;
  if i>0 then
    for i := Low(FMeasurementTypes) to High(FMeasurementTypes) do
      oOpt.AddMeasurementType(FMeasurementTypes[i].Name);
end;
 //------------------------------------------------------------------------------
// Flag the measurment types that the user ticked in the dialog box as
// selected
//------------------------------------------------------------------------------
Procedure TNBNExchangeAddinNuImp.RetrieveMeasurements(oOpt: TformOptions);
var i, j: integer;
    sType: ansistring;
begin
  // make sure they are all unselected
  for i := Low(FMeasurementTypes) to High(FMeasurementTypes) do
    FMeasurementTypes[i].Selected := False;

  if oOpt.MeasurementTypeCount > 0 then
    for i := 0 to oOpt.MeasurementTypeCount- 1 do
      if oOpt.MeasurementTypeSelected[i] then
      begin
        sType := oOpt.MeasurementType[i];
        for j := Low(FMeasurementTypes) to High(FMeasurementTypes) do
          if FMeasurementTypes[j].Name =  sType then
          begin
            FMeasurementTypes[j].Selected := True;
            Break; // we found it!
          end;
      end;
end;
 //------------------------------------------------------------------------------
// Get the MEASUREMENT_TYPE_KEYs that are actually used in
// TAXON_OCCURRENCE_DATA and load them and their labels into the
// FMeasurementTypes array
//------------------------------------------------------------------------------
Function TNBNExchangeAddinNuImp.GetMeasurementTypes: integer;
var oQuery: TADOQuery;
begin
{$IFDEF DEBUG}
   WriteLog('Getting measurement types', True);
{$ENDIF}
  Result := 0;
  oQuery := TADOQuery.Create(nil);
  try
    oQuery.Connection := NBNData;
    oQuery.SQL.ADD(MEASUREMENT_SQL);
    oQuery.CommandTimeout := 0;
    oQuery.Open;
    try
      if oQuery.RecordCount > 0 then
      begin
         // we need to know how many there are to dimension the arraqy
         oQuery.Last;
         SetLength(FMeasurementTypes,oQuery.RecordCount);

         // load keys and labels into the array
         oQuery.First;
         while not oQuery.EOF do
         begin
           FMeasurementTypes[Result].Key := oQuery.Fields[0].AsString;
           FMeasurementTypes[Result].Name := oQuery.Fields[1].AsString;
           // initailly, set Selected to false
           FMeasurementTypes[Result].Selected := False;
           inc(Result);
           oQuery.Next;
         end;
      end;
    finally
      oQuery.Close;
    end;
  finally
    oQuery.Free;
  end;
end;
 //------------------------------------------------------------------------------
// Takes the list of keys supplied by the Recorder interface and writes a row to
// the nbn_exchange_export table for each entry consisting of the table name and key.
// Need to deal with "MIXED" type lists as well as lists based on a single
// source table. Show progress bar.
// Returns the number of rows it INSERTed
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.WriteKeys(const iKeys: IKeyList; const aType: TKeyType): Longint;
var i: Longint;
    sKey, sTable, sMsg: ansistring;
    bMixed: boolean;
begin
  Result := 0;
  if iKeys.ItemCount > 0 then
  try
    // is it a "MIXED" list?
    bMixed := CompareText(iKeys.TableName, 'MIXED') = 0;
    // if it is not "MIXED", then the table name for all of the keys
    // is in the TableName property of the KeyList
    if not bMixed then
      sTable := iKeys.TableName;

    if aType = ktExport then
    begin
      sMsg := 'Writing keys of observation to temporary table.';
{$IFDEF DEBUG}
      WriteLog(Format('Writing keys of %d observation to temporary table.', [iKeys.ItemCount]), True);
{$ENDIF}
    end
    else
    begin
      sMsg := 'Writing invalid keys to temporary table.';
{$IFDEF DEBUG}
      WriteLog(Format('Writing %d invalid keys to temporary table.', [iKeys.ItemCount]), True);
{$ENDIF}
    end;

    Recorder2000.RecorderMainForm.StartProgressBar;
    Recorder2000.RecorderMainForm.StatusText := sMsg;
    for i:= 0 to iKeys.ItemCount-1 do
    begin
      sKey := iKeys.GetKeyItem(i).KeyField1;
      // if it is a "MIXED" list then the TableName is in the KeyField2
      // property of each individual KeyList entry
      if bMixed then
        sTable := iKeys.GetKeyItem(i).KeyField2;
      Result := Result + InsertIntoTempTable(sKey, sTable, aType);
      Recorder2000.RecorderMainForm.Progress := round(i/iKeys.ItemCount*100);
    end;
  finally
    // make sure we clean up the screen
    Recorder2000.RecorderMainForm.StopProgressBar;
    Recorder2000.RecorderMainForm.StatusText := '';
  end;
end;
  //------------------------------------------------------------------------------
// Count rows in the nbn_exchange_obs
// By the time this is used, we have added the RecordKeys to the table
// and removed invalid ones
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.RowsToExport: Longint;
var oQuery: TADOQuery;
begin
  Result := 0;
  // count the number of rows we will export
  oQuery := TADOQuery.Create(nil);
  try
    oQuery.Connection := NBNData;
    oQuery.SQL.Add('select count(RecordKey) from ##nbn_exchange_obs');
    oQuery.Open;
    if oQuery.RecordCount > 0 then
      Result := oQuery.Fields[0].AsInteger;
  finally
    oQuery.Free;
  end;
end;

//------------------------------------------------------------------------------
// Goes through the measurements that the user selected and calls
// AddMeasurement for each one in turn. Deals with user feedback.
//------------------------------------------------------------------------------
Procedure TNBNExchangeAddinNuImp.DoAbundance;
var i: integer;
    iToDo, iDone: Longint;
begin
  // load the selected measurements into taxon_measurements table
  if FAbundance then
  begin
{$IFDEF DEBUG}
      WriteLog('Adding abundance', True);
{$ENDIF}
      iDone := 0;
      // this is a very rough estimate of how many measurements
      // we might have to deal with - assmes all rows have data
      // so likely to be overestimated
      iToDo := FnRows * (high(FMeasurementTypes)+1);
      Recorder2000.RecorderMainForm.StartProgressBar;
      Recorder2000.RecorderMainForm.StatusText := 'Adding abundance';
      // walk through the measurement types updating the table
      try
        for i := low(FMeasurementTypes) to high(FMeasurementTypes) do
          if FMeasurementTypes[i].Selected then
            AddMeasurement(FMeasurementTypes[i].Key, FMeasurementTypes[i].Name, iDone, iToDo);
      finally
        Recorder2000.RecorderMainForm.StopProgressBar;
    end;
  end;
end;
 //------------------------------------------------------------------------------
// Once we have prepared taxon_obs, this function writes the information to the
// export file using a FileStream.
// Show a progress bar.
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.DoExport(const sFileName: ansistring): boolean;
var oStream: TFileStream;
    iCount, iNumRecs: Longint;
    sLine: ansistring;
    iField: integer;
    oQuery: TADOQuery;
begin

  WriteLog(Format('Exporting to file: %s', [sFileName]), True);
  iCount := 0;
  Result := False;
  oQuery := TADOQuery.Create(nil);

  try
    oQuery.Connection := NBNData;
    oQuery.SQL.Add('select * from ##nbn_exchange_obs');
    oQuery.CommandTimeout := 0;
    oQuery.Open;
    if oQuery.RecordCount > 0 then
    begin

      // jump to the end of the table to ensure we get an accurate count
      oQuery.Last;
      iNumRecs := oQuery.RecordCount;

      // initialise the progress bar
      Recorder2000.RecorderMainForm.StartProgressBar;
      Recorder2000.RecorderMainForm.StatusText := 'Exporting to "' + sFileName + '".';
      // open the output file exclusively for writing only - create it if it doesn't exist
      // Note that the Rec6 Export Dialog deals with the prompting the user to confirm
      // its OK to overwrite if the file does already exist.


      oStream := TFileStream.Create(sFileName, fmCreate or fmOpenWrite or fmShareExclusive);


      Try
          //---------------------------------
          // write the data section
          //---------------------------------
          // The first line is a list of field names

          sLine := '';
          for iField := 0 to oQuery.Fields.Count-1 do
            if WriteField(oQuery, iField) then
              sLine := sLine + oQuery.Fields[iField].DisplayName + Tab;
          // trim trailing tab
          sLine := Copy(sLine, 1, Length(sLine)-1);
          // write it to the output file
          AddStream(oStream, sLine);

          // start at the beginning
          oQuery.First;
          // now write a line for each row of data
          while not oQuery.EOF do
          begin
            sLine := '';
            for iField := 0 to oQuery.Fields.Count-1 do
              if WriteField(oQuery, iField) then
                sLine := sLine + GetField(oQuery, iField) + Tab;
            // trim trailing tab
            sLine := Copy(sLine, 1, Length(sLine)-1);
            // write it to the output file
            AddStream(oStream, sLine);

            // update the progress bar
            inc(iCount);
            Recorder2000.RecorderMainForm.Progress := round(iCount/iNumRecs*100);

            oQuery.Next;
          end;
          Result := True;

      finally
        // clean up status bar
        Recorder2000.RecorderMainForm.StopProgressBar;
        Recorder2000.RecorderMainForm.StatusText := '';
        // close the text file
        oStream.Free;
      end;

    end;
   finally
    oQuery.Close;
   end;

 end;
//------------------------------------------------------------------------------
// Write the XML file describing the dataset and surveys
// This is written to a file with the same path and name as the main
// text file, but with the extension ".xml".
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.WriteHeader(const sFileName: ansistring): boolean;
var oQuery: TADOQuery;
    oStream: TFileStream;
    sName: ansistring;
    i: integer;
begin
{$IFDEF DEBUG}
  WriteLog(Format('Writing XML header file: %s',[sFileName]), True);
{$ENDIF}
  //open the file
  sName := ChangeFileExt(sFileName, '.xml');
  oStream := TFileStream.Create(sName, fmCreate or fmOpenWrite or fmShareExclusive);
  try
    AddStream(oStream, Format('<Metadata version="2.1" datestamp="%s" gatewayId="%s">',
                              [DateToStr(now), GetItem('SETTING', 'DATA', 'SiteID', 'NAME')]));
    AddStream(oStream, Format('  <DatasetTitle>%s</DatasetTitle>',[GetMetadata('dataset_name')]));
    AddStream(oStream, Format('  <DatasetProvider>%s</DatasetProvider>',[GetMetadata('dataset_owner')]));
    AddStream(oStream,        '  <Abstract>');
    AddStream(oStream, Format('      <Description>%s</Description>',[GetMetadata('dataset_title')]));
    AddStream(oStream, Format('      <DatasetConfidence>%s</DatasetConfidence>',[GetMetadata('dataset_validation')]));
    AddStream(oStream,        '  </Abstract>');
    AddStream(oStream, Format('  <UseConstraints>%s</UseConstraints>',[GetMetadata('dataset_restrictions')]));
    AddStream(oStream, Format('  <AccessContstraints>%s</AccessContstraints>',[GetMetadata('dataset_restrictions')]));

    // Surveys section - there should be an entry here for each survey key mentioned in
    // the taxon_obs table
    oQuery := TADOQuery.Create(nil);
    try
      oQuery.Connection := NBNData;
      oQuery.SQL.Add(SURVEY_SQL);
      try
        oQuery.Open;
        if oQuery.RecordCount > 0 then
          While not oQuery.EOF do
          begin
            AddStream(oStream, '  <Survey providerId="' + oQuery.Fields[0].AsString + '">');
            AddStream(oStream, '      <Name>' + oQuery.Fields[1].AsString+ '</Name>');
            AddStream(oStream, '      <Abstract>');
            AddStream(oStream, '          <Description>' + oQuery.Fields[2].AsString+ '</Description>');
            AddStream(oStream, '          <GeographicCoverage>' + oQuery.Fields[3].AsString+ '</GeographicCoverage>');
         //   AddStream(oStream, '          <TemporalCoverage>' +  GetVagueDate(oQuery,4) + ' - ' + GetVagueDate(oQuery,7) + '</TemporalCoverage>');
            AddStream(oStream, '      </Abstract>');
            AddStream(oStream, '  </Survey>');
            oQuery.Next;
          end;
      finally
        oQuery.Close;
      end;
    finally
      oQuery.Free;
    end;

    // Attributes
    //--------------------------------------------------------------------------
    // Sample method
    if FSampleType then
    begin
      AddStream(oStream, '  <OccurrenceAttribute>');
      AddStream(oStream, '      <Name>SampleMethod</Name>');
      AddStream(oStream, '      <Description>Sampling method from which the observation resulted.</Description>');
      AddStream(oStream, '  </OccurrenceAttribute>');
    end;

    // Sample method
    if FSubstrate then
    begin
      AddStream(oStream, '  <OccurrenceAttribute>');
      AddStream(oStream, '      <Name>Substrate</Name>');
      AddStream(oStream, '      <Description>Substrate on which the taxon was observed.</Description>');
      AddStream(oStream, '  </OccurrenceAttribute>');
    end;

    // measurements
    if FAbundance then
      for i := low(FMeasurementTypes) to high(FMeasurementTypes) do
        if FMeasurementTypes[i].Selected then
        begin
          AddStream(oStream, '  <OccurrenceAttribute>');
          AddStream(oStream, '      <Name>' +  FMeasurementTypes[i].Name + '</Name>');
          AddStream(oStream, '      <Description>' +
            GetItem('MEASUREMENT_TYPE', 'DESCRIPTION', FMeasurementTypes[i].Key, 'MEASUREMENT_TYPE_KEY') +
            '</Description>');
          AddStream(oStream, '  </OccurrenceAttribute>');
        end;

    // closing tag
    AddStream(oStream, '</Metadata>');
  finally
    oStream.Free;
  end;

  Result := True;
end;
  //------------------------------------------------------------------------------
// Write the TableName and Key into the nbn_exchange_export table
// Returns the number of rows it INSERTed
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.InsertIntoTempTable(const sKey, sTable: ansistring; const aType: TKeyType): integer;
var sSQL, sToTable: ansistring;
begin
  if (aType = ktExport) or (FExportInvalid)  then
    sToTable := '##nbn_exchange_export'
  else
    sToTable := '##nbn_exchange_invalid';
  sSQL := 'INSERT INTO ' + sToTable + ' (nbn_key, table_name) ' +
          'VALUES (' + Quote + skey + Quote + ',' + Quote + sTable + Quote + ');';
  if ExecuteSQL(sSQL) then
    Result := 1
  else
    Result := 0;
end;
 //------------------------------------------------------------------------------
// Turn abundance entries from TAXON_OCCURRENCE_DATA into a semicolon
// delinated string.
//------------------------------------------------------------------------------
procedure TNBNExchangeAddinNuImp.AddMeasurement(const sTypeKey, sName: ansistring; var done, n: Longint);
var oQuery: TADOQuery;
    sKey, sLastKey, sItem, sList, sAccuracy, sUnit, sQualifier: ansistring;

    //--------------------------------------------------------------------------
    // Local procedure which actually inserts the string into our temp table
    // Increment the progress bar each time we update a row
    //--------------------------------------------------------------------------
    procedure DoUpdateAbundance(const sFieldName: ansistring);
    var sSQL: ansistring;
    begin
      // check we have a key
      if sLastKey <> '' then
      begin
        inc(done);
        // Attributes are not allowed to be over 255 chars
        sList := TruncateString(sList, [';'], 255);
        Recorder2000.RecorderMainForm.Progress := Trunc(done/n*100.0);
        sSQL := 'UPDATE ##nbn_exchange_obs ' +
                'SET [' +  sFieldName + '] = ' + Quote + sList + Quote + ' ' +
                'WHERE (RecordKey=' + Quote + sLastKey + Quote + ');';
        ExecuteSQL(sSQL);
      end;
    end; {DoUpdateAbundance}

begin
  sLastKey := '';
  oQuery := TADOQuery.Create(nil);
  try
    oQuery.Connection := NBNData;
    oQuery.SQL.Add(Format(OCCURRENCE_DATA_SQL, [sTypeKey]));
    oQuery.Open;
    try
      if oQuery.RecordCount > 0 then
      begin
         while not oQuery.EOF do
         begin
           // get key and name
           sKey := oQuery.FieldByName('RecordKey').AsString;
           sItem := oQuery.FieldByName('DATA').AsString;
           sQualifier := oQuery.FieldByName('Qualifier').AsString;
           sUnit := oQuery.FieldByName('Unit').AsString;
           sAccuracy := oQuery.FieldByName('ACCURACY').AsString;
           if (sUnit <> 'Count') then
              sItem := Trim(sItem + ' ' + sUnit);
           if (sQualifier <> 'None') then
              sItem := Trim(sItem + ' ' + sQualifier);
           if (sAccuracy <> '') and not ((sAccuracy = 'Default') or (sAccuracy = 'None')) then
              sItem := sItem + ' (' + Trim(sAccuracy) + ')';

           // check whether we have a new observation key
           if sKey <> sLastKey then
           begin
             // we have a new key, so do the update
             DoUpdateAbundance(sName);
             // reinitialise variables when a new key is found
             sList := sItem;
             sLastKey := sKey;
           end
           // we don't have a new key, so append name to the list
           else sList := sList + '; ' + sItem;

           oQuery.Next;
         end;
         // do the last update
         DoUpdateAbundance(sName);
      end;
    finally
      oQuery.Close;
    end;
  finally
    oQuery.Free;
  end;
end;
 //------------------------------------------------------------------------------
// Decide whether to output a field depending on the options selected
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.WriteField(oQuery: TADOQuery; const iField: integer): boolean;
begin
  Result := True;
  if (not FRecorders) and (oQuery.Fields[iField].DisplayName = 'Recorder') then
    Result := False;
  if (not FDeterminer) and (oQuery.Fields[iField].DisplayName = 'Determiner') then
    Result := False;
  if (not FSampleType) and (oQuery.Fields[iField].DisplayName = 'SampleMethod') then
    Result := False;
  if (not FComment) and (oQuery.Fields[iField].DisplayName = 'Comment') then
    Result := False;
  if (not FSubstrate) and (oQuery.Fields[iField].DisplayName = 'Substrate') then
    Result := False;
end;
//------------------------------------------------------------------------------
// Gets the value of the field as a string
// Make sure Recorder and Determiner fields are not over 140 chars and
// SiteName field over 80 chars
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.GetField(oQuery: TADOQuery; const iField: integer): ansistring;
begin
  // get the field's value as a string
  Result := Trim(oQuery.Fields[iField].AsString);

  // truncate if necessary
  if (oQuery.Fields[iField].DisplayName = 'Recorder')
  or (oQuery.Fields[iField].DisplayName = 'Determiner') then
    Result := TruncateString(Result, [' ','.',',',';'], 140);
  if (oQuery.Fields[iField].DisplayName = 'SiteName') then
    Result := TruncateString(Result, [' ','.',',',';'], 80);
end;

//------------------------------------------------------------------------------
// Truncate a string to iMax at a word boundary defined by character set sDelim
// If the string is shorter than iMax, the string is just returned
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.TruncateString(const aString: ansistring; sDelim: TSysCharSet; const iMax: integer): ansistring;
var work: ansistring;
    p: integer;
begin
  work := aString;
  if Length(work) > iMax then
  begin
    work := WrapText(work, '~', sDelim,  iMax);
    p := Pos('~', work);
    if p>0 then
      work := ansistring(Trim(Copy(work, 1, p-1)));
  end;
  Result := work;
end;
//------------------------------------------------------------------------------
// Get an item of metadata from the SPECIAL_XML_ELEMENT table.
// If the metadata has not been enterred, prompt the user to add it and call
// the Tools - Edit Metadata facility to allow it to be added.
//------------------------------------------------------------------------------
Function TNBNExchangeAddinNuImp.GetMetadata(const sMetadata: ansistring): ansistring;
var sItem: ansistring;
begin
  repeat
    sItem := GetItem('SPECIAL_XML_ELEMENT', 'DATA', sMetadata, 'NAME');
    if sItem = '' then
    begin
      MessageDlg('Metadata describing this dataset is required in order' + #13 +
                 'to submit data to the NBN Gateway. Please complete all' + #13 +
                 'the metadata fields before continuing.', mtWarning, [mbOK], 0);

      // let the user edit metadata
      Recorder2000.MenuOptionClick('Tools;Export Management;Edit Metadata...');

      // try and get it again
      sItem := GetItem('SPECIAL_XML_ELEMENT', 'DATA', sMetadata, 'NAME');
    end;
  until sItem <> '';
  Result := sItem;
end;

 
//------------------------------------------------------------------------------
// Execute the DDL SQL passed as a parameter
// Returns True on success
// If it fails, stores the error message in FError
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.ExecuteSQL(const sSQL: ansistring): boolean;
var oQuery: TADOQuery;
begin
  oQuery := TADOQuery.Create(nil);
  try
    try
      oQuery.Connection := NBNData;
      oQuery.SQL.Clear;
      oQuery.SQL.Add(sSQL);
      oQuery.ExecSQL;
      Result := True;
    except
      on e:Exception do
      begin
        FError := e.Message;
        ShowError('SQL: ' + sSQL, e.Message);
        Result := False;
      end;
    end;
  finally
    oQuery.Free
  end;
end;
//------------------------------------------------------------------------------
// Add a column to table nbn_exchange_obs for each selected measurement
// Returns the number of columns added
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.CreateTaxonMeasurements: integer;
var sSQL, sCol: ansistring;
    i: integer;
begin
  Result := 0;

  sSQL := 'ALTER TABLE [##nbn_exchange_obs] ADD ';

  // add columns for the select measurement types
  for i := low(FMeasurementTypes) to high(FMeasurementTypes) do
    if FMeasurementTypes[i].Selected then
    begin
      sCol := CheckColName(FMeasurementTypes[i].Name);
      // remember the updated name
      FMeasurementTypes[i].Name := sCol;
      sSQL := sSQL + '[' + sCol + '] varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL, ';
      inc(Result);
    end;

  // if we found one or more, run the SQL
  if Result > 0 then
  begin
    // remove trailing comma-space
    sSQL := Copy(sSQL, 1, Length(sSQL)-2);

    if not ExecuteSQL(sSQL) then
      Result := 0;
  end;

  FAbundance := (Result > 0);
end;
//------------------------------------------------------------------------------
// Write a string as a new line in the FileStream.
// Returns the number of bytes added
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.AddStream(fStream: TFileStream; const sLine: ansistring): Longint;
var s: ansistring;
begin
  s := sLine + EoL;
  Result := fStream.Write(s[1], Length(s));
end;
//------------------------------------------------------------------------------
// Get an item from a table and return it as a string
//------------------------------------------------------------------------------
function TNBNExchangeAddinNuImp.GetItem(const sTable, sField, sKey, sKeyField: ansistring): ansistring;
var oQuery: TADOQuery;
    sSQL: ansistring;
begin
    sSQL :=  'SELECT ' + sField + ' FROM ' + sTable +
             ' WHERE ' + sKeyField + '=' + Quote + sKey + Quote;
    oQuery := TADOQuery.Create(nil);
    try
      oQuery.Connection := NBNData;
      oQuery.SQL.Add(sSQL);
      try
        oQuery.Open;
        if oQuery.RecordCount > 0 then
           Result := oQuery.Fields[0].AsString
        else
           Result := '';
      finally
        oQuery.Close;
      end;
    finally
      oQuery.Free;
    end;
end;
//------------------------------------------------------------------------------
// Column names in Access can be up to 64 characters but cannot contain
// ".", "`", "!" or "[]". Check the string that is passed and modify it
// if necessary.
//------------------------------------------------------------------------------
Function TNBNExchangeAddinNuImp.CheckColName(const sName: ansistring): ansistring;
const ILLEGAL = '.!`[]';
var sWork, sTest: ansistring;
    i,j:  integer;
begin
  sWork := sName;
  if sWork <> '' then
  begin
    // replace any illegal characters with underscores
    if ILLEGAL <> '' then
      for i := 1 to Length(ILLEGAL) do
        sWork := AnsiReplaceStr(sWork, ILLEGAL[i], '_');
    // make sure it doesn't exceed 64 characters
    if Length(sWork) > 64 then
      sWork := Trim(Copy(sWork, 1, 64));
    // make sure it isn't a duplicate of one we already have
    j := 2;
    sTest := sWork;
    Repeat
      i := FColNames.IndexOf(sTest);
      if i>=0 then
      begin
        sTest := sWork + IntToStr(j);
        inc(j);
      end;
    Until i<0;
    sWork := sTest;
    FColNames.Add(sWork);
  end;
  Result := sWork;
end;
//------------------------------------------------------------------------------
// Offer the user the choice to get rid of the .txt and .xml file
// so that only the .zip file is kept
//------------------------------------------------------------------------------
procedure TNBNExchangeAddinNuImp.DeleteUnzippedFiles(const sFileName: ansistring);
var sName: ansistring;

begin

   if MessageDlg(Format('%0.0n taxon observations written to file "%s".' + #13 +
                    '%.0n temporary observations not written to file "%s".' + #13 +
                    'Metadata written to file "%s".' + #13 +
                    'These have been zipped together in  file "%s".' + #13 + #13 +
                    'Do you want to keep the .txt and .xml files?' + #13 +
                    '(You only need to submit the .zip file.)',
                    [FnRows*1.0, sFileName, FnExcludedTempRows*1.0,  sFileName,
                    ChangeFileExt(sFileName, '.xml'),
                    ChangeFileExt(sFileName, '.zip')]),
                mtInformation, [mbNo,mbYes], 0) = mrNo then
    begin

    WriteLog('Deleting the original file that have been zipped', True);

    sName := sFileName;
    if FileExists(sName) then
      DeleteFile(PChar(sName));
    sName := ChangeFileExt(sFileName, '.xml');
    if FileExists(sName) then
    DeleteFile(PChar(sName));
 end;
end;

 //------------------------------------------------------------------------------
// Zip the resulting files
// The file name that is passed as a parameter is the main file (.txt)
// We also want to add the .xml file
//------------------------------------------------------------------------------
procedure TNBNExchangeAddinNuImp.DoZip(const sFileName: ansistring);
var oZip: TAbZipper;
    sName: ansistring;
begin
  WriteLog(Format('Zipping to: %s', [sFileName]), True);
  sName := ChangeFileExt(sFileName, '.zip');
  if FileExists(sName) then
    DeleteFile(PChar(sName));
    oZip := TAbZipper.Create(nil);
  try
    oZip.FileName := sName;
    oZip.AutoSave := False;
    oZip.CompressionMethodToUse := smBestMethod;
    oZip.DeflationOption := doMaximum;
    // add the .xml file
    sName := ChangeFileExt(sFileName, '.xml');
    oZip.AddFiles(sName, 0);
    oZip.AddFiles(sFileName, 0);
    // actually do the zip

    oZip.Save;
  finally
    oZip.Free;
  end;
end;



procedure TNBNExchangeAddinNuImp.Install(
  const iInstalledFilePath: WideString);
begin

end;

initialization
  TAutoObjectFactory.Create(ComServer, TNBNExchangeAddinNuImp, Class_NBNExchangeAddinNuImp,
    ciMultiInstance, tmApartment);
end.
