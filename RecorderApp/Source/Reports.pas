//==============================================================================
//  Unit:        Reports
//
//  Implements:  TfrmReports
//
//  Description: Predefined reports.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 49 $
//    $Date: 1/05/09 15:30 $
//    $Author: Pauldavies $
//
//  $History: Reports.pas $
//  
//  *****************  Version 49  *****************
//  User: Pauldavies   Date: 1/05/09    Time: 15:30
//  Updated in $/JNCC/Development/Build/Source
//  Incident 19103
//  
//  Now filters places for occurrences and occurrences for places reports
//  using User_Survey_Restriction.
//  
//  *****************  Version 48  *****************
//  User: Rickyshrestha Date: 19/12/07   Time: 10:08
//  Updated in $/JNCC/Development/Build/Source
//  Changed some hardcoded strings to resourcestrings
//  ResStr_NoTaxaRecorded
//    ResStr_NoBiotopeRecorded
//    ResStr_NoRecordedTaxon
//    ResStr_NoRecordedBiotope
//    ResStr_NoLocationToReport
//    ResStr_NoOccurenceToReport
//  
//  *****************  Version 47  *****************
//  User: Ericsalmon   Date: 22/03/07   Time: 16:17
//  Updated in $/JNCC/Development/Build/Source
//  VI 13237. Spatial_Ref value stored in DB in english format. Displayed
//  using current locale.
//  
//  *****************  Version 46  *****************
//  User: Johnvanbreda Date: 11/08/06   Time: 15:48
//  Updated in $/JNCC/Development/Build/Source
//  IR12422
//  Report wizard exceptions fixed.
//  
//  *****************  Version 45  *****************
//  User: Johnvanbreda Date: 30/03/06   Time: 15:41
//  Updated in $/JNCC/Development/Build/Source
//  IR11165
//  Refactoring to fix bug
//  
//  *****************  Version 44  *****************
//  User: Ericsalmon   Date: 26/08/04   Time: 12:01
//  Updated in $/JNCC/Development/Build/Source
//  Forced nil on freed objects, trying to prevent AVs when closing app
//  while forms are still loading.
//  
//  *****************  Version 43  *****************
//  User: Ericsalmon   Date: 28/06/04   Time: 16:29
//  Updated in $/JNCC/Development/Build/Source
//  ID 5484. Fixed missing occurrences on reports.
//  
//  *****************  Version 42  *****************
//  User: Pollyshaw    Date: 19/02/03   Time: 11:03
//  Updated in $/JNCC/Source
//  Renamed DBEdits which have been turned into labels
//  
//  *****************  Version 41  *****************
//  User: Pollyshaw    Date: 19/02/03   Time: 10:38
//  Updated in $/JNCC/Source
//  IR 367: fixed reports
//  
//  *****************  Version 40  *****************
//  User: Pollyshaw    Date: 28/01/03   Time: 11:01
//  Updated in $/JNCC/Source
//  IR 369: fixed report data and layout.
//  
//  *****************  Version 39  *****************
//  User: Ericsalmon   Date: 7/01/03    Time: 11:10
//  Updated in $/JNCC/Source
//  Cleanup. Use Parameters collection of ADO query objects.
//  
//  *****************  Version 38  *****************
//  User: Pollyshaw    Date: 3/01/03    Time: 14:16
//  Updated in $/JNCC/Source
//  Made standard reports work
//  
//  *****************  Version 37  *****************
//  User: Pollyshaw    Date: 3/01/03    Time: 11:00
//  Updated in $/JNCC/Source
//  Report bugs in progress: working on Set AppRole
//  
//  *****************  Version 36  *****************
//  User: Ericsalmon   Date: 26/06/02   Time: 15:37
//  Updated in $/JNCC/Source
//  JNCC 565: Fixed. Problem was in populating a key list using a function
//  not appropriate for the circumstances, field names expected in function
//  differed from field names in query.
//  
//  *****************  Version 35  *****************
//  User: Ericsalmon   Date: 14/06/02   Time: 11:17
//  Updated in $/JNCC/Source
//  Replaced hourglass cursor with the combined arrow/hourglass
//  (crAppStart), as the application is not "frozen" while the reports are
//  generated.
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Reports;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QuickRpt, Qrctrls, ExtCtrls, ReportsData, StdCtrls, ComCtrls, Db, DataClasses,
  QRDesign, csProp, JNCCDatasets, ExceptionForm, GeneralFunctions, SpatialRefFuncs,
  Constants;

type
  EReportError = class(TExceptionPath);

  TfrmReports = class(TForm)
    qrptOccForPlace: TQuickRep;
    qrptPlacesForOcc: TQuickRep;
    qrbDesignations: TQRChildBand;
    qrlblDesignation: TQRLabel;
    qrmDesignations: TQRMemo;
    qrbMeasurements: TQRChildBand;
    qrlblMeasurement: TQRLabel;
    qrmMeasurements: TQRMemo;
    qrbDescription: TQRChildBand;
    qrlblDescription: TQRLabel;
    qrbOtherNames: TQRChildBand;
    qrlblOtherNames: TQRLabel;
    qrmOtherNames: TQRMemo;
    qrbGridRef: TQRChildBand;
    qrlblGridRef: TQRLabel;
    qrbLocationName: TQRBand;
    qrbAdminAreas: TQRChildBand;
    qrlblAdminArea: TQRLabel;
    qrmAdminAreas: TQRMemo;
    qrdbeLocationName: TQRDBText;
    qrreDescription: TQRRichText;
    reBuffer: TRichEdit;
    qrdbeGridRef: TQRDBText;
    qrloopBiotopes: TQRLoopBand;
    QRLabel7: TQRLabel;
    qrloopSpeciesGap: TQRLoopBand;
    qrloopSpecies: TQRLoopBand;
    QRLabel6: TQRLabel;
    qreTaxonScientific: TQRLabel;
    qreTaxonAuthority: TQRLabel;
    qreTaxonCommon: TQRLabel;
    qreTaxonDetDate: TQRLabel;
    qreTaxonDeterminer: TQRLabel;
    qrmTaxonMeasure: TQRMemo;
    qrbOPFooter: TQRBand;
    qrlblAppTitle1: TQRLabel;
    QRSysData1: TQRSysData;
    QRSysData2: TQRSysData;
    qrbOccDetails: TQRBand;
    qrePOItemName: TQRLabel;
    qrePOAuthority: TQRLabel;
    qrePOCommon: TQRLabel;
    qrbTaxonSynonyms: TQRChildBand;
    qrlblSynonyms: TQRLabel;
    qrmSynonyms: TQRMemo;
    qrloopDesignation: TQRLoopBand;
    QRLabel10: TQRLabel;
    qrsdDesignations: TQRSubDetail;
    qreDesType: TQRLabel;
    qreDates: TQRLabel;
    qrreDesDetail: TQRRichText;
    qrloopFacts: TQRLoopBand;
    QRLabel11: TQRLabel;
    qrsdFacts: TQRSubDetail;
    qrreFactDetail: TQRRichText;
    qrloopOccurrences: TQRLoopBand;
    QRLabel12: TQRLabel;
    qreRecordType: TQRLabel;
    qrsdOccurrences: TQRSubDetail;
    qreOccDate: TQRLabel;
    qreOccDet: TQRLabel;
    qrmOccMeasure: TQRMemo;
    qreOccRecType: TQRLabel;
    qreOccLocation: TQRLabel;
    qreOccGridRef: TQRLabel;
    Label1: TLabel;
    Label2: TLabel;
    qrloopBioGap: TQRLoopBand;
    Label3: TLabel;
    qrpOther: TDesignQuickReport;
    qrdOther: TQRepDesigner;
    qrsdTaxonDetails: TQRSubDetail;
    qrsdBioDetails: TQRSubDetail;
    qreBiotopeTerm: TQRLabel;
    qreBioDetDate: TQRLabel;
    qreBioDeterminer: TQRLabel;
    qrmBioMeasure: TQRMemo;
    qrmBioQualifier: TQRMemo;
    qrbPOFooter: TQRBand;
    qrlblAppTitle2: TQRLabel;
    QRSysData3: TQRSysData;
    QRSysData4: TQRSysData;
    GroupFooterBand1: TQRBand;
    qrreOccComment: TQRRichText;
    QRRichText1: TQRRichText;
    qrcbTaxon: TQRChildBand;
    qrreTaxonComment: TQRRichText;
    qrcbBio: TQRChildBand;
    qrreBioComment: TQRRichText;
    procedure qrptOccForPlaceBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure qrbDescriptionBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbLocationNameBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrsdBioDetailsBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrsdTaxonDetailsBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbOccDetailsBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrloopDesignationBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrsdDesignationsBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrloopFactsBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrsdFactsBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrsdOccurrencesBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrloopOccurrencesBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrptPlacesForOccBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure qrbTaxonSynonymsBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
    { Private declarations }
    FdmReports  :TdmReports;
    FTaxonIDs   :string;
    FBiotopeIDs :string;
    FName       :string;
    FListItemKey:string;
    FReportingTaxonRecord:boolean;
    procedure ChangeRTF(iField: TField; AQRRichEdit: TQRRichText);
    function GetKeyListForQuery(AQuery: TJNCCQuery): TKeyList;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure RunOccurrencesForPlaceReport(iKeyList : TKeyList; userID: string;
      iConstraints : TConstraints = [cConfidential, cChecked, cVerified, cZeroAbundance]); overload;
    procedure RunOccurrencesForPlaceReport(iSQL : String;  userID: string;
      iConstraints : TConstraints = [cConfidential, cChecked, cVerified, cZeroAbundance]); overload;
    procedure RunPlacesForOccurrencesReport( iKeyList : TKeyList;  userID: string;
      iConstraints : TConstraints = [cConfidential, cChecked, cVerified, cZeroAbundance] ); overload;
    procedure RunPlacesForOccurrencesReport( iSQL : String;  userID: string;
      iConstraints : TConstraints = [cConfidential, cChecked, cVerified, cZeroAbundance] ); overload;
    procedure RunOtherReport(const TemplateName, iSQL: string);
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FilterResult, ReportPreview, GeneralData, HTML2RTF;

resourcestring
  ResStr_NoTaxaRecorded = 'There are no taxa recorded at this location.';
  ResStr_NoBiotopeRecorded =  'There are no biotopes recorded at this location.';
  ResStr_NoRecordedTaxon =  'There are no recorded occurrences for this taxon.';
  ResStr_NoRecordedBiotope =  'There are no recorded occurrences for this biotope.';
  ResStr_NoLocationToReport = 'No locations have been found to report';
  ResStr_NoOccurenceToReport =  'No occurrences have been found to report';

//==============================================================================
{ TfrmReports }

constructor TfrmReports.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  qrlblAppTitle1.Caption := Application.Title;
  qrlblAppTitle2.Caption:= Application.Title;

  FdmReports:=TdmReports.Create(nil);
  { We need to explicitly define all datasets.  The one you define at design
      time has a habit of getting confused if there are 2 dmReports running
      when 2 previews are instantiated at one time }
  qrptPlacesForOcc.DataSet := FdmReports.qryOccurrences;
  qrsdDesignations.DataSet := FdmReports.qryTxDes;
  qrsdOccurrences.DataSet := FdmReports.qryTxOccs;
  qrptOccForPlace.DataSet := FdmReports.qryLocations;
  qrdbeGridRef.DataSet := FdmReports.qryLocations;
  qrdbeLocationName.DataSet := FdmReports.qryLocations;
  qrsdBioDetails.Dataset := FdmReports.qryBiotopes;
  qrsdTaxonDetails.DataSet := FdmReports.qryTaxa;
  qrreFactDetail.RichEdit.DefaultConverter := THTMLRTFConversion;
end;  // Create

//==============================================================================
destructor TfrmReports.Destroy;
begin
  FdmReports.Free;
  FdmReports := nil;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TfrmReports.ChangeRTF(iField:TField; AQRRichEdit:TQRRichText);
var lStream:TMemoryStream;
begin
  lStream:=TMemoryStream.Create;
  try
    reBuffer.Lines.Assign(iField);
    // Change Arial 8 used on entry forms to Times 10 used on the report.
    reBuffer.SelectAll;
    reBuffer.SelAttributes.Name:=qrptOccForPlace.Font.Name;
    reBuffer.SelAttributes.Size:=qrptOccForPlace.Font.Size;
    // Send the modified text to the quickreport control
    reBuffer.Lines.SaveToStream(lStream);
    lStream.Position:=0;
    AQRRichEdit.Lines.Clear;
    AQRRichEdit.Lines.LoadFromStream(lStream);
  finally
    lStream.Free;
  end;
end;  // ChangeRTF

//==============================================================================
procedure TfrmReports.qrptOccForPlaceBeforePrint(Sender: TCustomQuickRep;
  var PrintReport: Boolean);
begin
  // Before starting the report, make sure it's the first record
  qrptOccForPlace.Dataset.First;
end;  // qrptOccForPlaceBeforePrint

//==============================================================================
procedure TfrmReports.qrbLocationNameBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var lKey, lLine, SampleIDs:string;
    lCursor               :TCursor;
    lHideBand             :boolean;
begin
  lCursor := AppStartCursor;
  try
    // Update text in child bands
    with FdmReports do begin
      lKey:=qrptOccForPlace.Dataset.FieldByName('Location_Key').AsString;
      FdmReports.CurrentLocationItemKeys := '''' + lKey + '''';
      // Other Names (Synonyms)
      with qryOtherNames do begin
        qrmOtherNames.Lines.Clear;
        if Active then Close;
        ParseSQL := false;
        Parameters.ParamByName('Key').Value:=lKey;
        Open;
        while not Eof do begin
          qrmOtherNames.Lines.Add(FieldByName('Item_Name').AsString);
          Next;
        end;
        Close;
      end;  // qryOtherNames
      lHideBand:=qrmOtherNames.Lines.Count=0;
      qrlblOtherNames.Visible:=not lHideBand;
      qrmOtherNames.Visible  :=not lHideBand;
      if lHideBand then qrbOtherNames.Height:=0;

      // Admin areas
      with qryAdminAreas do begin
        qrmAdminAreas.Lines.Clear;
        if Active then Close;
        ParseSQL := false;
        Parameters.ParamByName('Key').Value:=lKey;
        Open;
        while not Eof do begin
          qrmAdminAreas.Lines.Add(FieldByName('Admin_Area_Name').AsString);
          Next;
        end;
        Close;
      end;  // qryAdminAreas
      lHideBand:=qrmAdminAreas.Lines.Count=0;
      qrlblAdminArea.Visible:=not lHideBand;
      qrmAdminAreas.Visible :=not lHideBand;
      if lHideBand then qrbAdminAreas.Height:=0;

      // Designations
      with qryDesignations do begin
        qrmDesignations.Lines.Clear;
        if Active then Close;
        ParseSQL := false;
        Parameters.ParamByName('Key').Value:=lKey;
        Open;
        while not Eof do begin
          lLine:=FieldByName('Short_Name').AsString+' ('+
                 dmGeneralData.GetName(FieldByName('Authority').AsString);
          if not FieldByName('Date_From').IsNull then begin
            lLine:=lLine+' '+FieldByName('Date_From').AsString;
            if not FieldByName('Date_To').IsNull then
            lLine:=lLine+' - '+FieldByName('Date_To').AsString;
          end;
          lLine:=lLine+')';
          qrmDesignations.Lines.Add(lLine);
          Next;
        end;
        Close;
      end;  // qryDesignations
      lHideBand:=qrmDesignations.Lines.Count=0;
      qrlblDesignation.Visible:=not lHideBand;
      qrmDesignations.Visible :=not lHideBand;
      if lHideBand then qrbDesignations.Height:=0;

      // Measurements
      with qryMeasurements do begin
        qrmMeasurements.Lines.Clear;
        if Active then Close;
        ParseSQL := false;
        SQL[1]:='FROM (((Location_Data LD';
        SQL[5]:='WHERE LD.Location_Key = '''+lKey+'''';
        qrmMeasurements.Lines.Clear;
        Open;
        while not Eof do begin
          lLine:=FieldByName('MType').AsString+', '+FieldByName('Qualifier').AsString+', '+
                 FieldByName('Data').AsString+' '+FieldByName('MUnit').AsString;
          if FieldByName('Accuracy').AsString<>'' then
            lLine:=lLine+' ('+FieldByName('Accuracy').AsString+')';
          qrmMeasurements.Lines.Add(lLine);
          Next;
        end;
        Close;
      end;  // qryMeasurements
      lHideBand:=qrmMeasurements.Lines.Count=0;
      qrlblMeasurement.Visible:=not lHideBand;
      qrmMeasurements.Visible :=not lHideBand;
      if lHideBand then qrbMeasurements.Height:=0;

      // Get the sample keys for the current location so we can get the occurrences
      with qryGetSampleKeys do begin
        if Active then Close;
        ParseSQL := false;
        Parameters.ParamByName('Key').Value:=lKey;
        Open;
        SampleIDs:='';
        if Eof then
          SampleIDs:='('''')'
        else begin
          SampleIDs:='('''+FieldByName('Sample_Key').AsString+'''';
          while not Eof do begin
            SampleIDs:=SampleIDs+','''+FieldByName('Sample_Key').AsString+'''';
            Next;
          end;
          SampleIDs:=SampleIDs+')';
        end;
        Close;
      end;  // qryGetSampleKeys
      // Prepare the Biotope and Taxon SQL
      with qryBiotopes do begin
        if Active then Close;
        ParseSQL := false;
        SQL.Clear;
        SQL.Add(ppBtoccsForLocation.Content);
        Open;
        First;
      end;  // qryBiotopes
      with qryTaxa do begin
        if Active then Close;
        ParseSQL := false;
        SQL.Clear;
        SQL.Add(ppTxoccsForLocation.Content);
        Open;
        First;
      end;  // qryTaxa
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // qrbLocationNameBeforePrint

//==============================================================================
procedure TfrmReports.qrbDescriptionBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := 'Select DESCRIPTION from LOCATION WHERE LOCATION_KEY = ' +
             FdmReports.CurrentLocationItemKeys;
    Open;
    try
      ChangeRTF(FieldByName('Description'),qrreDescription);
    finally
      Close;
    end; // try
  end;
  PrintBand:=qrreDescription.Lines.Count>0;
end;  // qrbDescriptionBeforePrint

//==============================================================================
procedure TfrmReports.qrsdBioDetailsBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var lBioTerm :string;
    lTopValue:byte;
    lCursor  :TCursor;
    liLine : String;
begin
  lCursor := AppStartCursor;
  try
    qreBiotopeTerm.Caption   :='';
    qreBiotopeTerm.Font.Style:=[fsUnderline];
    lTopValue:=0;
    if FdmReports.qryBiotopes.Eof then begin
      qrsdBioDetails.Height   :=32;
      qreBiotopeTerm.Font.Style:=[fsItalic];
      qreBiotopeTerm.Caption  :=ResStr_NoBiotopeRecorded;
      qreBioDetDate.Caption   :='';
      qreBioDeterminer.Caption:='';
      qrmBioMeasure.Caption   :='';
      qrmBioQualifier.Caption :='';
      qrreBioComment.Lines.Clear;
    end else
      with FdmReports do begin
        with qryBiotopes do begin
          // Check if we need to show the biotope term
          lBioTerm:=FieldByName('Short_Term').AsString;
          if FieldByName('Original_Code').AsString<>'' then
            lBioTerm:=FieldByName('Original_Code').AsString+', '+lBioTerm;

          if FName<>lBioTerm then begin
            // New or first biotope, so show it
            FName:=lBioTerm;
            qreBiotopeTerm.Caption:=lBioTerm;
            lTopValue:=32;
          end;
          // Set band height and move controls into position
          qrsdBioDetails.Height:=lTopValue+32;
          // Get the lines for the comment, changing the font to match the report font and size
          // Set FieldName to GeneralComment for Occurrence comment and
          // DeterminationComment for Preferred Determination comment. Both fields are
          // available in the query, use at your discretion.
          ChangeRTF(FieldByName('GeneralComment'),qrreBioComment);

          // Move the remaining controls and set their values
          qreBioDetDate.Top       :=lTopValue;
          qreBioDetDate.Caption   :=FieldByName('Vague_Date_Start').Text;
          qreBioDeterminer.Top    :=lTopValue;
          qreBioDeterminer.Caption:=dmGeneralData.GetName(FieldByName('Determiner').AsString);
          qrmBioMeasure.Top       :=lTopValue;
          qrmBioQualifier.Top     :=lTopValue;
        end;  // qryBiotopes
        // Display first measurement found for the current determination
        with qryMeasurements do begin
          if Active then Close;
          SQL[1]:='FROM (((Biotope_Occurrence_Data LD';
          SQL[5]:='WHERE LD.Biotope_Occurrence_Key = '''+qryBiotopes.FieldByName('Biotope_Occurrence_Key').AsString+'''';
          qrmBioMeasure.Lines.Clear;
          qrmBioQualifier.Lines.Clear;
          Open;
          while not Eof do
          begin
            liLine := FieldByName('Data').AsString+' '+FieldByName('MUnit').AsString;
            if FieldByName('Accuracy').AsString<>'' then
              liLine := liLine + ' ('+FieldByName('Accuracy').AsString+')';
            qrmBioMeasure.Lines.Add(liLine);
            qrmBioQualifier.Lines.Add(FieldByName('Qualifier').AsString);
            Next;
          end;
          Close;
        end;  // qryMeasurements
      end;
  finally
    DefaultCursor(lCursor);
  end;                                   
end;  // qrsdBioDetailsBeforePrint

//==============================================================================
procedure TfrmReports.qrsdTaxonDetailsBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var lTaxonName, lAuthority:string;
    lTopValue :byte;
    lCursor   :TCursor;
    ltfGetCommonName:boolean;
    liLine : String;
begin
  lCursor:=AppStartCursor;
  try
    qreTaxonScientific.Font.Name:='Arial';
    qreTaxonScientific.Caption:='';
    qreTaxonAuthority.Caption :='';
    qreTaxonCommon.Caption    :='';
    lTopValue:=0;
    if FdmReports.qryTaxa.Eof then begin
      qrsdTaxonDetails.Height:=32;
      qreTaxonScientific.Font.Name:='Times New Roman';
      qreTaxonScientific.Caption:=ResStr_NoTaxaRecorded;
      qreTaxonDetDate.Caption   :='';
      qreTaxonDeterminer.Caption:='';
      qrmTaxonMeasure.Caption   :='';
      qrreTaxonComment.Lines.Clear;
    end else
      with FdmReports do begin
        with qryTxSci do begin
          if Active then Close;
          ParseSQL := false;
          Parameters.ParamByName('Key').Value:=qryTaxa.FieldByName('Taxon_List_Item_Key').AsString;
          Open;
          lTaxonName:=FieldByName('Item_Name').AsString+FieldByName('Authority').AsString;
        end;  // qryTxSci
        with qryTaxa do begin
          // Check if we need to show the taxon name
          ltfGetCommonName:=false;
          if FName<>lTaxonName then begin
            // New or first taxon, so show it
            ltfGetCommonName:=true;
            FName:=lTaxonName;
            qreTaxonScientific.Caption:=qryTxSci.FieldByName('Item_Name').AsString;
            lAuthority:=qryTxSci.FieldByName('Authority').AsString;
            if lAuthority<>'' then begin
              if lAuthority[1]<>'(' then lAuthority:='('+lAuthority;
              if lAuthority[Length(lAuthority)]<>')' then lAuthority:=lAuthority+')';
            end;
            qreTaxonAuthority.Caption :=lAuthority;
            lTopValue:=32;
          end;
          // Set band height and move controls into position
          qrsdTaxonDetails.Height:=lTopValue+32;
          //qrreTaxonComment.Top   :=lTopValue+16;
          // Get the lines for the comment, changing the font to match the report font and size
          // Set FieldName to GeneralComment for Occurrence comment and
          // DeterminationComment for Preferred Determination comment. Both fields are
          // available in the query, use at your discretion.
          ChangeRTF(FieldByName('GeneralComment'),qrreTaxonComment);
          // If no comment, readjust to hide empty gap
          if qrreTaxonComment.Lines.Count=0 then
            qrsdTaxonDetails.Height:=qrsdTaxonDetails.Height-16;
          // Move the remaining controls and set their values
          qreTaxonDetDate.Top       :=lTopValue;
          qreTaxonDetDate.Caption   :=FieldByName('Vague_Date_Start').Text;
          qreTaxonDeterminer.Top    :=lTopValue;
          qreTaxonDeterminer.Caption:=dmGeneralData.GetName(FieldByName('Determiner').AsString);
          qrmTaxonMeasure.Top       :=lTopValue;
          qrmTaxonMeasure.Caption   :='';
          qreRecordType.Top       :=lTopValue;
          qreRecordType.Caption := FieldByName('Short_Name').AsString;
        end;  // qryTaxa
        // Display first measurement found for the current determination
        with qryMeasurements do begin
          if Active then Close;
          SQL[1]:='FROM (((Taxon_Occurrence_Data LD';
          SQL[5]:='WHERE LD.Taxon_Occurrence_Key = '''+qryTaxa.FieldByName('Taxon_Occurrence_Key').AsString+'''';
          qrmTaxonMeasure.Lines.Clear;
          Open;
          while not Eof do begin
            liLine := FieldByName('Data').AsString+' '+FieldByName('MUnit').AsString;
            if FieldByName('Accuracy').AsString<>'' then
              liLine:=liLine +' ('+FieldByName('Accuracy').AsString+')';
            qrmTaxonMeasure.Lines.Add(liLine);
            Next;
          end;
          Close;
        end;  // qryMeasurments

        // Get the Common name
        if ltfGetCommonName then
          with qryTxCommon do begin
            if Active then Close;
            ParseSQL := false;
            Parameters.ParamByName('Key').Value:=qryTxSci.FieldByName('Taxon_List_Item_Key').AsString;
            Open;
            First;
            if not Eof then qreTaxonCommon.Caption:=FieldByName('Item_Name').AsString;
            Close;
          end;  // qryTxCommon
        qryTxSci.Close;
        qreTaxonAuthority.Left:=qreTaxonScientific.Left+qreTaxonScientific.Width+10;
        qreTaxonCommon.Left   :=qreTaxonAuthority.Left+qreTaxonAuthority.Width+10;
      end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // qrsdTaxonDetailsBeforePrint

//==============================================================================
//==============================================================================
//==============================================================================
procedure TfrmReports.qrbOccDetailsBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var lName  :string;
    lCursor:TCursor;
begin
  FReportingTaxonRecord := FdmReports.qryOccurrences.FieldByName('RecordType').AsString = 'Taxon';
  lCursor:=AppStartCursor;
  try
    qrePOItemName.Caption :='';
    qrePOAuthority.Caption:='';
    qrePOCommon.Caption   :='';
    with FdmReports do begin
      with qryOccurrences do begin
        // Get the new item key, Taxon or Biotope
        FListItemKey:=FieldByName('ListItemKey').AsString;
        // Show and do something slightly different for Taxon/Biotope Occurrences
        if FReportingTaxonRecord then begin
          qrePOItemName.Font.Style:=[fsBold,fsItalic];
          qrePOItemName.Caption:=FieldByName('ItemName').AsString;
          lName:=FieldByName('Authority').AsString;
          if lName<>'' then begin
            if lName[1]<>'(' then lName:='('+lName;
            if lName[Length(lName)]<>')' then lName:=lName+')';
          end;
          qrePOAuthority.Caption:=lName;
          qrePOAuthority.Left:=qrePOItemName.Left+qrePOItemName.Width+10;
          if qryTxCommon.Active then qryTxCommon.Close;
          //qryTxCommon.ParseSQL := false;
          qryTxCommon.Parameters.ParamByName('Key').Value:=FListItemKey;
          qryTxCommon.Open;
          qrePOCommon.Caption:='';
          if not qryTxCommon.Eof then
            qrePOCommon.Caption:=qryTxCommon.FieldByName('Item_Name').AsString;
          qryTxCommon.Close;
          // Skip duplicates
          lName:=FieldByName('ItemName').AsString;
          Next;
          while (FieldByName('ItemName').AsString=lName) and not Eof do
            Next;
          // Make sure no records are skipped
          if not Eof then Prior;
          FdmReports.CurrentTaxonItemKeys := '''' + FListItemKey + '''';
        end else begin
          qrePOAuthority.Visible  :=false;
          qrePOCommon.Visible     :=false;
          qrePOItemName.Font.Style:=[fsBold];
          qrePOItemName.Caption:=FieldByName('ItemName').AsString;
          // Skip all duplicates
          Next;
          while (FieldByName('ListItemKey').AsString=FListItemKey) and not Eof do
            Next;
          // Make sure no records are skipped
          if not Eof then Prior;
          FdmReports.CurrentBiotopeItemKeys := '''' + FListItemKey + '''';
        end;
      end;  // qryOccurrences
      if FReportingTaxonRecord then begin
        // Get the synonyms
        with qryTaxonSynonyms do begin
          qrmSynonyms.Lines.Clear;
          if Active then Close;
          SQL[SQL.Count-1]:='('+ FdmReports.CurrentTaxonItemKeys + ')';
          Open;
          while not Eof do begin
            lName:=FieldByName('Authority').AsString;
            if lName<>'' then begin
              if lName[1]<>'(' then lName:='('+lName;
              if lName[Length(lName)]<>')' then lName:=lName+')';
            end;
            qrmSynonyms.Lines.Add(FieldByName('Item_Name').AsString+' '+lName);
            Next;
          end;
          Close;
        end;  // qrySynonyms
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // qrbOccDetailsBeforePrint

//==============================================================================
procedure TfrmReports.qrbTaxonSynonymsBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  PrintBand:=FReportingTaxonRecord and (qrmSynonyms.Lines.Count>0);
end;  // qrbTaxonSynonymsBeforePrint

//==============================================================================
procedure TfrmReports.qrloopDesignationBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var lCursor:TCursor;
begin
  if FReportingTaxonRecord then begin
    lCursor:=AppStartCursor;
    try
      with FdmReports do begin
        // Get the designations
        with qryTxDes do begin
          if Active then Close;
          SQL[SQL.Count-1]:='(' + FdmReports.CurrentTaxonItemKeys+'))';
          Open;
          PrintBand:=not Eof;
        end;
      end;
    finally
      DefaultCursor(lCursor);
    end;
  end else
    PrintBand:=false;
end;  // qrloopDesignationBeforePrint

//==============================================================================
procedure TfrmReports.qrsdDesignationsBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var lDateFrom, lDateTo:string;
    lCursor           :TCursor;
begin
  if FReportingTaxonRecord then begin
    lCursor:=AppStartCursor;
    try
      with FdmReports.qryTxDes do begin
        PrintBand:=not Eof;
        if not Eof then
          ChangeRTF(FieldByName('Details'),qrreDesDetail);
          lDateFrom:=FieldByName('Date_From').AsString;
          lDateTo  :=FieldByName('Date_To').AsString;
          qreDates.Caption:='';
          if lDateTo<>'' then qreDates.Caption:=lDateFrom+' - '+lDateTo;
          qreDesType.Caption := FieldByName('Short_Name').AsString;
        end;
    finally
      DefaultCursor(lCursor);
    end;
  end else
    PrintBand:=false;
end;  // qrsdDesignationsBeforePrint

//==============================================================================
procedure TfrmReports.qrloopFactsBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var lCursor:TCursor;
begin
  lCursor:=AppStartCursor;
  try
    if FReportingTaxonRecord then begin
      with FdmReports.qryTxFacts do begin
        if Active then Close;
        SQL[SQL.Count-1]:='(' + FdmReports.CurrentTaxonItemKeys+'))';
        Open;
        PrintBand:=not Eof;
      end;
    end else begin
      with FdmReports.qryBioFacts do begin
        if Active then Close;
        ParseSQL := false;
        Parameters.ParamByName('Key').Value:=FListItemKey;
        Open;
        PrintBand:=not Eof;
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // qrloopFactsBeforePrint

//==============================================================================
procedure TfrmReports.qrsdFactsBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var lqryFacts:TJnccQuery;
begin
  if FReportingTaxonRecord then lqryFacts:=FdmReports.qryTxFacts
                  else lqryFacts:=FdmReports.qryBioFacts;
  with lqryFacts do begin
    PrintBand:=not Eof;
    if not Eof then begin
      qrreFactDetail.Lines.Assign(FieldByName('Data'));
    end;
  end;
end;  // qrsdFactsBeforePrint

//==============================================================================
{ Loop through locations that a taxa is found at }
procedure TfrmReports.qrloopOccurrencesBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var lCursor :TCursor;
begin
  lCursor:=AppStartCursor;
  try
    FName:='';
    if FReportingTaxonRecord then begin
      qrsdOccurrences.DataSet:=FdmReports.qryTxOccs;
      with FdmReports.qryTxOccs do begin
        if Active then Close;
        ParseSQL := false;
        SQL.Clear;
        SQL.Add(FdmReports.ppLocationsForTaxon.Content);
        Open;
        First;
      end;
    end else begin
      qrsdOccurrences.DataSet:=FdmReports.qryBioOccs;
      with FdmReports.qryBioOccs do begin
        if Active then Close;
        ParseSQL := false;
        SQL.Clear;
        SQL.Add(FdmReports.ppLocationsForBiotope.Content);
        Open;
        First;
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // qrloopOccurrencesBeforePrint

//==============================================================================
procedure TfrmReports.qrsdOccurrencesBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var lLocation, lGridRef:string;
    lTopValue :byte;
    lTmpKey   :TKeyString;
    lCursor   :TCursor;
    lqryOccs  :TJnccQuery;
    liLine : String;
begin
  lCursor:=AppStartCursor;
  try
    qreOccLocation.Caption:='';
    qreOccGridRef.Caption :='';
    lTopValue:=0;
    with FdmReports do begin
      if FReportingTaxonRecord then
        lqryOccs:=qryTxOccs
      else begin
        lqryOccs:=qryBioOccs;
        qreOccRecType.Visible:=false;
      end;
      qreOccLocation.Font.Style:=[fsBold];

      if lqryOccs.Eof then begin
        qrsdOccurrences.Height   :=32;
        qreOccLocation.Font.Style:=[fsItalic];
        if FReportingTaxonRecord then
          qreOccLocation.Caption := ResStr_NoRecordedTaxon
        else
          qreOccLocation.Caption := ResStr_NoRecordedBiotope;
        qreOccGridRef.Caption    :='';
        qreOccDate.Caption       :='';
        qreOccDet.Caption        :='';
        qrmOccMeasure.Caption    :='';
        qrreOccComment.Lines.Clear;
      end else begin
        with lqryOccs do begin
          lTmpKey:=FieldByName('Location_Key').AsString;
          lLocation:=dmGeneralData.GetLocationName(lTmpKey);
          lGridRef := LocaliseSpatialRef(FieldByName('Spatial_Ref').AsString);
          // Check if we need to show the taxon name
          if FName<>lLocation+lGridRef then begin
            // New or first taxon, so show it
            FName:=lLocation+lGridRef;
            qreOccLocation.Caption:=lLocation;
            qreOccGridRef.Caption :=lGridRef;
            // Properly align GridRef if no location from Location table.
            if lLocation = '' then 
              qreOccGridRef.Left := qreOccLocation.Left
            else 
              qreOccGridRef.Left := qreOccLocation.Left + qreOccLocation.Width + 10;
            lTopValue:=28;
          end;
          // Set band height and move controls into position
          qrsdOccurrences.Height:=lTopValue+32;
          //qrreOccComment.Top    :=lTopValue+16;
          // Get the lines for the comment, changing the font to match the report font and size
          // Set FieldName to GeneralComment for Occurrence comment and
          // DeterminationComment for Preferred Determination comment. Both fields are
          // available in the query, use at your discretion.
          ChangeRTF(FieldByName('GeneralComment'),qrreOccComment);
          // If no comment, readjust to hide empty gap
          if qrreOccComment.Lines.Count=0 then
            qrsdOccurrences.Height:=qrsdOccurrences.Height-16;
          // Move the remaining controls and set their values
          qreOccDate.Top       :=lTopValue;
          qreOccDate.Caption   :=FieldByName('Vague_Date_Start').Text;
          qreOccDet.Top        :=lTopValue;
          qreOccDet.Caption    :=dmGeneralData.GetName(FieldByName('Determiner').AsString);
          qrmOccMeasure.Top    :=lTopValue;
          qrmOccMeasure.Caption:='';
          qreOccRecType.Top  :=lTopValue;
          if lqryOccs = qryTxOccs then
            qreOccRecType.Caption := FieldByName('Short_Name').Text;
        end;  // qryOccs

        // Display first measurement found for the current determination
        with qryMeasurements do begin
          if Active then Close;
          if FReportingTaxonRecord then begin
            SQL[1]:='FROM (((Taxon_Occurrence_Data LD';
            SQL[5]:='WHERE LD.Taxon_Occurrence_Key = '''+qryTxOccs.FieldByName('Taxon_Occurrence_Key').AsString+'''';
          end else begin
            SQL[1]:='FROM (((Biotope_Occurrence_Data LD';
            SQL[5]:='WHERE LD.Biotope_Occurrence_Key = '''+qryBioOccs.FieldByName('Biotope_Occurrence_Key').AsString+'''';
          end;
          qrmOccMeasure.Lines.Clear;
          Open;
          while not Eof do begin
            liLine := FieldByName('Data').AsString+' '+FieldByName('MUnit').AsString;
            if FieldByName('Accuracy').AsString<>'' then
              liLine:=liLine +' ('+FieldByName('Accuracy').AsString+')';
            qrmOccMeasure.Lines.Add(liLine);
            Next;
          end;
          Close;
        end;  // qryMeasurments
        qrreOccComment.Top := Max(qrreOccComment.Top, qrmOccMeasure.Top + qrmOccMeasure.Height + 2);
      end;
    end;
  finally
    DefaultCursor(lCursor);
  end;
end;  // qrsdOccurrencesBeforePrint

//==============================================================================
procedure TfrmReports.qrptPlacesForOccBeforePrint(Sender: TCustomQuickRep;
  var PrintReport: Boolean);
begin
  qrptPlacesForOcc.Dataset.First;
end;  // qrptPlacesForOccBeforePrint

//==============================================================================
{ CCN 69 - allow the user to call up a location report directly from a location
     in the hierarchy }
procedure TfrmReports.RunOccurrencesForPlaceReport(iKeyList : TKeyList; userID: string;
  iConstraints : TConstraints = [cConfidential, cChecked, cVerified, cZeroAbundance]);
var
  lCursor : TCursor;
begin
  lCursor := AppStartCursor;
  try
    FdmReports.AssignKeyList(iKeyList);
    { Apply maximum constraints for default report }
    FdmReports.Constraints := iConstraints;
    with FdmReports.qryLocations do begin
      if Active then Close;
      ParseSQL := false;
      SQL.Clear;
      SQL.Add(StringReplace(FdmReports.ppLocations.Content,
                            '@UserID',
                            QuotedStr(userID),
                            [rfIgnoreCase, rfReplaceAll]));
      Open;
      First;
    end;
  finally
    DefaultCursor(lCursor);
  end;
  { If nothing found, don't generate report }
  if FdmReports.qryLocations.RecordCount > 0 then
    qrptOccForPlace.Preview
  else
    raise EReportError.CreateNonCritical(ResStr_NoLocationToReport);
end;  // RunOccurrencesForPlaceReport

//==============================================================================
{ CCN 69 - allow the Occurrences for a Place report to be accessible FOR TAXA
     Takes a Key, plus the table name the key is for.
     Identifies the list of occurrences according to the table }
procedure TfrmReports.RunPlacesForOccurrencesReport(iKeyList : TKeyList; userID: string;
  iConstraints : TConstraints = [cConfidential, cChecked, cVerified, cZeroAbundance]);
var
  lCursor : TCursor;
begin
  lCursor := AppStartCursor;
  try
    FTAXONIDS := '';
    FBIOTOPEIDS := '';
    FdmReports.AssignKeyList(iKeyList);
    { Apply maximum constraints for default report }
    FdmReports.Constraints := iConstraints;
    with FdmReports.qryOccurrences do begin
      if Active then Close;
      ParseSQL := false;
      SQL.Clear;
      SQL.Add(StringReplace(FdmReports.ppListItems.Content,
                            '@UserID',
                            QuotedStr(userID),
                            [rfIgnoreCase, rfReplaceAll]));
      Open;
      First;
    end;
  finally
    DefaultCursor(lCursor);
  end;
  { If nothing found, don't generate report }
  if FdmReports.qryOccurrences.RecordCount>0 then
    qrptPlacesForOcc.Preview
  else
    raise EReportError.CreateNonCritical(ResStr_NoOccurenceToReport);
end;  // RunPlacesForOccurrencesReport

//==============================================================================
{ Overloaded version of method, allows SQL to be specified to identify keylist
    to include }
procedure TfrmReports.RunOccurrencesForPlaceReport(iSQL: String; userID: string;
  iConstraints: TConstraints = [cConfidential, cChecked, cVerified, cZeroAbundance]);
var lKeyList: TKeyList;
    lCursor : TCursor;
begin
  dmGeneralData.qryAllPurpose.SQL.Text := iSQL;
  lKeyList := nil;
  try
    lCursor := HourglassCursor;
    try
      lKeyList := GetKeyListForQuery(dmGeneralData.qryAllPurpose);
    finally
      DefaultCursor(lCursor);
    end;
    RunOccurrencesForPlaceReport(lKeyList, userID, iConstraints);
  finally
    lKeyList.Free;
  end;
end;  // RunOccurrencesForPlaceReport

//==============================================================================
{ Overloaded version of method, allows SQL to be specified to identify keylist
    to include }
procedure TfrmReports.RunPlacesForOccurrencesReport(iSQL: String; userID: string;
  iConstraints: TConstraints = [cConfidential, cChecked, cVerified, cZeroAbundance]);
var lKeyList: TKeyList;
    lCursor : TCursor;
begin
  dmGeneralData.qryAllPurpose.SQL.Text := iSQL;
  lKeyList := nil;
  try
    lCursor := HourglassCursor;
    try
      lKeyList := GetKeyListForQuery(dmGeneralData.qryAllPurpose);
    finally
      DefaultCursor(lCursor);
    end;
    RunPlacesForOccurrencesReport(lKeyList, userID, iConstraints);
  finally
    lKeyList.Free;    
  end;
end;  // RunPlacesForOccurrencesReport

//==============================================================================
procedure TfrmReports.RunOtherReport(const TemplateName, iSQL: string);
var lIdx:integer;
begin
  if FileExists(TemplateName) then begin
    qrdOther.LoadReport(TemplateName);
    FdmReports.qryResults.SQL.Text:=iSQL;
    FdmReports.qryResults.Open;
    // Use a very clever function from csProp unit to set the Dataset to the
    // proper query, not the dmWizard.qryResults, as it was when the report
    // was saved. And it works!!!!!
    for lIdx:=0 to ComponentCount-1 do
      SetPointerProperty(Components[lIdx],'DataSet',FdmReports.qryResults);

    qrdOther.QReport.Dataset:=FdmReports.qryResults;
    qrdOther.QReport.Preview;
  end;
end;  // RunOtherReport

//==============================================================================
function TfrmReports.GetKeyListForQuery(AQuery: TJNCCQuery): TKeyList;
var lNewKeyList: TEditableKeyList;
    ltfWasOpen : Boolean;
begin
  //Return an editable key list with the selected nodes key
  lNewKeyList:= TEditableKeyList.Create;
  lNewKeyList.SetTable( MIXED_DATA);
  // Construct a Key List from the result set
  with AQuery do begin
    ltfWasOpen := Active;
    if not Active then Open;
    // for each result set row
    First;
    while not Eof do begin
      // put the data into the Key List
      lNewKeyList.AddItem(FieldByName('Occurrence Key').AsString, FieldByName('Table Name').AsString);
      Next;
    end;
    if not ltfWasOpen then Close;
  end;
  Result:= lNewKeyList;
end;  // GetKeyListForQuery

//==============================================================================
end.

