//==============================================================================
//  Unit:        ObservationData
//
//  Implements:  TdmObservations
//
//  Description: Implements data access functionality for the observations screen.
//
//  Author:      Paul Thomas
//  Created:     15 April 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 35 $
//    $Date: 20/07/09 11:30 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit ObservationData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, Db, JNCCDatasets, Constants, ApplicationSettings, ADODB;

const
  //Some queries have rows designed to be replaced with extra where clauses.
  //These constants say which rows they are.
  QRYEVENT_REPLACE_ROW = 5;
  QRYSAMPLE_REPLACE_ROW = 6;
  QRYTAXOCC_REPLACE_ROW = 21;
  QRYBIOOCC_REPLACE_ROW = 12;

type
  TdmObservation = class(TBaseDataModule)
    qrySurvey: TJNCCQuery;
    qryEvent: TJNCCQuery;
    qrySample: TJNCCQuery;
    qryTaxonOcc: TJNCCQuery;
    qryBiotopeOcc: TJNCCQuery;
    qrySampleState: TJNCCQuery;
    qrySurveyByKey: TJNCCQuery;
    qryMultiPurpose: TJNCCQuery;
  private
    FEventSurvey: string;
    FSampleEvent: string;
    FOccurrenceSample: string;
    procedure SetEventSurvey(const Value: string);
    procedure SetSampleEvent(const Value: string);
    procedure SetOccurrenceSample(const Value: string);
  public
    constructor Create(AOwner: TComponent; const ASurveyField, AnEventField,
      ASampleField, ATaxonOccField, ABiotopeOccField: string); reintroduce;
    property EventSurvey: string read FEventSurvey write SetEventSurvey;
    property SampleEvent: string read FSampleEvent write SetSampleEvent;
    property OccurrenceSample: string read FOccurrenceSample write SetOccurrenceSample;
  end;

//==============================================================================
implementation

{$R *.DFM}

//==============================================================================
constructor TdmObservation.Create(AOwner: TComponent; const ASurveyField,
  AnEventField, ASampleField, ATaxonOccField, ABiotopeOccField: string);
var
  lFilter: String;
begin
  inherited Create(AOwner);
  // Add extra filter to show/hide confidential records depending on User Access Level
  // Do it here so it's done once when the DataModule is created
  with AppSettings do begin
    // Use the login's user access level, not the current applied user access level
    // to determine if a confidential occurrence is visible. See VI14738.
    if not ConfidentialAccessGranted(LoginUserAccessLevel) then
    begin
      if LoginUserAccessLevel = ualAddOnly then
        lFilter := 'AND (TXO.Confidential = 0 OR TXO.Entered_By = ''' + UserID + ''')'
      else
        lFilter := 'AND TXO.Confidential = 0';

      with qryTaxonOcc do SQL[SQL.Count - 3] := SQL[SQL.Count - 3] + lFilter;
    end;

    qrySurvey.SQL[17] := Format('AND USR.Name_Key = ''%s'' ', [UserID]);
  end;

  qrySurvey.SQL[qrySurvey.SQL.Count - 1] := 'ORDER BY ' + ASurveyField;
  qryEvent.SQL[qryEvent.SQL.Count - 1] := 'ORDER BY ' + AnEventField;
  qrySample.SQL[qrySample.SQL.Count - 1] := 'ORDER BY ' + ASampleField;
  qryTaxonOcc.SQL[qryTaxonOcc.SQL.Count - 1] := 'ORDER BY ' + ATaxonOccField;
  qryBiotopeOcc.SQL[qryBiotopeOcc.SQL.Count - 1] := 'ORDER BY ' + ABiotopeOccField;
end;  // Create

//==============================================================================
procedure TdmObservation.SetEventSurvey(const Value: string);
begin
  FEventSurvey := Value;
  qryEvent.SQL[3] := 'WHERE E.Survey_Key = '''+Value+'''';
end;

//==============================================================================
procedure TdmObservation.SetOccurrenceSample(const Value: string);
begin
  FOccurrenceSample := Value;
  qryTaxonOcc.SQL[18] := 'WHERE TXO.Sample_Key = '''+Value+'''';
  qryBiotopeOcc.SQL[10] := 'WHERE BO.Sample_Key = '''+Value+'''';
end;

//==============================================================================
procedure TdmObservation.SetSampleEvent(const Value: string);
begin
  FSampleEvent := Value;
  qrySample.SQL[5] := Format(
      'WHERE S.Survey_Event_Key = ''%s'' AND (LN.Preferred = 1 OR LN.Preferred IS NULL)',
      [Value]);
end;

end.
