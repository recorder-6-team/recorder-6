//==============================================================================
//  Unit:        SurveyData
//
//  Implements:  TdmSurvey
//
//  Description: Implements data access functionality for the survey details screen.
//
//  Author:      Paul Thomas
//  Created:     23 April 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 20 $
//    $Date: 11/03/08 11:08 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit SurveyData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, Db, JNCCDatasets, DataClasses, Constants, ADODB, Grids, ExceptionForm;

resourcestring
  ResStr_Tag = 'Tag';

type
  ETagError = class (TExceptionPath);

  TdmSurvey = class(TBaseDataModule)
    qrySurveyType: TJNCCQuery;
    qrySurveyStatus: TJNCCQuery;
    qrySurveyMedia: TJNCCQuery;
    dsSurveyMedia: TDataSource;
    dsSurveyStatus: TDataSource;
    dsSurveyType: TDataSource;
    qrySurvey: TJNCCQuery;
    dsSurvey: TDataSource;
    qrySurveyTags: TJNCCQuery;
    qrySurveyTagsSurvey_Tag_Key: TStringField;
    qrySurveyTagsConcept_Key: TStringField;
    qrySurveyTagsPreferred_Concept_Key: TStringField;
    qrySurveyTagsPlaintext: TWideStringField;
    qrySurveyTagsEntered_By: TStringField;
    qrySurveyTagsCustodian: TStringField;
    qrySurveyLicence: TJNCCQuery;
    dsSurveyLicence: TDataSource;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure DeleteRecord(const AKey: TKeyString);
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData;

//==============================================================================
constructor TdmSurvey.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  qrySurveyType.Open;
  qrySurveyStatus.Open;
  qrySurveyMedia.Open;
  qrySurveyLicence.Open;
end;  // Create

//==============================================================================
destructor TdmSurvey.Destroy;
begin
  qrySurveyType.Close;
  qrySurveyStatus.Close;
  qrySurveyMedia.Close;
  qrySurvey.Close;
  qrySurveyLicence.Close;
 inherited Destroy;
end;  // Destroy

//==============================================================================
// Delete records from detail table SURVEY_SOURCES, and then from the master
// table SURVEY.
procedure TdmSurvey.DeleteRecord(const AKey:TKeyString);
begin
  with dmGeneralData do begin
    DelSources('Survey_Sources','Survey_Key',AKey);

    ExecuteSQL('DELETE FROM Survey WHERE Survey_Key = '''+AKey+'''',
               ResStr_DelFail + ' - SURVEY');
  end;
end;  // DeleteRecord

end.
