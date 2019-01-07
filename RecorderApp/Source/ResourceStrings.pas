{===============================================================================
  Unit:        ResourceStrings.pas

  Description: This unit contains the resource strings used by the app.

  Created:     June 2004

  Last revision information:
    $Revision: 18 $
    $Date: 9/04/09 12:14 $
    $Author: Simonwood $
===============================================================================}

unit ResourceStrings;

interface

resourcestring
  ResStr_AnEvent = 'an Event';
  ResStr_AnOccurrence = 'an Occurrence';
  ResStr_ASample = 'a Sample';
  ResStr_BiotopeOccurrence = 'Biotope Occurrence';
  ResStr_CannotDeleteXAsContainsY = 'This %s cannot be deleted as it still contains %s.';
  ResStr_CSVFieldNotFound = 'The column ''%s'' was specified in the custom report CSVTable definition, '+
      'but could not be found in the corresponding CSV file.';
  ResStr_ColumnNotFound = 'The requested column %s was not found.';
  ResStr_DisplayDataTypeInvalid = 'The data type %s requested for data display by the addin is not recognised.';
  ResStr_DupParamTypeProblem = 'All conditions with name "%s" must have the same type attribute.';
  ResStr_Event = 'Event';
  ResStr_Events = 'Events';
  ResStr_EventCannotMove = 'Survey Event cannot be moved.  ';
  ResStr_TemporaryCannotMoveTo = 'Samples and Event cannot be moved to a temporary survey';
  ResStr_TemporaryCannotMoveFrom = 'Samples and Events cannot be moved from a temporary survey';
  ResStr_EventDateNotInSurvey = 'The Date for the survey event lies outside ' +
                                 'the recording dates of the survey.';
  ResStr_FileDoesNotExist = 'The file ''%s'' does not exist';
  ResStr_InvalidCSVDateType = '''%s'' is an invalid CSVDataType';
  ResStr_InvalidDateType = '''%s'' is an invalid DataType';
  ResStr_InvalidKeyType = '''%s'' is an invalid KeyType';
  ResStr_InvalidLikeType = 'Operator "like" is only available for datatype "text".';
  ResStr_InvalidOperator = '''%s'' is an invalid Operator';
  ResStr_InvalidTextControl = 'Text control is not TEdit, TRestrictedEdit or TLinkedEdit';
  ResStr_InvalidVagueDate = '''%s'' is an invalid Vague Date';
  ResStr_InvalidDate = '''%s'' is not recognised as a valid date';
  ResStr_UnhandledErrorOccur ='An unhandled error has occurred in %s.  Don''t ' +
                     'worry.  Send this error message to recorder@jncc.gov.uk.';
  ResStr_Item = 'item';
  ResStr_ItemCannotMove = 'Item cannot be moved.';
  ResStr_ItsDeletedFromDB = 'It has been deleted from the database.';
  ResStr_KeyTypeError = 'Error seeing available key types. It is likely this error is '+
      'because ReadXML hasn''t been called yet in the TFrmMain.CreateReport method, '+
      'or the XML file has no Where clauses and is therefore invalid';
  ResStr_NotAllowedToDeleteOtherPersonsData = 'You are not allowed to delete this record '+
      'because it was entered by a different user.';
  ResStr_NotAllowedToChangeOtherPersonsData = 'You are not allowed to change this record '+
      'because it was entered by a different user.';
  ResStr_Occurrence = 'Occurrence';
  ResStr_Occurrences = 'Occurrences';
  ResStr_Observations = 'Observations';
  ResStr_ParameterNotSupported = 'The ''%s'' parameter is not supported for a %s';
  ResStr_ParameterMissing = 'The ''%s'' parameter is missing for a %s';
  ResStr_ParameterRequired = 'The ''%s'' parameter requires a value';
  ResStr_ParseFailure = 'The XML Custom Report "%s" could not be parsed.  Error details:'#13#10'%s';
  ResStr_PermanentDeletionConfirm = 'You are about to PERMANENTLY delete the %s'#13#10 +
                  'Are you sure you want to continue?';
  ResStr_ProblemParsingFile = 'There is a problem parsing the XML file';
  ResStr_ProblemParsingSection = 'There is a problem parsing the %s section of the XML file';
  ResStr_RecorderNotInEvent = 'One or more recorders for the sample are not ' +
                              'recorders for the survey event.';
  ResStr_Sample = 'Sample';
  ResStr_SampleCannotMove = 'Sample cannot be moved.  ';
  ResStr_RecorderNotInSample = 'Recorders not in new sample. Do you wish to add them';
  ResStr_Samples = 'Samples';
  ResStr_SQLError = 'There is an error in the SQL code. The error message is: ''%s''';
  ResStr_SQLErrorDetail = 'There is an error in the SQL code. The error message is: ''%s'''#13#10'and the SQL is: ''%s''';
  ResStr_Survey = 'Survey';
  ResStr_TaxonOccurrence = 'Taxon Occurrence';
  ResStr_TemporaryTablePrefixError = 'The temporary table name ''%s'' should be prefixed with a ''#''';
  ResStr_UnableToBuildWhereClause = 'Unable to build Where clause';
  ResStr_UnableToDeleteReferential = 'Unable to delete this item as it is referenced by other records in the database';
  ResStr_UnsuitableValue = 'An unsuitable %s value has been used';
  ResStr_VagueDateOperators = 'Vague dates can only be compared using the Equal / Not Equal operators';
  ResStr_XMLReportDefProblem = 'The XML Custom Report definition is incorrect.'#13#10;
  ResStr_XMLReportEntryCountMustUseEqual = 'You must use the Equal operator if the entrycount '+
      'is specified for condition %s.';
  ResStr_XMLReportDataTypeMustUseEqual = 'You must use the Equal or NotEqual operator for '+
      'the datatype specified for condition %s.';
  ResStr_XMLReportParamInvalidTypeMultipleEntry = 'You cannot specify an entrycount for conditions ' +
      'which have a data type of CSV or date.  Condition %s is therefore invalid.';
  ResStr_XMLReportParamInvalidUseRucksack = 'You can only specify userucksack for parameters of type Taxon, Location, Individual, Organisation, or Name, and when entrycount = -1.' +
      #10#13'Condition "%s" is therefore invalid.';
  ResStr_Date = 'Date';
  ResStr_Parameter_Entry_Screen_Caption = 'Parameter Entry';
  ResStr_Future_VagueDate = 'The vague date %s is not valid because it is in the future.';
  ResStr_MadExceptErrorMessage = 'An unhandled error has occurred in the application.'#10#13 +
      'This error message has been saved to ''%s''';

implementation


end.
