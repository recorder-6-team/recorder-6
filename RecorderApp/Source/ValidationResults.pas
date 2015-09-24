//==============================================================================
//  Unit:        ValidationResults
//
//  Implements:  TdlgValidationResults
//
//  Description:
//
//  Author:      Eric Salmon
//  Created:     14 Jan 2008
//
//  Last Revision Details:
//    $Revision: 8 $
//    $Date: 16/02/09 11:15 $
//    $Author: Andrewkemp $
//
//==============================================================================

unit ValidationResults;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, ApplicationSettings, Constants,
  GeneralFunctions, ImageListButton, DataClasses, OnlineHelp;

resourcestring
  ResStr_FailedDatabaseValidation = 'Failed validation: %d';
  ResStr_ScreenSummary = '%s (Validation errors: %s)';

  ResStr_AdminArea          = 'Administrative Area';
  ResStr_AdminAreas         = 'Administrative Areas';
  ResStr_Biotope            = 'Biotope';
  ResStr_Biotopes           = 'Biotopes';
  ResStr_BiotopeOccurrence  = 'Biotope Occurrence';
  ResStr_BiotopeOccurrences = 'Biotope Occurrences';
  ResStr_Document           = 'Document';
  ResStr_Documents          = 'Documents';
  ResStr_Event              = 'Survey Event';
  ResStr_Events             = 'Survey Events';
  ResStr_Individual         = 'Individual';
  ResStr_Individuals        = 'Individuals';
  ResStr_Location           = 'Location';
  ResStr_Locations          = 'Locations';
  ResStr_LocationFeature    = 'Location Feature';
  ResStr_LocationFeatures   = 'Location Features';
  ResStr_Name               = 'Name';
  ResStr_Names              = 'Names';
  ResStr_Organisation       = 'Organisation';
  ResStr_Organisations      = 'Organisations';
  ResStr_Sample             = 'Sample';
  ResStr_Samples            = 'Samples';
  ResStr_Survey             = 'Survey';
  ResStr_Surveys            = 'Surveys';
  ResStr_Taxa               = 'Taxa';
  ResStr_Taxon              = 'Taxon';
  ResStr_TaxonOccurrence    = 'Taxon Occurrence';
  ResStr_TaxonOccurrences   = 'Taxon Occurrences';


type
  TdlgValidationResults = class(TForm)
    btnSave: TImageListButton;
    btnClose: TImageListButton;
    lblSummary: TLabel;
    Bevel1: TBevel;
    mmDetails: TMemo;
    dlgSaveDetails: TSaveDialog;
    procedure btnSaveClick(Sender: TObject);
  private
    FItems: TKeyList;
    FMessages: TStringList;
    FPartialValidation: Boolean;
    function TableNameToCaption(const tableName: String; plural: Boolean): String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetKeyList(const keyList: TKeyList; validationMessages: TStringList);
    property PartialValidation: Boolean read FPartialValidation write FPartialValidation;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  DefaultPaths;

//==============================================================================
{ TdlgValidationResults }

constructor TdlgValidationResults.Create(AOwner: TComponent);
begin
  inherited;
  Self.HelpContext := IDH_VALIDATIONRESULTS;
end;

{-------------------------------------------------------------------------------
  Parse the keylist for summary box.
}
procedure TdlgValidationResults.SetKeyList(const keyList: TKeyList; validationMessages: TStringList);
var
  i, idx: Integer;
begin
  FItems := keyList;
  FMessages := validationMessages;
  lblSummary.Caption := Format(ResStr_FailedDatabaseValidation, [keyList.Header.ItemCount]);

  if not PartialValidation then
    with TStringList.Create do
      try
        // Count items for each screen
        for i := 0 to FItems.Header.ItemCount - 1 do begin
          idx := IndexOfName(FItems.ItemTable[i]);
          if idx = -1 then
            Add(FItems.ItemTable[i] + '=1')
          else
            ValueFromIndex[idx] := IntToStr(StrToInt(ValueFromIndex[idx]) + 1);
        end;
        // Fill up the summary box
        for i := 0 to Count - 1 do
          mmDetails.Lines.Add(Format(
              ResStr_ScreenSummary,
              [TableNameToCaption(Names[i], True), ValueFromIndex[i]]));
      finally
        Free;
      end
  else
    // List all messages that came up.
    for i := 0 to FItems.Header.ItemCount - 1 do
      mmDetails.Lines.Add(Format(
          '%s: %s'#13#10'%s'#13#10,
          [TableNameToCaption(FItems.ItemTable[i], False),
           FItems.Items[i].KeyField1,
           FMessages[i]]));
end;

{-------------------------------------------------------------------------------
  Returns the name of the form that displays the data from the given table
}
function TdlgValidationResults.TableNameToCaption(const tableName: String; plural: Boolean): String;
begin
  if plural then begin
    if SameText(tableName, TN_SURVEY)             then Result := ResStr_Surveys            else
    if SameText(tableName, TN_SURVEY_EVENT)       then Result := ResStr_Events             else
    if SameText(tableName, TN_SAMPLE)             then Result := ResStr_Samples            else
    if SameText(tableName, TN_TAXON_OCCURRENCE)   then Result := ResStr_TaxonOccurrences   else
    if SameText(tableName, TN_BIOTOPE_OCCURRENCE) then Result := ResStr_BiotopeOccurrences else
    if SameText(tableName, TN_LOCATION)           then Result := ResStr_Locations          else
    if SameText(tableName, TN_LOCATION_FEATURE)   then Result := ResStr_LocationFeatures   else
    if SameText(tableName, TN_NAME)               then Result := ResStr_Names              else
    if SameText(tableName, TN_INDIVIDUAL)         then Result := ResStr_Individuals        else
    if SameText(tableName, TN_ORGANISATION)       then Result := ResStr_Organisations      else
    if SameText(tableName, TN_REFERENCE)          then Result := ResStr_Documents          else
    if SameText(tableName, TN_TAXON_LIST_ITEM)    then Result := ResStr_Taxa               else
    if SameText(tableName, TN_BIOTOPE_LIST_ITEM)  then Result := ResStr_Biotopes           else
    if SameText(tableName, TN_ADMIN_AREA)         then Result := ResStr_AdminAreas
    else Result := ReadableFormat(tableName);
  end else
    if SameText(tableName, TN_SURVEY)             then Result := ResStr_Survey            else
    if SameText(tableName, TN_SURVEY_EVENT)       then Result := ResStr_Event             else
    if SameText(tableName, TN_SAMPLE)             then Result := ResStr_Sample            else
    if SameText(tableName, TN_TAXON_OCCURRENCE)   then Result := ResStr_TaxonOccurrence   else
    if SameText(tableName, TN_BIOTOPE_OCCURRENCE) then Result := ResStr_BiotopeOccurrence else
    if SameText(tableName, TN_LOCATION)           then Result := ResStr_Location          else
    if SameText(tableName, TN_LOCATION_FEATURE)   then Result := ResStr_LocationFeature   else
    if SameText(tableName, TN_NAME)               then Result := ResStr_Name              else
    if SameText(tableName, TN_INDIVIDUAL)         then Result := ResStr_Individual        else
    if SameText(tableName, TN_ORGANISATION)       then Result := ResStr_Organisation      else
    if SameText(tableName, TN_REFERENCE)          then Result := ResStr_Document          else
    if SameText(tableName, TN_TAXON_LIST_ITEM)    then Result := ResStr_Taxon             else
    if SameText(tableName, TN_BIOTOPE_LIST_ITEM)  then Result := ResStr_Biotope           else
    if SameText(tableName, TN_ADMIN_AREA)         then Result := ResStr_AdminArea
    else Result := ReadableFormat(tableName);
end;

{-------------------------------------------------------------------------------
}
procedure TdlgValidationResults.btnSaveClick(Sender: TObject);
var
  i: Integer;
  lScope: String;
begin
  dlgSaveDetails.InitialDir := GetProgramDataFolder(PATH_USER_FILES);
  if dlgSaveDetails.Execute then begin
    if FPartialValidation then
      lScope := 'partial'
    else
      lScope := 'full';
    with TStringList.Create do
      try
        Add('Description: Results of ' + lScope + ' database validation '
            + ' on ' + FormatDateTime('dd/mm/yyyy', Now)
            + ' at ' + FormatDateTime('hh:mm:ss', Now));
        for i := 0 to FItems.Header.ItemCount - 1 do
          Add(Format(
              '%s'#9'%s'#9'%s',
              [FItems.Items[i].KeyField2,
               FItems.Items[i].KeyField1,
               StringReplace(
                   StringReplace(
                       StringReplace(FMessages[i], #13#10, ', ', [rfReplaceAll]),
                       #13, ', ', [rfReplaceAll]),
                   #10, ', ', [rfReplaceAll])]));
        ForceDirectories(ExtractFilePath(dlgSaveDetails.FileName));
        SaveToFile(dlgSaveDetails.FileName);
      finally
        Free;
      end;
  end;
end;

end.
