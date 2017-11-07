{===============================================================================
  Unit:        AddinSearchManager

  Defines:     TAddinSearchManager

  Description: Class for managing searches in the Cany addin

  Model:       Addins.mpb

  Last revision information:
    $Revision: 9 $
    $Date: 21/07/09 10:01 $
    $Author: Ericsalmon $

===============================================================================}
unit AddinSearchManager;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  AddinResourceStrings, AddinFind, ADODb, GeneralFunctions, AddinGeneralData,
  Variants;

const
  // Search type constants.  Addins are able to implement their own version of
  // AddinSearchManager and add extra constansts
  ST_INDIVIDUAL         = 0;
  ST_ORGANISATION       = 1;
  ST_SURVEY             = 2;
  ST_LOCATION           = 3;
  ST_BIOTOPE            = 4;
  ST_SPECIES            = 5;
  ST_SPECIESINLIST      = 6;
  ST_NAME               = 7;
  ST_TERM               = 8;
  ST_TERMINCONCEPTGROUP = 9;
  ST_TERMINSUBJECTAREA  = 10;

type
  TSearchType = Integer;

  TAddinSearchManager = class(TObject)
  private
    FAdditionalCaption: String;
    FResultText: String;
    FSearchKey: String;
    FSearchType: TSearchType;
    function GetRecordset(const ASearchTerm: String): _Recordset;
    function HaveOneExactMatch(ARecordset: _Recordset; const ASearchTerm: String;
        WantExact: Boolean): Boolean;
    procedure SetAdditionalCaption(const Value: String);
    procedure SetSearchKey(const Value: String);
    procedure SetSearchType(Value: TSearchType);
    function ShowFindDialog(const ASearchTerm: String; ARecordset: _Recordset): String;
    function PartialTaxonSearch: Boolean;
  protected
    function GetStoredProc: String; virtual;
    function GetResolveDuplicateStoredProc: String; virtual;
    function GetFindDialogTitle: String; virtual;
  public
    function FindUnique(const ASearchTerm: String): String;
    function RunSearch: String; overload;
    function RunSearch(const ASearchTerm: String; ANoFindDialog: Boolean = False): String; overload;
    property AdditionalCaption: String read FAdditionalCaption write SetAdditionalCaption;
    property ResultText: String read FResultText;
    property SearchKey: String read FSearchKey write SetSearchKey;
    property SearchType: TSearchType read FSearchType write SetSearchType;
  end;

//==============================================================================
implementation

uses
  ProjectSpecificAccess;

{-==============================================================================
    TAddinSearchManager
===============================================================================}
{-------------------------------------------------------------------------------
  Returns a key for the item searched if an exact single match is found.
}
function TAddinSearchManager.FindUnique(const ASearchTerm: String): String;
var
  lRecordset: _Recordset;
begin
  lRecordset := GetRecordset(ASearchTerm);

  if HaveOneExactMatch(lRecordset, ASearchTerm, True) then
  begin
    Result      := lRecordset.Fields['Item_Key'].Value;
    FResultText := lRecordset.Fields['SearchTerm'].Value;
  end else
    Result := '';
end;  // TAddinSearchManager.FindUnique

{-------------------------------------------------------------------------------
  Returns part of the find dialog caption appropriate to the search type.  The returned text
      has 'Search for ' prefixed to it to form the dialog caption.
}
function TAddinSearchManager.GetFindDialogTitle: String;
begin
  case FSearchType of
    ST_INDIVIDUAL:         Result := ResStr_Individuals;
    ST_ORGANISATION:       Result := ResStr_Organisations;
    ST_SURVEY:             Result := ResStr_Surveys;
    ST_LOCATION:           Result := ResStr_Locations;
    ST_BIOTOPE:            Result := ResStr_Biotopes;
    ST_SPECIES:            Result := ResStr_Species;
    ST_SPECIESINLIST:      Result := ResStr_Species;
    ST_NAME:               Result := ResStr_Names;
    ST_TERM:               Result := ResStr_Terms;
    ST_TERMINCONCEPTGROUP: Result := ResStr_Terms;
    ST_TERMINSUBJECTAREA:  Result := ResStr_Terms;
  end;
  if FAdditionalCaption <> '' then
    result := result + ' (' + FAdditionalCaption + ')';
end;  // TAddinSearchManager.GetFindDialogTitle

{-------------------------------------------------------------------------------
}
function TAddinSearchManager.GetRecordset(const ASearchTerm: String): _Recordset;
var
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  try
    // include the search key as an input parameter if we know it
    if SearchKey = '' then
      Result := dmGeneral.GetRecordset(
          GetStoredProc,
          ['@SearchText', StringReplace(ASearchTerm, '*', '%', [rfReplaceAll])])
    else
      Result := dmGeneral.GetRecordset(
          GetStoredProc,
          ['@SearchKey', SearchKey,
           '@SearchText', StringReplace(ASearchTerm, '*', '%', [rfReplaceAll])]);
  finally
    DefaultCursor(lCursor);
  end;
end;  // TAddinSearchManager.GetRecordset

{-------------------------------------------------------------------------------
}
function TAddinSearchManager.GetResolveDuplicateStoredProc: String;
begin
  case FSearchType of
    ST_LOCATION:           Result := 'usp_LocationFullySpecified_Select';
    ST_BIOTOPE:            Result := 'usp_BiotopeFullySpecified_Select';
    ST_SPECIESINLIST,
    ST_SPECIES:            Result := 'usp_SpeciesFullySpecified_Select';
    ST_TERMINSUBJECTAREA,
    ST_TERMINCONCEPTGROUP: Result := 'usp_ConceptFullySpecified_Select';
  else
    Result := '';
  end;
end;  // TAddinSearchManager.GetResolveDuplicateStoredProc

{-------------------------------------------------------------------------------
  Uses the search type to identify the stored procedure to run.
}
function TAddinSearchManager.GetStoredProc: String;
begin
  case FSearchType of
    ST_INDIVIDUAL:         Result := 'usp_Individual_Select_ForNameSearch';
    ST_LOCATION:           Result := 'usp_Locations_Select_ForSearch';
    ST_ORGANISATION:       Result := 'usp_Organisation_Select_ForNameSearch';
    ST_SURVEY:             Result := 'usp_Surveys_Select_ForSearch';
    ST_BIOTOPE:            Result := 'usp_Biotopes_Select_ForSearch';
    // CCN158b - ZENTRUM Partial word search on taxa - use different stored procs
    ST_SPECIES:
      begin
        if PartialTaxonSearch then
          Result := 'usp_Species_Select_ForSearch_Partial'
        else
          Result := 'usp_Species_Select_ForSearch';
      end;
    ST_SPECIESINLIST:
      begin
        if PartialTaxonSearch then
          Result := 'usp_Species_Select_ForSearchByList_Partial'
        else
          Result := 'usp_Species_Select_ForSearchByList';
      end;
    ST_NAME:               Result := 'usp_Name_Select_ForNameSearch';
    ST_TERM:               Result := 'usp_Terms_Select_ForSearch';
    ST_TERMINCONCEPTGROUP: Result := 'usp_Concept_Select_ForConceptGroupSearch';
    ST_TERMINSUBJECTAREA:  Result := 'usp_Concept_Select_ForSubjectAreaSearch';
  end;
end;  // TAddinSearchManager.GetStoredProc

{-------------------------------------------------------------------------------
  Returns trus if there is one, and only one exact match for the search term in the recordset.
}
function TAddinSearchManager.HaveOneExactMatch(ARecordset: _Recordset; const ASearchTerm: String;
    WantExact: Boolean): Boolean;
begin
  with ARecordset do begin
    // If only one record, then must be one match.
    Result := RecordCount = 1;

    // If a single exact match is required, check for it. This used to also require
    // that the record count was 1. However, it sometimes happened that more than
    // one record was returned, (i.e. 'dorset' and 'dorset software' when searching
    // for 'dorset'), yet there was is obviously an exact match here.
    if WantExact then
      Result := (RecordCount > 0) and
                (CompareText(VarToStr(Fields['SearchTerm'].Value), ASearchTerm) = 0)
    else
    if RecordCount > 1 then
    begin
      // If more than one record, check if first item matches but second one doesn't.
      if (not Result) and (RecordCount > 1) then
      begin
        MoveFirst;
        if CompareText(VarToStr(Fields['SearchTerm'].Value), ASearchTerm) = 0 then
        begin
          // first term does match
          MoveNext;
          // check second one doesn't
          Result := CompareText(VarToStr(Fields['SearchTerm'].Value), ASearchTerm) <> 0;
        end; // if
        MoveFirst;
      end; // if
    end; // if
  end;
end;  // TAddinSearchManager.HaveOneExactMatch

{-------------------------------------------------------------------------------
  Returns True if the PartialTaxonSearch flag is on, False otherwise
}
function TAddinSearchManager.PartialTaxonSearch: Boolean;
var
  AddinGeneralData: TdmAddinGeneral;
begin
  AddinGeneralData := TdmAddinGeneral.Create(nil);
  try
    Result := AddinGeneralData.Recorder.CurrentSettings.PartialTaxonSearch;
  finally
    AddinGeneralData.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Runs a search.  In this case, no initial search text is provided and the Find dialog is
      always shown.
}
function TAddinSearchManager.RunSearch: String;
begin
  Result := ShowFindDialog('', nil);
end;  // TAddinSearchManager.RunSearch

{-------------------------------------------------------------------------------
  Runs a search.  If a unique match is found, or the Find dialog is shown and the user selects
      a match, then the resulting item key is returned.  Otherwise the result is an empty
      string.
}
function TAddinSearchManager.RunSearch(const ASearchTerm: String; ANoFindDialog: Boolean = False):
    String;
var
  lRecordset: _Recordset;
  lSearchTerm: String;
begin
  // setup wildcards
  lSearchTerm := StringReplace(ASearchTerm, '%', '*', [rfReplaceAll]);
  lRecordset  := GetRecordset(lSearchTerm);
  if HaveOneExactMatch(lRecordset, lSearchTerm, False) then  begin
    Result      := lRecordset.Fields['Item_Key'].Value;
    FResultText := lRecordset.Fields['SearchTerm'].Value;
  end
  else if not ANoFindDialog then
    Result := ShowFindDialog(ASearchTerm, lRecordset);
end;  // TAddinSearchManager.RunSearch 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TAddinSearchManager.SetAdditionalCaption(const Value: String);
begin
  FAdditionalCaption := Value;
end;  // TAddinSearchManager.SetAdditionalCaption 

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TAddinSearchManager.SetSearchKey(const Value: String);
begin
  FSearchKey := Value;
end;  // TAddinSearchManager.SetSearchKey

{-------------------------------------------------------------------------------
  Accessor method. 
}
procedure TAddinSearchManager.SetSearchType(Value: TSearchType);
begin
  FSearchType := Value;
end;  // TAddinSearchManager.SetSearchType 

{-------------------------------------------------------------------------------
  Displays the find dialog as no unique match was found. 
}
function TAddinSearchManager.ShowFindDialog(const ASearchTerm: String; ARecordset: _Recordset):
    String;
begin
  with TdlgAddinFind.Create(nil) do
    try
      Title                          := GetFindDialogTitle;
      StoredProcName                 := GetStoredProc;
      ResolveDuplicateStoredProcName := GetResolveDuplicateStoredProc;
      LocationSearch                 := FSearchType = ST_LOCATION;

      // include the search key as an input parameter if we know it
      if SearchKey = '' then SetStoredProcParameters([])
                        else SetStoredProcParameters(['@SearchKey', SearchKey]);

      InitialiseSearch(ARecordset, ASearchTerm);

      if FSearchType in [ST_SPECIES, ST_SPECIESINLIST] then
        Width := 550;

      if ShowModal = mrOk then begin
        Result      := ResultKey;
        FResultText := ResultText;
      end else
        Result := '';
    finally
      Free;
    end;
end;  // TAddinSearchManager.ShowFindDialog

end.
