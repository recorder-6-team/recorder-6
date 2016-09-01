//==============================================================================
//  Unit:        ConceptTerm
//
//  Implements:  TConceptTerm
//
//  Description: Class that handles a concept and its term.
//
//  Created:     February 2008
//
//  Last Revision Details:
//    $Revision: 3 $
//    $Date: 28/02/08 10:54 $
//    $Author: Ericsalmon $
//==============================================================================
unit ConceptTerm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, ExceptionForm, DataClasses, Constants;

resourcestring
  ResStr_PleaseSpecifyATerm = 'Please specify a term.';

type
  EConceptTerm = class(TExceptionPath);

  TConceptTerm = class
  private
    FConceptKey: TKeyString;
    FTermKey: TKeyString;
    FTerm: String;
    FLanguageId: String;
    FSortCode: String;
    FTimestamp: TSQLSvrTimestamp;
    FConceptGroupKey: TKeyString;
    procedure AddOrModifyTerm;
    procedure SetConceptGroupKey(const Value: TKeyString);
    procedure SetConceptKey(const Value: TKeyString);
    procedure SetLanguageId(const Value: String);
    procedure SetSortCode(const Value: String);
    procedure SetTerm(const Value: String);

    function StripItalics(const text: String): String;
  public
    procedure Clear;
    procedure Load(const AConceptKey: TKeyString);
    function ReuseExistingConcept: Boolean;
    procedure Save;
    property ConceptKey: TKeyString read FConceptKey write SetConceptKey;
    property ConceptGroupKey: TKeyString read FConceptGroupKey write SetConceptGroupKey;
    property TermKey: TKeyString read FTermKey;
    property Term: String read FTerm write SetTerm;
    property LanguageId: String read FLanguageId write SetLanguageId;
    property SortCode: String read FSortCode write SetSortCode;
  end;           

{-------------------------------------------------------------------------------
}
implementation

uses
  ApplicationSettings, DatabaseAccessADO;

{-------------------------------------------------------------------------------
  Remove formatting from the term as not supported in this simple editor
}
function TConceptTerm.StripItalics(const text: String): String;
begin
  Result := StringReplace(text, '<i>', '', [rfReplaceAll]);
  Result := StringReplace(Result, '</i>', '', [rfReplaceAll]);
end;

{-------------------------------------------------------------------------------
  Comes here if the term or the language has changed for the concept.
}
procedure TConceptTerm.AddOrModifyTerm;
begin
  if TermKey = '' then
    with dmDatabase.GetRecordset(
        'usp_Terms_Select_ForSearch',
        ['@SearchText', Term, '@SearchKey', LanguageId]) do
    begin
      while not Eof do begin
        if SameText(VarToStr(Fields['SearchTerm'].Value), Term) then begin
          FTermKey := Fields['Item_Key'].Value;
          Break;
        end;
        MoveNext;
      end;
      Close;
    end;

  if TermKey = '' then
    FTermKey := VarToStr(dmDatabase.RunInsertStoredProc(
        TN_TERM,
        'usp_Term_Insert',
        ['@LanguageKey', LanguageId,
         '@ItemName', Term,
         '@Plaintext', StripItalics(Term),
         '@SessionID', AppSettings.SessionID],
         '@Key'));
end;

{-------------------------------------------------------------------------------
}
procedure TConceptTerm.Clear;
begin
  FConceptKey      := '';
  FConceptGroupKey := '';
  FTermKey         := '';
  FTerm            := '';
  FLanguageId      := '';
  FSortCode        := '';
  FTimestamp       := Null;
end;

{-------------------------------------------------------------------------------
}
procedure TConceptTerm.Load(const AConceptKey: TKeyString);
begin
  with dmDatabase.GetRecordset('usp_Concept_Select', ['@ConceptKey', AConceptKey]) do begin
    if not Eof then begin
      FConceptKey      := AConceptKey;
      FConceptGroupKey := Fields['Concept_Group_Key'].Value;
      FTermKey         := Fields['Item_Key'].Value;
      FTerm            := Fields['Item_Name'].Value;
      FLanguageId      := Fields['Language_Key'].Value;
      FTimestamp       := Fields['Timestamp'].Value;
      FSortCode        := vartostr(Fields['Sort_Code'].Value);
    end;
    Close;
  end;
end;

{-------------------------------------------------------------------------------
}
function TConceptTerm.ReuseExistingConcept: Boolean;
begin
  Result := False;
  if ConceptKey = '' then begin
    with dmDatabase.GetRecordset('usp_Concept_Select_ForConceptGroupSearch',
        ['@SearchKey', ConceptGroupKey, '@SearchText', Term]) do
    begin
      while not Eof do begin
        if SameText(Fields['SearchTerm'].Value, Term) then begin
          ConceptKey := Fields['Item_Key'].Value;
          Result := True;
          Break;
        end;
        MoveNext;
      end;
      Close;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TConceptTerm.Save;
var
  lParams: Array of Variant;
begin
  SetLength(lParams, 0);
  ValidateValue(Term <> '', ResStr_PleaseSpecifyATerm);

  // Check if there's a concept ready we can reuse, instead of adding duplicates.
  if ReuseExistingConcept then Exit;

  if (TermKey = '') then
    AddOrModifyTerm;

  dmDatabase.Connection.BeginTrans;

  try
    lParams := VarArrayOf([
        '@Key', ConceptKey,
        '@TermKey', TermKey,
        '@ConceptGroupKey', ConceptGroupKey,
        '@Preferred', True,
        '@ConceptRankKey', Null,
        '@NameTypeConceptKey', CONCEPT_KEY_COMMON,
        '@SortCode', SortCode,
        '@ListCode', Null,
        '@Timestamp', FTimestamp,
        '@SessionID', AppSettings.SessionID]);

    // Empty key means new record.
    if ConceptKey = '' then begin
      ConceptKey := VarToStr(dmDatabase.RunInsertStoredProc(
          TN_CONCEPT,
          'usp_ConceptSimple_Insert',
          lParams,
          '@Key'));
    end else
      dmDatabase.RunUpdateStoredProc('usp_ConceptSimple_Update', lParams);

    dmDatabase.Connection.CommitTrans;

    // Will load all other info.
    Load(ConceptKey);
  except
    on E:Exception do begin
      dmDatabase.Connection.RollbackTrans;
      if SameText(E.Message, 'Record updated by another user') then  // <= I don't like this!
        raise EConceptTerm.CreateNonCritical(ResStr_AnotherUserUpdated, E)
      else
        raise;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TConceptTerm.SetConceptGroupKey(const Value: TKeyString);
begin
  FConceptGroupKey := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TConceptTerm.SetConceptKey(const Value: TKeyString);
begin
  FConceptKey := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TConceptTerm.SetLanguageId(const Value: String);
begin
  FLanguageId := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TConceptTerm.SetSortCode(const Value: String);
begin
  FSortCode := Value;
end;

{-------------------------------------------------------------------------------
  Changing the term means the key may not be the correct one anymore.
  So clear it at the same time.
}
procedure TConceptTerm.SetTerm(const Value: String);
begin
  if Value <> FTerm then begin
    FTerm := Value;
    FTermKey := '';
  end;
end;

end.
