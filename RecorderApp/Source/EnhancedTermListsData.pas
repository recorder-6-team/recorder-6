//==============================================================================
//  Unit:        EnhancedTermListsData
//
//  Implements:  TdmEnhancedTermLists
//
//  Description: Data Module for reference keywords & other term lists
//
//  Author:      John van Breda
//  Created:     December 2005
//
//  Last Revision Details:
//    $Revision: 5 $
//    $Date: 6/02/08 15:14 $
//    $Author: Ericsalmon $
//==============================================================================
unit EnhancedTermListsData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Constants,
  Dialogs, BaseData, DB, ADODB, JNCCDatasets, DatabaseAccessADO, ExceptionForm;

resourcestring
  ResStr_SpecifyValidLanguage = 'Please specify a valid language.';

type
  EEnhancedTermListsData = class(TExceptionPath);

  TdmEnhancedTermLists = class(TBaseDataModule)
    qryLanguages: TJNCCQuery;
    qryLanguagesLanguage_Key: TStringField;
    qryLanguagesItem_Name: TStringField;
    qrySynonyms: TJNCCQuery;
    qrySynonymsItem_Key: TStringField;
    qrySynonymsItem_Name: TWideStringField;
    qrySynonymsLanguage_Key: TStringField;
    qrySynonymsLanguage: TStringField;
    qrySynonymsIndicator: TBooleanField;
    qrySynonymsCustodian: TStringField;
    qrySynonymsEntered_By: TStringField;
  private
    FSelectedConceptKey: string;
    procedure SetSelectedConceptKey(const Value: string);
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateRow( var ioRowKey : string; iData : TStringList );
    procedure DeleteRow( iRowKey : String );
    property SelectedConceptKey: string read FSelectedConceptKey write 
        SetSelectedConceptKey;
  end;

implementation

{$R *.dfm}

uses
  ApplicationSettings;


{ TdmEnhancedTermLists }

{-------------------------------------------------------------------------------
  Callback for the grid manager class to delete a row
}
procedure TdmEnhancedTermLists.DeleteRow(iRowKey: String);
begin
  dmDatabase.RunStoredProc('usp_Concept_Delete', ['@Key', iRowKey]);
end;

{-------------------------------------------------------------------------------
  Callback for the grid manager class to update a synonym
}
procedure TdmEnhancedTermLists.UpdateRow(var ioRowKey: string;
  iData: TStringList);
begin
  if iData.Values['Language_Key']='' then
    raise EEnhancedTermListsData.CreateNonCritical(ResStr_SpecifyValidLanguage);
  if ioRowKey = '' then begin
    dmDatabase.RunInsertStoredProc('Concept', 'usp_ConceptSynonym_Insert',
        ['@PreferredConceptKey', SelectedConceptKey,
        '@Term', iData.Values['Item_Name'],
        '@LanguageKey', iData.Values['Language_Key'],
        '@NameTypeConceptKey', CONCEPT_KEY_COMMON,
        '@SessionID', AppSettings.SessionID,
        '@SiteID', AppSettings.SiteID,
        '@SystemSuppliedData', 0
        ], '@ConceptKey')
  end
  else begin
    // Update existing row
    dmDatabase.RunUpdateStoredProc('usp_ConceptSynonym_Update',
        ['@ConceptKey', ioRowKey,
        '@Term', iData.Values['Item_Name'],
        '@LanguageKey', iData.Values['Language_Key'],
        '@SessionID', AppSettings.SessionID,
        '@SystemSuppliedData', 0
        ]);
  end;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TdmEnhancedTermLists.SetSelectedConceptKey(const Value: string);
begin
  FSelectedConceptKey := Value;
end;

end.
