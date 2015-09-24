//==============================================================================
//  Unit:        BaseBiotopeDictUnit
//
//  Implements:  TBaseBiotopeDict
//
//  Description: Base class for all biotope dictionaries, browser and editor.
//               Implements some abstract functions declared in TBaseDictionary
//               class, including the dictionary items comparison function which
//               is specific to each dictionary type, and adds menus common to
//               biotope dictionaries.
//
//  Author:      Eric Salmon
//  Created:     23 Nov 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 13 $
//    $Date: 21/01/08 14:27 $
//    $Author: Johnvanbreda $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BaseBiotopeDictUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseDictionaryUnit, exgrid, RapTree, ComCtrls, StdCtrls, ExtCtrls, ActnList,
  Menus, BaseBiotopeDictDataUnit, DataClasses, HierarchyNodes, Constants,
  KeyboardRapidTree;

type
  TBaseBiotopeDict = class(TBaseDictionary)
    pmHierarchy: TPopupMenu;
    pmHSortBy: TMenuItem;
    pmHSortDefault: TMenuItem;
    pmHSortName: TMenuItem;
    pmHSortCode: TMenuItem;
    pmHSortLongName: TMenuItem;
    pmSort: TPopupMenu;
    pmSortDefault: TMenuItem;
    pmSortName: TMenuItem;
    pmSortOriginalCode: TMenuItem;
    pmSortLongName: TMenuItem;
    mnuEdit: TMenuItem;
    mnuEditCopy: TMenuItem;
    N1: TMenuItem;
    mnuEditFind: TMenuItem;
    mnuEditSort: TMenuItem;
    mnuEditSortDefault: TMenuItem;
    mnuEditSortName: TMenuItem;
    mnuEditSortOriginalCode: TMenuItem;
    mnuEditSortLongName: TMenuItem;
    mnuEditFilter: TMenuItem;
    alBiotopeDiction: TActionList;
    actSortDefault: TAction;
    actSortName: TAction;
    actSortCode: TAction;
    actSortLongName: TAction;
    actFind: TAction;
    actFilter: TAction;
    procedure actSortDefaultExecute(Sender: TObject);
    procedure actSortNameExecute(Sender: TObject);
    procedure actSortCodeExecute(Sender: TObject);
    procedure actSortLongNameExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure tvDictionaryDblClick(Sender: TObject);
  protected
    function CompareDictionaryItems(const Data1, Data2: TDictionaryNode; const ASortOrder: String): Integer; override;
    procedure SetupObjects; override;
    function GetListForItem(const AKey: string): string; override;
    function DefaultSortField: string; override;
    function SortRegistryName: string; override;
    function SortNameToSortSQL(const ASortName: string): string; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, FormActions, Find, Rucksack, Maintbar, GeneralData,
  DatabaseAccessADO, Variants;

const
  DEFAULT_SORT   = 'Default';
  NAME_SORT      = 'Name';
  CODE_SORT      = 'Code';
  LONG_NAME_SORT = 'LongName';

resourcestring
  ResStr_FindBiotope =  'Find Biotope';
  ResStr_Biotope =  'biotope';
  ResStr_Biotopes =  'biotopes';


//==============================================================================
procedure TBaseBiotopeDict.SetupObjects;
begin
  DictionaryData := TBaseBiotopeDictData.Create(nil);
end;  // SetupObjects

//==============================================================================
procedure TBaseBiotopeDict.tvDictionaryDblClick(Sender: TObject);
var loForm: TForm;
begin
  inherited;
  with tvDictionary.Selected do
    if not HasChildren then begin
      loForm := frmMain.GetForm(TfrmRucksack);
      if loForm <> nil then
        TfrmRucksack(loForm).AddBiotopeToRucksack(TDictionaryNode(Data).ItemKey);
    end;
end;  // tvDictionaryDblClick

//==============================================================================
procedure TBaseBiotopeDict.actFindExecute(Sender: TObject);
begin
  inherited;
  LocateDictionaryItem(FindDictionaryItem('', ResStr_FindBiotope, ftBiotope), ResStr_Biotope);
end;  // actFindExecute

//==============================================================================
procedure TBaseBiotopeDict.actSortDefaultExecute(Sender: TObject);
begin
  SortOrder := DEFAULT_SORT;
end;  // actSortDefaultExecute

//==============================================================================
procedure TBaseBiotopeDict.actSortNameExecute(Sender: TObject);
begin
  SortOrder := NAME_SORT;
end;  // actSortNameExecute

//==============================================================================
procedure TBaseBiotopeDict.actSortCodeExecute(Sender: TObject);
begin
  SortOrder := CODE_SORT;
end;  // actSortCodeExecute

//==============================================================================
procedure TBaseBiotopeDict.actSortLongNameExecute(Sender: TObject);
begin
  SortOrder := LONG_NAME_SORT;
end;  // actSortLongNameExecute

//==============================================================================
function TBaseBiotopeDict.CompareDictionaryItems(const Data1, Data2: TDictionaryNode;
  const ASortOrder: String): Integer;
var loItem1, loItem2: TBiotopeDictionaryNode;
    lsItem1, lsItem2: String;
begin
  Result := 0;
  if Assigned(Data1) and Assigned(Data2) then begin
    loItem1 := TBiotopeDictionaryNode(Data1);
    loItem2 := TBiotopeDictionaryNode(Data2);

    if ASortOrder = DEFAULT_SORT then begin
      if loItem1.SortCode < loItem2.SortCode then Result := -1 else
      if loItem1.SortCode > loItem2.SortCode then Result :=  1
                                             else Result :=  0;
    end else begin
      if ASortOrder = NAME_SORT then begin
        lsItem1 := loItem1.ShortTerm;
        lsItem2 := loItem2.ShortTerm;
      end else if ASortOrder = CODE_SORT then begin
        lsItem1 := loItem1.OriginalCode;
        lsItem2 := loItem2.OriginalCode;
      end else begin  // LONG_NAME_SORT
        lsItem1 := loItem1.LongName;
        lsItem2 := loItem2.LongName;
      end;

      if (lsItem1 = '') and (lsItem2 = '') then
        Result := -1  // Using this leaves the order unchanged, apparently
      else
        Result := CompareText(lsItem1, lsItem2);
    end;
  end;
end;  // CompareDictionaryItems

//==============================================================================
procedure TBaseBiotopeDict.actFilterExecute(Sender: TObject);
var lListVersion: TListVersion;
begin
  inherited;
  lListVersion := dmGeneralData.SetListVersionKeys(true, ListKeyData.ItemKey);
  FilterDictionary('Biotope', ResStr_Biotopes, 'AND BT_CL_Version_Key IN (' + lListVersion.LatestVersions + ')');
end;  // actFilterExecute

//==============================================================================
function TBaseBiotopeDict.GetListForItem(const AKey: string): string;
const
  SQL_LIST_FOR_TAXON = 'SELECT BCV.Biotope_Classification_Key '+
      'FROM Biotope_Classification_Version BCV '+
      'INNER JOIN Biotope_List_Item BLI ON BLI.BT_CL_Version_Key=BCV.BT_CL_Version_Key '+
      'WHERE BLI.Biotope_List_Item_Key=''%s''';
begin
  with dmDatabase.ExecuteSQL(Format(SQL_LIST_FOR_TAXON, [AKey]), True) do
    if not (EOF and BOF) then
      Result := VarToStr(Fields[0].Value)
    else
      Result := '';
end;

{-------------------------------------------------------------------------------
  Retrieve the field used for a default sort
}
function TBaseBiotopeDict.DefaultSortField: string;
begin
  Result := DEFAULT_SORT;
end;

{-------------------------------------------------------------------------------
  Retrieve the name of the sort key in the registry
}
function TBaseBiotopeDict.SortRegistryName: string;
begin
  Result := 'Biotope';
end;

{-------------------------------------------------------------------------------
  Convert a sort field name to the actual sort SQL used
}
function TBaseBiotopeDict.SortNameToSortSQL(
  const ASortName: string): string;
begin
  if ASortName = NAME_SORT then
    Result := 'ORDER BY B.Short_Term, BLI.Sort_Code'
  else if ASortName = CODE_SORT then
    Result := 'ORDER BY BLI.Sort_Code, B.Short_Term'
  else if ASortName = LONG_NAME_SORT then
    Result := 'ORDER BY B.Full_Term, BLI.Sort_Code'
  else
    Result := 'ORDER BY BLI.Sort_Code, B.Short_Term';
end;

{-------------------------------------------------------------------------------
}
constructor TBaseBiotopeDict.Create(AOwner: TComponent);
begin
  TableName := 'BIOTOPE_LIST_ITEM';
  InitialListKey := AppSettings.BiotopeListKey;
  inherited;
end;

end.
