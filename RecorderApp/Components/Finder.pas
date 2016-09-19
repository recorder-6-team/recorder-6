//==============================================================================
//  Unit:        Finder
//
//  Implements:  TFinder
//
//  Description:
//
//  Author:      Robert Kinsey
//  Created:     7 February 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 50 $
//    $Date: 30/06/09 10:39 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Finder;

interface

uses
  Windows, Messages, SysUtils, Classes, StdCtrls, DataClasses, Controls, Forms,
  ComCtrls, JNCCDataSets, Constants, ADODB, Contnrs, Dialogs;

type
  TSearchMode = (smMinChars, smAlways, smOnRequest);

  TSearchFor = (sfTaxon, sfLocation, sfName, sfReference, sfNone);

  TGetFurtherDetails = function (const iKey: TKeyString ): string of object;

  //----------------------------------------------------------------------------
  TSourceItem = class
  private
    FKeyData: TKeyData;
    FDisplayText: String;
    FItems: TStringList;
    function GetItem(Index: Integer): String;
//    function PartialTaxonSearchMatch(ASearchAnywhere: Boolean; APartials: TStrings;
//      const ATextToCheck: String): Boolean;
    function PartialTaxonSearchMatch(const ASearchAnywhere: Boolean;
      const AGenusText, ATrinomialText, AQuadrinomialText, ASpeciesText, ATextToCheck: string): Boolean;
    function PerformPartialTaxonSearchMatch(const ASearchAnywhere,
        AMatchEndOfTrinomial, AMatchEndOfQuadrinomial, AMatchEndOfSpecies: Boolean;
        const AGenusText, ATrinomialText, AQuadrinomialText, ASpeciesText,
        AGenusToCheck, ATrinomialToCheck, AQuadrinomialToCheck, ASpeciesToCheck: string): Boolean;
  public
    constructor Create(const AKey: TKeyString; const ADisplayText: String;
      Strings: Array of String; AdditionalInfo: String = '');
    destructor Destroy; override;
    function IsTextInItems(const AText: String; ASearchMode: TSearchMode;
      const APartialTaxonSearch: Boolean): Boolean;
    property KeyData: TKeyData read FKeyData;
    property DisplayText: String read FDisplayText;
    property Items[Index: Integer]: String read GetItem; default;
  end;

  //----------------------------------------------------------------------------
  // ObjectList owns and frees its objects, so there is no need to override
  // the destructor, or the Clear method, to free everything.
  TSourceList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TSourceItem;
    procedure SetItem(Index: Integer; AObject: TSourceItem);
  public
    property Items[Index: Integer]: TSourceItem read GetItem write SetItem; default;
  end;

  //----------------------------------------------------------------------------
  TFinder = class(TCustomEdit)
  private
    FSourceList   : TSourceList;
    FOutputList   : TListBox;
    FDistinctList : TStringList;
    FNewIndex     : integer;
    FNoSourceItems: boolean;
    FOnPopulateList : TNotifyEvent;
    FOnPopulateStop : TNotifyEvent;
    FOldText : string;
    FSearchMode: TSearchMode;
    FSearchFor: TSearchFor;
    FDetailsQuery : TJNCCQuery;
    FLongCharSearch : Boolean;
    FPartialTaxonSearch: Boolean;
    FMinChars: integer;
    procedure AddToOutputList( const AString: string; AKeyData: TKeyData );
    procedure ChangeCursor;
    function  CompareNoStar(const iOldText, iNewText: String): boolean;
    function  GetAddress( const iNameKey: TKeyString ): string;
    function  GetDOB( const iNameKey: TKeyString ): string;
    function  GetFullReference ( const iReferenceKey: TKeyString ): string;
    function  GetNoSourceItems: boolean;
    function  GetPreferredTaxonName(const AKey:TKeyString): string;
    function  GetSiteCentroid( const iLocationNameKey: TKeyString ): string;
    function  LengthNoStar( const iText : string ) : integer;
    procedure PopulateOutputList;
    procedure ProcessDuplicates(const AString: string; AKeyData: TKeyData;
      iFirstDuplicate: Boolean);
    procedure ResetCursor;
    procedure SetOnPopulateList(const Value: TNotifyEvent);
    procedure SetOnPopulateStop(const Value: TNotifyEvent);
    procedure SetOutputList(const Value: TListBox);
    procedure SetSearchMode(const Value: TSearchMode);
    procedure SetPartialTaxonSearch(const Value: Boolean);
    procedure SetMinChars(const Value: integer);
  protected
    procedure Change; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetLongCharSearch(Value : Boolean);
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure ForcePopulate;
    procedure ClearSourceList;
    procedure AddToSourceList(const AKey: TKeyString; const DisplayText: String;
      Strings: Array of String);
    procedure AddToSourceListWithAddition(const AKey: TKeyString; const DisplayText: String;
      Strings: Array of String; const AdditionalInfo: String);
    procedure SetSearchText(const AString:string; const iForcePopulate:boolean=false);
    procedure SetDatabase(const ADatabase: TADOConnection);
    function ReInsert(const AString: string; const AKey: TKeyString):boolean;
    property SourceList :TSourceList read FSourceList;
    property NoSourceItems : boolean read GetNoSourceItems write FNoSourceItems;
    property HandleDuplicate:TSearchFor read FSearchFor write FSearchFor;
    property LongCharSearch : Boolean read FLongCharSearch Write SetLongCharSearch;
    property PartialTaxonSearch : Boolean read FPartialTaxonSearch write SetPartialTaxonSearch;
    property MinChars : integer read FMinChars write SetMinChars;
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property SearchMode: TSearchMode read FSearchMode write SetSearchMode default smMinChars;
    property OEMConvert;
    property OutputList:TListBox read FOutputList write SetOutputList;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnPopulateList : TNotifyEvent read FOnPopulateList write SetOnPopulateList;
    property OnPopulateStop : TNotifyEvent read FOnPopulateStop write SetOnPopulateStop;
  end;

//==============================================================================
implementation

resourcestring
  ResStr_NoAddressPresent = ' <No address present> ';
  ResStr_NoDOB = ' <No Dob of Dod present> ';
var
  lCursor: TCursor;

//==============================================================================
//==============================================================================
{ TSourceItem }
//==============================================================================
constructor TSourceItem.Create(const AKey: TKeyString; const ADisplayText: String;
  Strings: array of String; AdditionalInfo: String = '');
var i: Integer;
begin
  FKeyData := TKeyData.Create;
  FKeyData.ItemKey := AKey;
  FKeyData.ItemAdditional := AdditionalInfo;
  FDisplayText:= ADisplayText;
  if SizeOf(Strings) > 0 then begin
    FItems := TStringList.Create;
    FItems.Capacity := SizeOf(Strings);
    for i := 0 to High(Strings) do
      FItems.Add(LowerCase(Strings[i]));  // Change to lowercase now to
                                          // make search in strings easier later
  end;
end;

//------------------------------------------------------------------------------
destructor TSourceItem.Destroy;
begin
  FKeyData.Free;
  FKeyData := nil;
  FItems.Free;
  FItems := nil;
  inherited;
end;

//------------------------------------------------------------------------------
function TSourceItem.GetItem(Index: Integer): String;
begin
  if Assigned(FItems) then
    Result := FItems[Index]
  else
    Result := '';
end;

//------------------------------------------------------------------------------
// Search the Items for the given text.
function TSourceItem.IsTextInItems(const AText: String; ASearchMode: TSearchMode;
  const APartialTaxonSearch: Boolean): Boolean;
var i, len, spacePos: Integer;
    lowerCaseText   : String;
    genusText       : String;
    trinomialText   : String;
    quadrinomialText: String;
    speciesText     : String;
    tagfreeText     : String;
    searchAnywhere  : Boolean;
//    partials: TStringList;
begin
  // If search text is empty but still want to display the lot
  Result := (ASearchMode in [smAlways, smOnRequest]);
  // Remove italic tags
  tagFreeText := LowerCase(StringReplace(StringReplace(
      DisplayText,
      '<i>', '', [rfReplaceAll]),
      '</i>', '', [rfReplaceAll]));

  // If text not empty, filter the list
  if (AText <> '') and (ASearchMode <> smOnRequest) then begin
    // Switch to lowercase, just like text in Items
    searchAnywhere := False;
    lowerCaseText  := LowerCase(AText);
    if lowerCaseText[1] = '*' then begin
      searchAnywhere := True;
      lowerCaseText := Copy(lowerCaseText, 2, Length(lowerCaseText));
    end;

    // Remove trailing '*' so that they don't interfere with the search
    while (lowerCaseText <> '') and (lowerCaseText[Length(lowerCaseText)] = '*') do
      lowerCaseText := Copy(lowerCaseText, 1, Length(lowerCaseText) - 1);

    // VI 14642 - CCN158b - ZENTRUM Partial word search on taxa
    spacePos := Pos(' ', lowerCaseText);
    if APartialTaxonSearch and (spacePos > 0) then begin
{
      // Partial rewrite. Await confirmation before proceeding. Leave code in and commented for now
      partials := TStringList.Create;
      try
        partials.Delimiter := ' ';
        // Not taking any risks, don't want to see any ',' in there.
        partials.DelimitedText := StringReplace(lowerCaseText, ',', [rfReplaceAll]);
        Result := PartialTaxonSearchMatch(searchAnywhere, partials, tagFreeText);
      finally
        partials.Free;
      end;
}
      genusText   := Copy(lowerCaseText, 1, spacePos - 1);
      speciesText := Copy(lowerCaseText, spacePos + 1, Length(lowerCaseText));
      spacePos := Pos(' ', speciesText);
      if (spacePos > 0) then begin
        trinomialText := Copy(speciesText, 1, spacePos - 1);
        speciesText   := Copy(speciesText, spacePos + 1, Length(speciesText));
        spacePos := Pos(' ', speciesText);
        if (spacePos > 0) then begin
          quadrinomialText := Copy(speciesText, 1, spacePos - 1);
          speciesText      := Copy(speciesText, spacePos + 1, Length(speciesText));
        end else
          quadrinomialText := '';
      end else begin
        trinomialText := '';
      end;    // if (SpacePos > 0)

      // Start by checking the DisplayText, we might get lucky straight away
      Result := PartialTaxonSearchMatch(
          searchAnywhere, genusText, trinomialText, quadrinomialText, speciesText, tagFreeText);

      // No luck with DispayText, process individual strings then.
      if (not Result) and Assigned(FItems) then
        for i := 0 to FItems.Count - 1 do
          if PartialTaxonSearchMatch(
              searchAnywhere, genusText, trinomialText, quadrinomialText, speciesText, FItems[i]) then
          begin
            Result := True;
            Break;
          end;
    // End of partial taxon searching code
    end else begin
      len := Length(lowerCaseText);
      // Start by checking the DisplayText, we might get lucky straight away
      Result := (searchAnywhere and (Pos(lowerCaseText, tagFreeText) > 0)) or
                (Copy(tagFreeText, 1, Len) = lowerCaseText);
      // No luck with DispayText, process individual strings then.
      if (not Result) and Assigned(FItems) then
        for i := 0 to FItems.Count - 1 do
          if (searchAnywhere and (Pos(lowerCaseText, FItems[i]) > 0)) or
             (Copy(FItems[i], 1, Len) = lowerCaseText) then
          begin
            Result := True;
            Break;
          end;
    end;    // if (APartialTaxonSearch)
  end;
end;

//==============================================================================
//==============================================================================
{ TSourceList }
//==============================================================================
function TSourceList.GetItem(Index: Integer): TSourceItem;
begin
  Result := TSourceItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------
procedure TSourceList.SetItem(Index: Integer; AObject: TSourceItem);
begin
  inherited Items[Index] := AObject;
end;

//==============================================================================
//==============================================================================
{ TFinder }
//==============================================================================
constructor TFinder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSearchMode              := smMinChars;
  FNoSourceItems           := True;
  FDistinctList            := TStringList.Create;
  FDistinctList.Sorted     := True;
  FDistinctList.Duplicates := dupError;
  FDetailsQuery            := TJNCCQuery.Create(nil);
  FDetailsQuery.ParseSQL   := False;
  FLongCharSearch          := False; // default to search 2 chars
  FPartialTaxonSearch      := False; // default to standard taxon search
  FMinChars                := MIN_CHARS;
  FSourceList := TSourceList.Create;
end;  // Create

//==============================================================================
destructor TFinder.Destroy;
begin
  ClearSourceList;

  FSourceList.Free;  // ObjectList owns its objects and will free them itself.
                      // That's wy there no overridden destructor for it.
  FSourceList := nil;

  FDistinctList.Free;
  FDistinctList := nil;
  FDetailsQuery.Free;
  FDetailsQuery := nil;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TFinder.SetOutputList(const Value: TListBox);
begin
  FOutputList := Value;
end;  // SetOutputList

//==============================================================================
procedure TFinder.ClearSourceList;
begin
  FSourceList.Clear;  // ObjectList owns its objects and will free them itself.
  FDistinctList.Clear;
  FNoSourceItems:=True;
end;  // ClearSourceList

//==============================================================================
procedure TFinder.AddToSourceList(const AKey: TKeyString; const DisplayText: String;
  Strings: Array of String);
begin
  AddToSourceListWithAddition(AKey, DisplayText, Strings, '');
end;

//==============================================================================
procedure TFinder.AddToSourceListWithAddition(const AKey: TKeyString; const DisplayText: String;
  Strings: Array of String; const AdditionalInfo: String);
var NewSourceItem: TSourceItem;
begin
  NewSourceItem := TSourceItem.Create(AKey, DisplayText, Strings, AdditionalInfo);
  FNewIndex := FSourceList.Add(NewSourceItem);
  // Validate new item to see if it needs to go straight to the output list too
  if Assigned(FOutputList) then
    if FSourceList[FNewIndex].IsTextInItems(Text, SearchMode, PartialTaxonSearch) then
      AddToOutputList(DisplayText, FSourceList[FNewIndex].KeyData);
  FNoSourceItems := false;
end;

//==============================================================================
procedure TFinder.ForcePopulate;
begin
  if (SearchMode in [smAlways, smOnRequest]) and Assigned(FOnPopulateList) then FOnPopulateList(Self);
  PopulateOutputList;
end;  // ForcePopulate

//==============================================================================
procedure TFinder.KeyPress(var Key: Char);
begin
  if Key='|' then begin
    Beep;
    Key:=#0;
  end;
  inherited;
end;  // KeyPress

//==============================================================================
procedure TFinder.KeyDown(var Key: Word; Shift: TShiftState);
var liKeyData: Longint;
begin
  // If navigation key pressed, send a fake KEYDOWN message to the listbox,
  // so it can deal with it itself.
  if Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT] then begin
    liKeyData := 0;
    if ssShift in Shift then liKeyData := liKeyData  + VK_SHIFT;
    if ssCtrl in Shift then liKeyData := liKeyData + VK_CONTROL;
    SendMessage(FOutputList.Handle, WM_KEYDOWN, Key, liKeyData);
    Key := 0;
  end;
end;  // KeyDown


//==============================================================================
// Description : Accessor method
// created : 23/10/02
procedure TFinder.SetLongCharSearch(Value : Boolean);
begin
  FLongCharSearch := Value;
end;

//==============================================================================
procedure TFinder.Change;
var lLen:integer;
    lChanged : boolean;
begin
  if not (csDesigning in ComponentState) then begin
    FOutputlist.Enabled := True;
    lLen:=LengthNoStar(Text);
    if FLongCharSearch then
      // in LongCharSearch mode, any change to earlier chars causes a research
      lChanged := Copy(Text, 1, Length(FOldText)) <> FOldText
    else
      // normally, research on change to first 2 chars 
      lChanged := CompareNoStar(FOldText, Text);
    FOldText := Text;
    case FSearchMode of
      smMinChars:
          begin
            { trigger event to populate the output list - if any of the first few
               characters have changed, and we have enough characters }
            if (lLen>=FMinChars) and Assigned(FOnPopulateList) and
               lChanged then
            begin
              ClearSourcelist;
              FDistinctList.Clear;
              FOutputList.Clear;
              FOnPopulateList(Self);
            end
            else if (lLen>=FMinChars) then
              PopulateOutputList
            else begin
              if Assigned(FOnPopulateStop) then
                FOnPopulateStop(Self);
              ClearSourcelist;
              FOutputList.Clear;
              FDistinctList.Clear;
              FOutputList.Enabled := False;
              FOutputlist.Items.Add(Format(ResStr_MinCharSearch, [MinChars]));
            end;
          end; // smMinChars
      smAlways :
          PopulateOutputList;
    end; // case
  end;
  // Make sure the first item gets selected once the list is populated
  SendMessage(FOutputList.Handle, WM_KEYDOWN, VK_HOME, 0);
  inherited;
end;  // Change

//==============================================================================
{ Compare the two strings to see if the first characters have changed (up to
    FMinChars are compared).  Characters that were originally * and remain
    * are not counted }
function TFinder.CompareNoStar(const iOldText, iNewText : String): boolean;
var
  lPos, lCount : integer;
  lDiffer : boolean;
begin
  lPos := 1;
  lcount := 0; // no of chars other than * found
  lDiffer := false;
  while (lPos<=Length(iOldText)) and (lPos<=Length(iNewText)) and
        (lCount<FMinChars) and (not lDiffer) do
  begin
    lDiffer := iOldText[lPos] <> iNewText[lPos];
    if (iOldText[lPos]<>'*') or (iNewText[lPos]<>'*') then
      Inc(lCount);
    Inc(lPos);
  end;
  Result := lDiffer or (lCount<FMinChars);
end;  // CompareNoStar

//==============================================================================
procedure TFinder.PopulateOutputList;
var i: Integer;
begin
  if (Length(Text) >= FMinChars) or (FSearchMode in [smAlways, smOnRequest]) then begin
    ChangeCursor;
    FDistinctList.Clear;
    FOutputList.Clear;
    for i := 0 to FSourceList.Count - 1 do begin
      if FSourceList[i].IsTextInItems(Text, SearchMode, PartialTaxonSearch) then
        AddToOutputList(FSourceList[i].DisplayText, FSourceList[i].KeyData);
      // Try to show something in the listbox
      if (i mod 100) = 0 then FOutputList.Repaint;
    end;
    if FOutputList.Items.Count>0 then FOutputList.ItemIndex:=0;
    ResetCursor;
  end; // if
end;  // PopulateOutputList

//==============================================================================
procedure TFinder.SetSearchText(const AString: string; const iForcePopulate:boolean=false);
begin
  // Force a change otherwise it doesn't register
  if Text = AString then
    Text := ''
  else if SearchMode = smAlways then
    Text := '*';
  // Restore initial text
  Text := AString;
  SelStart := Length(Text);
  if iForcePopulate then ForcePopulate;
end;  // SetSearchText

//==============================================================================
procedure TFinder.ChangeCursor;
begin
  lCursor:=Screen.Cursor;
  Screen.Cursor:=crAppStart;
end;  // ChangeCursor

//==============================================================================
procedure TFinder.ResetCursor;
begin
  Screen.Cursor:=lCursor;
end;  // ResetCursor;

//==============================================================================
function TFinder.ReInsert(const AString: string; const AKey: TKeyString):boolean;
var lIdx: Integer;
begin
  Result := false;
  with FSourceList do
    for lIdx := 0 to FSourceList.Count - 1 do
      if FSourceList[lIdx].KeyData.ItemKey = AKey then begin
        if FSourceList[lIdx].IsTextInItems(Self.Text, SearchMode, PartialTaxonSearch) then
          FOutputList.Items.AddObject(AString, FSourceList[lIdx].KeyData);
        Result := true;
        Break;
      end;
end;  // ReInsert

//==============================================================================
procedure TFinder.SetOnPopulateList(const Value: TNotifyEvent);
begin
  FOnPopulateList := Value;
end;  // SetOnPopulateList

//==============================================================================
procedure TFinder.SetOnPopulateStop(const Value: TNotifyEvent);
begin
  FOnPopulateStop := Value;
end;  // SetOnPopulateStop

//==============================================================================
{ Function to return the length of a string, ignoring the star at the beginning
    and end if present }
function TFinder.LengthNoStar(const iText: string): integer;
var lStars : integer;
begin
  lStars := 0;
  if Length(iText)>0 then
  begin
    { Check beginning and end for * }
    if iText[1]='*' then
      lStars := 1;
    if iText[Length(iText)]='*' then
      Inc(lStars);
  end;
  Result := Length(iText)-lStars;
end;  // LengthNoStar

//==============================================================================
procedure TFinder.SetSearchMode(const Value: TSearchMode);
begin
  FSearchMode := Value;
end;  // SetSearchMode

//==============================================================================
{ No source items - used to indicate nothing available to search on.  However,
    we cannot do this if we are requiring 2 characters for initial search }
function TFinder.GetNoSourceItems: boolean;
begin
  Result := FNoSourceItems and (FSearchMode = smAlways);
end;

//==============================================================================
{ Checks to see if item is duplicated in taxon list. If so, then further details
  are found depending upon the type of the item  }
procedure TFinder.AddToOutputList(const AString: string; AKeyData: TKeyData);
var lDupIndex: integer;
begin
  if HandleDuplicate = sfNone then
    FOutputList.Items.AddObject(AString, AKeyData)
  else begin
    try
      FDistinctList.AddObject(AString, (Ptr(0)));
      FOutputList.Items.AddObject(AString, AKeyData); // If not duplicated then add item to output list
    except
      on EStringListError do begin
        lDupIndex := FDistinctList.IndexOf(AString); // Find duplicate item
        if Integer(FDistinctList.Objects[lDupIndex]) = 0 then begin // If duplicates exist already object=1
          ProcessDuplicates(AString, AKeyData, True);
          FDistinctList.Objects[lDupIndex] := Ptr(1); // Change flag to denote presence of modified item
        end else // this is the first duplicate encountered for this item
          ProcessDuplicates(AString, AKeyData, False);
      end;
    end; // try..finally
  end;
  if (FOutputList.Items.Count<>0) and (FOutputList.ItemIndex<>0) then
    FOutputList.ItemIndex:=0;  // Select first item in list
end; // AddToOutputList

//==============================================================================
{ Finds preferred names for duplicate items in the Source list and adds further
  information to the strings in FSourceList accordingly }
procedure TFinder.ProcessDuplicates(const AString: string; AKeyData: TKeyData;
  iFirstDuplicate: Boolean);
var
  idx: Integer;
  furtherDetails: String;
  lGetFurtherDetails : TGetFurtherDetails;
begin
  { Set method pointer depending on what we are searching for }
  lGetFurtherDetails := nil;
  case HandleDuplicate of
    sfTaxon    : lGetFurtherDetails := GetPreferredTaxonName;
    sfLocation : lGetFurtherDetails := GetSiteCentroid;
    sfName     : lGetFurtherDetails := GetDob;
    sfReference: lGetFurtherDetails := GetFullReference;
  end;

  with FOutputList.Items do begin
    if iFirstDuplicate then begin
      { Add additional differentiating info to existing item first }
      idx := IndexOf(AString);
      furtherDetails := lGetFurtherDetails(TKeyData(Objects[idx]).ItemKey);
      if Trim(furtherDetails) <> '' then
        Strings[idx] := Strings[idx] + ' (' + Trim(furtherDetails) + ')';
    end;

    { Get further details for the new item }
    furtherDetails := lGetFurtherDetails(AKeyData.ItemKey);
    if Trim(furtherDetails) <> '' then furtherDetails := ' (' + Trim(furtherDetails) + ')';
    AddObject(AString + furtherDetails, AKeyData)
  end;
end; // ProcessDuplicates

//==============================================================================
function TFinder.GetSiteCentroid(const iLocationNameKey: TKeyString): string;
begin
  with FDetailsQuery do begin
    if Active then Close;
    SQL.Text := 'SELECT L.Spatial_Ref, L.Spatial_Ref_System, L.LAT, L.LONG ' +
                'FROM Location L, Location_Name LN ' +
                'WHERE L.Location_Key = LN.Location_Key ' +
                'AND LN.Location_Name_Key = ''' + iLocationNameKey + ''' ';
    try
      Open;
      First;
      Result := FieldByName('Spatial_Ref').Text;
    finally
      Close;
    end;
  end;
end; // GetSiteCentroid
//==============================================================================
function TFinder.GetDob(const iNameKey: TKeyString): string;
  var
  lDOB: string;
begin
  with FDetailsQuery do begin
    if Active then Close;
    SQL.Text := 'SELECT [dbo].[ufn_GetDobandDod](''' + iNameKey + ''') as DOB';
    try
      Open;
      lDob :=  FieldByName('DOB').AsString ;
    finally
      Close;
    end;
  end;

  if lDob <>  ' - ' then
     Result:= lDob
  else
    Result :=  ResStr_NoDOb;

end;
//==============================================================================
function TFinder.GetAddress(const iNameKey: TKeyString): string;
var
  lAddressArray: array[1..6] of string;
  lFinalAddress: string;
  lCount: integer;
begin
  with FDetailsQuery do begin
    if Active then Close;
    SQL.Text := 'SELECT Address_1, Address_2, Address_3, Address_4, ' +
                '  Address_Country, Address_PostCode '+
                'FROM Address ' +
                'WHERE Name_Key = ''' + iNameKey + ''' ' +
                'AND Preferred = 1 ';
    try
      Open;
      First;
      for lCount:=1 to 4 do
        lAddressArray[lCount]:=FieldByName(('Address_'+intToStr(lcount))).AsString;
      lAddressArray[5]:=FieldByName('Address_PostCode').AsString;
      lAddressArray[6]:=FieldByName('Address_Country').AsString;
    finally
      Close;
    end;
  end;
  lFinalAddress:='';
  for lCount:=1 to 6 do begin
    if (lAddressArray[lCount]<>'') and not (lAddressArray[lCount]='         ') // Checks postcode is not empty!
    then
      lFinalAddress:=lFinalAddress + lAddressArray[lCount] + ' ';
  end;
  if lFinalAddress<>'' then Result:= lFinalAddress
                       else Result:=ResStr_NoAddressPresent;
end; // GetAddress

//==============================================================================
function TFinder.GetFullReference(const iReferenceKey: TKeyString): string;
var
  lCount                     :integer;
  lTitle,lFullRef,lRefDetails: string;
  lreTitle,lreFullRef        : TRichEdit;
begin
  lreTitle   := TRichEdit.Create(nil);
  lreFullRef := TRichEdit.Create(nil);
  lreTitle.Parent   := TWinControl(Self);
  lreFullRef.Parent := TWinControl(Self);
  lreTitle.Visible   := false;
  lreFullRef.Visible := false;

  with FDetailsQuery do begin
    if Active then Close;
    SQL.Text := 'SELECT Title, Full_Reference '+
                'FROM Reference '+
                'WHERE Source_Key = ''' + iReferenceKey + ''' ';
    try
      Open;
      First;
      lreTitle.Lines.Assign(FieldByName('Title'));
      lreFullRef.Lines.Assign(FieldByName('Full_Reference'));
    finally
      Close;
    end;
  end; // with qryAllPurpose

  with lreTitle do begin // Get Title text without RTF formatting
    lTitle:='';
    for lCount:=0 to Lines.Count do
      lTitle:=lTitle + ' ' + Lines[lCount]; //Build Title String
    Free;
  end;
  with lreFullRef do begin // Get Full Reference text without RTF formatting
    lFullRef:='';
    for lCount:=0 to Lines.Count do
     lFullRef:=lFullRef + ' ' + Lines[lCount]; // Build String
    Free;
  end;
  lRefDetails:=Copy(lFullRef,(Length(lTitle)+1),((Length(lFullRef))-Length(lTitle)));
  Result:=lRefDetails; // Assumes this is present (it is mandatory data)
end; // GetFullReference

//==============================================================================
{ Pass in a valid Taxon List Item Key to get the preferred name for it }
function TFinder.GetPreferredTaxonName(const AKey:TKeyString): string;
var lPrefNameKey : TKeyString;
begin
  Result:='';
  with FDetailsQuery do begin
    if Active then Close;
    { First query gets preferred name key of taxon key }
    SQL.Text := 'SELECT TLI.Preferred_Name ' +
                'FROM Taxon AS T INNER JOIN (Taxon_Version AS TV INNER JOIN Taxon_List_Item AS TLI ON ' +
                ' TV.Taxon_Version_Key = TLI.Taxon_Version_Key) ON T.Taxon_Key = TV.Taxon_Key ' +
                'WHERE TLI.Taxon_List_Item_Key = ''' + AKey + '''';
    try
      Open;
      if not Eof then lPrefNameKey:=FieldByName('Preferred_Name').AsString;
    finally
      Close;
    end;  // try..finally
    { Second query gets taxon name for the preferred name key }
    SQL.Text := 'SELECT T.Item_Name ' +
                'FROM Taxon AS T, Taxon_Version AS TV, Taxon_List_Item AS TLI ' +
                'WHERE T.Taxon_Key = TV.Taxon_Key ' +
                '  AND TV.Taxon_Version_Key = TLI.Taxon_Version_Key ' +
                '  AND TLI.Taxon_List_Item_Key = ''' + lPrefNameKey + '''';
    try
      Open;
      if not Eof then Result:=FieldByName('Item_Name').AsString;
    finally
      Close;
    end; // Try..Finally
  end;  // with FDetailsQuery
end;  // GetTaxonPreferredKey

//==============================================================================
procedure TFinder.SetDatabase(const ADatabase: TADOConnection);
begin
  FDetailsQuery.Connection := ADatabase;
end;  // SetDatabase

//==============================================================================
procedure TFinder.SetPartialTaxonSearch(const Value: Boolean);
begin
  FPartialTaxonSearch := Value;
end;

//==============================================================================
// Partial rewrite. Await confirmation before proceeding. Leave code in and commented for now
{
function TSourceItem.PartialTaxonSearchMatch(ASearchAnywhere: Boolean; APartials: TStrings;
  const ATextToCheck: String): Boolean;
var
  i: Integer;
  fulls: TStringList;

  function Check(offset: Integer; const part: String): Boolean;
  var
    j: Integer;
  begin
    Result := False;
    for j := offset to fulls.Count - 1 - offset do begin
      Result := Result or SameText(Copy(fulls, 1, Length(part)), part);
    end;
  end;

begin
  Result := Length(StringReplace(StringReplace(StringReplace(
      APartials.CommaText,
      ',', [rfReplaceAll]),  // <= get rid of comma introduced by CommaText...
      '*', [rfReplaceAll]),
      '.', [rfReplaceAll])) > 1;

  if Result then begin
    fulls := TStringList.Create;
    try
      fulls.Delimiter     := ' ';
      fulls.DelimitedText := ATextToCheck;

      // Searching on more parts than are available
      if fulls.Count < APartials.Count then
        Result := False
      else
        if not ASearchAnywhere then begin
          Result := SameText(Copy(fulls[0], 1, Length(APartials[0])), APartials[0]);

          if Result then
            for i := 1 to APartials.Count - 1 do begin
              Result := Result and Check(i, APartials[i]);
              if not Result then Break;
            end;
        end;
    finally
      fulls.Free;
    end;
  end;
end;
}
{-------------------------------------------------------------------------------
  VI 14642 - Checks whether ATextToCheck contains the genus contained in
  AGenusText and the species contained in ASpeciesText, using a wildcard match
  for both.  The species wildcard is placed at the start of the species if
  ASpeciesText begins with a '.', otherwise it is placed at the end.
}
function TSourceItem.PartialTaxonSearchMatch(const ASearchAnywhere: Boolean;
  const AGenusText, ATrinomialText, AQuadrinomialText, ASpeciesText, ATextToCheck: string): Boolean;
var
  genusToCheck, trinomialToCheck, quadrinomialToCheck, speciesToCheck,
  speciesText, trinomialText, quadrinomialText: String;
  spacePos, len: integer;
  matchEndOfSpecies, matchEndOfTrinomial, matchEndOfQuadrinomial: Boolean;
begin
  // Ensure that we have at least the minimum number of alpha characters
  Result := (Length(ATextToCheck) > 0)
    and (Length(StringReplace(AGenusText, '*', '', [rfReplaceAll])
    + StringReplace(ATrinomialText, '.', '', [rfReplaceAll])
    + StringReplace(AQuadrinomialText, '.', '', [rfReplaceAll])
    + StringReplace(ASpeciesText, '.', '', [rfReplaceAll])) > 1);

  if Result then begin
    // Initialise variables
    matchEndOfSpecies := (Length(ASpeciesText) > 0) and (ASpeciesText[1] = '.');
    if matchEndOfSpecies then
      speciesText := Copy(ASpeciesText, 2, Length(ASpeciesText))
    else
      speciesText := ASpeciesText;

    matchEndOfTrinomial := (Length(ATrinomialText) > 0) and (ATrinomialText[1] = '.');
    if matchEndOfTrinomial then
      trinomialText := Copy(ATrinomialText, 2, Length(ATrinomialText))
    else
      trinomialText := ATrinomialText;

    matchEndOfQuadrinomial := (Length(AQuadrinomialText) > 0) and (AQuadrinomialText[1] = '.');
    if matchEndOfQuadrinomial then
      quadrinomialText := Copy(AQuadrinomialText, 2, Length(AQuadrinomialText))
    else
      quadrinomialText := AQuadrinomialText;

    // Prepare the genus and species strings to be searched for matches
    spacePos := Pos(' ', ATextToCheck);
    if (spacePos = 0) then begin
      genusToCheck   := LowerCase(ATextToCheck);
      speciesToCheck := '';
    end else begin
      genusToCheck   := LowerCase(Copy(ATextToCheck, 1, spacePos - 1));
      speciesToCheck := LowerCase(Copy(ATextToCheck, spacePos + 1, Length(ATextToCheck)));
      // Extract the trinomial text to be searched
      spacePos := Pos(' ', speciesToCheck);
      if (spacePos > 0) then begin
        trinomialToCheck := Copy(speciesToCheck, 1, spacePos - 1);
        speciesToCheck := Copy(speciesToCheck, spacePos + 1, Length(speciesToCheck));
        if matchEndOfTrinomial then begin
          // Remove any commas from the end of trinomialToCheck
          len := Length(trinomialToCheck);
          if (len > 0) and (trinomialToCheck[len] = ',') then
            trinomialToCheck := Copy(trinomialToCheck, 1, len - 1);
        end;    // if matchEndOfTrinomial

        spacePos := Pos(' ', speciesToCheck);
        if (spacePos > 0) then begin
          quadrinomialToCheck := Copy(speciesToCheck, 1, spacePos - 1);
          speciesToCheck := Copy(speciesToCheck, spacePos + 1, Length(speciesToCheck));
          if matchEndOfQuadrinomial then begin
            // Remove any commas from the end of quadrinomialToCheck
            len := Length(quadrinomialToCheck);
            if (len > 0) and (quadrinomialToCheck[len] = ',') then
              quadrinomialToCheck := Copy(quadrinomialToCheck, 1, len - 1);
          end;    // if matchEndOfQuadrinomial
        end else
          quadrinomialToCheck := '';
      end else begin
        trinomialToCheck := '';
        quadrinomialToCheck := '';
      end;    // if (spacePos > 0)

      if matchEndOfSpecies then begin
        // Remove any subsequent words from speciesToCheck
        spacePos := Pos(' ', speciesToCheck);
        if (spacePos > 0) then
          speciesToCheck := Copy(speciesToCheck, 1, spacePos - 1);

        // Remove any commas from the end of speciesToCheck
        len := Length(speciesToCheck);
        if (len > 0) and (speciesToCheck[len] = ',') then
          speciesToCheck := Copy(speciesToCheck, 1, len - 1);
      end;    // if matchEndOfSpecies
    end;    // if (spacePos = 0)

    Result := PerformPartialTaxonSearchMatch(
        ASearchAnywhere,
        matchEndOfTrinomial, matchEndOfQuadrinomial, matchEndOfSpecies,
        AGenusText, trinomialText, quadrinomialText, speciesText,
        genusToCheck, trinomialToCheck, quadrinomialToCheck, speciesToCheck);
  end;    // if Result
end;

//==============================================================================
{ VI 14642 - Performs the actual check as to whether the genus and species
  portions are a partial match for the specified genus and species. }
function TSourceItem.PerformPartialTaxonSearchMatch(const ASearchAnywhere,
    AMatchEndOfTrinomial, AMatchEndOfQuadrinomial, AMatchEndOfSpecies: Boolean;
    const AGenusText, ATrinomialText, AQuadrinomialText, ASpeciesText,
    AGenusToCheck, ATrinomialToCheck, AQuadrinomialToCheck, ASpeciesToCheck: string): Boolean;
var
  len: Integer;

  function CheckToken(const ASearchText, AMatchText: String; AMatchEnd : Boolean): Boolean;
  begin
    Result := true;
    if (Length(ASearchText) > 0) or AMatchEnd then
      if (Length(AMatchText) > 0) then begin
        len := Length(ASearchText);
        if AMatchEnd then
          Result := (len = 0)
            or (Copy(AMatchText, Length(AMatchText) - len + 1, len) = ASearchText)
        else
          Result := Copy(AMatchText, 1, len) = ASearchText;
      end else begin
        Result := False;
      end;    // if (Length(AMatchText) > 0)
  end;

begin
  // Check genus
  len := Length(AGenusText);
  Result := (ASearchAnywhere and (Pos(AGenusText, AGenusToCheck) > 0)) or
            (Copy(AGenusToCheck, 1, len) = AGenusText);

  // Check trinomial
  if Result then
    Result := CheckToken(ATrinomialText, ATrinomialToCheck, AMatchEndOfTrinomial);

  // Check quadrinomial
  if Result then
    Result := CheckToken(AQuadrinomialText, AQuadrinomialToCheck, AMatchEndOfQuadrinomial);

  // Check species
  if Result then begin
    Result := CheckToken(ASpeciesText, ASpeciesToCheck, AMatchEndOfSpecies);
    // if the species doesn't match the 3rd token and the trinomial search
    // text is ommitted, then try the species search text against the trinomial
    if not Result and (ATrinomialText = '') then
      Result := CheckToken(ASpeciesText, ATrinomialToCheck, AMatchEndOfSpecies);

    if not Result and (AQuadrinomialText = '') then
      Result := CheckToken(ASpeciesText, AQuadrinomialToCheck, AMatchEndOfSpecies);
  end;
end;

//==============================================================================
{   Property accessor - determines the minimum number of characters required
    before carrying out a search. }
procedure TFinder.SetMinChars(const Value: integer);
begin
  FMinChars := Value;
end;

end.
