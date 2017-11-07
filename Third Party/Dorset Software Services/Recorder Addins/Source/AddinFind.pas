{===============================================================================
  Unit:        Find

  Defines:     TdlgAddinFind

  Description: General purpose Find dialog for use in Addins.

  Model:       Addins.mpb

  Created:     April 2004

  Last revision information:
    $Revision: 6 $
    $Date: 8/04/10 17:36 $
    $Author: Andrewkemp $

===============================================================================}
unit AddinFind;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AddinInterfaceDataModule, AddinGeneralData, StdCtrls, ImageListButton,
  ExtCtrls, AddinResourceStrings, ADODb, ComboListID, ExceptionForm, ComCtrls,
  AddinConstants, ADOInt, Registry;

type
  EFindException = class(TExceptionPath)
  end;

  {-----------------------------------------------------------------------------
    Find dialog used by addins.  The dialog is invoked whenever find functionality is
    required by an addin.  The list of data items searched is configured specifically for
    each type of search run.
    Search query operations are run asynchronously, so that the user is always able to type
    text in to the Find box and the dialog remains 'responsive' at all times.
  }
  TdlgAddinFind = class(TForm)
    Animation: TAnimate;
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    bvlFrame: TBevel;
    eSearchText: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbMatches: TIDListBox;
    procedure btnOkClick(Sender: TObject);
    procedure eSearchTextChange(Sender: TObject);
    procedure eSearchTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lbMatchesClick(Sender: TObject);
    procedure lbMatchesDblClick(Sender: TObject);
    procedure lbMatchesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
        State: TOwnerDrawState);
    procedure lbMatchesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCurrentSearchTerm: String;
    FLastSearchPopulated: String;
    FParameters: array of Variant;
    FPopulating: Boolean;
    FCurrentProcessingText: string;
    FPendingDataset: _Recordset;
    FProcessingUserInput: Boolean;
    FRequestStopPopulating: Boolean;
    FResolveDuplicateStoredProcName: String;
    FResultKey: String;
    FResultText: String;
    FSearchComponents: TStringList;
    FStoredProcName: String;
    FTitle: String;
    FLocationSearch: Boolean;
    FExtraLocationSearchColumns: TLocationSearchColumns;
    FSpatialSystem: String;
    procedure AddToListBox(const ADisplayTerm, AItemKey: string);
    procedure ClearListBox;
    procedure CloseDialogOk;
    procedure DisplaySearchAnimation(ASearching: boolean);
    procedure PopulateSearchList(ARecordset: _Recordset; const ASearchParam: String);
    function PopulationRequired(const ASearchParam: String): Boolean;
    procedure ReadExtraLocationSearchInfo;
    procedure SearchCallback(ATarget: TObject; ADataset: TCustomADODataset);
    procedure SetResolveDuplicateStoredProcName(const Value: String);
    procedure SetStoredProcName(const Value: String);
    procedure SetTitle(const Value: String);
    procedure StopPopulating;
  protected
    procedure WMDisplayPendingDataset(var Message: TMessage); message WM_DISPLAYPENDINGDATASET;
    procedure WMRefreshSearchList(var Message: TMessage); message WM_REFRESHSEARCHLIST;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddSearchComponent(AFieldName: String);
    procedure InitialiseSearch(ARecordset: _Recordset; const ASearchParam: String);
    procedure SetStoredProcParameters(AParameters: array of Variant);
    procedure StartSearch(const ASearchText: String);
    property LocationSearch: Boolean read FLocationSearch write FLocationSearch;
    property ResolveDuplicateStoredProcName: String read FResolveDuplicateStoredProcName write
        SetResolveDuplicateStoredProcName;
    property ResultKey: String read FResultKey;
    property ResultText: String read FResultText;
    property StoredProcName: String read FStoredProcName write SetStoredProcName;
    property Title: String read FTitle write SetTitle;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  ProjectSpecificAccess, SpatialRefFuncs;

{-==============================================================================
    TdlgAddinFind
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation. 
}
constructor TdlgAddinFind.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FSearchComponents      := TStringList.Create;
  FLastSearchPopulated   := '';
  FPopulating            := False;
  FRequestStopPopulating := False;
  btnOk.ImageList        := dmInterface.ilButtons;
  btnCancel.ImageList    := dmInterface.ilButtons;
  FProcessingUserInput   := False;

  // Read settings for Location extra data.
  ReadExtraLocationSearchInfo;
end;  // TdlgAddinFind.Create

{-------------------------------------------------------------------------------
  Reads the selection of extra location search columns from the registry.
}
procedure TdlgAddinFind.ReadExtraLocationSearchInfo;
var
  reg: TRegistry;
begin
  FExtraLocationSearchColumns := [];

  reg := TRegistry.Create;
  try
    if reg.OpenKeyReadOnly(REG_KEY_LOCATION_SEARCH_COLS) then
    begin
      if reg.ReadBool(REG_VALUE_LOCATION_TYPE) then
        Include(FExtraLocationSearchColumns, lscLocationType);
      if reg.ReadBool(REG_VALUE_LOCATION_SPATIAL_REF) then
        Include(FExtraLocationSearchColumns, lscSpatialReference);
      if reg.ReadBool(REG_VALUE_LOCATION_FILE_CODE) then
        Include(FExtraLocationSearchColumns, lscFileCode);
    end;
    if reg.OpenKeyReadOnly(REG_KEY_SETTINGS) then
      FSpatialSystem := reg.ReadString(REG_VALUE_SPATIAL_REF_SYSTEM);
  finally
    reg.Free;
  end;
end;  // ReadExtraLocationSearchInfo

{-------------------------------------------------------------------------------
}
destructor TdlgAddinFind.Destroy;
begin
  FSearchComponents.Free;
  
  inherited Destroy;
end;  // TdlgAddinFind.Destroy 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.AddSearchComponent(AFieldName: String);
begin
  FSearchComponents.Add(AFieldName);
end;  // TdlgAddinFind.AddSearchComponent

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.AddToListBox(const ADisplayTerm, AItemKey: String);
var
  lDuplicateIdx: Integer;
  lTopIndex: Integer;
begin
  with lbMatches do begin
    // Lock the window, since adding an item tends to scroll the list box
    // and we reset the top index to fix this, but we don't want flicked
    LockWindowUpdate(Handle);
    try
      lTopIndex := TopIndex;
      if ResolveDuplicateStoredProcName = '' then
        lDuplicateIdx := -1
      else
        lDuplicateIdx := IndexOf(ADisplayTerm);

      if lDuplicateIdx = -1 then
        Add(ADisplayTerm, AItemKey)
      else begin
        // We have a duplicate, so first ensure the new term we add is fully specified
        Add(dmGeneral.GetStoredProcOutputParam(ResolveDuplicateStoredProcName,
                                               ['@ItemKey', AItemKey], '@Output'), AItemKey);
        // And also the original term that was already on the list
        Items[lDuplicateIdx] := dmGeneral.GetStoredProcOutputParam(
                                               ResolveDuplicateStoredProcName,
                                               ['@ItemKey', StrID[lDuplicateIdx]], '@Output');
        TopIndex := lTopIndex;
      end;
    finally
      LockWindowUpdate(0);
    end; // try
  end;
end;  // TdlgAddinFind.AddToListBox 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.btnOkClick(Sender: TObject);
begin
  FResultKey  := lbMatches.CurrentStrID;
  FResultText := lbMatches.CurrentItem;
end;  // TdlgAddinFind.btnOkClick 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.ClearListBox;
begin
  lbMatches.Clear;
end;  // TdlgAddinFind.ClearListBox 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.CloseDialogOk;
begin
  btnOkClick(nil);
  ModalResult := mrOk;
end;  // TdlgAddinFind.CloseDialogOk 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.DisplaySearchAnimation(ASearching: boolean);
begin
  if ASearching then
    Screen.Cursor := crAppStart
  else begin
    Screen.Cursor := crDefault;
    dmGeneral.CancelAsyncCommands(SearchCallback);
  end;
  Animation.Active  := ASearching;
  Animation.Visible := ASearching;
end;  // TdlgAddinFind.DisplaySearchAnimation 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.eSearchTextChange(Sender: TObject);
begin
  // delay processing until all user input received, so we don't run unnecessary
  // queries
  PostMessage(Self.Handle, WM_REFRESHSEARCHLIST, 0, 0);
end;  // TdlgAddinFind.eSearchTextChange

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.eSearchTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lKeyData: Word;
begin
  // If navigation key pressed, send a fake KEYDOWN message to the listbox,
  // so it can deal with it itself.
  if Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT] then begin
    lKeyData := 0;
    if ssShift in Shift then lKeyData := lKeyData  + VK_SHIFT;
    if ssCtrl in Shift then lKeyData := lKeyData + VK_CONTROL;
    SendMessage(lbMatches.Handle, WM_KEYDOWN, Key, lKeyData);
    Key := 0;
  end else
  if (Key = VK_RETURN) and (btnOk.Enabled) then
    CloseDialogOk;
end;  // TdlgAddinFind.eSearchTextKeyDown 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.FormActivate(Sender: TObject);
begin
  eSearchText.SelStart := Length(eSearchText.Text);
end;  // TdlgAddinFind.FormActivate 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dmGeneral.CancelAsyncCommands(SearchCallback);
end;  // TdlgAddinFind.FormClose 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FPopulating then
    StopPopulating;
end;  // TdlgAddinFind.FormCloseQuery 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.FormResize(Sender: TObject);
begin
  lbMatches.Invalidate;
end;  // TdlgAddinFind.FormResize

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.lbMatchesClick(Sender: TObject);
begin
  btnOk.Enabled := lbMatches.ItemIndex <> -1;
end;  // TdlgAddinFind.lbMatchesClick

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.lbMatchesDblClick(Sender: TObject);
begin
  CloseDialogOk;
end;  // TdlgAddinFind.lbMatchesDblClick 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.lbMatchesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
    State: TOwnerDrawState);
begin
  dmInterface.DrawTerm(lbMatches.Canvas, Rect, lbMatches.Items[Index], odSelected in State);
end;  // TdlgAddinFind.lbMatchesDrawItem 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.lbMatchesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (btnOk.Enabled) then
    CloseDialogOk;
end;  // TdlgAddinFind.lbMatchesKeyDown 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.InitialiseSearch(ARecordset: _Recordset; const ASearchParam: String);
begin
  FCurrentSearchTerm   := ASearchParam;
  FLastSearchPopulated := ASearchParam;
  FPendingDataset      := ARecordset;
  eSearchText.Text     := ASearchParam;
end;  // TdlgAddinFind.PopulateFromExistingRecordset 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.PopulateSearchList(ARecordset: _Recordset; const ASearchParam: String);
var
  displayTerm: String;

  {-----------------------------------------------------------------------------
    Text that should be displayed for the current location.
  }
  function LocationText: String;
  var
    extras: String;

    {---------------------------------------------------------------------------
      Add an extra term to the text for the location.
    }
    procedure AddTerm(Term: String);
    begin
      if Term <> '' then
      begin
        if extras <> '' then extras := extras + '; ';
        extras := extras + Term;
      end;
    end;

  begin
    extras := '';
    if lscLocationType in FExtraLocationSearchColumns then
      AddTerm(ARecordset.Fields['Location_Type'].Value);
    if lscSpatialReference in FExtraLocationSearchColumns then
      AddTerm(LocaliseSpatialRef(GetDisplaySpatialRef(
          FSpatialSystem,
          VarToStr(ARecordset.Fields['Spatial_Ref'].Value),
          VarToStr(ARecordset.Fields['Spatial_Ref_System'].Value),
          VarToStr(ARecordset.Fields['Lat'].Value),
          VarToStr(ARecordset.Fields['Long'].Value),
          '')));
    if lscFileCode in FExtraLocationSearchColumns then
      AddTerm(VarToStr(ARecordset.Fields['File_Code'].Value));

    Result := ARecordset.Fields['DisplayTerm'].Value;
    if extras <> '' then Result := Result + ' (' + extras + ')';
  end;

begin
  FPopulating := True;
  try
    FLastSearchPopulated := ASearchParam;
    with ARecordset do
      while not Eof do
      begin
        if FRequestStopPopulating then
          Break;

        displayTerm := Fields['DisplayTerm'].Value;

        if FLocationSearch and (FExtraLocationSearchColumns <> []) then
          displayTerm := LocationText;

        // If another search starts whilst this one is running, then stop
        if FCurrentSearchTerm <> ASearchParam then
          Break
        else begin
          FLastSearchPopulated := ASearchParam;
          AddToListBox(displayTerm, VarToStr(Fields['Item_Key'].Value));
          // if adding first item to list, auto-select it
          if lbMatches.ItemIndex = -1 then begin
            lbMatches.ItemIndex := 0;
            lbMatchesClick(nil);
          end;
          if lbMatches.Count mod 500 = 0 then
            Application.ProcessMessages;
          MoveNext;
        end;
      end;
  finally
    FPopulating := False;
  end; // try
end;  // TdlgAddinFind.PopulateSearchList 

{-------------------------------------------------------------------------------
}
function TdlgAddinFind.PopulationRequired(const ASearchParam: String): Boolean;
var
  lLastSearchMatch: Integer;
  lThisSearchMatch: Integer;
  lCurrentText: String;
begin
  // FLastSearchPopulated and ASearchParam have the '*' converted to '%' already.
  // So if we need to do the same for current text, or it won't work.
  lCurrentText := StringReplace(eSearchText.Text, '*', '%', [rfReplaceAll]);

  // Don't search if nothing to do (i.e. this is an old recordset returning)
  // Find out how many characters of the current required search we have already done
  if (FLastSearchPopulated = Copy(lCurrentText, 1, Length(FLastSearchPopulated)))
     and (Length(lCurrentText) >= Length(FLastSearchPopulated)) then
    lLastSearchMatch := Length(FLastSearchPopulated)
  else
    lLastSearchMatch := 0;

  if ASearchParam = Copy(lCurrentText, 1, Length(ASearchParam)) then
    lThisSearchMatch := Length(ASearchParam)
  else
    lThisSearchMatch := 0;

  Result := lThisSearchMatch > lLastSearchMatch;
end;  // TdlgAddinFind.PopulationRequired

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.SearchCallback(ATarget: TObject; ADataset: TCustomADODataset);
var
  lThisSearchParam: String;
begin
  if not (ADataset is TADODataset) then
    raise EFindException.Create(Format(ResStr_InvalidMethodCall, ['TdlgFind.SearchCallback']));

  lThisSearchParam :=
      VarToStr(TADODataset(ADataset).Parameters.ParamByName('@SearchText').Value);
  // don't populate unless this is an improvement on what we already have
  if PopulationRequired(lThisSearchParam) then
  begin
    FCurrentSearchTerm := lThisSearchParam;
    StopPopulating;
    ClearListBox;
    PopulateSearchList(ADataset.Recordset, lThisSearchParam);
    // Cancel other commands if this one is up to date with the current search text
    if lThisSearchParam=eSearchText.Text then
      DisplaySearchAnimation(False);
  end;
end;  // TdlgAddinFind.SearchCallback 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.SetResolveDuplicateStoredProcName(const Value: String);
begin
  FResolveDuplicateStoredProcName := Value;
end;  // TdlgAddinFind.SetResolveDuplicateStoredProcName 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.SetStoredProcName(const Value: String);
begin
  FStoredProcName := Value;
end;  // TdlgAddinFind.SetStoredProcName 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.SetStoredProcParameters(AParameters: array of Variant);
var
  lIdx: Integer;
begin
  SetLength(FParameters, Length(AParameters) + 2);
  for lIdx := 0 to High(AParameters) do
    FParameters[lIdx] := AParameters[lIdx];
  // set up parameter for search text
  FParameters[High(FParameters) - 1] := '@SearchText';
end;  // TdlgAddinFind.SetStoredProcParameters 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.SetTitle(const Value: String);
begin
  FTitle := Value;
  
  Caption := ResStr_Find + ' ' + Value;
end;  // TdlgAddinFind.SetTitle 

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.StartSearch(const ASearchText: String);
begin
  eSearchText.Text := ASearchText;
end;  // TdlgAddinFind.StartSearch

{-------------------------------------------------------------------------------
}
procedure TdlgAddinFind.StopPopulating;
begin
  FRequestStopPopulating := True;
  while FPopulating do
    Application.ProcessMessages;
  FRequestStopPopulating := False;
end;  // TdlgAddinFind.StopPopulating

{-------------------------------------------------------------------------------
  Handle search input, refresh the search list
}
procedure TdlgAddinFind.WMRefreshSearchList(var Message: TMessage);
begin
  if FProcessingUserInput then begin
    PostMessage(Self.Handle, WM_REFRESHSEARCHLIST, 0, 0);
    Exit;
  end;

  if FCurrentProcessingText = eSearchText.Text then
    Exit;

  FCurrentProcessingText := eSearchText.Text;
  FProcessingUserInput := True;
  try
    // Only allow searching on something, even if it is '*'
    if eSearchText.Text = '' then begin
      DisplaySearchAnimation(False);
      StopPopulating;
      ClearListBox;
      FCurrentSearchTerm   := '';
      FLastSearchPopulated := '';
      Exit;
    end;

    dmGeneral.CancelAsyncCommands(SearchCallback);

    if eSearchText.Text <> FCurrentSearchTerm then
    begin
      // put parameter in for search text
      FParameters[High(FParameters)] :=
          StringReplace(eSearchText.Text, '*', '%', [rfReplaceAll]);
      dmGeneral.GetAsyncRecordset(FStoredProcName, FParameters, lbMatches, SearchCallback);
      DisplaySearchAnimation(True);
    end; // if
  finally
    FProcessingUserInput := False;
  end;
end;

{-------------------------------------------------------------------------------
  When first displayed, if we have a search already done, then display the
    records
}
procedure TdlgAddinFind.FormShow(Sender: TObject);
begin
  PostMessage(Self.Handle, WM_DISPLAYPENDINGDATASET, 0, 0);
end;

{-------------------------------------------------------------------------------
  Delay this till after the form is displayed
}
procedure TdlgAddinFind.WMDisplayPendingDataset(var Message: TMessage);
begin
  if Assigned(FPendingDataset) then
    if FPendingDataset.RecordCount > 0 then
    begin
      FPendingDataset.MoveFirst;
      PopulateSearchList(FPendingDataset, FLastSearchPopulated);
    end;
  FPendingDataset := nil;
end;

end.


