unit DropSource;

interface

uses
  Windows, ActiveX, ComObj, DataClasses, DropStruct, Controls, Grids;

type
  EDropSourceError = class(EDragDropError);

  { Forward declaration }
  TJNCCDropSource = class;

  TDataDraggedEvent = procedure(const Sender: TObject;
                          var oDropSource: TJNCCDropSource) of object;

  TAdvancedDataDraggedEvent = procedure(const Sender: TObject;
                          var oDropSource: TJNCCDropSource;
                          const oIsCopyOperation: Boolean ) of object;

  TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
  private
    FmtPtr : LongInt;
  public
    constructor Create;
    // IEnumFormatEtc interface
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enum: IEnumFormatEtc): HResult; stdcall;
  end;

  TJNCCDropSource = class(TInterfacedObject, IDataObject, IDropSource)
  private
    FControl          : TWinControl;
    FKeyList          : TDropData;
  protected
    {IDropSource}
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
    {IDataObject}
    function GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium):
      HResult; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium):
      HResult; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
      out FormatEtcOut: TFormatEtc): HResult; stdcall;
    function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
      IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const FormatEtc: TFormatEtc; advf: Longint;
      const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
    { other methods }
  public
    constructor Create( iControl : TWinControl );
    destructor Destroy; override;
    function GetText: string; // construct a string for the drop data
    procedure AssignKeyList( iKeyList : TKeyList );  // automatic assignment into DropData
    property DropData : TDropData read FKeyList;  // allow access to the key list
  end;

const
  Class_TJNCCDropSource: TGUID = '{A46DEFD1-EE65-11D2-B708-0060085B710F}';

//==============================================================================
implementation

uses
  DropTarget, {ComServ, }StdCtrls, Sysutils, ComCtrls, RapTree, DBGrids,
  DB, GeneralFunctions, JNCCGrid;

var
  FormatEtc : TFormatEtc;
  
resourcestring
  ResStr_WrongItemCount = 'Wrong number of items added to the drag structure.';

  ResStr_FormatSupportInConsistency =
            'Internal error in DropSource.GetData - supported formats inconsistent';

  ResStr_ControlNotSupported  = 'Control type not supported as text drag source :';
  { TJNCCDropSource }



//==============================================================================
constructor TJNCCDropSource.Create(iControl : TWinControl);
begin
  inherited Create;
  FControl := iControl; // so we can automatically grab text for CF_TEXT
  FKeyList := TDropData.Create;
end;  // Create

//==============================================================================
destructor TJNCCDropSource.Destroy;
begin
  FKeyList.Free;
  inherited Destroy;
end;  // Destroy

//==============================================================================
function TJNCCDropSource.GiveFeedback(dwEffect: Integer): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;  // GiveFeedback

//==============================================================================
function TJNCCDropSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: Integer): HResult;
begin
  Result := S_OK;
  // cancel drag on escape
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL;
  // commit drag on left mouse button up
  if (grfKeyState and MK_LBUTTON) <> MK_LBUTTON then
    Result := DRAGDROP_S_DROP;
end;  // QueryContinueDrag

//==============================================================================
function TJNCCDropSource.GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HResult;
var
  MemHandle   : THandle;
  lMemPointer : Pointer;
begin
  // look if format ok
  Result := QueryGetData(FormatEtcIn);
  if Failed(Result) then
    Exit;
  if FormatEtcIn.cfFormat = CF_JNCCDATA then begin
    try
      FKeyList.WriteToGlobalMemory(MemHandle);
      Medium.tymed  :=TYMED_HGLOBAL;
      Medium.hGlobal:=MemHandle;
      // receiver shall free memory
      medium.unkForRelease := nil;
    except
      Result := E_UNEXPECTED;
      GlobalFree(MemHandle);
    end;
  end
  else if (FormatEtcIn.cfFormat = CF_TEXT) then begin
    MemHandle := GlobalAlloc(GMEM_MOVEABLE, Length(GetText)+1);
    try
      lMemPointer := GlobalLock(MemHandle);
      StrPCopy(lMemPointer, GetText + #0);
      GlobalUnlock(MemHandle);
      Medium.tymed  :=TYMED_HGLOBAL;
      Medium.hGlobal:=MemHandle;
      // receiver shall free memory
      Medium.unkForRelease := nil;
    except
      Result := E_UNEXPECTED;
      GlobalFree(MemHandle);
    end;
  end else
    raise EDropSourceError.Create(ResStr_FormatSupportInConsistency);
end;  // GetData


//==============================================================================
{ Read the plain text from the control for the clipboard.  Controls supported are
       TCustomEdit + descendants, TListBox, TCustomTreeView, TCustomRapidTree,
       TStringGrid, TListView }
function TJNCCDropSource.GetText: string;
var
  i           : integer;
  liColumn    : integer;
  lTempString : string;
  lListItem   : TListItem;
  ltfControlsDisabled : boolean;
  lBookMark : TBookMarkStr;
  function CopyRow (ADBGrid :TCustomDBGrid) : String;
  var i : integer;
  begin
    Result := '';
    for i := 0 to ADBGrid.FieldCount -1 do
      With ADBGrid.Fields[i] do
        if Visible  and not IsBlob then
          Result := Result + '"' + DuplicateCharacters(StringReplace(AsString, #9, ' ', [rfReplaceAll]), '"') + '"'#9;
    if Result <> '' then
          // lop off last tab
      Result := Copy(Result, 1, Length(Result)-1);
  end;

begin
  if FControl = nil then
    Result := ''
  else
  if FControl is TCustomEdit then
   with TCustomEdit(FControl) do begin
     { Export selected data if a selction is available }
     if Length(SelText) > 0 then
       Result := SelText
     else
       Result := Text;
    end
  else if FControl is TListBox then
    with TListBox(FControl) do begin
      if MultiSelect then begin
        lTempString := '';
        for i := 0 to Items.Count-1 do
          if Selected[i] then
            lTempString := lTempString + Items[i] + #13;
        Result := lTempString;
      end else
        Result := Items[ItemIndex];
    end
  else if FControl is TCustomTreeView then
    with TCustomTreeView(FControl) do begin
      if  Selected <> nil then
        Result := Selected.Text
      else
        Result := ''
    end
  else if FControl is TCustomRapidTree then
    with TCustomRapidTree(FControl) do begin
      if  Selected <> nil then
        Result := Selected.Text
      else
        Result := ''
    end
  else if FControl is TStringGrid then
    with TStringGrid(FControl) do begin
      if Selection.Top >= 1 then begin
        lTempString := '';
        for i:=Selection.Top to Selection.Bottom do begin
          for liColumn:= Selection.Left to Selection.Right do
            lTempString:= lTempString + Cells[liColumn,i] + #9;
          lTempString:= Copy(lTempString,0,Length(lTempString)-1) + #13#10;
        end;
        Result := lTempString;
      end else
        Result := ''
    end
  else if FControl is TListView then
    with TListView(FControl) do begin
      if MultiSelect and (SelCount>0) then begin
        // Get the first selected item
        Result:=Selected.Caption;
        // find the next one, if any
        lListItem:=GetNextItem(Selected,sdAll,[isSelected]);
        // and carry on to the end of the selection
        while lListItem<>nil do begin
          Result:=Result+#1310+lListItem.Caption;
          lListItem:=GetNextItem(lListItem,sdAll,[isSelected]);
        end;
      end else
        Result:=ItemFocused.Caption;
    end
  else if FControl is TDBJNCCGrid then
    with TDBJNCCGrid(FControl) do
      if not (dgRowSelect in Options) then
        result := SelectedField.AsString
      else
      begin
        if not (dgMultiSelect in Options) then
          Result := CopyRow(TDBJNCCGrid(FControl))
        else
        begin
          ltfControlsDisabled := Datasource.DataSet.ControlsDisabled;

          lBookMark := Datasource.DataSet.Bookmark;
          Datasource.DataSet.DisableControls;
          try
            for i := 0 to SelectedRows.Count -1 do
            begin
              Datasource.DataSet.Bookmark := SelectedRows[i];
              Result := Result + CopyRow(TCustomDBGrid(FControl)) + #13#10;
            end;
            if Result <> '' then
            // lop off last line break
              Result := Copy(Result, 1, Length(Result)-2);
          finally
            Datasource.DataSet.Bookmark := lBookmark;
            if not ltfControlsDisabled then Datasource.DataSet.EnableControls;
          end;
        end;
      end
  else
    raise EDropSourceError.Create(ResStr_ControlNotSupported + FControl.Classname);
end;  // GetText

//==============================================================================
{ Create a new DropData instance and assign the key list into it one item at
     a time }
procedure TJNCCDropSource.AssignKeyList(iKeyList: TKeyList);
var i : integer;
begin
  // NB. Don't use 'With' for FKeyList because the free followed by the create
  // means it sometimes points to the destroyed one, rather than the new one.
  FKeyList.Free;
  FKeyList := TDropData.Create;
  FKeyList.SetTable(iKeyList.Header.TableName);
  for i := 0 to iKeyList.Header.ItemCount-1 do
    FKeyList.AddItem(iKeyList.Items[i].KeyField1, iKeyList.Items[i].KeyField2);
end;  // AssignKeyList

//==============================================================================
function TJNCCDropSource.GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := E_NOTIMPL;
end;  // GetDataHere

//==============================================================================
{ Check if the format requested is one of our supported formats }
function TJNCCDropSource.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  { Note - we need to check other contents of FormatEtc here as well }
  if (FormatEtc.cfFormat = CF_JNCCDATA) or (FormatEtc.cfFormat = CF_TEXT) then
    Result := S_OK
  else
    Result := DV_E_FORMATETC;
end;  // QueryGetData

//==============================================================================
function TJNCCDropSource.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
  out FormatEtcOut: TFormatEtc): HResult;
begin
  Result := E_NOTIMPL;
end;  // GetCanonicalFormatEtc

//==============================================================================
function TJNCCDropSource.SetData(const FormatEtc: TFormatEtc;
  var Medium: TStgMedium; fRelease: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;  // SetData

//==============================================================================
function TJNCCDropSource.EnumFormatEtc(dwDirection: Longint;
  out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  if dwDirection = DATADIR_SET then
  begin
    Result := E_NOTIMPL;
    Exit;
  end;
  enumFormatEtc := TEnumFormatEtc.Create;
  if enumFormatEtc = nil
  then
    Result := E_OUTOFMEMORY
  else
    Result := S_OK;
end;  // EnumFormatEtc

//==============================================================================
function TJNCCDropSource.DAdvise(const FormatEtc: TFormatEtc; advf: Longint;
  const advSink: IAdviseSink; out dwConnection: Longint): HResult;
begin
  Result := E_NOTIMPL;
end;  // DAdvise

//==============================================================================
function TJNCCDropSource.DUnadvise(dwConnection: Longint): HResult;
begin
  Result := E_NOTIMPL;
end;  // DUnadvise

//==============================================================================
function TJNCCDropSource.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := E_NOTIMPL;
end;  // EnumAdvise

//==============================================================================
//==============================================================================
constructor TEnumFormatEtc.Create;
begin
  inherited Create;
  Reset;
end;  // Create

//==============================================================================
function TEnumFormatEtc.Next(celt: Longint; out elt; pceltFetched: PLongint): HResult;
begin
  Result :=  S_FALSE;
  // all out ?
  if FmtPtr = 1
  then
    Exit;
  Pointer(elt) := @FormatEtc;
  Inc(FmtPtr);
  if pceltFetched <> nil
  then
    pceltFetched^ := 1;
  if celt = 1
  then
    Result := S_OK;
end;  // Next

//==============================================================================
function TEnumFormatEtc.Skip(celt: Longint): HResult;
begin
  if FmtPtr + celt > 1 then
  begin
    Result :=  S_FALSE;
    Exit;
  end;
  FmtPtr := FmtPtr + celt;
  Result := S_OK;
end;  // Skip

//==============================================================================
function TEnumFormatEtc.Reset: HResult;
begin
  FmtPtr := 1;
  Result := S_OK;
end;  // Reset

//==============================================================================
function TEnumFormatEtc.Clone(out enum: IEnumFormatEtc): HResult;
var
  NewEnum : TEnumFormatEtc;
begin
  // create object
  NewEnum := TEnumFormatEtc.Create;
  if NewEnum = nil then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;
  // clone current state
  NewEnum.FmtPtr := FmtPtr;
  enum := NewEnum;
  Result := S_OK;
end;  // Clone

//==============================================================================
end.
