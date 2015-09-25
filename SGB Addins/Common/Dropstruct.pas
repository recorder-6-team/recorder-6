{ dropstruct unit
Defines drage drop/clipboard structures and helper objects for JNCC
By John van Breda 12/4/99 }

unit dropstruct;

interface

uses
  Windows, Sysutils, ActiveX, AddinClasses, Classes, Controls, ShellAPI;

type
  EDragDropError = class(Exception);  // base class exception for drag-drop stuff

  { Class to pass lists of key values for a table from global memory }
  TDropData = class(TEditableKeyList)
  public
    procedure ReadFromGlobalMemory( iGlobalMem : HGlobal );
    procedure WriteToGlobalMemory(var ioMemHandle: HGlobal);
  end;


  TDataDroppedEvent = procedure(const Sender: TObject;
          const iFormat : integer; const iSourceData: TKeyList;
          const iTextStrings : TstringList; var ioHandled : boolean) of object;

  { object to store clipboard capabilities for a component }
  TClipboardCapability = class(Tobject)
  private
    FTableArray : array of string;
    FClipboardFormats : array of integer;
    FDropEvent : TDataDroppedEvent;
    function GetCountOfFormats: integer;
    function GetCountOfTables: integer;
    function GetFormat(Index: integer): integer;
    function GetTableName(Index: integer): string;
  protected
    function AddToControl(iControl: TControl;
             const iTextStrings: TStringList): boolean;
  public
    constructor Create( iTableArray : Array of String;
                        iClipboardFormats : Array of Integer;
                        iDropEvent : TDataDroppedEvent );
    destructor Destroy; override;
    function IsTableSupported( iTable : string ): boolean;
    function IsFormatSupported( iFormat : integer ): boolean;
    function GetBestFormat( iDataObj : IDataObject;
                         var iStorage : TStgMedium ): integer;
    procedure ActOnGlobalMemory( ihGlobal : THandle; iFormat : integer;
              iControl : TControl );

    {SGB 24/6/00}
    procedure AddTable(iTable: string);
    procedure RemoveTable(iTable: string);

    property DropEvent : TDataDroppedEvent read FDropEvent;
    property TableCount : integer read GetCountOfTables;
    property FormatCount : integer read GetCountOfFormats;
    property TableName[Index: integer] : string read GetTableName;
    property Format[Index: integer]: integer read GetFormat;
  end;


implementation

uses DropTarget, stdctrls, comctrls;


{ TDropData }

{ Extract the data from a global memory area }
procedure TDropData.ReadFromGlobalMemory(iGlobalMem: HGlobal);
var
  StructPtr : Pointer;
  i         : integer;
begin
  { Get a lock on the global memory }
  StructPtr := GlobalLock(iGlobalMem);
  FHeader.TableName := TStructHeader(StructPtr^).TableName;
  FHeader.ItemCount := TStructHeader(StructPtr^).ItemCount;
  { Resize a dynamic array }
  SetLength(FItems, FHeader.ItemCount);
  { Locate the first item }
  StructPtr := Ptr(Integer(StructPtr) + SizeOf(TStructHeader));
  { Loop through each item }
  for i := 0 to FHeader.ItemCount-1 do
  begin
    FItems[i].KeyField1 := TStructItem(StructPtr^).KeyField1;
    FItems[i].KeyField2 := TStructItem(StructPtr^).KeyField2;
    { Locate the next item }
    StructPtr := Ptr(Integer(StructPtr) + SizeOf(TStructItem));
  end;
  { Release the memory }
  GlobalUnlock(iGlobalMem);
end;


{ Extract the data from a global memory area }
procedure TDropData.WriteToGlobalMemory( var ioMemHandle: HGlobal);
var
  lMemPointer : Pointer;
  i           : integer;
begin
  { Allocate global memory space }
  ioMemHandle := GlobalAlloc(GMEM_MOVEABLE,
                             sizeof(TStructHeader) +
                             Header.ItemCount * sizeof(TStructItem));
  { Lock the space to us }
  lMemPointer := GlobalLock(ioMemHandle);
  { Setup the header }
  TStructHeader(lMemPointer^).ItemCount := Header.ItemCount;
  TStructHeader(lMemPointer^).TableName := Header.TableName;
  { Advanve the pointer to the first item }
  lMemPointer := Ptr(Integer(lMemPointer) + sizeof(TStructHeader));
  { loop and add the items }
  for i := 0 to Header.ItemCount-1 do
  begin
    TStructItem(lMemPointer^).KeyField1 := Items[i].KeyField1;
    TStructItem(lMemPointer^).KeyField2 := Items[i].KeyField2;
    { Advance the pointer to the next item }
    lMemPointer := Ptr(Integer(lMemPointer) + sizeof(TStructItem));
  end;
  { Unlock the space so it can be accessed elsewhere }
  GlobalUnlock(ioMemHandle);
end;



{ TClipboardCapability }


//==============================================================================
procedure TClipboardCapability.ActOnGlobalMemory(ihGlobal: THandle;
          iFormat : integer; iControl : TControl);
var
  liFileCount, i : integer;
  lTextStrings    : TStringList;
  lstText : string;
  lHandled : boolean;
  lDropData : TDropData;
  lStructPtr : Pointer; // to global memory
begin
  if not Assigned(DropEvent) then
    Exit;
  lDropData := TDropData.Create;
  lTextStrings := TStringList.Create;
  try
    if (iFormat = CF_JNCCDATA) then
        lDropData.ReadFromGlobalMemory(ihGlobal)
    else
      if (iFormat = CF_TEXT) then
      begin  // other formats for custom edits (text etc)
        lStructPtr := GlobalLock(ihGlobal);
        lstText := StrPas(lStructPtr);
        lTextStrings.Add(lstText);
        GlobalUnlock(ihGlobal);
      end
      else
        if (iFormat = CF_HDROP) then // dropping a file name
        begin
          {get number of files back from the data - asking for FFFFFFFF gets count}
          liFileCount:=DragQueryFile(ihGlobal,$FFFFFFFF,nil,0);
          {get each file back out of data}
          SetLength(lstText, 255);
          for i:=0 to liFileCount-1 do
          begin
            DragQueryFile(ihGlobal,i,PChar(lstText),255);
            { note we must reduce string length to appropriate size }
            lTextStrings.Add(Copy(lstText, 1, Pos(#0, lstText)- 1));
          end;
        end; // if format = CF_HDROP
    lHandled := False;
    DropEvent(Self, iFormat, lDropData, lTextStrings, lHandled);
    if not lHandled then
      AddToControl( iControl, lTextStrings );
  finally
    lDropData.Free;
    lTextStrings.Free;
  end; // try.. finally
end;


//==============================================================================
function TClipboardCapability.AddToControl(iControl: TControl;
           const iTextStrings: TStringList): boolean;
var
  i : integer;
begin
  Result := true; //Doesn't appear to be used so, be positive! GAD.
  if (iControl is TCustomEdit) and (iTextStrings.Count > 0) then
  begin
    { If some text selected, replace it otherwise replace all text }
    { note we only bother with the first string }
    if Length(TCustomEdit(iControl).SelText) > 0 then
      TCustomEdit(iControl).SelText := iTextStrings[0]
    else
      TCustomEdit(iControl).Text := iTextStrings[0];
  end
  else
    if iControl is TCustomListBox then
    begin
      { Add every string }
      for i := 0 to iTextStrings.Count-1 do
        TCustomListBox(iControl).Items.Add( iTextStrings[i] );
    end; // if iControl
end;


//==============================================================================
constructor TClipboardCapability.Create( iTableArray : Array of String;
                        iClipboardFormats : Array of Integer;
                        iDropEvent : TDataDroppedEvent );
var i : integer;
begin
  inherited Create;
  // Copy the arrays for local storage
  SetLength(FTableArray, High(iTableArray) + 1);
  for i := 0 to High(FTableArray) do
    FTableArray[i] := iTableArray[i];
  SetLength(FClipboardFormats, High(iClipboardFormats) + 1);
  for i := 0 to High(FClipboardFormats) do
    FClipboardFormats[i] := iClipboardFormats[i];
  { Record event to be called when JNCC data is dropped }
  FDropEvent := iDropEvent;
end;


//==============================================================================
destructor TClipboardCapability.Destroy;
begin
  inherited;
end;


//==============================================================================
{ Find the best format out of those available in a data object }
function TClipboardCapability.GetBestFormat(iDataObj: IDataObject;
                                       var iStorage : TStgMedium ): integer;
var
  loFormat     : TFormatEtc;
  i : integer;
begin
  i := 0;
  FillChar(loFormat,sizeOf(TFormatEtc),#0);
  loFormat.tymed:=TYMED_HGLOBAL;
  while i <= High(FClipboardFormats) do
  begin
    loFormat.cfFormat:=FClipboardFormats[i];
    FillChar(iStorage,sizeOf(TStgMedium),#0);
    {get data in required format - fail if the format is wrong}
    if iDataObj.GetData(loFormat,iStorage) = S_OK then
      break;  // from While loop
    Inc(i);
  end;
  if i > High(FClipboardFormats) then // no suitable formats available
    Result := FORMAT_UNWANTED
  else
    Result := FClipboardFormats[i];
end;



//==============================================================================
function TClipboardCapability.IsFormatSupported(
  iFormat: integer): boolean;
var i : integer;
begin
  Result := False;  // default value
  for i := 0 to High(FClipboardFormats) do
  begin
    if FClipboardFormats[i] = iFormat then
    begin
      Result := True;
      Exit;  // From the loop AND function
    end;
  end;
end;


//==============================================================================
function TClipboardCapability.IsTableSupported(
  iTable: string): boolean;
var i : integer;
begin
  Result := False;  // default value
  for i := 0 to High(FTableArray) do
  begin
    if FTableArray[i] = iTable then
    begin
      Result := True;
      Exit;  // From the loop AND function
    end;
  end;
end;

{SGB 24/6/00}
//==============================================================================
procedure TClipboardCapability.AddTable( iTable : string);
var i : integer;
begin
     If not IsTableSupported(iTable) then
     begin
          i := High(FTableArray) + 1;
          inc(i);
          SetLength(FTableArray, i);
          FTableArray[i-1] := iTable;
     end;
end;

//==============================================================================
procedure TClipboardCapability.RemoveTable( iTable : string);
var i, j : integer;
begin
     i:=0;
     While i<= High(FTableArray) do
     begin
           If CompareText(FTableArray[i], iTable) = 0 then
           begin
                if i< High(FTableArray) then
                   for j:=i+1 to High(FTableArray) do
                       FTableArray[j-1] := FTableArray[j];
                SetLength(FTableArray, High(FTableArray));
                break;
           end;
           inc(i);
     end;
end;

function TClipboardCapability.GetCountOfFormats: integer;
begin
    Result := High(FClipboardFormats)+1;
end;

function TClipboardCapability.GetCountOfTables: integer;
begin
    Result := High(FTableArray)+1;
end;

function TClipboardCapability.GetFormat(Index: integer): integer;
begin
     Result := -1;
     If (Index>=Low(FClipboardFormats)) and (Index<= High(FClipboardFormats)) then
        Result := FClipboardFormats[Index];
end;

function TClipboardCapability.GetTableName(Index: integer): string;
begin
     Result := '';
     If (Index>=Low(FTableArray)) and (Index<= High(FTableArray)) then
        Result := FTableArray[Index];
end;

end.
