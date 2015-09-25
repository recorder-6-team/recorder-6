{ Unit DropTarget
Defines the TJNCCDropTarget object for Recorder 2000.  This provides
handling for OLE drag and drop.  Implements the IDropTarget interface. }

unit DropTarget;

interface

uses
  Windows, Controls, Classes, Sysutils, ActiveX, ComObj, ApiUtils,
  Dialogs, DropStruct, DragDrop;

type
  EDropTargetError = class(EDragDropError);

  TJNCCDropTarget = class(TDropTarget, IDropTarget)
  private
    FClipboardCapability: TClipboardCapability;
    FControlHandle      : THandle;   // just record the handle so drag drop can be revoked
    FControl            : TWinControl;  // note this can't be used to get the handle during destruction
    FFormat             : integer; // index of the format - set when checkformat occurs
    FTableName          : string;
    FKeyField           : String;
    FDraggedFiles       : TStringList;
    FDragOverCheckEvent : TDragOverCheckEvent;
    FDragFileOverCheckEvent: TDragFileOverCheckEvent;
    procedure GenericConstruction(iControl: TWinControl;
        iDragOverCheck: TDragOverCheckEvent=nil;
        iDragFileOverCheck: TDragFileOverCheckEvent=nil);
    procedure GetTableNameAndKeyField(const dataObj: IDataObject);
    procedure GetFileNames(const dataObj: IDataObject);
  protected
    { Note IDropTarget is implemented in the base class, not here}
    procedure DoDrop(const dataObj: IDataObject); override;
    function CheckFormats( const dataObj: IDataObject; Pt: TPoint): boolean; override;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; override; stdcall;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; override; stdcall;
  public
    constructor Create(iControl: TWinControl;
                       iClipboardFormats: array of integer;
                       iTableList: array of string;
                       iDropEvent: TDataDroppedEvent;
                       iDragOverCheck: TDragOverCheckEvent=nil;
                       iDragFileOverCheck: TDragFileOverCheckEvent=nil); reintroduce; overload;
    constructor Create(iControl: TWinControl;
                       iClipboardFormats: array of integer;
                       iTableList: array of string;
                       iDropEvent: TAdvancedDataDroppedEvent;
                       iDragOverCheck: TDragOverCheckEvent=nil;
                       iDragFileOverCheck: TDragFileOverCheckEvent=nil); reintroduce; overload;
    destructor Destroy; override;
    property ControlHandle: THandle read FControlHandle;
    property TableName: string read FTableName;
    property KeyField: String read FKeyField;
  end;

const
  Class_TJNCCDropTarget: TGUID = '{E5EC6D72-EE51-11D2-B708-0060085B710F}';
  FORMAT_UNWANTED              = -1;

var
  CF_JNCCDATA: integer;

//==============================================================================
implementation

uses
  DataClasses, ShellAPI;

resourcestring
  ResStr_NotTreeView = 'Cannot create TTVDropTarget objects for non-treeview components: ';

{===============================================================================
 TJNCCDropTarget
===============================================================================}
{-------------------------------------------------------------------------------
  Standard object constructor.
}
constructor TJNCCDropTarget.Create(iControl: TWinControl;
                                  iClipboardFormats: array of integer;
                                  iTableList: array of string;
                                  iDropEvent: TDataDroppedEvent;
                                  iDragOverCheck: TDragOverCheckEvent=nil;
                                  iDragFileOverCheck: TDragFileOverCheckEvent=nil);
begin
  inherited Create;
  GenericConstruction(iControl, iDragOverCheck, iDragFileOverCheck);
  { Record the formats and tables supported }
  FClipboardCapability := TClipboardCapability.Create(iTableList,
                                                      iClipboardFormats,
                                                      iDropEvent);
end;

{-------------------------------------------------------------------------------
  Advanced object constructor - drop event takes a parameter that defines if
     the event was a drop or a paste operation.
}
constructor TJNCCDropTarget.Create(iControl: TWinControl;
                                  iClipboardFormats: array of integer;
                                  iTableList: array of string;
                                  iDropEvent: TAdvancedDataDroppedEvent;
                                  iDragOverCheck: TDragOverCheckEvent=nil;
                                  iDragFileOverCheck: TDragFileOverCheckEvent=nil);
begin
  inherited Create;
  GenericConstruction(iControl, iDragOverCheck, iDragFileOverCheck);
  { Record the formats and tables supported }
  FClipboardCapability := TClipboardCapability.Create(iTableList,
                                                      iClipboardFormats,
                                                      iDropEvent);
end;

{-------------------------------------------------------------------------------
  Constructor code common to all overloads
}
procedure TJNCCDropTarget.GenericConstruction(iControl: TWinControl;
    iDragOverCheck: TDragOverCheckEvent=nil;
    iDragFileOverCheck: TDragFileOverCheckEvent=nil);
begin
  FControlHandle          := iControl.Handle;
  FControl                := iControl;
  FDragOverCheckEvent     := iDragOverCheck;
  FDragFileOverCheckEvent := iDragFileOverCheck;
  FDraggedFiles           := TStringList.Create;
end;

//==============================================================================
destructor TJNCCDropTarget.Destroy;
begin
  FClipboardCapability.Free;
  FDraggedFiles.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Checks that one of the available formats can be dropped onto the point
  specified.
}
function TJNCCDropTarget.CheckFormats(const dataObj: IDataObject;
                                                 pt: TPoint ): Boolean;
begin
  Result := True; // Default to Ok unless we detect otherwise
  GetTableNameAndKeyField(dataObj);
  if FTableName<>'' then begin
    // If a DragOverCheckEvent is assigned, then request that it double checks
    // the drop is OK, otherwise use automatic check on table name
    if Assigned(FDragOverCheckEvent) then
      FDragOverCheckEvent(pt, FTableName, FKeyField, Result)
    else if not FClipboardCapability.IsTableSupported(FTableName) then begin
      { Can't accept - wrong source table }
      Result := False;
      FTableName := '';
    end;
  end; // if clipboardformats
end;

{-------------------------------------------------------------------------------
  Retrieve the table name from the memory passed as a drag object.
}
procedure TJNCCDropTarget.GetTableNameAndKeyField(const dataObj: IDataObject);
var
  loStorage : TStgMedium; {data storage}
  lStructPtr: Pointer;
begin
  // query clipboard format
  FFormat := FClipboardCapability.GetBestFormat(dataObj, loStorage);
  if (FFormat = FORMAT_UNWANTED) or (not FControl.Enabled) then
    FTableName := ''
  else if FFormat = CF_JNCCDATA then
  begin
    lStructPtr := GlobalLock(loStorage.hGlobal);
    FTableName := TStructHeader(lStructPtr^).TableName;

    // Skip header and reach for first item.
    if TStructHeader(lStructPtr^).ItemCount > 0 then begin
      lStructPtr := Ptr(Integer(lStructPtr) + SizeOf(TStructHeader));
      FKeyField  := TStructItem(lStructPtr^).KeyField1;
    end else
      FKeyField := '';

    GlobalUnlock(loStorage.hGlobal);
  end;
end;

{-------------------------------------------------------------------------------
  Retrieve the list of file names from the memory passed as a drag object.
}
procedure TJNCCDropTarget.GetFileNames(const dataObj: IDataObject);
var
  loStorage : TStgMedium;     // data storage
  lDroppedFileCount: Integer;  // number of dropped files
  i: Integer;                  // loops thru dropped files
  lFileNameLength: Integer;    // length of a dropped file name
  lFileName: string;           // name of a dropped file
begin
  // query clipboard format
  FFormat := FClipboardCapability.GetBestFormat(dataObj, loStorage);
  if FFormat = CF_HDROP then begin
    FDraggedFiles.Clear;
    try
      try
        // Get count of files dropped
        lDroppedFileCount := DragQueryFile(loStorage.hGlobal, $FFFFFFFF, nil, 0);
        // Get name of each file dropped and process it
        for i := 0 to Pred(lDroppedFileCount) do begin
          // get length of file name, then name itself
          lFileNameLength := DragQueryFile(loStorage.hGlobal, i, nil, 0);
          SetLength(lFileName, lFileNameLength);
          DragQueryFile(loStorage.hGlobal, i, PChar(lFileName), lFileNameLength + 1);
          // add file name to list
          FDraggedFiles.Add(lFileName);
        end;    // for i := 0 to Pred(lDroppedFileCount) do
      finally
        DragFinish(loStorage.hGlobal);
      end;
    finally
      ReleaseStgMedium(loStorage);
    end;
  end;    // if FFormat = CF_HDROP
end;

{-------------------------------------------------------------------------------
  Actually do the drop handling.
}
procedure TJNCCDropTarget.DoDrop(const dataObj: IDataObject);
var
  loFormat: TFormatEtc; {target format}
  loStorage: TStgMedium; {data storage}
begin
  if FFormat = FORMAT_UNWANTED then
    Exit;
  {prepare format structs}
  FillChar(loFormat, sizeOf(TFormatEtc), #0);
  loFormat.cfFormat := FFormat;
  loFormat.tymed    := TYMED_HGLOBAL;
  {prepare storage struct - not strictly neccessary as stgmedium is an out}
  FillChar(loStorage, sizeOf(TStgMedium), #0);
  {get data in required format}
  OleCheck(DataObj.GetData(loFormat, loStorage));
  try
    FClipboardCapability.ActOnGlobalMemory(loStorage.hGlobal, FFormat, FControl, False);
  finally
    // From MSDN: ReleaseStgMedium calls the relevant function, including GlobalFree if needed.
    ReleaseStgMedium(loStorage);
  end;{try f}
end;

{-------------------------------------------------------------------------------
  If the component has custom drag acceptance code, then call it as the mouse
  moves over the component.
}
function TJNCCDropTarget.DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
var
  lAccept: boolean;
begin
  Result := S_OK;
  if Assigned(FDragOverCheckEvent) then begin
    FDragOverCheckEvent(pt, FTableName, FKeyField, lAccept);
    if not lAccept then
      Result := E_FAIL;
  end;    // if Assigned(FDragOverCheckEvent)

  if Assigned(FDragFileOverCheckEvent) and (not lAccept) then begin
    FDragFileOverCheckEvent(pt, FDraggedFiles, lAccept);
    if lAccept then
      Result := S_OK;
  end;    // if Assigned(FDragFileOverCheckEvent) and (not lAccept)
end;

{-------------------------------------------------------------------------------
}
function TJNCCDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  // If we are using a DragOverCheckEvent, then all checking must be done as
  // the mouse moves around
  if Assigned(FDragOverCheckEvent) or Assigned(FDragFileOverCheckEvent) then begin
    if Assigned(FDragOverCheckEvent) then
      GetTableNameAndKeyField(dataObj);
    if Assigned(FDragFileOverCheckEvent) then
      GetFileNames(dataObj);
    Result := S_OK
  end else
    Result := inherited DragEnter(dataObj, grfKeyState, pt, dwEffect);
end;

//==============================================================================
initialization
  { Get a clipboard format for JNCC data }
  CF_JNCCDATA := RegisterClipboardFormat( 'CF_JNCCDATA' );

end.
