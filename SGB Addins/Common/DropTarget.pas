{ Unit DropTarget
Defines the TJNCCDropTarget object for Recorder 2000.  This provides
handling for OLE drag and drop.  Implements the IDropTarget interface. }

unit DropTarget;

interface

uses
  Windows, Controls, Classes, Sysutils, ActiveX, ComObj,
  Dialogs, dropstruct, dragdrop;

type
  EDropTargetError = class(EDragDropError);

  TJNCCDropTarget = class(TjvbDropTarget, IDropTarget)
  private
    FClipboardCapability : TClipboardCapability;
    FControlHandle       : THandle;   // just record the handle so drag drop can be revoked
    FControl             : TWinControl;  // note this can't be used to get the handle during destruction
    FFormat              : integer; // index of the format - set when checkformat occurs
    FTableName           : string; // name of table if CF_JNCCDATA
  protected
    { Note IDropTarget is implemented in the base class, not here}
    procedure DoDrop(const dataObj: IDataObject); override;
    function CheckFormats( const dataObj : IDataObject; Pt : TPoint):
                                                         boolean; override;
  public
    constructor Create( iControl : TWinControl;
                        iClipboardFormats: array of integer;
                        iTableList: array of string;
                        iDropEvent: TDataDroppedEvent);
    destructor Destroy; override;
    property ControlHandle : THandle read FControlHandle;
    property TableName : string read FTableName;
  end;


(*
  TTVDropTarget = class(TJNCCDropTarget, IDropTarget)
  private
    FLastNode : TObject;  // pointer to the last node dragged over so we can test for changes
  protected
    { Note IDropTarget is implemented in the base class, not here}
    function CheckFormats( const dataObj : IDataObject; Pt : TPoint):
                                                         boolean; override;
  public
    constructor Create( iControl : TWinControl;
                        iClipboardFormats: array of integer;
                        iTableList: array of string;
                        iDropEvent: TDataDroppedEvent);
  end;

  *)

const
  Class_TJNCCDropTarget: TGUID = '{E5EC6D72-EE51-11D2-B708-0060085B710F}';
  FORMAT_UNWANTED              = -1;

var
  CF_JNCCDATA : integer;


implementation

uses
  ComServ, Addinclasses;

const
  EST_NOT_TREEVIEW = 'Cannot create TTVDropTarget objects for non-treeview components : ';

  { TJNCCDropTarget }

  //==============================================================================
constructor TJNCCDropTarget.Create(iControl : TWinControl;
                                  iClipboardFormats : array of integer;
                                  iTableList : array of string;
                                  iDropEvent : TDataDroppedEvent );
begin
  inherited Create;
  FControlHandle := iControl.Handle;
  FControl := iControl;
  { Record the formats and tables supported }
  FClipboardCapability := TClipboardCapability.Create( iTableList,
                                                       iClipboardFormats,
                                                       iDropEvent );
end;



//==============================================================================
destructor TJNCCDropTarget.Destroy;
begin
  FClipboardCapability.Free;
  inherited;
end;



{ Checks that one of the available formats can be dropped onto the point
     specified }
function TJNCCDropTarget.CheckFormats(const dataObj: IDataObject;
                                                 pt: TPoint ): Boolean;
var
  loStorage  : TStgMedium; {data storage}
  lStructPtr : Pointer;
begin
  Result := True; // Default to Ok unless we detect otherwise
  // query clipboard format
  FFormat := FClipboardCapability.GetBestFormat(dataObj, loStorage);
  if FFormat = FORMAT_UNWANTED then
    Result := False;
  { We are now OK unless the data is CF_JNCCdata - further test required }
  if FFormat = CF_JNCCDATA then
  begin
    lStructPtr := GlobalLock(loStorage.hGlobal);
    FTableName := TStructHeader(lStructPtr^).TableName;
    GlobalUnlock(loStorage.hGlobal);
    { Check that the source table is acceptable to the drag destination }
    if  not FClipboardCapability.IsTableSupported(FTableName) then
    begin
      { Can't accept - wrong source table }
      Result := False;
      FTableName := '';
    end;
  end; // if clipboardformats
end;



{ Actually do the drop handling }
procedure TJNCCDropTarget.DoDrop(const dataObj : IDataObject);
var
  loFormat   : TFormatEtc; {target format}
  loStorage  : TStgMedium; {data storage}
begin
  if FFormat = FORMAT_UNWANTED then
    Exit;
  {prepare format structs}
  FillChar(loFormat,sizeOf(TFormatEtc),#0);
  loFormat.cfFormat:=FFormat;
  loFormat.tymed:=TYMED_HGLOBAL;
  {prepare storage struct - not strictly neccessary as stgmedium is an out}
  FillChar(loStorage,sizeOf(TStgMedium),#0);
  {get data in required format}
  OleCheck(DataObj.GetData(loFormat,loStorage));
  try
    FClipboardCapability.ActOnGlobalMemory( loStorage.hGlobal, FFormat, FControl )
  finally
    { The way we are doing it, the data receiver is responsible for freeing
         the global memory }
    GlobalFree(loStorage.hGlobal);
    {NB release storage medium knows what to do}
    ReleaseStgMedium(loStorage);
  end;{try f}
end;





(*

//==============================================================================

{ TTVDropTarget }

function TTVDropTarget.CheckFormats(const dataObj: IDataObject;
  Pt: TPoint): boolean;
begin
  Result := inherited CheckFormats(DataObj, PT);
  if FFormat = CF_JNCCDATA then
  begin
      FLastNode := TCustomTreeView(FControl).
                               GetNodeAt(PT.X + FControl.Left, PT.Y + FControl.Top);
    { We are only bothered if the node is a TNodeObject }
    if FLastNode = nil then
      Exit;
    if (FLastNode is TSampleNode) then
    begin
      if (TableName <> 'TAXON_LIST_ITEM') and ('

    end;
  end; // if FFormat
end;


//==============================================================================
constructor TTVDropTarget.Create(iControl: TWinControl;
  iClipboardFormats: array of integer; iTableList: array of string;
  iDropEvent: TDataDroppedEvent);
begin
  if not (iControl is TCustomTreeView) then
    raise EDropTargetError.Create(EST_NOT_TREEVIEW + iControl.name);
  inherited Create(iControl, iClipboardFormats, iTableList, iDropEvent);
end;
*)

//==============================================================================


initialization
  { Get a clipboard format for JNCC data }
  CF_JNCCDATA := RegisterClipboardFormat( 'CF_JNCCDATA' );




end.
