unit DragDropComponent;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DropTarget, ActiveX, DropStruct, DropSource, StdCtrls;

const
  // Messages used in the application
  { Our own user message -needed because Drag Drop cannot be setup until after
         everything else is ready }
  DRAG_THRESHOLD = 5;

  WM_SETDRAGDROP                = WM_APP + 100;
  WM_STARTDRAG                  = WM_APP + 101;

type
  EBaseDragFormError = class(Exception);

  { The following class is a hack which allows us to access the protected
       OnEnter and OnExit stuff in TWinControl }
  TDummyWinControl = class(TWinControl);

  { As above for TControl.OnStartDrag }
  TDummyControl = class(TControl);

  { Simple event container so we can store event easily on a string list }
  TDragEventContainer = class
  private
    FEvent : TDataDraggedEvent;
  public
    constructor Create( iEvent : TDataDraggedEvent );
    property Event : TDataDraggedEvent read FEvent;
  end;

  //----------------------------------------------------------------------------

type
  TJNCCDragDrop = class(TWinControl)
  private
    { Private declarations }
    FDropComponentList : TStringlist;
    FDragComponentList : TStringList;
	  FDragStartPos: TPoint;
    procedure ReRegister(iControl: TWinControl);
  protected
    procedure WMSetupDragDrop( var Message : TMessage ); message WM_SETDRAGDROP;
    procedure WMStartDragDrop( var Message : TMessage ); message WM_STARTDRAG;
    procedure DragControlMouseDown(Sender: TObject; Button: TMouseButton;
									      					 Shift: TShiftState; X, Y: Integer);
    procedure DragControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DragControlMouseUp(Sender: TObject; Button: TMouseButton;
      													 Shift: TShiftState; X, Y: Integer);
    procedure StartDrag(Sender: TObject; var DragObject: TDragObject);
    function GetDropComponentIndex( iComponent : TObject ): integer;
    procedure SetupDestinationControls; virtual;
    { Event handlers to set copy/paste action enabled - don't do anything yet }
    procedure EnterComponent( Sender : TObject ); virtual;
    procedure LeaveComponent( Sender : TObject ); virtual;
    procedure DoStandardCopy(iWinControl : TWinControl);
    procedure DoStandardPaste(iWinControl: TWinControl);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure RegisterDropComponent( iControl : TWinControl;
                                     iDropEvent: TDataDroppedEvent;
                                     iTableList: array of string;
                                     iFormatList: array of integer);
    procedure RegisterDragComponent( iControl : TWinControl;
                                     iDragEvent: TDataDraggedEvent);
    procedure ExecutePaste( Sender : TObject );
    procedure ExecuteCopy( Sender : TObject );
    {SGB 24/6/00}
    procedure AddTable( iControl : TWinControl; iTable : string);
    procedure RemoveTable( iControl : TWinControl; iTable : string);

    property DropComponentList : TStringList read FDropComponentList;
    property DragComponentList : TStringList read FDragComponentList;
  end;


//==============================================================================
implementation

uses
  Clipbrd{, Htmlview};

const
  EST_CONSTRUCT_ERROR      = 'Error occurred in TDragEventContainer constructor';

//==============================================================================
{ TDragEventContainer }
constructor TDragEventContainer.Create(iEvent: TDataDraggedEvent);
begin
  try
    inherited Create;
    FEvent := iEvent;
  except
    on E:Exception do
      raise EBaseDragFormError.Create( 'Drag drop error' );
  end;
end;  // Create

//==============================================================================
{ Message handler for our custom message WM_SETDRAGDROP.  Calls
     SetupDestinationControls which should be inherited in the derived forms }
procedure TJNCCDragDrop.WMSetupDragDrop(var Message: TMessage);
begin
  SetupDestinationControls;
end;  // WMSetupDragDrop

//==============================================================================
procedure TJNCCDragDrop.WMStartDragDrop(var Message: TMessage);
var
  lDropSource : TJNCCDropSource;
  lEffect     : integer;
  lIndex      : integer;
begin
  inherited;
  { Test we have been passed a proper object }
  if not (TObject(Ptr(Message.LParam)) is TWinControl) then
    raise EBaseDragFormError.Create('Drag drop error');
  with TControl(Ptr(Message.LParam)) do
  begin
    EndDrag(False);
    lIndex := FDragComponentList.IndexOf(Name);
    if lIndex = -1 then
      raise EBaseDragFormError.Create('Not registered');
    lDropSource := TJNCCDropSource.Create(TWinControl(Ptr(Message.LParam)));
    TDragEventContainer(FDragComponentList.Objects[lIndex]).
                              Event(TControl(Ptr(Message.LParam)), lDropSource);
  end;
  if lDropSource.DropData.Header.ItemCount > 0 then
    DoDragDrop( lDropSource as IDataObject,
              lDropSource as IDropSource,
              DROPEFFECT_COPY,
              lEffect);
end;  // WMStartDragDrop

//==============================================================================
{ Find the component's position in the list of drop components. }
function TJNCCDragDrop.GetDropComponentIndex(iComponent: TObject): integer;
begin
  if not (iComponent is TComponent) then
    raise EBaseDragFormError.Create('Drag drop error');
  Result := FDropComponentList.IndexOf( TComponent(iComponent).Name );
end;  // GetDropComponentIndex

//==============================================================================

{ Procedure called by derived classes to register a component as a drop
     destination.  Note that the seuqnce iFormatList is provided in indicates
     the order of preference if multiple formats are available }
procedure TJNCCDragDrop.RegisterDropComponent(iControl: TWinControl; iDropEvent :
           TDataDroppedEvent; iTableList: array of string;
           iFormatList : array of integer);
var
  lResult              : HResult;
  lDropTarget          : TJNCCDropTarget;
  lClipboardCapability : TClipboardCapability;
begin
  if Assigned(icontrol) then begin
    { Create a drop target object }
    lDropTarget := TJNCCDropTarget.Create( iControl, iFormatList, iTableList,
                iDropEvent );
    { API call to set up OLE drag drop for the control }
    lResult := RegisterDragDrop(iControl.Handle, lDropTarget as IDropTarget);
    if lResult <> S_OK then
    begin
      raise EBaseDragFormError.Create(iControl.Name );
    end;
    { NB The drop target object is reference counted so we don't need to free it }
    { Setup the clipboard capability list }
    lClipboardCapability := TClipboardCapability.Create
                            ( iTableList, iFormatList, iDropEvent );
    FDropComponentList.AddObject(iControl.Name, lClipboardCapability );
    TDummyWinControl(iControl).OnEnter := EnterComponent;
    TDummyWinControl(iControl).OnExit  := LeaveComponent;
  end;
end;  // RegisterDropComponent

//==============================================================================
procedure TJNCCDragDrop.RegisterDragComponent(iControl: TWinControl;
  iDragEvent: TDataDraggedEvent);
var
  lEventContainer : TDragEventContainer;
begin
  if Assigned(iControl) then begin
    lEventContainer := TDragEventContainer.Create( iDragEvent );
    FDragComponentList.AddObject(iControl.Name, lEventContainer);
    TDummyWinControl(iControl).OnEnter := EnterComponent;
    TDummyWinControl(iControl).OnExit  := LeaveComponent;
    TDummyControl(iControl).DragMode := dmManual;
    TDummyControl(iControl).OnMouseDown := DragControlMouseDown;
    TDummyControl(iControl).OnMouseUp := DragControlMouseUp;
    TDummyControl(iControl).OnStartDrag := StartDrag;
  end;
end;  // RegisterDragcomponent

//==============================================================================
{ Overridable procedure called by the constructor to set up the information
     destination controls.  Not virtual, despite being empty, so we don't get
     an error if it is not implemented in the derived class. }
procedure TJNCCDragDrop.SetupDestinationControls;
begin
  { Yes- really do nothing!}
end;  // SetupDestinationControls

//==============================================================================
procedure TJNCCDragDrop.ExecutePaste(Sender: TObject);
var
  lFormatLoop : integer;
//  lDropData   : TDropData; // for CF_JNCCDATA format
  lIndex      : integer; // index in the component list
//  lHandled    : boolean;
begin
  if Sender is TWinControl then
  begin
    lIndex := GetDropComponentIndex(Sender);
    if lIndex = -1 then
      DoStandardPaste(TWinControl(Sender))
    else
    begin
      for lFormatLoop := 0 to Clipboard.FormatCount-1 do
      begin
        if TClipboardCapability(FDropComponentList.Objects[lIndex]).
                     IsFormatSupported(Clipboard.Formats[lFormatLoop]) then
        begin
          {lDropData := }TDropData.Create;
          TClipboardCapability(FDropComponentList.Objects[lIndex]).
                 ActOnGlobalMemory(Clipboard.GetAsHandle( Clipboard.Formats[lFormatLoop] ),
                 Clipboard.Formats[lFormatLoop],
                 TControl(Sender));

        end; // if TClipboardcapability
      end; // For
    end; // if JNCC paste
  end; // if TWinControl
end;  // ExecutePaste

//==============================================================================
procedure TJNCCDragDrop.ExecuteCopy(Sender: TObject);
var
  lIndex      : integer;
  lDropSource : TJNCCDropSource;
//  lpText      : PChar;
  lHandle     : THandle;
  lstDropText : string;
begin
  if Sender is TWinControl then
  begin
    lIndex := FDragComponentList.IndexOf(TWinControl(Sender).Name);
    if lIndex = -1 then
      DoStandardCopy(TWinControl(Sender))
    else
    begin
      lDropSource := TJNCCDropSource.Create( TWinControl(Sender) );
      try
        TDragEventContainer(FDragComponentList.Objects[lIndex]).
                                             Event(Sender, lDropSource);
        { Read the clipboard data required as text }
        lstDropText := lDropSource.GetText;
        { and now JNCCDATA }
        lDropSource.DropData.WriteToGlobalMemory( lHandle );
        with Clipboard do
        begin
          { Must open/close clipboard for multiple formats }
          Open;
          SetTextBuf(pChar(lstDropText));
          SetAsHandle(CF_JNCCDATA, lHandle);
          Close;
        end;
      finally
        lDropSource.Free;
      end; // finally
    end; // if component in DragComponentList
  end;
end;  // ExecuteCopy

//==============================================================================
procedure TJNCCDragDrop.EnterComponent(Sender : TObject);
begin
  { Really do nothing!}
end;  // EnterComponent

//==============================================================================
procedure TJNCCDragDrop.LeaveComponent(Sender : TObject);
begin
  { Really do nothing!}
end;  // LeaveComponent

//==============================================================================
constructor TJNCCDragDrop.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropComponentList := TStringList.Create;
  FDragComponentList := TStringList.Create;
  { When everything else is ready - setup drag and drop }
  {PostMessage( Self.Handle, WM_SETDRAGDROP, 0, 0 );}
end;  // Create

//==============================================================================
destructor TJNCCDragDrop.Destroy;
var
  i : integer;
begin
  { Walk the drag components list to free associated objects }
  for i := 0 to FDragComponentList.Count-1 do
    FDragComponentList.Objects[i].Free;
  FDragComponentList.Free;
  { Walk the drop components list to free associated objects }
  for i := 0 to FDropComponentList.Count-1 do
    FDropComponentList.Objects[i].Free;
  FDropComponentList.Free;
  inherited Destroy;
end;  // Destroy

//==============================================================================
{ Copy a control which does not feature the CF_JNCCDATA format }
procedure TJNCCDragDrop.DoStandardCopy(iWinControl: TWinControl);
begin
  { TCustomEdit covers all descendants, eg TEdit, TRichEdit, TMemo }
  if iWinControl is TCustomEdit then
    TEdit(iWinControl).CopyToClipboard
//  else if (iWinControl is THTMLViewer) then
//    THTMLViewer(iWinControl).CopyToClipBoard
  else
    Raise Exception.Create(iWinControl.Name);
end;  // DoStandardCopy

//==============================================================================
{ Paste a control which does not feature the CF_JNCCDATA format }
procedure TJNCCDragDrop.DoStandardPaste(iWinControl: TWinControl);
begin
  { TCustomEdit covers all descendants, eg TEdit, TRichEdit, TMemo }
  if iWinControl is TCustomEdit then
    TCustomEdit(iWinControl).PasteFromClipboard
  else
    raise Exception.Create(iWinControl.Name);
end;  // DoStandardPaste

//==============================================================================
{Initiate drag}
procedure TJNCCDragDrop.DragControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Set drag values only if left mouse button down
  if Button=mbLeft then begin
    //Set drag start position
    FDragStartPos.X:= X;
    FDragStartPos.Y:= Y;

    //Set mouse move cursor to check for drag
    TDummyControl(Sender).OnMouseMove:= DragControlMouseMove;
  end;
end;  // DragControlMouseDown

//==============================================================================
{Start dragging if mouse moved far enough. Event called only when dragging,
 as it is set in MouseDown event only if Left button pressed}
procedure TJNCCDragDrop.DragControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  //Check to see if mouse has moved far enough to start dragging
  if (Abs(FDragStartPos.X - X) >= DRAG_THRESHOLD) or
    (Abs(FDragStartPos.Y - Y) >= DRAG_THRESHOLD) then

    //Initiate drag
    (Sender as TControl).BeginDrag(True);
end;  // DragControlMouseMove

//==============================================================================
{Stop checking for drag}
procedure TJNCCDragDrop.DragControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Remove MouseMove handler
  TDummyControl(Sender).OnMouseMove:= Nil;
end;  // DragControlMouseUp

//==============================================================================
{ Handle the OnStartDrag event for all draggable components.  We need to replace
     Delphi's own drag drop implementation with our own, but it is too early to
     do this so we post a message to ourselves to do this.  Scary Stuff! }
procedure TJNCCDragDrop.StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  inherited;
  if not (Sender is TWinControl) then
    raise EBaseDragFormError.Create('Drag drop error');
  PostMessage(Self.Handle, WM_STARTDRAG, 0, Integer(Sender));
end;  // StartDrag


{SGB 24/6/00}
//==============================================================================
procedure TJNCCDragDrop.AddTable( iControl : TWinControl; iTable : string);
var lIndex : integer;
begin
    lIndex := FDropComponentList.IndexOf(iControl.Name);
    if lIndex = -1 then
      raise EBaseDragFormError.Create('Not registered')
    else
    begin
      TClipboardCapability(FDropComponentList.Objects[lIndex]).AddTable(iTable);
      ReRegister(iControl);
    end
end;

//==============================================================================
procedure TJNCCDragDrop.RemoveTable( iControl : TWinControl; iTable : string);
var lIndex : integer;
begin
    lIndex := FDropComponentList.IndexOf(iControl.Name);
    if lIndex = -1 then
      raise EBaseDragFormError.Create('Not registered')
    else
    begin
      TClipboardCapability(FDropComponentList.Objects[lIndex]).RemoveTable(iTable);
      ReRegister(iControl);
    end;
end;

procedure TJNCCDragDrop.ReRegister(iControl : TWinControl);
var lIndex, i: integer;
    lDropTarget: TJNCCDropTarget;
    lResult: HResult;
    lTableList: array of string;
    lFormatList: array of integer;
begin
  if Assigned(icontrol) then
  begin
     lIndex := FDropComponentList.IndexOf(iControl.Name);
     if lIndex > -1 then
     begin
        //Remove previous registration
        RevokeDragDrop(iControl.handle);
        //re-Register as drop target with new parameters
        with TClipboardCapability(FDropComponentList.Objects[lIndex]) do
        begin
          // Set up table list
          SetLength(lTableList, TableCount);
          for i := 0 to TableCount-1 do
              lTableList[i] := TableName[i];
          // Set up format list
          SetLength(lFormatList, FormatCount);
          for i := 0 to FormatCount-1 do
              lFormatList[i] := Format[i];
          // Create a drop target object
          lDropTarget := TJNCCDropTarget.Create( iControl, lFormatList, lTableList,
                         DropEvent );
          // API call to set up OLE drag drop for the control
          lResult := RegisterDragDrop(iControl.Handle, lDropTarget as IDropTarget);
          if lResult <> S_OK then
          begin
               raise EBaseDragFormError.Create(iControl.Name );
          end;
          // NB The drop target object is reference counted so we don't need to free it
        end;
     end;
  end;
end;


end.
