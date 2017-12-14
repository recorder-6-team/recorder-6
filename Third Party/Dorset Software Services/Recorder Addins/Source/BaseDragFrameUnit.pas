{===============================================================================
  Unit:        BaseDragFrameUnit

  Implements:  TBaseDragFrame

  Description: Base form for drag/drop and copy/paste handling

  Model:       CollectionBrowserFramework.mpb

  Created:     August 2003

  Last Revision Details:
    $Revision: 1 $
    $Date: 2/09/04 15:22 $
    $Author: Andrewkemp $

===============================================================================}

unit BaseDragFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, DropTarget, DropStruct, DropSource, StdCtrls, ExceptionForm, Htmlview,
  Grids, ExtCtrls, ComCtrls, UserMessages;

type
  EBaseDragFrameError = class (TExceptionPath)
  end;
  
  TBaseDragFrame = class (TFrame)
  private
    FDragComponentList: TStringList;
    FDragDropComponentsRegistered: Boolean;
    FDragStartPos: TPoint;
    FDropComponentList: TStringList;
    FEventList: TStringList;
    FRegisteredDragDropComponents: TList;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    function GetPasteEnabled: Boolean;
    procedure InternalRegisterDropComponent(AWinControl: TWinControl; ADropEvent: 
        TDataDroppedEvent; AAdvancedDropEvent: TAdvancedDataDroppedEvent; ATableList: array of 
        String; AFormatList: array of Integer; ADragOverCheck: TDragOverCheckEvent);
    procedure SetDragDropColours;
    procedure WMSetupDragDrop(var Message: TMessage); message WM_SETUPDRAGDROP;
  protected
    function CheckTableSupported(ADropComponentIndex: integer; const ATableName: string): 
        Boolean; virtual;
    procedure CreateWnd; override;
    procedure DoStandardCopy(AWinControl: TWinControl);
    procedure DoStandardPaste(AWinControl: TWinControl);
    procedure DragControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; 
        X, Y: Integer);
    procedure DragControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DragControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, 
        Y: Integer);
    function GetActiveControl(AComponent: TComponent): TWinControl;
    function GetDropComponentIndex(AComponent: TObject): Integer;
    procedure RegisterDragDropComponents; virtual;
    procedure SetupDragDropComponents;
    procedure StartDragDrop(Sender: TWinControl);
    procedure UnregisterDragDropComponents;
    procedure UpdateCopyPaste(AWinControl: TWinControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteCopy(Sender: TObject);
    procedure ExecutePaste(Sender: TObject);
    procedure RegisterCopyPasteComponent(AControl: TWinControl);
    procedure RegisterDragComponent(AWinControl: TWinControl; ADragEvent: TDataDraggedEvent);
    procedure RegisterDropComponent(AWinControl: TWinControl; ADropEvent: 
        TAdvancedDataDroppedEvent; ATableList: array of String; AFormatList: Array of Integer; 
        ADragOverCheck: TDragOverCheckEvent=nil); overload;
    procedure RegisterDropComponent(AWinControl: TWinControl; ADropEvent: TDataDroppedEvent; 
        ATableList: array of String; AFormatList: Array of Integer; ADragOverCheck: 
        TDragOverCheckEvent=nil); overload;
    property DragComponentList: TStringList read FDragComponentList;
    property DropComponentList: TStringList read FDropComponentList;
    property PasteEnabled: Boolean read GetPasteEnabled;
  end;
  
  {-----------------------------------------------------------------------------
    Class to allow access to the protected ReadOnly property of TCustomEdit.
  }
  TCustomEditAccessor = class (TCustomEdit)
  public
    property ReadOnly;
  end;
  
  {-----------------------------------------------------------------------------
    To allow access to the protected Color, OnEnter and OnExit properties in TWinControl.
  }
  TWinControlAccessor = class (TWinControl)
  public
    property Color;
    property OnEnter;
    property OnExit;
  end;
  
  {-----------------------------------------------------------------------------
    To allow access to the protected OnStartDrag and Text properties in TControl.
  }
  TControlAccessor = class (TControl)
  public
    property Text;
  end;
  

//==============================================================================
implementation

{$R *.DFM}

uses
  Clipbrd, AddinApplicationSettings, ProjectSpecificAccess, BaseCompositeComponent,
  LinkedControls;

const
  DRAG_THRESHOLD = 5;

resourcestring
  { TODO : Move to correct unit }
  ResStr_DragDropRegFailed  = 'Could not register the following control for drag drop: ';
  ResStr_NotComponent       = 'Object is not component';
  ResStr_NotRegistered      = 'Component not registered to drag from';

type
  {-----------------------------------------------------------------------------
    Drag and Drop support class.
  }
  TDragEventContainer = class (TObject)
  private
    FEvent: TDataDraggedEvent;
  public
    constructor Create(AEvent: TDataDraggedEvent);
    property Event: TDataDraggedEvent read FEvent;
  end;
  
  {-----------------------------------------------------------------------------
    Drag and Drop support class.
  }
  TMouseEventContainer = class (TObject)
  private
    FEvent: TMouseEvent;
  public
    constructor Create(AEvent: TMouseEvent);
    property Event: TMouseEvent read FEvent;
  end;
  
  {-----------------------------------------------------------------------------
    Drag and Drop support class.
  }
  TMouseMoveEventContainer = class (TObject)
  private
    FEvent: TMouseMoveEvent;
  public
    constructor Create(AEvent: TMouseMoveEvent);
    property Event: TMouseMoveEvent read FEvent;
  end;
  
  {-----------------------------------------------------------------------------
    To allow access to the in-place editor in grids.
  }
  TCustomGridAccessor = class (TCustomGrid)
  end;
  
{-==============================================================================
    TBaseDragFrame
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseDragFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FDropComponentList := TStringList.Create;
  FDragComponentList := TStringList.Create;
  FRegisteredDragDropComponents := TList.Create;
  FEventList := TStringList.Create;
  
  // Setup the drag/drop colours
  SetDragDropColours;
end;  // TBaseDragFrame.Create 

{-------------------------------------------------------------------------------
}
destructor TBaseDragFrame.Destroy;
var
  i: Integer;
begin
  { Walk the drag components list to free associated objects }
  if Assigned(FDragComponentList) then // safety in case constructor failed
    with FDragComponentList do begin
      for i := 0 to Count-1 do Objects[i].Free;
      Free;
    end;
  
  { Walk the drop components list to free associated objects }
  if Assigned(FRegisteredDragDropComponents) then
  begin
    with FDropComponentList do begin
      for i := 0 to Count-1 do Objects[i].Free;
      Free;
    end;
  end;
  try
    UnregisterDragDropComponents;
  except
    on Exception do ; // Ignore any errors here as we are shutting down anyway
  end;
  FRegisteredDragDropComponents.Free;
  
  if Assigned(FEventList) then // safety in case constructor failed
    with FEventList do begin
      for i := 0 to Count-1 do Objects[i].Free;
      Free;
    end;
  
  inherited Destroy;
end;  // TBaseDragFrame.Destroy

{-------------------------------------------------------------------------------
  Checks if a TableName is supported by a specified DropComponent.
}
function TBaseDragFrame.CheckTableSupported(ADropComponentIndex: integer; const ATableName: 
    string): Boolean;
begin
  Result := TClipboardCapability(DropComponentList.Objects[ADropComponentIndex])
                .IsTableSupported(ATableName);
end;  // TBaseDragFrame.CheckTableSupported 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.CMFocusChanged(var Message: TCMFocusChanged);
begin
  UpdateCopyPaste(GetActiveControl(Self));
  // Broadcast message to ActiveControl's owned controls
  Broadcast(Message);
end;  // TBaseDragFrame.CMFocusChanged 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.CreateWnd;
begin
  inherited;
  PostMessage(Handle, WM_SETUPDRAGDROP, 0, 0);
end;  // TBaseDragFrame.CreateWnd 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.DoStandardCopy(AWinControl: TWinControl);
var
  lEditor: TInPlaceEdit;
begin
  { TCustomEdit covers all descendants, eg TEdit, TRichEdit, TMemo }
  if AWinControl is TCustomEdit then
    TEdit(AWinControl).CopyToClipboard
  else if (AWinControl is THTMLViewer) then
    // If something selected, copy only the selection to the clipboard
    with THTMLViewer(AWinControl) do
      if SelLength <> 0 then
        CopyToClipboard
      else begin
        // Otherwise, copy everything
        SelectAll;
        CopyToClipboard;
        SelLength := 0;
      end
  else if (AWinControl is TComboBox) then
    SendMessage(AWinControl.Handle, WM_COPY, 0, 0)
  else if (AWinControl is TCustomGrid) then begin
    lEditor := TCustomGridAccessor(AWinControl).InplaceEditor;
    if Assigned(lEditor) then SendMessage(lEditor.Handle, WM_COPY, 0, 0)
  end;
end;  // TBaseDragFrame.DoStandardCopy 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.DoStandardPaste(AWinControl: TWinControl);
var
  lEditor: TInplaceEdit;
begin
  { TCustomEdit covers all descendants, eg TEdit, TRichEdit, TMemo }
  if AWinControl is TCustomEdit then
    TCustomEdit(AWinControl).PasteFromClipboard
  else if (AWinControl is TComboBox) then
    SendMessage(AWinControl.Handle, WM_PASTE, 0, 0)
  else if (AWinControl is TCustomGrid) then begin
    lEditor := TCustomGridAccessor(AWinControl).InPlaceEditor;
    if Assigned(lEditor) then SendMessage(lEditor.Handle, WM_PASTE, 0, 0)
  end;
end;  // TBaseDragFrame.DoStandardPaste 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.DragControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: 
    TShiftState; X, Y: Integer);
var
  lCanDrag: Boolean;
  lOldEvent: TMouseEvent;
  lEventIdx: Integer;
begin
  lCanDrag := true;
  // Don't want to start dragging if resizing columns only!!!
  if (Sender is TCustomGrid) then
    with TCustomGridAccessor(Sender) do
      if (FixedRows > 0) and (MouseCoord(X, Y).Y < FixedRows) then lCanDrag := false;
  
  // Set drag values only if left mouse button down
  if lCanDrag and (Button=mbLeft) then begin
    //Set drag start position
    FDragStartPos.X:= X;
    FDragStartPos.Y:= Y;
    //Set mouse move cursor to check for drag
    if Assigned(TControlAccessor(Sender).OnMouseMove) then
      FEventList.AddObject(TControlAccessor(Sender).name + 'MouseMove',
          TMouseMoveEventContainer.Create(TControlAccessor(Sender).OnMouseMove));
    TControlAccessor(Sender).OnMouseMove:= DragControlMouseMove;
  end;
  lEventIdx := FEventList.IndexOf((sender as TControl).name + 'MouseDown');
  if lEventIdx >=0 then
  begin
    lOldEvent := TMouseEventContainer(FEventList.Objects[lEventIdx]).Event;
    lOldEvent(Sender, Button, Shift, X,Y);
  end;
end;  // TBaseDragFrame.DragControlMouseDown 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.DragControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: 
    Integer);
begin
  //Check to see if mouse has moved far enough to start dragging
  if ((Abs(FDragStartPos.X - X) >= DRAG_THRESHOLD) or
      (Abs(FDragStartPos.Y - Y) >= DRAG_THRESHOLD)) or
     (Sender is TListbox) and (ssLeft in Shift) then begin
    //Initiate drag
    if Sender is TWinControl then
      StartDragDrop(TWinControl(Sender));
  end;
end;  // TBaseDragFrame.DragControlMouseMove 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.DragControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: 
    TShiftState; X, Y: Integer);
var
  lOldEvent: TMouseEvent;
  lEventIdx: Integer;
begin
  // Remove MouseMove handler
  lEventIdx := FEventList.IndexOf((Sender as TControl).Name + 'MouseMove');
  if lEventIdx >=0 then
    TControlAccessor(Sender).OnMouseMove := TMouseMoveEventContainer(
        FEventList.Objects[lEventIdx]).Event
  else
    TControlAccessor(Sender).OnMouseMove := nil;
  
  //Execute the user's event
  lEventIdx := FEventList.IndexOf((Sender as TControl).Name + 'MouseUp');
  if lEventIdx >=0 then
  begin
    lOldEvent := TMouseEventContainer(FEventList.Objects[lEventIdx]).Event;
    lOldEvent(Sender, Button, Shift, X,Y);
  end;
end;  // TBaseDragFrame.DragControlMouseUp 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.ExecuteCopy(Sender: TObject);
var
  lIndex: Integer;
  lDropSource: TJNCCDropSource;
  lHandle: THandle;
  lDropText: String;
begin
  if Sender is TWinControl then begin
    lIndex := FDragComponentList.IndexOf(TWinControl(Sender).Name);
    if lIndex = -1 then
      DoStandardCopy(TWinControl(Sender))
    else begin
      lDropSource := TJNCCDropSource.Create(TWinControl(Sender));
      try
        if Assigned(FDragComponentList.Objects[lIndex]) then
          TDragEventContainer(FDragComponentList.Objects[lIndex]).Event(Sender,
              lDropSource);
        { Read the clipboard data required as text }
        lDropText := lDropSource.GetText;
        { and now JNCCDATA }
        lDropSource.DropData.WriteToGlobalMemory(lHandle);
        with Clipboard do begin
          { Must open/close clipboard for multiple formats }
          Open;
          SetTextBuf(PChar(lDropText));
          SetAsHandle(CF_JNCCDATA, lHandle);
          Close;
        end;
      finally
        lDropSource.Free;
      end; // finally
    end; // if component in DragComponentList
  end;
end;  // TBaseDragFrame.ExecuteCopy 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.ExecutePaste(Sender: TObject);
var
  lFormatLoop: Integer;
  lIndex: Integer;
begin
  if Sender is TWinControl then begin
    lIndex := GetDropComponentIndex(Sender);
    if lIndex = -1 then
      DoStandardPaste(TWinControl(Sender))
    else
      for lFormatLoop := 0 to Clipboard.FormatCount-1 do
        if TClipboardCapability(FDropComponentList.Objects[lIndex]).
                     IsFormatSupported(Clipboard.Formats[lFormatLoop]) then
        begin
          TClipboardCapability(FDropComponentList.Objects[lIndex]).
                 ActOnGlobalMemory(Clipboard.GetAsHandle(Clipboard.Formats[lFormatLoop]),
                 Clipboard.Formats[lFormatLoop],
                 TControl(Sender));
        end; // if TClipboardcapability
  end; // if TWinControl
end;  // TBaseDragFrame.ExecutePaste 

{-------------------------------------------------------------------------------
}
function TBaseDragFrame.GetActiveControl(AComponent: TComponent): TWinControl;
begin
  if not Assigned(AComponent) then
    Result := nil
  else
  if AComponent is TCustomForm then //TCustomForm includes TForm and TActiveForm
    Result := TForm(AComponent).ActiveControl
  else
    Result := GetActiveControl(AComponent.Owner);
end;  // TBaseDragFrame.GetActiveControl 

{-------------------------------------------------------------------------------
}
function TBaseDragFrame.GetDropComponentIndex(AComponent: TObject): Integer;
begin
  Result := -1;
  if Assigned(FDropComponentList) then begin
    if not (AComponent is TComponent) then
      raise EBaseDragFrameError.Create(ResStr_NotComponent);
    Result := FDropComponentList.IndexOf(TComponent(AComponent).Name);
  end;
end;  // TBaseDragFrame.GetDropComponentIndex 

{-------------------------------------------------------------------------------
}
function TBaseDragFrame.GetPasteEnabled: Boolean;
var
  lFormat, lIdx: Integer;
  lDropData: TDropData;
  lWinControl: TWinControl;
begin
  Result := False;
  lWinControl := GetActiveControl(Self);
  
  if lWinControl <> nil then begin
    Result := False;  // default
    lIdx := GetDropComponentIndex(lWinControl);
    // Handle drop/paste stuff
    if lIdx = -1 then
      // Clipboard must have some text to have Paste enabled in this case
      Result := Clipboard.HasFormat(CF_TEXT) and
                (((lWinControl is TCustomEdit) and
                  not TCustomEditAccessor(lWinControl).ReadOnly) or
                 ((lWinControl is TComboBox) and
                  (TComboBox(lWinControl).Style in [csDropDown, csSimple])) or
                 ((lWinControl is TCustomGrid) and
                  (goEditing in TCustomGridAccessor(lWinControl).Options))
                )
    else
      // Check the registered components and clipboard formats
      for lFormat := 0 to Clipboard.FormatCount - 1 do
        if TClipboardCapability(DropComponentList.Objects[lIdx]).IsFormatSupported(
            Clipboard.Formats[lFormat]) then
        begin
          if Clipboard.Formats[lFormat] = CF_JNCCDATA then begin
            // Format available is supported - need to check table on clipboard
            lDropData := TDropData.Create;
            lDropData.ReadFromGlobalMemory(Clipboard.GetAsHandle(CF_JNCCDATA));
  
            Result := CheckTableSupported(lIdx, lDropData.Header.TableName);
          end else // if clipboard.formats
            // Other formats - can accept
            Result := True;
        end; // if TClipboardcapability
  end;
end;  // TBaseDragFrame.GetPasteEnabled 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.InternalRegisterDropComponent(AWinControl: TWinControl; ADropEvent: 
    TDataDroppedEvent; AAdvancedDropEvent: TAdvancedDataDroppedEvent; ATableList: array of 
    String; AFormatList: array of Integer; ADragOverCheck: TDragOverCheckEvent);
var
  lResult: HResult;
  lDropTarget: TJNCCDropTarget;
  lClipboardCapability: TClipboardCapability;
  lDropControl: TWinControl;
begin
  if Assigned(AWinControl) then begin
    // Create a drop target object
    if Assigned(ADropEvent) then
      lDropTarget := TJNCCDropTarget.Create(AWinControl, AFormatList,
          ATableList,  ADropEvent, ADragOverCheck)
    else
      lDropTarget := TJNCCDropTarget.Create(AWinControl, AFormatList,
          ATableList,  AAdvancedDropEvent, ADragOverCheck);
    // API call to set up OLE drag drop for the control
    lDropControl := AWinControl;
    if AWinControl is TLinkedEdit then
      lDropControl := TLinkedEdit(AWinControl).EditBox;
    lResult := RegisterDragDrop(lDropControl.Handle, lDropTarget as IDropTarget);
    // Remember so we can revoke drag and drop. RevokeDragDrop works on registered control.
    FRegisteredDragDropComponents.Add(lDropControl);
    if lResult <> S_OK then
      raise EBaseDragFrameError.Create(ResStr_DragDropRegFailed + AWinControl.Name +
                                       #13'Result: ' + IntToStr(lResult));
  
    // NB The drop target object is reference counted so we don't need to free it
    // Setup the clipboard capability list
    if Assigned(ADropEvent) then
      lClipboardCapability := TClipboardCapability.Create(ATableList, AFormatList,
          ADropEvent)
    else
      lClipboardCapability := TClipboardCapability.Create(ATableList, AFormatList,
          AAdvancedDropEvent);
    FDropComponentList.AddObject(AWinControl.Name, lClipboardCapability);
  end;
  UpdateCopyPaste(GetActiveControl(Self));
end;  // TBaseDragFrame.InternalRegisterDropComponent 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.RegisterCopyPasteComponent(AControl: TWinControl);
begin
  if Assigned(AControl) then
    FDragComponentList.Add(AControl.Name);
  UpdateCopyPaste(GetActiveControl(Self));
end;  // TBaseDragFrame.RegisterCopyPasteComponent 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.RegisterDragComponent(AWinControl: TWinControl; ADragEvent: 
    TDataDraggedEvent);
var
  lEventContainer: TDragEventContainer;
begin
  if Assigned(AWinControl) then begin
    lEventContainer := TDragEventContainer.Create(ADragEvent);
    FDragComponentList.AddObject(AWinControl.Name, lEventContainer);
  
    //Remember the old events
    if Assigned(TControlAccessor(AWinControl).OnMouseDown) then
      FEventList.AddObject(TControlAccessor(AWinControl).Name + 'MouseDown',
              TMouseEventContainer.Create(TControlAccessor(AWinControl).OnMouseDown));
  
    if Assigned(TControlAccessor(AWinControl).OnMouseUp) then
      FEventList.AddObject(TControlAccessor(AWinControl).Name + 'MouseUp',
              TMouseEventContainer.Create(TControlAccessor(AWinControl).OnMouseUp));
  
    TControlAccessor(AWinControl).DragMode    := dmManual;
    TControlAccessor(AWinControl).OnMouseDown := DragControlMouseDown;
    TControlAccessor(AWinControl).OnMouseUp   := DragControlMouseUp;
  end;
  UpdateCopyPaste(GetActiveControl(Self));
end;  // TBaseDragFrame.RegisterDragComponent 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.RegisterDragDropComponents;
begin
  // Do nothing in base class, but don't force descendants to implement it either.
end;  // TBaseDragFrame.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
  Registration of a drop component where differentiation between paste and drop operations are 
      required.
}
procedure TBaseDragFrame.RegisterDropComponent(AWinControl: TWinControl; ADropEvent: 
    TAdvancedDataDroppedEvent; ATableList: array of String; AFormatList: Array of Integer; 
    ADragOverCheck: TDragOverCheckEvent=nil);
begin
  InternalRegisterDropComponent(AWinControl, nil, ADropEvent, ATableList,
      AFormatList, ADragOverCheck);
end;  // TBaseDragFrame.RegisterDropComponent 

{-------------------------------------------------------------------------------
  Default registration of a drop component
}
procedure TBaseDragFrame.RegisterDropComponent(AWinControl: TWinControl; ADropEvent: 
    TDataDroppedEvent; ATableList: array of String; AFormatList: Array of Integer; 
    ADragOverCheck: TDragOverCheckEvent=nil);
begin
  InternalRegisterDropComponent(AWinControl, ADropEvent, nil, ATableList,
      AFormatList, ADragOverCheck);
end;  // TBaseDragFrame.RegisterDropComponent 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.SetDragDropColours;
var
  i: Integer;
  lSourceColour, lDestColour: TColor;
begin
  if AppSettings.DisableDragDropFrames then begin
    lSourceColour := Color;
    lDestColour   := Color;
  end else begin
    lSourceColour := AppSettings.DragSourceColour;
    lDestColour   := AppSettings.DragDestinationColour;
  end;
  
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TPanel then begin
      case Components[i].Tag of
        1 : TPanel(Components[i]).Color := lSourceColour;
        2 : TPanel(Components[i]).Color := lDestColour;
      end  // case
    end else
    if Components[i] is TShape then begin
      TShape(Components[i]).Pen.Style   := psSolid;
      case Components[i].Tag of
        1 : TShape(Components[i]).Pen.Color := lSourceColour;
        2 : TShape(Components[i]).Pen.Color := lDestColour;
        3 : begin
              TShape(Components[i]).Brush.Color := lSourceColour;
              TShape(Components[i]).Pen.Color   := lDestColour;
              TShape(Components[i]).Pen.Style   := psDash;
            end;
      end  // case
    end else
    if Components[i] is TTabSheet then begin
      case Components[i].Tag of
        1 : TTabSheet(Components[i]).Brush.Color := lSourceColour;
        2 : TTabSheet(Components[i]).Brush.Color := lDestColour;
      end // case
    end else
    if Components[i] is TBaseCompositeComponent then begin
      TBaseCompositeComponent(Components[i]).DragSourceColour := lSourceColour;
      TBaseCompositeComponent(Components[i]).DragDestinationColour := lDestColour;
    end;
end;  // TBaseDragFrame.SetDragDropColours 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.SetupDragDropComponents;
begin
  if not FDragDropComponentsRegistered then
    RegisterDragDropComponents;
  FDragDropComponentsRegistered := True;
end;  // TBaseDragFrame.SetupDragDropComponents 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.StartDragDrop(Sender: TWinControl);
var
  lDropSource: TJNCCDropSource;
  lEffect: Integer;
  i: Integer;
  lEventIdx: Integer;
begin
  inherited;
  try
    with Sender do begin
      EndDrag(False);
      Invalidate;  // make sure Delphi drag cursor cleared from screen
      i := FDragComponentList.IndexOf(Name);
      if i = -1 then
        raise EBaseDragFrameError.Create(ResStr_NotRegistered);
      lDropSource := TJNCCDropSource.Create(TWinControl(Sender));
      if Assigned(FDragComponentList.Objects[i]) then
        TDragEventContainer(FDragComponentList.Objects[i]).Event(Sender, lDropSource);
    end;
    if lDropSource.DropData.Header.ItemCount > 0 then begin
      DoDragDrop(lDropSource as IDataObject,
                 lDropSource as IDropSource,
                 DROPEFFECT_COPY,
                 lEffect);
    end;
  finally
    // Remove MouseMove handler
    lEventIdx := FEventList.IndexOf(Sender.Name + 'MouseMove');
    if lEventIdx >= 0 then
      TControlAccessor(Sender).OnMouseMove :=
          TMouseMoveEventContainer(FEventList.Objects[lEventIdx]).Event
    else
      TControlAccessor(Sender).OnMouseMove :=nil;
  end;
end;  // TBaseDragFrame.StartDragDrop 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.UnregisterDragDropComponents;
var
  i: Integer;
begin
  { Walk the drop components list to free associated objects }
  if Assigned(FRegisteredDragDropComponents) then
    with FRegisteredDragDropComponents do begin
      for i := 0 to Count-1 do
        try
          RevokeDragDrop(TWinControl(Items[i]).Handle);
        except
          on Exception do ; // will cause a memory leak but nothing else
        end;
      Clear;
    end;
end;  // TBaseDragFrame.UnregisterDragDropComponents 

{-------------------------------------------------------------------------------
}
procedure TBaseDragFrame.UpdateCopyPaste(AWinControl: TWinControl);
begin
  // Use PasteEnabled property instead.
end;  // TBaseDragFrame.UpdateCopyPaste 

{-------------------------------------------------------------------------------
  Handle the message to trigger initialisation of drag and drop.  This is delayed till the 
      frame is completely ready.
}
procedure TBaseDragFrame.WMSetupDragDrop(var Message: TMessage);
begin
  SetupDragDropComponents;
end;  // TBaseDragFrame.WMSetupDragDrop 

{-==============================================================================
    TDragEventContainer
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TDragEventContainer.Create(AEvent: TDataDraggedEvent);
begin
  FEvent := AEvent;
end;  // TDragEventContainer.Create 

{-==============================================================================
    TMouseEventContainer
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TMouseEventContainer.Create(AEvent: TMouseEvent);
begin
  FEvent := AEvent;
end;  // TMouseEventContainer.Create 

{-==============================================================================
    TMouseMoveEventContainer
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TMouseMoveEventContainer.Create(AEvent: TMouseMoveEvent);
begin
  FEvent := AEvent;
end;  // TMouseMoveEventContainer.Create 


end.











