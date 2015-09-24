//==============================================================================
//  Unit:        BaseDragFormUnit
//
//  Implements:  TBaseDragForm
//               TDragEventContainer
//
//  Description: Base form for drag/drop and copy/paste handling
//
//               TDragEventContainer
//               Simple event container so we can store event easily on a
//               string list
//
//  Author:      John van Breda
//  Created:     21 April 1999
//
//  Changes:     Eric Salmon - 19 March 2002
//               Refresh Copy/Paste when focus moves to another control.
//
//  Last Revision Details:
//    $Revision: 50 $
//    $Date: 20/12/07 11:01 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BaseDragFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DropTarget, ActiveX, DropStruct, DropSource, StdCtrls, ExceptionForm, DBCtrls,
  Constants, Htmlview, Grids, RapTree;

type
  EBaseDragFormError = class(TExceptionPath);

  { The following class is a hack which allows us to access the protected
       OnEnter and OnExit stuff in TWinControl }
  TDummyWinControl = class(TWinControl);

  { As above for TControl.OnStartDrag }
  TDummyControl = class(TControl);

  { and allow access to the in place editor }
  TDummyCustomGrid = class(TCustomGrid);

  { Simple event containers so we can store events easily on a string list }
  TDragEventContainer = class(TObject)
  private
    FEvent : TDataDraggedEvent;
  public
    constructor Create( iEvent : TDataDraggedEvent );
    property Event : TDataDraggedEvent read FEvent;
  end;

  TMouseEventContainer = class
  private
    FEvent : TMouseEvent;
  public
    constructor Create( iEvent : TMouseEvent );
    property Event : TMouseEvent read FEvent;
  end;

  TMouseMoveEventContainer = class
  private
    FEvent : TMouseMoveEvent;
  public
    constructor Create( iEvent : TMouseMoveEvent );
    property Event : TMouseMoveEvent read FEvent;
  end;
  //end of event containers

  //----------------------------------------------------------------------------

type
  TBaseDragForm = class(TForm)
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    FDropComponentClipboardList : TStringlist;
    FDragComponentList : TStringList;
    FDragStartPos: TPoint;
    FDropComponentList : TList;
    FEventList: TStringList; //list for storing the events assigned by the developer to
    FIsCut: Boolean;
                             //drag controls
    procedure InternalRegisterDropComponent(AWinControl: TWinControl; ADropEvent:
      TDataDroppedEvent; AAdvancedDropEvent: TAdvancedDataDroppedEvent; ATableList: Array of
      String; AFormatList: Array of Integer; ADragOverCheck: TDragOverCheckEvent);
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMSetupDragDrop(var Message: TMessage); message WM_SETDRAGDROP;
   protected
    procedure StartDragDrop(Sender: TWinControl);
    procedure DoStandardCopy(iWinControl: TWinControl);
    procedure DoStandardPaste(iWinControl: TWinControl);
    procedure DragControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DragControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DragControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function GetDropComponentIndex(iComponent: TObject): Integer;
    procedure SetupDestinationControls; virtual;
    procedure UpdateCopyPaste(AControl: TWinControl);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteCopy(Sender: TObject; AIsCut: Boolean = False);
    procedure ExecutePaste(Sender: TObject);
    procedure RegisterCopyPasteComponent(iControl:TWinControl);
    procedure RegisterDragComponent(iControl: TWinControl;
                                    iDragEvent: TDataDraggedEvent);
    procedure RegisterDropComponent(iControl: TWinControl;
                                    iDropEvent: TDataDroppedEvent;
                                    iTableList: array of String;
                                    iFormatList: array of Integer);
    procedure RegisterDropComponentAdvanced(AControl: TWinControl;
                                            AAdvancedDropEvent: TAdvancedDataDroppedEvent;
                                            ATableList: Array of String;
                                            AFormatList : Array of Integer;
                                            ADragOverCheck: TDragOverCheckEvent);
    procedure UnRegisterDragDropComponent(AWinControl: TWinControl); 
    procedure UnRegisterDragDropComponents(AWinControls: Array of TWinControl);
    property DropComponentList: TStringList read FDropComponentClipboardList;
    property DragComponentList: TStringList read FDragComponentList;
    property IsCut: Boolean read FIsCut;
  end;

//var
//  BaseDragForm : TBaseDragForm;

//==============================================================================
implementation

{$R *.DFM}

uses
  Clipbrd, FormActions, Maintbar;

const
  ST_MOUSEUP   = 'MouseUp';
  ST_MOUSEDOWN = 'MouseDown';
  ST_MOUSEMOVE = 'MouseMove';

resourcestring
  ResStr_ConstructError = 'Error occurred in TBaseDragForm constructor';
  
type
  { The following class is a hack which allows us to access the protected
       ReadOnly property in TCustomEdit}
  TDummyCustomEdit = class(TCustomEdit);

//==============================================================================
constructor TBaseDragForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropComponentClipboardList := TStringList.Create;
  FDragComponentList := TStringList.Create;
  FDropComponentList := TList.Create;
  FEventList := TStringList.Create;

  { When everything else is ready - setup drag and drop }
  PostMessage(Self.Handle, WM_SETDRAGDROP, 0, 0);
end;  // Create

//==============================================================================
destructor TBaseDragForm.Destroy;
var
  i : integer;
begin
  { Walk the drag components list to free associated objects }
  if FDragComponentList <> nil then // safety in case constructor failed
    for i := 0 to FDragComponentList.Count - 1 do
      FDragComponentList.Objects[i].Free;
  FreeAndNil(FDragComponentList);

  { Walk the drop components list to free associated objects }
  if FDropComponentList <> nil then
  begin
    for i := 0 to FDropComponentClipboardList.Count - 1 do
      FDropComponentClipboardList.Objects[i].Free;
    FreeAndNil(FDropComponentClipboardList);
    for i := 0 to FDropComponentList.Count - 1 do
      try
        RevokeDragDrop(TWinControl(FDropComponentList[i]).Handle);
      except
        on Exception do ; // will cause a memory leak but nothing else
      end;
    FreeAndNil(FDropComponentList);
  end;

  if FEventList <> nil then // safety in case constructor failed
    for i := 0 to FEventList.Count - 1 do
      FEventList.Objects[i].Free;
  FreeAndNil(FEventList);

  inherited Destroy;
end;  // Destroy

//==============================================================================
{ Message handler for our custom message WM_SETDRAGDROP.  Calls
     SetupDestinationControls which should be inherited in the derived forms }
procedure TBaseDragForm.WMSetupDragDrop(var Message: TMessage);
begin
  SetupDestinationControls;
end;  // WMSetupDragDrop

//==============================================================================
procedure TBaseDragForm.StartDragDrop(Sender: TWinControl);
var
  lDropSource  : TJNCCDropSource;
  lEffect, lIdx: Integer;
begin
  inherited;
  try
    with Sender do
    begin
      EndDrag(False);
      Invalidate;  // make sure Delphi drag cursor cleared from screen
      lIdx := FDragComponentList.IndexOf(Name);
      if lIdx = -1 then
        raise EBaseDragFormError.Create(ResStr_NotRegistered);
      lDropSource := TJNCCDropSource.Create(TWinControl(Sender));
      if Assigned(FDragComponentList.Objects[lIdx]) then
        TDragEventContainer(FDragComponentList.Objects[lIdx]).Event(Sender, lDropSource);
    end;

    if lDropSource.DropData.Header.ItemCount > 0 then
      DoDragDrop(lDropSource as IDataObject,
                 lDropSource as IDropSource,
                 DROPEFFECT_COPY,
                 lEffect);
  finally
    // Remove MouseMove handler
    lIdx := FEventList.IndexOf(Sender.Name + 'MouseMove');
    if lIdx >= 0 then
      TDummyControl(Sender).OnMouseMove := TMouseMoveEventContainer(FEventList.Objects[lIdx]).Event
    else
      TDummyControl(Sender).OnMouseMove := nil;
  end;
end;  // WMStartDragDrop

//==============================================================================
{ Find the component's position in the list of drop components. }
function TBaseDragForm.GetDropComponentIndex(iComponent: TObject): Integer;
begin
  Result := -1;
  if Assigned(FDropComponentClipboardList) then begin
    if not (iComponent is TComponent) then
      raise EBaseDragFormError.Create(ResStr_NotComponent);
    Result := FDropComponentClipboardList.IndexOf(TComponent(iComponent).Name);
  end;
end;  // GetDropComponentIndex

//==============================================================================
procedure TBaseDragForm.InternalRegisterDropComponent(AWinControl: TWinControl; ADropEvent:
  TDataDroppedEvent; AAdvancedDropEvent: TAdvancedDataDroppedEvent; ATableList: Array of String;
  AFormatList: Array of Integer; ADragOverCheck: TDragOverCheckEvent);
var
  lResult              : HResult;
  lDropTarget          : TJNCCDropTarget;
  lClipboardCapability : TClipboardCapability;
begin
  if Assigned(AWinControl) then begin
    // Create a drop target object
    if Assigned(ADropEvent) then
      lDropTarget := TJNCCDropTarget.Create(AWinControl, AFormatList, ATableList,
                                            ADropEvent, ADragOverCheck)
    else
      lDropTarget := TJNCCDropTarget.Create(AWinControl, AFormatList, ATableList,
                                            AAdvancedDropEvent, ADragOverCheck);

    { API call to set up OLE drag drop for the control }
    lResult := RegisterDragDrop(AWinControl.Handle, lDropTarget as IDropTarget);
    FDropComponentList.Add(AWinControl); // remember so we can revoke drag and drop
    if lResult <> S_OK then
      raise EBaseDragFormError.Create(ResStr_DragDropRegFailed + AWinControl.Name +
                                      ', Result : ' + IntToStr(lResult));

    // NB The drop target object is reference counted so we don't need to free it
    // Setup the clipboard capability list
    if Assigned(ADropEvent) then
      lClipboardCapability := TClipboardCapability.Create(ATableList, AFormatList, ADropEvent)
    else
      lClipboardCapability := TClipboardCapability.Create(ATableList, AFormatList, AAdvancedDropEvent);
    FDropComponentClipboardList.AddObject(AWinControl.Name, lClipboardCapability);
  end;
  UpdateCopyPaste(ActiveControl);
end;  // InternalRegisterDropComponent

{-------------------------------------------------------------------------------
  Procedure called by derived classes to register a component as a drop
  destination.  Note that the sequence iFormatList is provided in indicates
  the order of preference if multiple formats are available.
  The DragOverCheck event allows a control to handle the acceptance of drop and perform
  additional processing, if required.
}
procedure TBaseDragForm.RegisterDropComponentAdvanced(AControl: TWinControl;
  AAdvancedDropEvent: TAdvancedDataDroppedEvent; ATableList: Array of String;
  AFormatList : Array of Integer; ADragOverCheck: TDragOverCheckEvent);
begin
  InternalRegisterDropComponent(AControl, nil, AAdvancedDropEvent, ATableList, AFormatList,
                                ADragOverCheck);
end;  // RegisterDropComponentAdvanced

{-------------------------------------------------------------------------------
 Procedure called by derived classes to register a component as a drop
 destination.  Note that the sequence iFormatList is provided in indicates
 the order of preference if multiple formats are available.
}
procedure TBaseDragForm.RegisterDropComponent(iControl: TWinControl; iDropEvent :
  TDataDroppedEvent; iTableList: array of String; iFormatList : array of Integer);
begin
  InternalRegisterDropComponent(iControl, iDropEvent, nil, iTableList, iFormatList, nil);
end;

//==============================================================================
procedure TBaseDragForm.RegisterDragComponent(iControl: TWinControl;
  iDragEvent: TDataDraggedEvent);
var
  lEventContainer: TDragEventContainer;
  lCtrl: TDummyControl;
begin
  if Assigned(iControl) then begin
    lEventContainer := TDragEventContainer.Create(iDragEvent);
    FDragComponentList.AddObject(iControl.Name, lEventContainer);

    lCtrl := TDummyControl(iControl);
    //Remember the old events
    if Assigned(lCtrl.OnMouseDown) then
      FEventList.AddObject(lCtrl.Name + ST_MOUSEDOWN,
                           TMouseEventContainer.Create(lCtrl.OnMouseDown));

    if Assigned(lCtrl.OnMouseUp) then
      FEventList.AddObject(lCtrl.Name + ST_MOUSEUP,
                           TMouseEventContainer.Create(lCtrl.OnMouseUp));

    lCtrl.DragMode := dmManual;
    lCtrl.OnMouseDown := DragControlMouseDown;
    lCtrl.OnMouseUp := DragControlMouseUp;
  end;
  UpdateCopyPaste(ActiveControl);
end;  // RegisterDragcomponent

//==============================================================================
procedure TBaseDragForm.RegisterCopyPasteComponent(iControl:TWinControl);
begin
  if Assigned(iControl) then
    FDragComponentList.Add(iControl.Name);
  UpdateCopyPaste(ActiveControl);
end;  // RegisterCopyPasteComponent

//==============================================================================
{ Overridable procedure called by the constructor to set up the information
     destination controls.  Not virtual, despite being empty, so we don't get
     an error if it is not implemented in the derived class. }
procedure TBaseDragForm.SetupDestinationControls;
begin
  { Yes- really do nothing!}
end;  // SetupDestinationControls

//==============================================================================
procedure TBaseDragForm.ExecutePaste(Sender: TObject);
var
  i, lIndex: integer; // index in the component list
  lCapability: TClipboardCapability;
begin
  if Sender is TWinControl then begin
    lIndex := GetDropComponentIndex(Sender);
    if lIndex = -1 then
      DoStandardPaste(TWinControl(Sender))
    else begin
      lCapability := TClipboardCapability(FDropComponentClipboardList.Objects[lIndex]);
      for i := 0 to Clipboard.FormatCount - 1 do
        if lCapability.IsFormatSupported(Clipboard.Formats[i]) then
          lCapability.ActOnGlobalMemory(Clipboard.GetAsHandle(Clipboard.Formats[i]),
                                        Clipboard.Formats[i],
                                        TControl(Sender));
    end;
    UpdateCopyPaste(TWinControl(Sender));
  end;
end;  // ExecutePaste

//==============================================================================
procedure TBaseDragForm.ExecuteCopy(Sender: TObject; AIsCut: Boolean = False);
var
  lIndex      : integer;
  lDropSource : TJNCCDropSource;
  lHandle     : THandle;
  lstDropText : string;
begin
  FIsCut := AIsCut;
  if Sender is TWinControl then begin
    lIndex := FDragComponentList.IndexOf(TWinControl(Sender).Name);
    if lIndex = -1 then
      DoStandardCopy(TWinControl(Sender))
    else begin
      lDropSource := TJNCCDropSource.Create(TWinControl(Sender));
      try
        if Assigned(FDragComponentList.Objects[lIndex]) then
          TDragEventContainer(FDragComponentList.Objects[lIndex]).Event(Sender, lDropSource);
        { Read the clipboard data required as text }
        lstDropText := lDropSource.GetText;
        { and now JNCCDATA }
        lDropSource.DropData.WriteToGlobalMemory( lHandle );
        with Clipboard do begin
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
    UpdateCopyPaste(TWinControl(Sender));
  end;
end;  // ExecuteCopy

//==============================================================================
procedure TBaseDragForm.FormActivate(Sender: TObject);
begin
  inherited;
  UpdateCopyPaste(ActiveControl);
end;  // FormActivate

//==============================================================================
{ Copy a control which does not feature the CF_JNCCDATA format }
procedure TBaseDragForm.DoStandardCopy(iWinControl: TWinControl);
var
  editor: TInPlaceEdit;
  lItems: String;
  i: Integer;
begin
  { TCustomEdit covers all descendants, eg TEdit, TRichEdit, TMemo }
  if iWinControl is TCustomEdit then
    TEdit(iWinControl).CopyToClipboard
  else
  if iWinControl is THTMLViewer then
    // If something selected, copy only the selection to the clipboard
    with THTMLViewer(iWinControl) do
      if SelLength <> 0 then
        CopyToClipboard
      else begin
        // Otherwise, copy everything
        SelectAll;
        CopyToClipboard;
        SelLength := 0;
      end
  else
  if iWinControl is TComboBox then
    SendMessage(iWinControl.Handle, WM_COPY, 0, 0)
  else
  if iWinControl is TCustomGrid then begin
    editor := TDummyCustomGrid(iWinControl).InplaceEditor;
    if Assigned(editor) then SendMessage(editor.Handle,WM_COPY,0,0)
  end else
  if iWinControl is TRapidTree then
    Clipboard.AsText := TRapidTree(iWinControl).Selected.Text
  else
  if (iWinControl is TListBox) and (iWinControl.Tag<>-1) then
    with TListBox(iWinControl) do begin
      if ItemIndex <> -1 then begin
        lItems := '';
        for i := 0 to Items.Count - 1 do
          if Selected[i] then lItems := lItems + Items[i] + #13#10;
        Clipboard.AsText := lItems;
      end;
    end
  {$IFDEF DEBUG}
  else
    Raise TExceptionPath.CreateNonCritical(ResStr_ComponentCantCopy + iWinControl.Name)
  {$ENDIF};
end;  // DoStandardCopy

//==============================================================================
{ Paste a control which does not feature the CF_JNCCDATA format }
procedure TBaseDragForm.DoStandardPaste(iWinControl: TWinControl);
var
  editor: TInplaceEdit;
begin
  { TCustomEdit covers all descendants, eg TEdit, TRichEdit, TMemo }
  if iWinControl is TCustomEdit then
    TCustomEdit(iWinControl).PasteFromClipboard
  else
  if (iWinControl is TComboBox) then
    SendMessage(iWinControl.Handle, WM_PASTE, 0, 0)
  else
  if (iWinControl is TCustomGrid) then begin
    editor := TDummyCustomGrid(iWinControl).InPlaceEditor;
    if Assigned(editor) then SendMessage(editor.Handle, WM_PASTE, 0, 0)
  end
  {$IFDEF DEBUG}
  else
    raise TExceptionPath.CreateNonCritical(ResStr_ComponentCantPaste + iWinControl.Name)
  {$ENDIF};
end;  // DoStandardPaste

//==============================================================================
{Initiate drag}
procedure TBaseDragForm.DragControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ltfCanDrag: Boolean;
    loldEvent : TMouseEvent;
    lIdx: integer;
    lCtrl: TDummyControl;
begin
  ltfCanDrag := true;
  // Don't want to start dragging if resizing columns only!!!
  if (Sender is TCustomGrid) then
    with TDummyCustomGrid(Sender) do
      if (FixedRows > 0) and (MouseCoord(X, Y).Y < FixedRows) then ltfCanDrag := false;

  // Set drag values only if left mouse button down
  if ltfCanDrag and (Button = mbLeft) then
  begin
    //Set drag start position
    FDragStartPos.X:= X;
    FDragStartPos.Y:= Y;
    //Set mouse move cursor to check for drag
    lCtrl := TDummyControl(Sender);
    if Assigned(lCtrl.OnMouseMove) then
      FEventList.AddObject(lCtrl.Name + ST_MOUSEMOVE,
                           TMouseMoveEventContainer.Create(lCtrl.OnMouseMove));
    lCtrl.OnMouseMove:= DragControlMouseMove;
  end;

  lIdx := FEventList.IndexOf((Sender as TControl).Name + ST_MOUSEDOWN);
  if lIdx >= 0 then
  begin
    loldEvent := TMouseEventContainer(FEventList.Objects[lIdx]).Event;
    loldEvent(Sender, Button,Shift, X,Y);
  end;
end;  // DragControlMouseDown

//==============================================================================
{Start dragging if mouse moved far enough. Event called only when dragging,
 as it is set in MouseDown event only if Left button pressed}
procedure TBaseDragForm.DragControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  //Check to see if mouse has moved far enough to start dragging
  if ((Abs(FDragStartPos.X - X) >= DRAG_THRESHOLD) or
      (Abs(FDragStartPos.Y - Y) >= DRAG_THRESHOLD)) or
     (Sender is TListbox) and (ssLeft in Shift) then
  begin
    //Initiate drag
    if Sender is TWinControl then
      StartDragDrop(TWinControl(Sender));
  end;
end;  // DragControlMouseMove

//==============================================================================
{Stop checking for drag}
procedure TBaseDragForm.DragControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var loldEvent : TMouseEvent;
    lIdx: integer;
begin
  // Remove MouseMove handler
  lIdx := FEventList.IndexOf((Sender as TControl).Name + ST_MOUSEMOVE);
  if lIdx >= 0 then
    TDummyControl(Sender).OnMouseMove := TMouseMoveEventContainer(FEventList.Objects[lIdx]).Event
  else
    TDummyControl(Sender).OnMouseMove := nil;

  //Execute the user's event
  lIdx := FEventList.IndexOf((Sender as TControl).Name + ST_MOUSEUP);
  if lIdx >= 0 then
  begin
    loldEvent := TMouseEventContainer(FEventList.Objects[lIdx]).Event;
    loldEvent(Sender, Button, Shift, X,Y);
  end;
end;  // DragControlMouseUp

//==============================================================================
procedure TBaseDragForm.CMFocusChanged(var Message: TCMFocusChanged);
begin
  UpdateCopyPaste(ActiveControl);
  // Broadcast message to ActiveControl's owned controls
  Broadcast(Message);
end;

{-------------------------------------------------------------------------------
}
procedure TBaseDragForm.UnRegisterDragDropComponent(AWinControl: TWinControl);
var
  lIdx: Integer;
  lCtrl: TDummyControl;
begin
  if Assigned(AWinControl) then begin
    lCtrl := TDummyControl(AWinControl);
    // Remove related items from FEventList. Restore events before removing.
    lIdx := FEventList.IndexOf(lCtrl.Name + ST_MOUSEMOVE);
    if lIdx <> -1 then begin
      lCtrl.OnMouseMove := TMouseMoveEventContainer(FEventList.Objects[lIdx]).Event;
      FEventList.Objects[lIdx].Free;
      FEventList.Delete(lIdx);
    end;

    lIdx := FEventList.IndexOf(AWinControl.Name + ST_MOUSEDOWN);
    if lIdx <> -1 then begin
      lCtrl.OnMouseDown := TMouseEventContainer(FEventList.Objects[lIdx]).Event;
      FEventList.Objects[lIdx].Free;
      FEventList.Delete(lIdx);
    end;

    lIdx := FEventList.IndexOf(AWinControl.Name + ST_MOUSEUP);
    if lIdx <> -1 then begin
      lCtrl.OnMouseUp := TMouseEventContainer(FEventList.Objects[lIdx]).Event;
      FEventList.Objects[lIdx].Free;
      FEventList.Delete(lIdx);
    end;

    // Remove related items from FDragComponentList.
    lIdx := FDragComponentList.IndexOf(lCtrl.Name);
    if lIdx <> -1 then begin
      FDragComponentList.Objects[lIdx].Free;
      FDragComponentList.Delete(lIdx);
    end;

    // Remove related items from FDropComponentClipboardList
    lIdx := FDropComponentClipboardList.IndexOf(lCtrl.Name);
    if lIdx <> -1 then begin
      FDropComponentClipboardList.Objects[lIdx].Free;
      FDropComponentClipboardList.Delete(lIdx);
    end;

    // Remove related items from FDropComponentList.
    lIdx := FDropComponentList.IndexOf(lCtrl);
    if lIdx <> -1 then begin
      try
        RevokeDragDrop(AWinControl.Handle);
      except
        on Exception do; // Will cause a memory leak but nothing else
      end;
      FDropComponentList.Delete(lIdx);
    end;
  end;
end;  // TBaseDragForm.UnRegisterDragDropComponent

{-------------------------------------------------------------------------------
}
procedure TBaseDragForm.UnRegisterDragDropComponents(AWinControls: Array of TWinControl);
var
  i: Integer;
begin
  for i := 0 to High(AWinControls) do UnRegisterDragDropComponent(AWinControls[i]);
end;  // TBaseDragForm.UnRegisterDragDropComponents

//==============================================================================
procedure TBaseDragForm.UpdateCopyPaste(AControl: TWinControl);
var liFormat, lIdx: Integer;
    lDropData     : TDropData;
    lDragForm     : TBaseDragForm;
    lCapability   : TClipboardCapability;
    lUnavailable  : Boolean;
begin
  if AControl <> nil then begin
    dmFormActions.actPaste.Enabled := False;  // default

    if not (AControl.Owner is TBaseDragForm) then
      lDragForm := Self
    else
      lDragForm := TBaseDragForm(AControl.Owner);

    lIdx := lDragForm.GetDropComponentIndex(AControl);

    // Handle drop/paste stuff
    if lIdx = -1 then
      // Clipboard must have some text to have Paste enabled in this case
      dmFormActions.actPaste.Enabled := Clipboard.HasFormat(CF_TEXT) and
                                        (((AControl is TCustomEdit) and
                                          not TDummyCustomEdit(AControl).ReadOnly) or
                                         ((AControl is TComboBox) and
                                          (TComboBox(AControl).Style in [csDropDown, csSimple])) or
                                         ((AControl is TDBComboBox) and
                                          not TDBComboBox(AControl).ReadOnly and
                                          (TDBComboBox(AControl).Style in [csDropDown, csSimple])) or
                                         ((AControl is TCustomGrid) and
                                          (goEditing in TDummyCustomGrid(AControl).Options)))
    else
    begin
      lUnavailable := (not AControl.Enabled)
          or ((AControl is TCustomEdit) and TDummyCustomEdit(AControl).ReadOnly)
          or ((AControl is TDBComboBox) and TDBComboBox(AControl).ReadOnly);

      if not lUnavailable then
      begin
        lCapability :=
            lDragForm.DropComponentList.Objects[lIdx] as TClipboardCapability;

        // Check the registered components and clipboard formats
        for liFormat := 0 to Clipboard.FormatCount - 1 do
          if lCapability.IsFormatSupported(Clipboard.Formats[liFormat]) then
          begin
            if Clipboard.Formats[liFormat] <> CF_JNCCDATA then
              // Other formats - can accept
              dmFormActions.actPaste.Enabled := True
            else
            begin
              // Format available is supported - need to check table on clipboard
              lDropData := TDropData.Create;
              try
                lDropData.ReadFromGlobalMemory(Clipboard.GetAsHandle(CF_JNCCDATA));

                if lCapability.IsTableSupported(lDropData.Header.TableName) then
                  dmFormActions.actPaste.Enabled := True;
              finally
                lDropData.Free;
              end;
            end; // if clipboard.formats
          end; // if lCapability.IsFormatSupported
      end;
    end;  // if not lUnavailable

    dmFormActions.actCopy.Enabled := (AControl is TCustomEdit)
        or (AControl is TComboBox)
        or (AControl is TDBComboBox)
        or (AControl is THTMLViewer)
        or (AControl is TCustomGrid)
        or (Assigned(lDragForm.DragComponentList)
            and (lDragForm.DragComponentList.IndexOf(AControl.Name) <> -1))
  end;
end;  // UpdateCopyPaste

//==============================================================================
{ TMouseEventContainer }
//------------------------------------------------------------------------------
//Simple constructor
constructor TMouseEventContainer.Create(iEvent: TMouseEvent);
begin
  FEvent:=iEvent;
end;

//==============================================================================
{ TMouseMoveEventContainer }
//------------------------------------------------------------------------------
//Simple constructor
constructor TMouseMoveEventContainer.Create(iEvent: TMouseMoveEvent);
begin
  FEvent:=iEvent;
end;

//==============================================================================
{ TDragEventContainer }
//------------------------------------------------------------------------------
constructor TDragEventContainer.Create(iEvent: TDataDraggedEvent);
begin
  try
    inherited Create;
    FEvent := iEvent;
  except
    on E:Exception do
      raise EBaseDragFormError.Create(ResStr_ConstructError, E );
  end;
end;  // Create

//==============================================================================
end.

