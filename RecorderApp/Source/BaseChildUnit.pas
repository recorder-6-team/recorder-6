//==============================================================================
//  Unit:        BaseChildUnit
//
//  Implements:  TBaseChild
//
//  Description: Defines the base class for all MDI child forms, and implements
//               common functions.
//
//  Author:
//  Created:
//
//  Last Revision Details:
//    $Revision: 84 $
//    $Date: 20/07/09 11:30 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BaseChildUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseFormUnit, Registry, ExtCtrls, StdCtrls, Grids, DataClasses, ComCtrls,
  HierarchyNodes, Constants, RapTree, TreeColl, Menus, DBListCombo,
  CRCommonClasses, Filter;

resourcestring
  ResStr_NotAllowedToEditOtherPersonsData = 'You are not allowed to edit the data in this record '+
      'because it was entered by a different user and you do not have the required '+
      'access rights to edit other people''s data.';
  ResStr_NotAllowedToEditOtherSitesData = 'You are not allowed to edit the data in this record '+
      'because it was entered on a different site and you do not have custody of the data.';
  ResStr_NotAllowedToEditData = 'You are not allowed to edit the data in this record '+
      'because you do not have the required access rights.';

type
  TCustodyDeterminer = function (const AKey: TKeyString): String of object;

  TBaseChild = class(TBaseForm)
    mnuChildMerge: TMainMenu;
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FDataFilter: TFilterCriteria;
    FMenuWindowItem: TMenuItem;
    procedure LoadSize;
    procedure LoadState;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMUpdateMenuIcons(var Msg:TMessage); message WM_UPDATE_MENU_ICONS;
    procedure MergeMenus;
    procedure RefreshWindowsState;
    procedure UnMergeMenus;
    procedure ReadPosition(const iReg: TRegistry);
  protected
    FCustodian: string;
    FEditMode: TEditMode;
    procedure DoShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo: THintInfo);
        virtual;
    function GetStateDataFromNode(const iNode: TTreeNode): TKeyDataSysSupplied; overload;
    function GetStateDataFromNode(const iNode: TFlynode): TKeyDataSysSupplied; overload;
    function AddButtonState: Boolean;
    function EditButtonState(const AStateData: TKeyDataSysSupplied; Custodian:
        TCustodyDeterminer): Boolean; overload;
    function EditButtonState (const AStateData: TKeyDataSysSupplied;
        ACustodian: string): Boolean; overload;
    function DeleteButtonState(const iStateData: TKeyDataSysSupplied): Boolean;
    procedure WindowMenuItemClick(Sender: TObject);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetDetailsPageControl: TPageControl; virtual;
    function GetTreeView: TRapidTree; virtual;

    // Override if screen implements filtering.
    procedure ApplyFilter(AKeyList: TKeyList); virtual; abstract;

    procedure ClearFilter(const AName: String); virtual;  
    function GetCurrentControlEditMode: TEditMode; virtual;
    procedure GetFilter(const AName, ATableName: String; const AAdditionalCriteria: String = '');
    procedure GetFilteredKeys; virtual;
    function  LoadFilter(const AName: String; AFirstLoad: Boolean = True): Boolean; virtual;
    procedure SaveFilter(const AName: String); virtual;
    function GetHaveCustody: Boolean; virtual;
    function GetCustodian: string; virtual;
    procedure RememberPathToSelectedNode(treeview: TRapidTree; nodeChain: TStringList);
    procedure RedisplaySelectedNode(treeview: TRapidTree; nodeChain: TStringList);
    procedure EnableSortToolbutton(AEnabled: boolean; ASortPopupMenu: TPopupMenu);
    procedure Redraw;
    property DataFilter: TFilterCriteria read FDataFilter;
    property HaveCustody: boolean read GetHaveCustody;
    property TreeView: TRapidTree read GetTreeView;

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure ApplySecurity; virtual;
    procedure CheckReenableParent;
    procedure PreviewScreen; virtual; // Called by File/Preview
    procedure PrintScreen; virtual; // Called by File/Print
    procedure ShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo: THintInfo);
    function SelectTab(const ATabName: string): boolean; virtual;
    function SelectNode(const ANodeType, ANodeKey: string): TFlyNode; virtual;
    procedure ExpandAllNodes; virtual;
    function ItemKey: string; virtual;
    function CustomReportKeyType: TKeyType; virtual;
    property Custodian: string read GetCustodian; 
    property EditMode: TEditMode read FEditMode;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Maintbar, ApplicationSettings, ReportPreview, CsProp, DB, SpatialRef,
  DBCtrls, CheckLst, ExceptionForm, GeneralFunctions, JNCCDatasets,
  DatabaseAccessADO, TypInfo;

//==============================================================================
constructor TBaseChild.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  LoadState; // do this first to avoid unnecessary zooming effects
  Application.MainForm.Refresh;
  SupplierForm    := nil;
  RequestorForm   := nil;
  RequestorUpdate := nil;
  //Application.OnShowHint := FormShowHint;
  // Add an Item to the Window menu - default MDI behaviour
  FMenuWindowItem := TMenuItem.Create(Self);
  if Active then
    FMenuWindowItem.Checked := True;
  FMenuWindowItem.Caption := ' ' + Caption; // space is required to indicate where numbering should go
  FMenuWindowItem.OnClick := WindowMenuItemClick;
  FEditMode := emView;

  with frmMain do begin
    WindowMenu.Add(FMenuWindowItem);
    mnuWindowSeparator.Visible := True;
  end;
end;  // Create

//==============================================================================
destructor TBaseChild.Destroy;
var lReg   :Tregistry;
    PosRect:TRect;
    iCount :Integer;
begin
  UnMergeMenus;
  lReg := TRegistry.Create;
  try
    if lReg.OpenKey(REG_KEY_FORMS + '\' +
                    Copy(Self.ClassName,5,Length(Self.ClassName)), True) then
    begin
      case WindowState of
        wsMinimized :
            lReg.WriteInteger('State', 1);
        wsNormal    :
            begin
              lReg.WriteInteger('State', 2);
              PosRect := Rect(Left, Top, Width, Height);
              if BorderStyle = bsSizeable then
                lReg.WriteBinaryData('Position', PosRect, SizeOf(TRect))
              else
                lReg.WriteBinaryData('Position', PosRect.TopLeft, SizeOf(TPoint));
            end;
        wsMaximized :
            lReg.WriteInteger('State', 3);
      end;  // case

      for iCount := 0 to ControlCount - 1 do
        if not (Controls[iCount] is TSplitter) and (Controls[iCount].Align = alRight) then begin
          lReg.WriteInteger('Splitter Position', Controls[iCount].Width);
          Break;
        end;
      // Writes True if form still open when main form closing.
      lReg.WriteBool('Opened', frmMain.Closing);
    end;
  finally
    lReg.CloseKey;
    lReg.Free;
  end;
  CheckReenableParent;

  // Remove any return data link before closing form
  if Assigned(SupplierForm) then
  begin
    SupplierForm.RequestorForm:= nil;
    SupplierForm.RequestorUpdate:= nil;
	  SupplierForm:= nil;
  end;
  if Assigned(RequestorForm) then
  begin
    RequestorForm.SupplierForm:= nil;
	  RequestorForm:= nil;
	  RequestorUpdate:= nil;
  end;

  if not frmMain.Closing then begin
    if frmMain.ActiveMDIChild = Self then
      frmMain.ClearContextToolbar(False);
    LockWindowUpdate(0);
  end;
  frmMain.mnuFileClose.Enabled      := False;
  frmMain.mnuFileCloseAll.Enabled   := False;
  frmMain.mnuWindowsCascade.Enabled := False;
  inherited Destroy;
  // Hide Window menu separator if no windows left }
  frmMain.mnuWindowSeparator.Visible := frmMain.MDIChildCount > 0;
end;  // Destroy

{-------------------------------------------------------------------------------
  I know this looks stupid, but there is a reason! When restoring a maximised window,
  delphi seems to get a little confused and not line things up properly (the
  right hand panel will be positioned incorrectly, hanging over the side of the
  form). Increasing then decreasing the width of the window will re-throw the
  resize event and Delphi will sort itself out.
}
procedure TBaseChild.Redraw;
begin
  Width := Width + 1;
  Width := Width - 1;
end;

//==============================================================================
procedure TBaseChild.FormActivate(Sender: TObject);
var
  lStyle : Integer;
begin
  inherited;
  with (Application.MainForm as TfrmMain) do begin
    CurrentForm := Self;
    mnuFileClose.Enabled   :=True;
    mnuFileCloseAll.Enabled:=True;
    mnuWindowsCascade.Enabled:=True;
    dmFormActions.SetActionVisibility(dmFormActions.actExport, False);
  end;
  MergeMenus;
  { Check the appropriate menu item }
  if Assigned(FMenuWindowItem) then
    FMenuWindowItem.Checked := True;
  frmMain.SetMDIButtonVisible(WindowState = wsMaximized);
  lStyle := GetWindowLong(Handle, GWL_STYLE) or WS_SYSMENU;
  SetWindowLong(Handle, GWL_STYLE, lStyle);
  Invalidate;
  LockWindowUpdate(0);
end;  // FormActivate

//==============================================================================
procedure TBaseChild.FormDeactivate(Sender: TObject);
begin
  inherited;
  with (Application.MainForm as TfrmMain) do
    if CurrentForm = Self then CurrentForm := nil;
  UnMergeMenus;
  if Assigned(FMenuWindowItem) then
    FMenuWindowItem.Checked := False;
end;  // FormDeactivate

//==============================================================================
{ Manual menu merging, because we use toolbuttons on the main form }
procedure TBaseChild.MergeMenus;
var
  lChildMenuIndex : Integer;
begin
  for lChildMenuIndex := 0 to mnuChildMerge.Items.Count-1 do
    with frmMain.tbMenu.Buttons[mnuChildMerge.Items[lChildMenuIndex].GroupIndex-1] do begin
      MenuItem := mnuChildMerge.Items[lChildMenuIndex];
      Visible := True;
    end;
end;  // MergeMenus

//==============================================================================
{ Revert frmMain's menu to its original state }
procedure TBaseChild.UnMergeMenus;
var
  lChildMenuIndex : Integer;
begin
  with frmMain do begin
    { Hide the non-standard toolbuttons }
    tbtnMnuMerge1.Visible := False;
    tbtnMnuMerge2.Visible := False;
    { Reset the standard toolbutton menus by matching button indices to the tag
          of the top level menu items }
    for lChildMenuIndex := 0 to mnuChildMerge.Items.Count-1 do begin
      with tbMenu.Buttons[mnuChildMerge.Items[lChildMenuIndex].GroupIndex-1] do
        if Visible then
          MenuItem := mnuMain.Items[Tag];
    end;
  end;
end;  // UnMergeMenus

//==============================================================================
procedure TBaseChild.FormClose(Sender: TObject; var Action: TCloseAction);
var iCount : Integer;
begin
  { close any owned preview windows - avoids QR access violations if they are auto-
      freed later.  Bizarre but True! Use while loop as count may change }
  iCount := 0;
  while iCount<ComponentCount do
    if Components[iCount] is TfrmReportPreview then
      Components[iCount].Free // this removes 1 from the list
    else
      Inc(iCount);
  inherited;
end;  // FormClose

//==============================================================================
{ FormShow appears to be the best time to load form size info from the registry,
     resulting in the least flicker }
procedure TBaseChild.FormShow(Sender: TObject);
begin
  inherited;
  LoadSize;
  // Fixes a strange bug, appearing in dictionaries, but to make sure it doesn't
  // happen anywhere else, put it here
  SetDragColours;
end;  // FormShow

//==============================================================================
{ Handle the WMSIze message.  This allows us to get accurate information about
    the window state when switching from Maximised to normal.  FormResize is too
    early to read the correct WindowState }
procedure TBaseChild.WMSize(var Message: TWMSize);
begin
  inherited;
  RefreshWindowsState;
end;

//==============================================================================
{ For an unknown reason, this method must be left here otherwise none of the
    resize code occurs.  Its theoretically redundant as this is handled by
    WMSize. }
procedure TBaseChild.FormResize(Sender: TObject);
begin
  inherited;
  RefreshWindowsState;
end;  // FormResize

//==============================================================================
procedure TBaseChild.RefreshWindowsState;
begin
  AppSettings.MaximizedChildWindows := (WindowState = wsMaximized);
  frmMain.SetMDIButtonVisible((WindowState = wsMaximized) and
                              not (csDestroying in ComponentState));
end;  // WMSize

//==============================================================================
{ Update menu icon appearnance & XP Menus when requested }
procedure TBaseChild.WMUpdateMenuIcons(var Msg: TMessage);
begin
  if AppSettings.ShowMenuIcons then
    mnuChildMerge.Images := dmFormActions.ilMenuOn
  else
    mnuChildMerge.Images := nil;
  RefreshXPMenu;
end;  // WMUpdateMenuIcons

{-------------------------------------------------------------------------------
  Public method that calls the virtual (and protected) method that formats the hint.
}
procedure TBaseChild.ShowHint(var HintStr: String; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  DoShowHint(HintStr, CanShow, HintInfo);
end;

{-------------------------------------------------------------------------------
}
procedure TBaseChild.DoShowHint(var HintStr: String; var CanShow: Boolean; var HintInfo:
    THintInfo);
var
  lCol, lRow, iIndex: Integer;
  lItemPoint: TPoint;
  lRect: TRect;
  lCellText: String;
begin
  if (HintInfo.HintControl is TStringGrid) then
    with TStringGrid(HintInfo.HintControl) do begin
      if Hint = '' then begin
        MouseToCell(HintInfo.CursorPos.X, HintInfo.CursorPos.Y, lCol, lRow);
        if (lRow <> -1) and (lCol <> -1) then begin
          // Find out if the text shown is shorter than the text stored.
          lRect := CellRect(lCol, lRow);
          lCellText := GetTextWithinLimit(Canvas, Cells[lCol, lRow],
                                          lRect.Right - lRect.Left - 4);
          if lCellText <> Cells[lCol, lRow] then begin
            HintStr := Cells[lCol, lRow];
            HintInfo.ReshowTimeout := 200;
          end else
            HintStr := '';
        end;
      end;
    end
  else
  // Display truncated list box items as hints
  if HintInfo.HintControl is TListBox then begin
    with TListBox(HintInfo.HintControl) do
      begin
        lItemPoint.X:=(Mouse.CursorPos.X)-(ClientOrigin.X);
        lItemPoint.Y:=(Mouse.CursorPos.Y)-(ClientOrigin.Y);
        iIndex:=ItemAtPos(lItemPoint,True);
        if iIndex<>-1 then begin
          if Canvas.TextWidth(Items.Strings[iIndex]) >
             (ClientRect.Right - ClientRect.Left) then begin
              HintInfo.ReShowTimeout:=200;
              HintStr:=Items.Strings[iIndex];
              CanShow:=True;
          end else
            CanShow:=False; // not necessary because item fits in box
        end;
      end; // with TListBox...
  end else begin
    // Show hint as normal
    HintInfo.ReshowTimeout := 200;
    HintStr := HintInfo.HintControl.Hint;
    CanShow := True;
  end;
end;  // FormShowHint

//==============================================================================
{ Read the positional data from the registry }
procedure TBaseChild.ReadPosition(const iReg: TRegistry);
var
  PosRect:TRect;
begin
  if WindowState=wsNormal then
  begin
    if iReg.ValueExists('Position') then begin
      if BorderStyle=bsSizeable then begin
        iReg.ReadBinaryData('Position',PosRect,SizeOf(TRect));
        Width :=PosRect.Right;
        Height:=PosRect.Bottom;
      end else
        iReg.ReadBinaryData('Position',PosRect.TopLeft,SizeOf(TPoint));
      Left:=PosRect.Left;
      Top :=PosRect.Top;
    end;
  end;
end;  // ReadPosition

//==============================================================================
{ Checks if the owner is a form, and if it is, then checks that the form is
    enabled.  If so, then the form called this form in a normal fashion so no
    action required.  Otherwise, the owner must be enabled and shown. }
procedure TBaseChild.CheckReenableParent;
begin
  if Owner is TForm then
    with TForm(Owner) do
      if not Enabled then
      begin
        Enabled := True;
        Show;
      end;
end;  // CheckReenableParent

//==============================================================================
function TBaseChild.AddButtonState: Boolean;
begin
  Result := AppSettings.UserAccessLevel in [ualAdmin,ualFullUser,ualAddOnly];
end;  // AddButtonState

//==============================================================================
function TBaseChild.EditButtonState(const AStateData: TKeyDataSysSupplied; 
    Custodian: TCustodyDeterminer): Boolean;
begin
  Result := EditButtonState(AStateData, Custodian(AStateData.ItemKey));
end;  // EditButtonState

//==============================================================================
function TBaseChild.DeleteButtonState(const iStateData: TKeyDataSysSupplied): Boolean;
begin
  Result := False;
  if Assigned(iStateData) then
    Result := (not iStateData.SysSupplied)
              and (AppSettings.UserAccessLevel in [ualAdmin,ualFullUser]);
end;  // DeleteButtonState

//==============================================================================
{ This method should be deleted once TRapidTree conversion is done }
function TBaseChild.GetStateDataFromNode(const iNode: TTreeNode): TKeyDataSysSupplied;
begin
  Result := nil;
  if Assigned(iNode) and Assigned(iNode.Data) then
    Result := TKeyDataSysSupplied(iNode.Data);
end;  // GetStateDataFromNode

//==============================================================================
function TBaseChild.GetStateDataFromNode(const iNode: TFlyNode): TKeyDataSysSupplied;
begin
  Result := nil;
  if Assigned(iNode) and Assigned(iNode.Data) then
    Result := TKeyDataSysSupplied(iNode.Data);
end;  // GetStateDataFromNode

//==============================================================================
procedure TBaseChild.LoadSize;
var lReg   :TRegistry;
    iCount,lState :Integer;
begin
  lReg := TRegistry.Create;
  try
    try
      if lReg.OpenKey(REG_KEY_FORMS + '\' + Copy(Self.Name,4,Length(Self.Name)), False) then
      begin
        if lReg.ValueExists('Splitter Position') then
          for iCount:=0 to ControlCount-1 do
            if not (Controls[iCount] is TSplitter) and (Controls[iCount].Align=alRight) then
            begin
              lState:=lReg.ReadInteger('Splitter Position');
              if lState=0 then lState:=1;
              Controls[iCount].Width:=lState;
              Break;
            end;
        if WindowState = wsNormal then
          ReadPosition(lReg);
      end;  // if Form in registry
    except
      // do nothing, ignore it and carry on
    end;
  finally
    lReg.CloseKey;
    lReg.Free;
  end;
end;  // LoadSize

//==============================================================================
{ Load state sets normal/minimised/maximised state of a new window.  Depends
    on registry, plus depends on the state of the currently active window }
procedure TBaseChild.LoadState;
var lReg   :TRegistry;
    lState :Integer;
begin
  lReg := TRegistry.Create;
  try
    try
      if lReg.OpenKey(REG_KEY_FORMS + '\' + Copy(Self.Name,4,Length(Self.Name)), False) then
      begin
        if AppSettings.MaximizedChildWindows then
            WindowState:=wsMaximized
        else if lReg.ValueExists('State') then begin
          lState:=lReg.ReadInteger('State');
          AppSettings.MaximizedChildWindows:=lState=3;
          case lState of
            1 : WindowState:=wsMinimized;
            2 : begin
                  WindowState:=wsNormal;
                end;
            3 : WindowState:=wsMaximized;
          end;
        end;
      end;  // if Form in registry
    except
      // do nothing, ignore it and carry on - only prob should be incorrect form state
    end;
  finally
    lReg.CloseKey;
    lReg.Free;
  end;
end;  // LoadState

//==============================================================================
{ When the corresponding item is selected in the Window menu, bring this window
    to the front.  Ensure it is not minimised }
procedure TBaseChild.WindowMenuItemClick(Sender: TObject);
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  BringToFront;
end;  // WindowMenuItemClick

//==============================================================================
{ Description : trap all key presses.  If the current control is data aware,
    but it is not editable, then a message is displayed about not editing
    other people's data
    uses csprop unit to access properties via rtti
  Created : 15/11/2002 }
procedure TBaseChild.KeyDown(var Key: Word; Shift: TShiftState);
var
  lControlDS : TDatasource;
  ltfShowMessage : Boolean;
begin
  // Only check what is going on if in edit mode
  if (GetCurrentControlEditMode = emEdit) and Assigned(Self.ActiveControl) then begin
    ltfShowMessage := False; // default
    // is it a data aware control?
    if PropertyExists(Self.ActiveControl, 'DataSource') then begin
      lControlDS := TDatasource(GetPointerProperty(Self.ActiveControl, 'DataSource'));
      if (lControlDS.Dataset.State = dsBrowse) then
        ltfShowMessage := True
      else
      if PropertyExists(Self.ActiveControl, 'Readonly') then begin
        ltfShowMessage := GetBooleanProperty(Self.ActiveControl, 'Readonly');
        // Try not to show any message if control is shown as readonly.
        if PropertyExists(Self.ActiveControl, 'Color') then
          ltfShowMessage := ltfShowMessage
               and (TColor(GetInt64Prop(Self.ActiveControl, 'Color')) <> ReadOnlyColour);
      end;
      //DBCombos will have their recordset state as INactive,
      //therefore need different way of testing.
      if Self.ActiveControl is TDbListCombo then
        if TDBListCombo(Self.ActiveControl).Readonly then
        begin
          ltfShowMessage := True;
          //Send a key up message to the control so that it thinks that the key
          // has been raised (Because Key is set to 0 at the end KeyUp will not be called
          // by default.
          PostMessage(Self.ActiveControl.Handle, WM_KEYUP, VK_LEFT,0);
        end;
      if Self.ActiveControl is TDBLookupComboBox then
        if ltfShowMessage then PostMessage(Self.ActiveControl.Handle, WM_KEYUP, VK_LEFT,0);
    end else
    if  Self.ActiveControl is TCheckListBox then begin
      with TCheckListBox(Self.ActiveControl) do
        if (ItemIndex > -1) and
           not (AppSettings.AllowEdit(emEdit) and
                ((Copy(TKeyData(Items.Objects[ItemIndex]).ItemKey, 1, 8) = AppSettings.SiteID) or
                 (TKeyData(Items.Objects[ItemIndex]).ItemKey= ''))) then
        begin
           ltfShowMessage :=True;
           PostMessage(Self.ActiveControl.Handle, WM_KEYUP, VK_LEFT,0);
        end
    end else
    if Self.ActiveControl is TSpatialRef then
      ltfShowMessage := TSpatialRef(Self.ActiveControl).EditMode = emView
    else
    if PropertyExists(Self.ActiveControl, 'Readonly') then begin
      ltfShowMessage := GetBooleanProperty(Self.ActiveControl, 'Readonly');
      // Try not to show any message if control is shown as readonly.
      if PropertyExists(Self.ActiveControl, 'Color') then
        ltfShowMessage := ltfShowMessage
             and (TColor(GetInt64Prop(Self.ActiveControl, 'Color')) <> ReadOnlyColour);
    end else
    if Self.ActiveControl is TStringGrid then
      ltfShowMessage := not (goEditing in TStringGrid(Self.ActiveControl).Options);

    // Tell the users they can't edit what they don't own,  if necessary
    if ltfShowMessage then begin
      if not HaveCustody then
        ShowInformation(ResStr_NotAllowedToEditOtherSitesData)
      else
      if AppSettings.RestrictFullEdit then
        ShowInformation(ResStr_NotAllowedToEditOtherPersonsData)
      else
        ShowInformation(ResStr_NotAllowedToEditData);
      Key := 0; // remove keypress
    end;
  end;
end;  // KeyPress

//==============================================================================
{ Description : Basic implementation of GetEditMode, should be overridden in
     each descendant form
  Created 15/11/02 }
function TBaseChild.GetCurrentControlEditMode: TEditMode;
begin
  Result := emNone;
end;  // GetCurrentControlEditMode

//==============================================================================
procedure TBaseChild.PreviewScreen;
begin
  // Do nothing here. Override in inherited forms if needed
end;

procedure TBaseChild.PrintScreen;
begin
  // Do nothing here. Override in inherited forms if needed
end;

{-------------------------------------------------------------------------------
  Select the node of the appropriate type and key from the list of existing
    nodes.  Implemented in derived classes.
}
function TBaseChild.SelectNode(const ANodeType, ANodeKey: string): TFlyNode;
begin
  Result := nil;// no implementation
end;

{-------------------------------------------------------------------------------
  Expands all parent nodes in teh child list
}
procedure TBaseChild.ExpandAllNodes ;
var
  lMovingNode: TFlyNode;
begin
//  kjg 26/11/2004
  if Assigned(TreeView) then begin
    try
      TreeView.items.BeginUpdate;
      lMovingNode := TreeView.Items.GetFirstNode;
      while Assigned(lMovingNode) do begin
        lMovingNode.expand(true);
        lMovingNode := lMovingNode.GetNext;
      end;
    finally
      TreeView.items.EndUpdate;
    end
  end;
end;

{-------------------------------------------------------------------------------
  Select the tab on the details page - with '\' characters separating tabs
    if they are nested.  Returns trus if successful
}
function TBaseChild.SelectTab(const ATabName: string): boolean;
var
  lTabs: TStringList;
  lCurrentText: string;
  lPos: integer;
  lPageControl: TPageControl;
  i, j: integer;
  lTab: TTabSheet;
begin
  if GetDetailsPageControl<>nil then begin
    lTabs := TStringList.Create;
    try
      // Parse tab list into list of nested tabs
      lCurrentText := ATabName;
      lPos := Pos('\', lCurrentText);
      while lPos>0 do begin
        lTabs.Add(Copy(lCurrentText, 1, lPos-1));
        lCurrentText := Copy(lCurrentText, lPos+1, Length(lCurrentText));
        lPos := Pos('\', lCurrentText);
      end;
      lTabs.Add(lCurrentText);
      // Now select the tab on the top level tab control, then go to the next
      // layer and so on
      lPageControl := GetDetailsPageControl;
      lTab := nil;
      for i := 0 to lTabs.Count-1 do begin
        lTab := nil;
        for j := 0 to lPageControl.PageCount-1 do
          if CompareText(lPageControl.Pages[j].Caption, lTabs[i])=0 then begin
            lTab := lPageControl.Pages[j];
            lPageControl.ActivePage := lTab;
            Break; // from inner loop
          end;
        if assigned(lTab) then begin
          lPageControl := nil;
          // find the nested page control
          for j := 0 to lTab.ControlCount-1 do
            if lTab.Controls[j] is TPageControl then begin
              lPageControl := TPageControl(lTab.Controls[j]);
            end;
        end;
        if (not Assigned(lTab)) or (not Assigned(lPageControl)) then
          Break; // from outer loop
      end; // for
      if not Assigned(lTab) then
        Result := False
      else if CompareText(lTab.Caption, lTabs[lTabs.Count-1])<>0 then
        Result := False
      else
        Result := True;
    finally
      lTabs.Free;
    end; // try
  end
  else
    Result := False;
end;

{-------------------------------------------------------------------------------
}
function TBaseChild.GetDetailsPageControl: TPageControl;
begin
  Result := nil;
end;

{-------------------------------------------------------------------------------
  Default node type for custom reporting (this value means no reports available
     unless overriden
}
function TBaseChild.CustomReportKeyType: TKeyType;
begin
  Result := ktDefault;
end;

{-------------------------------------------------------------------------------
  Override to return current node's key
}
function TBaseChild.ItemKey: string;
begin
  Result := '';
end;

function TBaseChild.GetTreeView: TRapidTree;
begin
  Result := nil;
end;

{-------------------------------------------------------------------------------
 Gets a new filter from Filter screen.
}
procedure TBaseChild.GetFilter(const AName, ATableName: String; const AAdditionalCriteria: String = '');
begin
  with TdlgFilter.Create(nil, ATableName, FDataFilter) do
    try
      if ShowModal = mrOk then begin
        // Clear previous filter, if any.
        ClearFilter(AName);
        // Setup new filter.
        FDataFilter.TableName := ATableName;
        FDataFilter.FilteredFieldKey := FilteredField.ItemKey;
        FDataFilter.Condition := StringReplace(rgConditions.Items[rgConditions.ItemIndex],
                                               '&', '', [rfReplaceAll]);
        FDataFilter.Criteria1 := eCriteria1.Text;
        FDataFilter.Criteria2 := eCriteria2.Text;
        FDataFilter.AdditionalCriteria := AAdditionalCriteria;
        FDataFilter.IsSet := True;
        // Save to registry now, so won't have to worry about when to save it later.
        if AppSettings.RememberFilters then SaveFilter(AName);
        // Run the query.
        GetFilteredKeys;
      end;
    finally
      Free;
    end;
end;

{-------------------------------------------------------------------------------
 Load an existing filter from the registry and apply it.
}
function TBaseChild.LoadFilter(const AName: String; AFirstLoad: Boolean = True): Boolean;
var
  lReg: TRegistry;
  lData: String;

  // Break filter down.
  function ParseData: String;
  var
    lPos: Integer;
  begin
    lPos := Pos(#1, lData);
    if lPos = 0 then
      Result := lData
    else begin
      Result := Copy(lData, 1, lPos - 1);
      lData := Copy(lData, lPos + 1, Length(lData) - lPos);
    end;
  end;

begin
  Result := False;
  // Cleanup registry if user doesn't want the filters but they're in from previous run.
  if not AppSettings.RememberFilters and AFirstLoad then
    ClearFilter(AName)
  else begin
    lReg := TRegistry.Create;
    try
      if lReg.OpenKey(REG_KEY_FORMS + '\' + Copy(ClassName, 5, Length(ClassName)), True) then
        if lReg.ValueExists(AName) then begin
          lData := lReg.ReadString(AName);
          with FDataFilter do begin
            TableName          := ParseData;
            FilteredFieldKey   := ParseData;
            Condition          := ParseData;
            Criteria1          := ParseData;
            Criteria2          := ParseData;
            AdditionalCriteria := ParseData;
            IsSet              := ParseData = BoolString[True];
          end;
          // Run the query.
          if FDataFilter.IsSet then GetFilteredKeys;
        end else
          FDataFilter.IsSet := False;
      Result := FDataFilter.IsSet;
      lReg.CloseKey;
    finally
      lReg.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
 Save the filter to registry. If there isn't anything to save, just remove the
 registry value, if it exists.
}
procedure TBaseChild.SaveFilter(const AName: String);
var
  lReg: TRegistry;
begin
  lReg := TRegistry.Create;
  try
    if lReg.OpenKey(REG_KEY_FORMS + '\' + Copy(ClassName, 5, Length(ClassName)), True) then
      if FDataFilter.IsSet then
        with FDataFilter do
          lReg.WriteString(AName, TableName + #1 + FilteredFieldKey + #1 + Condition  + #1 +
                                  Criteria1 + #1 + Criteria2 + #1 + AdditionalCriteria + #1 +
                                  BoolString[IsSet])
      else
      if lReg.ValueExists(AName) then
        lReg.DeleteValue(AName);
    lReg.CloseKey;
  finally
    lReg.Free;
  end;
end;

{-------------------------------------------------------------------------------
 Clear the applied filter, and removes it from the registry, through SaveFilter.
}
procedure TBaseChild.ClearFilter(const AName: String);
begin
  // Clear the filter.
  with FDataFilter do begin
    TableName          := '';
    FilteredFieldKey   := '';
    Condition          := '';
    Criteria1          := '';
    Criteria2          := '';
    AdditionalCriteria := '';
    IsSet              := False;
  end;
  // And clear it from registry.
  SaveFilter(AName);
end;

{-------------------------------------------------------------------------------
 Retrieves the keys through the query built from the info held in FDataFilter.
 Calls ApplyFilter to let the calling form handle the result.
}
procedure TBaseChild.GetFilteredKeys;
var
  lQry: TJnccQuery;
  lKeys: TEditableKeyList;
  lField: TReportField;
  lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  lQry := nil;
  lKeys := nil;
  lField := nil;
  try
    lQry := TJnccQuery.Create(nil);
    lKeys := TEditableKeyList.Create;
    lField := TReportField.Create;
    lField.Initialise(FDataFilter.FilteredFieldKey);
    dmDatabase.SetDatabaseLocal([lQry]);
    with lQry do begin
      SQL.Text := lField.SQLFilter(EncodeCondition(FDataFilter.Condition),
                                   FDataFilter.Criteria1,
                                   FDataFilter.Criteria2) + ' ' +
                  FDataFilter.AdditionalCriteria;

      // Taxon Occurrences have a confidential flag that needs to be checked.
      if SameText(FDataFilter.TableName, TN_TAXON_OCCURRENCE) and
         not AppSettings.ConfidentialAccessGranted(AppSettings.UserAccessLevel) then
        SQL.Text := SQL.Text + ' AND Confidential = 0';

      Open;
      while not Eof do begin
        lKeys.AddItem(FieldByName('ItemKey').AsString, FDataFilter.TableName);
        Next;
      end;
      Close;
    end;
    ApplyFilter(lKeys);

    Application.ProcessMessages;
  finally
    lQry.Free;
    lKeys.Free;
    lField.Free;
    DefaultCursor(lCursor);
  end;
end;

{-------------------------------------------------------------------------------
  Default accessor
}
function TBaseChild.GetHaveCustody: Boolean;
begin
  Result := (Custodian = AppSettings.SiteID);
end;

{-------------------------------------------------------------------------------
  Default accessor
}
function TBaseChild.GetCustodian: string;
begin
  Result := FCustodian;
end;

{-------------------------------------------------------------------------------
  Determine the edit button enabled state
}
function TBaseChild.EditButtonState(const AStateData: TKeyDataSysSupplied;
  ACustodian: string): Boolean;
begin
  Result := Assigned(AStateData) and
            (ACustodian = AppSettings.SiteID) and
            (not AStateData.SysSupplied) and
            (AppSettings.UserAccessLevel in [ualAdmin,ualFullUser]);
end;

{-------------------------------------------------------------------------------
  Enable the button on the context toolbar for sorting.  This is the one with
    the sort options attached as a popup menu.
}
procedure TBaseChild.EnableSortToolbutton(AEnabled: boolean;
  ASortPopupMenu: TPopupMenu);
var
  i: integer;
begin
  with frmMain.tbContext do
    for i:=0 to ButtonCount-1 do
      if Buttons[i].DropDownMenu=ASortPopupMenu then begin
        Buttons[i].Enabled:=AEnabled;
        Break;
      end;
end;

{-------------------------------------------------------------------------------
}
procedure TBaseChild.ApplySecurity;
begin
  // Nothing necessary here. Override in inherited forms if needed.
end;

(**
 * Populates the nodeChain string list with a list of key value pairs, starting
 * with the selected node and walking up to the top of the tree. The key is the
 * node's data class name, the value is the item key.
 *)
procedure TBaseChild.RememberPathToSelectedNode(treeview: TRapidTree;
  nodeChain: TStringList);
var
  current: TFlynode;
  nodeInfo : TNodeObject;  
begin
  current := treeview.Selected;
  while assigned(current) do begin
    nodeInfo := TNodeObject(current.Data);
    nodeChain.Add(nodeInfo.ClassName+'|'+nodeInfo.ItemKey);
    current := current.Parent;
  end;
end;

(**
 * Redisplays the selected node previously remembered by a call to
 * RememberPathToSelectedNode. This works even if the tree has been reloaded
 * since the call.
 *)
procedure TBaseChild.RedisplaySelectedNode(treeview: TRapidTree;
  nodeChain: TStringList);
var
  level, nodeidx: integer;
  chainPos: TTreeCollection;
  nodeInfo : TNodeObject;
  currentItem, currentClass: string;
begin
  // start at the top and work down the node chain to the previously selected node
  chainPos := treeview.Items;
  for level := nodeChain.Count-1 downto 0 do begin
    if chainPos is TFlyNode then
      TFlyNode(chainPos).Expand(false);
    currentClass := Copy(nodeChain[level], 1, Pos('|', nodeChain[level])-1);
    currentItem := Copy(nodeChain[level], Pos('|', nodeChain[level])+1, 255);
    // iterate the nodes at this level to find the one in the chain
    for nodeidx := 0 to chainPos.Count-1 do begin
      nodeInfo := TNodeObject(chainPos.Items[nodeidx].Data);
      if (nodeInfo.ItemKey = currentItem) and (nodeInfo.ClassName = currentClass) then begin
        chainPos := chainPos.Items[nodeidx];
        break;
      end;
    end;
  end;
  if chainPos is TFlyNode then
    treeview.Selected := TFlyNode(chainPos)
  else
    treeview.Selected := treeview.Items.GetFirstNode;
end;

end.
