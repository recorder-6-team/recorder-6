{===============================================================================
  Unit:        OLEContainer

  Defines:     TfrmMDIContainer

  Description: Container form for addins.

  Model:       <none>

  Created:

  Last revision information:
    $Revision: 40 $
    $Date: 4/01/08 13:15 $
    $Author: Rickyshrestha $

===============================================================================}

unit OLEContainer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Oletools, StdCtrls, Buttons, COMClasses, Recorder2000_TLB, BaseChildUnit,
  DataClasses, Menus, ActiveX, Constants, Contnrs;

type
  TfrmMDIContainer = class(TBaseChild)
    mnuEdit: TMenuItem;
    mnuEditTransferData: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FOLEProxy : TOLEProxy;
    FOleToolbarProxy : TOleProxy;
    FFormName : string; // allow us to check if one already available
    FKeyListSupplier : IKeyListSupplier;
    FSuppliesKeyList : boolean; // flag indication if above interface is implemented
    FMenuItemList: TObjectList;
    FMenuImages: TImageList;
    procedure InitContainer;
    procedure AddToolbar;
    function GetGUID: TGuid;
    function GetOleObject: IUnknown;
    procedure Initialise;
    procedure MergeMenus;
    procedure ShowMenuItems(AVisible: boolean);
  protected
    procedure WMHideComToolbar(var Message: TMessage); message WM_HIDE_COMTOOLBAR;
    procedure WMTransferDone(var Message: TMessage); message WM_TRANSFER_DONE;
  public
    constructor Create(AOwner: TComponent; iClassID: TGuid); reintroduce; overload;
    constructor Create(AOwner: TComponent; iProxy: TOleProxy); reintroduce; overload;
    destructor Destroy; override;
    function GetKeyList: TKeyList; override;
    procedure PreviewScreen; override;  // Called by File/Preview
    procedure PrintScreen; override;    // Called by File/Print
    procedure UpdateMapWindowSelector; override;
    property FormName: string read FFormName;
    property Guid: TGuid read GetGUID;
    property OleObject: IUnknown read GetOleObject;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, FormActions, Maintbar, CommCtrl;

resourcestring
  ResStr_Unknown = 'Unknown';

{==============================================================================}
constructor TfrmMDIContainer.Create(AOwner: TComponent; iClassID : TGuid);
begin
  FOLEProxy := TOLEProxy.Create(Self, iClassID);
  inherited Create(AOwner);
  Initialise;
end;

{ Overloaded version of the constructor where the com object already exists }
constructor TfrmMDIContainer.Create(AOwner: TComponent; iProxy: TOleProxy);
begin
  FOleProxy := iProxy;
  inherited Create(AOwner);
  Initialise;
end;

{-------------------------------------------------------------------------------
  Initialisation commmon to both constructors
}
procedure TfrmMDIContainer.Initialise;
begin
  InitContainer;
  AddToolbar;
  MergeMenus;
end;

{-------------------------------------------------------------------------------
  If the form has an implementation of IMergeMenuManager, then merge menus
}
procedure TfrmMDIContainer.MergeMenus;
var
  lMergeIntf: IMergeMenuManager;
  lItemIdx: integer;
begin
  // If we already have our menu items, just make them visible
  if Assigned(FMenuItemList) then
    ShowMenuItems(True)
  else
    // Otherwise, create them
    try
      lMergeIntf := FOleProxy.ControlInterface as IMergeMenuManager;
      FMenuItemList := TObjectList.Create;
      if lMergeIntf.DynamicMenuList.ImageListHandle<>0 then begin
        FMenuImages := TImageList.Create(nil);
        FMenuImages.Handle := ImageList_Duplicate(HIMAGELIST(lMergeIntf.DynamicMenuList.ImageListHandle));
      end else
        FMenuImages := nil;
      for lItemIdx := 0 to lMergeIntf.DynamicMenuList.Count-1 do
        AppSettings.ComAddins.ProcessTopLevelMenu(lMergeIntf.DynamicMenuList,
            lItemIdx, FMenuImages, FMenuItemList);
    except
      on EIntfCastError do; // nothing - interface not supported so ignore
    end; // try
end;

{ The part of the constructor that is common to both overloaded versions }
procedure TfrmMDIContainer.InitContainer;
begin
  { Put the activex onto the form }
  FOLEProxy.Parent := Self;
  FOleProxy.Align := alClient;
  try  // record the name being replaced only for replacement forms
    FFormname := (FOleProxy.ControlInterface as IReplacementForm).FormToReplace;
    // Map old form names (deprecated) onto new ones
    if CompareText(FFormName, 'TfrmTaxonDictionary ')=0 then
      FFormname := 'TfrmTaxonDictBrowser'
    else if CompareText(FFormName, 'TfrmTaxonDictDetails ')=0 then
      FFormName := 'TfrmTaxonDictEditor'
    else if CompareText(FFormName, 'TfrmBiotopeDictionary ')=0 then
      FFormname := 'TfrmBiotopeDictBrowser'
    else if CompareText(FFormName, 'TfrmBiotopeDictDetails ')=0 then
      FFormName := 'TfrmBiotopeDictEditor'
    else if CompareText(FFormName, 'TfrmAdminAreaDictionary  ')=0 then
      FFormName := 'TfrmAdminAreaDictBrowser';
  except
    on EIntfCastError do ; // nothing
  end;
  FSuppliesKeyList := True; // default, unless interface cast fails
  try
    FKeyListSupplier := FOleProxy.ControlInterface as IKeyListSupplier;
    dmFormActions.SetActionVisibility(dmFormActions.actExport, FKeyListSupplier.Exportable);
  except  // if not supported, allow the base class method to continue
    on EIntfCastError do
    begin
      FSuppliesKeyList := False;
      mnuEdit.Visible := False; // Hide return data
    end;
  end; // try
end;

{-------------------------------------------------------------------------------
  Set all the owned menu items visibility state
}
procedure TfrmMDIContainer.ShowMenuItems(AVisible: boolean);
var
  lItemIdx : integer;
begin
  if Assigned(FMenuItemList) then
    for lItemIdx := 0 to FMenuItemList.Count-1 do
      TMenuItem(FMenuItemList[lItemIdx]).Visible := AVisible;
end;

{ If the addin implements IFormWithToolbar, then add the toolbar to the main
     form }
procedure TfrmMDIContainer.AddToolbar;
var
  lToolbar : IOleObject;
begin
  try
    lToolbar := (FOleProxy.ControlInterface as IFormWithToolbar).Toolbar as IOleObject;
    FOleToolbarProxy := TOleProxy.Create(Self, lToolbar);
    FOleToolbarProxy.Parent := frmMain.CoolBarMain;// add it to the toolbar
(*    frmMain.ApplyToolBarSettings; //make sure it is the right width and everything*)
  except
    on EIntfCastError do
      FOleToolbarProxy := nil; // form doesn't have toolbar or toolbar is not IOleObject
  end;
end;

{ Free the form when it closes }
procedure TfrmMDIContainer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  { if pointer to toolbar in main screen is to this form's toolbar, then nil the
       reference for saftey }
  with (Application.MainForm as TfrmMain) do
    if LastVisibleAddinToolbar = FOleToolbarproxy then
      LastVisibleAddinToolbar := nil;
  if Assigned(FOLEToolbarProxy) then
  begin
    FOLEToolbarProxy.Parent:=nil;
    FOLEToolbarProxy.Free;
    FOLEToolbarProxy := nil;
  end;
  Action := caFree;
end;


destructor TfrmMDIContainer.Destroy;
begin
  FMenuItemList.Free;
  FMenuItemList := nil;
  FOLEProxy.Free;
  FOLEProxy := nil;
  FMenuImages.Free;
  FMenuImages := nil;
  inherited Destroy;
end;


{ If the ActiveX supports IKeyListSupplier, then we must expose that KeyList to
    the rest of the application }
function TfrmMDIContainer.GetKeyList: TKeyList;
begin
  if not FSuppliesKeyList then
    Result := Inherited GetKeyList
  else
   Result := TEditableKeyList.Create(FKeyListSupplier.KeyList);
end;

{------------------------------------------------------------------------------}
procedure TfrmMDIContainer.FormActivate(Sender: TObject);
begin
  inherited;
  { Following line of code only required because there is a lockwindowupdate
     gone astray somewhere else }
  LockWindowUpdate(0);
  if FSuppliesKeyList then
    dmFormActions.SetActionVisibility(dmFormActions.actExport, FKeyListSupplier.Exportable);
  frmMain.tbContext.Visible := False;
  if Assigned(FOLEToolbarProxy) then
    if not FOleToolbarProxy.Visible then begin
(*      frmMain.ApplyToolBarSettings;*)
      FOleToolbarProxy.Visible := True;
      FOleToolbarProxy.Anchors := [akLeft, akTop];
    end;
  frmMain.LastVisibleAddinToolbar := FOleToolbarProxy;
  if assigned(FOleProxy) then
    MergeMenus;
end;


{ Return the Guid by looking at the OLEProxy object }
function TfrmMDIContainer.GetGUID: TGuid;
begin
  Result := FOleProxy.AssignedGuid;
end;

{ Accessor method - return ole object by looking into proxy object }
function TfrmMDIContainer.GetOleObject: IUnknown;
begin
  Result := FOleProxy.OleObject;
end;

{ The following two methods do nothing for a COM form }
procedure TfrmMDIContainer.PreviewScreen;
begin

end;

procedure TfrmMDIContainer.PrintScreen;
begin

end;


procedure TfrmMDIContainer.FormDeactivate(Sender: TObject);
begin
  inherited;
  { Rather than hide the toolbar when we deactivate, wait for all other messages
     to process because it could be that we remain active (due to the toolbar
     itself taking temporary focus).  This avoids the toolbar pinging to the right }
  PostMessage(Self.Handle, WM_HIDE_COMTOOLBAR, 0, 0);
end;


procedure TfrmMDIContainer.WMHideComToolbar(var Message: TMessage);
begin
  if (not Self.Active) then begin
    if Assigned(FOLEToolbarProxy) then
      FOLEToolbarProxy.Visible := False;
    ShowMenuItems(False);
  end;
end;

{-------------------------------------------------------------------------------
  Description : After a return data completes, show this form
  Created : 21/10/2003 }
procedure TfrmMDIContainer.WMTransferDone(var Message: TMessage);
begin
  Show;
end;

{-------------------------------------------------------------------------------
  This is the appropriate time to read the form's caption, otherwise the Windows
      Menu won't be correct.
}
procedure TfrmMDIContainer.FormCreate(Sender: TObject);
var
  lFormCaption: IFormCaption;
  lAddin: IRecorderAddin;
begin
  inherited;
  { A Caption specification interface  is optional, else use the name of the
        addin }
  if Supports(FOleProxy.ControlInterface, IID_IFormCaption, lFormCaption) then
    Caption := lFormCaption.FormCaption
  else if Supports(FOleProxy.ControlInterface, IID_IRecorderAddin, lAddin) then
    Caption := lAddin.Name
  else
    Caption := ResStr_Unknown;
end;

{-------------------------------------------------------------------------------
  Pass FormCloseQuery checks on to the ActiveX form. Prevent the form from
     closing when the ActiveX doesn't want to
}
procedure TfrmMDIContainer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  CanClose := SendMessage(FOleProxy.Handle, WM_QUERYENDSESSION, 0, 0)=1;
end;

{-------------------------------------------------------------------------------
  Try to tell the contained COM object that the map window selectors should be
  updated.
}
procedure TfrmMDIContainer.UpdateMapWindowSelector;
begin
  // Check the object supports the interface before tyring to use it.
  if Supports(OleObject, IID_IMapWindowSelector) then
    (OleObject as IMapWindowSelector).UpdateMapWindowSelector;
  RefreshXPMenu;
end;

end.
