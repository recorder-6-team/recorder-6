{===============================================================================
  Unit:        ComAddinUnit

  Defines:     TAddinAction       (TAction)
               TAddinInfo         (TObject)
               TComAddins         (TObject)
               TDynamicMenuItem   (TMenuItem)

  Description:

  Model:       <none>

  Created:     June 1999

  Last revision information:
    $Revision: 72 $
    $Date: 18/02/09 17:36 $
    $Author: Ericsalmon $

===============================================================================}

unit ComAddinUnit;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls, Registry, Recorder2000_TLB, ExceptionForm, ImgList,
  ComObj, ActnList, OLETools, CommCtrl, Contnrs, LocOnFly;

resourcestring
  ResStr_CantInitDynamicMenus = 'Cannot initialise dynamic menus without a main menu';
  ResStr_AddinMenuPositionInvalid = 'The menu item requested by an addin cannot be found: %s';
  ResStr_NoItems = 'No items';
  ResStr_ComRegistryProblem = 'A problem occurred opening registry keys for COM addins';
  ResStr_InvalidAddin = 'A Registered Add-in is not valid and cannot be used.';
  ResStr_AddinImageProblem = 'A problem occurred loading the image for an add-in';
  ResStr_BadAddins = 'The following addins could not be loaded for the specified reasons:'#13;
  ResStr_GoodAddins = 'The following addins loaded successfully:'#13;
  ResStr_ReportSavedIn = 'This report has been saved in %s';
  ResStr_Version = 'Version';
  ResStr_Date = 'Date';
  ResStr_GetAddinHelp = 'Get additional help for the %s addin';
  ResStr_HelpOn = 'Help on %s';
  ResStr_AddinMenusFailed = 'The %s addin failed to initialise its menus.  Some functionality '+
      'may not be available for this addin.  '#13#10'The error is described as:'#13#10'%s: %s';


type
  EComAddinError = class(TExceptionPath);

  { Class to store basic info about an addin }
  TAddinInfo = class
  private
    FClsID: TGUID;
    FName: String;
    FDescription: String;
    FImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
  public
    constructor Create(iClsID : TGUID; iName, iDescription : String);
    property ClsID: TGUID read FClsID;
    property Name: String read FName;
    property Description: String read FDescription;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
  end;

  TComAddins = class(TObject)
  private
    FAddinList: TObjectList;
    FImages : TImageList;           // list of images for each Add-in
    FReplacementForms: TStringList; // name=clsid
    FAdditionalPages: TStringList; // name=clsid
    FImportFilters : TStringList; // name=clsid
    FExportFilters : TStringList; // name=clsid
    FHelpSystems : TStringList; // clsid
    FSpatialSystems : TStringList; // system name
    FSpatialSystemInterfaces : TInterfaceList; // matching list to above for interface pointers
    FReportKeyTypeInterfaces : TInterfaceList;
    FFormEventAddins : TStringList; // just form_name=clsId
    FValidators : TStringList; // clsid
    FDynamicMenuLists: TInterfaceList; // clsId
    FNodeMenuManagers: TInterfaceList;
    FOptionsPages: TStringList; // clsid
    FOccurrenceNodeManagers: TStringList;
    FLocationNodeManagers: TStringList;
    FMapDropFormatAddins: TStringList;
    FComItems: TList;
    FAdditionalFilterClass : TGUID;
    FHasAdditionalFilter : Boolean;
    FMainMenu: TMainMenu; // of application
    FDynamicMenuImageLists: TObjectList;
    function AddNewComObject(iClsID: TGUID): string;
    function GetAddinCount: Integer;
    function GetAddinList(const iIndex: Integer): TAddinInfo;
    procedure GetDynamicMenuImageList(AMenuListIntf: IDynamicMenuList; AIndex: Integer);
    procedure InitializeActionData(var ActionData: TActionData);
    procedure SetMainMenu(const Value: TMainMenu);
    procedure SafeLoadBmp(iBmp: TBitmap; const iFileName: String);
    procedure ReadComObjects;
    procedure CheckReplacementForm(iComObject: IUnknown; iClsID: TGUID);
    procedure CheckAdditionalPage(iComObject: IUnknown; iClsID: TGUID);
    procedure CheckNewAction(iComObject: IUnknown; iClsID: TGUID);
    procedure CheckImportFilter(iComObject: IUnknown; iClsID: TGUID);
    procedure CheckExportFilter(iComObject: IUnknown; iClsID: TGUID);
    procedure CheckFormEvents(iComObject: IUnknown; iClsID: TGUID);
    procedure CheckHelpSystem(iComObject: IUnknown; iClsID: TGUID);
    procedure CheckCardHeader(iComObject: IUnknown; iClsID: TGUID);
    procedure CheckAdditionalFilter(iComObject: IUnknown; iClsID: TGUID);
    procedure CheckSpatialReference(iComObject: IUnknown);
    procedure CheckReportKeytype(iComObject: IInterface);
    procedure CheckInterface(AComObject: IUnknown; AInterface: TGUID; AClsID: TGUID;
      AIntfList: TStringList); overload;
    procedure CheckInterface(AComObject: IUnknown; AInterface: TGUID;
      AIntfList: TInterfaceList); overload;
    procedure SetupImage(var ioAddinInfo: TAddinInfo; iRecorderAddin: IRecorderAddin);
    function AddNewActionImages(iComObject: IUnknown): Integer;
    procedure AddActionToMenu(iAction: TAction; iNewActionIntf: INewAction); overload;
    procedure AddActionToMenu(iAction: TAction;
      iParentItem, iItemToInsertBefore: TMenuItem); overload;
    procedure CheckForActionOnToolbar(iAction: TAction);
    function GetUntranslatedCaption(AMenuItem: TMenuItem): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessTopLevelMenu(AMenuListIntf: IDynamicMenuList; AIndex: Integer;
        AImageList: TImageList; AMenuItemList: TObjectList = nil); overload;
    procedure ProcessTopLevelMenu(AMenuListIntf: IDynamicMenuList; AIndex: Integer;
        AImageList: TImageList;  AMenuToAddTo: TMenuItem;
        AMenuItemList: TObjectList = nil); overload;
    procedure InitialiseDynamicMenus;
    property AddinCount: Integer read GetAddinCount;
    property AddinList[const iIndex: Integer]: TAddinInfo read GetAddinList;
    property AdditionalFilterClass: TGUID read FAdditionalFilterClass;
    property AdditionalPages: TStringList read FAdditionalPages;
    property DynamicMenus: TInterfaceList read FDynamicMenuLists;
    property ExportFilters: TStringList  read FExportFilters;
    function FindMenu(const ACaption: String; AMenu: TMenuItem): Integer;
    property FormEventAddins: TStringList read FFormEventAddins;
    property HasAdditionalFilter: Boolean read FHasAdditionalFilter;
    property Images: TImageList read FImages;
    property ImportFilters: TStringList  read FImportFilters;
    property MainMenu: TMainMenu read FMainMenu write SetMainMenu;
    property MapDropFormatAddins: TStringList read FMapDropFormatAddins;
    property OccurrenceNodeManagers: TStringList read FOccurrenceNodeManagers;
    property LocationNodeManagers: TStringList read FLocationNodeManagers;
    property NodeMenuManagers: TInterfaceList read FNodeMenuManagers;
    property OptionsPages: TStringList read FOptionsPages;
    property ReplacementForms: TStringList read FReplacementForms;
    property SpatialSystems: TStringList read FSpatialSystems;
    property SpatialSystemInterfaces: TInterfaceList read FSpatialSystemInterfaces;
    property ReportKeyTypeInterfaces: TInterfaceList read FReportKeyTypeInterfaces;
    property Validators: TStringList read FValidators;
  end;  { TComAddins }

//==============================================================================
implementation

uses
  FormActions, MainTBar, ApplicationSettings, Genfuncs, XPToolbutton,
  GeneralFunctions, BaseChildUnit, VersionInfo, Constants;

const
  GUID_FilterResultToExcel : TGUID = '{1EEF0271-98BD-42DE-90DA-7888E95F5591}';

type
  { TAction subclass required to hold extra info about dynamic addin actions }
  TAddinAction = class(TAction)
  private
    FMenuIntf: IDynamicMenu;
    procedure SetMenuIntf(const Value: IDynamicMenu);
    procedure DoExecute(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property MenuIntf: IDynamicMenu read FMenuIntf write SetMenuIntf;
  end;

  { TMenuItem subclass required to hold extra info about dynamic addin menus }
  TDynamicMenuItem = class(TMenuItem)
  private
    FMenuIntf: IDynamicMenu;
    FImageList: TImageList;
    FHasSubMenu: boolean;
    procedure SetMenuIntf(const Value: IDynamicMenu);
    procedure SetActionProperties(AAction: TAddinAction;
      AActionData: TActionData);
    procedure SetImageList(const Value: TImageList);
  public
    procedure BuildDynamicMenu;
    procedure Click; override;
    property MenuIntf: IDynamicMenu read FMenuIntf write SetMenuIntf;
    property ImageList: TImageList read FImageList write SetImageList;
  end;

//------------------------------------------------------------------------------
function ReplaceAmpersands(const AString: String): String;
begin
  Result := StringReplace(AString, '&', '', [rfReplaceAll]);
end;

//------------------------------------------------------------------------------
//--TAddinAction----------------------------------------------------------------
//------------------------------------------------------------------------------
{-------------------------------------------------------------------------------
  Constructor links up the OnExecute handler
}
constructor TAddinAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnExecute := DoExecute;
end;

{-------------------------------------------------------------------------------
  OnExecute handler links the action to the addin
}
procedure TAddinAction.DoExecute(Sender: TObject);
begin
  dmFormActions.RunAction(FMenuIntf.Execute);
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TAddinAction.SetMenuIntf(const Value: IDynamicMenu);
begin
  FMenuIntf := Value;
end;


//------------------------------------------------------------------------------
//--TDynamicMenuItem------------------------------------------------------------
//------------------------------------------------------------------------------

{-------------------------------------------------------------------------------
  Click handler, populates the sub menu dynamically
}
procedure TDynamicMenuItem.Click;
var
  lIdx: Integer;
  lSubItem: TDynamicMenuItem;
  lOrigCount: Integer;
begin
  inherited;
  // Note the number of old items to delete later
  lOrigCount := Count;
  // Add the new items before deleting the old, otherwise we temporarily lose
  // the sub-menu causing flicker
  if FMenuIntf.ChildCount > 0 then begin
    for lIdx := 0 to FMenuIntf.ChildCount - 1 do begin
      lSubItem := TDynamicMenuItem.Create(Self);
      lSubItem.ImageList := FImageList;
      lSubItem.MenuIntf  := FMenuIntf.Child(lIdx) as IDynamicMenu;
      lSubItem.BuildDynamicMenu;
      Add(lSubItem);
    end;
  end
  else if FHasSubMenu then begin
    // Create a disabled item so the sub-menu doesn't disappear
    lSubItem := TDynamicMenuItem.Create(Self);
    lSubItem.Enabled := False;
    lSubItem.Caption := '(' + ResStr_NoItems + ')';
    Add(lSubItem);
  end;
  // Clear out the old items
  for lIdx := 0 to lOrigCount - 1 do
    Items[0].Free;

  if AppSettings.ShowMenuIcons and Assigned(frmMain.XPMenu) then begin
    frmMain.RefreshXPMenu;  // For main menu
    frmMain.XPMenu.ActivateMenuItem(Self, True);  // For sub-menu
  end;
end;

{-------------------------------------------------------------------------------
  Sets up an action using the COM interface
}
procedure TDynamicMenuItem.SetActionProperties(AAction: TAddinAction; AActionData: TActionData);
var
  lMenuPath  : TStringList;
begin
  // Caption is last item in menu path.
  lMenuPath := TStringList.Create;
  try
    lMenuPath.CommaText := '"' + StringReplace(AActionData.Caption, '\', '","', [rfReplaceAll]) + '"';
    with AAction do begin
      Caption  := lMenuPath[lMenuPath.Count - 1];
      Enabled  := AActionData.Enabled;
      Hint     := AActionData.Hint;
      ShortCut := AActionData.ShortCut;
      Visible  := AActionData.Visible;
    end;
    if Assigned(ImageList) and (AActionData.ImageIndex <> -1) then begin
      // Clear the bitmap to "prepare" it for the glyph
      Bitmap.Canvas.FillRect(Rect(0, 0, 15, 15));
      ImageList.GetBitmap(AActionData.ImageIndex, Bitmap);
    end;
  finally
    lMenuPath.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TDynamicMenuItem.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TDynamicMenuItem.SetMenuIntf(const Value: IDynamicMenu);
begin
  FMenuIntf := Value;
end;

{-------------------------------------------------------------------------------
  Builds a single dynamic menu item in the menu
}
procedure TDynamicMenuItem.BuildDynamicMenu;
var
  lAction: TAddinAction;
  lActionData: TActionData;
begin
  // Get additional properties of action
  FMenuIntf.GetItemData(lActionData);
  Self.Caption := lActionData.Caption;
  if FMenuIntf.HasSubmenu then begin
    if Assigned(ImageList) and (lActionData.ImageIndex <> -1) then begin
      // Clear the bitmap to "prepare" it for the glyph
      Bitmap.Canvas.FillRect(Rect(0, 0, 15, 15));
      ImageList.GetBitmap(lActionData.ImageIndex, Bitmap);
    end;
    // Create a dummy menu to force a submenu to be displayed
    Add(TMenuItem.Create(Self));
    FHasSubMenu := True;
  end
  else begin
    lAction := TAddinAction.Create(nil);
    lAction.Tag := -1;
    with lAction do begin
      // Add action to actionlist.
      ActionList := dmFormActions.alForms;
      SetActionProperties(lAction, lActionData);
    end;
    lAction.MenuIntf := FMenuIntf; // doesn't work in the With!!
    Action := lAction;
    FHasSubMenu := False;
  end;
end;

//==============================================================================
{ Creates an object of type TComAddins, and initializes properties. }
constructor TComAddins.Create;
begin
  inherited Create;
  FAddinlist              := TObjectList.Create;
  FReplacementForms       := TStringList.Create;
  FAdditionalPages        := TStringList.Create;
  FImportFilters          := TStringList.Create;
  FExportFilters          := TStringList.Create;
  FHelpSystems            := TStringList.Create;
  FFormEventAddins        := TStringList.Create;
  FSpatialSystems         := TStringList.Create;
  FValidators             := TStringList.Create;
  FOptionsPages           := TStringList.Create;
  FOccurrenceNodeManagers := TStringList.Create;
  FLocationNodeManagers   := TStringList.Create;
  FMapDropFormatAddins    := TStringList.Create;
  FDynamicMenuLists       := TInterfaceList.Create;
  FNodeMenuManagers       := TInterfaceList.Create;
  FSpatialSystemInterfaces:= TInterfaceList.Create;
  FReportKeyTypeInterfaces:= TInterfaceList.Create;
  FHasAdditionalFilter    := False;
  { Create a list to hold any new menu items/actions so we can remove them }
  FComItems      := TList.Create;
  FImages        := TImageList.Create(nil);
  FImages.Width  := BITMAP_SIZE;
  FImages.Height := BITMAP_SIZE;
  FDynamicMenuImageLists := TObjectList.Create;

  // We want to be able to find Spatial Systems even if they come in lower/upper/mixed case.
  FspatialSystems.CaseSensitive := False;

  ReadComObjects;
end;  // Create

//==============================================================================
{ Destroys and cleanup for the TComAddins class }
destructor TComAddins.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FAddinList);

  { Free up the class id lists }
  FreeAndNil(FReplacementForms);
  FreeAndNil(FAdditionalPages);
  FreeAndNil(FImportFilters);
  FreeAndNil(FExportFilters);
  FreeAndNil(FHelpSystems);
  FreeAndNil(FFormEventAddins);
  FreeAndNil(FSpatialSystems);
  FreeAndNil(FValidators);
  FreeAndNil(FOptionsPages);
  FreeAndNil(FOccurrenceNodeManagers);
  FreeAndNil(FLocationNodeManagers);
  FreeAndNil(FMapDropFormatAddins);
  FreeAndNil(FDynamicMenuLists);
  FreeAndNil(FNodeMenuManagers);
  FreeAndNil(FSpatialSystemInterfaces);
  FreeAndNil(FReportKeyTypeInterfaces);

  {Remove the menu items backwards, to avoid parent menu items freeing children problems}
  for i := FComItems.Count - 1 downto 0 do
    TObject(FComItems[i]).Free;
  FreeAndNil(FComItems);

  FreeAndNil(FImages);
  FreeAndNil(FDynamicMenuImageLists);
  inherited Destroy;
end;  // Destroy

//==============================================================================
{ Read each com object installed from the registry.  Initiate scanning of the
     capabilities of the com objects }
procedure TComAddins.ReadComObjects;
var
  lKeys : TStringList;
  i : Integer;
  lInstalled : boolean;
  lClsId : TGUID;
  lBadCOMs: TStringList;
  lGoodCOMs: TStringList;
  lResult: String;
begin
  { Read COM objects from the registry }
  lKeys := TStringList.Create;
  lBadCOMs := TStringList.Create;
  lGoodCOMs := TStringList.Create;
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKeyReadOnly(REG_KEY_ADDIN);
      GetKeyNames(lKeys);  { Create }
      { Loop through the objects in regsitry }
      for i := 0 to lKeys.Count - 1 do
      begin
        CloseKey;
        if not OpenKeyReadOnly(REG_KEY_ADDIN + '\' + lKeys[i]) then
          raise EComAddinError.Create(ResStr_ComRegistryProblem);
        lInstalled := (ReadString(REG_INSTALLED) = '1');

        { Check its installed.  If not, ignore it }
        if lInstalled then
        begin
          lClsID := StringToGuid(ReadString(REG_CLASS_ID));
          // filterresulttoexcel addin deprecated
          if not IsEqualGUID(lClsID, GUID_FilterResultToExcel) then
            try
              lGoodCOMs.Add(#9 + AddNewComObject(lClsID));
            except
              on E:Exception do
                lBadCOMs.Add(#9 + lKeys[i] + ': ' + E.Message);
            end;
        end;
      end;

      if lBadCOMs.Count > 0 then begin
        lResult := ResStr_BadAddins + lBadComs.Text;
        lResult := lResult + #13 + ResStr_GoodAddins;
        lResult := lResult + lGoodCOMs.Text;
        MessageDlg(lResult + #13 + Format(ResStr_ReportSavedIn,
                   [AppSettings.AddinPath + 'AddinLoadFailed.txt']), mtWarning, [mbOk], 0);
        lBadCOMs.Text := ResStr_Version + ': ' + GetFileVersion + #13 +
                         ResStr_Date + ': ' + DateTimeToStr(Now) + #13#13 + lResult;
        lBadCOMs.SaveToFile(AppSettings.AddinPath + 'AddinLoadFailed.txt');
      end;
    finally
      Free;
      lKeys.Free;
      lBadCOMs.Free;
      lGoodCOMs.Free;
    end;// try..finally
end;  // ReadComObjects

//==============================================================================
{ Checks the capabilities of a COM object, and adds all required info to the
     local lists.  Actually has to create a temporary reference to the com
     object and uses QueryInterface.
     Returns the addin name }
function TComAddins.AddNewComObject(iClsID: TGUID): string;
var
  lComObject : IUnknown;
  lRecorderAddin : IRecorderAddin;
  lAddinInfo : TAddinInfo;
begin
  Result := '';
  lComObject := CreateComObject(iClsID);
  if not Supports(lComObject, IRecorderAddin, lRecorderAddin) then
    Raise EComAddinError.Create(ResStr_InvalidAddin);
  Result := lRecorderAddin.Name;
  { Read the basic information about the addin }
  lAddinInfo := TAddinInfo.Create(iClsID, lRecorderAddin.Name,
                                          lRecorderAddin.Description);
  SetupImage(lAddinInfo, lRecorderAddin);
  FAddinList.Add(lAddinInfo);
  CheckReplacementForm(lComObject, iClsID);
  CheckNewAction(lComObject, iClsID);
  CheckAdditionalPage(lComObject, iClsID);
  CheckExportFilter(lComObject, iClsID);
  CheckImportFilter(lComObject, iClsID);
  CheckCardHeader(lComObject, iClsID);
  CheckFormEvents(lComObject, iClsID);
  CheckAdditionalFilter(lComObject, iClsID);
  CheckHelpSystem(lComObject, iClsID);
  CheckSpatialReference(lComObject);
  CheckReportKeytype(lComObject);
  CheckInterface(lComObject, IID_IValidation, iClsID, FValidators);
  CheckInterface(lComObject, IID_IOccurrenceNodeManager, iClsID, FOccurrenceNodeManagers);
  CheckInterface(lComObject, IID_ILocationNodeManager, iClsID, FLocationNodeManagers);
  CheckInterface(lComObject, IID_IDynamicMenuList, FDynamicMenuLists);
  CheckInterface(lComObject, IID_INodeMenuManager, FNodeMenuManagers);
  CheckInterface(lComObject, IID_IOptionsPages, iClsID, FOptionsPages);
  CheckInterface(lComObject, IID_IMapDropFormat, iClsID, FMapDropFormatAddins);
end;  // AddNewComObject

//==============================================================================
{ Checks if the com object specified supports IReplacementForm.  If so, reads
    the necessary information into a String list to make use of the replacement
    form much easier later. }
procedure TComAddins.CheckReplacementForm(iComObject: IUnknown; iClsID : TGUID);
var
  lReplacementForm : IReplacementForm;
  lFormToReplace : String;
begin
  try
    lReplacementForm := iComObject as IReplacementForm;
  except
    on E:EIntfCastError do
      Exit; // replacement form not supported
  end;
  lFormToReplace := lReplacementForm.FormToReplace;
  { Add a name value pair for the form name and class id }
  FReplacementForms.Add(lFormToReplace + '=' + GuidToString(iClsId));
end;  // CheckReplacementForm

//==============================================================================
{ Checks if the com object specified supports IAdditionalPage.  If so, reads
    the necessary information into a String list to make use of the additional
    page much easier later.  These are tab control page additions. }
procedure TComAddins.CheckAdditionalPage(iComObject: IUnknown;
  iClsID: TGUID);
var
  lAdditionalPage : IAdditionalPage;
  lForm : String;
begin
  try
    lAdditionalPage := iComObject as IAdditionalPage;
  except
    on E:EIntfCastError do
      Exit; // replacement form not supported
  end;
  lForm := lAdditionalPage.Form;
  { Add a name value pair for the form name and class id }
  FAdditionalPages.Add(lForm + '=' + GuidToString(iClsId));
end;  // CheckAdditionalPage

//==============================================================================
{ Checks if the com object specified supports IAdditionalPage.  If so, reads
    the necessary information into a String list to make use of the additional
    page much easier later.  These are tab control page additions. }
procedure TComAddins.CheckNewAction(iComObject: IUnknown; iClsID: TGUID);
var
  lAction : TComAction;
  lNewAction : INewAction;
  lImageIndex : Integer;
begin
  try
    lNewAction := iComObject as INewAction;
  except
    on E:EIntfCastError do
      Exit; // replacement form not supported
  end;
  lImageIndex      := AddNewActionImages(iComObject);
  lAction          := TComAction.Create(dmFormActions,iClsID);
  lAction.ActionList:=dmFormActions.alForms;
  lAction.Caption  := lNewAction.ActionCaption;
  lAction.Hint     := lNewAction.Hint;
  lAction.OnExecute:= dmFormActions.ExecuteComAction;
  if lImageIndex <> -1 then // ie we have images
    lAction.ImageIndex := lImageIndex;
  // Set Action tag to 1 so it can appear in Options dialog and be put on toolbar.
  if lNewAction.CanAddToToolbar and (lNewAction.DimmedImageFilename<>'') then
  begin
    lAction.Tag:=1;
    CheckForActionOnToolbar(lAction);
  end;
  AddActionToMenu(lAction, iComObject as INewAction);
end;  // CheckNewAction


{ Check if a blank toolbar button has been created from the user settings in the
    registry.  If found and the index (button's tag) is this action, then link
    the toolbar button up }
procedure TComAddins.CheckForActionOnToolbar(iAction : TAction);
var
  i : Integer;
begin
  with frmMain.tbMainToolbar do
  begin
    for i := 0 to ButtonCount-1 do
      if Buttons[i].Tag = iAction.Index then
        Buttons[i].Action := iAction;
  end;
end;


//==============================================================================
{ Checks if the com object specified supports IExportFilter.  If so, reads
    the necessary information into a String list to make use of the filter
    much easier later. }
procedure TComAddins.CheckExportFilter(iComObject: IUnknown;
  iClsID: TGUID);
var
  lExportFilter : IExportFilter;
  lFilter : IFilter;
begin
  try
    // Need to check both interfaces are implemented
    lExportFilter := iComObject as IExportFilter;
    lFilter := iComObject as IFilter;
  except
    on EIntfCastError do
      Exit; // import filter not supported
  end;
  { Just store the filter in a list }
  FExportFilters.Add((iComObject as IRecorderAddin).Name + '=' +
                      GuidToString(iClsID));
end;  // CheckExportFilter

//==============================================================================
{ Checks if the com object specified supports IImportFilter.  If so, reads
    the necessary information into a String list to make use of the filter
    much easier later.}
procedure TComAddins.CheckImportFilter(iComObject: IUnknown;
  iClsID: TGUID);
var
  lImportFilter : IImportFilter;
  lFilter : IFilter;
begin
  try
    // Need to check both interfaces are implemented
    lImportFilter := iComObject as IImportFilter;
    lFilter := iComObject as IFilter;
  except
    on EIntfCastError do
      Exit; // import filter not supported
  end;
  { Jsut store the filter in a list }
  FImportFilters.Add((iComObject as IRecorderAddin).Name + '=' +
                      GuidToString(iClsID));
end;  // CheckImportFilter

//==============================================================================
{ Checks if the object supports IHelp.  If so, sets up the relevant menu option,
    and records the Help object in a String list for access later }
procedure TComAddins.CheckHelpSystem(iComObject: IUnknown; iClsID: TGUID);
var
  lHelpSystem : IHelp;
  lAction : TComHelpAction;
  lImageIndex : Integer;
begin
  try
    lHelpSystem := iComObject as IHelp;
  except
    on EIntfCastError do
      Exit; // help not supported
  end;
  if not Supports(iComObject, IAdditionalPage) then begin
    // If an additional page, then F1 used to access help file. Otherwise
    // we create a help menu item action.
    { Store the class id }
    FHelpSystems.Add(GuidToString(iclsID));
    { Set up the action for the main menu }
    lImageIndex := AddNewActionImages(iComObject);
    lAction := TComHelpAction.Create(dmFormActions,iClsID);
    lAction.Caption := Format(ResStr_HelpOn, [(iComObject as IRecorderAddin).Name]);
    lAction.Hint := Format(ResStr_GetAddinHelp, [(iComObject as IRecorderAddin).Name]);
    lAction.OnExecute := dmFormActions.ExecuteComAction;
    if lImageIndex <> -1 then // ie we have images
      lAction.ImageIndex := lImageIndex;
    AddActionToMenu(lAction, frmMain.mnuHelp, frmMain.mnuHelpAbout);
  end;
end;  // CheckHelpSystem

//==============================================================================
{ Check if the object supports form event handlers.  If so, stores the necessary
     information.  When the relevant form is created, it starts up the class,
     and triggers various events as relevant in that class. }
procedure TComAddins.CheckFormEvents(iComObject: IUnknown; iClsID: TGUID);
var
  lFormEventIntf : IRecorderFormEvents;
begin
  try
    lFormEventIntf := iComObject as IRecorderFormEvents;
  except
    on EIntfCastError do
      Exit; // form events not supported
  end;
  { Store the form name and class_id }
  FFormEventAddins.Add(lFormEventIntf.FormName + '=' + GuidToString(iClsId));
end;  // CheckFormEvents

//==============================================================================
{ Check if the object supports IRecordCardHeader.  If so, then sets up the
    menu option for it }
procedure TComAddins.CheckCardHeader(iComObject: IUnknown; iClsID: TGUID);
var
  lCardHeader : IRecordCardHeader;
begin
  try
    lCardHeader := iComObject as IRecordCardHeader;
  except
    on EIntfCastError do
      Exit; // record card header not supported
  end;
end;  // CheckCardHeader

//==============================================================================
{ checks if a com object supports IDynamicMenu.  If so, the
     class_ID is stored for future use }
procedure TComAddins.CheckInterface(AComObject: IUnknown; AInterface: TGUID;
          AClsID: TGUID; AIntfList: TStringList);
var
  lIntfResult : IUnknown;
begin
  if Supports(AComObject, AInterface, lIntfResult) then
    AIntfList.Add(GuidToString(AClsID));
end;

//==============================================================================
{ checks if a com object supports IDynamicMenu.  If so, the
     interface pointer is stored for future use }
procedure TComAddins.CheckInterface(AComObject: IUnknown; AInterface: TGUID;
          AIntfList: TInterfaceList);
var
  lIntfResult : IUnknown;
begin
  if Supports(AComObject, AInterface, lIntfResult) then
    AIntfList.Add(AComObject);
end;

//==============================================================================
{ checks if a com object supports IAdditionalFilterDialog.  If so, the
    first one encountered is stored in FAdditionalFilterClass }
procedure TComAddins.CheckAdditionalFilter(iComObject: IUnknown;
  iClsID: TGUID);
var
  lAdditionalFilterIntf : IAdditionalFilterDialog;
  lDialogIntf : IDialog;
begin
  if not FHasAdditionalFilter then
  begin
    try
      { Additional filters must support 2 interfaces }
      lAdditionalFilterIntf := icomObject as IAdditionalFilterDialog;
      lDialogIntf := iComObject as IDialog;
    except
      on EIntfCastError do
        Exit;
    end;  // try
    FAdditionalFilterClass := iClsID;
    FHasAdditionalFilter := True;
  end;
end;  // CheckAdditionalFilter

//==============================================================================
{ Adds the enabled, disabled and dimmed images to form action's images for a
    new COM action.  Returns zero if no image, otherwise returns the
    imageindex. }
function TComAddins.AddNewActionImages(iComObject: IUnknown): Integer;
var
  lNewImage : TBitmap;
  lFilePath : String;
begin
  Result := -1; // no image - default
  lNewImage := TBitmap.Create;
  try
    lFilePath := AppSettings.AddinPath + 'Images\';
     { Check we have an image }                                              
    if (iComObject as IRecorderAddin).ImageFileName <> '' then
    begin
      SafeLoadBmp(lNewImage,lFilePath +
                                 (iComObject as IRecorderAddin).ImageFileName);
      Result := dmFormActions.ilMenuOn.AddMasked(lNewImage,
                                                 lNewImage.TransparentColor);
    end;
  finally
    lNewImage.Free;
  end; // try.finally
end;  // AddNewActionImages

//==============================================================================
{ Accessor method to return a count of available addins }
function TComAddins.GetAddinCount: Integer;
begin
  Result := FAddinList.Count;
end;  // GetAddinCount

//==============================================================================
function TComAddins.GetAddinList(const iIndex: Integer): TAddinInfo;
begin
  Result := TAddinInfo(FAddinList[iIndex]);
end;  // GetAddinList

//==============================================================================
{ Reads an image for the addin from the disk.  Adds the image to the image list
     for addins and stores the index in the addininfo }
procedure TComAddins.SetupImage(var ioAddinInfo: TAddinInfo;
  iRecorderAddin: IRecorderAddin);
var
  lFileName : String;
  lBitmap : TBitmap;
begin
  if iRecorderAddin.ImageFileName = '' then
    lFileName := AppSettings.AddinPath + 'Images\' + 'default.bmp'
  else
    lFileName := AppSettings.AddinPath + 'Images\' + iRecorderAddin.ImageFileName;

  if not FileExists(lFileName) then begin
    lFileName := AppSettings.AddinPath + 'Images\' + 'default.bmp';
    if not fileExists(lFileName) then
      raise EComAddinError.Create(ResStr_AddinImageProblem);
  end;
  { Create a bitmap instance, load the file and store it }
  lBitmap := TBitmap.Create;
  try
    lBitmap.LoadFromFile(lFileName);
    ioAddinInfo.ImageIndex := FImages.AddMasked(lBitmap,lBitmap.TransparentColor);
  finally
    lBitmap.Free;
  end; // try..finally
end;  // SetupImage

//==============================================================================
{ Adds an action to the appropriate menu according to the COM interface.  If the
    parent menu doesn't already exist, then create it. }
procedure TComAddins.AddActionToMenu(iAction: TAction;
  iNewActionIntf: INewAction);
var
  i : Integer;
  lItemToAddTo : TMenuItem;
  lNewItem : TMenuItem;
begin
  lItemToAddTo := nil;
  for i := 0 to frmMain.mnuMain.Items.Count - 1 do
    if StripAmpersands(GetUntranslatedCaption(frmMain.mnuMain.Items[i])) =
        StripAmpersands(iNewActionIntf.ParentMenu) then
    begin
      lItemToAddTo := frmMain.mnuMain.Items[i];
      break; // from loop - found our parent item
    end;
  lNewItem := TMenuItem.Create(frmMain);
  lNewItem.Action := iAction;
  { If the parent item does not already exist, create it }
  if lItemToAddTo = nil then
  begin
    { need a new parent menu item }
    lItemToAddTo := TMenuItem.Create(frmMain);
    lItemToAddTo.Caption := iNewActionIntf.ParentMenu;
    { Splice it in before the Window menu }
    frmMain.mnuMain.Items.Insert(6,lItemToAddTo);
    { And create the toolbutton to hold the menu }
    with TXPToolbutton.Create(frmMain) do begin
      Left := frmMain.tbtnMnuWindow.Left;
      Parent := frmMain.tbMenu;
      MenuItem := lItemToAddTo;
      Autosize := True;
    end;
    FComItems.Add(lItemToAddTo);
  end;
  { Add the new action menu item }
  lItemToAddTo.Add(lNewItem);
  FComItems.Add(lNewItem);
end;  // AddActionToMenu

//==============================================================================
 {Overloaded version of above specifically for adding item before another
    already present item in the main menu}
procedure TComAddins.AddActionToMenu(iAction: TAction;
          iParentItem, iItemToInsertBefore : TMenuItem);
var
  lNewItem : TMenuItem;
begin
  lNewItem := TMenuItem.Create(frmMain);
  lNewItem.Action := iAction;
  { Remember it so we can free it! }
  FComItems.Add(lNewItem);
  { Insert it before the specified menu option }
  iParentItem.Insert(iItemToInsertBefore.MenuIndex, lNewItem);
end;  // AddActionToMenu

//==============================================================================
//==============================================================================
{ TAddinInfo }
//==============================================================================
{ Constructor - stores info in private data }
constructor TAddinInfo.Create(iClsID: TGUID; iName, iDescription: String);
begin
  inherited Create;
  FClsID := iClsID;
  FName := iName;
  FDescription := iDescription;
end;  // Create

//==============================================================================
{ Accessor method allowing the image index to be stored for the addin class }
procedure TAddinInfo.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;  // SetImageIndex

//==============================================================================
{ Procedure to load a bitmap, if the file exists.  If not, loads the default.bmp,
    and generates an exception if this is also missing }
procedure TComAddins.SafeLoadBmp(iBmp: TBitmap; const iFileName: String);
begin
  if FileExists(iFileName) then
    iBmp.LoadFromFile(iFileName)
  else
    iBMP.LoadFromFile(ExtractFilePath(iFileName) + 'default.bmp');
end;

//==============================================================================
procedure TComAddins.InitializeActionData(var ActionData: TActionData);
begin
  with ActionData do begin
    Caption  := '';
    Hint     := '';
    Enabled  := True;
    ShortCut := 0;
    Visible  := True;
  end;
end;

{-------------------------------------------------------------------------------
  Adds menu items to the main menu using the supplied information from the
    IDynamicMenuList, for the top level item at the supplied index. If
    iMenuItemList is supplied, it is populated with each item created, so the
    caller is able to further control the items created.
}
procedure TComAddins.ProcessTopLevelMenu(AMenuListIntf: IDynamicMenuList; AIndex: Integer;
    AImageList: TImageList; AMenuItemList: TObjectList);
begin
  ProcessTopLevelMenu(AMenuListIntf, AIndex, AImageList, FMainMenu.Items);
end;

{-------------------------------------------------------------------------------
  Adds menu items to the specified menu using the supplied information from the
    IDynamicMenuList, for the top level item at the supplied index.  If
    iMenuItemList is supplied, it is populated with each item created, so the
    caller is able to further control the items created.
}
procedure TComAddins.ProcessTopLevelMenu(AMenuListIntf: IDynamicMenuList; AIndex: Integer;
    AImageList: TImageList; AMenuToAddTo: TMenuItem; AMenuItemList: TObjectList);
var lMenuItem, lChildMenuItem, lSubItem: TMenuItem;
    i, lIndex          : Integer;
    lPath, lInsertPath : TStringList;
    lActionData        : TActionData;
begin
  if not Assigned(AMenuListIntf.Items(AIndex)) then Exit;
  
  lPath := TStringList.Create;
  lInsertPath := TStringList.Create;
  try
    InitializeActionData(lActionData);
    AMenuListIntf.Items(AIndex).GetItemData(lActionData);
    // Break down menu path into stringlist, easier to process
    lPath.CommaText := '"' + StringReplace(AMenuListIntf.MenuPath[AIndex], '\', '","', [rfReplaceAll]) + '"';
    // Setup the InsertAfter/BeforeMenu
    if AMenuListIntf.InsertAfterMenu[AIndex] <> '' then
      lInsertPath.CommaText := '"' + StringReplace(AMenuListIntf.InsertAfterMenu[AIndex], '\', '","', [rfReplaceAll]) + '"'
    else
    if AMenuListIntf.InsertBeforeMenu[AIndex] <> '' then
      lInsertPath.CommaText := '"' + StringReplace(AMenuListIntf.InsertBeforeMenu[AIndex], '\', '","', [rfReplaceAll]) + '"';

    // Start from the top.  If we have an ActiveMDIChild, then we are merging
    // with its menu and the main screens, otherwise just the main screen menu
    if Assigned(frmMain.ActiveMDIChild) then
      lChildMenuItem := TBaseChild(frmMain.ActiveMDIChild).mnuChildMerge.Items
    else
      lChildMenuItem := nil;

    lMenuItem := AMenuToAddTo;
    for i := 0 to lPath.Count - 1 do begin
      // Try to locate a menu item with the same caption
      lIndex := FindMenu(lPath[i], lMenuItem);
      if lIndex <> -1 then begin
        // Found existing menu in application's main menu, but if it's from COM addin,
        // ignore it, or we get menu items that don't show up when they should.
        if lMenuItem[lIndex].Tag = -1 then
          lIndex := -1
        else
          lMenuItem := lMenuItem[lIndex];
      end else
      if (i = 0) and Assigned(lChildMenuItem) then begin // search child menu as well
        lIndex := FindMenu(lPath[i], lChildMenuItem);
        if lIndex <> -1 then
          lMenuItem := lChildMenuItem[lIndex];
      end;

      if lIndex = -1 then begin
        // Add new menu, regardless. If menu becomes action, still need it anyway.
        if i = lPath.Count - 1 then begin
          if Assigned(frmMain.ActiveMdiChild) then
            lSubItem := TDynamicMenuItem.Create(frmMain.ActiveMDIChild)
          else
            lSubItem := TDynamicMenuItem.Create(frmMain);
          TDynamicMenuItem(lSubItem).MenuIntf := AMenuListIntf.Items(AIndex);
          TDynamicMenuItem(lSubItem).ImageList := AImageList;
        end
        else
          lSubItem := TMenuItem.Create(nil);
        if Assigned(AMenuItemList) then
          AMenuItemList.Add(lSubItem);
        lSubItem.Tag := -1;  // Use tag to indicate addin menuitem
        lSubItem.Caption := lPath[i];

        if lMenuItem <> FMainMenu.Items then begin
          // Last item in path, use the IDynamicMenu to build the menu structure
          if i = lPath.Count - 1 then
            TDynamicMenuItem(lSubItem).BuildDynamicMenu;
        end;

        // If lInsertPath is not empty, each item should correspond to the current
        // level of lPath being dealt with.
        if i < lInsertPath.Count then begin
          lIndex := FindMenu(lInsertPath[i], lMenuItem);
          // Found menu item
          if lIndex <> -1 then
            // Insert new either after or before existing one.
            if AMenuListIntf.InsertAfterMenu[AIndex] <> '' then
              lMenuItem.Insert(lIndex + 1, lSubItem)
            else
              lMenuItem.Insert(lIndex, lSubItem)
          else
            ShowInformation(Format(ResStr_AddinMenuPositionInvalid, [lInsertPath[i]]));
        end else
          // Just add, as there is no InsertAfter/BeforeMenu specified
          lMenuItem.Add(lSubItem);
        // Update menuitem to point to newly added one
        lMenuItem := lSubItem;
      end;
    end;
  finally
    lInsertPath.Free;
    lPath.Free;
  end;
end;

//==============================================================================
procedure TComAddins.SetMainMenu(const Value: TMainMenu);
begin
  FMainMenu := Value;
end;

//==============================================================================
procedure TComAddins.InitialiseDynamicMenus;
var
  lListIdx, lItemIdx: Integer;
begin
  if not Assigned(FMainMenu) then
    raise EComAddinError.Create(ResStr_CantInitDynamicMenus);
  // for each list
  for lListIdx := 0 to FDynamicMenuLists.Count - 1 do begin
    try
      GetDynamicMenuImageList(FDynamicMenuLists.Items[lListIdx] as IDynamicMenuList, lListIdx);
      // for each top level item on the list
      for lItemIdx := 0 to (FDynamicMenuLists.Items[lListIdx] as IDynamicMenuList).Count-1 do
        ProcessTopLevelMenu(FDynamicMenuLists.Items[lListIdx] as IDynamicMenuList,
            lItemIdx, TImageList(FDynamicMenuImageLists[lListIdx]));
    except on E:Exception do
      ShowInformation(Format(ResStr_AddinMenusFailed, [
          (FDynamicMenuLists.Items[lListIdx] as IRecorderAddin).Name,
          E.Classname, E.Message]));
    end;
  end;
end;

//==============================================================================
function TComAddins.FindMenu(const ACaption: String; AMenu: TMenuItem): Integer;
var lItemIdx: Integer;
    lMenuCaption, lSearchCaption: String;
begin
  Result := -1;
  for lItemIdx := 0 to AMenu.Count - 1 do begin
    // strip ampersands and ... from the caption (untranslated)
    lMenuCaption := GetUntranslatedCaption(AMenu[lItemIdx]);
    lMenuCaption := StringReplace(lMenuCaption, '&', '', [rfReplaceAll]);
    lMenuCaption := StringReplace(lMenuCaption, '.', '', [rfReplaceAll]);
    lSearchCaption := StringReplace(ReplaceAmpersands(ACaption), '&', '', [rfReplaceAll]);
    lSearchCaption := StringReplace(lSearchCaption, '.', '', [rfReplaceAll]);
    if CompareText(lMenuCaption, lSearchCaption) = 0 then
    begin
      Result := lItemIdx;
      Exit;
    end;
  end; // for
end;

{-------------------------------------------------------------------------------
  If the IDynamicMenu has an image list, instantiate one on the object list and
      duplicate the addins images onto it.  Note that an item is left on the
      list as nil if the image list is not required.
}
procedure TComAddins.GetDynamicMenuImageList(
  AMenuListIntf: IDynamicMenuList; AIndex: Integer);
var
  lImageList: TImageList;
begin
  // Ensure list is big enough
  while FDynamicMenuImageLists.Count-1<AIndex do
    FDynamicMenuImageLists.Add(nil);
  if AMenuListIntf.ImageListHandle>0 then begin
    lImageList := TImageList.Create(nil);
    // Need to duplicate the imagelist to access bitmaps properly
    lImageList.Handle := ImageList_Duplicate(HIMAGELIST(AMenuListIntf.ImageListHandle));
    FDynamicMenuImageLists[AIndex] := lImageList;
  end;
end;

{-------------------------------------------------------------------------------
  Checks if an object supports ISpatialReference or ISpatialReferenceList
}
procedure TComAddins.CheckSpatialReference(iComObject: IInterface);
var
  i: Integer;
  lSpatialRefIntf: ISpatialReference;
  lSpatialRefListIntf: ISpatialReferenceList;
begin
  if Supports(iComObject, IID_ISpatialReference, lSpatialRefIntf) then begin
    // Store the 4 character ID
    FSpatialSystems.Add(lSpatialRefIntf.SpatialRefSystem);
    // and an interface pointer
    FSpatialSystemInterfaces.Add(lSpatialRefIntf);
  end
  else if Supports(iComObject, IID_ISpatialReferenceList, lSpatialRefListIntf) then begin
    for i:=0 to lSpatialRefListIntf.SystemCount-1 do begin
      // Store the 4 character ID
      FSpatialSystems.Add(lSpatialRefListIntf.SystemInterface[i].SpatialRefSystem);
      // and an interface pointer
      FSpatialSystemInterfaces.Add(lSpatialRefListIntf.SystemInterface[i]);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Returns the untranslated version of a menu caption
}
function TComAddins.GetUntranslatedCaption(AMenuItem: TMenuItem): string;
var
  lCaption: string;
begin
  if Assigned(AMenuItem.Action) then begin
    if AMenuItem.Action.Name='' then
      Result := AMenuItem.Caption // can't undo translation for dynamic addin menu items
    else
      Result := LocalizerOnFly.PropValueByName(LocalizerOnFly.NativeLocale,
            TComponent(AMenuItem.Action.Owner).Name, AMenuItem.Action.Name + '.Caption')
  end
  else begin
    lCaption := 'frmMain';
    if Assigned(AMenuItem.Owner) then
      if AMenuItem.Owner is TForm then
        lCaption := TForm(AMenuItem.Owner).Name;
    Result := LocalizerOnFly.PropValueByName(LocalizerOnFly.NativeLocale,
          lCaption, AMenuItem.Name + '.Caption');
  end;
  if Result = '' then
    Result := AMenuItem.Caption;
end;

{-------------------------------------------------------------------------------
  Checks if an object supports IReportKeyTypeList
}
procedure TComAddins.CheckReportKeytype(iComObject: IInterface);
var
  i: Integer;
  lReportKeyTypeListIntf: IReportKeyTypeList;
begin
  if Supports(iComObject, IID_IReportKeyTypeList, lReportKeyTypeListIntf) then begin
    for i:=0 to lReportKeyTypeListIntf.KeytypeCount-1 do begin
      // and an interface pointer
      FReportKeyTypeInterfaces.Add(lReportKeyTypeListIntf.Keytype[i]);
    end;
  end;
end;

end.
