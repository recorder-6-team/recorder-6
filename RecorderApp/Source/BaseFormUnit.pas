//==============================================================================
//  Unit:        BaseFormUnit
//
//  Implements:  TBaseForm
//
//  Description: Defines the base class for the majority of forms used in the
//               application, and implements the common functions available for
//               each of these forms (setting drag/drop properties, add-ins
//               event handling, ...)
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon - 7 February 2002
//               New function to reset text attributes on DBRichEdit controls.
//
//               Eric Salmon - 19 March 2002
//               Moved Copy/Paste refresh to BaseDragFormUnit
//
//               Polly Shaw - 11 November 2002
//               Made a change to TBaseForm.SwitchToDetails meaning that
//               edit boxes are made ReadOnly rather than disabled.
//
//  Last Revision Details:
//    $Revision: 98 $
//    $Date: 9/03/09 14:09 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BaseFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Registry, ExtCtrls, StdCtrls, Grids, SpatialRef, BaseDragFormUnit,
  ComCtrls, DataClasses, ComFormControls, ExceptionForm, Recorder2000_TLB,
  Constants, Htmlview, DBCtrls, AddinLinkedControls, Contnrs;

type
  EBaseFormError = class(EBaseDragFormError);

  TRequestorUpdateFunction = procedure(KeyList: TKeyList) of object;

  // Accessor class for TCustomEdit protected properties
  TDummyEdit = class(TCustomEdit);

  TBaseForm = class(TBaseDragForm)
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    FSupplierForm     : TBaseForm;
    FRequestorForm    : TBaseForm;
    FPageProxys : TList;
    FEventAddins : TInterfaceList;
    FComForm : TComForm;  // COM interface to form - only present if required
    FCreatedObjects : TStringList;
    FRequestor : IRequestor;
    FLastKey : TKeyString;
    FLastTable : string;
    FDualEvents : TObjectList;
    FReadOnlyColour: TColor;
    procedure SetFontStyleButtons(Sender: TObject);
    procedure UnlinkReturnDataSupplier;
    procedure ProcessComponentsList(AOwnerComponent: TComponent; AColSource,
        AColDest: TColor);
    function CombineEvents(firstEvent, secondEvent: TNotifyEvent): TNotifyEvent;
  protected
    FEditDetails: Boolean; // edit mode state
    function AlreadyCreated(const iGUID: String): Boolean; // has this form created an addin?
    procedure ChangeAdditionalPage(Sender : TObject);
    procedure CheckAdditionalPages;
    procedure CheckForEventAddins;
    procedure DoFormEventAddinAdd;
    procedure DoFormEventAddinCancels;
    procedure DoFormEventAddinChanges(const iTableName: string; const iKeyValue: TKeyString);
    procedure DoFormEventAddinDelete;
    procedure DoFormEventAddinEditModes;
    procedure DoFormEventAddinSaves;
    function DoFormEventCheckCanSave: Boolean;
    function FindPageControl: TPageControl;
    function GetGuid(const iNameValue: String): TGuid;
    procedure NotifyDataItemChange; virtual;
    procedure RequestorUpdateProxy(KeyList: TKeyList);
    procedure ResetDBRichEditControls(AControls: array of TDBRichEdit);
    procedure SetFirstPage( iControl : TPageControl );
    procedure SetRequestorForm(const Value: TBaseForm);
    procedure SetSupplierForm(const Value: TBaseForm);
    procedure SwitchToDetails(sgGrid : TStringGrid; iAdd, iEdit, iDel,
      iSave, iCancel:TControl; gbDetails:TGroupBox; tfDetails: Boolean); virtual;
    procedure ReadSortOrders; virtual;
    procedure WriteSortOrders; virtual;
    procedure EnableDetailsAddEditButtons(const ATable, APKName, AKey,
      ACustodian: string; AEditMode: TEditMode; AAdded: boolean;
      AEditButton, ADeleteButton: TButton;
      AOnlyCustodianCanDelete: boolean = false);
    procedure WMDoReturnData(var Message: TMessage); message WM_DORETURNDATA;
  public
    //Update procedure for return data
    RequestorUpdate: TRequestorUpdateFunction;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddAdditionalPages;
    procedure CancelAdditionalPages;
    function CheckAdditionalPageCanSave: Boolean;
    procedure DeleteAdditionalPages;
    function GetKeyList: TKeyList; virtual;
    function GetKeyListForValidation: TKeyList; virtual;
    function GetRecorderFormEvents(iGUID: TGuid): IUnknown;
    procedure RefreshXPMenu; virtual;
    procedure SaveAdditionalPages;
    procedure SetAdditionalPagesState(iEnabled: Boolean);
    procedure SetDragColours;
    procedure SetReadOnlyFieldsColourState(const AReadOnly: Boolean; AControls: array of TWinControl);
    procedure SetRequiredFieldsColourState(const AEditing: Boolean; AControls:array of TWinControl);
    procedure SetupCOMLink(Supplier, Requestor: TBaseForm; RequestorUpdate: IRequestor);
    procedure SetupLink(Supplier, Requestor: TBaseForm; RequestorUpdate: TRequestorUpdateFunction);
    procedure UpdateMapWindowSelector; virtual;
    procedure ValidateValue(const iValue:boolean; const iMsg:string); overload;
    procedure ValidateValue(const iValue:boolean; const iMsg:string;
      const iControl:TWinControl); overload;
    property ReadOnlyColour: TColor read FReadOnlyColour write FReadOnlyColour;
    property  SupplierForm:TBaseForm read FSupplierForm write SetSupplierForm;
    property  RequestorForm:TBaseForm read FRequestorForm write SetRequestorForm;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Maintbar, CompositeComponent, VagueDateEdit, DropStruct, Clipbrd,
  DropTarget, OLETools, ComObj, ComClasses, ApplicationSettings, GeneralData,
  GeneralFunctions, AddinCompositeComponent;

resourcestring
  ResStr_CantSetAddPageKey  = 'The additional page''s current item cannot be set.';
  ResStr_InvalidNameValue   = 'Attempt to read a GUID from an invalid name value pair';
  ResStr_ProcedureError     = 'Error occurred in TBaseForm ';
  ResStr_InvalidKeyList     = 'The keylist containing the newly selected data is invalid';

////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
//                              Hack zone, be warned!!!!                                  //
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
type
  TDummyColourControl = class(TControl)  // Colour property needs to be surfaced so it can
  public                                 // be changed for any kind of TWinControl.
    property Color;                      // Use as typecasting class.
  end;                                   // See SetRequiredFieldsColourState for how it's used.
////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
   
  // Class to allow two events to be combined together into a single event.
  TDualEvent = class(TObject)
  private
    FFirstEvent: TNotifyEvent;
    FSecondEvent: TNotifyEvent;
  public
    constructor Create(firstEvent, secondEvent: TNotifyEvent);
    procedure Call(Sender: TObject);
  end;
  
//==============================================================================
constructor TBaseForm.Create(AOwner: TComponent);
begin
  try
    FDualEvents := TObjectList.Create;
    inherited Create(AOwner);
    { Create a string list to store GUIDs of created objects }
    if FCreatedObjects = nil then
      FCreatedObjects := TStringList.Create;
    SetDragColours;
    CheckAdditionalPages;
    CheckForEventAddins;
    ReadSortOrders;
    FReadOnlyColour := clBtnFace;
  except
    on E:Exception do
      raise EBaseFormError.Create( ResStr_ProcedureError + 'constructor', E );
  end;
end;  // Create

//==============================================================================
destructor TBaseForm.Destroy;
var
  i : integer;
begin
  FDualEvents.Free;
  WriteSortOrders;
  //Remove return data link if required
  //Remove requestor link
  if Assigned(SupplierForm) then
  begin
    UnlinkReturnDataSupplier;
    SupplierForm:= Nil;
  end;

  //Remove supplier link
  if Assigned(RequestorForm) then
  begin
    RequestorForm.SupplierForm:= Nil;
    RequestorForm:= Nil;
    RequestorUpdate:= Nil;
  end;

  if FPageProxys <> nil then // this list is only created if required
  begin
    for i := 0 to FPageProxys.Count-1 do
      TOLEProxy(FPageProxys[i]).Free;
    FPageProxys.Free;
  end;

  if FCreatedObjects <> nil then FCreatedObjects.Free;

  FEventAddins.Free; // doesn't matter if not created
  FEventAddins := nil;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TBaseForm.FormActivate(Sender: TObject);
begin
  inherited;
  (Application.MainForm as TfrmMain).CurrentForm := Self;
  dmFormActions.actTransferData.Enabled:= Assigned(RequestorForm);
end;  // FormActivate

//==============================================================================
procedure TBaseForm.FormDeactivate(Sender: TObject);
begin
  if (Application.MainForm as TfrmMain).CurrentForm = Self then
    (Application.MainForm as TfrmMain).CurrentForm := nil;
end;  // FormDeactivate

//==============================================================================
{ Set the colour of any drag drop panels according to the user settings.
     Now, also sets bold/italic/underline checked status handlers in rich
     edir controls,  }
procedure TBaseForm.SetDragColours;
var
  colSource, colDest : TColor;
begin
  try
    if AppSettings.DisableDragDropFrames then begin
      colSource:=Color;
      colDest  :=Color;
    end else begin
      colSource:=AppSettings.DragSourceColour;
      colDest  :=AppSettings.DragDestColour;
    end;
    ProcessComponentsList(Self, colSource, colDest);
  except
    on E:Exception do
      raise EBaseFormError.Create( ResStr_ProcedureError + 'SetDragColours', E );
  end;
end;  // SetDragColour

//==============================================================================
procedure TBaseForm.SetRequestorForm(const Value: TBaseForm);
begin
  FRequestorForm := Value;
end;  // SetRequestorForm

//==============================================================================
procedure TBaseForm.SetSupplierForm(const Value: TBaseForm);
begin
  FSupplierForm:=Value;
end;  // SetSupplierForm

//==============================================================================
procedure TBaseForm.SetupLink(Supplier, Requestor: TBaseForm;
  RequestorUpdate: TRequestorUpdateFunction);
begin
  //Setup two-way link between forms
  //Supplier holds method pointer to update procedure in requestor form

  //Check for an existing link to another supplier and remove
  if Assigned(SupplierForm) then
  begin
    UnlinkReturnDataSupplier;
    SupplierForm:= Nil;
  end;

  //Check for an existing link to the new supplier and remove
  if Assigned(Supplier.RequestorForm) then
  begin
    Supplier.RequestorForm.SupplierForm:= Nil;
    Supplier.RequestorForm:= Nil;
    Supplier.RequestorUpdate:= Nil;
  end;

  //Create new link
  SupplierForm:= Supplier;
  Supplier.RequestorForm:= Requestor;
  Supplier.RequestorUpdate:= RequestorUpdate;

  //Activate form
  Supplier.Activate;
end;  // SetupLink

//==============================================================================
{ Version of setup link called when a COM addin requests data.  This time, the
     Requestor update is an interface specifying the method to call.  We use
     our own RequestorUpdateProxy method to call this interface's method when
     data is returned, so all the other standard code will work }
procedure TBaseForm.SetupCOMLink(Supplier, Requestor: TBaseForm;
  RequestorUpdate: IRequestor);
begin
  { Store the interface }
  FRequestor := RequestorUpdate;
  { Call standard SetupLInk with our proxy method }
  SetupLink(Supplier, Requestor, RequestorUpdateProxy);
end;  // SetupCOMLink

//==============================================================================
{ Scan the COM add-ins for additional pages for this form's tab notebook }
procedure TBaseForm.CheckAdditionalPages;
var
  i : integer;
  lPageControl : TPageControl;
  lTabSheet : TTabSheet;
  lOLEProxy : TOLEProxy;
  lGUID : string;
begin
  try
    lPageControl := nil; // safety
    for i := 0 to AppSettings.ComAddins.AdditionalPages.Count-1 do
      if AppSettings.ComAddins.AdditionalPages.Names[i] = Self.ClassName then
      begin
        if lPageControl = nil then
        begin
          lPageControl := FindPageControl;
          if lPagecontrol = nil then
            Exit; // can't do anything - no page control.  Don't raise any exception
        end;
        lTabSheet := TTabSheet.Create(self);
        { Stick the new sheet onto the control }
        lTabSheet.PageControl := lPageControl;
        { Create a proxy object to the COM class }
        with AppSettings.ComAddins do
          lGUID := Copy(AdditionalPages[i], Pos('=', AdditionalPages[i])+1, 255);
        lOLEProxy := TOLEProxy.Create( Self, StringToGUID(lGUID) );
        lOLEProxy.Parent := lTabSheet;
        lOleProxy.Align := alClient;
        lTabSheet.Caption := (lOleProxy.ControlInterface as IAdditionalPage).
                             PageCaption;
        if Supports(lOleProxy.ControlInterface, IAdditionalPage6) then
          lTabSheet.PageIndex := (lOleProxy.ControlInterface as IAdditionalPage6).TabIndex;
        { Create a list to keep any COM Proxy objects which will need freeing }
        if FPageProxys = nil then
          FPageProxys := TList.Create;
        FPageProxys.Add(lOleProxy);
        FCreatedObjects.Add(lGUID);
      end; // if
  except
    on E:Exception do
      raise EBaseFormError.Create( ResStr_ProcedureError + 'CheckAdditionalPages', E );
  end;
end;  // CheckAdditionalPages

//==============================================================================
{ Look through components on the page for a TPageControl.  Returns the first one
    found - so there ought to be only one.  Returns nil if none present }
function TBaseForm.FindPageControl: TPageControl;
var
  i : integer;
begin
  Result := nil; // default
  for i := 0 to ComponentCount-1 do
  begin
    if Components[i] is TPageControl then
    begin
      Result := TPageControl(Components[i]);
      Break; // from loop - finished
    end;
  end;
end;  // FindPageControl

//==============================================================================
{ Sets EditMode in any relevant addins }
procedure TBaseForm.SetAdditionalPagesState(iEnabled: Boolean);
begin
  if iEnabled then
    DoFormEventAddinEditModes;
end;  // SetAdditionalPagesState

//==============================================================================
{ Called when data is posted to the database.  Triggers additional pages
     and embedded activex's to dothe same }
procedure TBaseForm.SaveAdditionalPages;
begin
  DoFormEventAddinSaves;
  UnlinkReturnDataSupplier;
end;  // SaveAdditionalPages

//==============================================================================
{ Called when data is cancelled in the database.  Triggers additional pages
    and embedded activeX's to do the same }
procedure TBaseForm.CancelAdditionalPages;
begin
  DoFormEventAddinCancels;
  UnlinkReturnDataSupplier;
end;  // CancelAdditionalPages

//==============================================================================
{ Unlinks the return data between this form and a supplier form}
procedure TBaseForm.UnlinkReturnDataSupplier;
begin
  If SupplierForm <> nil then begin
    SupplierForm.RequestorForm:= Nil;
    SupplierForm.RequestorUpdate:= Nil;
  end; // if
end;  // UnlinkReturnDataSupplier

//==============================================================================
{ Called when a form is put into ADD mode.
  Triggers additional pages and embedded activeX's to do the same }
procedure TBaseForm.AddAdditionalPages;
begin
  DoFormEventAddinAdd;
end;  // AddAdditionalPages

//==============================================================================
{ Called when a form tries to save changes.
  Triggers additional pages and embedded activeX's to do the same }
function TBaseForm.CheckAdditionalPageCanSave : boolean;
begin
  Result := DoFormEventCheckCanSave;
end;  // CheckAdditionalPageCanSave

//==============================================================================
{ Called when a form asked to delete an item.
  Triggers additional pages and embedded activeX's to do the same }
procedure TBaseForm.DeleteAdditionalPages;
begin
  DoFormEventAddinDelete;
end;  // DeleteAdditionalPages

//==============================================================================
{ Procedure to handle a switch to a COM additional page in a tab notebook.
     Should be called by a form every time the current item changes.
     Sender is the page control changing, or nil. }
procedure TBaseForm.ChangeAdditionalPage(Sender: TObject);
var
  lKeyList : TKeyList;
  i, j : integer;
  lOleProxy : TOLEProxy;
  lPageControl : TPageControl;
  lTabSheet : TTabSheet;
  lAddPageIntf : IAdditionalPage;
  lCurrentTable : string;
begin
  lOleProxy := nil; // safety
  {Call inherited form to get current item keylist}
  lKeyList := GetKeyList;
  try
    { Validate the KeyList - must have 1 sigle key item }
    If lKeyList = nil then
      raise EBaseFormError.Create(ResStr_CantSetAddPageKey);
    if (lKeyList.Header.ItemCount <> 1) then
      raise EBaseFormError.Create(ResStr_CantSetAddPageKey);
    if (lKeyList.Items[0].KeyField2 <> '') and
                       (lKeyList.Header.TableName <> MIXED_DATA) then
      raise EBaseFormError.Create(ResStr_CantSetAddPageKey);
    { Find the KeyList's Table name }
    if lKeyList.Header.TableName <> MIXED_DATA then
      lCurrentTable := lKeyList.Header.TableName
    else
      lCurrentTable := lKeyList.Items[0].KeyField2;
    { iterate thro' all pages of the sender control seeing whether or not they contain
      TOLEProxy controls and if so notify of the currently selected key }
    lPageControl := TPageControl(Sender);
    if lPageControl <> nil then
      for j := 0 to lPageControl.PageCount - 1 do begin
        lTabSheet := TTabSheet(lPageControl.Pages[j]);
        { First find the OleProxy component }
        for i := 0 to lTabSheet.ControlCount - 1 do
          if lTabSheet.Controls[i] is TOLEProxy then
          begin
            lOleProxy := TOleProxy(lTabSheet.Controls[i]);
            break; // from loop
          end;
        if lOleProxy <> nil then
        begin
          { Find an interface pointer to the COM object }
          try
            lAddPageIntf := lOleProxy.ControlInterface as IAdditionalPage;
            { And pass it the table and itemkey }
            lAddPageIntf.CurrentTable := lCurrentTable;
            lAddPageIntf.CurrentKey := lKeyList.Items[0].KeyField1;
          except
            on EIntfCastError do;  // nothing, may exist TOLEProxy objects which are not
                                   // additional pages
          end;
        end;
      end; // for
  finally
    lKeyList.Free;
  end;
end;  // ChangeAdditionalPage

//==============================================================================
{ Get KeyList function which most or all sub-forms should override.  This
    provides a safety dummy for cases where they don't.  Should return the
    currently selected item on the form. }
function TBaseForm.GetKeyList: TKeyList;
begin
  Result := nil;
end;  // GetKeyList

{-------------------------------------------------------------------------------
  Get KeyListForValidation should return items that need to be (re)validated.
}
function TBaseForm.GetKeyListForValidation: TKeyList;
begin
  Result := nil;
end;  // GetKeyListForValidation

//==============================================================================
{ Checks if there are any event addins for the current form }
procedure TBaseForm.CheckForEventAddins;
var
  i : integer;
  lEventAddinIntf : IUnknown;
  lEventIntf : IRecorderFormEvents;
  lFormIntf  : IRecorderForm;
  lGUID : TGUID;
begin
  try
    { First quick scan to see if there is anything to do }
    if AppSettings.ComAddins.FormEventAddins.IndexOfName( Self.ClassName ) <> -1 then begin
      { Now loop to find ALL the event addins for this form }
      with AppSettings.ComAddins do
      begin
        for i := 0 to FormEventAddins.Count-1 do
          if CompareText(FormEventAddins.Names[i], Self.ClassName) = 0 then
          begin
            { Create a list to hold the interfaces }
            if FEventaddins = nil then
              FEventaddins := TInterfaceList.Create;
            { Create something to implement IRecorderForm }
            if FComForm = nil then
            begin
              FComForm := TComForm.Create(Self);
              lFormIntf := FComForm as IRecorderForm;
            end;
            lGUID := GetGuid(FormEventAddins[i]);
            if not AlreadyCreated(GUIDToString(lGUID)) then begin
              lEventAddinIntf := CreateComObject(lGUID);
            end else begin
              if GetRecorderFormEvents(lGUID) <> nil then
                lEventAddinIntf := GetRecorderFormEvents(lGUID)
              else
                raise EBaseFormError.Create( ResStr_ProcedureError + 'GetRecorderFormEvents' );
            end;
            FEventAddins.Add(lEventAddinIntf);
            FCreatedObjects.Add(GuidToString(lGUID));
            try
              lEventIntf := lEventAddinIntf as IRecorderFormEvents;
              lEventIntf.SetForm(lFormIntf);
            except
              on E:Exception do
                raise EBaseFormError.Create('Error occurred in Set form method of addin ' +
                      (lEventAddinIntf as IRecorderAddin).Name, E);
            end;
          end;
      end; // with
    end;
  except
    on E:Exception do
      raise EBaseFormError.Create( ResStr_ProcedureError + 'CheckForEventAddins', E );
  end;
end;  // CheckForEventAddins

//==============================================================================
{ Returns a GUID extracted from a name/value pair string }
function TBaseForm.GetGuid(const iNameValue: String): TGuid;
var
  lEqualsPos : integer;
begin
  lEqualsPos := Pos('=',iNameValue);
  if lEqualsPos = 0 then
    raise EBaseFormError.Create(ResStr_InvalidNameValue);
  Result := StringToGUID(Copy(iNameValue, lEqualsPos + 1, 255));
end;  // GetGuid

//==============================================================================
{ Signal the event of an item changing to any addins that want to know }
procedure TBaseForm.DoFormEventAddinChanges(const iTableName: String;
  const iKeyValue : TKeyString );
var
  i : integer;
begin
  if FEventAddins <> nil then begin
    if (FLastKey<>iKeyValue) or (FLastTable<>iTableName) then begin  // Check this really is a change
      for i := 0 to FEventAddins.Count-1 do
        (FEventAddins[i] as IRecorderFormEvents).DoItemChange
                            ( iTableName, iKeyValue );
      // Remember values for checking nex time
      FLastKey := iKeyValue;
      FLastTable := iTableName;
    end;
  end;
end;  // DoFormEventAddinChanges

//==============================================================================
{ Signal the event of an item cancel to any addins that want to know }
procedure TBaseForm.DoFormEventAddinCancels;
var
  i : integer;
begin
  if FEventAddins <> nil then
    for i := 0 to FEventAddins.Count-1 do
      (FEventAddins[i] as IRecorderFormEvents).DoCancel;
end;  // DoFormEventAddinCancel

//==============================================================================
{ Signal the event of an item save to any addins that want to know }
procedure TBaseForm.DoFormEventAddinSaves;
var
  i : integer;
begin
  if FEventAddins <> nil then
    for i := 0 to FEventAddins.Count-1 do
      (FEventAddins[i] as IRecorderFormEvents).DoSave;
end;  // DoFormEventAddinSaves

//==============================================================================
{ Signal going into edit mode for addins that need to know }
procedure TBaseForm.DoFormEventAddinEditModes;
var
  i : integer;
begin
  if FEventAddins <> nil then
    for i := 0 to FEventAddins.Count-1 do
      (FEventAddins[i] as IRecorderFormEvents).DoEditMode;
end;  // DoFormEventAddinEditModes

//==============================================================================
{ Signal the event of an item ADD to any addins that want to know }
procedure TBaseForm.DoFormEventAddinAdd;
var
  i : integer;
begin
  if FEventAddins <> nil then
    for i := 0 to FEventAddins.Count-1 do
      (FEventAddins[i] as IRecorderFormEvents).DoAdd;
end;  // DoFormEventAddinAdd

//==============================================================================
{ Signal the event of an item DELETE to any addins that want to know }
procedure TBaseForm.DoFormEventAddinDelete;
var
  i : integer;
begin
  if FEventAddins <> nil then
    for i := 0 to FEventAddins.Count-1 do
      (FEventAddins[i] as IRecorderFormEvents).DoDelete;
end;  // DoFormEventAddinDelete

//==============================================================================
{ Signal that the item is about to save for any addins that want to know }
function TBaseForm.DoFormEventCheckCanSave : boolean;
var
  i : integer;
begin
  Result := true; // default
  if FEventAddins <> nil then
    for i := 0 to FEventAddins.Count-1 do
      // set to false if any event addin fails
      Result := (FEventAddins[i] as IRecorderFormEvents).CheckCanSave and Result;
end;  // DoFormEventCheckCanSave

//==============================================================================
procedure TBaseForm.ValidateValue(const iValue: boolean; const iMsg: string);
begin
  if not iValue then
    raise TExceptionPath.CreateNonCritical(iMsg);
end;  // Validate

//==============================================================================
procedure TBaseForm.ValidateValue(const iValue: boolean; const iMsg: string;
  const iControl: TWinControl);
begin
  if not iValue then begin
    if iControl.Parent is TTabSheet then
      TTabSheet(iControl.Parent).PageControl.TabIndex := TTabSheet(iControl.Parent).PageIndex;
    if iControl.Visible then // safety in case hidden by COM addin
      iControl.SetFocus;
    raise TExceptionPath.CreateValidation(iMsg,iControl);
  end;
end;  // Validate

//==============================================================================
procedure TBaseForm.SetReadOnlyFieldsColourState(const AReadOnly: Boolean;
  AControls: array of TWinControl);
var
  colour: TColor;
  i: Integer;
begin
  if AReadOnly then
    colour := ReadOnlyColour
  else
    colour := clWindow;

  for i := 0 to High(AControls) do
    if AControls[i] is TAddinLinkedEdit then
      TAddinLinkedEdit(AControls[i]).Color := colour
    else
      TDummyColourControl(AControls[i]).Color := colour;
end;  // SetReadOnlyFieldsColourState

//==============================================================================
procedure TBaseForm.SetRequiredFieldsColourState(const AEditing: boolean;
  AControls: array of TWinControl);
var
  colour: TColor;
  i: Integer;
begin
  if AEditing then
    colour := AppSettings.MandatoryColour
  else
    colour := clWindow;

  for i := 0 to High(AControls) do
    if AControls[i] is TAddinLinkedEdit then
      TAddinLinkedEdit(AControls[i]).Color := colour
    else
      TDummyColourControl(AControls[i]).Color := colour;
end;  // SetRequiredFieldsColourState

//==============================================================================
function TBaseForm.AlreadyCreated(const iGUID: String): Boolean;
begin
  Result := False;
  if FCreatedObjects.IndexOf(iGUID) <> -1 then Result := True;
end;  // AlreadyCreated

//==============================================================================
// loop through IRecorderFormEvents objects to find a match for our Guid
function TBaseForm.GetRecorderFormEvents(iGUID: TGuid): IUnknown;
var
  i : integer;
  lCurrent : TOLEProxy;
begin
  result := nil; // default
  if FPageProxys <> nil then
    for i := 0 to FPageProxys.Count - 1 do begin
      lCurrent := TOLEProxy(FPageProxys[i]);
      if CompareGUIDs(lCurrent.AssignedGUID, iGUID) then begin
        result := TOLEProxy(FPageProxys[i]).ControlInterface;
        break;
      end; // if
    end; // for
end;  // GetRecorderFormEvents

//==============================================================================

{ Proxy method to map return data update calls onto the interface supplied by
     a COM Addin }
procedure TBaseForm.RequestorUpdateProxy(KeyList: TKeyList);
var
  lKeyList : TComKeyList;
begin
  if FRequestor <> nil then
  begin
    lKeyList := TComKeyList.Create(KeyList);
    FRequestor.Update(lKeyList);
    FRequestor := nil;
    BringToFront;
  end;
end;  // RequestorUpdateProxy

//==============================================================================
{ Event handler to set the font style button status as you navigate rich text.
    Must be assigned to both the OnChange and OnSelectionChange of the rich
    text to work correctly }
procedure TBaseForm.SetFontStyleButtons( Sender: TObject);
begin
  inherited;
  dmFormActions.ActBold.Checked := fsBold in TCustomRichEdit(Sender).SelAttributes.Style;
  dmFormActions.ActItalic.Checked := fsItalic in TCustomRichEdit(Sender).SelAttributes.Style;
  dmFormActions.ActUnderline.Checked := fsUnderline in TCustomRichEdit(Sender).SelAttributes.Style;
end;  // SetFontStyleButtons

//==============================================================================
{ Locates the first visible tab page on a page control, and shows it }
procedure TBaseForm.SetFirstPage(iControl: TPageControl);
var
  i : integer;
begin
  i := 0;
  while not ((iControl.Pages[i].TabVisible) or (i=iControl.PageCount-1)) do
  begin
    Inc(i);
  end;
  if iControl.Pages[i].TabVisible then
    iControl.ActivePage := iControl.Pages[i];
end;  // SetFirstPage

//==============================================================================
{ For grid entry via a details panel, handles the enabling/disabling of controls
    en masse.  Virtual }
procedure TBaseForm.SwitchToDetails(sgGrid : TStringGrid;iAdd, iEdit, iDel,
          iSave, iCancel:TControl; gbDetails:TGroupBox; tfDetails:boolean);
var iIndex:integer;
begin
  { Note - this must all be handled by setting the dataset readonly property!}
  if gbDetails <> nil then with gbDetails do
    for iIndex:=0 to ControlCount-1 do
    begin
      // control tag of 1 indicates a source edit box that must remain enabled
      if (not (Controls[iIndex] is TLabel)) and (not (Controls[iIndex] is TCustomMemo))
                    and (not ((Controls[iIndex] is TEdit) and (Controls[iIndex].Tag=1))) then begin
        Controls[iIndex].Enabled:=tfDetails;
        if Controls[iIndex] is TAddinLinkedEdit then begin
          if tfDetails then
            TAddinLinkedEdit(Controls[iIndex]).EditMode := AddinCompositeComponent.emEdit
          else
            TAddinLinkedEdit(Controls[iIndex]).EditMode := AddinCompositeComponent.emBrowse;
          TAddinLinkedEdit(Controls[iIndex]).EditBox.Enabled := tfDetails;
        end;
      end
      else if Controls[iIndex] is TCustomMemo then
        // for memos, we must allow scroll bar use
        TDummyEdit(Controls[iIndex]).ReadOnly := not tfDetails
      else if (Controls[iIndex] is TEdit) and (Controls[iIndex].Tag=1) then
        TEdit(Controls[iIndex]).ReadOnly := not tfDetails
    end;

  if sgGrid<>nil then
    sgGrid.Enabled:=not tfDetails;
  if iAdd<>nil then
    iAdd.Enabled :=not tfDetails;
  if iEdit<>nil then
    iEdit.Enabled:=not tfDetails and (sgGrid.Objects[0,1]<>nil);
  if iDel<>nil then
    iDel.Enabled :=iEdit.Enabled;
  if iSave<>nil then
    iSave.Enabled  :=not tfDetails;
  if iCancel <> nil then
    iCancel.Enabled:=not tfDetails;
  // Used to enable or disable drag/drop functionality on detail part
  FEditDetails  :=tfDetails;
end;  // SwitchToDetails


procedure TBaseForm.ReadSortOrders;
begin
  // To be implemented by some child classes
end;

procedure TBaseForm.WriteSortOrders;
begin
  // To be implemented by some child classes
end;

procedure TBaseForm.NotifyDataItemChange;
var
  lKeyList : TKeyList;
  lCurrentTable : string;
begin
  lCurrentTable := '';
  lKeyList := GetKeyList;
  try
    { Validate the KeyList - must have 1 sigle key item }
    If lKeyList = nil then
      Exit;    // cannot notify a change as the keylist is nil;
    if (lKeyList.Header.ItemCount <> 1) then
      raise EBaseFormError.Create(ResStr_InvalidKeyList);
    if (lKeyList.Items[0].KeyField2 <> '') and
                       (lKeyList.Header.TableName <> MIXED_DATA) then
      raise EBaseFormError.Create(ResStr_InvalidKeyList);
    { Find the KeyList's Table name }
    if lKeyList.Header.TableName <> MIXED_DATA then
      lCurrentTable := lKeyList.Header.TableName
    else
      lCurrentTable := lKeyList.Items[0].KeyField2;
    { Send the TableName and Key to the Add-In }
    DoFormEventAddinChanges(lCurrentTable, lKeyList.Items[0].KeyField1);
  finally
    lKeyList.Free;
  end;
end; // NotifyDataItemChange

//==============================================================================
procedure TBaseForm.ResetDBRichEditControls(AControls: array of TDBRichEdit);
var liIdx: Integer;
begin
  for liIdx := 0 to High(AControls) do
    with AControls[liIdx] do begin
      Clear;
      with DefAttributes do begin
        Style := [];
        Color := clBlack;
        Name  := Self.Font.Name;
        Size  := Self.Font.Size;
      end;
    end;
end;  // ResetDBRichEditControls

{-------------------------------------------------------------------------------
}
procedure TBaseForm.RefreshXPMenu;
begin
  if Assigned(frmMain.XPMenu) then
    frmMain.XPMenu.InitComponent(Self);
end;

{-------------------------------------------------------------------------------
}
procedure TBaseForm.UpdateMapWindowSelector;
begin
  // Override as and when needed.
end;

{-------------------------------------------------------------------------------
  Enable the buttons for editing or deleting a sub item on a list
}
procedure TBaseForm.EnableDetailsAddEditButtons(const ATable, APKName,
          AKey, ACustodian: string; AEditMode: Constants.TEditMode;
          AAdded: boolean; AEditButton, ADeleteButton: TButton;
          AOnlyCustodianCanDelete: boolean);
var
  lFullEditAccess: boolean;
begin
  if AAdded then begin
    if Assigned(AEditButton) then
      AEditButton.Enabled := true;
    if Assigned(ADeleteButton) then
      ADeleteButton.Enabled := true;
  end
  else begin
    lFullEditAccess := dmGeneralData.HasFullEditAccess(ATable, APKName, AKey);
    if Assigned(AEditButton) then
      AEditButton.Enabled := AppSettings.AllowEdit(AEditMode) and
        (AAdded or ((ACustodian = AppSettings.SiteID)
        and lFullEditAccess));
    if Assigned(ADeleteButton) then
      ADeleteButton.Enabled  := AppSettings.AllowEdit(AEditMode)
        and lFullEditAccess
        and ((not AOnlyCustodianCanDelete) or AAdded or (ACustodian = AppSettings.SiteID));
  end;
end;

{-------------------------------------------------------------------------------
  Loop through the components list and set up the drag drop colours, plus
      check for TNameLinkedEdits which need to know the logged in user info
      to enable the F11 key.
}
procedure TBaseForm.ProcessComponentsList(AOwnerComponent: TComponent;
    AColSource, AColDest: TColor);
var
  i: integer;
begin
  with AOwnerComponent do
    for i:= 0 to ComponentCount-1 do
    begin
      if Components[i] is TPanel then
        case Components[i].Tag of
          1 : TPanel(Components[i]).Color:=AColSource;
          2 : TPanel(Components[i]).Color:=AColDest;
          3 : TPanel(Components[i]).Color:=clBtnFace;
        end  // case
      else if Components[i] is TShape then
        case Components[i].Tag of
          1 : TShape(Components[i]).Pen.Color:=AColSource;
          2 : TShape(Components[i]).Pen.Color:=AColDest;
          3 : begin
                TShape(Components[i]).Brush.Color:=AColSource;
                TShape(Components[i]).Pen.Color  :=AColDest;
              end;
        end  // case
      else if Components[i] is TTabSheet then
        case Components[i].Tag of
          1 : TTabSheet(Components[i]).Brush.Color := AColSource;
          2 : TTabSheet(Components[i]).Brush.Color := AColDest;
        end // case
      else if Components[i] is TCompositeComponent then begin
        TCompositeComponent(Components[i]).SourceCol:=AColSource;
        TCompositeComponent(Components[i]).DestCol:=AColDest;
      end
      else if Components[i] is TAddinLinkedEdit then begin
        TAddinLinkedEdit(Components[i]).DragSourceColour:=AColSource;
        TAddinLinkedEdit(Components[i]).DragDestinationColour:=AColDest;
        // Also check for name linked edits to set up the username info for F11 shortcut
        if Components[i] is TNameLinkedEdit then begin
          TNameLinkedEdit(Components[i]).NameKey  := AppSettings.UserId;
          TNameLinkedEdit(Components[i]).NameText := AppSettings.UserName;
        end;
      end
      else if Components[i] is TFrame then
        // recurse into the controls owned by the frame
        ProcessComponentsList(Components[i], AColSource, AColDest);
      if Components[i] is TRichEdit then begin
        TRichEdit(Components[i]).OnChange :=
            CombineEvents(TRichEdit(Components[i]).OnChange, SetFontStyleButtons);
        TRichEdit(Components[i]).OnSelectionChange :=
            CombineEvents(TRichEdit(Components[i]).OnSelectionChange, SetFontStyleButtons);
      end else if Components[i] is TDBRichEdit then begin
        TDBRichEdit(Components[i]).OnChange :=
            CombineEvents(TDBRichEdit(Components[i]).OnChange, SetFontStyleButtons);
        TDBRichEdit(Components[i]).OnSelectionChange :=
            CombineEvents(TDBRichEdit(Components[i]).OnSelectionChange, SetFontStyleButtons);
      end;
    end;
end;

{-------------------------------------------------------------------------------
  Message handler to invoke return data function.
}
procedure TBaseForm.WMDoReturnData(var Message: TMessage);
begin
  //Check link is valid
  if Assigned(RequestorForm)
     and Assigned(RequestorUpdate)
     and Assigned(RequestorForm.SupplierForm) then
  begin
    //Make requestor form visible
    PostMessage(RequestorForm.Handle, WM_TRANSFER_DONE, 0, 0);
    //Call requestor update function with key list
    RequestorUpdate(GetKeyList);
  end else
    ShowInformation(ResStr_ReturnDataLinkInvalid);

  //Remove link
  if Assigned(RequestorForm) then
    RequestorForm.SupplierForm:= nil;
  RequestorForm:= nil;
  RequestorUpdate:= nil;
end;

{-------------------------------------------------------------------------------
  Combines two events into a single event, so that they are both called together.
  The returned value is the new event to call which references both of them. Is
  able to deal with nil events (returns nil if both events are nil).
}
function TBaseForm.CombineEvents(firstEvent, secondEvent: TNotifyEvent): TNotifyEvent;
var
  dualEvent: TDualEvent;
begin
  if Assigned(firstEvent) and Assigned(secondEvent) then
  begin
    dualEvent := TDualEvent.Create(firstEvent, secondEvent);
    // Adds the new object to a list of such objects maintained by the form, so
    // that it can be Disposed when the form is closed.
    FDualEvents.Add(dualEvent);
    Result := dualEvent.Call;
  end
  else if Assigned(firstEvent) then
    Result := firstEvent
  else
    Result := secondEvent;
end;

{-------------------------------------------------------------------------------
  Creates an instance of a class for combining two seperate events into a single
  event. Takes the two events to combine as parameters.
}
constructor TDualEvent.Create(firstEvent, secondEvent: TNotifyEvent);
begin
  FFirstEvent := firstEvent;
  FSecondEvent := secondEvent;
end;

{-------------------------------------------------------------------------------
  Fires off the two events in this dual event.
}
procedure TDualEvent.Call(Sender: TObject);
begin
  FFirstEvent(Sender);
  FSecondEvent(Sender);
end;

end.
