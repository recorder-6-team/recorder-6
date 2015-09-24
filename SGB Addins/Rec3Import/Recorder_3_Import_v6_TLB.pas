unit Recorder_3_Import_v6_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 19/06/2008 10:32:07 from Type Library described below.

// ************************************************************************  //
// Type Lib: D:\Delphi stuff\Recorder 6 addins\Rec3Import\Recorder_3_Import_v6.tlb (1)
// LIBID: {392A3538-E0C9-4BB0-9F06-72774CDE9325}
// LCID: 0
// Helpfile: 
// HelpString: Recorder_3_Import_v2 Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\STDOLE2.TLB)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  Recorder_3_Import_v6MajorVersion = 1;
  Recorder_3_Import_v6MinorVersion = 0;

  LIBID_Recorder_3_Import_v6: TGUID = '{392A3538-E0C9-4BB0-9F06-72774CDE9325}';

  IID_IImportProgressX: TGUID = '{9430DDA4-DF52-44C9-B9C0-2D33B733B880}';
  DIID_IImportProgressXEvents: TGUID = '{3D62B626-70D5-4EFE-9C31-171B414F6748}';
  CLASS_ImportProgressX: TGUID = '{C3C57510-569E-4655-AC9B-E7B45A5BDC3C}';
  IID_IRec3Importer: TGUID = '{C0616779-FBB1-4A49-9DD0-070587870949}';
  CLASS_Rec3Importer: TGUID = '{3EDC2FF6-03E8-472F-9E61-87F663926325}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TxActiveFormBorderStyle
type
  TxActiveFormBorderStyle = TOleEnum;
const
  afbNone = $00000000;
  afbSingle = $00000001;
  afbSunken = $00000002;
  afbRaised = $00000003;

// Constants for enum TxPrintScale
type
  TxPrintScale = TOleEnum;
const
  poNone = $00000000;
  poProportional = $00000001;
  poPrintToFit = $00000002;

// Constants for enum TxMouseButton
type
  TxMouseButton = TOleEnum;
const
  mbLeft = $00000000;
  mbRight = $00000001;
  mbMiddle = $00000002;

// Constants for enum TxBiDiMode
type
  TxBiDiMode = TOleEnum;
const
  bdLeftToRight = $00000000;
  bdRightToLeft = $00000001;
  bdRightToLeftNoAlign = $00000002;
  bdRightToLeftReadingOnly = $00000003;

// Constants for enum TxTask
type
  TxTask = TOleEnum;
const
  taskInProgress = $00000000;
  taskDone = $00000001;
  taskCreate = $00000002;
  taskImport = $00000003;
  taskAllocate = $00000004;
  taskFix = $00000005;
  taskCopy = $00000006;
  taskZip = $00000007;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IImportProgressX = interface;
  IImportProgressXDisp = dispinterface;
  IImportProgressXEvents = dispinterface;
  IRec3Importer = interface;
  IRec3ImporterDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ImportProgressX = IImportProgressX;
  Rec3Importer = IRec3Importer;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPUserType1 = ^IFontDisp; {*}


// *********************************************************************//
// Interface: IImportProgressX
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9430DDA4-DF52-44C9-B9C0-2D33B733B880}
// *********************************************************************//
  IImportProgressX = interface(IDispatch)
    ['{9430DDA4-DF52-44C9-B9C0-2D33B733B880}']
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_AutoScroll: WordBool; safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    function Get_AutoSize: WordBool; safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_Color: OLE_COLOR; safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    function Get_Font: IFontDisp; safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    function Get_KeyPreview: WordBool; safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    function Get_PixelsPerInch: Integer; safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    function Get_Scaled: WordBool; safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    function Get_Active: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    function Get_HelpFile: WideString; safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_BiDiMode: TxBiDiMode; safecall;
    procedure Set_BiDiMode(Value: TxBiDiMode); safecall;
    function Get_Cursor: Smallint; safecall;
    procedure Set_Cursor(Value: Smallint); safecall;
    procedure ShowTitle(const aControl: WideString); safecall;
    procedure Set_Progress(Param1: Integer); safecall;
    procedure Set_ProgressMax(Param1: Integer); safecall;
    procedure Set_ProgressTables(Param1: Integer); safecall;
    procedure Set_ProgressTablesMax(Param1: Integer); safecall;
    procedure Set_CurrentTableName(const Param1: WideString); safecall;
    function ElapsedTime: TDateTime; safecall;
    procedure Set_NoErrors(Param1: Integer); safecall;
    procedure ShowTask(Task: TxTask; TaskStatus: TxTask); safecall;
    procedure ReportCompletion(NoErrors: Integer); safecall;
    procedure ZipFile(var aFile: OleVariant); safecall;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property AutoScroll: WordBool read Get_AutoScroll write Set_AutoScroll;
    property AutoSize: WordBool read Get_AutoSize write Set_AutoSize;
    property AxBorderStyle: TxActiveFormBorderStyle read Get_AxBorderStyle write Set_AxBorderStyle;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Color: OLE_COLOR read Get_Color write Set_Color;
    property Font: IFontDisp read Get_Font write Set_Font;
    property KeyPreview: WordBool read Get_KeyPreview write Set_KeyPreview;
    property PixelsPerInch: Integer read Get_PixelsPerInch write Set_PixelsPerInch;
    property PrintScale: TxPrintScale read Get_PrintScale write Set_PrintScale;
    property Scaled: WordBool read Get_Scaled write Set_Scaled;
    property Active: WordBool read Get_Active;
    property DropTarget: WordBool read Get_DropTarget write Set_DropTarget;
    property HelpFile: WideString read Get_HelpFile write Set_HelpFile;
    property DoubleBuffered: WordBool read Get_DoubleBuffered write Set_DoubleBuffered;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property BiDiMode: TxBiDiMode read Get_BiDiMode write Set_BiDiMode;
    property Cursor: Smallint read Get_Cursor write Set_Cursor;
    property Progress: Integer write Set_Progress;
    property ProgressMax: Integer write Set_ProgressMax;
    property ProgressTables: Integer write Set_ProgressTables;
    property ProgressTablesMax: Integer write Set_ProgressTablesMax;
    property CurrentTableName: WideString write Set_CurrentTableName;
    property NoErrors: Integer write Set_NoErrors;
  end;

// *********************************************************************//
// DispIntf:  IImportProgressXDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9430DDA4-DF52-44C9-B9C0-2D33B733B880}
// *********************************************************************//
  IImportProgressXDisp = dispinterface
    ['{9430DDA4-DF52-44C9-B9C0-2D33B733B880}']
    property Visible: WordBool dispid 1;
    property AutoScroll: WordBool dispid 2;
    property AutoSize: WordBool dispid 3;
    property AxBorderStyle: TxActiveFormBorderStyle dispid 4;
    property Caption: WideString dispid -518;
    property Color: OLE_COLOR dispid -501;
    property Font: IFontDisp dispid -512;
    property KeyPreview: WordBool dispid 5;
    property PixelsPerInch: Integer dispid 6;
    property PrintScale: TxPrintScale dispid 7;
    property Scaled: WordBool dispid 8;
    property Active: WordBool readonly dispid 9;
    property DropTarget: WordBool dispid 10;
    property HelpFile: WideString dispid 11;
    property DoubleBuffered: WordBool dispid 12;
    property Enabled: WordBool dispid -514;
    property BiDiMode: TxBiDiMode dispid 13;
    property Cursor: Smallint dispid 14;
    procedure ShowTitle(const aControl: WideString); dispid 15;
    property Progress: Integer writeonly dispid 17;
    property ProgressMax: Integer writeonly dispid 18;
    property ProgressTables: Integer writeonly dispid 19;
    property ProgressTablesMax: Integer writeonly dispid 20;
    property CurrentTableName: WideString writeonly dispid 21;
    function ElapsedTime: TDateTime; dispid 22;
    property NoErrors: Integer writeonly dispid 23;
    procedure ShowTask(Task: TxTask; TaskStatus: TxTask); dispid 24;
    procedure ReportCompletion(NoErrors: Integer); dispid 25;
    procedure ZipFile(var aFile: OleVariant); dispid 201;
  end;

// *********************************************************************//
// DispIntf:  IImportProgressXEvents
// Flags:     (4096) Dispatchable
// GUID:      {3D62B626-70D5-4EFE-9C31-171B414F6748}
// *********************************************************************//
  IImportProgressXEvents = dispinterface
    ['{3D62B626-70D5-4EFE-9C31-171B414F6748}']
    procedure OnActivate; dispid 1;
    procedure OnClick; dispid 2;
    procedure OnCreate; dispid 3;
    procedure OnDblClick; dispid 4;
    procedure OnDestroy; dispid 5;
    procedure OnDeactivate; dispid 6;
    procedure OnKeyPress(var Key: Smallint); dispid 10;
    procedure OnPaint; dispid 15;
  end;

// *********************************************************************//
// Interface: IRec3Importer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C0616779-FBB1-4A49-9DD0-070587870949}
// *********************************************************************//
  IRec3Importer = interface(IDispatch)
    ['{C0616779-FBB1-4A49-9DD0-070587870949}']
  end;

// *********************************************************************//
// DispIntf:  IRec3ImporterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C0616779-FBB1-4A49-9DD0-070587870949}
// *********************************************************************//
  IRec3ImporterDisp = dispinterface
    ['{C0616779-FBB1-4A49-9DD0-070587870949}']
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TImportProgressX
// Help String      : ImportProgressX Control
// Default Interface: IImportProgressX
// Def. Intf. DISP? : No
// Event   Interface: IImportProgressXEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TImportProgressXOnKeyPress = procedure(ASender: TObject; var Key: Smallint) of object;

  TImportProgressX = class(TOleControl)
  private
    FOnActivate: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnKeyPress: TImportProgressXOnKeyPress;
    FOnPaint: TNotifyEvent;
    FIntf: IImportProgressX;
    function  GetControlInterface: IImportProgressX;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    procedure ShowTitle(const aControl: WideString);
    function ElapsedTime: TDateTime;
    procedure ShowTask(Task: TxTask; TaskStatus: TxTask);
    procedure ReportCompletion(NoErrors: Integer);
    procedure ZipFile(var aFile: OleVariant);
    property  ControlInterface: IImportProgressX read GetControlInterface;
    property  DefaultInterface: IImportProgressX read GetControlInterface;
    property Visible: WordBool index 1 read GetWordBoolProp write SetWordBoolProp;
    property Active: WordBool index 9 read GetWordBoolProp;
    property DropTarget: WordBool index 10 read GetWordBoolProp write SetWordBoolProp;
    property HelpFile: WideString index 11 read GetWideStringProp write SetWideStringProp;
    property DoubleBuffered: WordBool index 12 read GetWordBoolProp write SetWordBoolProp;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp;
    property BiDiMode: TOleEnum index 13 read GetTOleEnumProp write SetTOleEnumProp;
    property Progress: Integer index 17 write SetIntegerProp;
    property ProgressMax: Integer index 18 write SetIntegerProp;
    property ProgressTables: Integer index 19 write SetIntegerProp;
    property ProgressTablesMax: Integer index 20 write SetIntegerProp;
    property CurrentTableName: WideString index 21 write SetWideStringProp;
    property NoErrors: Integer index 23 write SetIntegerProp;
  published
    property Anchors;
    property  ParentColor;
    property  ParentFont;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property AutoScroll: WordBool index 2 read GetWordBoolProp write SetWordBoolProp stored False;
    property AutoSize: WordBool index 3 read GetWordBoolProp write SetWordBoolProp stored False;
    property AxBorderStyle: TOleEnum index 4 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Caption: WideString index -518 read GetWideStringProp write SetWideStringProp stored False;
    property Color: TColor index -501 read GetTColorProp write SetTColorProp stored False;
    property Font: TFont index -512 read GetTFontProp write SetTFontProp stored False;
    property KeyPreview: WordBool index 5 read GetWordBoolProp write SetWordBoolProp stored False;
    property PixelsPerInch: Integer index 6 read GetIntegerProp write SetIntegerProp stored False;
    property PrintScale: TOleEnum index 7 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Scaled: WordBool index 8 read GetWordBoolProp write SetWordBoolProp stored False;
    property Cursor: Smallint index 14 read GetSmallintProp write SetSmallintProp stored False;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnKeyPress: TImportProgressXOnKeyPress read FOnKeyPress write FOnKeyPress;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

// *********************************************************************//
// The Class CoRec3Importer provides a Create and CreateRemote method to          
// create instances of the default interface IRec3Importer exposed by              
// the CoClass Rec3Importer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRec3Importer = class
    class function Create: IRec3Importer;
    class function CreateRemote(const MachineName: string): IRec3Importer;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TImportProgressX.InitControlData;
const
  CEventDispIDs: array [0..7] of DWORD = (
    $00000001, $00000002, $00000003, $00000004, $00000005, $00000006,
    $0000000A, $0000000F);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID: '{C3C57510-569E-4655-AC9B-E7B45A5BDC3C}';
    EventIID: '{3D62B626-70D5-4EFE-9C31-171B414F6748}';
    EventCount: 8;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$00000000*);
    Flags: $0000001D;
    Version: 401;
    FontCount: 1;
    FontIDs: @CTFontIDs);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnActivate) - Cardinal(Self);
end;

procedure TImportProgressX.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IImportProgressX;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TImportProgressX.GetControlInterface: IImportProgressX;
begin
  CreateControl;
  Result := FIntf;
end;

procedure TImportProgressX.ShowTitle(const aControl: WideString);
begin
  DefaultInterface.ShowTitle(aControl);
end;

function TImportProgressX.ElapsedTime: TDateTime;
begin
  Result := DefaultInterface.ElapsedTime;
end;

procedure TImportProgressX.ShowTask(Task: TxTask; TaskStatus: TxTask);
begin
  DefaultInterface.ShowTask(Task, TaskStatus);
end;

procedure TImportProgressX.ReportCompletion(NoErrors: Integer);
begin
  DefaultInterface.ReportCompletion(NoErrors);
end;

procedure TImportProgressX.ZipFile(var aFile: OleVariant);
begin
  DefaultInterface.ZipFile(aFile);
end;

class function CoRec3Importer.Create: IRec3Importer;
begin
  Result := CreateComObject(CLASS_Rec3Importer) as IRec3Importer;
end;

class function CoRec3Importer.CreateRemote(const MachineName: string): IRec3Importer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Rec3Importer) as IRec3Importer;
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TImportProgressX]);
end;

end.
