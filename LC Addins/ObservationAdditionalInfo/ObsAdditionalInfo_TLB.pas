unit ObsAdditionalInfo_TLB;

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
// File generated on 02/04/2018 14:59:34 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\localsource\recorder-6\LC Addins\ObservationAdditionalInfo\ObsAdditionalInfo.tlb (1)
// LIBID: {4CE59372-C101-4BEB-A6C8-2F4763C46711}
// LCID: 0
// Helpfile: 
// HelpString: ObsAdditionalInfo Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
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
  ObsAdditionalInfoMajorVersion = 1;
  ObsAdditionalInfoMinorVersion = 0;

  LIBID_ObsAdditionalInfo: TGUID = '{4CE59372-C101-4BEB-A6C8-2F4763C46711}';

  IID_IObsAddInfoButton: TGUID = '{35516551-F101-43E0-9904-28FDFC5D6334}';
  DIID_IObsAddInfoButtonEvents: TGUID = '{3765BB4F-890B-4118-A1AD-E27E4B0D4212}';
  CLASS_ObsAddInfoButton: TGUID = '{F247C066-4035-4692-8AA9-68728197B567}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TxDragMode
type
  TxDragMode = TOleEnum;
const
  dmManual = $00000000;
  dmAutomatic = $00000001;

// Constants for enum TxMouseButton
type
  TxMouseButton = TOleEnum;
const
  mbLeft = $00000000;
  mbRight = $00000001;
  mbMiddle = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IObsAddInfoButton = interface;
  IObsAddInfoButtonDisp = dispinterface;
  IObsAddInfoButtonEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ObsAddInfoButton = IObsAddInfoButton;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPUserType1 = ^IFontDisp; {*}


// *********************************************************************//
// Interface: IObsAddInfoButton
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {35516551-F101-43E0-9904-28FDFC5D6334}
// *********************************************************************//
  IObsAddInfoButton = interface(IDispatch)
    ['{35516551-F101-43E0-9904-28FDFC5D6334}']
    function Get_Cancel: WordBool; safecall;
    procedure Set_Cancel(Value: WordBool); safecall;
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    function Get_Default: WordBool; safecall;
    procedure Set_Default(Value: WordBool); safecall;
    function Get_DragCursor: Smallint; safecall;
    procedure Set_DragCursor(Value: Smallint); safecall;
    function Get_DragMode: TxDragMode; safecall;
    procedure Set_DragMode(Value: TxDragMode); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_Font: IFontDisp; safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_WordWrap: WordBool; safecall;
    procedure Set_WordWrap(Value: WordBool); safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; safecall;
    procedure InitiateAction; safecall;
    function IsRightToLeft: WordBool; safecall;
    function UseRightToLeftReading: WordBool; safecall;
    function UseRightToLeftScrollBar: WordBool; safecall;
    procedure SetSubComponent(IsSubComponent: WordBool); safecall;
    property Cancel: WordBool read Get_Cancel write Set_Cancel;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Default: WordBool read Get_Default write Set_Default;
    property DragCursor: Smallint read Get_DragCursor write Set_DragCursor;
    property DragMode: TxDragMode read Get_DragMode write Set_DragMode;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Font: IFontDisp read Get_Font write Set_Font;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property WordWrap: WordBool read Get_WordWrap write Set_WordWrap;
    property DoubleBuffered: WordBool read Get_DoubleBuffered write Set_DoubleBuffered;
    property AlignDisabled: WordBool read Get_AlignDisabled;
    property VisibleDockClientCount: Integer read Get_VisibleDockClientCount;
  end;

// *********************************************************************//
// DispIntf:  IObsAddInfoButtonDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {35516551-F101-43E0-9904-28FDFC5D6334}
// *********************************************************************//
  IObsAddInfoButtonDisp = dispinterface
    ['{35516551-F101-43E0-9904-28FDFC5D6334}']
    property Cancel: WordBool dispid 201;
    property Caption: WideString dispid -518;
    property Default: WordBool dispid 202;
    property DragCursor: Smallint dispid 203;
    property DragMode: TxDragMode dispid 204;
    property Enabled: WordBool dispid -514;
    property Font: IFontDisp dispid -512;
    property Visible: WordBool dispid 205;
    property WordWrap: WordBool dispid 206;
    property DoubleBuffered: WordBool dispid 207;
    property AlignDisabled: WordBool readonly dispid 208;
    property VisibleDockClientCount: Integer readonly dispid 209;
    function DrawTextBiDiModeFlagsReadingOnly: Integer; dispid 210;
    procedure InitiateAction; dispid 211;
    function IsRightToLeft: WordBool; dispid 212;
    function UseRightToLeftReading: WordBool; dispid 213;
    function UseRightToLeftScrollBar: WordBool; dispid 214;
    procedure SetSubComponent(IsSubComponent: WordBool); dispid 215;
  end;

// *********************************************************************//
// DispIntf:  IObsAddInfoButtonEvents
// Flags:     (4096) Dispatchable
// GUID:      {3765BB4F-890B-4118-A1AD-E27E4B0D4212}
// *********************************************************************//
  IObsAddInfoButtonEvents = dispinterface
    ['{3765BB4F-890B-4118-A1AD-E27E4B0D4212}']
    procedure OnClick; dispid 201;
    procedure OnKeyPress(var Key: Smallint); dispid 202;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TObsAddInfoButton
// Help String      : ObsAddInfoButton Control
// Default Interface: IObsAddInfoButton
// Def. Intf. DISP? : No
// Event   Interface: IObsAddInfoButtonEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TObsAddInfoButtonOnKeyPress = procedure(ASender: TObject; var Key: Smallint) of object;

  TObsAddInfoButton = class(TOleControl)
  private
    FOnClick: TNotifyEvent;
    FOnKeyPress: TObsAddInfoButtonOnKeyPress;
    FIntf: IObsAddInfoButton;
    function  GetControlInterface: IObsAddInfoButton;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    function DrawTextBiDiModeFlagsReadingOnly: Integer;
    procedure InitiateAction;
    function IsRightToLeft: WordBool;
    function UseRightToLeftReading: WordBool;
    function UseRightToLeftScrollBar: WordBool;
    procedure SetSubComponent(IsSubComponent: WordBool);
    property  ControlInterface: IObsAddInfoButton read GetControlInterface;
    property  DefaultInterface: IObsAddInfoButton read GetControlInterface;
    property DoubleBuffered: WordBool index 207 read GetWordBoolProp write SetWordBoolProp;
    property AlignDisabled: WordBool index 208 read GetWordBoolProp;
    property VisibleDockClientCount: Integer index 209 read GetIntegerProp;
  published
    property Anchors;
    property  ParentFont;
    property  TabStop;
    property  Align;
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
    property Cancel: WordBool index 201 read GetWordBoolProp write SetWordBoolProp stored False;
    property Caption: WideString index -518 read GetWideStringProp write SetWideStringProp stored False;
    property Default: WordBool index 202 read GetWordBoolProp write SetWordBoolProp stored False;
    property DragCursor: Smallint index 203 read GetSmallintProp write SetSmallintProp stored False;
    property DragMode: TOleEnum index 204 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property Enabled: WordBool index -514 read GetWordBoolProp write SetWordBoolProp stored False;
    property Font: TFont index -512 read GetTFontProp write SetTFontProp stored False;
    property Visible: WordBool index 205 read GetWordBoolProp write SetWordBoolProp stored False;
    property WordWrap: WordBool index 206 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnKeyPress: TObsAddInfoButtonOnKeyPress read FOnKeyPress write FOnKeyPress;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TObsAddInfoButton.InitControlData;
const
  CEventDispIDs: array [0..1] of DWORD = (
    $000000C9, $000000CA);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID: '{F247C066-4035-4692-8AA9-68728197B567}';
    EventIID: '{3765BB4F-890B-4118-A1AD-E27E4B0D4212}';
    EventCount: 2;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$8007007E*);
    Flags: $0000001C;
    Version: 401;
    FontCount: 1;
    FontIDs: @CTFontIDs);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnClick) - Cardinal(Self);
end;

procedure TObsAddInfoButton.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IObsAddInfoButton;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TObsAddInfoButton.GetControlInterface: IObsAddInfoButton;
begin
  CreateControl;
  Result := FIntf;
end;

function TObsAddInfoButton.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  Result := DefaultInterface.DrawTextBiDiModeFlagsReadingOnly;
end;

procedure TObsAddInfoButton.InitiateAction;
begin
  DefaultInterface.InitiateAction;
end;

function TObsAddInfoButton.IsRightToLeft: WordBool;
begin
  Result := DefaultInterface.IsRightToLeft;
end;

function TObsAddInfoButton.UseRightToLeftReading: WordBool;
begin
  Result := DefaultInterface.UseRightToLeftReading;
end;

function TObsAddInfoButton.UseRightToLeftScrollBar: WordBool;
begin
  Result := DefaultInterface.UseRightToLeftScrollBar;
end;

procedure TObsAddInfoButton.SetSubComponent(IsSubComponent: WordBool);
begin
  DefaultInterface.SetSubComponent(IsSubComponent);
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TObsAddInfoButton]);
end;

end.
