unit ProjectTaxonMetaButton_TLB;

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
// File generated on 29/09/2015 14:47:45 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\localsource\recorder-6\LC Addins\TaxonMetaButton\ProjectTaxonMetaButton.tlb (1)
// LIBID: {8ADE301C-EB60-4FD5-877C-3250D99C2C15}
// LCID: 0
// Helpfile: 
// HelpString: ProjectTaxonMetaButton Library
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
  ProjectTaxonMetaButtonMajorVersion = 1;
  ProjectTaxonMetaButtonMinorVersion = 0;

  LIBID_ProjectTaxonMetaButton: TGUID = '{8ADE301C-EB60-4FD5-877C-3250D99C2C15}';

  IID_ITaxonButton: TGUID = '{7490EAC0-9C8C-48E2-8EB7-F597D2D2CFF3}';
  DIID_ITaxonButtonEvents: TGUID = '{64DAF6ED-900A-4407-A721-B5303C015DEE}';
  CLASS_TaxonButton: TGUID = '{20818C0D-5FBF-4D0F-BB34-054049CDAF42}';

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
  ITaxonButton = interface;
  ITaxonButtonDisp = dispinterface;
  ITaxonButtonEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  TaxonButton = ITaxonButton;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PPUserType1 = ^IFontDisp; {*}


// *********************************************************************//
// Interface: ITaxonButton
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7490EAC0-9C8C-48E2-8EB7-F597D2D2CFF3}
// *********************************************************************//
  ITaxonButton = interface(IDispatch)
    ['{7490EAC0-9C8C-48E2-8EB7-F597D2D2CFF3}']
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
// DispIntf:  ITaxonButtonDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7490EAC0-9C8C-48E2-8EB7-F597D2D2CFF3}
// *********************************************************************//
  ITaxonButtonDisp = dispinterface
    ['{7490EAC0-9C8C-48E2-8EB7-F597D2D2CFF3}']
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
// DispIntf:  ITaxonButtonEvents
// Flags:     (4096) Dispatchable
// GUID:      {64DAF6ED-900A-4407-A721-B5303C015DEE}
// *********************************************************************//
  ITaxonButtonEvents = dispinterface
    ['{64DAF6ED-900A-4407-A721-B5303C015DEE}']
    procedure OnClick; dispid 201;
    procedure OnKeyPress(var Key: Smallint); dispid 202;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TTaxonButton
// Help String      : TaxonButton Control
// Default Interface: ITaxonButton
// Def. Intf. DISP? : No
// Event   Interface: ITaxonButtonEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TTaxonButtonOnKeyPress = procedure(ASender: TObject; var Key: Smallint) of object;

  TTaxonButton = class(TOleControl)
  private
    FOnClick: TNotifyEvent;
    FOnKeyPress: TTaxonButtonOnKeyPress;
    FIntf: ITaxonButton;
    function  GetControlInterface: ITaxonButton;
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
    property  ControlInterface: ITaxonButton read GetControlInterface;
    property  DefaultInterface: ITaxonButton read GetControlInterface;
    property DoubleBuffered: WordBool index 207 read GetWordBoolProp write SetWordBoolProp;
    property AlignDisabled: WordBool index 208 read GetWordBoolProp;
    property VisibleDockClientCount: Integer index 209 read GetIntegerProp;
  published
    property Anchors;
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
    property OnKeyPress: TTaxonButtonOnKeyPress read FOnKeyPress write FOnKeyPress;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TTaxonButton.InitControlData;
const
  CEventDispIDs: array [0..1] of DWORD = (
    $000000C9, $000000CA);
  CTFontIDs: array [0..0] of DWORD = (
    $FFFFFE00);
  CControlData: TControlData2 = (
    ClassID: '{20818C0D-5FBF-4D0F-BB34-054049CDAF42}';
    EventIID: '{64DAF6ED-900A-4407-A721-B5303C015DEE}';
    EventCount: 2;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80040154*);
    Flags: $0000001C;
    Version: 401;
    FontCount: 1;
    FontIDs: @CTFontIDs);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnClick) - Cardinal(Self);
end;

procedure TTaxonButton.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as ITaxonButton;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TTaxonButton.GetControlInterface: ITaxonButton;
begin
  CreateControl;
  Result := FIntf;
end;

function TTaxonButton.DrawTextBiDiModeFlagsReadingOnly: Integer;
begin
  Result := DefaultInterface.DrawTextBiDiModeFlagsReadingOnly;
end;

procedure TTaxonButton.InitiateAction;
begin
  DefaultInterface.InitiateAction;
end;

function TTaxonButton.IsRightToLeft: WordBool;
begin
  Result := DefaultInterface.IsRightToLeft;
end;

function TTaxonButton.UseRightToLeftReading: WordBool;
begin
  Result := DefaultInterface.UseRightToLeftReading;
end;

function TTaxonButton.UseRightToLeftScrollBar: WordBool;
begin
  Result := DefaultInterface.UseRightToLeftScrollBar;
end;

procedure TTaxonButton.SetSubComponent(IsSubComponent: WordBool);
begin
  DefaultInterface.SetSubComponent(IsSubComponent);
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TTaxonButton]);
end;

end.
