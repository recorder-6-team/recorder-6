unit Recorder2000_TLB;

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

// $Rev: 52393 $
// File generated on 11/12/2012 12:28:10 from Type Library described below.

// ************************************************************************  //
// Type Lib: d:\Recorder 6\RecorderApp.exe (1)
// LIBID: {801EBE82-91CE-11D3-B564-005004B0B698}
// LCID: 0
// Helpfile: 
// HelpString: Recorder2000 Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
//   (2) v2.1 ADODB, (C:\Program Files (x86)\Common Files\System\ado\msado21.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Hint: Symbol 'System' renamed to 'System_'
// Cmdline:
//   tlibimp  -P -DC:\temp "d:\Recorder 6\RecorderApp.exe"
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, ADODB_TLB, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  Recorder2000MajorVersion = 1;
  Recorder2000MinorVersion = 0;

  LIBID_Recorder2000: TGUID = '{801EBE82-91CE-11D3-B564-005004B0B698}';

  IID_IAutoApplicationSettings: TGUID = '{801EBE83-91CE-11D3-B564-005004B0B698}';
  CLASS_AutoApplicationSettings: TGUID = '{801EBE85-91CE-11D3-B564-005004B0B698}';
  IID_IReplacementForm: TGUID = '{801EBE8B-91CE-11D3-B564-005004B0B698}';
  IID_IAdditionalPage: TGUID = '{801EBE8D-91CE-11D3-B564-005004B0B698}';
  IID_INewAction: TGUID = '{801EBE8F-91CE-11D3-B564-005004B0B698}';
  IID_IImportFilter: TGUID = '{801EBE91-91CE-11D3-B564-005004B0B698}';
  IID_IExportByColumn: TGUID = '{801EBE95-91CE-11D3-B564-005004B0B698}';
  IID_ISpatialReference: TGUID = '{801EBE97-91CE-11D3-B564-005004B0B698}';
  IID_IRecordCardHeader: TGUID = '{801EBE99-91CE-11D3-B564-005004B0B698}';
  IID_IKeyItem: TGUID = '{801EBE9D-91CE-11D3-B564-005004B0B698}';
  IID_IKeyList: TGUID = '{801EBE9F-91CE-11D3-B564-005004B0B698}';
  IID_IRucksack: TGUID = '{801EBEA1-91CE-11D3-B564-005004B0B698}';
  IID_ICurrentSettings: TGUID = '{801EBEA3-91CE-11D3-B564-005004B0B698}';
  IID_IRecorder2000: TGUID = '{801EBEA5-91CE-11D3-B564-005004B0B698}';
  IID_IRecorderAddin: TGUID = '{801EBEA7-91CE-11D3-B564-005004B0B698}';
  IID_IFilter: TGUID = '{801EBEA9-91CE-11D3-B564-005004B0B698}';
  IID_IDialog: TGUID = '{801EBEAB-91CE-11D3-B564-005004B0B698}';
  IID_IFormCaption: TGUID = '{801EBEAD-91CE-11D3-B564-005004B0B698}';
  IID_IExecuteAction: TGUID = '{801EBEAF-91CE-11D3-B564-005004B0B698}';
  IID_IHelp: TGUID = '{801EBEB1-91CE-11D3-B564-005004B0B698}';
  IID_IRecorderControl: TGUID = '{801EBEB5-91CE-11D3-B564-005004B0B698}';
  IID_IEditControl: TGUID = '{801EBEB7-91CE-11D3-B564-005004B0B698}';
  IID_IListControl: TGUID = '{801EBEB9-91CE-11D3-B564-005004B0B698}';
  IID_IGridControl: TGUID = '{801EBEBB-91CE-11D3-B564-005004B0B698}';
  IID_ILabelControl: TGUID = '{801EBEBD-91CE-11D3-B564-005004B0B698}';
  IID_ITabPageControl: TGUID = '{801EBEBF-91CE-11D3-B564-005004B0B698}';
  IID_IRecorderFormEvents: TGUID = '{801EBEC1-91CE-11D3-B564-005004B0B698}';
  IID_IComboControl: TGUID = '{801EBEC3-91CE-11D3-B564-005004B0B698}';
  IID_IAdditionalFilter: TGUID = '{801EBEC5-91CE-11D3-B564-005004B0B698}';
  IID_IKeyListSupplier: TGUID = '{801EBEC7-91CE-11D3-B564-005004B0B698}';
  IID_IValidation: TGUID = '{801EBEC9-91CE-11D3-B564-005004B0B698}';
  IID_IExportFilter: TGUID = '{801EBECB-91CE-11D3-B564-005004B0B698}';
  IID_IAdditionalFilterDialog: TGUID = '{801EBECD-91CE-11D3-B564-005004B0B698}';
  IID_IRecorderForm: TGUID = '{801EBECF-91CE-11D3-B564-005004B0B698}';
  IID_IRequestor: TGUID = '{21A6FBA1-A890-11D3-B7C2-0060085B710F}';
  IID_IEventHandler: TGUID = '{36904D41-E5F3-11D3-B54F-0050DA5FA07B}';
  IID_IRecorderFunctions: TGUID = '{FE68E8F1-321E-11D4-BD47-0050DA5FA07B}';
  IID_IBaseMapFormat: TGUID = '{77337221-FE76-11D4-B5B8-00105A9841ED}';
  IID_ILatLong: TGUID = '{69752F50-0276-11D5-B5BC-00105A9841ED}';
  IID_IVagueDate: TGUID = '{4234BD81-0322-11D5-B5BD-00105A9841ED}';
  IID_IFormWithToolbar: TGUID = '{7BA0831C-7C87-484D-A9B0-A8C987F4C062}';
  IID_IRecorderMainForm: TGUID = '{359E6BC6-5BBB-4D07-9254-5E24ED634C12}';
  IID_IRecorderMap: TGUID = '{C0E1175F-B5AF-48EC-8E94-EBFF087410D5}';
  IID_IReportResults: TGUID = '{8787F00E-18E4-4B33-8882-CC04E44E9FB1}';
  IID_IProvidesOccurrencesSQL: TGUID = '{EED143EC-45A5-4338-A329-860B23F3A7C6}';
  IID_ICurrentSettings6: TGUID = '{C03D3B3B-B543-4B97-BF7D-5EF0677CB86A}';
  IID_IMergeMenuManager: TGUID = '{5DF1B2E0-9A24-45A1-A56C-EAA05B8E20A6}';
  IID_INodeManager: TGUID = '{8BF99A9C-125B-468C-88E3-C8867465D922}';
  IID_IOptionsPage: TGUID = '{B02EEC11-F45E-4B3E-90D9-4EDBFC63404E}';
  IID_IOptionsPages: TGUID = '{629DE5B3-A866-4D32-8BE8-78937C864774}';
  IID_IMapPoint: TGUID = '{B9870681-B2F1-4664-98FD-0EEC3DB510F9}';
  IID_IMapDropFormat: TGUID = '{F51213F5-8845-468F-BEB0-7CFD874DCD95}';
  IID_IOccurrenceNodeManager: TGUID = '{CA30D9F0-DAD9-40CB-9439-2B861A28470D}';
  IID_ILocationNodeManager: TGUID = '{453CEF6E-19F1-4CC1-8564-40EF48A4FD21}';
  IID_IDynamicMenu: TGUID = '{6B5606DA-3664-4065-9B66-EED00EBB53FF}';
  IID_IDynamicMenuList: TGUID = '{A8B79E68-7426-4B99-BBF3-AC5DAC8E7F01}';
  IID_IRecorderDetailScreen: TGUID = '{A40C35D0-0E71-4312-B2DB-E02B42C0E49C}';
  IID_IRecorderDetailScreenEvents: TGUID = '{05129295-B0FD-416C-BBF4-94A5BDBA317E}';
  IID_IDisplayData: TGUID = '{16B1083A-DCDE-46C4-9BEA-4B03ADEBCEA7}';
  IID_IMapWindowSelector: TGUID = '{CC73F7AB-2171-4AA9-AFEC-152665273090}';
  IID_IAvailableMap: TGUID = '{8889F4D5-739C-4E97-B74B-2C7220DD0AE9}';
  CLASS_AvailableMap: TGUID = '{3BE5EBCC-EF8A-4DD2-97B4-186EEED886B2}';
  IID_IRecorder6: TGUID = '{6443B816-0A35-428E-9A14-C1062B12B526}';
  IID_ISpatialReference6: TGUID = '{5EB0BFBD-A61F-4F11-8FBD-64A2643FB1D4}';
  IID_IGridSystem: TGUID = '{28871FA6-4CEB-44A3-8901-DA602F13382C}';
  IID_IENBox: TGUID = '{766E6BBB-25A8-4532-AE4C-F188357159E5}';
  IID_IAdditionalPage6: TGUID = '{2865ED1F-02BD-4CAA-BFCC-78C79726BF08}';
  IID_ISpatialReferenceList: TGUID = '{326DE097-273F-44E4-AB00-4355978B8660}';
  IID_IBaseMapFormatList: TGUID = '{E451A16A-6108-4DF4-AEE5-785DFF49F7D4}';
  IID_INamedSpatialReference: TGUID = '{71ED641E-5401-4896-8BE8-E18F804332D7}';
  IID_IBaseMapScaleList: TGUID = '{85B8381E-810A-42D0-B9D9-668F0612AF70}';
  IID_IReportKeytype: TGUID = '{F5757D4C-994D-42A6-A1A0-19EB849828CF}';
  IID_IReportKeytypeList: TGUID = '{4DB607C0-F61D-474C-80BE-A705C6ADA1D9}';
  IID_IXmlReport: TGUID = '{83F33C13-CF9F-448E-A3D1-A4EC914A85F3}';
  IID_IXmlReportList: TGUID = '{7843E83E-A9F1-4596-8DF8-BC34F15C5DEF}';
  IID_IValidation6: TGUID = '{6496C721-70E5-41F8-957A-6F95BC552EA0}';
  IID_IExportFilter6: TGUID = '{DDB265FD-0D72-4527-A3D6-BE7278002A6D}';
  IID_IBaseMapScaleList_614: TGUID = '{70FA545C-9DE4-494E-AB85-E04C5FCEB7D8}';
  IID_INodeMenuManager: TGUID = '{6ED9E090-E055-4F58-87F2-412C6CD50379}';
  IID_ISpatialRefControl: TGUID = '{57D0741A-38CE-4B16-9274-3B3AC44AB205}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IAutoApplicationSettings = interface;
  IAutoApplicationSettingsDisp = dispinterface;
  IReplacementForm = interface;
  IReplacementFormDisp = dispinterface;
  IAdditionalPage = interface;
  IAdditionalPageDisp = dispinterface;
  INewAction = interface;
  INewActionDisp = dispinterface;
  IImportFilter = interface;
  IImportFilterDisp = dispinterface;
  IExportByColumn = interface;
  IExportByColumnDisp = dispinterface;
  ISpatialReference = interface;
  ISpatialReferenceDisp = dispinterface;
  IRecordCardHeader = interface;
  IRecordCardHeaderDisp = dispinterface;
  IKeyItem = interface;
  IKeyItemDisp = dispinterface;
  IKeyList = interface;
  IKeyListDisp = dispinterface;
  IRucksack = interface;
  IRucksackDisp = dispinterface;
  ICurrentSettings = interface;
  ICurrentSettingsDisp = dispinterface;
  IRecorder2000 = interface;
  IRecorder2000Disp = dispinterface;
  IRecorderAddin = interface;
  IRecorderAddinDisp = dispinterface;
  IFilter = interface;
  IFilterDisp = dispinterface;
  IDialog = interface;
  IDialogDisp = dispinterface;
  IFormCaption = interface;
  IFormCaptionDisp = dispinterface;
  IExecuteAction = interface;
  IExecuteActionDisp = dispinterface;
  IHelp = interface;
  IHelpDisp = dispinterface;
  IRecorderControl = interface;
  IRecorderControlDisp = dispinterface;
  IEditControl = interface;
  IEditControlDisp = dispinterface;
  IListControl = interface;
  IListControlDisp = dispinterface;
  IGridControl = interface;
  IGridControlDisp = dispinterface;
  ILabelControl = interface;
  ILabelControlDisp = dispinterface;
  ITabPageControl = interface;
  ITabPageControlDisp = dispinterface;
  IRecorderFormEvents = interface;
  IRecorderFormEventsDisp = dispinterface;
  IComboControl = interface;
  IComboControlDisp = dispinterface;
  IAdditionalFilter = interface;
  IAdditionalFilterDisp = dispinterface;
  IKeyListSupplier = interface;
  IKeyListSupplierDisp = dispinterface;
  IValidation = interface;
  IValidationDisp = dispinterface;
  IExportFilter = interface;
  IExportFilterDisp = dispinterface;
  IAdditionalFilterDialog = interface;
  IAdditionalFilterDialogDisp = dispinterface;
  IRecorderForm = interface;
  IRecorderFormDisp = dispinterface;
  IRequestor = interface;
  IRequestorDisp = dispinterface;
  IEventHandler = interface;
  IEventHandlerDisp = dispinterface;
  IRecorderFunctions = interface;
  IRecorderFunctionsDisp = dispinterface;
  IBaseMapFormat = interface;
  IBaseMapFormatDisp = dispinterface;
  ILatLong = interface;
  ILatLongDisp = dispinterface;
  IVagueDate = interface;
  IVagueDateDisp = dispinterface;
  IFormWithToolbar = interface;
  IFormWithToolbarDisp = dispinterface;
  IRecorderMainForm = interface;
  IRecorderMainFormDisp = dispinterface;
  IRecorderMap = interface;
  IRecorderMapDisp = dispinterface;
  IReportResults = interface;
  IReportResultsDisp = dispinterface;
  IProvidesOccurrencesSQL = interface;
  IProvidesOccurrencesSQLDisp = dispinterface;
  ICurrentSettings6 = interface;
  ICurrentSettings6Disp = dispinterface;
  IMergeMenuManager = interface;
  IMergeMenuManagerDisp = dispinterface;
  INodeManager = interface;
  INodeManagerDisp = dispinterface;
  IOptionsPage = interface;
  IOptionsPageDisp = dispinterface;
  IOptionsPages = interface;
  IOptionsPagesDisp = dispinterface;
  IMapPoint = interface;
  IMapPointDisp = dispinterface;
  IMapDropFormat = interface;
  IMapDropFormatDisp = dispinterface;
  IOccurrenceNodeManager = interface;
  IOccurrenceNodeManagerDisp = dispinterface;
  ILocationNodeManager = interface;
  ILocationNodeManagerDisp = dispinterface;
  IDynamicMenu = interface;
  IDynamicMenuDisp = dispinterface;
  IDynamicMenuList = interface;
  IDynamicMenuListDisp = dispinterface;
  IRecorderDetailScreen = interface;
  IRecorderDetailScreenDisp = dispinterface;
  IRecorderDetailScreenEvents = interface;
  IRecorderDetailScreenEventsDisp = dispinterface;
  IDisplayData = interface;
  IDisplayDataDisp = dispinterface;
  IMapWindowSelector = interface;
  IMapWindowSelectorDisp = dispinterface;
  IAvailableMap = interface;
  IAvailableMapDisp = dispinterface;
  IRecorder6 = interface;
  IRecorder6Disp = dispinterface;
  ISpatialReference6 = interface;
  ISpatialReference6Disp = dispinterface;
  IGridSystem = interface;
  IGridSystemDisp = dispinterface;
  IENBox = interface;
  IENBoxDisp = dispinterface;
  IAdditionalPage6 = interface;
  IAdditionalPage6Disp = dispinterface;
  ISpatialReferenceList = interface;
  ISpatialReferenceListDisp = dispinterface;
  IBaseMapFormatList = interface;
  IBaseMapFormatListDisp = dispinterface;
  INamedSpatialReference = interface;
  INamedSpatialReferenceDisp = dispinterface;
  IBaseMapScaleList = interface;
  IBaseMapScaleListDisp = dispinterface;
  IReportKeytype = interface;
  IReportKeytypeDisp = dispinterface;
  IReportKeytypeList = interface;
  IReportKeytypeListDisp = dispinterface;
  IXmlReport = interface;
  IXmlReportDisp = dispinterface;
  IXmlReportList = interface;
  IXmlReportListDisp = dispinterface;
  IValidation6 = interface;
  IValidation6Disp = dispinterface;
  IExportFilter6 = interface;
  IExportFilter6Disp = dispinterface;
  IBaseMapScaleList_614 = interface;
  IBaseMapScaleList_614Disp = dispinterface;
  INodeMenuManager = interface;
  INodeMenuManagerDisp = dispinterface;
  ISpatialRefControl = interface;
  ISpatialRefControlDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  AutoApplicationSettings = IAutoApplicationSettings;
  AvailableMap = IAvailableMap;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  TActionData = record
    Caption: WideString;
    Enabled: WordBool;
    Hint: WideString;
    ShortCut: Word;
    Visible: WordBool;
    ImageIndex: Integer;
  end;

  TImageProperties = record
    ImageIndex: Integer;
    FileName: WideString;
    TransparentColor: LongWord;
    TransparentModeAuto: WordBool;
  end;

  TNodeInfo = record
    Caption: WideString;
    ChildrenPopulated: WordBool;
    Deletable: WordBool;
    Editable: WordBool;
    ImageIndex: Integer;
    ItemKey: WideString;
    SelectedIndex: Integer;
    StateIndex: Integer;
    TypeID: Integer;
    TableName: WideString;
  end;


// *********************************************************************//
// Interface: IAutoApplicationSettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE83-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IAutoApplicationSettings = interface(IDispatch)
    ['{801EBE83-91CE-11D3-B564-005004B0B698}']
  end;

// *********************************************************************//
// DispIntf:  IAutoApplicationSettingsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE83-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IAutoApplicationSettingsDisp = dispinterface
    ['{801EBE83-91CE-11D3-B564-005004B0B698}']
  end;

// *********************************************************************//
// Interface: IReplacementForm
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE8B-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IReplacementForm = interface(IDispatch)
    ['{801EBE8B-91CE-11D3-B564-005004B0B698}']
    function Get_FormToReplace: WideString; safecall;
    property FormToReplace: WideString read Get_FormToReplace;
  end;

// *********************************************************************//
// DispIntf:  IReplacementFormDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE8B-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IReplacementFormDisp = dispinterface
    ['{801EBE8B-91CE-11D3-B564-005004B0B698}']
    property FormToReplace: WideString readonly dispid 1;
  end;

// *********************************************************************//
// Interface: IAdditionalPage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE8D-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IAdditionalPage = interface(IDispatch)
    ['{801EBE8D-91CE-11D3-B564-005004B0B698}']
    function Get_Form: WideString; safecall;
    function Get_PageCaption: WideString; safecall;
    function Get_CurrentTable: WideString; safecall;
    procedure Set_CurrentTable(const Value: WideString); safecall;
    function Get_CurrentKey: WideString; safecall;
    procedure Set_CurrentKey(const Value: WideString); safecall;
    property Form: WideString read Get_Form;
    property PageCaption: WideString read Get_PageCaption;
    property CurrentTable: WideString read Get_CurrentTable write Set_CurrentTable;
    property CurrentKey: WideString read Get_CurrentKey write Set_CurrentKey;
  end;

// *********************************************************************//
// DispIntf:  IAdditionalPageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE8D-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IAdditionalPageDisp = dispinterface
    ['{801EBE8D-91CE-11D3-B564-005004B0B698}']
    property Form: WideString readonly dispid 1;
    property PageCaption: WideString readonly dispid 2;
    property CurrentTable: WideString dispid 5;
    property CurrentKey: WideString dispid 6;
  end;

// *********************************************************************//
// Interface: INewAction
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE8F-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  INewAction = interface(IDispatch)
    ['{801EBE8F-91CE-11D3-B564-005004B0B698}']
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
    property ActionCaption: WideString read Get_ActionCaption;
    property Hint: WideString read Get_Hint;
    property DimmedImageFilename: WideString read Get_DimmedImageFilename;
    property DisabledImageFileName: WideString read Get_DisabledImageFileName;
    property ParentMenu: WideString read Get_ParentMenu;
    property CanAddToToolbar: WordBool read Get_CanAddToToolbar;
  end;

// *********************************************************************//
// DispIntf:  INewActionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE8F-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  INewActionDisp = dispinterface
    ['{801EBE8F-91CE-11D3-B564-005004B0B698}']
    property ActionCaption: WideString readonly dispid 2;
    property Hint: WideString readonly dispid 3;
    property DimmedImageFilename: WideString readonly dispid 4;
    property DisabledImageFileName: WideString readonly dispid 5;
    property ParentMenu: WideString readonly dispid 6;
    property CanAddToToolbar: WordBool readonly dispid 7;
  end;

// *********************************************************************//
// Interface: IImportFilter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE91-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IImportFilter = interface(IDispatch)
    ['{801EBE91-91CE-11D3-B564-005004B0B698}']
    function ImportFile(const iSourceFile: WideString): WordBool; safecall;
    function Get_LastImportError: WideString; safecall;
    property LastImportError: WideString read Get_LastImportError;
  end;

// *********************************************************************//
// DispIntf:  IImportFilterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE91-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IImportFilterDisp = dispinterface
    ['{801EBE91-91CE-11D3-B564-005004B0B698}']
    function ImportFile(const iSourceFile: WideString): WordBool; dispid 4;
    property LastImportError: WideString readonly dispid 6;
  end;

// *********************************************************************//
// Interface: IExportByColumn
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE95-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IExportByColumn = interface(IDispatch)
    ['{801EBE95-91CE-11D3-B564-005004B0B698}']
    function Get_MeasurementUnitKey: WideString; safecall;
    procedure Set_MeasurementUnitKey(const Value: WideString); safecall;
    function Get_NumberOfRanges: Integer; safecall;
    procedure Set_NumberOfRanges(Value: Integer); safecall;
    function Get_MinValue: Double; safecall;
    procedure Set_MinValue(Value: Double); safecall;
    function Get_MaxValue: Double; safecall;
    procedure Set_MaxValue(Value: Double); safecall;
    property MeasurementUnitKey: WideString read Get_MeasurementUnitKey write Set_MeasurementUnitKey;
    property NumberOfRanges: Integer read Get_NumberOfRanges write Set_NumberOfRanges;
    property MinValue: Double read Get_MinValue write Set_MinValue;
    property MaxValue: Double read Get_MaxValue write Set_MaxValue;
  end;

// *********************************************************************//
// DispIntf:  IExportByColumnDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE95-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IExportByColumnDisp = dispinterface
    ['{801EBE95-91CE-11D3-B564-005004B0B698}']
    property MeasurementUnitKey: WideString dispid 1;
    property NumberOfRanges: Integer dispid 2;
    property MinValue: Double dispid 3;
    property MaxValue: Double dispid 4;
  end;

// *********************************************************************//
// Interface: ISpatialReference
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE97-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  ISpatialReference = interface(IDispatch)
    ['{801EBE97-91CE-11D3-B564-005004B0B698}']
    function Get_SpatialRefSystem: WideString; safecall;
    function ConvertToLat(const iSpatialRef: WideString): WideString; safecall;
    function ConvertToLong(const iSpatialRef: WideString): WideString; safecall;
    function ConvertFromLatLong(const iLat: WideString; const iLong: WideString): WideString; safecall;
    function Get_GetLastError: WideString; safecall;
    function ValidSpatialRef(const iSpatialRef: WideString): WordBool; safecall;
    property SpatialRefSystem: WideString read Get_SpatialRefSystem;
    property GetLastError: WideString read Get_GetLastError;
  end;

// *********************************************************************//
// DispIntf:  ISpatialReferenceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE97-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  ISpatialReferenceDisp = dispinterface
    ['{801EBE97-91CE-11D3-B564-005004B0B698}']
    property SpatialRefSystem: WideString readonly dispid 1;
    function ConvertToLat(const iSpatialRef: WideString): WideString; dispid 2;
    function ConvertToLong(const iSpatialRef: WideString): WideString; dispid 3;
    function ConvertFromLatLong(const iLat: WideString; const iLong: WideString): WideString; dispid 4;
    property GetLastError: WideString readonly dispid 5;
    function ValidSpatialRef(const iSpatialRef: WideString): WordBool; dispid 7;
  end;

// *********************************************************************//
// Interface: IRecordCardHeader
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE99-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecordCardHeader = interface(IDispatch)
    ['{801EBE99-91CE-11D3-B564-005004B0B698}']
    function SaveHeaderInfo: WideString; safecall;
    function Get_GetLastError: WideString; safecall;
    property GetLastError: WideString read Get_GetLastError;
  end;

// *********************************************************************//
// DispIntf:  IRecordCardHeaderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE99-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecordCardHeaderDisp = dispinterface
    ['{801EBE99-91CE-11D3-B564-005004B0B698}']
    function SaveHeaderInfo: WideString; dispid 1;
    property GetLastError: WideString readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IKeyItem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE9D-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IKeyItem = interface(IDispatch)
    ['{801EBE9D-91CE-11D3-B564-005004B0B698}']
    function Get_KeyField1: WideString; safecall;
    function Get_KeyField2: WideString; safecall;
    property KeyField1: WideString read Get_KeyField1;
    property KeyField2: WideString read Get_KeyField2;
  end;

// *********************************************************************//
// DispIntf:  IKeyItemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE9D-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IKeyItemDisp = dispinterface
    ['{801EBE9D-91CE-11D3-B564-005004B0B698}']
    property KeyField1: WideString readonly dispid 1;
    property KeyField2: WideString readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IKeyList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE9F-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IKeyList = interface(IDispatch)
    ['{801EBE9F-91CE-11D3-B564-005004B0B698}']
    function Get_ItemCount: Integer; safecall;
    function Get_TableName: WideString; safecall;
    function GetKeyItem(iIndex: Integer): IKeyItem; safecall;
    property ItemCount: Integer read Get_ItemCount;
    property TableName: WideString read Get_TableName;
  end;

// *********************************************************************//
// DispIntf:  IKeyListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBE9F-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IKeyListDisp = dispinterface
    ['{801EBE9F-91CE-11D3-B564-005004B0B698}']
    property ItemCount: Integer readonly dispid 1;
    property TableName: WideString readonly dispid 2;
    function GetKeyItem(iIndex: Integer): IKeyItem; dispid 3;
  end;

// *********************************************************************//
// Interface: IRucksack
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEA1-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRucksack = interface(IDispatch)
    ['{801EBEA1-91CE-11D3-B564-005004B0B698}']
    function Get_TaxonList: IKeyList; safecall;
    function Get_BiotopeList: IKeyList; safecall;
    function Get_LocationList: IKeyList; safecall;
    function Get_NameList: IKeyList; safecall;
    function Get_DocumentList: IKeyList; safecall;
    function Get_RucksackFile: WideString; safecall;
    property TaxonList: IKeyList read Get_TaxonList;
    property BiotopeList: IKeyList read Get_BiotopeList;
    property LocationList: IKeyList read Get_LocationList;
    property NameList: IKeyList read Get_NameList;
    property DocumentList: IKeyList read Get_DocumentList;
    property RucksackFile: WideString read Get_RucksackFile;
  end;

// *********************************************************************//
// DispIntf:  IRucksackDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEA1-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRucksackDisp = dispinterface
    ['{801EBEA1-91CE-11D3-B564-005004B0B698}']
    property TaxonList: IKeyList readonly dispid 1;
    property BiotopeList: IKeyList readonly dispid 2;
    property LocationList: IKeyList readonly dispid 3;
    property NameList: IKeyList readonly dispid 4;
    property DocumentList: IKeyList readonly dispid 5;
    property RucksackFile: WideString readonly dispid 7;
  end;

// *********************************************************************//
// Interface: ICurrentSettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEA3-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  ICurrentSettings = interface(IDispatch)
    ['{801EBEA3-91CE-11D3-B564-005004B0B698}']
    function Get_TaxonListKey: WideString; safecall;
    function Get_BiotopeListKey: WideString; safecall;
    function Get_AdminAreaListKey: WideString; safecall;
    function Get_ResultList: IKeyList; safecall;
    function Get_UserIDKey: WideString; safecall;
    property TaxonListKey: WideString read Get_TaxonListKey;
    property BiotopeListKey: WideString read Get_BiotopeListKey;
    property AdminAreaListKey: WideString read Get_AdminAreaListKey;
    property ResultList: IKeyList read Get_ResultList;
    property UserIDKey: WideString read Get_UserIDKey;
  end;

// *********************************************************************//
// DispIntf:  ICurrentSettingsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEA3-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  ICurrentSettingsDisp = dispinterface
    ['{801EBEA3-91CE-11D3-B564-005004B0B698}']
    property TaxonListKey: WideString readonly dispid 4;
    property BiotopeListKey: WideString readonly dispid 5;
    property AdminAreaListKey: WideString readonly dispid 6;
    property ResultList: IKeyList readonly dispid 7;
    property UserIDKey: WideString readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IRecorder2000
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEA5-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecorder2000 = interface(IDispatch)
    ['{801EBEA5-91CE-11D3-B564-005004B0B698}']
    function Get_CurrentSettings: ICurrentSettings6; safecall;
    function Get_Rucksack: IRucksack; safecall;
    function Get_JNCCFormat: Integer; safecall;
    procedure MenuOptionClick(const iItemName: WideString); safecall;
    procedure DisplayData(const iDataType: WideString; const iItems: IKeyList); safecall;
    procedure RequestData(const Requestor: IRequestor; const DataType: WideString); safecall;
    function ShowActiveForm(iForm: TGUID): IUnknown; safecall;
    function Find(const iTitle: WideString; const iType: WideString; const iInitialText: WideString): WideString; safecall;
    function GetNextKey(const iTableName: WideString): WideString; safecall;
    procedure MergeDatabase(const iDatabasePath: WideString); safecall;
    function Get_RecorderFunctions: IRecorderFunctions; safecall;
    procedure CheckDatabase(const iPath: WideString; const iDetails: WideString); safecall;
    function Get_SpatialRefSystem: WideString; safecall;
    function Get_RecorderMainForm: IRecorderMainForm; safecall;
    function Get_CanExportSystemSupplied: WordBool; safecall;
    procedure Set_CanExportSystemSupplied(Value: WordBool); safecall;
    function Get_RecorderMap: IRecorderMap; safecall;
    function Get_Version: WideString; safecall;
    function AccessSQLtoSQLServerSQL(const iSQLStatement: WideString): WideString; safecall;
    function Get_ConnectionString: WideString; safecall;
    procedure SetApplicationSecurity(const Connection: IUnknown); safecall;
    procedure ExportKeyList(const KeyList: IKeyList; const Destination: WideString; 
                            const _Type: WideString); safecall;
    function Get_ReportResults: IReportResults; safecall;
    function RequestCOMData(const ARequestor: IRequestor; ASupplierGUID: TGUID): IUnknown; safecall;
    property CurrentSettings: ICurrentSettings6 read Get_CurrentSettings;
    property Rucksack: IRucksack read Get_Rucksack;
    property JNCCFormat: Integer read Get_JNCCFormat;
    property RecorderFunctions: IRecorderFunctions read Get_RecorderFunctions;
    property SpatialRefSystem: WideString read Get_SpatialRefSystem;
    property RecorderMainForm: IRecorderMainForm read Get_RecorderMainForm;
    property CanExportSystemSupplied: WordBool read Get_CanExportSystemSupplied write Set_CanExportSystemSupplied;
    property RecorderMap: IRecorderMap read Get_RecorderMap;
    property Version: WideString read Get_Version;
    property ConnectionString: WideString read Get_ConnectionString;
    property ReportResults: IReportResults read Get_ReportResults;
  end;

// *********************************************************************//
// DispIntf:  IRecorder2000Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEA5-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecorder2000Disp = dispinterface
    ['{801EBEA5-91CE-11D3-B564-005004B0B698}']
    property CurrentSettings: ICurrentSettings6 readonly dispid 1;
    property Rucksack: IRucksack readonly dispid 2;
    property JNCCFormat: Integer readonly dispid 3;
    procedure MenuOptionClick(const iItemName: WideString); dispid 4;
    procedure DisplayData(const iDataType: WideString; const iItems: IKeyList); dispid 5;
    procedure RequestData(const Requestor: IRequestor; const DataType: WideString); dispid 6;
    function ShowActiveForm(iForm: {NOT_OLEAUTO(TGUID)}OleVariant): IUnknown; dispid 7;
    function Find(const iTitle: WideString; const iType: WideString; const iInitialText: WideString): WideString; dispid 8;
    function GetNextKey(const iTableName: WideString): WideString; dispid 9;
    procedure MergeDatabase(const iDatabasePath: WideString); dispid 10;
    property RecorderFunctions: IRecorderFunctions readonly dispid 12;
    procedure CheckDatabase(const iPath: WideString; const iDetails: WideString); dispid 11;
    property SpatialRefSystem: WideString readonly dispid 13;
    property RecorderMainForm: IRecorderMainForm readonly dispid 14;
    property CanExportSystemSupplied: WordBool dispid 16;
    property RecorderMap: IRecorderMap readonly dispid 17;
    property Version: WideString readonly dispid 15;
    function AccessSQLtoSQLServerSQL(const iSQLStatement: WideString): WideString; dispid 201;
    property ConnectionString: WideString readonly dispid 202;
    procedure SetApplicationSecurity(const Connection: IUnknown); dispid 203;
    procedure ExportKeyList(const KeyList: IKeyList; const Destination: WideString; 
                            const _Type: WideString); dispid 204;
    property ReportResults: IReportResults readonly dispid 205;
    function RequestCOMData(const ARequestor: IRequestor; 
                            ASupplierGUID: {NOT_OLEAUTO(TGUID)}OleVariant): IUnknown; dispid 206;
  end;

// *********************************************************************//
// Interface: IRecorderAddin
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEA7-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecorderAddin = interface(IDispatch)
    ['{801EBEA7-91CE-11D3-B564-005004B0B698}']
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    property Name: WideString read Get_Name;
    property Description: WideString read Get_Description;
    property ImageFileName: WideString read Get_ImageFileName;
  end;

// *********************************************************************//
// DispIntf:  IRecorderAddinDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEA7-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecorderAddinDisp = dispinterface
    ['{801EBEA7-91CE-11D3-B564-005004B0B698}']
    property Name: WideString readonly dispid 2;
    property Description: WideString readonly dispid 3;
    property ImageFileName: WideString readonly dispid 1;
    procedure Install(const iInstalledFilePath: WideString); dispid 5;
  end;

// *********************************************************************//
// Interface: IFilter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEA9-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IFilter = interface(IDispatch)
    ['{801EBEA9-91CE-11D3-B564-005004B0B698}']
    function Get_DefaultFileExtension: WideString; safecall;
    property DefaultFileExtension: WideString read Get_DefaultFileExtension;
  end;

// *********************************************************************//
// DispIntf:  IFilterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEA9-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IFilterDisp = dispinterface
    ['{801EBEA9-91CE-11D3-B564-005004B0B698}']
    property DefaultFileExtension: WideString readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IDialog
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEAB-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IDialog = interface(IDispatch)
    ['{801EBEAB-91CE-11D3-B564-005004B0B698}']
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function DoOk: WordBool; safecall;
    procedure DoCancel; safecall;
    property Width: Integer read Get_Width;
    property Height: Integer read Get_Height;
  end;

// *********************************************************************//
// DispIntf:  IDialogDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEAB-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IDialogDisp = dispinterface
    ['{801EBEAB-91CE-11D3-B564-005004B0B698}']
    property Width: Integer readonly dispid 1;
    property Height: Integer readonly dispid 2;
    function DoOk: WordBool; dispid 3;
    procedure DoCancel; dispid 4;
  end;

// *********************************************************************//
// Interface: IFormCaption
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEAD-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IFormCaption = interface(IDispatch)
    ['{801EBEAD-91CE-11D3-B564-005004B0B698}']
    function Get_FormCaption: WideString; safecall;
    property FormCaption: WideString read Get_FormCaption;
  end;

// *********************************************************************//
// DispIntf:  IFormCaptionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEAD-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IFormCaptionDisp = dispinterface
    ['{801EBEAD-91CE-11D3-B564-005004B0B698}']
    property FormCaption: WideString readonly dispid 1;
  end;

// *********************************************************************//
// Interface: IExecuteAction
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEAF-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IExecuteAction = interface(IDispatch)
    ['{801EBEAF-91CE-11D3-B564-005004B0B698}']
    procedure Execute; safecall;
  end;

// *********************************************************************//
// DispIntf:  IExecuteActionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEAF-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IExecuteActionDisp = dispinterface
    ['{801EBEAF-91CE-11D3-B564-005004B0B698}']
    procedure Execute; dispid 1;
  end;

// *********************************************************************//
// Interface: IHelp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEB1-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IHelp = interface(IDispatch)
    ['{801EBEB1-91CE-11D3-B564-005004B0B698}']
    procedure DoHelpContents; safecall;
  end;

// *********************************************************************//
// DispIntf:  IHelpDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEB1-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IHelpDisp = dispinterface
    ['{801EBEB1-91CE-11D3-B564-005004B0B698}']
    procedure DoHelpContents; dispid 2;
  end;

// *********************************************************************//
// Interface: IRecorderControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEB5-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecorderControl = interface(IDispatch)
    ['{801EBEB5-91CE-11D3-B564-005004B0B698}']
    function Get_Name: WideString; safecall;
    function Get_Left: Integer; safecall;
    procedure Set_Left(Value: Integer); safecall;
    function Get_Top: Integer; safecall;
    procedure Set_Top(Value: Integer); safecall;
    function Get_Width: Integer; safecall;
    procedure Set_Width(Value: Integer); safecall;
    function Get_Height: Integer; safecall;
    procedure Set_Height(Value: Integer); safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure RegisterEventHandler(const iEventName: WideString; const EventHandler: IEventHandler); safecall;
    property Name: WideString read Get_Name;
    property Left: Integer read Get_Left write Set_Left;
    property Top: Integer read Get_Top write Set_Top;
    property Width: Integer read Get_Width write Set_Width;
    property Height: Integer read Get_Height write Set_Height;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  end;

// *********************************************************************//
// DispIntf:  IRecorderControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEB5-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecorderControlDisp = dispinterface
    ['{801EBEB5-91CE-11D3-B564-005004B0B698}']
    property Name: WideString readonly dispid 1;
    property Left: Integer dispid 7;
    property Top: Integer dispid 9;
    property Width: Integer dispid 10;
    property Height: Integer dispid 11;
    property Visible: WordBool dispid 12;
    property Enabled: WordBool dispid 13;
    procedure RegisterEventHandler(const iEventName: WideString; const EventHandler: IEventHandler); dispid 2;
  end;

// *********************************************************************//
// Interface: IEditControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEB7-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IEditControl = interface(IDispatch)
    ['{801EBEB7-91CE-11D3-B564-005004B0B698}']
    function Get_Text: WideString; safecall;
    procedure Set_Text(const Value: WideString); safecall;
    property Text: WideString read Get_Text write Set_Text;
  end;

// *********************************************************************//
// DispIntf:  IEditControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEB7-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IEditControlDisp = dispinterface
    ['{801EBEB7-91CE-11D3-B564-005004B0B698}']
    property Text: WideString dispid 1;
  end;

// *********************************************************************//
// Interface: IListControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEB9-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IListControl = interface(IDispatch)
    ['{801EBEB9-91CE-11D3-B564-005004B0B698}']
    function Get_ItemCount: Integer; safecall;
    function Get_Items(iIndex: Integer): WideString; safecall;
    procedure Set_Items(iIndex: Integer; const Value: WideString); safecall;
    function Get_SelectedIndex: Integer; safecall;
    procedure Set_SelectedIndex(Value: Integer); safecall;
    function Get_Selected(iIndex: Integer): WordBool; safecall;
    property ItemCount: Integer read Get_ItemCount;
    property Items[iIndex: Integer]: WideString read Get_Items write Set_Items;
    property SelectedIndex: Integer read Get_SelectedIndex write Set_SelectedIndex;
    property Selected[iIndex: Integer]: WordBool read Get_Selected;
  end;

// *********************************************************************//
// DispIntf:  IListControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEB9-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IListControlDisp = dispinterface
    ['{801EBEB9-91CE-11D3-B564-005004B0B698}']
    property ItemCount: Integer readonly dispid 1;
    property Items[iIndex: Integer]: WideString dispid 2;
    property SelectedIndex: Integer dispid 3;
    property Selected[iIndex: Integer]: WordBool readonly dispid 4;
  end;

// *********************************************************************//
// Interface: IGridControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEBB-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IGridControl = interface(IDispatch)
    ['{801EBEBB-91CE-11D3-B564-005004B0B698}']
    function Get_ColumnCount: Integer; safecall;
    function Get_RowCount: Integer; safecall;
    function Get_CurrentRow: Integer; safecall;
    function Get_CurrentColumn: Integer; safecall;
    function Get_Cells(iX: Integer; iY: Integer): WideString; safecall;
    procedure Set_Cells(iX: Integer; iY: Integer; const iText: WideString); safecall;
    property ColumnCount: Integer read Get_ColumnCount;
    property RowCount: Integer read Get_RowCount;
    property CurrentRow: Integer read Get_CurrentRow;
    property CurrentColumn: Integer read Get_CurrentColumn;
    property Cells[iX: Integer; iY: Integer]: WideString read Get_Cells write Set_Cells;
  end;

// *********************************************************************//
// DispIntf:  IGridControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEBB-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IGridControlDisp = dispinterface
    ['{801EBEBB-91CE-11D3-B564-005004B0B698}']
    property ColumnCount: Integer readonly dispid 1;
    property RowCount: Integer readonly dispid 2;
    property CurrentRow: Integer readonly dispid 3;
    property CurrentColumn: Integer readonly dispid 4;
    property Cells[iX: Integer; iY: Integer]: WideString dispid 5;
  end;

// *********************************************************************//
// Interface: ILabelControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEBD-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  ILabelControl = interface(IDispatch)
    ['{801EBEBD-91CE-11D3-B564-005004B0B698}']
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    property Caption: WideString read Get_Caption write Set_Caption;
  end;

// *********************************************************************//
// DispIntf:  ILabelControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEBD-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  ILabelControlDisp = dispinterface
    ['{801EBEBD-91CE-11D3-B564-005004B0B698}']
    property Caption: WideString dispid 1;
  end;

// *********************************************************************//
// Interface: ITabPageControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEBF-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  ITabPageControl = interface(IDispatch)
    ['{801EBEBF-91CE-11D3-B564-005004B0B698}']
    function Get_Caption: WideString; safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    property Caption: WideString read Get_Caption write Set_Caption;
  end;

// *********************************************************************//
// DispIntf:  ITabPageControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEBF-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  ITabPageControlDisp = dispinterface
    ['{801EBEBF-91CE-11D3-B564-005004B0B698}']
    property Caption: WideString dispid 1;
  end;

// *********************************************************************//
// Interface: IRecorderFormEvents
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEC1-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecorderFormEvents = interface(IDispatch)
    ['{801EBEC1-91CE-11D3-B564-005004B0B698}']
    function Get_FormName: WideString; safecall;
    procedure DoItemChange(const iTableName: WideString; const iKeyValue: WideString); safecall;
    procedure DoSave; safecall;
    procedure DoCancel; safecall;
    procedure SetForm(const iForm: IRecorderForm); safecall;
    procedure DoEditMode; safecall;
    procedure DoAdd; safecall;
    procedure DoDelete; safecall;
    function CheckCanSave: WordBool; safecall;
    property FormName: WideString read Get_FormName;
  end;

// *********************************************************************//
// DispIntf:  IRecorderFormEventsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEC1-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecorderFormEventsDisp = dispinterface
    ['{801EBEC1-91CE-11D3-B564-005004B0B698}']
    property FormName: WideString readonly dispid 10;
    procedure DoItemChange(const iTableName: WideString; const iKeyValue: WideString); dispid 11;
    procedure DoSave; dispid 12;
    procedure DoCancel; dispid 14;
    procedure SetForm(const iForm: IRecorderForm); dispid 5;
    procedure DoEditMode; dispid 1;
    procedure DoAdd; dispid 2;
    procedure DoDelete; dispid 3;
    function CheckCanSave: WordBool; dispid 6;
  end;

// *********************************************************************//
// Interface: IComboControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEC3-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IComboControl = interface(IDispatch)
    ['{801EBEC3-91CE-11D3-B564-005004B0B698}']
    function Get_ItemCount: Integer; safecall;
    function Get_Items(iIndex: Integer): WideString; safecall;
    procedure Set_Items(iIndex: Integer; const Value: WideString); safecall;
    function Get_SelectedIndex: Integer; safecall;
    procedure Set_SelectedIndex(Value: Integer); safecall;
    property ItemCount: Integer read Get_ItemCount;
    property Items[iIndex: Integer]: WideString read Get_Items write Set_Items;
    property SelectedIndex: Integer read Get_SelectedIndex write Set_SelectedIndex;
  end;

// *********************************************************************//
// DispIntf:  IComboControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEC3-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IComboControlDisp = dispinterface
    ['{801EBEC3-91CE-11D3-B564-005004B0B698}']
    property ItemCount: Integer readonly dispid 1;
    property Items[iIndex: Integer]: WideString dispid 2;
    property SelectedIndex: Integer dispid 3;
  end;

// *********************************************************************//
// Interface: IAdditionalFilter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEC5-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IAdditionalFilter = interface(IDispatch)
    ['{801EBEC5-91CE-11D3-B564-005004B0B698}']
    function Get_FieldKey: WideString; safecall;
    function Get_Criteria: WideString; safecall;
    function Get_Condition: WideString; safecall;
    function Get_AndOperation: WordBool; safecall;
    property FieldKey: WideString read Get_FieldKey;
    property Criteria: WideString read Get_Criteria;
    property Condition: WideString read Get_Condition;
    property AndOperation: WordBool read Get_AndOperation;
  end;

// *********************************************************************//
// DispIntf:  IAdditionalFilterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEC5-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IAdditionalFilterDisp = dispinterface
    ['{801EBEC5-91CE-11D3-B564-005004B0B698}']
    property FieldKey: WideString readonly dispid 1;
    property Criteria: WideString readonly dispid 2;
    property Condition: WideString readonly dispid 3;
    property AndOperation: WordBool readonly dispid 5;
  end;

// *********************************************************************//
// Interface: IKeyListSupplier
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEC7-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IKeyListSupplier = interface(IDispatch)
    ['{801EBEC7-91CE-11D3-B564-005004B0B698}']
    function Get_KeyList: IKeyList; safecall;
    function Get_Exportable: WordBool; safecall;
    property KeyList: IKeyList read Get_KeyList;
    property Exportable: WordBool read Get_Exportable;
  end;

// *********************************************************************//
// DispIntf:  IKeyListSupplierDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEC7-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IKeyListSupplierDisp = dispinterface
    ['{801EBEC7-91CE-11D3-B564-005004B0B698}']
    property KeyList: IKeyList readonly dispid 1;
    property Exportable: WordBool readonly dispid 2;
  end;

// *********************************************************************//
// Interface: IValidation
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEC9-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IValidation = interface(IDispatch)
    ['{801EBEC9-91CE-11D3-B564-005004B0B698}']
    function Validate(const iPath: WideString): Integer; safecall;
    function Get_KeyList: IKeyList; safecall;
    function Get_ErrorString(iIndex: Integer): WideString; safecall;
    property KeyList: IKeyList read Get_KeyList;
    property ErrorString[iIndex: Integer]: WideString read Get_ErrorString;
  end;

// *********************************************************************//
// DispIntf:  IValidationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBEC9-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IValidationDisp = dispinterface
    ['{801EBEC9-91CE-11D3-B564-005004B0B698}']
    function Validate(const iPath: WideString): Integer; dispid 1;
    property KeyList: IKeyList readonly dispid 2;
    property ErrorString[iIndex: Integer]: WideString readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IExportFilter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBECB-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IExportFilter = interface(IDispatch)
    ['{801EBECB-91CE-11D3-B564-005004B0B698}']
    function ExportFile(const iItemsToExport: IKeyList; const iDestinationFile: WideString): WordBool; safecall;
    function Get_LastExportError: WideString; safecall;
    property LastExportError: WideString read Get_LastExportError;
  end;

// *********************************************************************//
// DispIntf:  IExportFilterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBECB-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IExportFilterDisp = dispinterface
    ['{801EBECB-91CE-11D3-B564-005004B0B698}']
    function ExportFile(const iItemsToExport: IKeyList; const iDestinationFile: WideString): WordBool; dispid 2;
    property LastExportError: WideString readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IAdditionalFilterDialog
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBECD-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IAdditionalFilterDialog = interface(IDispatch)
    ['{801EBECD-91CE-11D3-B564-005004B0B698}']
    procedure AddFilterableField(const iKeyValue: WideString); safecall;
    procedure FinishAddingFields; safecall;
    function GetNextFilter: IAdditionalFilter; safecall;
  end;

// *********************************************************************//
// DispIntf:  IAdditionalFilterDialogDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBECD-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IAdditionalFilterDialogDisp = dispinterface
    ['{801EBECD-91CE-11D3-B564-005004B0B698}']
    procedure AddFilterableField(const iKeyValue: WideString); dispid 1;
    procedure FinishAddingFields; dispid 2;
    function GetNextFilter: IAdditionalFilter; dispid 3;
  end;

// *********************************************************************//
// Interface: IRecorderForm
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBECF-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecorderForm = interface(IDispatch)
    ['{801EBECF-91CE-11D3-B564-005004B0B698}']
    function Get_Control(const iName: WideString): IUnknown; safecall;
    procedure EmbedActiveX(const iControl: IUnknown; iClsId: TGUID; 
                           const iParent: IRecorderControl; iLeft: Integer; iTop: Integer; 
                           iWidth: Integer; iHeight: Integer); safecall;
    property Control[const iName: WideString]: IUnknown read Get_Control;
  end;

// *********************************************************************//
// DispIntf:  IRecorderFormDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {801EBECF-91CE-11D3-B564-005004B0B698}
// *********************************************************************//
  IRecorderFormDisp = dispinterface
    ['{801EBECF-91CE-11D3-B564-005004B0B698}']
    property Control[const iName: WideString]: IUnknown readonly dispid 1;
    procedure EmbedActiveX(const iControl: IUnknown; iClsId: {NOT_OLEAUTO(TGUID)}OleVariant; 
                           const iParent: IRecorderControl; iLeft: Integer; iTop: Integer; 
                           iWidth: Integer; iHeight: Integer); dispid 2;
  end;

// *********************************************************************//
// Interface: IRequestor
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {21A6FBA1-A890-11D3-B7C2-0060085B710F}
// *********************************************************************//
  IRequestor = interface(IDispatch)
    ['{21A6FBA1-A890-11D3-B7C2-0060085B710F}']
    procedure Update(const KeyList: IKeyList); safecall;
  end;

// *********************************************************************//
// DispIntf:  IRequestorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {21A6FBA1-A890-11D3-B7C2-0060085B710F}
// *********************************************************************//
  IRequestorDisp = dispinterface
    ['{21A6FBA1-A890-11D3-B7C2-0060085B710F}']
    procedure Update(const KeyList: IKeyList); dispid 1;
  end;

// *********************************************************************//
// Interface: IEventHandler
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {36904D41-E5F3-11D3-B54F-0050DA5FA07B}
// *********************************************************************//
  IEventHandler = interface(IDispatch)
    ['{36904D41-E5F3-11D3-B54F-0050DA5FA07B}']
    function OnEvent(const iControlName: WideString; const iEventName: WideString): WordBool; safecall;
  end;

// *********************************************************************//
// DispIntf:  IEventHandlerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {36904D41-E5F3-11D3-B54F-0050DA5FA07B}
// *********************************************************************//
  IEventHandlerDisp = dispinterface
    ['{36904D41-E5F3-11D3-B54F-0050DA5FA07B}']
    function OnEvent(const iControlName: WideString; const iEventName: WideString): WordBool; dispid 1;
  end;

// *********************************************************************//
// Interface: IRecorderFunctions
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FE68E8F1-321E-11D4-BD47-0050DA5FA07B}
// *********************************************************************//
  IRecorderFunctions = interface(IDispatch)
    ['{FE68E8F1-321E-11D4-BD47-0050DA5FA07B}']
    function HTMLtoRTF(const iHTML: WideString): WideString; safecall;
    function DecodeSpatialRef(const iSpatialRef: WideString): ILatLong; safecall;
    function GetSpatialRefFromRecord(const IKeyList: IKeyList; const iFieldSpecifier: WideString): WideString; safecall;
    function DecodeVagueDate(const IVagueDate: WideString): IVagueDate; safecall;
    function GetDateFromRecord(const IKeyList: IKeyList; const iFieldSpecifier: WideString): WideString; safecall;
    function EncodeVagueDate(const IVagueDate: IVagueDate): WideString; safecall;
    function Get_SpatialRefSystem: WideString; safecall;
    function EncodeSpatialRef(const iReference: ILatLong): WideString; safecall;
    function IdentifySpatialRefSystem(const iSpatialRef: WideString): WideString; safecall;
    function GetFilterItems(const ExportFilterKey: WideString): IKeyList; safecall;
    property SpatialRefSystem: WideString read Get_SpatialRefSystem;
  end;

// *********************************************************************//
// DispIntf:  IRecorderFunctionsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FE68E8F1-321E-11D4-BD47-0050DA5FA07B}
// *********************************************************************//
  IRecorderFunctionsDisp = dispinterface
    ['{FE68E8F1-321E-11D4-BD47-0050DA5FA07B}']
    function HTMLtoRTF(const iHTML: WideString): WideString; dispid 1;
    function DecodeSpatialRef(const iSpatialRef: WideString): ILatLong; dispid 2;
    function GetSpatialRefFromRecord(const IKeyList: IKeyList; const iFieldSpecifier: WideString): WideString; dispid 3;
    function DecodeVagueDate(const IVagueDate: WideString): IVagueDate; dispid 4;
    function GetDateFromRecord(const IKeyList: IKeyList; const iFieldSpecifier: WideString): WideString; dispid 5;
    function EncodeVagueDate(const IVagueDate: IVagueDate): WideString; dispid 6;
    property SpatialRefSystem: WideString readonly dispid 8;
    function EncodeSpatialRef(const iReference: ILatLong): WideString; dispid 7;
    function IdentifySpatialRefSystem(const iSpatialRef: WideString): WideString; dispid 9;
    function GetFilterItems(const ExportFilterKey: WideString): IKeyList; dispid 201;
  end;

// *********************************************************************//
// Interface: IBaseMapFormat
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {77337221-FE76-11D4-B5B8-00105A9841ED}
// *********************************************************************//
  IBaseMapFormat = interface(IDispatch)
    ['{77337221-FE76-11D4-B5B8-00105A9841ED}']
    function MapCoordToLat(iEasting: Double; iNorthing: Double): WideString; safecall;
    function MapCoordToLong(iEasting: Double; iNorthing: Double): WideString; safecall;
    function Get_CanDisplayGrids: WordBool; safecall;
    function Get_MapCoordsPerMetre: Double; safecall;
    function LatLongToMapCoordX(iLat: Double; iLong: Double): Double; safecall;
    function LatLongToMapCoordY(iLat: Double; iLong: Double): Double; safecall;
    property CanDisplayGrids: WordBool read Get_CanDisplayGrids;
    property MapCoordsPerMetre: Double read Get_MapCoordsPerMetre;
  end;

// *********************************************************************//
// DispIntf:  IBaseMapFormatDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {77337221-FE76-11D4-B5B8-00105A9841ED}
// *********************************************************************//
  IBaseMapFormatDisp = dispinterface
    ['{77337221-FE76-11D4-B5B8-00105A9841ED}']
    function MapCoordToLat(iEasting: Double; iNorthing: Double): WideString; dispid 1;
    function MapCoordToLong(iEasting: Double; iNorthing: Double): WideString; dispid 2;
    property CanDisplayGrids: WordBool readonly dispid 5;
    property MapCoordsPerMetre: Double readonly dispid 6;
    function LatLongToMapCoordX(iLat: Double; iLong: Double): Double; dispid 3;
    function LatLongToMapCoordY(iLat: Double; iLong: Double): Double; dispid 4;
  end;

// *********************************************************************//
// Interface: ILatLong
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {69752F50-0276-11D5-B5BC-00105A9841ED}
// *********************************************************************//
  ILatLong = interface(IDispatch)
    ['{69752F50-0276-11D5-B5BC-00105A9841ED}']
    function Get_Latitude: Double; safecall;
    function Get_Longitude: Double; safecall;
    function Get_ErrorMsg: WideString; safecall;
    property Latitude: Double read Get_Latitude;
    property Longitude: Double read Get_Longitude;
    property ErrorMsg: WideString read Get_ErrorMsg;
  end;

// *********************************************************************//
// DispIntf:  ILatLongDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {69752F50-0276-11D5-B5BC-00105A9841ED}
// *********************************************************************//
  ILatLongDisp = dispinterface
    ['{69752F50-0276-11D5-B5BC-00105A9841ED}']
    property Latitude: Double readonly dispid 1;
    property Longitude: Double readonly dispid 3;
    property ErrorMsg: WideString readonly dispid 4;
  end;

// *********************************************************************//
// Interface: IVagueDate
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4234BD81-0322-11D5-B5BD-00105A9841ED}
// *********************************************************************//
  IVagueDate = interface(IDispatch)
    ['{4234BD81-0322-11D5-B5BD-00105A9841ED}']
    function Get_StartDate: TDateTime; safecall;
    function Get_EndDate: TDateTime; safecall;
    function Get_DateType: WideString; safecall;
    function Get_ErrorMsg: WideString; safecall;
    property StartDate: TDateTime read Get_StartDate;
    property EndDate: TDateTime read Get_EndDate;
    property DateType: WideString read Get_DateType;
    property ErrorMsg: WideString read Get_ErrorMsg;
  end;

// *********************************************************************//
// DispIntf:  IVagueDateDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4234BD81-0322-11D5-B5BD-00105A9841ED}
// *********************************************************************//
  IVagueDateDisp = dispinterface
    ['{4234BD81-0322-11D5-B5BD-00105A9841ED}']
    property StartDate: TDateTime readonly dispid 1;
    property EndDate: TDateTime readonly dispid 2;
    property DateType: WideString readonly dispid 3;
    property ErrorMsg: WideString readonly dispid 4;
  end;

// *********************************************************************//
// Interface: IFormWithToolbar
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7BA0831C-7C87-484D-A9B0-A8C987F4C062}
// *********************************************************************//
  IFormWithToolbar = interface(IDispatch)
    ['{7BA0831C-7C87-484D-A9B0-A8C987F4C062}']
    function Get_Toolbar: IUnknown; safecall;
    property Toolbar: IUnknown read Get_Toolbar;
  end;

// *********************************************************************//
// DispIntf:  IFormWithToolbarDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7BA0831C-7C87-484D-A9B0-A8C987F4C062}
// *********************************************************************//
  IFormWithToolbarDisp = dispinterface
    ['{7BA0831C-7C87-484D-A9B0-A8C987F4C062}']
    property Toolbar: IUnknown readonly dispid 1;
  end;

// *********************************************************************//
// Interface: IRecorderMainForm
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {359E6BC6-5BBB-4D07-9254-5E24ED634C12}
// *********************************************************************//
  IRecorderMainForm = interface(IDispatch)
    ['{359E6BC6-5BBB-4D07-9254-5E24ED634C12}']
    function Get_StatusText: WideString; safecall;
    procedure Set_StatusText(const Value: WideString); safecall;
    procedure StartProgressBar; safecall;
    procedure StopProgressBar; safecall;
    function Get_Progress: Integer; safecall;
    procedure Set_Progress(Value: Integer); safecall;
    property StatusText: WideString read Get_StatusText write Set_StatusText;
    property Progress: Integer read Get_Progress write Set_Progress;
  end;

// *********************************************************************//
// DispIntf:  IRecorderMainFormDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {359E6BC6-5BBB-4D07-9254-5E24ED634C12}
// *********************************************************************//
  IRecorderMainFormDisp = dispinterface
    ['{359E6BC6-5BBB-4D07-9254-5E24ED634C12}']
    property StatusText: WideString dispid 2;
    procedure StartProgressBar; dispid 3;
    procedure StopProgressBar; dispid 4;
    property Progress: Integer dispid 5;
  end;

// *********************************************************************//
// Interface: IRecorderMap
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C0E1175F-B5AF-48EC-8E94-EBFF087410D5}
// *********************************************************************//
  IRecorderMap = interface(IDispatch)
    ['{C0E1175F-B5AF-48EC-8E94-EBFF087410D5}']
    function Get_Scale: WideString; safecall;
    procedure PanMap(const ISpatialReference: WideString); safecall;
    function Get_BaseMapRefSystem: WideString; safecall;
    function RefreshSheets: WordBool; safecall;
    property Scale: WideString read Get_Scale;
    property BaseMapRefSystem: WideString read Get_BaseMapRefSystem;
  end;

// *********************************************************************//
// DispIntf:  IRecorderMapDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C0E1175F-B5AF-48EC-8E94-EBFF087410D5}
// *********************************************************************//
  IRecorderMapDisp = dispinterface
    ['{C0E1175F-B5AF-48EC-8E94-EBFF087410D5}']
    property Scale: WideString readonly dispid 2;
    procedure PanMap(const ISpatialReference: WideString); dispid 3;
    property BaseMapRefSystem: WideString readonly dispid 4;
    function RefreshSheets: WordBool; dispid 1;
  end;

// *********************************************************************//
// Interface: IReportResults
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8787F00E-18E4-4B33-8882-CC04E44E9FB1}
// *********************************************************************//
  IReportResults = interface(IDispatch)
    ['{8787F00E-18E4-4B33-8882-CC04E44E9FB1}']
    function Get_ReportConnection: _Connection; safecall;
    function Get_ReportSQL: WideString; safecall;
    property ReportConnection: _Connection read Get_ReportConnection;
    property ReportSQL: WideString read Get_ReportSQL;
  end;

// *********************************************************************//
// DispIntf:  IReportResultsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8787F00E-18E4-4B33-8882-CC04E44E9FB1}
// *********************************************************************//
  IReportResultsDisp = dispinterface
    ['{8787F00E-18E4-4B33-8882-CC04E44E9FB1}']
    property ReportConnection: _Connection readonly dispid 201;
    property ReportSQL: WideString readonly dispid 202;
  end;

// *********************************************************************//
// Interface: IProvidesOccurrencesSQL
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EED143EC-45A5-4338-A329-860B23F3A7C6}
// *********************************************************************//
  IProvidesOccurrencesSQL = interface(IDispatch)
    ['{EED143EC-45A5-4338-A329-860B23F3A7C6}']
    function Get_CanProvideSQL: WordBool; safecall;
    function Get_OccurrencesSQL(const ATypes: WideString): WideString; safecall;
    property CanProvideSQL: WordBool read Get_CanProvideSQL;
    property OccurrencesSQL[const ATypes: WideString]: WideString read Get_OccurrencesSQL;
  end;

// *********************************************************************//
// DispIntf:  IProvidesOccurrencesSQLDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EED143EC-45A5-4338-A329-860B23F3A7C6}
// *********************************************************************//
  IProvidesOccurrencesSQLDisp = dispinterface
    ['{EED143EC-45A5-4338-A329-860B23F3A7C6}']
    property CanProvideSQL: WordBool readonly dispid 201;
    property OccurrencesSQL[const ATypes: WideString]: WideString readonly dispid 202;
  end;

// *********************************************************************//
// Interface: ICurrentSettings6
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C03D3B3B-B543-4B97-BF7D-5EF0677CB86A}
// *********************************************************************//
  ICurrentSettings6 = interface(ICurrentSettings)
    ['{C03D3B3B-B543-4B97-BF7D-5EF0677CB86A}']
    function Get_DragDestinationColour: Integer; safecall;
    function Get_DragSourceColour: Integer; safecall;
    function Get_MandatoryColour: Integer; safecall;
    function Get_DisableDragDropFrames: WordBool; safecall;
    function Get_AvailableMapCount: Integer; safecall;
    function Get_AvailableMap(Index: Integer): IAvailableMap; safecall;
    function Get_DefaultMap: IAvailableMap; safecall;
    function Get_SessionID: WideString; safecall;
    function Get_PartialTaxonSearch: WordBool; safecall;
    property DragDestinationColour: Integer read Get_DragDestinationColour;
    property DragSourceColour: Integer read Get_DragSourceColour;
    property MandatoryColour: Integer read Get_MandatoryColour;
    property DisableDragDropFrames: WordBool read Get_DisableDragDropFrames;
    property AvailableMapCount: Integer read Get_AvailableMapCount;
    property AvailableMap[Index: Integer]: IAvailableMap read Get_AvailableMap;
    property DefaultMap: IAvailableMap read Get_DefaultMap;
    property SessionID: WideString read Get_SessionID;
    property PartialTaxonSearch: WordBool read Get_PartialTaxonSearch;
  end;

// *********************************************************************//
// DispIntf:  ICurrentSettings6Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C03D3B3B-B543-4B97-BF7D-5EF0677CB86A}
// *********************************************************************//
  ICurrentSettings6Disp = dispinterface
    ['{C03D3B3B-B543-4B97-BF7D-5EF0677CB86A}']
    property DragDestinationColour: Integer readonly dispid 8;
    property DragSourceColour: Integer readonly dispid 9;
    property MandatoryColour: Integer readonly dispid 10;
    property DisableDragDropFrames: WordBool readonly dispid 11;
    property AvailableMapCount: Integer readonly dispid 301;
    property AvailableMap[Index: Integer]: IAvailableMap readonly dispid 302;
    property DefaultMap: IAvailableMap readonly dispid 304;
    property SessionID: WideString readonly dispid 303;
    property PartialTaxonSearch: WordBool readonly dispid 305;
    property TaxonListKey: WideString readonly dispid 4;
    property BiotopeListKey: WideString readonly dispid 5;
    property AdminAreaListKey: WideString readonly dispid 6;
    property ResultList: IKeyList readonly dispid 7;
    property UserIDKey: WideString readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IMergeMenuManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5DF1B2E0-9A24-45A1-A56C-EAA05B8E20A6}
// *********************************************************************//
  IMergeMenuManager = interface(IDispatch)
    ['{5DF1B2E0-9A24-45A1-A56C-EAA05B8E20A6}']
    function Get_DynamicMenuList: IDynamicMenuList; safecall;
    property DynamicMenuList: IDynamicMenuList read Get_DynamicMenuList;
  end;

// *********************************************************************//
// DispIntf:  IMergeMenuManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5DF1B2E0-9A24-45A1-A56C-EAA05B8E20A6}
// *********************************************************************//
  IMergeMenuManagerDisp = dispinterface
    ['{5DF1B2E0-9A24-45A1-A56C-EAA05B8E20A6}']
    property DynamicMenuList: IDynamicMenuList readonly dispid 201;
  end;

// *********************************************************************//
// Interface: INodeManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8BF99A9C-125B-468C-88E3-C8867465D922}
// *********************************************************************//
  INodeManager = interface(IDispatch)
    ['{8BF99A9C-125B-468C-88E3-C8867465D922}']
    function AddNode(TypeID: Integer; const ParentItemKey: WideString; var NodeInfo: TNodeInfo): WordBool; safecall;
    function GetAddSubType(ParentTypeID: Integer; Index: Integer; var TypeID: Integer): WordBool; safecall;
    function GetAddType(ParentTypeID: Integer; Index: Integer; var TypeID: Integer): WordBool; safecall;
    function GetData(ParentTypeID: Integer; TypeID: Integer; const ParentKey: WideString; 
                     var SQL: WideString): WordBool; safecall;
    function GetNodeInfo(TypeID: Integer; const DataFields: Fields; var NodeInfo: TNodeInfo): WordBool; safecall;
    function GetSubNodeInfo(ParentTypeID: Integer; Index: Integer; var NodeInfo: TNodeInfo): WordBool; safecall;
    function NodeChanged(const ParentItemKey: WideString; var NodeInfo: TNodeInfo; 
                         var DetailScreenGUID: TGUID): WordBool; safecall;
    function Get_ImageList: LongWord; safecall;
    function Get_StateImageList: LongWord; safecall;
    function Get_TypeCount: Integer; safecall;
    function Get_TypeID(Index: Integer): Integer; safecall;
    function Get_TypeName(TypeID: Integer): WideString; safecall;
    property ImageList: LongWord read Get_ImageList;
    property StateImageList: LongWord read Get_StateImageList;
    property TypeCount: Integer read Get_TypeCount;
    property TypeID[Index: Integer]: Integer read Get_TypeID;
    property TypeName[TypeID: Integer]: WideString read Get_TypeName;
  end;

// *********************************************************************//
// DispIntf:  INodeManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8BF99A9C-125B-468C-88E3-C8867465D922}
// *********************************************************************//
  INodeManagerDisp = dispinterface
    ['{8BF99A9C-125B-468C-88E3-C8867465D922}']
    function AddNode(TypeID: Integer; const ParentItemKey: WideString; 
                     var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant): WordBool; dispid 201;
    function GetAddSubType(ParentTypeID: Integer; Index: Integer; var TypeID: Integer): WordBool; dispid 204;
    function GetAddType(ParentTypeID: Integer; Index: Integer; var TypeID: Integer): WordBool; dispid 205;
    function GetData(ParentTypeID: Integer; TypeID: Integer; const ParentKey: WideString; 
                     var SQL: WideString): WordBool; dispid 206;
    function GetNodeInfo(TypeID: Integer; const DataFields: Fields; 
                         var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant): WordBool; dispid 207;
    function GetSubNodeInfo(ParentTypeID: Integer; Index: Integer; 
                            var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant): WordBool; dispid 208;
    function NodeChanged(const ParentItemKey: WideString; 
                         var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant; 
                         var DetailScreenGUID: {NOT_OLEAUTO(TGUID)}OleVariant): WordBool; dispid 209;
    property ImageList: LongWord readonly dispid 210;
    property StateImageList: LongWord readonly dispid 211;
    property TypeCount: Integer readonly dispid 212;
    property TypeID[Index: Integer]: Integer readonly dispid 213;
    property TypeName[TypeID: Integer]: WideString readonly dispid 214;
  end;

// *********************************************************************//
// Interface: IOptionsPage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B02EEC11-F45E-4B3E-90D9-4EDBFC63404E}
// *********************************************************************//
  IOptionsPage = interface(IDispatch)
    ['{B02EEC11-F45E-4B3E-90D9-4EDBFC63404E}']
    function Get_Title: WideString; safecall;
    procedure Load; safecall;
    procedure Save; safecall;
    procedure Default; safecall;
    property Title: WideString read Get_Title;
  end;

// *********************************************************************//
// DispIntf:  IOptionsPageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B02EEC11-F45E-4B3E-90D9-4EDBFC63404E}
// *********************************************************************//
  IOptionsPageDisp = dispinterface
    ['{B02EEC11-F45E-4B3E-90D9-4EDBFC63404E}']
    property Title: WideString readonly dispid 201;
    procedure Load; dispid 202;
    procedure Save; dispid 203;
    procedure Default; dispid 204;
  end;

// *********************************************************************//
// Interface: IOptionsPages
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {629DE5B3-A866-4D32-8BE8-78937C864774}
// *********************************************************************//
  IOptionsPages = interface(IDispatch)
    ['{629DE5B3-A866-4D32-8BE8-78937C864774}']
    function Get_OptionsPageCount: Integer; safecall;
    function Get_OptionsPageItems(Index: Integer): IOptionsPage; safecall;
    property OptionsPageCount: Integer read Get_OptionsPageCount;
    property OptionsPageItems[Index: Integer]: IOptionsPage read Get_OptionsPageItems;
  end;

// *********************************************************************//
// DispIntf:  IOptionsPagesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {629DE5B3-A866-4D32-8BE8-78937C864774}
// *********************************************************************//
  IOptionsPagesDisp = dispinterface
    ['{629DE5B3-A866-4D32-8BE8-78937C864774}']
    property OptionsPageCount: Integer readonly dispid 201;
    property OptionsPageItems[Index: Integer]: IOptionsPage readonly dispid 202;
  end;

// *********************************************************************//
// Interface: IMapPoint
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B9870681-B2F1-4664-98FD-0EEC3DB510F9}
// *********************************************************************//
  IMapPoint = interface(IDispatch)
    ['{B9870681-B2F1-4664-98FD-0EEC3DB510F9}']
    function Get_Lat: Double; safecall;
    function Get_Long: Double; safecall;
    function Get_Accuracy: Integer; safecall;
    function Get_RecordKey: WideString; safecall;
    function Get_Date: TDateTime; safecall;
    function Get_Value: Integer; safecall;
    property Lat: Double read Get_Lat;
    property Long: Double read Get_Long;
    property Accuracy: Integer read Get_Accuracy;
    property RecordKey: WideString read Get_RecordKey;
    property Date: TDateTime read Get_Date;
    property Value: Integer read Get_Value;
  end;

// *********************************************************************//
// DispIntf:  IMapPointDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B9870681-B2F1-4664-98FD-0EEC3DB510F9}
// *********************************************************************//
  IMapPointDisp = dispinterface
    ['{B9870681-B2F1-4664-98FD-0EEC3DB510F9}']
    property Lat: Double readonly dispid 201;
    property Long: Double readonly dispid 202;
    property Accuracy: Integer readonly dispid 203;
    property RecordKey: WideString readonly dispid 204;
    property Date: TDateTime readonly dispid 205;
    property Value: Integer readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IMapDropFormat
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F51213F5-8845-468F-BEB0-7CFD874DCD95}
// *********************************************************************//
  IMapDropFormat = interface(IDispatch)
    ['{F51213F5-8845-468F-BEB0-7CFD874DCD95}']
    procedure InitialiseDataset(const KeyList: IKeyList); safecall;
    function Get_DatasetTableName: WideString; safecall;
    function Get_MapPointCount: Integer; safecall;
    function Get_MapPoint(Index: Integer): IMapPoint; safecall;
    function Get_SupportedTableCount: Integer; safecall;
    function Get_SupportedTable(Index: Integer): WideString; safecall;
    property DatasetTableName: WideString read Get_DatasetTableName;
    property MapPointCount: Integer read Get_MapPointCount;
    property MapPoint[Index: Integer]: IMapPoint read Get_MapPoint;
    property SupportedTableCount: Integer read Get_SupportedTableCount;
    property SupportedTable[Index: Integer]: WideString read Get_SupportedTable;
  end;

// *********************************************************************//
// DispIntf:  IMapDropFormatDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F51213F5-8845-468F-BEB0-7CFD874DCD95}
// *********************************************************************//
  IMapDropFormatDisp = dispinterface
    ['{F51213F5-8845-468F-BEB0-7CFD874DCD95}']
    procedure InitialiseDataset(const KeyList: IKeyList); dispid 202;
    property DatasetTableName: WideString readonly dispid 203;
    property MapPointCount: Integer readonly dispid 204;
    property MapPoint[Index: Integer]: IMapPoint readonly dispid 205;
    property SupportedTableCount: Integer readonly dispid 206;
    property SupportedTable[Index: Integer]: WideString readonly dispid 201;
  end;

// *********************************************************************//
// Interface: IOccurrenceNodeManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CA30D9F0-DAD9-40CB-9439-2B861A28470D}
// *********************************************************************//
  IOccurrenceNodeManager = interface(INodeManager)
    ['{CA30D9F0-DAD9-40CB-9439-2B861A28470D}']
  end;

// *********************************************************************//
// DispIntf:  IOccurrenceNodeManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CA30D9F0-DAD9-40CB-9439-2B861A28470D}
// *********************************************************************//
  IOccurrenceNodeManagerDisp = dispinterface
    ['{CA30D9F0-DAD9-40CB-9439-2B861A28470D}']
    function AddNode(TypeID: Integer; const ParentItemKey: WideString; 
                     var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant): WordBool; dispid 201;
    function GetAddSubType(ParentTypeID: Integer; Index: Integer; var TypeID: Integer): WordBool; dispid 204;
    function GetAddType(ParentTypeID: Integer; Index: Integer; var TypeID: Integer): WordBool; dispid 205;
    function GetData(ParentTypeID: Integer; TypeID: Integer; const ParentKey: WideString; 
                     var SQL: WideString): WordBool; dispid 206;
    function GetNodeInfo(TypeID: Integer; const DataFields: Fields; 
                         var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant): WordBool; dispid 207;
    function GetSubNodeInfo(ParentTypeID: Integer; Index: Integer; 
                            var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant): WordBool; dispid 208;
    function NodeChanged(const ParentItemKey: WideString; 
                         var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant; 
                         var DetailScreenGUID: {NOT_OLEAUTO(TGUID)}OleVariant): WordBool; dispid 209;
    property ImageList: LongWord readonly dispid 210;
    property StateImageList: LongWord readonly dispid 211;
    property TypeCount: Integer readonly dispid 212;
    property TypeID[Index: Integer]: Integer readonly dispid 213;
    property TypeName[TypeID: Integer]: WideString readonly dispid 214;
  end;

// *********************************************************************//
// Interface: ILocationNodeManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {453CEF6E-19F1-4CC1-8564-40EF48A4FD21}
// *********************************************************************//
  ILocationNodeManager = interface(INodeManager)
    ['{453CEF6E-19F1-4CC1-8564-40EF48A4FD21}']
  end;

// *********************************************************************//
// DispIntf:  ILocationNodeManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {453CEF6E-19F1-4CC1-8564-40EF48A4FD21}
// *********************************************************************//
  ILocationNodeManagerDisp = dispinterface
    ['{453CEF6E-19F1-4CC1-8564-40EF48A4FD21}']
    function AddNode(TypeID: Integer; const ParentItemKey: WideString; 
                     var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant): WordBool; dispid 201;
    function GetAddSubType(ParentTypeID: Integer; Index: Integer; var TypeID: Integer): WordBool; dispid 204;
    function GetAddType(ParentTypeID: Integer; Index: Integer; var TypeID: Integer): WordBool; dispid 205;
    function GetData(ParentTypeID: Integer; TypeID: Integer; const ParentKey: WideString; 
                     var SQL: WideString): WordBool; dispid 206;
    function GetNodeInfo(TypeID: Integer; const DataFields: Fields; 
                         var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant): WordBool; dispid 207;
    function GetSubNodeInfo(ParentTypeID: Integer; Index: Integer; 
                            var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant): WordBool; dispid 208;
    function NodeChanged(const ParentItemKey: WideString; 
                         var NodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant; 
                         var DetailScreenGUID: {NOT_OLEAUTO(TGUID)}OleVariant): WordBool; dispid 209;
    property ImageList: LongWord readonly dispid 210;
    property StateImageList: LongWord readonly dispid 211;
    property TypeCount: Integer readonly dispid 212;
    property TypeID[Index: Integer]: Integer readonly dispid 213;
    property TypeName[TypeID: Integer]: WideString readonly dispid 214;
  end;

// *********************************************************************//
// Interface: IDynamicMenu
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6B5606DA-3664-4065-9B66-EED00EBB53FF}
// *********************************************************************//
  IDynamicMenu = interface(IDispatch)
    ['{6B5606DA-3664-4065-9B66-EED00EBB53FF}']
    function Execute: IUnknown; safecall;
    function Get_ChildCount: Integer; safecall;
    function Child(Index: Integer): IUnknown; safecall;
    procedure GetItemData(var ActionData: TActionData); safecall;
    function Get_HasSubmenu: WordBool; safecall;
    property ChildCount: Integer read Get_ChildCount;
    property HasSubmenu: WordBool read Get_HasSubmenu;
  end;

// *********************************************************************//
// DispIntf:  IDynamicMenuDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6B5606DA-3664-4065-9B66-EED00EBB53FF}
// *********************************************************************//
  IDynamicMenuDisp = dispinterface
    ['{6B5606DA-3664-4065-9B66-EED00EBB53FF}']
    function Execute: IUnknown; dispid 201;
    property ChildCount: Integer readonly dispid 204;
    function Child(Index: Integer): IUnknown; dispid 203;
    procedure GetItemData(var ActionData: {NOT_OLEAUTO(TActionData)}OleVariant); dispid 202;
    property HasSubmenu: WordBool readonly dispid 205;
  end;

// *********************************************************************//
// Interface: IDynamicMenuList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A8B79E68-7426-4B99-BBF3-AC5DAC8E7F01}
// *********************************************************************//
  IDynamicMenuList = interface(IDispatch)
    ['{A8B79E68-7426-4B99-BBF3-AC5DAC8E7F01}']
    function Get_Count: Integer; safecall;
    function Items(AIndex: Integer): IDynamicMenu; safecall;
    function Get_ImageListHandle: Integer; safecall;
    function Get_MenuPath(AIndex: Integer): WideString; safecall;
    function Get_InsertBeforeMenu(AIndex: Integer): WideString; safecall;
    function Get_InsertAfterMenu(AIndex: Integer): WideString; safecall;
    property Count: Integer read Get_Count;
    property ImageListHandle: Integer read Get_ImageListHandle;
    property MenuPath[AIndex: Integer]: WideString read Get_MenuPath;
    property InsertBeforeMenu[AIndex: Integer]: WideString read Get_InsertBeforeMenu;
    property InsertAfterMenu[AIndex: Integer]: WideString read Get_InsertAfterMenu;
  end;

// *********************************************************************//
// DispIntf:  IDynamicMenuListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A8B79E68-7426-4B99-BBF3-AC5DAC8E7F01}
// *********************************************************************//
  IDynamicMenuListDisp = dispinterface
    ['{A8B79E68-7426-4B99-BBF3-AC5DAC8E7F01}']
    property Count: Integer readonly dispid 201;
    function Items(AIndex: Integer): IDynamicMenu; dispid 202;
    property ImageListHandle: Integer readonly dispid 205;
    property MenuPath[AIndex: Integer]: WideString readonly dispid 203;
    property InsertBeforeMenu[AIndex: Integer]: WideString readonly dispid 204;
    property InsertAfterMenu[AIndex: Integer]: WideString readonly dispid 206;
  end;

// *********************************************************************//
// Interface: IRecorderDetailScreen
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A40C35D0-0E71-4312-B2DB-E02B42C0E49C}
// *********************************************************************//
  IRecorderDetailScreen = interface(IDispatch)
    ['{A40C35D0-0E71-4312-B2DB-E02B42C0E49C}']
    procedure EditRecord; safecall;
    procedure DeleteRecord; safecall;
    function Get_Editing: WordBool; safecall;
    function Get_Events: IRecorderDetailScreenEvents; safecall;
    procedure Set_Events(const Value: IRecorderDetailScreenEvents); safecall;
    procedure LoadContent(const TypeName: WideString; const ParentItemKey: WideString; 
                          const ItemKey: WideString); safecall;
    function Get_SelectedItemCaption: WideString; safecall;
    procedure Set_SelectedItemCaption(const Value: WideString); safecall;
    function Get_CanClose: WordBool; safecall;
    property Editing: WordBool read Get_Editing;
    property Events: IRecorderDetailScreenEvents read Get_Events write Set_Events;
    property SelectedItemCaption: WideString read Get_SelectedItemCaption write Set_SelectedItemCaption;
    property CanClose: WordBool read Get_CanClose;
  end;

// *********************************************************************//
// DispIntf:  IRecorderDetailScreenDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A40C35D0-0E71-4312-B2DB-E02B42C0E49C}
// *********************************************************************//
  IRecorderDetailScreenDisp = dispinterface
    ['{A40C35D0-0E71-4312-B2DB-E02B42C0E49C}']
    procedure EditRecord; dispid 202;
    procedure DeleteRecord; dispid 203;
    property Editing: WordBool readonly dispid 204;
    property Events: IRecorderDetailScreenEvents dispid 205;
    procedure LoadContent(const TypeName: WideString; const ParentItemKey: WideString; 
                          const ItemKey: WideString); dispid 206;
    property SelectedItemCaption: WideString dispid 207;
    property CanClose: WordBool readonly dispid 201;
  end;

// *********************************************************************//
// Interface: IRecorderDetailScreenEvents
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {05129295-B0FD-416C-BBF4-94A5BDBA317E}
// *********************************************************************//
  IRecorderDetailScreenEvents = interface(IDispatch)
    ['{05129295-B0FD-416C-BBF4-94A5BDBA317E}']
    procedure OnEditModeChange; safecall;
    procedure OnRefreshScreenInfo(const Key: WideString; const Caption: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IRecorderDetailScreenEventsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {05129295-B0FD-416C-BBF4-94A5BDBA317E}
// *********************************************************************//
  IRecorderDetailScreenEventsDisp = dispinterface
    ['{05129295-B0FD-416C-BBF4-94A5BDBA317E}']
    procedure OnEditModeChange; dispid 201;
    procedure OnRefreshScreenInfo(const Key: WideString; const Caption: WideString); dispid 202;
  end;

// *********************************************************************//
// Interface: IDisplayData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {16B1083A-DCDE-46C4-9BEA-4B03ADEBCEA7}
// *********************************************************************//
  IDisplayData = interface(IDispatch)
    ['{16B1083A-DCDE-46C4-9BEA-4B03ADEBCEA7}']
    procedure DisplayData(const AKeyList: IKeyList); safecall;
    function SupportsTable(const ATableName: WideString): WordBool; safecall;
  end;

// *********************************************************************//
// DispIntf:  IDisplayDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {16B1083A-DCDE-46C4-9BEA-4B03ADEBCEA7}
// *********************************************************************//
  IDisplayDataDisp = dispinterface
    ['{16B1083A-DCDE-46C4-9BEA-4B03ADEBCEA7}']
    procedure DisplayData(const AKeyList: IKeyList); dispid 201;
    function SupportsTable(const ATableName: WideString): WordBool; dispid 202;
  end;

// *********************************************************************//
// Interface: IMapWindowSelector
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CC73F7AB-2171-4AA9-AFEC-152665273090}
// *********************************************************************//
  IMapWindowSelector = interface(IDispatch)
    ['{CC73F7AB-2171-4AA9-AFEC-152665273090}']
    procedure UpdateMapWindowSelector; safecall;
  end;

// *********************************************************************//
// DispIntf:  IMapWindowSelectorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CC73F7AB-2171-4AA9-AFEC-152665273090}
// *********************************************************************//
  IMapWindowSelectorDisp = dispinterface
    ['{CC73F7AB-2171-4AA9-AFEC-152665273090}']
    procedure UpdateMapWindowSelector; dispid 201;
  end;

// *********************************************************************//
// Interface: IAvailableMap
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8889F4D5-739C-4E97-B74B-2C7220DD0AE9}
// *********************************************************************//
  IAvailableMap = interface(IDispatch)
    ['{8889F4D5-739C-4E97-B74B-2C7220DD0AE9}']
    function Get_Title: WideString; safecall;
    function Get_Key: WideString; safecall;
    function Get_IsDefault: WordBool; safecall;
    function Get_SpatialSystem: WideString; safecall;
    procedure Display; safecall;
    procedure DisplayDistributionPoints(const ADatasetTitle: WideString; 
                                        const AMapPoints: IMapDropFormat); safecall;
    property Title: WideString read Get_Title;
    property Key: WideString read Get_Key;
    property IsDefault: WordBool read Get_IsDefault;
    property SpatialSystem: WideString read Get_SpatialSystem;
  end;

// *********************************************************************//
// DispIntf:  IAvailableMapDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8889F4D5-739C-4E97-B74B-2C7220DD0AE9}
// *********************************************************************//
  IAvailableMapDisp = dispinterface
    ['{8889F4D5-739C-4E97-B74B-2C7220DD0AE9}']
    property Title: WideString readonly dispid 201;
    property Key: WideString readonly dispid 202;
    property IsDefault: WordBool readonly dispid 203;
    property SpatialSystem: WideString readonly dispid 204;
    procedure Display; dispid 205;
    procedure DisplayDistributionPoints(const ADatasetTitle: WideString; 
                                        const AMapPoints: IMapDropFormat); dispid 206;
  end;

// *********************************************************************//
// Interface: IRecorder6
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6443B816-0A35-428E-9A14-C1062B12B526}
// *********************************************************************//
  IRecorder6 = interface(IRecorder2000)
    ['{6443B816-0A35-428E-9A14-C1062B12B526}']
    procedure DisplayDataFromSQL(const ADataType: WideString; const ASQL: WideString); safecall;
    procedure DisplayTab(const ADataType: WideString; const ASQL: WideString; 
                         const AItemSQL: WideString; const ATab: WideString; AExpand: WordBool; 
                         AExpandRecurse: WordBool); safecall;
    function GetAvailableXmlReports(const AKeytype: WideString): IXmlReportList; safecall;
  end;

// *********************************************************************//
// DispIntf:  IRecorder6Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6443B816-0A35-428E-9A14-C1062B12B526}
// *********************************************************************//
  IRecorder6Disp = dispinterface
    ['{6443B816-0A35-428E-9A14-C1062B12B526}']
    procedure DisplayDataFromSQL(const ADataType: WideString; const ASQL: WideString); dispid 301;
    procedure DisplayTab(const ADataType: WideString; const ASQL: WideString; 
                         const AItemSQL: WideString; const ATab: WideString; AExpand: WordBool; 
                         AExpandRecurse: WordBool); dispid 302;
    function GetAvailableXmlReports(const AKeytype: WideString): IXmlReportList; dispid 303;
    property CurrentSettings: ICurrentSettings6 readonly dispid 1;
    property Rucksack: IRucksack readonly dispid 2;
    property JNCCFormat: Integer readonly dispid 3;
    procedure MenuOptionClick(const iItemName: WideString); dispid 4;
    procedure DisplayData(const iDataType: WideString; const iItems: IKeyList); dispid 5;
    procedure RequestData(const Requestor: IRequestor; const DataType: WideString); dispid 6;
    function ShowActiveForm(iForm: {NOT_OLEAUTO(TGUID)}OleVariant): IUnknown; dispid 7;
    function Find(const iTitle: WideString; const iType: WideString; const iInitialText: WideString): WideString; dispid 8;
    function GetNextKey(const iTableName: WideString): WideString; dispid 9;
    procedure MergeDatabase(const iDatabasePath: WideString); dispid 10;
    property RecorderFunctions: IRecorderFunctions readonly dispid 12;
    procedure CheckDatabase(const iPath: WideString; const iDetails: WideString); dispid 11;
    property SpatialRefSystem: WideString readonly dispid 13;
    property RecorderMainForm: IRecorderMainForm readonly dispid 14;
    property CanExportSystemSupplied: WordBool dispid 16;
    property RecorderMap: IRecorderMap readonly dispid 17;
    property Version: WideString readonly dispid 15;
    function AccessSQLtoSQLServerSQL(const iSQLStatement: WideString): WideString; dispid 201;
    property ConnectionString: WideString readonly dispid 202;
    procedure SetApplicationSecurity(const Connection: IUnknown); dispid 203;
    procedure ExportKeyList(const KeyList: IKeyList; const Destination: WideString; 
                            const _Type: WideString); dispid 204;
    property ReportResults: IReportResults readonly dispid 205;
    function RequestCOMData(const ARequestor: IRequestor; 
                            ASupplierGUID: {NOT_OLEAUTO(TGUID)}OleVariant): IUnknown; dispid 206;
  end;

// *********************************************************************//
// Interface: ISpatialReference6
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5EB0BFBD-A61F-4F11-8FBD-64A2643FB1D4}
// *********************************************************************//
  ISpatialReference6 = interface(ISpatialReference)
    ['{5EB0BFBD-A61F-4F11-8FBD-64A2643FB1D4}']
    function Get_EquivalentBaseMapSystem: WideString; safecall;
    property EquivalentBaseMapSystem: WideString read Get_EquivalentBaseMapSystem;
  end;

// *********************************************************************//
// DispIntf:  ISpatialReference6Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5EB0BFBD-A61F-4F11-8FBD-64A2643FB1D4}
// *********************************************************************//
  ISpatialReference6Disp = dispinterface
    ['{5EB0BFBD-A61F-4F11-8FBD-64A2643FB1D4}']
    property EquivalentBaseMapSystem: WideString readonly dispid 301;
    property SpatialRefSystem: WideString readonly dispid 1;
    function ConvertToLat(const iSpatialRef: WideString): WideString; dispid 2;
    function ConvertToLong(const iSpatialRef: WideString): WideString; dispid 3;
    function ConvertFromLatLong(const iLat: WideString; const iLong: WideString): WideString; dispid 4;
    property GetLastError: WideString readonly dispid 5;
    function ValidSpatialRef(const iSpatialRef: WideString): WordBool; dispid 7;
  end;

// *********************************************************************//
// Interface: IGridSystem
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {28871FA6-4CEB-44A3-8901-DA602F13382C}
// *********************************************************************//
  IGridSystem = interface(IDispatch)
    ['{28871FA6-4CEB-44A3-8901-DA602F13382C}']
    procedure InitialiseGridSquareRange(const SWCorner: WideString; const NECorner: WideString); safecall;
    function Get_GridSquareRangeCount: Integer; safecall;
    function Get_GridSquareRangeItem(Index: Integer): WideString; safecall;
    function ReduceGridSquarePrecision(const AGridSquare: WideString; APrecision: Integer): WideString; safecall;
    function GridSquareAsENBox(const GridSquare: WideString): IENBox; safecall;
    property GridSquareRangeCount: Integer read Get_GridSquareRangeCount;
    property GridSquareRangeItem[Index: Integer]: WideString read Get_GridSquareRangeItem;
  end;

// *********************************************************************//
// DispIntf:  IGridSystemDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {28871FA6-4CEB-44A3-8901-DA602F13382C}
// *********************************************************************//
  IGridSystemDisp = dispinterface
    ['{28871FA6-4CEB-44A3-8901-DA602F13382C}']
    procedure InitialiseGridSquareRange(const SWCorner: WideString; const NECorner: WideString); dispid 201;
    property GridSquareRangeCount: Integer readonly dispid 202;
    property GridSquareRangeItem[Index: Integer]: WideString readonly dispid 203;
    function ReduceGridSquarePrecision(const AGridSquare: WideString; APrecision: Integer): WideString; dispid 204;
    function GridSquareAsENBox(const GridSquare: WideString): IENBox; dispid 205;
  end;

// *********************************************************************//
// Interface: IENBox
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {766E6BBB-25A8-4532-AE4C-F188357159E5}
// *********************************************************************//
  IENBox = interface(IDispatch)
    ['{766E6BBB-25A8-4532-AE4C-F188357159E5}']
    function Get_SWE: Integer; safecall;
    function Get_SWN: Integer; safecall;
    function Get_NEE: Integer; safecall;
    function Get_NEN: Integer; safecall;
    function Get_System_: WideString; safecall;
    property SWE: Integer read Get_SWE;
    property SWN: Integer read Get_SWN;
    property NEE: Integer read Get_NEE;
    property NEN: Integer read Get_NEN;
    property System_: WideString read Get_System_;
  end;

// *********************************************************************//
// DispIntf:  IENBoxDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {766E6BBB-25A8-4532-AE4C-F188357159E5}
// *********************************************************************//
  IENBoxDisp = dispinterface
    ['{766E6BBB-25A8-4532-AE4C-F188357159E5}']
    property SWE: Integer readonly dispid 201;
    property SWN: Integer readonly dispid 202;
    property NEE: Integer readonly dispid 203;
    property NEN: Integer readonly dispid 204;
    property System_: WideString readonly dispid 205;
  end;

// *********************************************************************//
// Interface: IAdditionalPage6
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2865ED1F-02BD-4CAA-BFCC-78C79726BF08}
// *********************************************************************//
  IAdditionalPage6 = interface(IAdditionalPage)
    ['{2865ED1F-02BD-4CAA-BFCC-78C79726BF08}']
    function Get_TabIndex: Integer; safecall;
    property TabIndex: Integer read Get_TabIndex;
  end;

// *********************************************************************//
// DispIntf:  IAdditionalPage6Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2865ED1F-02BD-4CAA-BFCC-78C79726BF08}
// *********************************************************************//
  IAdditionalPage6Disp = dispinterface
    ['{2865ED1F-02BD-4CAA-BFCC-78C79726BF08}']
    property TabIndex: Integer readonly dispid 301;
    property Form: WideString readonly dispid 1;
    property PageCaption: WideString readonly dispid 2;
    property CurrentTable: WideString dispid 5;
    property CurrentKey: WideString dispid 6;
  end;

// *********************************************************************//
// Interface: ISpatialReferenceList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {326DE097-273F-44E4-AB00-4355978B8660}
// *********************************************************************//
  ISpatialReferenceList = interface(IDispatch)
    ['{326DE097-273F-44E4-AB00-4355978B8660}']
    function Get_SystemCount: Integer; safecall;
    function Get_SystemInterface(Index: Integer): ISpatialReference; safecall;
    property SystemCount: Integer read Get_SystemCount;
    property SystemInterface[Index: Integer]: ISpatialReference read Get_SystemInterface;
  end;

// *********************************************************************//
// DispIntf:  ISpatialReferenceListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {326DE097-273F-44E4-AB00-4355978B8660}
// *********************************************************************//
  ISpatialReferenceListDisp = dispinterface
    ['{326DE097-273F-44E4-AB00-4355978B8660}']
    property SystemCount: Integer readonly dispid 201;
    property SystemInterface[Index: Integer]: ISpatialReference readonly dispid 202;
  end;

// *********************************************************************//
// Interface: IBaseMapFormatList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E451A16A-6108-4DF4-AEE5-785DFF49F7D4}
// *********************************************************************//
  IBaseMapFormatList = interface(IDispatch)
    ['{E451A16A-6108-4DF4-AEE5-785DFF49F7D4}']
    function Get_Count: Integer; safecall;
    function Get_SystemBaseMapFormat(Index: Integer): IBaseMapFormat; safecall;
    property Count: Integer read Get_Count;
    property SystemBaseMapFormat[Index: Integer]: IBaseMapFormat read Get_SystemBaseMapFormat;
  end;

// *********************************************************************//
// DispIntf:  IBaseMapFormatListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E451A16A-6108-4DF4-AEE5-785DFF49F7D4}
// *********************************************************************//
  IBaseMapFormatListDisp = dispinterface
    ['{E451A16A-6108-4DF4-AEE5-785DFF49F7D4}']
    property Count: Integer readonly dispid 201;
    property SystemBaseMapFormat[Index: Integer]: IBaseMapFormat readonly dispid 202;
  end;

// *********************************************************************//
// Interface: INamedSpatialReference
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {71ED641E-5401-4896-8BE8-E18F804332D7}
// *********************************************************************//
  INamedSpatialReference = interface(ISpatialReference)
    ['{71ED641E-5401-4896-8BE8-E18F804332D7}']
    function Get_SpatialSystemName: WideString; safecall;
    property SpatialSystemName: WideString read Get_SpatialSystemName;
  end;

// *********************************************************************//
// DispIntf:  INamedSpatialReferenceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {71ED641E-5401-4896-8BE8-E18F804332D7}
// *********************************************************************//
  INamedSpatialReferenceDisp = dispinterface
    ['{71ED641E-5401-4896-8BE8-E18F804332D7}']
    property SpatialSystemName: WideString readonly dispid 401;
    property SpatialRefSystem: WideString readonly dispid 1;
    function ConvertToLat(const iSpatialRef: WideString): WideString; dispid 2;
    function ConvertToLong(const iSpatialRef: WideString): WideString; dispid 3;
    function ConvertFromLatLong(const iLat: WideString; const iLong: WideString): WideString; dispid 4;
    property GetLastError: WideString readonly dispid 5;
    function ValidSpatialRef(const iSpatialRef: WideString): WordBool; dispid 7;
  end;

// *********************************************************************//
// Interface: IBaseMapScaleList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {85B8381E-810A-42D0-B9D9-668F0612AF70}
// *********************************************************************//
  IBaseMapScaleList = interface(IBaseMapFormat)
    ['{85B8381E-810A-42D0-B9D9-668F0612AF70}']
    function Get_GridScaleCount: Integer; safecall;
    function Get_GridScaleCaption(Index: Integer): WideString; safecall;
    function Get_GridScaleX(Index: Integer): Double; safecall;
    function Get_GridScaleY(Index: Integer): Double; safecall;
    function Get_PointSizeCount: Integer; safecall;
    function Get_PointSizeCaption(Index: Integer): WideString; safecall;
    function Get_PointSizeX(Index: Integer): Double; safecall;
    function Get_PointSizeY(Index: Integer): Double; safecall;
    property GridScaleCount: Integer read Get_GridScaleCount;
    property GridScaleCaption[Index: Integer]: WideString read Get_GridScaleCaption;
    property GridScaleX[Index: Integer]: Double read Get_GridScaleX;
    property GridScaleY[Index: Integer]: Double read Get_GridScaleY;
    property PointSizeCount: Integer read Get_PointSizeCount;
    property PointSizeCaption[Index: Integer]: WideString read Get_PointSizeCaption;
    property PointSizeX[Index: Integer]: Double read Get_PointSizeX;
    property PointSizeY[Index: Integer]: Double read Get_PointSizeY;
  end;

// *********************************************************************//
// DispIntf:  IBaseMapScaleListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {85B8381E-810A-42D0-B9D9-668F0612AF70}
// *********************************************************************//
  IBaseMapScaleListDisp = dispinterface
    ['{85B8381E-810A-42D0-B9D9-668F0612AF70}']
    property GridScaleCount: Integer readonly dispid 301;
    property GridScaleCaption[Index: Integer]: WideString readonly dispid 302;
    property GridScaleX[Index: Integer]: Double readonly dispid 303;
    property GridScaleY[Index: Integer]: Double readonly dispid 304;
    property PointSizeCount: Integer readonly dispid 305;
    property PointSizeCaption[Index: Integer]: WideString readonly dispid 306;
    property PointSizeX[Index: Integer]: Double readonly dispid 307;
    property PointSizeY[Index: Integer]: Double readonly dispid 308;
    function MapCoordToLat(iEasting: Double; iNorthing: Double): WideString; dispid 1;
    function MapCoordToLong(iEasting: Double; iNorthing: Double): WideString; dispid 2;
    property CanDisplayGrids: WordBool readonly dispid 5;
    property MapCoordsPerMetre: Double readonly dispid 6;
    function LatLongToMapCoordX(iLat: Double; iLong: Double): Double; dispid 3;
    function LatLongToMapCoordY(iLat: Double; iLong: Double): Double; dispid 4;
  end;

// *********************************************************************//
// Interface: IReportKeytype
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5757D4C-994D-42A6-A1A0-19EB849828CF}
// *********************************************************************//
  IReportKeytype = interface(IDispatch)
    ['{F5757D4C-994D-42A6-A1A0-19EB849828CF}']
    function Get_Name: WideString; safecall;
    function Get_Multivalue: WordBool; safecall;
    property Name: WideString read Get_Name;
    property Multivalue: WordBool read Get_Multivalue;
  end;

// *********************************************************************//
// DispIntf:  IReportKeytypeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5757D4C-994D-42A6-A1A0-19EB849828CF}
// *********************************************************************//
  IReportKeytypeDisp = dispinterface
    ['{F5757D4C-994D-42A6-A1A0-19EB849828CF}']
    property Name: WideString readonly dispid 201;
    property Multivalue: WordBool readonly dispid 202;
  end;

// *********************************************************************//
// Interface: IReportKeytypeList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4DB607C0-F61D-474C-80BE-A705C6ADA1D9}
// *********************************************************************//
  IReportKeytypeList = interface(IDispatch)
    ['{4DB607C0-F61D-474C-80BE-A705C6ADA1D9}']
    function Get_KeytypeCount: Integer; safecall;
    function Get_Keytype(Index: Integer): IReportKeytype; safecall;
    property KeytypeCount: Integer read Get_KeytypeCount;
    property Keytype[Index: Integer]: IReportKeytype read Get_Keytype;
  end;

// *********************************************************************//
// DispIntf:  IReportKeytypeListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4DB607C0-F61D-474C-80BE-A705C6ADA1D9}
// *********************************************************************//
  IReportKeytypeListDisp = dispinterface
    ['{4DB607C0-F61D-474C-80BE-A705C6ADA1D9}']
    property KeytypeCount: Integer readonly dispid 201;
    property Keytype[Index: Integer]: IReportKeytype readonly dispid 202;
  end;

// *********************************************************************//
// Interface: IXmlReport
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {83F33C13-CF9F-448E-A3D1-A4EC914A85F3}
// *********************************************************************//
  IXmlReport = interface(IDispatch)
    ['{83F33C13-CF9F-448E-A3D1-A4EC914A85F3}']
    function Get_ReportTitle: WideString; safecall;
    function Get_ReportPath: WideString; safecall;
    function Execute(const AKeyList: IKeyList): WordBool; safecall;
    property ReportTitle: WideString read Get_ReportTitle;
    property ReportPath: WideString read Get_ReportPath;
  end;

// *********************************************************************//
// DispIntf:  IXmlReportDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {83F33C13-CF9F-448E-A3D1-A4EC914A85F3}
// *********************************************************************//
  IXmlReportDisp = dispinterface
    ['{83F33C13-CF9F-448E-A3D1-A4EC914A85F3}']
    property ReportTitle: WideString readonly dispid 201;
    property ReportPath: WideString readonly dispid 202;
    function Execute(const AKeyList: IKeyList): WordBool; dispid 203;
  end;

// *********************************************************************//
// Interface: IXmlReportList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7843E83E-A9F1-4596-8DF8-BC34F15C5DEF}
// *********************************************************************//
  IXmlReportList = interface(IDispatch)
    ['{7843E83E-A9F1-4596-8DF8-BC34F15C5DEF}']
    function Get_ReportCount: Integer; safecall;
    function Get_Report(Index: Integer): IXmlReport; safecall;
    property ReportCount: Integer read Get_ReportCount;
    property Report[Index: Integer]: IXmlReport read Get_Report;
  end;

// *********************************************************************//
// DispIntf:  IXmlReportListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7843E83E-A9F1-4596-8DF8-BC34F15C5DEF}
// *********************************************************************//
  IXmlReportListDisp = dispinterface
    ['{7843E83E-A9F1-4596-8DF8-BC34F15C5DEF}']
    property ReportCount: Integer readonly dispid 201;
    property Report[Index: Integer]: IXmlReport readonly dispid 202;
  end;

// *********************************************************************//
// Interface: IValidation6
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6496C721-70E5-41F8-957A-6F95BC552EA0}
// *********************************************************************//
  IValidation6 = interface(IValidation)
    ['{6496C721-70E5-41F8-957A-6F95BC552EA0}']
    function ValidateAll(const Connection: _Connection): Integer; safecall;
    function ValidateKeyList(const Connection: _Connection; const KeyList: IKeyList): Integer; safecall;
    procedure Cancel; safecall;
  end;

// *********************************************************************//
// DispIntf:  IValidation6Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6496C721-70E5-41F8-957A-6F95BC552EA0}
// *********************************************************************//
  IValidation6Disp = dispinterface
    ['{6496C721-70E5-41F8-957A-6F95BC552EA0}']
    function ValidateAll(const Connection: _Connection): Integer; dispid 301;
    function ValidateKeyList(const Connection: _Connection; const KeyList: IKeyList): Integer; dispid 302;
    procedure Cancel; dispid 303;
    function Validate(const iPath: WideString): Integer; dispid 1;
    property KeyList: IKeyList readonly dispid 2;
    property ErrorString[iIndex: Integer]: WideString readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IExportFilter6
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DDB265FD-0D72-4527-A3D6-BE7278002A6D}
// *********************************************************************//
  IExportFilter6 = interface(IExportFilter)
    ['{DDB265FD-0D72-4527-A3D6-BE7278002A6D}']
    function ExportFileWithFilteredInvalids(const iItemsToExport: IKeyList; 
                                            const iInvalidItems: IKeyList; 
                                            const iDestinationFile: WideString): WordBool; safecall;
  end;

// *********************************************************************//
// DispIntf:  IExportFilter6Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DDB265FD-0D72-4527-A3D6-BE7278002A6D}
// *********************************************************************//
  IExportFilter6Disp = dispinterface
    ['{DDB265FD-0D72-4527-A3D6-BE7278002A6D}']
    function ExportFileWithFilteredInvalids(const iItemsToExport: IKeyList; 
                                            const iInvalidItems: IKeyList; 
                                            const iDestinationFile: WideString): WordBool; dispid 301;
    function ExportFile(const iItemsToExport: IKeyList; const iDestinationFile: WideString): WordBool; dispid 2;
    property LastExportError: WideString readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IBaseMapScaleList_614
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {70FA545C-9DE4-494E-AB85-E04C5FCEB7D8}
// *********************************************************************//
  IBaseMapScaleList_614 = interface(IBaseMapScaleList)
    ['{70FA545C-9DE4-494E-AB85-E04C5FCEB7D8}']
    function Get_GridOffsetX(Index: Integer): Double; safecall;
    function Get_GridOffsetY(Index: Integer): Double; safecall;
    property GridOffsetX[Index: Integer]: Double read Get_GridOffsetX;
    property GridOffsetY[Index: Integer]: Double read Get_GridOffsetY;
  end;

// *********************************************************************//
// DispIntf:  IBaseMapScaleList_614Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {70FA545C-9DE4-494E-AB85-E04C5FCEB7D8}
// *********************************************************************//
  IBaseMapScaleList_614Disp = dispinterface
    ['{70FA545C-9DE4-494E-AB85-E04C5FCEB7D8}']
    property GridOffsetX[Index: Integer]: Double readonly dispid 401;
    property GridOffsetY[Index: Integer]: Double readonly dispid 402;
    property GridScaleCount: Integer readonly dispid 301;
    property GridScaleCaption[Index: Integer]: WideString readonly dispid 302;
    property GridScaleX[Index: Integer]: Double readonly dispid 303;
    property GridScaleY[Index: Integer]: Double readonly dispid 304;
    property PointSizeCount: Integer readonly dispid 305;
    property PointSizeCaption[Index: Integer]: WideString readonly dispid 306;
    property PointSizeX[Index: Integer]: Double readonly dispid 307;
    property PointSizeY[Index: Integer]: Double readonly dispid 308;
    function MapCoordToLat(iEasting: Double; iNorthing: Double): WideString; dispid 1;
    function MapCoordToLong(iEasting: Double; iNorthing: Double): WideString; dispid 2;
    property CanDisplayGrids: WordBool readonly dispid 5;
    property MapCoordsPerMetre: Double readonly dispid 6;
    function LatLongToMapCoordX(iLat: Double; iLong: Double): Double; dispid 3;
    function LatLongToMapCoordY(iLat: Double; iLong: Double): Double; dispid 4;
  end;

// *********************************************************************//
// Interface: INodeMenuManager
// Flags:     (320) Dual OleAutomation
// GUID:      {6ED9E090-E055-4F58-87F2-412C6CD50379}
// *********************************************************************//
  INodeMenuManager = interface(IUnknown)
    ['{6ED9E090-E055-4F58-87F2-412C6CD50379}']
    function GetMenuItemsForNode(ANodeInfo: TNodeInfo): IDynamicMenuList; safecall;
  end;

// *********************************************************************//
// DispIntf:  INodeMenuManagerDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {6ED9E090-E055-4F58-87F2-412C6CD50379}
// *********************************************************************//
  INodeMenuManagerDisp = dispinterface
    ['{6ED9E090-E055-4F58-87F2-412C6CD50379}']
    function GetMenuItemsForNode(ANodeInfo: {NOT_OLEAUTO(TNodeInfo)}OleVariant): IDynamicMenuList; dispid 101;
  end;

// *********************************************************************//
// Interface: ISpatialRefControl
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {57D0741A-38CE-4B16-9274-3B3AC44AB205}
// *********************************************************************//
  ISpatialRefControl = interface(IDispatch)
    ['{57D0741A-38CE-4B16-9274-3B3AC44AB205}']
    function Get_EnteredRef: WideString; safecall;
    procedure Set_EnteredRef(const Value: WideString); safecall;
    function Get_EnteredSystem: WideString; safecall;
    procedure Set_EnteredSystem(const Value: WideString); safecall;
    function Get_DisplayRef: WideString; safecall;
    procedure Set_DisplayRef(const Value: WideString); safecall;
    function Get_DisplaySystem: WideString; safecall;
    function Get_Qualifier: WideString; safecall;
    procedure Set_Qualifier(const Value: WideString); safecall;
    property EnteredRef: WideString read Get_EnteredRef write Set_EnteredRef;
    property EnteredSystem: WideString read Get_EnteredSystem write Set_EnteredSystem;
    property DisplayRef: WideString read Get_DisplayRef write Set_DisplayRef;
    property DisplaySystem: WideString read Get_DisplaySystem;
    property Qualifier: WideString read Get_Qualifier write Set_Qualifier;
  end;

// *********************************************************************//
// DispIntf:  ISpatialRefControlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {57D0741A-38CE-4B16-9274-3B3AC44AB205}
// *********************************************************************//
  ISpatialRefControlDisp = dispinterface
    ['{57D0741A-38CE-4B16-9274-3B3AC44AB205}']
    property EnteredRef: WideString dispid 201;
    property EnteredSystem: WideString dispid 202;
    property DisplayRef: WideString dispid 203;
    property DisplaySystem: WideString readonly dispid 204;
    property Qualifier: WideString dispid 205;
  end;

// *********************************************************************//
// The Class CoAutoApplicationSettings provides a Create and CreateRemote method to          
// create instances of the default interface IAutoApplicationSettings exposed by              
// the CoClass AutoApplicationSettings. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAutoApplicationSettings = class
    class function Create: IAutoApplicationSettings;
    class function CreateRemote(const MachineName: string): IAutoApplicationSettings;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TAutoApplicationSettings
// Help String      : AutoApplicationSettings Object
// Default Interface: IAutoApplicationSettings
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TAutoApplicationSettings = class(TOleServer)
  private
    FIntf: IAutoApplicationSettings;
    function GetDefaultInterface: IAutoApplicationSettings;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IAutoApplicationSettings);
    procedure Disconnect; override;
    property DefaultInterface: IAutoApplicationSettings read GetDefaultInterface;
  published
  end;

// *********************************************************************//
// The Class CoAvailableMap provides a Create and CreateRemote method to          
// create instances of the default interface IAvailableMap exposed by              
// the CoClass AvailableMap. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAvailableMap = class
    class function Create: IAvailableMap;
    class function CreateRemote(const MachineName: string): IAvailableMap;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TAvailableMap
// Help String      : 
// Default Interface: IAvailableMap
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TAvailableMap = class(TOleServer)
  private
    FIntf: IAvailableMap;
    function GetDefaultInterface: IAvailableMap;
  protected
    procedure InitServerData; override;
    function Get_Title: WideString;
    function Get_Key: WideString;
    function Get_IsDefault: WordBool;
    function Get_SpatialSystem: WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IAvailableMap);
    procedure Disconnect; override;
    procedure Display;
    procedure DisplayDistributionPoints(const ADatasetTitle: WideString; 
                                        const AMapPoints: IMapDropFormat);
    property DefaultInterface: IAvailableMap read GetDefaultInterface;
    property Title: WideString read Get_Title;
    property Key: WideString read Get_Key;
    property IsDefault: WordBool read Get_IsDefault;
    property SpatialSystem: WideString read Get_SpatialSystem;
  published
  end;

procedure Register;

resourcestring
  dtlServerPage = 'Servers';

  dtlOcxPage = 'ActiveX';

implementation

uses System.Win.ComObj;

class function CoAutoApplicationSettings.Create: IAutoApplicationSettings;
begin
  Result := CreateComObject(CLASS_AutoApplicationSettings) as IAutoApplicationSettings;
end;

class function CoAutoApplicationSettings.CreateRemote(const MachineName: string): IAutoApplicationSettings;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AutoApplicationSettings) as IAutoApplicationSettings;
end;

procedure TAutoApplicationSettings.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{801EBE85-91CE-11D3-B564-005004B0B698}';
    IntfIID:   '{801EBE83-91CE-11D3-B564-005004B0B698}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TAutoApplicationSettings.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IAutoApplicationSettings;
  end;
end;

procedure TAutoApplicationSettings.ConnectTo(svrIntf: IAutoApplicationSettings);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TAutoApplicationSettings.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TAutoApplicationSettings.GetDefaultInterface: IAutoApplicationSettings;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TAutoApplicationSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TAutoApplicationSettings.Destroy;
begin
  inherited Destroy;
end;

class function CoAvailableMap.Create: IAvailableMap;
begin
  Result := CreateComObject(CLASS_AvailableMap) as IAvailableMap;
end;

class function CoAvailableMap.CreateRemote(const MachineName: string): IAvailableMap;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AvailableMap) as IAvailableMap;
end;

procedure TAvailableMap.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{3BE5EBCC-EF8A-4DD2-97B4-186EEED886B2}';
    IntfIID:   '{8889F4D5-739C-4E97-B74B-2C7220DD0AE9}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TAvailableMap.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IAvailableMap;
  end;
end;

procedure TAvailableMap.ConnectTo(svrIntf: IAvailableMap);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TAvailableMap.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TAvailableMap.GetDefaultInterface: IAvailableMap;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TAvailableMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TAvailableMap.Destroy;
begin
  inherited Destroy;
end;

function TAvailableMap.Get_Title: WideString;
begin
  Result := DefaultInterface.Title;
end;

function TAvailableMap.Get_Key: WideString;
begin
  Result := DefaultInterface.Key;
end;

function TAvailableMap.Get_IsDefault: WordBool;
begin
  Result := DefaultInterface.IsDefault;
end;

function TAvailableMap.Get_SpatialSystem: WideString;
begin
  Result := DefaultInterface.SpatialSystem;
end;

procedure TAvailableMap.Display;
begin
  DefaultInterface.Display;
end;

procedure TAvailableMap.DisplayDistributionPoints(const ADatasetTitle: WideString; 
                                                  const AMapPoints: IMapDropFormat);
begin
  DefaultInterface.DisplayDistributionPoints(ADatasetTitle, AMapPoints);
end;

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TAutoApplicationSettings, TAvailableMap]);
end;

end.
