unit LCDmapExport7_TLB;

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
// File generated on 15/11/2014 08:30:23 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\localsource\Recorder\LC Addins\DMAPExtractor\LCDmapExport7.tlb (1)
// LIBID: {9296A9A8-CC14-4D48-97D7-CB59106D6807}
// LCID: 0
// Helpfile: 
// HelpString: LCDmapExport7 Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  LCDmapExport7MajorVersion = 1;
  LCDmapExport7MinorVersion = 0;

  LIBID_LCDmapExport7: TGUID = '{9296A9A8-CC14-4D48-97D7-CB59106D6807}';

  IID_ITLcDmap7: TGUID = '{389A135C-F8F2-4640-82E9-F4F7DABE9B16}';
  CLASS_TLcDmap7: TGUID = '{D92AFE10-32F2-4F6E-AF10-AD6E7CD2E0A1}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ITLcDmap7 = interface;
  ITLcDmap7Disp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  TLcDmap7 = ITLcDmap7;


// *********************************************************************//
// Interface: ITLcDmap7
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {389A135C-F8F2-4640-82E9-F4F7DABE9B16}
// *********************************************************************//
  ITLcDmap7 = interface(IDispatch)
    ['{389A135C-F8F2-4640-82E9-F4F7DABE9B16}']
  end;

// *********************************************************************//
// DispIntf:  ITLcDmap7Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {389A135C-F8F2-4640-82E9-F4F7DABE9B16}
// *********************************************************************//
  ITLcDmap7Disp = dispinterface
    ['{389A135C-F8F2-4640-82E9-F4F7DABE9B16}']
  end;

// *********************************************************************//
// The Class CoTLcDmap7 provides a Create and CreateRemote method to          
// create instances of the default interface ITLcDmap7 exposed by              
// the CoClass TLcDmap7. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTLcDmap7 = class
    class function Create: ITLcDmap7;
    class function CreateRemote(const MachineName: string): ITLcDmap7;
  end;

implementation

uses ComObj;

class function CoTLcDmap7.Create: ITLcDmap7;
begin
  Result := CreateComObject(CLASS_TLcDmap7) as ITLcDmap7;
end;

class function CoTLcDmap7.CreateRemote(const MachineName: string): ITLcDmap7;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TLcDmap7) as ITLcDmap7;
end;

end.
