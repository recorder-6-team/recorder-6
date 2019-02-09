unit NBNExchangeAddinNu_TLB;

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
// File generated on 05/02/2019 18:02:07 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\localsource\recorder-6\LC Addins\NBNExchangeAddinNu\NBNExchangeAddinNu.tlb (1)
// LIBID: {5E3EF605-4561-4828-834A-3EE6AFCEA5D1}
// LCID: 0
// Helpfile: 
// HelpString: NBNExchangeAddinNu Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
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
  NBNExchangeAddinNuMajorVersion = 1;
  NBNExchangeAddinNuMinorVersion = 0;

  LIBID_NBNExchangeAddinNu: TGUID = '{5E3EF605-4561-4828-834A-3EE6AFCEA5D1}';

  IID_INBNExchangeAddinNuImp: TGUID = '{6591C78A-E6AB-4B4A-BA74-43A78F0C2FAC}';
  CLASS_NBNExchangeAddinNuImp: TGUID = '{9EC6EF66-7323-4AB7-8D4B-E4AA18336047}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  INBNExchangeAddinNuImp = interface;
  INBNExchangeAddinNuImpDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  NBNExchangeAddinNuImp = INBNExchangeAddinNuImp;


// *********************************************************************//
// Interface: INBNExchangeAddinNuImp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6591C78A-E6AB-4B4A-BA74-43A78F0C2FAC}
// *********************************************************************//
  INBNExchangeAddinNuImp = interface(IDispatch)
    ['{6591C78A-E6AB-4B4A-BA74-43A78F0C2FAC}']
  end;

// *********************************************************************//
// DispIntf:  INBNExchangeAddinNuImpDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6591C78A-E6AB-4B4A-BA74-43A78F0C2FAC}
// *********************************************************************//
  INBNExchangeAddinNuImpDisp = dispinterface
    ['{6591C78A-E6AB-4B4A-BA74-43A78F0C2FAC}']
  end;

// *********************************************************************//
// The Class CoNBNExchangeAddinNuImp provides a Create and CreateRemote method to          
// create instances of the default interface INBNExchangeAddinNuImp exposed by              
// the CoClass NBNExchangeAddinNuImp. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoNBNExchangeAddinNuImp = class
    class function Create: INBNExchangeAddinNuImp;
    class function CreateRemote(const MachineName: string): INBNExchangeAddinNuImp;
  end;

implementation

uses ComObj;

class function CoNBNExchangeAddinNuImp.Create: INBNExchangeAddinNuImp;
begin
  Result := CreateComObject(CLASS_NBNExchangeAddinNuImp) as INBNExchangeAddinNuImp;
end;

class function CoNBNExchangeAddinNuImp.CreateRemote(const MachineName: string): INBNExchangeAddinNuImp;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NBNExchangeAddinNuImp) as INBNExchangeAddinNuImp;
end;

end.
