unit XMLSystems1_TLB;

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
// File generated on 24/09/2015 15:00:51 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Localsource\recorder-6\RecorderApp\Add-ins\XMLSystems\XMLSystems.tlb (1)
// LIBID: {652B8AB8-C580-4613-A327-57F15F7793AC}
// LCID: 0
// Helpfile: 
// HelpString: XMLSystems1 Library
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
  XMLSystems1MajorVersion = 1;
  XMLSystems1MinorVersion = 0;

  LIBID_XMLSystems1: TGUID = '{652B8AB8-C580-4613-A327-57F15F7793AC}';

  IID_IXMLSystems: TGUID = '{42364F7A-049C-4E59-9880-3AC90DFAC1CD}';
  CLASS_XMLSystems: TGUID = '{06009274-ACEC-44F7-BCE8-884F2DA7EB8B}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IXMLSystems = interface;
  IXMLSystemsDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  XMLSystems = IXMLSystems;


// *********************************************************************//
// Interface: IXMLSystems
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {42364F7A-049C-4E59-9880-3AC90DFAC1CD}
// *********************************************************************//
  IXMLSystems = interface(IDispatch)
    ['{42364F7A-049C-4E59-9880-3AC90DFAC1CD}']
  end;

// *********************************************************************//
// DispIntf:  IXMLSystemsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {42364F7A-049C-4E59-9880-3AC90DFAC1CD}
// *********************************************************************//
  IXMLSystemsDisp = dispinterface
    ['{42364F7A-049C-4E59-9880-3AC90DFAC1CD}']
  end;

// *********************************************************************//
// The Class CoXMLSystems provides a Create and CreateRemote method to          
// create instances of the default interface IXMLSystems exposed by              
// the CoClass XMLSystems. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoXMLSystems = class
    class function Create: IXMLSystems;
    class function CreateRemote(const MachineName: string): IXMLSystems;
  end;

implementation

uses ComObj;

class function CoXMLSystems.Create: IXMLSystems;
begin
  Result := CreateComObject(CLASS_XMLSystems) as IXMLSystems;
end;

class function CoXMLSystems.CreateRemote(const MachineName: string): IXMLSystems;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XMLSystems) as IXMLSystems;
end;

end.
