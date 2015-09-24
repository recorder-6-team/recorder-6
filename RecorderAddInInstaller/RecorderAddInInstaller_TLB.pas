unit RecorderAddInInstaller_TLB;

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
// File generated on 15/02/2012 09:33:02 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Localsource\Recorder\RecorderAddInInstaller\RecorderAddInInstaller.tlb (1)
// LIBID: {88B2BD02-D46E-445B-99F1-0C0DD2F7A9F5}
// LCID: 0
// Helpfile: 
// HelpString: RecorderAddInInstaller Library
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
  RecorderAddInInstallerMajorVersion = 1;
  RecorderAddInInstallerMinorVersion = 0;

  LIBID_RecorderAddInInstaller: TGUID = '{88B2BD02-D46E-445B-99F1-0C0DD2F7A9F5}';

  IID_IRecorderAddInRegistration: TGUID = '{EBAED27B-A116-400C-8785-3D03E93B7998}';
  CLASS_RecorderAddInRegistration: TGUID = '{4D0275AE-3517-4BC3-95B1-86645CCEB1EC}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IRecorderAddInRegistration = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  RecorderAddInRegistration = IRecorderAddInRegistration;


// *********************************************************************//
// Interface: IRecorderAddInRegistration
// Flags:     (256) OleAutomation
// GUID:      {EBAED27B-A116-400C-8785-3D03E93B7998}
// *********************************************************************//
  IRecorderAddInRegistration = interface(IUnknown)
    ['{EBAED27B-A116-400C-8785-3D03E93B7998}']
    function Install(const FileName: WideString; const Path: WideString; DoCopy: WordBool): HResult; stdcall;
    function Remove(ClassID: TGUID): HResult; stdcall;
    function IsElevated: WordBool; stdcall;
    function Get_Messages(out Value: WideString): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoRecorderAddInRegistration provides a Create and CreateRemote method to          
// create instances of the default interface IRecorderAddInRegistration exposed by              
// the CoClass RecorderAddInRegistration. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRecorderAddInRegistration = class
    class function Create: IRecorderAddInRegistration;
    class function CreateRemote(const MachineName: string): IRecorderAddInRegistration;
  end;

implementation

uses ComObj;

class function CoRecorderAddInRegistration.Create: IRecorderAddInRegistration;
begin
  Result := CreateComObject(CLASS_RecorderAddInRegistration) as IRecorderAddInRegistration;
end;

class function CoRecorderAddInRegistration.CreateRemote(const MachineName: string): IRecorderAddInRegistration;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RecorderAddInRegistration) as IRecorderAddInRegistration;
end;

end.
