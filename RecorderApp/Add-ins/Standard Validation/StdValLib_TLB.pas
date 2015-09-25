unit StdValLib_TLB;

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
// File generated on 24/09/2015 14:02:59 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Localsource\recorder-6\RecorderApp\Add-ins\Standard Validation\StdValLib.tlb (1)
// LIBID: {925704D8-0554-411E-A070-D2B00ED7B81B}
// LCID: 0
// Helpfile: 
// HelpString: StdValLib Library
// DepndLst: 
//   (1) v1.0 Recorder2000, (C:\Program Files (x86)\Recorder 6\RecorderApp.exe)
//   (2) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, Recorder2000_TLB, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  StdValLibMajorVersion = 1;
  StdValLibMinorVersion = 0;

  LIBID_StdValLib: TGUID = '{925704D8-0554-411E-A070-D2B00ED7B81B}';

  CLASS_Recorder2000Validation: TGUID = '{1E275EBF-65D1-4AF6-A66E-1C222253A586}';
type

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Recorder2000Validation = IRecorderAddin;


// *********************************************************************//
// The Class CoRecorder2000Validation provides a Create and CreateRemote method to          
// create instances of the default interface IRecorderAddin exposed by              
// the CoClass Recorder2000Validation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRecorder2000Validation = class
    class function Create: IRecorderAddin;
    class function CreateRemote(const MachineName: string): IRecorderAddin;
  end;

implementation

uses ComObj;

class function CoRecorder2000Validation.Create: IRecorderAddin;
begin
  Result := CreateComObject(CLASS_Recorder2000Validation) as IRecorderAddin;
end;

class function CoRecorder2000Validation.CreateRemote(const MachineName: string): IRecorderAddin;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Recorder2000Validation) as IRecorderAddin;
end;

end.
