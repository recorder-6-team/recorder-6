unit DMAPServer_TLB;

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
// File generated on 04/10/2010 13:37:42 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Source\Recorder\RecorderApp\trunk\Add-ins\DMAP Server\DMAPServer.tlb (1)
// LIBID: {7C15E931-1AA0-42BA-9522-A1AB3491C70B}
// LCID: 0
// Helpfile: 
// HelpString: DMAPServer Library
// DepndLst: 
//   (1) v1.0 Recorder2000, (C:\Program Files\Recorder 6\RecorderApp.exe)
//   (2) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
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
  DMAPServerMajorVersion = 1;
  DMAPServerMinorVersion = 0;

  LIBID_DMAPServer: TGUID = '{7C15E931-1AA0-42BA-9522-A1AB3491C70B}';

  CLASS_Exporter: TGUID = '{F2C05FC5-73B2-4FB7-96F9-792D09F3E029}';
type

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Exporter = IRecorderAddin;


// *********************************************************************//
// The Class CoExporter provides a Create and CreateRemote method to          
// create instances of the default interface IRecorderAddin exposed by              
// the CoClass Exporter. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoExporter = class
    class function Create: IRecorderAddin;
    class function CreateRemote(const MachineName: string): IRecorderAddin;
  end;

implementation

uses ComObj;

class function CoExporter.Create: IRecorderAddin;
begin
  Result := CreateComObject(CLASS_Exporter) as IRecorderAddin;
end;

class function CoExporter.CreateRemote(const MachineName: string): IRecorderAddin;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Exporter) as IRecorderAddin;
end;

end.
