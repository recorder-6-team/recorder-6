unit NBNExchange_TLB;

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
// File generated on 14/12/2012 11:18:55 from Type Library described below.

// ************************************************************************  //
// Type Lib: D:\Delphi progs\Recorder 6 addins\NBN Exchange\version_613\NBNExchange (1)
// LIBID: {23D19BC8-BD89-4C37-A774-60061882D2A8}
// LCID: 0
// Helpfile:
// HelpString: NBNExchange Library
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  NBNExchangeMajorVersion = 1;
  NBNExchangeMinorVersion = 0;

  LIBID_NBNExchange: TGUID = '{23D19BC8-BD89-4C37-A774-60061882D2A8}';

  IID_INBNExchangeX: TGUID = '{F44F8662-4AF5-4A38-A11D-933C431E3144}';
  CLASS_NBNExchangeX: TGUID = '{96A6FD19-2227-4BD9-B327-F6C985359068}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  INBNExchangeX = interface;
  INBNExchangeXDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  NBNExchangeX = INBNExchangeX;


// *********************************************************************//
// Interface: INBNExchangeX
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F44F8662-4AF5-4A38-A11D-933C431E3144}
// *********************************************************************//
  INBNExchangeX = interface(IDispatch)
    ['{F44F8662-4AF5-4A38-A11D-933C431E3144}']
  end;

// *********************************************************************//
// DispIntf:  INBNExchangeXDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F44F8662-4AF5-4A38-A11D-933C431E3144}
// *********************************************************************//
  INBNExchangeXDisp = dispinterface
    ['{F44F8662-4AF5-4A38-A11D-933C431E3144}']
  end;

// *********************************************************************//
// The Class CoNBNExchangeX provides a Create and CreateRemote method to
// create instances of the default interface INBNExchangeX exposed by
// the CoClass NBNExchangeX. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoNBNExchangeX = class
    class function Create: INBNExchangeX;
    class function CreateRemote(const MachineName: string): INBNExchangeX;
  end;

implementation

uses System.Win.ComObj;

class function CoNBNExchangeX.Create: INBNExchangeX;
begin
  Result := CreateComObject(CLASS_NBNExchangeX) as INBNExchangeX;
end;

class function CoNBNExchangeX.CreateRemote(const MachineName: string): INBNExchangeX;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NBNExchangeX) as INBNExchangeX;
end;

end.

