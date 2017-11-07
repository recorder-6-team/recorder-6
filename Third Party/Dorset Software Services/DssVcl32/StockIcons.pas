{===============================================================================
  Unit:         StockIcons

  Defines:      CreateStockIcon

  Description:  Access to Windows stock icons.

                Requires Windows Vista or later.

  Created:      February 2009

  Last revision information:
    $Revision: 2 $
    $Date: 19/02/09 11:12 $
    $Author: Andrewkemp $

  Copyright © Dorset Software Services Ltd, 2009

===============================================================================}
unit StockIcons;

interface

uses
  Graphics;

type
  TStockIconID = (
      siidDocNoAssoc = 0,
      siidDocAssoc = 1,
      siidApplication = 2,
      siidFolder = 3,
      siidFolderOpen = 4,
      siidDrive525 = 5,
      siidDrive35 = 6,
      siidDriveRemove = 7,
      siidDriveFixed = 8,
      siidDriveNet = 9,
      siidDriveNetDisabled = 10,
      siidDriveCD = 11,
      siidDriveRAM = 12,
      siidWorld = 13,
      siidServer = 15,
      siidPrinter = 16,
      siidMyNetwork = 17,
      siidFind = 22,
      siidHelp = 23,
      siidShare = 28,
      siidLink = 29,
      siidSlowFile = 30,
      siidRecycler = 31,
      siidRecyclerFull = 32,
      siidMediaCDAudio = 40,
      siidLock = 47,
      siidAutoList = 49,
      siidPrinterNet = 50,
      siidServerShare = 51,
      siidPrinterFax = 52,
      siidPrinterFaxNet = 53,
      siidPrinterFile = 54,
      siidStack = 55,
      siidMediaSVCD = 56,
      siidStuffedFolder = 57,
      siidDriveUnknown = 58,
      siidDriveDVD = 59,
      siidMediaDVD = 60,
      siidMediaDVDRAM = 61,
      siidMediadDVDRW = 62,
      siidMediaDVDR = 63,
      siidMediaDVDROM = 64,
      siidMediaCDAudioPlus = 65,
      siidMediaCDRW = 66,
      siidMediaCDR = 67,
      siidMediaCDBurn = 68,
      siidMediaBlankCD = 69,
      siidMediaCDROM = 70,
      siidAudioFiles = 71,
      siidImageFiles = 72,
      siidVideoFiles = 73,
      siidMixedFiles = 74,
      siidFolderBack = 75,
      siidFolderFront = 76,
      siidShield = 77,
      siidWarning = 78,
      siidInfo = 79,
      siidError = 80,
      siidKey = 81,
      siidSoftware = 82,
      siidRename = 83,
      siidDelete = 84,
      siidMediaAudiodvd = 85,
      siidMediaMoviedvd = 86,
      siidMediaEnhancedCD = 87,
      siidMediaEnhancedDVD = 88,
      siidMediaHDDVD = 89,
      siidMediaBluray = 90,
      siidMediaVCD = 91,
      siidMediaDVDPlusR = 92,
      siidMediaDVDPlusRW = 93,
      siidDesktopPC = 94,
      siidMobilePC = 95,
      siidUsers = 96,
      siidMediaSmartMedia = 97,
      siidMediaCompactFlash = 98,
      siidDeviceCellPhone = 99,
      siidDeviceCamera = 100,
      siidDeviceVideoCamera = 101,
      siidDeviceAudioPlayer = 102,
      siidNetworkConnect = 103,
      siidInternet = 104,
      siidZipFile = 105,
      siidSettings = 106,
      siidDriveHDDVD = 132,
      siidDriveBD = 133,
      siidMediaHDDVDROM = 134,
      siidMediaHDDVDR = 135,
      siidMediaHDDVDRAM = 136,
      siidMediaBDROM = 137,
      siidMediaBDR = 138,
      siidMediaBDRE = 139,
      siidClusteredDrive = 140);
      
  TStockIconSize = (sisLarge, sisSmall, sisShell);

function CreateStockIcon(
  IconID: TStockIconID;
  Size: TStockIconSize;
  LinkOverlay: Boolean = False): TIcon;


implementation

{$WARN SYMBOL_PLATFORM OFF}

uses
  ActiveX, Windows, ShellAPI, ComObj, SysUtils;

type
  SHSTOCKICONID = TOleEnum;

  TShStockIconInfo = record
    cbSize: DWORD;
    hIcon: HICON;
    iSysImageIndex: Integer;
    iIcon: Integer;
    szPath: array[0..MAX_PATH - 1] of WCHAR;
  end;

  PShStockIconInfo = ^TShStockIconInfo;

  SHGetStockIconInfoFunction = function(
      Siid: SHSTOCKICONID;
      Flags: UINT;
      Psii: PShStockIconInfo): HResult; stdcall;

const
  SHGSI_ICONLOCATION = 0;
  SHGSI_ICON = SHGFI_ICON;
  SHGSI_SYSICONINDEX = SHGFI_SYSICONINDEX;
  SHGSI_LINKOVERLAY = SHGFI_LINKOVERLAY;
  SHGSI_SELECTED = SHGFI_SELECTED;
  SHGSI_LARGEICON = SHGFI_LARGEICON;
  SHGSI_SMALLICON = SHGFI_SMALLICON;
  SHGSI_SHELLICONSIZE = SHGFI_SHELLICONSIZE;

{ ------------------------------------------------------------------------------
  Invokes the Windows Shell API call of the same name.

  This could not be implemented as an external function declaration because
  that would cause dynamic link errors on versions of Windows where the API
  function does not exist (i.e. those earlier than Vista).
}
function SHGetStockIconInfo(
  Siid: SHSTOCKICONID;
  Flags: UINT;
  Psii: PShStockIconInfo): HResult; stdcall;
var
  Module: HMODULE;
  SHGetStockIconInfo: SHGetStockIconInfoFunction;
begin
  Result := E_NOTIMPL;  // error code in case of earlier versions of Windows
  
  Module := LoadLibrary('shell32.dll');
  if Module <> 0 then
  begin
    SHGetStockIconInfo := GetProcAddress(Module, 'SHGetStockIconInfo');
    if Assigned(SHGetStockIconInfo) then
    begin
      Result := SHGetStockIconInfo(Siid, Flags, Psii);
    end;
    Win32Check(FreeLibrary(Module));
  end;
end;

{ ------------------------------------------------------------------------------
  Creates a TIcon containing the specified stock icon.

  Requires Windows Vista, or later.

  Note that the resulting icon canvas may be larger than the requested image
  size.  You can use GetSystemMetrics with SM_CXICON and SM_CYICON or
  SM_CXSMICON and SM_CYSMICON to find the dimensions of the actual image
  on the canvas (for the large and small sizes, respectively).
}
function CreateStockIcon(
  IconID: TStockIconID;
  Size: TStockIconSize;
  LinkOverlay: Boolean): TIcon;
var
  Info: TShStockIconInfo;
  Flags: UINT;
begin
  Flags := SHGSI_ICON;
  if LinkOverlay then Flags := Flags or SHGSI_LINKOVERLAY;
  case Size of
    sisLarge: Flags := Flags or SHGSI_LARGEICON;
    sisSmall: Flags := Flags or SHGSI_SMALLICON;
    sisShell: Flags := Flags or SHGSI_SHELLICONSIZE;
  end;
  Info.cbSize := SizeOf(TShStockIconInfo);
  OleCheck(SHGetStockIconInfo(TOleEnum(IconID), Flags, @Info));
  Result := TIcon.Create();
  try
    Result.Handle := Info.hIcon;  
  except
    Result.Free;
    raise;
  end;
end;

end.
