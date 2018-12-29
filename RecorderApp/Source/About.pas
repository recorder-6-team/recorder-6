//==============================================================================
//  Unit:        About
//
//  Implements:  TdlgAbout
//
//  Description: About box to display information about the application,
//               version/built number, date of last built. Also displays a list
//               of loaded DLLs and Add-In modules.
//
//  Author:      John van Breda
//  Created:     8 Apr 1999
//
//  Last Revision Details:
//    $Revision: 20 $
//    $Date: 3/04/09 17:02 $
//    $Author: Hainanwang $
//
//  $History: About.pas $
//  
//  *****************  Version 20  *****************
//  User: Hainanwang   Date: 3/04/09    Time: 17:02
//  Updated in $/JNCC/Development/Build/Source
//  VI 18937  (CCN310)
//  Splited the dictionary caption into 2 lines 
//  
//  *****************  Version 19  *****************
//  User: Pauldavies   Date: 23/12/08   Time: 14:32
//  Updated in $/JNCC/Development/Build/Source
//  
//  *****************  Version 18  *****************
//  User: Pauldavies   Date: 23/12/08   Time: 13:53
//  Updated in $/JNCC/Development/Build/Source
//  CCN 310
//  Incident 18364
//  
//  Added a new read only property 'DictionaryVersion' to
//  TApplcationSettings, and a procedure to access it. The About screen
//  uses this property to display the current dictionary version.
//  
//  *****************  Version 17  *****************
//  User: Johnvanbreda Date: 11/02/08   Time: 16:31
//  Updated in $/JNCC/Development/Build/Source
//  CCN248 - F11 hotkey basic work
//  
//  *****************  Version 16  *****************
//  User: Davidkelly   Date: 11/12/07   Time: 15:40
//  Updated in $/JNCC/Development/Build/Source
//  Added a missing semicolon.
//  
//  *****************  Version 15  *****************
//  User: Rickyshrestha Date: 11/12/07   Time: 14:24
//  Updated in $/JNCC/Development/Build/Source
//  Changed one constant to resourcestring (ResStr_XPMenu_Acknowledgement)
//
//  *****************  Version 14  *****************
//  User: Johnvanbreda Date: 17/01/06   Time: 8:52
//  Updated in $/JNCC/Development/Build/Source
//  Added username label
//  
//  *****************  Version 13  *****************
//  User: Johnvanbreda Date: 6/02/03    Time: 13:08
//  Updated in $/JNCC/Source
//  Rebranding
//
//  *****************  Version 12  *****************
//  User: Johnvanbreda Date: 17/07/02   Time: 10:20a
//  Updated in $/JNCC/Source
//  Improved layout of More tab.
//  
//  *****************  Version 11  *****************
//  User: Ericsalmon   Date: 16/07/02   Time: 17:22
//  Updated in $/JNCC/Source
//  Removed GetOSInfo function. Use the updated one in GeneralFunctions
//  instead. 
//  
//  *****************  Version 10  *****************
//  User: Ericsalmon   Date: 19/06/02   Time: 17:30
//  Updated in $/JNCC/Source
//  Replaced BitBtn with ImageListButton
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, VersionInfo, DLLPeek, ComAddInUnit,
  ApplicationSettings, ImageListButton, FormActions, GeneralFunctions;

type
  TdlgAbout = class(TForm)
    pcAbout: TPageControl;
    tsVersion: TTabSheet;
    tsMoreInfo: TTabSheet;
    Label1: TLabel;
    Version: TLabel;
    Label4: TLabel;
    lDate: TLabel;
    Label2: TLabel;
    OS: TLabel;
    Label3: TLabel;
    PhysMem: TLabel;
    Bevel2: TBevel;
    Label6: TLabel;
    StaticText1: TStaticText;
    Label5: TLabel;
    mmMoreInfo: TMemo;
    Image: TImage;
    Label7: TLabel;
    MapServer: TLabel;
    btnOk: TImageListButton;
    Label8: TLabel;
    lblUsername: TLabel;
    lblDictionaryVersionCaption: TLabel;
    lblDictionaryVersion: TLabel;
    lblDictionaryVersionCaption2: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    lblDatabaseVersion: TLabel;
    Label12: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    function GetFileDate(Name: string): string;
    procedure InitializeCaptions;
    procedure InitializeMoreTab;
  public
  end;

//==============================================================================
implementation

{$R *.DFM}

resourcestring
  ResStr_XPMenu_Acknowledgement = 'XP Menu appearance provided by XPMenu component version 2.21 ' +
                                '(http://www.shagrouni.com/english/software/xpmenu.html)'#13#10 +
                                'Copyright (C) 2001, 2002 Khaled Shagrouni.'#13#10;
  ResStr_DLL  ='Dynamic Link Libraries (DLLs)';
  ResStr_AddinModules =  'Add-in modules';
  ResStr_AboutTitle = 'About %s';

//==============================================================================
procedure TdlgAbout.FormCreate(Sender: TObject);
var lFileName:string;
begin
  {Load from res file??}
  lFileName:=ExtractFilePath(Application.ExeName)+'Images\AboutR2K_2.bmp';
  if FileExists(lFileName) then
    Image.Picture.Bitmap.LoadFromFile(lFileName);
  InitializeCaptions;
  InitializeMoreTab;
  pcAbout.ActivePage:=tsVersion;
  Caption := Format(ResStr_AboutTitle, [Application.Title]);
end;  // FormCreate

//==============================================================================
function TdlgAbout.GetFileDate(Name: string): string;
var  Handle: integer;
begin
  Result := '';
  Handle := FileOpen(Name, fmOpenRead or fmShareDenyNone);
  if Handle > 0 then begin
    {the 'c' constant uses the system short time/date format defined in
     Windows setup}
    {but this gets the date how I wantm it!}
    Result := FormatDateTime('d mmm yyyy', FileDateToDateTime(FileGetDate(Handle)));
    FileClose(Handle);
  end;
end;  // GetFileDate

//==============================================================================
procedure TdlgAbout.InitializeCaptions;
var
  MS: TMemoryStatus;
begin
  OS.Caption := GetOSInfo;
  MS.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(MS);
  PhysMem.Caption     := FormatFloat('#,###" KB"', MS.dwTotalPhys div 1024);
  Version.Caption     := GetFileVersion(Application.ExeName);
  lDate.Caption       := GetFileDate(Application.ExeName);
  lblDictionaryVersion.Caption := AppSettings.DictionaryVersion;
  lblDatabaseVersion.Caption := AppSettings.DatabaseVersion;
  lblUsername.Caption := AppSettings.UserName;
end;  // InitializeCaptions

//==============================================================================
procedure TdlgAbout.InitializeMoreTab;
var lDlls:TStringList;
    lIdx :integer;
begin
  lDlls:=TStringList.Create;
  try
    with mmMoreInfo do begin
      Clear;
      Lines.Add(ResStr_XPMenu_Acknowledgement);
      Lines.Add(ResStr_DLL);
      Lines.Add('-----------------------------');
      lDlls.Sorted:=true;  // Will prevent duplicates
      GetDllList(Application.ExeName,lDlls);
      for lIdx:=0 to lDlls.Count-1 do begin
        Lines.Add('  '+lDlls[lIdx]+' ('+GetFileVersion(lDlls[lIdx])+')');
      end;

      with AppSettings.ComAddIns do
        if AddInCount>0 then begin
          Lines.Add('');
          Lines.Add(ResStr_AddinModules);
          Lines.Add('--------------');
          for lIdx:=0 to AddInCount-1 do begin
            if AddInList[lIdx].Description='' then
              Lines.Add('  '+AddInList[lIdx].Name)
            else
              Lines.Add('  '+AddInList[lIdx].Name+', '+AddInList[lIdx].Description);
            Lines.Add('');
          end; // for
        end;
    end;
  finally
    lDlls.Free;
  end;
end;

//==============================================================================
end.
