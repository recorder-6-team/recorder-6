//==============================================================================
//  Unit:        InstallDiction
//
//  Implements:  TdlgInstallDictionary
//
//  Description: Implements the interface to install additional dictionaries.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Last Revision Details:
//    $Revision: 24 $
//    $Date: 13/12/07 11:56 $
//    $Author: Rickyshrestha $
//
//  $History: InstallDiction.pas $
//  
//  *****************  Version 24  *****************
//  User: Rickyshrestha Date: 13/12/07   Time: 11:56
//  Updated in $/JNCC/Development/Build/Source
//  Chnaged a hardcoded string to resourcestring
//  ResStr_InsertCD
//  
//  *****************  Version 23  *****************
//  User: Ericsalmon   Date: 4/12/02    Time: 16:06
//  Updated in $/JNCC/Source
//  Removed USE_TITAN compiler directive and D4-specific code.
//  
//  *****************  Version 22  *****************
//  User: Ericsalmon   Date: 19/06/02   Time: 17:04
//  Updated in $/JNCC/Source
//  Replaced BitBtns with ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit InstallDiction;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Math, TaxonDictBrowserData, DataClasses, Constants,
  Buttons, InstallCheckListData, InstallCheckList, OnlineHelp, GeneralFunctions,
  ImageListButton, DatabaseAccessADO;

resourcestring
  ResStr_InsertCD = 'Please insert the Taxon Dictionary CD into the CD-ROM drive before continuing';


type
  TdlgInstallDictionary = class(TForm)
    Label1: TLabel;
    lbAvailable: TListBox;
    lbSelected: TListBox;
    Label2: TLabel;
    Bevel1: TBevel;
    Label3: TLabel;
    lblDiskSpace: TLabel;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    bbAdd: TImageListButton;
    bbRemove: TImageListButton;
    bbAddAll: TImageListButton;
    bbClearAll: TImageListButton;
    procedure bbAddClick(Sender: TObject);
    procedure bbRemoveClick(Sender: TObject);
    procedure bbClearAllClick(Sender: TObject);
    procedure bbAddAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
  private
    hlpInstall: TOnlineHelp;
    FdmTaxonDictBrowser: TdmTaxonDictBrowser;
    FdmInstallData : TdmInstallCheckList;
    procedure ResetExtents(AListBox:TListBox);
  protected
    procedure CheckButtons;
    procedure SetDiskSpace;
    procedure PopulateAvailable;
    procedure SetupObjects;
    procedure FreeObjects;
  public
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData, Maintbar, FormActions;

//==============================================================================
procedure TdlgInstallDictionary.FormCreate(Sender: TObject);
begin
  inherited;
  SetDiskSpace;
  SetupObjects;
  PopulateAvailable;
  CheckButtons;

  //Help Setup
  hlpInstall := TOnlineHelp.Create(Self.Handle);
  OnHelp := hlpInstall.OnHelpReplacement;
  HelpContext := IDH_DICTINSTALL;
end;  // FormCreate

//==============================================================================
procedure TdlgInstallDictionary.FormDestroy(Sender: TObject);
begin
  hlpInstall.Free;
  FreeObjects;
  inherited;
end;  // FormDestroy

//==============================================================================
procedure TdlgInstallDictionary.SetDiskSpace;
var
  liFreeBytes : Int64;
  lDriveNum : byte;
begin
  lDrivenum := Ord(AppSettings.DatabasePath[1])-64;
  liFreeBytes := DiskFree(lDriveNum);
  if liFreeBytes > Power(1024,2) then
    lblDiskSpace.Caption := IntToStr( trunc( (liFreebytes/ Power(1024,2)) + 0.5 ) ) + ' Mb'
  else if liFreeBytes > 1024 then
    lblDiskSpace.Caption := IntToStr( trunc( (liFreebytes/ 1024)  + 0.5 ) ) + ' Kb'
  else
    lblDiskSpace.Caption := IntToStr( liFreebytes ) + ' bytes';
end;  // SetDiskSpace

//==============================================================================
procedure TdlgInstallDictionary.SetupObjects;
begin
  FdmTaxonDictBrowser := TdmTaxonDictBrowser.Create(Self);
  FdmInstallData      := TdmInstallCheckList.Create(Self);
  with FdmInstallData do begin
    dmDatabase.SetDatabaseLocal([qryTLIDest, qryTVDest, qryTDest, qryTFDest,
                                 qryTDDest, qrySDest, qryRSDest, qryFSDest,
                                 qryTLDest, qryTLVDest, qryTDTDest, qryTRDest,
                                 qryTLTDest, qryTNTDest, qryTSDest]);

    dmDatabase.SetDatabaseCD([qryTLISource, qryTVSource, qryTSource, qryTFSource,
                              qryTDSource, qrySSource, qryRSSource, qryFSSource,
                              qryTLSource, qryTLVSource, qryTDTSource, qryTLTSource,
                              qryTRSource, qryTNTSource, qryTSSource]);
  end;
end;  // SetupObjects

//==============================================================================
procedure TdlgInstallDictionary.FreeObjects;
var lIndex:integer;
begin
  with lbAvailable.Items do
    for lIndex := 0 to Count -1 do begin
      Objects[lIndex].Free;
      Objects[lIndex]:=nil;
    end;
  with lbSelected.Items do
    for lIndex := 0 to Count -1 do begin
      Objects[lIndex].Free;
      Objects[lIndex]:=nil;
    end;
  // Owner is Self.  Self will destroy them
//  FdmTaxonDictBrowser.Free;
//  FdmTaxonDictBrowser:=nil;
//  FdmInstallData.Free;
//  FdmInstallData:=nil;
end;  // FreeObjects

//==============================================================================
procedure TdlgInstallDictionary.PopulateAvailable;
var
  lListObj : TKeyData;
begin
  lbAvailable.Clear;
  with FdmTaxonDictBrowser.qryList do begin
    try
      Open;
      while not Eof do begin
        if not FieldByName('IsLocal').AsBoolean then begin
          lListObj := TKeyData.Create;
          lListObj.ItemKey := FieldByName('KeyField').AsString;
          lbAvailable.Items.AddObject(FieldByName('DisplayField').AsString,lListObj);
        end;
        Next;
      end;
    finally
      Close;
    end;
  end;
end;  // PopulateAvailable

//==============================================================================
procedure TdlgInstallDictionary.CheckButtons;
begin
  bbAdd.Enabled     :=lbAvailable.ItemIndex<>-1;
  bbAddAll.Enabled  :=lbAvailable.Items.Count>0;
  bbRemove.Enabled  :=lbSelected.ItemIndex<>-1;
  bbClearAll.Enabled:=lbSelected.Items.Count>0;
  // Enable OK only when something has been selected
  bbOk.Enabled:=bbClearAll.Enabled;
  ResetExtents(lbAvailable);
  ResetExtents(lbSelected);
end;  // CheckButtons

//==============================================================================
procedure TdlgInstallDictionary.bbAddClick(Sender: TObject);
var iIndex:integer;
begin
  inherited;
  iIndex:=lbAvailable.ItemIndex;
  if iIndex<>-1 then begin
    lbSelected.Items.AddObject(lbAvailable.Items[iIndex],lbAvailable.Items.Objects[iIndex]);
    lbAvailable.Items.Delete(iIndex);
    if (iIndex=0) and (lbAvailable.Items.Count>0) then lbAvailable.ItemIndex:=0
                                                  else lbAvailable.ItemIndex:=iIndex-1;
    CheckButtons;
  end;
end;  // bbAddClick

//==============================================================================
procedure TdlgInstallDictionary.bbRemoveClick(Sender: TObject);
var iIndex:integer;
begin
  inherited;
  iIndex:=lbSelected.ItemIndex;
  if iIndex<>-1 then begin
    lbAvailable.Items.AddObject(lbSelected.Items[iIndex],lbSelected.Items.Objects[iIndex]);
    lbSelected.Items.Delete(iIndex);
    if (iIndex=0) and (lbSelected.Items.Count>0) then lbSelected.ItemIndex:=0
                                                 else lbSelected.ItemIndex:=iIndex-1;
    CheckButtons;
  end;
end;  // bbRemoveClick

//==============================================================================
procedure TdlgInstallDictionary.bbAddAllClick(Sender: TObject);
var iIndex:integer;
begin
  inherited;
  for iIndex:=0 to lbAvailable.Items.Count-1 do begin
    lbSelected.Items.AddObject(lbAvailable.Items[0],lbAvailable.Items.Objects[0]);
    lbAvailable.Items.Delete(0);
  end;
  CheckButtons;
end;  // bbAddAllClick

//==============================================================================
procedure TdlgInstallDictionary.bbClearAllClick(Sender: TObject);
var iIndex:integer;
begin
  inherited;
  for iIndex:=lbSelected.Items.Count-1 downto 0 do begin
    lbAvailable.Items.AddObject(lbSelected.Items[iIndex],lbSelected.Items.Objects[iIndex]);
    lbSelected.Items.Delete(iIndex);
  end;
  CheckButtons;
end;  // bbClearAllClick

//==============================================================================
procedure TdlgInstallDictionary.ListClick(Sender: TObject);
begin
  CheckButtons;
end;  // ListClick

//==============================================================================
procedure TdlgInstallDictionary.bbOkClick(Sender: TObject);
var
  lIndex : Integer;
  lTLImp : TTLImporter;
  lCursor:TCursor;
begin
  while not FileExists(AppSettings.CDDatabasePath) and
        (MessageDlg(ResStr_InsertCD, mtInformation,[mbRetry,mbCancel],0) = mrRetry) do
    WaitFor(5);

  // Exit loop only if cancel selected or FileExists
  if FileExists(AppSettings.CDDatabasePath) then begin
    for lIndex := 0 to lbSelected.Items.Count -1 do
    begin
      lCursor := HourglassCursor;
      lTLImp := TTLImporter.Create(FdmInstallData);
      try
        lTLImp.TLKey_Source := TKeyData(lbSelected.Items.Objects[lIndex]).ItemKey;
        lTLImp.Import;
        FdmTaxonDictBrowser.MarkListAsLocal(
                              TKeyData(lbSelected.Items.Objects[lIndex]).ItemKey);
      finally
        lTLImp.Free;
        SetDiskSpace;
        DefaultCursor(lCursor);
      end;
    end;
  end;
end;  // bbOkClick

//==============================================================================
procedure TdlgInstallDictionary.ResetExtents(AListBox:TListBox);
var lIdx,ScrollWidth:integer;
begin
  with AListBox do begin
    ScrollWidth:=0;
    // Find the maximum width
    for lIdx:=0 to Items.Count-1 do
      if ScrollWidth<Canvas.TextWidth(Items[lIdx]) then
        ScrollWidth:=Canvas.TextWidth(Items[lIdx]);

    // If scroll bar needed, show it, with correct width
    SendMessage(Handle,WM_HSCROLL,MAKELONG(0,SB_TOP),Handle);
    if ScrollWidth>Width then
      SendMessage(Handle,LB_SETHORIZONTALEXTENT,ScrollWidth+4,0)
    else
      // Otherwise, remove the scrollbar
      SendMessage(Handle,LB_SETHORIZONTALEXTENT,0,0);
  end;
end;  // ResetExtents

//==============================================================================
end.
 