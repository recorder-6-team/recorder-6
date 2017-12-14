{===============================================================================

  Copyright © Dorset Software Services Ltd, 1999

  Component:
    TFolderBrowser - Johnny Mamenko 01/07/1998
                     E-mail: mamenko@iname.com
                     HTTP:   //attend.to/johnny

  Updates:
    TFolderBrowser - Eric Salmon 15/10/1999

  Packages:
    InHouse4, Delphi 4 package for in house components.
    InHouse5, Delphi 5 package for in house components.

  Description:
    Wrapper control around the SHBrowseForFolder function. Display a folder
    selection dialog box, no files, and returns a selected path.

===============================================================================}

{$I DelphiVersions.Inc}

unit FolderBrowser;

interface

uses
  Windows, SysUtils, Classes, Controls, ShlObj;

type
  TBrowseFlag = (bfComputersOnly, bfPrintersOnly, bfDirsOnly, bfStatusText);

  TBrowseFlags = set of TBrowseFlag;

  TFolderChangeEvent = procedure (const Folder: string;
                                  var EnabledOK: integer;
                                  // 0 - Disables the OK button
                                  // 1 - Enables the OK button
                                  //-1 - leave as is
                                  var StatusText: string) of object;

  TFolderBrowser = class (TComponent)
  private
    FTitle : string;
    FBrowseFlags : TBrowseFlags;
    FFolder: string;
    FOwnerHandle : HWND;
    FOnChangeFolder: TFolderChangeEvent;
    FNewStyle: Boolean;
    procedure SetFolder(const Value: string);
    procedure SetOnChangeFolder(const Value: TFolderChangeEvent);
    procedure SetBrowseFlags(const Value: TBrowseFlags);
    procedure SetNewStyle(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: boolean;
  published
    property BrowseFlags : TBrowseFlags read FBrowseFlags write SetBrowseFlags;
    property Folder : string read FFolder write SetFolder;
    property NewDialogStyle: Boolean read FNewStyle write SetNewStyle;
    property Title : string read FTitle write FTitle;
    property OnChangeFolder : TFolderChangeEvent read FOnChangeFolder write SetOnChangeFolder;
  end;

function FolderCallBack(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;

//==============================================================================
implementation

const
  BIF_NONEWFOLDERBUTTON = $200;

resourcestring
  ResStr_SelectFolder = 'Select folder';

var
  CurrentOpenedFolder: string;
  CurrentEventHandler: TFolderChangeEvent;

//==============================================================================
// ES - Set the status to the currently selected path
function SetDefaultStatusText(Wnd: HWND; const stFolder: string): string;
const MAX_WIDTH = 355;
var DC           : HDC;
    lSize        : TSize;
    stStart,stEnd: string;
    lIndex       : integer;
begin
  Result := stFolder;
  DC     := GetDC(Wnd);
  try
    // Get the width of the selected folder
    GetTextExtentPoint(DC, PChar(stFolder), Length(stFolder), lSize);
    // If larger than allowed, get some bits out
    if lSize.cX >= MAX_WIDTH then begin
      // Always keep the last sub folder
      lIndex := Length(stFolder);
      while (lIndex > 0) and (stFolder[lIndex] <> '\') do Dec(lIndex);
      stEnd := Copy(stFolder, lIndex, 255);
      // Skip the folder above, and replace it with '...'
      Dec(lIndex);
      while (lIndex > 0) and (stFolder[lIndex] <> '\') do Dec(lIndex);
      stStart := Copy(stFolder, 1, lIndex) + '...';
      // Continue to remove sub folders until the text can be displayed on one line
      GetTextExtentPoint(DC, PChar(stStart + stEnd), Length(stStart + stEnd), lSize);
      while (lIndex > 0) and (lSize.cX >= MAX_WIDTH) do begin
        Dec(lIndex);
        while (lIndex > 0) and (stFolder[lIndex] <> '\') do Dec(lIndex);
        stStart := Copy(stFolder, 1, lIndex) + '...';
        GetTextExtentPoint(DC, PChar(stStart + stEnd), Length(stStart + stEnd), lSize);
      end;
      // Set the result string
      Result := stStart + stEnd;
    end;
  finally
    ReleaseDC(Wnd, DC);
  end;
end;  // SetDefaultStatusText

//==============================================================================
function FolderCallBack(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
var
  lszFolder : array[0..MAX_PATH] of Char;
  EnabledOK : integer;
  StatusText, Folder : string;
begin
  Result:=0;
  // ES - Initialization
  case uMsg of
    BFFM_INITIALIZED :
      begin
        StrPCopy(lszFolder, CurrentOpenedFolder);
        SendMessage(Wnd, BFFM_SETSELECTION, Ord(True), Integer(@lszFolder[0]));
      end;

    BFFM_SELCHANGED :
      begin
        EnabledOK  := -1;
        StatusText := '';
        // ES - Get the selected folder
        SHGetPathFromIDList(Pointer(lParam), lszFolder);
        Folder := StrPas(lszFolder);
        StatusText := SetDefaultStatusText(Wnd, Folder);
        if Assigned(CurrentEventHandler) and (Folder <> '') then
          CurrentEventHandler(Folder, EnabledOK, StatusText);

        if EnabledOK <> -1 then
          SendMessage(Wnd, BFFM_ENABLEOK, EnabledOK, EnabledOK);

        // No effect with NewDialogStyle
        if StatusText <> '' then
          SendMessage(Wnd, BFFM_SETSTATUSTEXT, EnabledOK, Integer(PChar(StatusText)));
      end;
  end;
end;  // FolderCallBack

//==============================================================================
{TFolderBrowser}
constructor TFolderBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle       := ResStr_SelectFolder;  // Not title bar caption!
  FBrowseFlags := [];
  FFolder      := '';
end;  // Create

//==============================================================================
function TFolderBrowser.Execute: boolean;
var bi       : TBrowseInfoA;
    lszFolder: array[0..MAX_PATH] of Char;
    lszTitle : PChar;
    idl      : PItemIDList;
begin
  lszTitle:=StrAlloc(Length(FTitle) + 1);
  try
    StrPCopy(lszTitle, FTitle);
    // Fix for apparent bug when using frames, Owner's handle is different here
    // from the one during the Create. Go figure... :-(
    if Owner is TWinControl then
      FOwnerHandle := (Owner As TWinControl).Handle;
    // ES - Set the BrowserInfo structure
    bi.hwndOwner      := FOwnerHandle;
    bi.pszDisplayName := @lszFolder[0];
    bi.lpszTitle      := lszTitle;
    bi.ulFlags  := {$IFDEF DELPHI7UP}
                   BIF_NEWDIALOGSTYLE * Ord(FNewStyle) +
                   BIF_NONEWFOLDERBUTTON +
                   {$ENDIF}
                   BIF_BROWSEFORCOMPUTER * Ord(bfComputersOnly in BrowseFlags) +
                   BIF_BROWSEFORPRINTER * Ord(bfPrintersOnly in BrowseFlags) +
                   BIF_RETURNONLYFSDIRS * Ord(bfDirsOnly in BrowseFlags) +
                   BIF_STATUSTEXT * Ord(bfStatusText in BrowseFlags);
    bi.lpfn     := FolderCallBack;
    bi.lParam   := 0;
    bi.pidlRoot := nil;

    CurrentOpenedFolder := FFolder;
    CurrentEventHandler := FOnChangeFolder;
    // ES - Call the function to display the dialog
    idl := SHBrowseForFolder(bi);
    if idl <> nil then begin
      // ES - Get the selected folder
      SHGetPathFromIDList(idl, lszFolder);
      FFolder := StrPas(lszFolder);
      Result  := true;
    end else
      Result := false;
  finally
    StrDispose(lszTitle);
  end;//finally
end;  // Execute

//==============================================================================
procedure TFolderBrowser.SetBrowseFlags(const Value: TBrowseFlags);
begin
  if FBrowseFlags <> Value then begin
    FBrowseFlags := Value;
    if bfStatusText in FBrowseFlags then NewDialogStyle := False;
  end;
end;

//==============================================================================
procedure TFolderBrowser.SetFolder(const Value: string);
begin
  FFolder := Value;
end;  // SetFolder

//==============================================================================
procedure TFolderBrowser.SetNewStyle(const Value: Boolean);
begin
  if FNewStyle <> Value then begin
    FNewStyle := Value;
    if Value then BrowseFlags := BrowseFlags - [bfStatusText];
  end;
end;

//==============================================================================
procedure TFolderBrowser.SetOnChangeFolder(const Value: TFolderChangeEvent);
begin
  FOnChangeFolder := Value;
end;  // SetOnChangeFolder

//==============================================================================
initialization
  CurrentOpenedFolder := '';
  CurrentEventHandler := nil;
end.
