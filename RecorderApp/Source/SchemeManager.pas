//==============================================================================
//  Unit:        SchemeManager
//
//  Implements:  TdlgSchemeManager
//
//  Description: Dialog for managing contributions to recording schemes.
//
//  Author:      John van Breda
//  Created:     25 February 2000
//
//  Changes:     Eric Salmon 13/06/02
//               Database export.
//
//  Last Revision Details:
//    $Revision: 18 $
//    $Date: 19/12/07 10:53 $
//    $Author: Rickyshrestha $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================
unit SchemeManager;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SchemeManagerData, Grids, StdCtrls, Buttons, DataStringGrid, Menus, XPMenu,
  GeneralData, ExceptionForm, MapiDLLFunctions, ImageListButton, Constants,
  OnlineHelp, ControlStringGrid;

type
  ESchemeDialog = class(TExceptionPath);

  TdlgSchemeManager = class(TForm)
    sgSchemes: TControlStringGrid;
    lblSchemes: TLabel;
    pmSchemeGrid: TPopupMenu;
    mnuNewRecordingScheme: TMenuItem;
    mnuDelete: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnOk: TImageListButton;
    btnCancel: TImageListButton;
    btnOpen: TImageListButton;
    btnSave: TImageListButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private
    FdmSchemeManager: TdmSchemeManager;
    FGridManager : TDataStringGrid;
    hlpSchemeMan : TOnlineHelp;
    FXPMenu : TXPMenu;
    procedure OpenFilter(AStrings: TStrings);
    procedure OpenOldScheme(AStrings: TStrings);
    procedure WMUpdateMenuIcons(var Msg:TMessage); message WM_UPDATE_MENU_ICONS;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralFunctions, FormActions, ExportFilters, ApplicationSettings;

const
  SQL_SELECT_SCHEME = 'Select RECORDING_SCHEME_KEY, ITEM_NAME, EMAIL, EXPORT_FILTER_KEY, EXPORT_FORMAT_ID ' +
                      'From RECORDING_SCHEME Where RECORDING_SCHEME_KEY=''%s''';

resourcestring
  ResStr_BadFile = 'The file is not a valid recording scheme file';

  ResStr_SaveChanges = 'Before saving a scheme file, the changes must be saved to the database.'#13#10 +
                       'Save changes now?';

  ResStr_SchemeAlreadyExists =  'The scheme you are importing already exists on this database.  ' +
                                'Do you want to overwrite it?';

  ResStr_OverwriteFilter =  'This scheme relates to a filter already in the database.  Overwrite filter?';
  ResStr_AbortImportScheme =  'Import of Recording Scheme aborted.';

//==============================================================================
{ TdlgSchemeManager }
//==============================================================================
{ Constructor starts up the datamodule }
constructor TdlgSchemeManager.Create(AOwner: TComponent);
var
  i : integer;
const
  SCREEN_SIZE_INCREASE = 1.2; // multiplier used to increase size for larger screens
begin
  inherited;
  FXPMenu := TXPMenu.Create(Self);
  with FXPMenu do begin
    XPControls := [xcMainMenu, xcPopupMenu];
    Active := AppSettings.ShowMenuIcons;
    Gradient := AppSettings.GraduatedMenus;
  end;
  FdmSchemeManager := TdmSchemeManager.Create(nil);
  // and use screen size if we have it
  if Screen.Width >= 1024 then begin
    Width := Round(Width * SCREEN_SIZE_INCREASE);
    with FdmSchemeManager.qrySchemes do
      for i := 0 to FieldCount-1 do
        Fields[i].DisplayWidth := Round(Fields[i].DisplayWidth * SCREEN_SIZE_INCREASE);

    // Anchors don't take effect during constructor, so move components
    sgSchemes.Width := Width - 22;
    btnCancel.Left  := Width - 90;
    btnOk.Left      := Width - 174;
  end;

  FGridManager := TDataStringGrid.Create(sgSchemes, FdmSchemeManager.qrySchemes,
                                         'RECORDING_SCHEME_KEY');
  FGridManager.Indicator := True;
  { Assign callback to handle row updates }
  FGridManager.OnUpdateRow := FdmSchemeManager.UpdateRow; // edit or append
  FGridManager.OnDeleteRow := FdmSchemeManager.DeleteRow; // delete
  FGridManager.PopulateGrid;
  FGridManager.Enabled := True;
  { Assign popup menu handlers }
  mnuNewRecordingScheme.OnClick := FGridManager.AddToGrid;
  mnuDelete.OnClick             := FGridManager.DeleteFromGrid;

  // Set to sizeable after inherited create, otherwise Base Form resizes the dialog!
  BorderStyle := bsSizeable;
  // Setup online help
  hlpSchemeMan   := TOnlineHelp.Create(Self.Handle);
  Self.OnHelp     := hlpSchemeMan.OnHelpReplacement;
end;  // Create

//==============================================================================
{ Destructor - free resources }
destructor TdlgSchemeManager.Destroy;
begin
  FdmSchemeManager.Free;
  FGridManager.Free;
  inherited;
end;  // Destroy

//==============================================================================
procedure TdlgSchemeManager.btnOkClick(Sender: TObject);
begin
  inherited;
  try
    FGridManager.Save;
  except
    on EDataStringGridError do begin
      ModalResult := mrNone; // abort closing dialog as validation failed
      raise;
    end;
  end; // try
end;  // btnOkClick

//==============================================================================
procedure TdlgSchemeManager.btnSaveClick(Sender: TObject);
var
  lFileStrings : TStringList;
  lFilter: TExportFilter;
begin
  inherited;
  if FGridManager.RowEdited[sgSchemes.Row] then begin
    if MessageDlg(ResStr_SaveChanges, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      FGridManager.PostRow(sgSchemes.Row)
    else
      Exit; // from function
  end;
  if SaveDialog.Execute then begin
    lFileStrings := TStringList.Create;
    with dmGeneralData.qryAllPurpose do try
      SQL.Text := Format(SQL_SELECT_SCHEME, [FGridManager.Key[sgSchemes.Row]]);;
      Open;
      try
        lFileStrings.Add('[Recording Scheme]');
        lFileStrings.Add('Recording_scheme_key=' + FieldByName('RECORDING_SCHEME_KEY').AsString);
        lFileStrings.Add('Scheme_Name=' + FieldByName('ITEM_NAME').AsString);
        lFileStrings.Add('Email=' + FieldByName('EMAIL').AsString);
        lFileStrings.Add('Export_Format_Id=' + FieldByName('Export_Format_Id').AsString);
        if not FieldByName('EXPORT_FILTER_KEY').IsNull then begin
          //lFileStrings.Add('Export_Filter_Key=' + FieldByName('EXPORT_FILTER_KEY').AsString);
          lFilter := TExportFilter.CreateFromDatabase(FieldByName('EXPORT_FILTER_KEY').AsString);
          try lFilter.SaveToStrings(lFileStrings);
          finally lFilter.Free end;
        end;

//        if ExtractWithoutExt(SaveDialog.FileName) = '' then
        if ExtractFileExt(SaveDialog.FileName) = '' then
          lFileStrings.SaveToFile(SaveDialog.FileName + '.sch')
        else
          lFileStrings.SaveToFile(SaveDialog.FileName);
      finally Close end;
    finally
      lFileStrings.Free;
    end; // with qryAllPurpose do try
  end;
end;  // btnSaveClick

//==============================================================================
// Allows user to open a scheme from a saved XML file }
procedure TdlgSchemeManager.btnOpenClick(Sender: TObject);
var
  lFileStrings: TStringList;
  lKey : string;
  lExists : boolean;
begin
  inherited;
  if OpenDialog.Execute then begin
    lFileStrings := TStringList.Create;
    try
      lFileStrings.LoadFromFile(OpenDialog.FileName);
      if lFileStrings[0] <> '[Recording Scheme]' then
        raise ESchemeDialog.CreateNonCritical(ResStr_BadFile);
      lKey := lFileStrings.Values['Recording_scheme_key'];
      // Check if scheme already exists
      with dmGeneralData.qryAllPurpose do begin
        SQL.Text := Format(SQL_SELECT_SCHEME, [lKey]);
        Open;
        try
          lExists := RecordCount > 0;
        finally
          Close;
        end;
        { Ensure file strings only contains required fields }
        lFileStrings.Delete(0);
        lFileStrings.Delete(0);
        if lFileStrings.Values['Export_Filter_Key'] <> '' then
          OpenFilter(lFileStrings)
        else if lFileStrings.Values['Survey_Key'] <> '' then
          OpenOldScheme(lFileStrings);
        if lExists then begin // existing
          if ConfirmYesNo(ResStr_SchemeAlreadyExists) = mrYes then
            FdmSchemeManager.UpdateRowKnownInsert(lKey, lFileStrings, false);
        end else
          FdmSchemeManager.UpdateRowKnownInsert(lKey, lFileStrings, true);
      end;
      FGridManager.RefreshRow(lKey);
    finally
      lFileStrings.Free;
      dmGeneralData.qryAllPurpose.Close; // safety net
    end;
  end;
end;  // btnOpenClick

//==============================================================================
procedure TdlgSchemeManager.OpenFilter(AStrings: TStrings);
var lFilter: TExportFilter;
begin
  if TExportFilter.FilterExists(AStrings.Values['Export_Filter_Key']) then
    case DefMessageDlg
           (ResStr_OverwriteFilter, mtWarning, mbYesNoCancel, mbNo, 0) of
      idYes: begin
        lFilter := TExportFilter.CreateFromDatabase(AStrings.Values['Export_Filter_Key']);
        try
          lFilter.PopulateFromStrings(AStrings);
          lFilter.SaveFilter;
        finally lFilter.Free end;
      end;
      idCancel: raise TExceptionPath.CreateNonCritical(ResStr_AbortImportScheme);
    end
  else begin
    lFilter := TExportFilter.Create;
    try
      lFilter.PopulateFromStrings(AStrings);
      lFilter.SaveFilter;
    finally lFilter.Free end;
  end;
end;

procedure TdlgSchemeManager.OpenOldScheme(AStrings: TStrings);
var lFilter: TExportFilter;
begin
  lFilter := TExportFilter.Create;
  try
    lFilter.FilterName := FdmSchemeManager.GetSurveyName(AStrings.Values['Survey_Key']) + ' Filter';
    lFilter.AddSurvey('', AStrings.Values['Survey_Key']);
    lFilter.IncludeOccurrences := True;
    lFilter.IncludeLocations := True;
    lFilter.IncludeNames := True;
    lFilter.SaveFilter;
    AStrings.Add('Export_Filter_Key=' + lFilter.FilterKey);
  finally lFilter.Free end;
end;

procedure TdlgSchemeManager.WMUpdateMenuIcons(var Msg: TMessage);
begin
  FXPMenu.Active := AppSettings.ShowMenuIcons;
  FXPMenu.Gradient := AppSettings.GraduatedMenus;
end;

end.
