//==============================================================================
//  Unit:        Metadata
//
//  Implements:  TdlgMeteData
//
//  Description:
//
//  Author:      John van Breda
//  Created:     8 February 2000
//
//  Last Revision Details:
//    $Revision: 19 $
//    $Date: 22/04/09 14:06 $
//    $Author: Ericsalmon $
//
//  $History: Metadata.pas $
//  
//  *****************  Version 19  *****************
//  User: Ericsalmon   Date: 22/04/09   Time: 14:06
//  Updated in $/JNCC/Development/Build/Source
//  Control alignments.
//  
//  *****************  Version 18  *****************
//  User: Rickyshrestha Date: 18/12/07   Time: 9:56
//  Updated in $/JNCC/Development/Build/Source
//  Changed some constant and hardcoded strings to resourcestring
//   ResStr_BadCharsError
//    ResStr_AllInfoMissing
//    ResStr_ColumnCaption0
//    ResStr_ColumnCaption1
//  
//  *****************  Version 17  *****************
//  User: Ericsalmon   Date: 21/03/07   Time: 16:27
//  Updated in $/JNCC/Development/Build/Source
//  VI 13079. Converted around 75 strings to resource strings for
//  translation.
//  
//  *****************  Version 16  *****************
//  User: Johnvanbreda Date: 25/09/06   Time: 16:27
//  Updated in $/JNCC/Development/Build/Source
//  IR12619
//  Fixed problem with window hiding when using Help
//  
//  *****************  Version 15  *****************
//  User: Ericsalmon   Date: 9/02/04    Time: 11:20
//  Updated in $/JNCC/Development/Build/Source
//  ID 2457. Set ModalResult to mrNone instead of leaving it to mrOK, which
//  caused the dialog to always close.
//  
//  *****************  Version 14  *****************
//  User: Ericsalmon   Date: 24/02/03   Time: 11:03
//  Updated in $/JNCC/Source
//  Cleanup. Inherits from TForm, no need for TBaseForm.
//  
//  *****************  Version 13  *****************
//  User: Andrewkemp   Date: 17/01/03   Time: 12:28
//  Updated in $/JNCC/Source
//  IR240: disallow characters that would require escaping in XML text
//  nodes.
//  
//  *****************  Version 12  *****************
//  User: Andrewkemp   Date: 10/01/03   Time: 16:29
//  Updated in $/JNCC/Source
//  IR192: When dmGeneralData.qryMetaData's SQL is restored on close (eww!)
//  the query is set back to its original SQL instead of a slightly
//  modified (broken) version...
//  
//  *****************  Version 11  *****************
//  User: Ericsalmon   Date: 4/12/02    Time: 17:04
//  Updated in $/JNCC/Source
//  Removed USE_TITAN compiler directive and D4-specific code.
//  
//  *****************  Version 10  *****************
//  User: Johnvanbreda Date: 3/12/02    Time: 12:49
//  Updated in $/JNCC/Source
//  Bugfixes
//  
//  *****************  Version 9  *****************
//  User: Ericsalmon   Date: 28/06/02   Time: 10:50
//  Updated in $/JNCC/Source
//  JNCC 579: Single quote character problem fixed.
//  
//  *****************  Version 8  *****************
//  User: Ericsalmon   Date: 19/06/02   Time: 17:14
//  Updated in $/JNCC/Source
//  Replaced BitBtns with ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Metadata;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DBCtrls, Mask, Db, JNCCDatasets, Grids, DBGrids, ExceptionForm,
  RTFGrid, JNCCGrid, Buttons, ExtCtrls, OnlineHelp, ImageListButton,
  GeneralFunctions, ADODB;

type
  TdlgMetadata = class(TForm)
    dsMetadata: TDataSource;
    lblDataItem: TLabel;
    lblData: TLabel;
    lblInstruct: TLabel;
    lblSelect: TLabel;
    Bevel1: TBevel;
    dbgMetadataItems: TDBRTFGrid;
    mmInfo: TMemo;
    bbContinue: TImageListButton;
    bbCancel: TImageListButton;
    bbMetaDataEdit: TImageListButton;
    bbAccept: TImageListButton;
    bbDiscard: TImageListButton;
    lblMetaItem: TLabel;
    procedure bbContinueClick(Sender: TObject);
    procedure bbMetaDataEditClick(Sender: TObject);
    procedure bbAcceptClick(Sender: TObject);
    procedure bbDiscardClick(Sender: TObject);
    procedure dsMetadataDataChange(Sender: TObject; Field: TField);
    procedure dbgMetadataItemsDrawColumnCell(Sender: TObject;
      const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FToEdit: boolean;
    FOriginalSql: string;
    hlpMetadata : TOnlineHelp;
    procedure SetGridDisplay;
    procedure EditMode(iEditState : boolean);
    function Validate: Boolean;
  public
    constructor Create(AOwner : TComponent; ToEdit: boolean = False); reintroduce;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  Generaldata, FormActions, GenFuncs;

const
  BAD_CHARS = '<>&';

resourcestring
  ResStr_BadCharsError = 'Invalid characters in data.'#13#13'You may not use any of the characters <>&&';
  ResStr_AllInfoMissing = 'Please enter information for all items';
  ResStr_ColumnCaption0 = 'Data item';
  ResStr_ColumnCaption1 = 'Exported information';


//==============================================================================
constructor TdlgMetadata.Create(AOwner: TComponent; ToEdit: boolean = False);
begin
  inherited Create(AOwner);
  FToEdit := ToEdit;
  with dmGeneralData.qryMetadata do begin
    if Active then Close;
    if FToEdit then begin
      FOriginalSql := SQL.Text;
      SQL.Strings[2] := ';';
    end;
    Open;
  end;
  SetGridDisplay;
  EditMode(False);
  hlpMetadata   := TOnlineHelp.Create(Self.Handle);
  Self.OnHelp     := hlpMetadata.OnHelpReplacement;
  Self.HelpContext := IDH_METADATA;
end;

//==============================================================================
{ Set the columns so they display nicely }
procedure TdlgMetadata.SetGridDisplay;
begin
  with dbgMetadataItems do begin
    Columns[0].Width := 100;
    Columns[1].Width := 160;
    Columns[0].Title.Caption := ResStr_ColumnCaption0;
    Columns[1].Title.Caption := ResStr_ColumnCaption1;
  end;
end;

//==============================================================================
procedure TdlgMetadata.bbContinueClick(Sender: TObject);
var
  lResult : Word;
begin
  inherited;
  if bbAccept.Enabled then
  begin
    lResult := MessageDlg(ResStr_StillEditing, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case lResult of
      mrYes : dmGeneralData.qryMetaData.Post;
      mrNo  : dmGeneralData.qryMetaData.Cancel;
      mrCancel : Exit;
    end; // case
  end;
  dmGeneralData.qryMetadata.Close;
  dmGeneralData.qryMetadata.Open;
  SetGridDisplay;
  EditMode(False);
  if not (FToEdit or (dmGeneralData.qryMetadata.RecordCount = 0)) then begin
    ModalResult := mrNone;
    raise TExceptionPath.CreateNonCritical(ResStr_AllInfoMissing);
  end;
end;

//==============================================================================
procedure TdlgMetadata.bbMetaDataEditClick(Sender: TObject);
begin
  inherited;
  EditMode(True);
  mmInfo.Setfocus;
end;

//==============================================================================
procedure TdlgMetadata.bbAcceptClick(Sender: TObject);
begin
  inherited;
  if not Validate then Exit;
  EditMode(False);
  with dmGeneralData.qryAllPurpose do
  begin
    if Active then Close;
    SQL.Clear;
    SQL.Add('UPDATE Special_XML_Element');
    SQL.Add('SET Data=''' + DuplicateCharacters(mmInfo.Lines.Text, #39) + '''');
    SQL.Add('WHERE Name=''' + dmGeneralData.qryMetadata.FieldByName('NAME').AsString + '''');
    SQL.ADD('AND Type=''M''');
    ExecSQL;
  end;
  dmGeneralData.qryMetadata.Refresh;
end;

//==============================================================================
procedure TdlgMetadata.bbDiscardClick(Sender: TObject);
begin
  inherited;
  mmInfo.Lines.Assign(dmGeneralData.qryMetadata.FieldByName('DATA'));
  EditMode(False);
  dbgMetaDataItems.SetFocus;
end;

//==============================================================================
procedure TdlgMetadata.EditMode(iEditState: boolean);
begin
  bbAccept.Enabled := iEditState;
  bbDiscard.Enabled := iEditState;
  mmInfo.Enabled := iEditState;
  bbMetadataEdit.Enabled := not iEditState;
  dbgMetaDataItems.Enabled := not iEditState;
  bbContinue.Enabled := not iEditState;
  bbCancel.Enabled := not iEditState;
end;

//==============================================================================
procedure TdlgMetadata.dsMetadataDataChange(Sender: TObject; Field: TField);
begin
  inherited;
  if not (csDestroying in ComponentState) then
  begin
    if Field = nil then // moving record
      lblMetaItem.Caption := ReadableFormat
         (dmGeneralData.qryMetadata.FieldByName('NAME').AsString);
    mmInfo.Lines.Assign(dmGeneralData.qryMetadata.FieldByName('DATA'));
  end;
end;

//==============================================================================
procedure TdlgMetadata.dbgMetadataItemsDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  inherited;
  with dbgMetaDataItems.Canvas do
    if Column.FieldName = 'NAME' then
      TextOut(Rect.Left + 2, Rect.Top + 2, ReadableFormat(Column.Field.AsString));
end;

//==============================================================================
procedure TdlgMetadata.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  with dmGeneralData.qryMetadata do begin
    Close;
    if FToEdit then SQL.Text := FOriginalSql;
  end;
end;

//==============================================================================
{ disallow characters that would require escaping for use in XML }
function TdlgMetaData.Validate: Boolean;
begin
  Result := True;
  if LastDelimiter(BAD_CHARS, mmInfo.Text) > 0 then
  begin
    MessageDlg(ResStr_BadCharsError, mtWarning, [mbOk], 0);
    mmInfo.SetFocus;
    Result := False;
  end;
end;

end.
