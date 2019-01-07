unit MultipleMatches;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImageListButton, StdCtrls, DatabaseAccessADO,DataClasses,GeneralFunctions;


 resourcestring
  ResStr_No_Further_Info = 'No further information available';

type
  TdlgMultipleMatches = class(TForm)
    lbMultiMatches: TListBox;
    btnAction: TButton;
    bbCancel: TImageListButton;
    memDetailedNotes: TMemo;
    lblInputValue: TLabel;
    lblImportValue: TLabel;
    cbExtraInfo: TCheckBox;
    procedure btnActionClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure lbMultiMatchesClick(Sender: TObject);
    procedure cbExtraInfoClick(Sender: TObject);

  private
    procedure displayExtraInfo(Details: Integer);
  public

  end;

var
  dlgMultipleMatches: TdlgMultipleMatches;

implementation
{$R *.dfm}
uses basematchpage, GeneralData;


procedure TdlgMultipleMatches.btnActionClick(Sender: TObject);
var ListItemKey: string;
    iIndex : integer;
begin
  ListItemKey := '';
  iIndex := lbMultiMatches.ItemIndex;
  if iIndex > -1 then
    ListItemKey  := TKeyData(lbMultiMatches.Items.Objects[iIndex]).ItemKey;
  if ListItemKey = '' then
    ModalResult:= mrNone // dont allow out if nothing selected
  else begin
    dmDatabase.RunStoredProc
    (UpdateProcedure, ['@ImportValue', InputValue,
    '@MatchKey',ListItemKey,'@ManualMatch',1,'@Remembered',0,'@MatchCount',1]);
    ModalResult:= mrOK;
  end;
end;

procedure TdlgMultipleMatches.FormShow(Sender: TObject);
var lKey : TkeyData;
    lCursor : TCursor;
    lSelected : Integer;
begin
  lCursor := HourglassCursor;
  lblImportValue.Caption := InputValue;
  lSelected := -1;
  with dmDatabase.GetRecordset(DisplayProcedure, ['@key', InputValue]) do
  begin
    while not Eof do begin
      lKey := TKeyData.Create;
      lKey.Itemkey :=  Fields['AKey'].Value;
      lbMultiMatches.Items.AddObject(Fields['FULLDETAILS'].Value,lkey);
      if (Fields['AKey'].Value = Fields['MatchKey'].Value) and
         (lSelected = -1 ) then
       lSelected := lbMultiMatches.Count-1;
       movenext;
    end;
  end;
  close;
  if lbMultiMatches.Items.Count = 0 then begin
    lKey := TKeyData.Create;
    lKey.Itemkey :=  '';
    lbMultiMatches.Items.AddObject(ResStr_No_Further_Info,lkey);
  end;
  if lSelected > -1 then
  begin
    lbMultiMatches.ItemIndex := lSelected;
    displayExtraInfo(0);
  end;

  DefaultCursor(lCursor);

end;

procedure TdlgMultipleMatches.bbCancelClick(Sender: TObject);
begin
    ModalResult:= mrCancel;
end;

procedure TdlgMultipleMatches.lbMultiMatchesClick(Sender: TObject);
begin
  if cbExtraInfo.Checked  then
    displayExtraInfo(1)
  else
    displayExtraInfo(0);

end;

procedure TdlgMultipleMatches.displayExtraInfo(Details: Integer);
var ListItemKey: string;
    iIndex : integer;
    lCursor : TCursor;
begin
  iIndex := lbMultiMatches.ItemIndex;
  ListItemKey := '';
  If iIndex > -1 then
    ListItemKey  := TKeyData(lbMultiMatches.Items.Objects[iIndex]).ItemKey;
    if  ListItemKey  <> '' then begin
      lCursor := HourglassCursor;
      if DetailsProcedure = '' then
        memDetailedNotes.Lines.Text := ResStr_No_Further_Info
      else begin
        with dmDatabase.GetRecordset(DetailsProcedure, ['@key',ListItemKey,'@Detail',Details, '@ImportValue',lblImportValue.Caption]) DO
        begin
          while not Eof do begin
            memDetailedNotes.Lines.Text := Fields['DETAILS'].Value;
            movenext;
          end;
        end;
        close;
    end;
    ModalResult:= mrNone;
    DefaultCursor(lCursor);
  end;
end;


procedure TdlgMultipleMatches.cbExtraInfoClick(Sender: TObject);
begin
  if lbMultiMatches.ItemIndex > -1 then begin
    if cbExtraInfo.Checked then
      displayExtraInfo(1)
    else
      displayExtraInfo(0)
  end;
end;


end.


