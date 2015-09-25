//==============================================================================
//  Unit: CustodyTransfer
//
//  Description: Used to transfer custody of all data with the current site ID.
//
//  Author: Michael Bailey
//  Created: Dec 2002
//
//  Last Revision Details:
//    $Revision: 7 $
//    $Date: 12/12/07 13:46 $
//    $Author: Rickyshrestha $
//
//  $History: CustodyTransfer.pas $
//  
//  *****************  Version 7  *****************
//  User: Rickyshrestha Date: 12/12/07   Time: 13:46
//  Updated in $/JNCC/Development/Build/Source
//  Changes some constants to resourcestring
//   ResStr_NotCurrentSite,
//    ResStr_NotValidID,
//    ResStr_Warning,
//   ResStr_InProgress = 'Transfer of Data Custody in progress.';
//    ResStr_GoTable = 'Transferring Custody of Data in Table : %s.';
//    ResStr_Complete
//  
//  *****************  Version 6  *****************
//  User: Ericsalmon   Date: 7/01/03    Time: 15:48
//  Updated in $/JNCC/Source
//  Switch OK/Cancel buttons around.
//  
//  *****************  Version 5  *****************
//  User: Michaelbailey Date: 20/12/02   Time: 16:35
//  Updated in $/JNCC/Source
//  Probably needs a few more comments, but I don't have a lot of time
//  left.
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================


unit CustodyTransfer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ExtCtrls, ImageListButton, RestrictedEdits;

type
  TdlgCustodyTransfer = class(TForm)
    lblExplanation: TLabel;
    bvlExplanation: TBevel;
    lblCurrentSiteID: TLabel;
    lblNewCustodian: TLabel;
    btnCancel: TImageListButton;
    btnOk: TImageListButton;
    eCurrentSite: TRestrictedEdit;
    eNewCustodian: TRestrictedEdit;
    procedure btnOkClick(Sender: TObject);
    procedure eCheckText(Sender: TObject; const iText: String; var ioAccept: Boolean);
  private
    procedure Validate;
  public
    procedure NextTable(const TableName: string);
  end;

implementation

{$R *.dfm}

uses
  ApplicationSettings, ExceptionForm, GeneralFunctions, Custody, GeneralData, Maintbar;

resourcestring
  ResStr_NotCurrentSite = 'The Site ID you have entered does not match the current Site ID.';
  ResStr_NotValidID = 'Invalid Site ID.  All Site IDs consist of 8 alphanumeric characters.';

  ResStr_Warning = 'This will transfer custody of data that is currently in the custody of this ' +
                'site to Site ID %s.  You will not be able to modify existing data after the ' +
                'transfer.  Please ensure you have typed the Site ID receiving custody ' +
                'correctly.  Transfer custody of all data to %s?';

  ResStr_InProgress = 'Transfer of Data Custody in progress.';
  ResStr_GoTable = 'Transferring Custody of Data in Table : %s.';
  ResStr_Complete = 'Transfer of Data Custody complete.';


procedure TdlgCustodyTransfer.btnOkClick(Sender: TObject);
var lCursor: TCursor;
begin
  Validate;
  if DefMessageDlg(Format(ResStr_Warning, [eNewCustodian.Text, eNewCustodian.Text]),
                    mtWarning, mbOKCancel, mbCancel, 0) = idOK then begin
    lCursor := HourglassCursor;
    try
      btnCancel.Enabled := False;
      btnOK.Enabled := False;
      eCurrentSite.Enabled := False;
      eNewCustodian.Enabled := False;
      lblExplanation.Caption := ResStr_InProgress;
      frmMain.ProgressBar.Min := 0;
      frmMain.ProgressBar.Max := TCustodyInfo.CustodyTables;
      frmMain.ProgressBar.Position := 0;
      with TCustodyRelinquisher.Create do try
        OnNextTable := NextTable;
        RelinquishAllCustody(dmGeneralData.qryAllPurpose, eNewCustodian.Text);
      finally Free end;
      frmMain.ProgressBar.Position := 0;
      lblExplanation.Caption := ResStr_Complete;
      btnOK.OnClick := nil;
      btnOK.ModalResult := mrOK;
      btnOK.Enabled := True;
    finally DefaultCursor(lCursor) end;
  end;
end;

procedure TdlgCustodyTransfer.eCheckText
  (Sender: TObject; const iText: String; var ioAccept: Boolean);
var k: Integer;
begin
  ioAccept := Length(iText) <= 8;
  for k := Length(iText) downto 1 do
    ioAccept := ioAccept and (iText[k] in ['A'..'Z', '0'..'9']);
end;

procedure TdlgCustodyTransfer.NextTable(const TableName: string);
begin
  lblExplanation.Caption := Format(ResStr_GoTable, [TableName]);
  frmMain.ProgressBar.Position := frmMain.ProgressBar.Position + 1;
  Application.ProcessMessages;
end;

procedure TdlgCustodyTransfer.Validate;
begin
  if eCurrentSite.Text <> AppSettings.SiteID then begin
    eCurrentSite.SetFocus;
    raise TExceptionPath.CreateNonCritical(ResStr_NotCurrentSite);
  end;
  if Length(eNewCustodian.Text) <> 8 then begin
    eNewCustodian.SetFocus;
    raise TExceptionPath.CreateNonCritical(ResStr_NotValidID);
  end;
end;

end.
