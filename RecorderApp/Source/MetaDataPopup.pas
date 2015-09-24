//==============================================================================
//  Unit:        MetaDataPopup
//
//  Implements:  TdlgMetaDataPopup
//
//  Description: Displays metadata about the currently active data.
//
//  Author:      Michael Bailey
//  Created:     12 February 2001
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 15 $
//    $Date: 17/07/09 14:26 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit MetaDataPopup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, StdCtrls, Buttons, Htmlview, JNCCDataSets, DataClasses, ExtCtrls,
  Menus;

resourcestring
  ResStr_MetadataDetail     = 'Metadata for %s: %s.';
  ResStr_RecordCreation     = 'The record was created by %s on %s';
  ResStr_LastChanged        = ' and was last changed by %s on %s';
  ResStr_UniqueIdentifier   = 'The unique record identifier is %s';
  ResStr_ImportedRecordType = 'This %s was imported from database %s';
  ResStr_HeldCustody        = 'The custody of this %s is held by Site ID %s';

type
  TdlgMetaDataPopup = class(TForm)
    pnlHTMLViewer: TPanel;
    hvMetadata: THTMLViewer;
    pmCopy: TPopupMenu;
    mnuCopy: TMenuItem;
    pnlButton: TPanel;
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
  public
    procedure ShowStandard(const RecordType, RecordLabel: string;
      const AKey: TKeyString; qrySome: TJNCCQuery);
    procedure DisplayStringList(AStringList: TStringList);
    procedure ShowCustom(const AHeader, ABody: string);
  end;

function MetaDataPaneItem(const iTitle, iText: string): string;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, GeneralData;

const
  HTML_TEMPLATE=
    '<HTML> '
    + '<BODY> '
    + '</BODY> '
    + '<TABLE WIDTH="100%%" BORDER="0"> '
    + '<TR> '
    + '<TD><HR>'
    + '<FONT style=''font-family:"Arial";font-size:12pt;font-weight:bold;''>%s</FONT><HR></TD> '
    + '</TR> '
    + '<TR> '
    + '<TD style="PADDING-TOP: 12px"><FONT style=''font-family:"Arial";font-size:10pt;font-weight:normal;''>%s</FONT></TD> '
    + '</TR> '
    + '</TABLE> '
    + '</HTML>';

//==============================================================================
procedure TdlgMetaDataPopup.btnCloseClick(Sender: TObject);
begin
  Close;
end;  // btnCloseClick

//==============================================================================
{Takes a StringList containing HTML and displays it in the HTMLViewer, showing
the (modal) dialogue box}
procedure TdlgMetaDataPopup.DisplayStringList(AStringList: TStringList);
begin
  hvMetadata.LoadStrings(AStringList);
  ShowModal;
end;  // DisplayStringList

//==============================================================================
{Takes two strings a simple header and a main body which should include all
paragraph marks.  The method embeds these strings into an HTML StringList and
calls DisplayStringList}
procedure TdlgMetaDataPopup.ShowCustom(const AHeader, ABody: string);
var AStringList: TStringList;
begin
  AStringList := TStringList.Create;
  with AStringList do
    try
      Add(Format(HTML_TEMPLATE, [AHeader, ABody]));
      DisplayStringList(AStringList);
    finally
      Free;
    end;
end;  // ShowCustom

//==============================================================================
{Creates a standard metadata header and body to pass to ShowCustom.}
procedure TdlgMetaDataPopup.ShowStandard(const RecordType, RecordLabel: string;
  const AKey: TKeyString; qrySome: TJNCCQuery);
var
  header, body: string;
begin
  qrySome.Close;
  qrySome.Open;

  header := Format('<P>' + ResStr_MetadataDetail, [RecordType, RecordLabel]);

  body := Format(
      '<P>' + ResStr_RecordCreation,
      [dmGeneralData.GetIndividualName(qrySome.FieldByName('Entered_By').AsString),
       DateToStr(qrySome.FieldByName('Entry_Date').AsDateTime)]);

  if qrySome.FieldByName('Changed_By').AsString <> '' then
    body := Format(
        body + ResStr_LastChanged,
        [dmGeneralData.GetIndividualName(qrySome.FieldByName('Changed_By').AsString),
         DateToStr(qrySome.FieldByName('Changed_Date').AsDateTime)]);
  body := body + '.</P>';
  body := body + '<P>' + Format(ResStr_UniqueIdentifier, [AKey]) + '.</P>';

  if Copy(AKey, 1, 8) <> AppSettings.SiteID then
    body := body + '<P>'
        + Format(ResStr_ImportedRecordType, [RecordType, Copy(AKey, 1, 8)])
        + '.</P>';

  if Copy(AKey, 1, 8) <> qrySome.FieldByName('Custodian').AsString then
    body := body + '<P>'
        + Format(ResStr_HeldCustody, [RecordType, qrySome.FieldByName('Custodian').AsString])
        + '.</P>';

  ShowCustom(header, body);
end;  // ShowStandard

//==============================================================================
{ Returns a piece of HTML suitable for display in the metadata pane - title is
 bold, followed by the text }
function MetaDataPaneItem(const iTitle, iText: string): string;
begin
  Result := '<P><STRONG>' + iTitle + ':</STRONG> ' + iText + '</P>';
end;  // MetaDataPaneItem

//==============================================================================
procedure TdlgMetaDataPopup.mnuCopyClick(Sender: TObject);
var
  lAllSelected: boolean;
begin
  if hvMetadata.SelLength = 0 then begin
    hvMetadata.SelectAll;
    lAllSelected := True;
  end  else
    lAllSelected := False;
  hvMetadata.CopyToClipboard;
  if lAllSelected then hvMetadata.SelLength := 0;
end;

end.
