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
  Menus,ADODB, DatabaseAccessADO,Variants;
const
  SQL_OBSERVATIONS =
    ' SELECT ITN.ACTUAL_NAME as EnteredName,ITN.Authority as ' +
    ' EnteredAuth, ITN.ACTUAL_NAME_ATTRIBUTE as Enteredattribute, ' +
    ' TL.Item_Name as EnteredDict, ITN.Taxon_List_Item_Key, ' +
    ' Tocc.Zero_Abundance, ' +
    ' ITN2.ACTUAL_NAME as RecName,ITN2.Authority as RecAuth, ' +
    ' ITN2.ACTUAL_NAME_ATTRIBUTE as Recattribute, ' +
    ' TL2.Item_Name as RecDict, ITN2.Taxon_List_Item_Key as RecTLiKey ' +
    ' FROM TAXON_OCCURRENCE TOCC INNER JOIN TAXON_DETERMINATION TDET ' +
    ' ON TDET.TAXON_OCCURRENCE_KEY = TOCC.TAXON_OCCURRENCE_KEY ' +
    ' AND TDET.PREFERRED = 1 INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY ' +
    ' = TDET.TAXON_LIST_ITEM_KEY ' +
    ' INNER JOIN INDEX_TAXON_NAME ITN2 ON ITN2.TAXON_LIST_ITEM_KEY ' +
    ' = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY ' +
    ' INNER JOIN TAXON_LIST_VERSION TLV ON TLV.TAXON_LIST_VERSION_KEY = ' +
    ' ITN.TAXON_LIST_VERSION_KEY ' +
    ' INNER JOIN TAXON_LIST TL ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY ' +
    ' INNER JOIN TAXON_LIST_VERSION TLV2 ON TLV2.TAXON_LIST_VERSION_KEY = ' +
    ' ITN2.TAXON_LIST_VERSION_KEY ' +
    ' INNER JOIN TAXON_LIST TL2 ON TL2.TAXON_LIST_KEY = TLV2.TAXON_LIST_KEY ' +

    ' WHERE TOCC.TAXON_OCCURRENCE_KEY = ''%s''';
 
resourcestring
  ResStr_MetadataDetail     = 'Metadata for %s: %s.';
  ResStr_RecordCreation     = 'The record was created by %s on %s';
  ResStr_LastChanged        = ' and was last changed by %s on %s';
  ResStr_UniqueIdentifier   = 'The unique record identifier is %s';
  ResStr_ImportedRecordType = 'This %s was imported from database %s';
  ResStr_HeldCustody        = 'The custody of this %s is held by Site ID %s';
  Restr_EnteredName = 'Entered Name is ';
  Restr_EnteredAuthor = 'Entered Author is ';
  Restr_EnteredAttribute  = 'Entered Attribute is ';
  Restr_EnteredDictionary  = 'Entered Dictionary is ';
  Restr_EnteredKey  = 'Entered Key is ';
  Restr_HasZeroAbundance = 'Zero Abundance';
  Restr_NotZeroAbundance = 'Not zero Abundance';
  Restr_RecName = 'Recommended Name is ';
  Restr_RecAuthor = 'Recommended Author is ';
  Restr_RecAttribute  = 'Recommended Attribute is ';
  Restr_RecDictionary  = 'Recommended Dictionary is ';
  Restr_RecKey  = 'Recommended Key is ';

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
  private
    Function getExtraInfo(RecordType: string;
             const AKey: TKeyString):string;
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
  //metadata
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

  body := body + '<P>'
        + getExtraInfo(RecordType,AKey)
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
//==============================================================================
Function TdlgMetaDataPopup.getExtraInfo(RecordType: string;
  const AKey: TKeyString):string;
var
  rs : _Recordset;
  lExtraInfo : string;
begin
  if RecordType = 'Taxon Occurrence' then begin
    rs := dmDatabase.ExecuteSQL(FORMAT(SQL_OBSERVATIONS,[AKEY]), true);
    If not rs.eof then begin
      lExtraInfo :=  '<P>' + Restr_EnteredName + rs.Fields.Item[0].value  + '.</P>';
      lExtraInfo :=  lExtraInfo + '<P>' + Restr_EnteredAuthor  +  vartostr(rs.Fields.Item[1].value)  + '.</P>';
      lExtraInfo :=  lExtraInfo + '<P>' + Restr_EnteredAttribute  +  vartostr(rs.Fields.Item[2].value )  + '.</P>';
      lExtraInfo :=  lExtraInfo + '<P>' + Restr_EnteredDictionary  +  vartostr(rs.Fields.Item[3].value )  + '.</P>';
      lExtraInfo :=  lExtraInfo + '<P>' + Restr_EnteredKey  +  vartostr(rs.Fields.Item[4].value) + '.</P>';
      lExtraInfo :=  lExtraInfo + '<P>' + Restr_RecName + rs.Fields.Item[6].value  + '.</P>';
      lExtraInfo :=  lExtraInfo + '<P>' + Restr_RecAuthor  +  vartostr(rs.Fields.Item[7].value)  + '.</P>';
      lExtraInfo :=  lExtraInfo + '<P>' + Restr_RecAttribute  +  vartostr(rs.Fields.Item[8].value )  + '.</P>';
      lExtraInfo :=  lExtraInfo + '<P>' + Restr_RecDictionary  +  vartostr(rs.Fields.Item[9].value )  + '.</P>';
      lExtraInfo :=  lExtraInfo + '<P>' + Restr_RecKey  +  vartostr(rs.Fields.Item[10].value) + '.</P>';
      if rs.Fields.Item[5].value = true then
        lExtraInfo :=  lExtraInfo + '<P>' + Restr_HasZeroAbundance  + '.</P>'
      else
        lExtraInfo :=  lExtraInfo + '<P>' + Restr_NotZeroAbundance  + '.</P>';


    end;
  end;
  result := lExtraInfo;

end;
end.
