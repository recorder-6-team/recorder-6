//==============================================================================
//  Unit:        Welcome
//
//  Implements:  TdlgWelcome
//
//  Description:
//
//  Author:      Paul Thomas
//  Created:     28 July 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 24 $
//    $Date: 21/03/07 16:27 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Welcome;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, Db, Htmlview, OnlineHelp, Constants,
  ImageListButton, DatabaseAccessADO, GeneralFunctions;

const
  WM_DOCOUNTS = WM_APP + 1;

resourcestring
  SWelcome_Welcome             = 'Welcome to %s';
  SWelcome_UserName            = 'User Name';
  SWelcome_CopyIdentifier      = 'Copy Identifier';
  SWelcome_AccessLevel         = 'Access Level';
  SWelcome_DatabaseContent     = 'The database you are using contains';
  SWelcome_Counting            = 'Counting...';
  SWelcome_Locations           = 'Locations';
  SWelcome_NamesAddresses      = 'Names &amp; addresses';
  SWelcome_References          = 'References';
  SWelcome_Surveys             = 'Surveys';
  SWelcome_SpeciesObservations = 'Species observations';
  SWelcome_HabitatObservations = 'Habitat observations';
  SWelcome_Tips                = 'Tips';

type
  TdlgWelcome = class(TForm)
    llbQuickStartChoices: TLabel;
    pnlQuickStart: TPanel;
    bbSpecies: TBitBtn;
    bbSite: TBitBtn;
    bbReport: TBitBtn;
    pmSpecies: TPopupMenu;
    pnlHTML: TPanel;
    cbShowWelcomeAtStart: TCheckBox;
    pnlHTMLViewer: TPanel;
    HTMLViewer: THTMLViewer;
    Bevel1: TBevel;
    ImageListButton1: TImageListButton;
    procedure HTMLViewerHotSpotClick(Sender: TObject; const SRC: String;
      var Handled: Boolean);
    procedure bbSpeciesClick(Sender: TObject);
    procedure bbSiteClick(Sender: TObject);
    procedure bbReportClick(Sender: TObject);
    procedure pmSpeciesItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FHTML: TStringList;
    Help: TOnlineHelp;
    procedure PopulateSpeciesMenu;
    Procedure LoadHTML;
    Function GetTip: string;
    function GetTableCount(istTable: string): integer;
    procedure WMDoCounts(var message : TMessage); message WM_DOCOUNTS;
  public
    { Public declarations }
    QuickStartOption:byte;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  FormActions, Maintbar, ApplicationSettings, GeneralData;

//==============================================================================
procedure TdlgWelcome.LoadHTML;
var
  lstTip: String;
begin
  {Build HTML}
  with FHTML do
  begin
    Clear;
    Add('<HTML><BODY>');
    Add('<H2 Align="Center"><FONT Color="Red">' + Format(SWelcome_Welcome, [Application.Title]) + '</FONT></H2>');
    Add('<CENTER><TABLE Width="80%"><TR><TD><FONT  Color="Teal">' + SWelcome_UserName + ':</FONT></TD>');
    Add('<TD>'+ dmGeneralData.GetIndividualName(AppSettings.UserID) +'</TD></TR>');
    Add('<TR><TD><FONT  Color="Teal">' + SWelcome_CopyIdentifier + ':</FONT></TD>');
    Add('<TD>'+ Appsettings.SiteID +'</TD></TR>');
    Add('<TR><TD><FONT Color="Teal">' + SWelcome_AccessLevel + ':</FONT></TD>');
    Add('<TD>'+ AppSettings.UserAccessLevelAsString +'</TD></TR>');
    Add('</TABLE><BR>');
    Add('<TABLE Width="80%"><TR COLSPAN="2" Align="Left"><TH>' + SWelcome_DatabaseContent + ':</TH></TR>');
    Add('<TD><FONT  Color="Teal">' + SWelcome_Locations + ':</FONT></TD>');
    Add('<TD Align="Right">' + SWelcome_Counting + '</TD></TR>');
    Add('<TR><TD><FONT  Color="Teal">' + SWelcome_NamesAddresses + ':</FONT></TD>');
    Add('<TD Align="Right">...</TD></TR>');
    Add('<TR><TD><FONT Color="Teal">' + SWelcome_References + ':</FONT></TD>');
    Add('<TD Align="Right">...</TD></TR>');
    Add('<TR><TD><FONT Color="Teal">' + SWelcome_Surveys + ':</FONT></TD>');
    Add('<TD Align="Right">...</TD></TR>');
    Add('<TR><TD><FONT Color="Teal">' + SWelcome_SpeciesObservations + ':</FONT></TD>');
    Add('<TD Align="Right">...</TD></TR>');
    Add('<TR><TD><FONT Color="Teal">' + SWelcome_HabitatObservations + ':</FONT></TD>');
    Add('<TD Align="Right">...</TD></TR>');
    Add('</TABLE></CENTER>');
    lstTip := GetTip;
    If lstTip <> '' then
        Add('<P><B><A HREF="#Tip">' + SWelcome_Tips + ':</B></A> ' + lstTip + '</P>');
    Add('<HR><CENTER><FONT Size="-2" Color="Teal">' + DateTimeToStr(Now) + '</FONT></CENTER>');
    Add('</BODY></HTML>');
    HTMLViewer.LoadStrings(FHTML);
  end;
end;  // LoadHTML

//==============================================================================
function TdlgWelcome.GetTip: string;
var lslTips: TStrings;
	  i      : Integer;
begin
  if FileExists(ExtractFilePath(Application.ExeName) + 'Help\tips.txt') then begin
    Result := '';
    lslTips := TStringList.Create;
    try
      try
        {file name should come from a "Tips file" registry setting}
        lslTips.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Help\tips.txt');
        if lslTips.Count > 0 then
        begin
          i:= Random(lslTips.Count);
          Result := lslTips[i]
        end;
      except
        Result := '';
      end;
    finally
      lslTips.Free;
    end;
  end; // fileexists
end;  // GetTip

//==============================================================================
procedure TdlgWelcome.HTMLViewerHotSpotClick(Sender: TObject;
  const SRC: String; var Handled: Boolean);
begin
{Only HotSpot is "Tips" so just reload to get another one}
  LoadHTML;
end;  // HTMLViewerHotSpotClick

//==============================================================================
procedure TdlgWelcome.bbSpeciesClick(Sender: TObject);
var PosPopup:TPoint;
begin
  inherited;
  if pmSpecies.Items.Count > 0 then
  begin
    PosPopup:=ClientToScreen(Point(bbSpecies.Left+bbSpecies.Width,bbSpecies.Top));
    pmSpecies.Popup(PosPopup.X,PosPopup.Y);
  end else
  begin
    QuickStartOption := 1;
    Close;
  end;
end;  // bSpeciesClick

//==============================================================================
procedure TdlgWelcome.PopulateSpeciesMenu;
var
  lMenuItem: TMenuItem;
  i: integer;
begin
  i := 0;
  while frmMain.mnuDataEntrySpecies.items[i].Caption <> '-' do
  begin
    lMenuItem := TMenuItem.Create(Self);
    lMenuItem.Caption   := frmMain.mnuDataEntrySpecies.items[i].Caption;
    lMenuItem.ImageIndex:= frmMain.mnuDataEntrySpecies.items[i].ImageIndex;
    lMenuItem.OnClick   := pmSpeciesItemClick;
    pmSpecies.Items.Add(lMenuItem);
    inc(i);
  end;
end;  // PopulateSpeciesMenu

//==============================================================================
procedure TdlgWelcome.bbSiteClick(Sender: TObject);
begin
  QuickStartOption:=2;
  Close;
end;  // bbSiteClick

//==============================================================================
procedure TdlgWelcome.bbReportClick(Sender: TObject);
begin
  QuickStartOption:=3;
  Close;
end;  // bbReportClick

//==============================================================================
function TdlgWelcome.GetTableCount(istTable: string): integer;
begin
  with dmGeneralData.qryAllPurpose do
  begin
    SQL.Text := 'Select count(*) from ' + istTable;
    try
      ParseSQL := False;
      Open;
      Result := Fields[0].AsInteger;
    finally  // must ALWAYS close this query
      Close;
      ParseSQL := True;
    end;
  end;
end;  // GetTableCount

//==============================================================================
procedure TdlgWelcome.pmSpeciesItemClick(Sender: TObject);
begin
  Hide;
  frmMain.mnuRecordingCardClick(Sender);
  Close;
end;  // pmSpeciesItemClick

//==============================================================================
procedure TdlgWelcome.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AppSettings.ShowWelcomeAtStart := cbShowWelcomeAtStart.Checked;
end;


{===============================================================================
 Description : On receipt of a message (so that the form is displayed)l, start
             doing the table counts and refreshing the HTML one table at a time.
 Created : 29/1/2003 }
procedure TdlgWelcome.WMDoCounts(var message: TMessage);
var
  lCursor : TCursor;
    procedure DoTableCount(TableName : string);
    begin
        FHtml.Text := StringReplace(FHtml.Text, SWelcome_Counting,
             Format('%10.0n',[GetTableCount(TableName)*1.0]), [rfIgnoreCase]);
        // next table now shows as counting
        FHtml.Text := StringReplace(FHtml.Text, '...', SWelcome_Counting, [rfIgnoreCase]);
        HTMLViewer.LoadStrings(FHTML);
        Application.ProcessMessages;
    end;
begin
  lCursor := HourglassCursor;
  try
    DoTableCount('Location');
    DoTableCount('Name');
    DoTableCount('Reference');
    DoTableCount('Survey');
    DoTableCount('Taxon_Occurrence');
    DoTableCount('Biotope_Occurrence');
  finally
    DefaultCursor(lCursor);
  end; // try
end;

{===============================================================================
 Description : Initialise the form and kick the counting process off
 Created : 29/1/2003 }
constructor TdlgWelcome.Create(AOwner: TComponent);
begin
  inherited;
    //Random tip generator
  Randomize;

  FHTML := TStringList.Create;
  PopulateSpeciesMenu;
  LoadHTML;
  QuickStartOption :=0;

  //Security
  if pmSpecies.Items.Count = 0 then
    bbSpecies.Enabled:= (AppSettings.UserAccessLevel >= ualAdmin)
  else
    bbSpecies.Enabled:= (AppSettings.UserAccessLevel >= ualRecorder);

  { Help Setup - needs a separate help object because the Welcome Window
   is not a child of the main MDI window. }
  Help := TOnlineHelp.Create(Self.Handle);
  Self.OnHelp := Help.OnHelpReplacement;
  Self.HelpContext := IDH_WELCOME;
  //Show Screen Again
  cbShowWelcomeAtStart.Checked := AppSettings.ShowWelcomeAtStart;
  // kick off the counting process.
  PostMessage(Self.Handle, WM_DOCOUNTS, 0, 0);
end;

{===============================================================================
 Description : Destructor - cleanup
 Created : 29/1/2003 }
destructor TdlgWelcome.Destroy;
begin
  FHTML.Free;
  Help.Free;
  inherited;
end;

end.
