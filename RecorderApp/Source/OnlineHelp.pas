unit OnlineHelp;

interface

uses Windows, SysUtils, Dialogs, ApplicationSettings;

function HtmlHelp(hwndCaller: HWND;  // Parent window handle - 0 for no parent
                  pszFile: PChar;       // Help file name and location
                  uCommand: Longint;    // Flags
                  dwData: DWord): Integer; stdcall; external 'HHCtrl.ocx' name 'HtmlHelpA';

type TOnlineHelp = class
private
  FAppHandle : HWND;
  function HelpFileExists: Boolean;
public
  constructor Create(Handle: HWND);
  function OnHelpReplacement(Command: Word; Data: Integer; var CallHelp: Boolean): Boolean;
  procedure ShowHelp(Context: Integer);
  procedure ShowContext(Context: Integer);
  property AppHandle : HWND read FAppHandle write FAppHandle;
end;

var
  OHelp : TOnlineHelp;
const
  IDH_OVERVIEW : Integer = 1;
  IDH_LOGIN: Integer = 2;
  IDH_WELCOME: Integer = 3;
  IDH_RUCKSACK: Integer = 4;
  IDH_EDITMENU: Integer = 5;
  IDH_DATAENTRYMENU: Integer = 6;
  IDH_DICTIONARIESMENU: Integer = 7;
  IDH_MAPMENU: Integer = 8;
  IDH_HELPMENU: Integer = 9;
  IDH_TOOLSMENU: Integer = 10;
  IDH_OPTIONS: Integer = 11;
  IDH_OPTIONSGENERAL: Integer = 12;
  IDH_OPTIONSMAIN: Integer = 13;
  IDH_OPTIONSAPP: Integer = 14;
  IDH_OPTIONSFILES: Integer = 15;
  IDH_OPTIONSSPATIAL: Integer = 16;
  IDH_OPTIONSDEFAULT: Integer = 17;
  IDH_FIND: Integer = 18;
  IDH_FINDLOCATION: Integer = 19;
  IDH_FINDSAMPLE: Integer = 20;
  IDH_FINDSURVEYEVENT: Integer = 21;
  IDH_FINDTAXON: Integer = 22;
  IDH_FINDDOCUMENT: Integer = 23;
  IDH_FINDBIOTOPE: Integer = 24;
  IDH_FINDINDIVIDUAL: Integer = 25;
  IDH_FINDORGANISATION: Integer = 26;
  IDH_NEWRECCARD: Integer = 27;
  IDH_PLACECARD: Integer = 28;
  IDH_NAMEADDR: Integer = 29;
  IDH_OBSERVATIONS: Integer = 30;
  IDH_SURVEYS: Integer = 31;
  IDH_SURVEYSGENERAL: Integer = 32;
  IDH_SURVEYSGEOGRAPHY: Integer = 33;
  IDH_SURVEYSSOURCES: Integer = 34;
  IDH_LOCATION: Integer = 35;
  IDH_LOCATIONGENERAL: Integer = 36;
  IDH_LOCATIONDESIG: Integer = 37;
  IDH_LOCATIONMEASURES: Integer = 38;
  IDH_LOCATIONSOURCES: Integer = 39;
  IDH_LOCATIONAAS: Integer = 40;
  IDH_LOCATIONGRIDSQUS: Integer = 41;
  IDH_LOCATIONLANDP: Integer = 42;
  IDH_LOCATIONBOUNDS: Integer = 43;
  IDH_LOCATIONRELATION: Integer = 44;
  IDH_LOCATIONUSES: Integer = 45;
  IDH_LOCATIONTENURE: Integer = 46;
  IDH_LOCATIONAPPROACH: Integer = 47;
  IDH_NAMEADDRINDIVS: Integer = 48;
  IDH_NAMEADDRORGS: Integer = 49;
  IDH_NAMEADDRADDRS: Integer = 50;
  IDH_NAMEADDRCONTACT: Integer = 51;
  IDH_NAMEADDRCOMMS: Integer = 52;
  IDH_NAMEADDRASSOCS: Integer = 53;
  IDH_NAMEADDRBIOG: Integer = 54;
  IDH_NAMEADDRSOURCES: Integer = 55;
  IDH_DOCS: Integer = 56;
  IDH_DOCSGENERAL: Integer = 57;
  IDH_DOCSDETAILS: Integer = 58;
  IDH_DOCSOTHER: Integer = 59;
  IDH_SPECIESCARD: Integer = 60;
  IDH_MAPDATASET: Integer = 61;
  IDH_MAPBROWSER: Integer = 62;
  IDH_REPORTOPEN: Integer = 63;
  IDH_REPORT: Integer = 64;
  IDH_TAXONDICT: Integer = 66;
  IDH_BIOTOPEDICT: Integer = 67;
  IDH_AADICT: Integer = 68;
  IDH_NEWTAXA: Integer = 69;
  IDH_NEWTAXAGENERAL: Integer = 70;
  IDH_NEWTAXASTATUSES: Integer = 71;
  IDH_NEWTAXAFACTS: Integer = 72;
  IDH_NEWTAXASOURCES: Integer = 73;
  IDH_NEWBIOTOPE: Integer = 74;
  IDH_NEWBIOTOPEGENERAL: Integer = 75;
  IDH_NEWBIOTOPEFACTS: Integer = 76;
  IDH_NEWBIOTOPESOURCES: Integer = 77;
  IDH_MAPCONFIG: Integer = 78;
  // IDH_WIZARD: Integer = 79;  // Not Used
  IDH_WIZARDSTYLE: Integer = 80;
  IDH_WIZARDSSELECTOR: Integer = 81;
  IDH_WIZARDSSELECTION: Integer = 82;
  IDH_WIZARDAA: Integer = 83;
  IDH_WIZRADTAXASELECT: Integer = 84;
  IDH_WIZRADBIOSELECT: Integer = 85;
  IDH_WIZARDPLACESSELECTOR: Integer = 86;
  IDH_WIZARDPLACESSELECTION: Integer = 87;
  IDH_WIZARDSPATIAL: Integer = 88;
  IDH_WIZARDINFOSELECTOR: Integer = 89;
  IDH_WIZARDINFOSELECTION: Integer = 90;
  IDH_WIZARDLAYOUT: Integer = 91;
  IDH_WIZARDSELECTATTRIB: Integer = 92;
  IDH_WIZARDSORTATTRIB: Integer = 93;
  IDH_WIZARDSUMMARY: Integer = 94;
  IDH_TERMLISTS: Integer = 95;
  IDH_USERCONFIG: Integer = 96;
  IDH_PASSWORD: Integer = 97;
  IDH_DICTINSTALL: Integer = 98;
  IDH_IMPORTDATA: Integer = 99;
  IDH_MERGEDATA: Integer = 100;
  IDH_ADDIN: Integer = 101;
  IDH_DATAEXPORT: Integer = 102;
  IDH_FILTER: Integer = 103;
  IDH_SAMPLEGENERAL: Integer = 104;
  IDH_SAMPLERECORDER: Integer = 105;
  IDH_SAMPLEMEASURES: Integer = 106;
  IDH_SAMPLERELATIONS: Integer = 107;
  IDH_SAMPLESOURCES: Integer = 108;
  IDH_EVENTGENERAL: Integer = 109;
  IDH_EVENTSOURCES: Integer = 110;
  IDH_TAXONGENERAL: Integer = 111;
  IDH_TAXONDETS: Integer = 112;
  IDH_TAXONMEASURES: Integer = 113;
  IDH_TAXONRELATEDOCCS: Integer = 114;
  IDH_TAXONSPECIMENS: Integer = 115;
  IDH_TAXONSOURCES: Integer = 116;
  IDH_BIOTOPEGENERAL: Integer = 117;
  IDH_BIOTOPEMEASURES: Integer = 118;
  IDH_BIOTOPEDETS: Integer = 119;
  IDH_BIOTOPESOURCES: Integer = 120;
  IDH_ADDFILTERS: Integer = 121;
  IDH_DUPLICATERECORDS: Integer = 123;
  IDH_FEATUREGENERAL: Integer = 124;
  IDH_FEATUREAIMS: Integer = 125;
  IDH_FEATURETHREATS: Integer = 126;
  IDH_FEATUREDAMAGES: Integer = 127;
  IDH_FEATURESOURCES: Integer = 128;
  IDH_AUTHORINPUT: Integer = 129;
  IDH_SORT: Integer = 130;
  IDH_SAMPLETYPES: Integer = 131;
  IDH_REGISTERMAP: Integer = 132;
  IDH_ADDFILTERSCONDITIONS: Integer = 133;
  IDH_ADDFILTERSCONSTRAINTS: Integer = 134;
  IDH_FILTERRESULT: Integer = 135;
  IDH_REPORTDESIGN: Integer = 136;
  IDH_VIEWMENU: Integer = 137;
  IDH_FILEMENU: Integer = 138;
  IDH_REPORTSMENU: Integer = 139;
  IDH_WINDOWMENU: Integer = 140;
  IDH_FINDADMINAREA: Integer = 141;
  IDH_FINDNAME: Integer = 142;
  IDH_FINDREF: Integer = 143;
  IDH_FINDTAXOCC: Integer = 144;
  IDH_FINDFEATURE: Integer = 145;
  IDH_FINDNEARGRIDREF: Integer = 146;
  IDH_METADATA: Integer = 147;
  IDH_MAPOPTIONSDIST : Integer = 148;
  IDH_MAPOPTIONSBACK : Integer = 149;
  IDH_MAPOPTIONSPOLY : Integer = 150;
  IDH_MAPOPTIONSBASE : Integer = 151;
  IDH_SCHEMEMANAGER : Integer = 152;
  IDH_TAXONGROUPS : Integer = 153;     // on Edit Taxon Details
  IDH_SELECTBYPOLYGON : Integer = 154; // on Report Wizard
  IDH_TAXONNAMES : Integer = 155;      // On Edit Taxon Details
  IDH_FINDKEYWORD: Integer = 156;
  IDH_ENHANCEDTERMLISTS: Integer = 157;
  IDH_EVENTOWNERS: Integer = 158;
  IDH_NAMEDEPTS: Integer = 159;
  IDH_DOCSKEYWORDS: Integer = 160;
  IDH_BATCHUPDATES: Integer = 161;
  IDH_SURVEYTAGSGENERAL : Integer = 162;
  IDH_SURVEYTAGS        : Integer = 163;
  IDH_VALIDATIONRESULTS : Integer = 164;
  IDH_IMPORTRESULTS     : Integer = 165;
  IDH_CASCADINGUPDATE   : Integer = 166;
  IDH_MAPNAVIGATOR      : Integer = 167;
  IDH_IMPORTWIZARD      : Integer = 168;
  IDH_IWFILESELECT      : Integer = 169; // first Import Wizard page, with select Template combo
  IDH_IWFIXEDWIDTHS     : Integer = 170; // Import Wizard - fixed widths text import page
  IDH_IWCOLUMNTYPES     : Integer = 171; // Import Wizard - columns matching page
  IDH_IWMISSINGDATA     : Integer = 172; // Import Wizard - page requesting additional missing data
  IDH_IWMATCHGENERIC    : Integer = 173; // Import Wizard - matching pages
  IDH_IWMATCHLOCATIONS  : Integer = 174; // Import Wizard - locations matching page
  IDH_IWIMPORTANALYSIS  : Integer = 175; // Import Wizard - Import Wizard analysis page
  IDH_IWLOCATIONCOLTYPE : Integer = 176; // Import Wizard - Location column types



implementation

const
  HH_DISPLAY_TOPIC : Longint = $0;
  HH_KEYWORD_LOOKUP : Longint = $D;
  HH_DISPLAY_TEXT_POPUP : Longint = $E;      // display string resource id or text in a popup window
  HH_HELP_CONTEXT : Longint = $F;            // display mapped numeric value in dwData
  HH_TP_HELP_CONTEXTMENU : Longint = $10;    // text popup help, same as WinHelp HELP_CONTEXTMENU
  HH_TP_HELP_WM_HELP : Longint = $11;        // text popup help, same as WinHelp HELP_WM_HELP
  HH_CLOSE_ALL : Longint = $12;              // close all windows opened directly or indirectly by the caller
  HH_ALINK_LOOKUP : Longint = $13;           // ALink version of HH_KEYWORD_LOOKUP
  HH_INITIALIZE : Longint = $1C;             // Initializes the help system.
  HH_UNINITIALIZE : Longint = $1D;           // Uninitializes the help system.
  HH_SYNC : Longint = $0009;                 // Synchronizes Nagigation Pane to Topic Pane

resourcestring
  ResStr_CannotFindHelp = 'Could not find Help File:';
  ResStr_IncorrectHelpFormat = 'Help File is not in the correct format';

constructor TOnlineHelp.Create(Handle: HWND);
  begin
    inherited Create;
    AppHandle:=Handle;
  end;

function TOnlineHelp.OnHelpReplacement(Command: Word; Data: Integer; var CallHelp: Boolean): Boolean;
{ Displays Topic corresponding to the current Frame's HelpContext Property
  If the Context is invalid, the default help page comes up }
  begin
    CallHelp := False;
    ShowContext(Data);
    result := True;
  end;

procedure TOnlineHelp.ShowHelp(Context: Integer);
{ Displays Topic corresponding to the passed Help Context constant in the default Help Window }
begin
    if HelpFileExists then
      if HtmlHelp(FAppHandle , PChar(AppSettings.HelpPath + '>Rec2000'), HH_HELP_CONTEXT, Context) = 0 then
        {If the context id is invalid, no help is displayed, and the function returns 0 for the window handle}
        {If this happens, then display the default topic}
        HtmlHelp(FAppHandle, PChar(AppSettings.HelpPath), HH_DISPLAY_TOPIC, 0);
end;

procedure TOnlineHelp.ShowContext(Context: Integer);
begin
  if HelpFileExists then
    if HtmlHelp(FAppHandle , PChar(AppSettings.HelpPath + '>Context'), HH_HELP_CONTEXT, Context) = 0 then
      HtmlHelp(FAppHandle, PChar(AppSettings.HelpPath + '>Context'), HH_DISPLAY_TOPIC, 0);
end;

function TOnlineHelp.HelpFileExists: Boolean;
begin
    if not FileExists(AppSettings.HelpPath)then
      MessageDlg(ResStr_CannotFindHelp + #13 + AppSettings.HelpPath, mtError, [mbOK], 0)
    else
      if StrPos(PChar(AppSettings.HelpPath), PChar('.chm')) = nil then
        MessageDlg(ResStr_IncorrectHelpFormat + #13 + AppSettings.HelpPath, mtError, [mbOK], 0)
      else
        begin
         HelpFileExists := True;
         Exit;
        end;
    result := False;
end;

end.
