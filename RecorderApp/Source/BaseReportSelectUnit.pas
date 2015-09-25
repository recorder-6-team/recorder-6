//==============================================================================
//  Unit:        BaseReportSelect
//
//  Implements:  TBaseReportSelect
//
//  Description: Base calss for OpenReport and BatchUpdate dialogs.
//
//  Author:      David Kelly
//  Created:     21 Jan 2008
//
//  Last Revision Details:
//    $Revision: 7 $
//    $Date: 15/04/08 9:52 $
//    $Author: Ericsalmon $
//
//==============================================================================
unit BaseReportSelectUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImageListButton, ExtCtrls, ComCtrls, exgrid, RapTree,
  FolderBrowser, TreeColl, StrUtils, OnlineHelp, CRReportIndex, XMLDoc, XMLIntf,
  ExceptionForm, KeyboardRapidTree;

resourcestring
  ResStr_SelectFile              = 'You must select a report file.';
  ResStr_SelectExistingFile      = 'You must select an existing report file.';
  ResStr_NoFileSelected          = 'No file selected.';
  ResStr_ReportWizardReports     = 'Report Wizard Saved Reports';
  ResStr_BiotopeReport           = 'Biotope Report';
  ResStr_PlaceReport             = 'Location Report';
  ResStr_TaxonReport             = 'Taxon Report';
  ResStr_NoReportAvailable       = 'No report available to retrieve filename';
  ResStr_ReportDisplayedUsing    = 'Report is displayed using %s.';
  ResStr_ReportGeneratesSnapshot = 'Report generates the snapshot %s.';
  ResStr_NoDescription           = 'No description available';

type  
  EReportSelectException = class(TExceptionPath);

  TFolderNode = class(TFlyNode)
  public
    constructor Create(AOwner: TTreeCollection); override;
  end;      

  TReportNode = class(TFlyNode)
  private
    FFileName: string;
    procedure SetFileName(const Value: string);
  public
    property FileName: string read FFileName write SetFileName;
  end;

  TXMLReportNode = class(TReportNode)
  public
    constructor Create(AOwner: TTreeCollection); override;
  end;

  TBaseReportSelect = class(TForm)
    btnCancel: TImageListButton;
    btnOK: TImageListButton;
    eDirectory: TEdit;
    dlgFolder: TFolderBrowser;
    btnBrowse: TButton;
    Label2: TLabel;
    Bevel1: TBevel;
    Label1: TLabel;
    pnlFiles: TPanel;
    tvSelectFile: TKeyboardRapidTree;
    Splitter: TSplitter;
    pnlDescription: TPanel;
    rtfDescription: TRichEdit;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject); virtual;
    procedure tvSelectFileDblClick(Sender: TObject);
    procedure tvSelectFileChange(Sender: TObject; Node: TFlyNode);
    procedure SplitterMoved(Sender: TObject);
  private
    hlpOpenReport : TOnlineHelp;
    FDirectory: String;
    FIndex: TBaseReportIndex;
    FDblClickBug: Boolean;
    procedure FillFileList;
    function GetFileName: string;
    procedure AddXMLFiles;
    procedure SetNodeDescription(Node: TFlyNode);
  protected
    procedure AddExtraFiles; virtual;
    function GetDirectory: String; virtual; abstract;
    function GetHelpContext: Integer; virtual; abstract;
    function GetIndex: TBaseReportIndex; virtual; abstract;
    procedure ProcessFile; virtual;
    procedure SetDescriptionTitle(const ATitle: string);
    procedure SetExtraNodeDescription(Node: TFlyNode); virtual; 
    procedure SetNoFileSelectedMessage; virtual;
    procedure SetXMLReportNodeDescription(ANode: TXMLReportNode); virtual;
  public                            
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FileName:string read GetFileName;
  end;

implementation

uses
  ApplicationSettings, GeneralFunctions, GeneralData, CRConstants;

{$R *.dfm}
   
{-------------------------------------------------------------------------------
}
constructor TBaseReportSelect.Create;
begin
  inherited;
  FDirectory := GetDirectory;
  FIndex := GetIndex;
  // Template removed as cannot open reports from Open report screen as set to
  // template path not that of saved reports
  if FDirectory='' then
    eDirectory.Text:=ExtractFilePath(Application.ExeName)
  else
    eDirectory.Text:=FDirectory;

  FillFileList;

  //Help Setup
  hlpOpenReport    := TOnlineHelp.Create(Self.Handle);
  Self.OnHelp      := hlpOpenReport.OnHelpReplacement;
  Self.HelpContext := GetHelpContext;
  SetNoFileSelectedMessage;
end;
    
{-------------------------------------------------------------------------------
}
destructor TBaseReportSelect.Destroy;
begin
  hlpOpenReport.Free;
  inherited;
end;
   
{-------------------------------------------------------------------------------
}
procedure TBaseReportSelect.btnBrowseClick(Sender: TObject);
begin
  dlgFolder.Folder := ExcludeTrailingPathDelimiter(eDirectory.Text);
  if dlgFolder.Execute then begin
    eDirectory.Text := IncludeTrailingPathDelimiter(dlgFolder.Folder);
    FIndex.RebuildIndex(eDirectory.Text);
    FillFileList;
    FIndex.RebuildIndex;
  end;
end;
    
{-------------------------------------------------------------------------------
}
procedure TBaseReportSelect.btnOKClick(Sender: TObject);
begin
  ProcessFile;
end;
   
{-------------------------------------------------------------------------------
}
procedure TBaseReportSelect.ProcessFile;
begin  
  // need to explicitly set result as this is also called by double click on hierarchy
  ModalResult := mrOk;
  if not Assigned(tvSelectFile.Selected) then
  begin
    ShowInformation(ResStr_SelectFile);
    ModalResult := mrNone;
  end
  else
    if (not FileExists(FileName)) then
    begin
      ShowInformation(ResStr_SelectExistingFile);
      ModalResult := mrNone;
    end;
end;
   
{-------------------------------------------------------------------------------
}
function TBaseReportSelect.GetFileName: string;
begin
  if Assigned(tvSelectFile.Selected) and (tvSelectFile.Selected is TReportNode) then
    Result := eDirectory.Text +
              TReportNode(tvSelectFile.Selected).FileName
  else
    raise EReportSelectException.Create(ResStr_NoReportAvailable);
end;  

{-------------------------------------------------------------------------------
  Set the title of the description box, with appropriate formatting
}
procedure TBaseReportSelect.SetDescriptionTitle(const ATitle: string);
begin
  rtfDescription.Lines.Clear;
  rtfDescription.SelAttributes.Size  := Font.Size + 2;
  rtfDescription.SelAttributes.Style := [fsBold];
  rtfDescription.SelAttributes.Color := clHotLight;
  try
    rtfDescription.Lines.Add(ATitle);
  finally
    rtfDescription.SelAttributes.Size  := Font.Size;
    rtfDescription.SelAttributes.Style := [];
    rtfDescription.SelAttributes.Color := clWindowText;
  end;
end;

{-------------------------------------------------------------------------------
  Add a message to the rtf box - no report selected
}
procedure TBaseReportSelect.SetNoFileSelectedMessage;
begin
  rtfDescription.Lines.Clear;
  rtfDescription.SelAttributes.Name  := 'Arial';
  rtfDescription.SelAttributes.Style := [fsItalic];
  rtfDescription.Lines.Add(ResStr_NoFileSelected);
  rtfDescription.SelAttributes.Style := [];
end;

{-------------------------------------------------------------------------------
  Select a node - populate the description memo
}
procedure TBaseReportSelect.tvSelectFileChange(Sender: TObject;
  Node: TFlyNode);
begin
  btnOK.Enabled := (Node is TReportNode);
  rtfDescription.Lines.Clear;
  SetNodeDescription(Node);
end;            
   
{-------------------------------------------------------------------------------
}
procedure TBaseReportSelect.tvSelectFileDblClick(Sender: TObject);
begin
  inherited;
  if not Assigned(tvSelectFile.Selected) then Exit;

  if not (tvSelectFile.Selected is TFolderNode) then begin
    // ES: Dodgy fix, don't have time to look into it further. It works, move along.
    if FDblClickBug then
      FDblClickBug := False
    else begin
      FDblClickBug := True;
      ProcessFile;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TBaseReportSelect.SetNodeDescription(Node: TFlyNode);
begin
  if Node is TXMLReportNode then
    SetXMLReportNodeDescription(TXMLReportNode(Node))
  else
    SetExtraNodeDescription(Node);
end;

{-------------------------------------------------------------------------------
}
procedure TBaseReportSelect.SetExtraNodeDescription(Node: TFlyNode);
begin
  SetNoFileSelectedMessage;
end;

{-------------------------------------------------------------------------------
  Setup the description box for an XML report
}
procedure TBaseReportSelect.SetXMLReportNodeDescription(ANode: TXMLReportNode);
var
  lXMLDoc: IXMLDocument;
  lNode: IXMLNode;
begin
  lXMLDoc := NewXMLDocument;
  lXMLDoc.LoadFromFile(eDirectory.Text + ANode.FileName);
  SetDescriptionTitle(ANode.Caption);
  lNode := lXMLDoc.ChildNodes[FIndex.DocumentNode];
  if lNode.HasAttribute(AT_DESCRIPTION) then
    // XML only stores LF, so convert to CRLF
    rtfDescription.Lines.Add(StringReplace(lNode.Attributes[AT_DESCRIPTION],
        #10, #13#10, [rfReplaceAll]))
  else
    rtfDescription.Lines.Add(ResStr_NoDescription);
  if lNode.HasAttribute(AT_TEMPLATE) then
    rtfDescription.Lines.Add(Format(ResStr_ReportDisplayedUsing, [ExtractWithoutExt(
        lNode.Attributes[AT_TEMPLATE])]));
end;

{-------------------------------------------------------------------------------
}
procedure TBaseReportSelect.FillFileList;
var lCursor: TCursor;
begin
  lCursor := HourglassCursor;
  tvSelectFile.Items.BeginUpdate;
  try
    tvSelectFile.Items.Clear;
    AddExtraFiles;
    AddXMLFiles;
  finally
    tvSelectFile.Items.EndUpdate;
    DefaultCursor(lCursor);
  end;
end;  // FillReportList
       
{-------------------------------------------------------------------------------
}
procedure TBaseReportSelect.AddExtraFiles;
begin
  // Nothing here. Implement in inherited classes if necessary
end;

{-------------------------------------------------------------------------------
  Add the XML Reports currently available to the hierarchy
}
procedure TBaseReportSelect.AddXMLFiles;
var
  i: integer;

    function GetReportFileName(AIndex: integer): string;
    begin
      with FIndex.FileTypes do
        Result := LeftStr(Strings[AIndex], Pos(':', Strings[AIndex])-1);
    end;

    function GetReportTitle(AIndex: integer): string;
    begin
      with FIndex.Titles do
        Result := Values[GetReportFileName(AIndex)];
      with FIndex.Templates do
        if Values[GetReportFileName(AIndex)]<>'' then
          result := result + ' (' + Values[GetReportFileName(AIndex)] + ')';
    end;

    // retrieve a report folder node from the children of the current parent.
    // Create it if required.
    function FindFolder(AParentFolder: TFolderNode; const AName: string): TFolderNode;
    var
      j: integer;
    begin
      if Assigned(AParentFolder) then begin
        for j := 0 to AParentFolder.Count-1 do
          if Sametext(AParentFolder.Items[j].Caption, AName) and
              (AParentFolder.Items[j] is TFolderNode) then begin
            Result := TFolderNode(AParentFolder.Items[j]);
            Exit; // from inner proc
          end;
      end
      else begin
        for j := 0 to tvSelectFile.Items.Count-1 do
          if Sametext(tvSelectFile.Items[j].Caption, AName) and
              (tvSelectFile.Items[j] is TFolderNode) then begin
            Result := TFolderNode(tvSelectFile.Items[j]);
            Exit; // from inner proc
          end;
      end;
      // if we have got this far then it means the folder does not exist yet
      Result := TFolderNode(tvSelectFile.Items.AddTypedChild(AParentFolder, TFolderNode));
      Result.Caption := AName;
    end;

    // recursive proc that builds a folder path to the report
    procedure AddToPath(AParentFolder: TFolderNode; APath: string; AIndex: integer);
    var
      lPathName, lRemainingPath: string;
    begin
      // if no path specified then add menu item
      if APath='' then
        with TXMLReportNode(tvSelectFile.Items.AddTypedChild(AParentFolder, TXMLReportNode)) do
        begin
          Caption := GetReportTitle(AIndex);
          FileName := GetReportFileName(AIndex);
        end
      else begin
        // path specified so need to find or create top menu item and recurse
        lPathName := LeftStr(APath, Pos('\', APath)-1);
        if lPathName = '' then
          lPathName := APath;
        lRemainingPath := Copy(APath, Length(lPathName)+2, Length(APath));
        AddToPath(FindFolder(AParentFolder, lPathName), lRemainingPath, AIndex);
      end;
    end;

begin
  with FIndex.FileTypes do
    for i := 0 to Count-1 do
      // only handle reports that link to the default type
      if Copy(Strings[i], Pos(':', Strings[i])+1, Length(Strings[i])) = '0' then
        AddToPath(nil, FIndex.Paths.Values[
            LeftStr(Strings[i], Pos(':', Strings[i])-1)], i);
end;

{-------------------------------------------------------------------------------
}
procedure TBaseReportSelect.SplitterMoved(Sender: TObject);
begin
  tvSelectFile.Refresh;
  rtfDescription.Refresh;
end;

{-------------------------------------------------------------------------------
  TFolderNode
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Set default image index
}
constructor TFolderNode.Create(AOwner: TTreeCollection);
begin
  inherited;
  ImageIndex := 59;
  SelectedIndex := 59;
end;

{-------------------------------------------------------------------------------
 TReportNode
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TReportNode.SetFileName(const Value: string);
begin
  FFileName := Value;
end;  

{-------------------------------------------------------------------------------
  TXMLReportNode
-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------
  Set the default image index
}
constructor TXMLReportNode.Create(AOwner: TTreeCollection);
begin
  inherited;
  ImageIndex := 66;
  SelectedIndex := 66;
end;

end.
