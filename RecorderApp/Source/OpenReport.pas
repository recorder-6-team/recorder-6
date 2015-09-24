//==============================================================================
//  Unit:        OpenReport
//
//  Implements:  TdlgOpenReport
//
//  Description:
//
//  Author:      John van Breda
//  Created:     8 Apr 1999
//
//  Last Revision Details:
//    $Revision: 31 $
//    $Date: 3/04/08 8:30 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit OpenReport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Variants,
  StdCtrls, Buttons, ExtCtrls, FolderBrowser, OnlineHelp, ImageListButton,
  exgrid, RapTree, StrUtils, TreeColl, XMLDoc, XMLIntf, ExceptionForm,
  ComCtrls, Registry, Constants, BaseReportSelectUnit, CRReportIndex,
  KeyboardRapidTree;

type
  TWizardReportNode = class(TReportNode)
  public
    constructor Create(AOwner: TTreeCollection); override;
  end;

  TdlgOpenReport = class(TBaseReportSelect)
    pnlFavourite: TPanel;
    chkIsFavourite: TCheckBox;
    procedure chkIsFavouriteClick(Sender: TObject);
  private
    FUpdateFavouriteReports: Boolean;   
    procedure AddReportOutputInfo(ADOMNode: IXMLNode);
    procedure GetIsFavouriteStatus(const AReportFile: string);  
    procedure AddWizardFiles;
  protected
    procedure AddExtraFiles; override;
    function GetDirectory: string; override;    
    function GetHelpContext: Integer; override;
    function GetIndex: TBaseReportIndex; override;
    procedure SetNoFileSelectedMessage; override;
    procedure SetExtraNodeDescription(Node: TFlyNode); override;
    procedure SetWizardReportNodeDescription(ANode: TWizardReportNode); 
    procedure SetXMLReportNodeDescription(ANode: TXMLReportNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, FormActions, GeneralFunctions, CRCommonClasses,
  GeneralData, CRConstants, MainTBar, Math;


{-------------------------------------------------------------------------------
 TdlgOpenReport
-------------------------------------------------------------------------------}

//==============================================================================
destructor TdlgOpenReport.Destroy;
begin
  if FUpdateFavouriteReports then
    frmMain.CreateFavouriteReportMenuOptions;
  inherited;
end;

{-------------------------------------------------------------------------------
  Initialise dialog
}
constructor TdlgOpenReport.Create(AOwner: TComponent);
begin
  inherited;         
  FUpdateFavouriteReports := false;  
end;

{-------------------------------------------------------------------------------
  Sets the status of the IsFavourite checkbox when a report is selected.
}
procedure TdlgOpenReport.GetIsFavouriteStatus(const AReportFile: string);
begin
  // disable OnClick on the checkbox since we are reading the value.  Onclick
  // is for user input.
  chkIsFavourite.OnClick := nil;
  try
    chkIsFavourite.Checked := False;
    with TRegistry.Create do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKeyReadOnly(REG_KEY_FAVOURITE_REPORTS) then
          chkIsFavourite.Checked := ValueExists(AReportFile);
      finally
        CloseKey;
        Free;
      end;
  finally
    chkIsFavourite.OnClick := chkIsFavouriteClick;
  end;
end;

{-------------------------------------------------------------------------------
  Add a message to the rtf box - no report selected
}
procedure TdlgOpenReport.SetNoFileSelectedMessage;
begin
  inherited;
  chkIsFavourite.Enabled := False;
  chkIsFavourite.Checked := False;
end;

{-------------------------------------------------------------------------------
  Persist the favourite report setting to the registry
}
procedure TdlgOpenReport.chkIsFavouriteClick(Sender: TObject);
begin
  if Assigned(tvSelectFile.Selected) then
    if tvSelectFile.Selected is TReportNode then
      with TRegistry.Create do
        try
          RootKey := HKEY_CURRENT_USER;
          if OpenKey(REG_KEY_FAVOURITE_REPORTS, True) then
            if chkIsFavourite.Checked then
              WriteString(TReportNode(tvSelectFile.Selected).Filename, '1')
            else begin
              if ValueExists(TReportNode(tvSelectFile.Selected).Filename) then
                DeleteValue(TReportNode(tvSelectFile.Selected).Filename);
            end;
          FUpdateFavouriteReports := true;
        finally
          CloseKey;
          Free;
        end;
end;
   
{-------------------------------------------------------------------------------
}
function TdlgOpenReport.GetDirectory: String;
begin
  Result := AppSettings.ReportPath;
end;
    
{-------------------------------------------------------------------------------
}
function TdlgOpenReport.GetIndex: TBaseReportIndex;
begin
  Result := AppSettings.CRReportIndex;
end;
     
{-------------------------------------------------------------------------------
}
function TdlgOpenReport.GetHelpContext: Integer;
begin
  Result := IDH_REPORTOPEN;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgOpenReport.SetExtraNodeDescription(Node: TFlyNode);
begin   
  if Node is TWizardReportNode then
    SetWizardReportNodeDescription(TWizardReportNode(Node))
  else
    SetNoFileSelectedMessage;
end;
   
{-------------------------------------------------------------------------------
}
procedure TdlgOpenReport.SetWizardReportNodeDescription(ANode: TWizardReportNode);
var
  lFile: TStringList;
  lHasTaxa, lHasBiotopes: boolean;
  lXMLDoc: IXMLDocument;
begin
  lFile := TStringlist.Create;
  try
    lFile.LoadFromFile(eDirectory.Text + ANode.FileName);
    lHasTaxa := Pos('''T'' AS Type', lFile.Text) > 0;
    lHasBiotopes := Pos('''B'' AS Type', lFile.Text) > 0;
    SetDescriptionTitle(ANode.Caption);
    if lHasTaxa and lHasBiotopes then
      rtfDescription.Lines.Add(ResStr_PlaceReport)
    else if lHasTaxa then
      rtfDescription.Lines.Add(ResStr_TaxonReport)
    else if lHasBiotopes then
      rtfDescription.Lines.Add(ResStr_BiotopeReport);
    lXMLDoc := NewXMLDocument;
    lXMLDoc.LoadFromFile(eDirectory.Text + ANode.FileName);
    AddReportOutputInfo(lXMLDoc.ChildNodes['Report']);
    chkIsFavourite.Enabled := True;
    GetIsFavouriteStatus(ANode.Filename);
  finally
    lFile.Free;
  end;
end;  

{-------------------------------------------------------------------------------
  Taks a report XML node (wizard or XML) and uses the output element to add
    details of the template or snapshot to the description
}
procedure TdlgOpenReport.AddReportOutputInfo(ADOMNode: IXMLNode);
var
  lOutputNode: IXMLNode;
begin
  lOutputNode := ADOMNode.ChildNodes[EL_OUTPUT];
  if not VarIsNull(lOutputNode.ChildNodes[EL_TEMPLATE].Attributes[AT_FILE]) then
    rtfDescription.Lines.Add(Format(ResStr_ReportDisplayedUsing, [ExtractWithoutExt(
        lOutputNode.ChildNodes[EL_TEMPLATE].Attributes[AT_FILE])]));
  if not VarIsNull(lOutputNode.ChildNodes['Snapshot'].Attributes[AT_FILE]) then
    rtfDescription.Lines.Add(Format(ResStr_ReportGeneratesSnapshot, [ExtractWithoutExt(
        lOutputNode.ChildNodes['Snapshot'].Attributes[AT_FILE])]));
end;

{-------------------------------------------------------------------------------
  Adds the list of report wizard saved files to the supplied node
}
procedure TdlgOpenReport.AddWizardFiles;
var
  lSearchRec: TSearchRec;
  lSearchResult: Integer;
  lWizFolderNode: TFolderNode;
begin
  lSearchResult := FindFirst(eDirectory.Text + '*.wzd', faAnyFile, lSearchRec);
  if lSearchResult = 0 then begin
    lWizFolderNode := TFolderNode(tvSelectFile.Items.AddTypedChild(nil, TFolderNode));
    lWizFolderNode.Caption := ResStr_ReportWizardReports;
    while lSearchResult = 0 do begin
      with TWizardReportNode(tvSelectFile.Items.AddTypedChild(lWizFolderNode, TWizardReportNode)) do begin
        Caption := dmGeneralData.GetReportCaption(eDirectory.Text + lSearchRec.Name);
        FileName := lSearchRec.Name;
      end;
      lSearchResult := FindNext(lSearchRec);
    end;
  end;
  FindClose(lSearchRec);
end;
      
{-------------------------------------------------------------------------------
}
procedure TdlgOpenReport.AddExtraFiles;
begin
  AddWizardFiles;
end;

{-------------------------------------------------------------------------------
}
procedure TdlgOpenReport.SetXMLReportNodeDescription(ANode: TXMLReportNode);
begin
  inherited;
  chkIsFavourite.Enabled := True;
  GetIsFavouriteStatus(ANode.Filename);
end; 

{-------------------------------------------------------------------------------
  TWizardReportNode
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Set the default image index
}
constructor TWizardReportNode.Create(AOwner: TTreeCollection);
begin
  inherited;
  ImageIndex := 65;
  SelectedIndex := 65;
end;

end.
