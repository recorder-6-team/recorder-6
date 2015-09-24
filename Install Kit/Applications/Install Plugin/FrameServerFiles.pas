{-------------------------------------------------------------------------------
  Unit:        FrameServerFiles

  Defines:     TfraServerFiles

  Description: Additional information for install.

  Created:     April 2003

  Last revision information:
    $Revision: 5 $
    $Date: 8/03/04 10:24 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit FrameServerFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrameBase, Settings, StdCtrls;

type
  TfraServerFiles = class(TPageFrame)
    Label6: TLabel;
    cbShareReports: TCheckBox;
    cbShareTemplates: TCheckBox;
    cbShareReportSnapshots: TCheckBox;
    cbShareRecordCards: TCheckBox;
    cbSharePolygonFilters: TCheckBox;
    cbShareRucksacks: TCheckBox;
    Label4: TLabel;
    procedure cbShareClick(Sender: TObject);
  private
    FJustCreated: Boolean;
  protected
    function GetNextFrame: TPageFrameClass; override;
    function GetPrevFrame: TPageFrameClass; override;
    procedure SetSettings(const Value: TSettings); override;
    procedure SetNextButton(const Value: TButton); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FrameSiteInfo, FrameNewOrExisting;

//==============================================================================
{ TfraServerFiles }
//------------------------------------------------------------------------------
constructor TfraServerFiles.Create(AOwner: TComponent);
begin
  inherited;
  FJustCreated := true;
end;

//------------------------------------------------------------------------------
function TfraServerFiles.GetNextFrame: TPageFrameClass;
begin
  if not FJustCreated then
    Settings.CreateOptionalServerFolders;
  FJustCreated := false;
  Result := TfraNewOrExisting;
end;

//------------------------------------------------------------------------------
function TfraServerFiles.GetPrevFrame: TPageFrameClass;
begin
  Result := TfraSiteInfo;
end;

//------------------------------------------------------------------------------
procedure TfraServerFiles.SetNextButton(const Value: TButton);
begin
  inherited;
  if Assigned(NextButton) then NextButton.Enabled := true;
end;

//------------------------------------------------------------------------------
procedure TfraServerFiles.SetSettings(const Value: TSettings);
begin
  inherited;
  cbShareReports.Checked         := Settings.ShareReports;
  cbShareTemplates.Checked       := Settings.ShareTemplates;
  cbShareReportSnapshots.Checked := Settings.ShareReportSnapshots;
  cbShareRecordCards.Checked     := Settings.ShareRecordCards;
  cbSharePolygonFilters.Checked  := Settings.SharePolygonFilters;
  cbShareRucksacks.Checked       := Settings.ShareRucksacks;
  if Assigned(NextButton) then NextButton.Enabled := true;
end;

//------------------------------------------------------------------------------
procedure TfraServerFiles.cbShareClick(Sender: TObject);
begin
  inherited;
  Settings.ShareReports         := cbShareReports.Checked;
  Settings.ShareTemplates       := cbShareTemplates.Checked;
  Settings.ShareReportSnapshots := cbShareReportSnapshots.Checked;
  Settings.ShareRecordCards     := cbShareRecordCards.Checked;
  Settings.SharePolygonFilters  := cbSharePolygonFilters.Checked;
  Settings.ShareRucksacks       := cbShareRucksacks.Checked;
end;

//------------------------------------------------------------------------------
end.
