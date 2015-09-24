{===============================================================================
  Unit:        MigrateSettings2Page

  Defines:     TfraMigrateSettings2

  Description:

  Model:

  Created:     March 2004

  Last revision information:
    $Revision: 7 $
    $Date: 5/08/09 10:47 $
    $Author: Ericsalmon $

===============================================================================}

unit MigrateSettings2Page;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasePage, StdCtrls, FolderBrowser, ExtCtrls;

type
  TfraMigrateSettings2 = class(TBasePage)
    btnCardsFolder: TButton;
    btnImagesFolder: TButton;
    btnRucksacksFolder: TButton;
    btnTemplatesFolder: TButton;
    chkCards: TCheckBox;
    chkImages: TCheckBox;
    chkRucksacks: TCheckBox;
    chkTemplates: TCheckBox;
    dlgFolder: TFolderBrowser;
    eCardsPath: TEdit;
    eImagesPath: TEdit;
    eRucksacksPath: TEdit;
    eTemplatesPath: TEdit;
    lblInformation: TLabel;
    procedure CheckChange(Sender: TObject);
    procedure FolderClick(Sender: TObject);
  private
    procedure GetFolder(AEdit: TEdit);
  protected
    function GetConfirmCancel: Boolean; override;
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetNextCaption: String; override;
    function GetPrevious: TBasePageClass; override;
    function GetResourceImage: String; override;
    procedure LoadContent; override;
    procedure SaveContent; override;
  public
    procedure ValidateContent; override;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

uses
  MigrateSettings1Page, MigratePage, Settings, TextMessages, Functions,
  GeneralFunctions, SetupConstants, MigrationSettings;

{-==============================================================================
    TfraMigrateSettings2
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings2.CheckChange(Sender: TObject);
  
  procedure SetControlsState(AState: Boolean; AEdit: TEdit; AButton: TButton);
  begin
    AEdit.Enabled := AState;
    AButton.Enabled := AState;
  end;
  
begin
  inherited;
  if Sender = chkRucksacks then
    SetControlsState(chkRucksacks.Checked, eRucksacksPath, btnRucksacksFolder)
  else
  if Sender = chkCards then
    SetControlsState(chkCards.Checked, eCardsPath, btnCardsFolder)
  else
  if Sender = chkTemplates then
    SetControlsState(chkTemplates.Checked, eTemplatesPath, btnTemplatesFolder)
  else
  if Sender = chkImages then
    SetControlsState(chkImages.Checked, eImagesPath, btnImagesFolder);
  ChangedContent;
end;  // TfraMigrateSettings2.CheckChange 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings2.FolderClick(Sender: TObject);
begin
  inherited;
  if Sender = btnRucksacksFolder then GetFolder(eRucksacksPath) else
  if Sender = btnCardsFolder     then GetFolder(eCardsPath) else
  if Sender = btnTemplatesFolder then GetFolder(eTemplatesPath) else
  if Sender = btnImagesFolder    then GetFolder(eImagesPath);
end;  // TfraMigrateSettings2.FolderClick

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings2.GetConfirmCancel: Boolean;
begin
  Result := False;  // Confirmation not necessary
end;  // TfraMigrateSettings2.GetConfirmCancel 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings2.GetFolder(AEdit: TEdit);
begin
  with dlgFolder do begin
    Folder := ExpandLongPathName(AEdit.Text);
    if Execute then begin
      AEdit.Text := ExcludeTrailingPathDelimiter(Folder);
      ChangedContent;
    end;
  end;
end;  // TfraMigrateSettings2.GetFolder

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings2.GetHasNext: Boolean;
begin
  Result := (not chkRucksacks.Checked or (eRucksacksPath.Text <> '')) and
            (not chkCards.Checked or (eCardsPath.Text <> '')) and
            (not chkTemplates.Checked or (eTemplatesPath.Text <> '')) and
            (not chkImages.Checked or (eImagesPath.Text <> ''));
end;  // TfraMigrateSettings2.GetHasNext

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings2.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraMigrateSettings2.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings2.GetNext: TBasePageClass;
begin
  Result := TfraMigrate;
end;  // TfraMigrateSettings2.GetNext

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings2.GetNextCaption: String;
begin
  Result := ResStr_TransferCaption;
end;  // TfraMigrateSettings2.GetNextCaption

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings2.GetPrevious: TBasePageClass;
begin
  Result := TfraMigrateSettings1;
end;  // TfraMigrateSettings2.GetPrevious

{-------------------------------------------------------------------------------
}
function TfraMigrateSettings2.GetResourceImage: String;
begin
  Result := ResImg_MigrateSettings2;
end;  // TfraMigrateSettings2.GetResourceImage 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings2.LoadContent;
begin
  inherited;
  with TMigrationSettings(Settings) do begin
    eRucksacksPath.Text := MigrationRucksacksPath;
    eCardsPath.Text     := MigrationCardsPath;
    eTemplatesPath.Text := MigrationTemplatesPath;
    eImagesPath.Text    := MigrationImagesPath;

    chkRucksacks.Checked := MigrateRucksacks;
    chkCards.Checked     := MigrateCards;
    chkTemplates.Checked := MigrateTemplates;
    chkImages.Checked    := MigrateImages;
  end;
  CheckChange(chkRucksacks);
  CheckChange(chkCards);
  CheckChange(chkTemplates);
  CheckChange(chkImages);
end;  // TfraMigrateSettings2.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings2.SaveContent;
begin
  inherited;
  with TMigrationSettings(Settings) do begin
    MigrationRucksacksPath := ExcludeTrailingPathDelimiter(eRucksacksPath.Text);
    MigrationCardsPath     := ExcludeTrailingPathDelimiter(eCardsPath.Text);
    MigrationTemplatesPath := ExcludeTrailingPathDelimiter(eTemplatesPath.Text);
    MigrationImagesPath    := ExcludeTrailingPathDelimiter(eImagesPath.Text);

    MigrateRucksacks := chkRucksacks.Checked;
    MigrateCards     := chkCards.Checked;
    MigrateTemplates := chkTemplates.Checked;
    MigrateImages    := chkImages.Checked;
  end;
end;  // TfraMigrateSettings2.SaveContent 

{-------------------------------------------------------------------------------
}
procedure TfraMigrateSettings2.ValidateContent;
begin
  inherited;
  // Check the paths are valid.
  if chkRucksacks.Checked then CheckPath(eRucksacksPath);
  if chkCards.Checked then CheckPath(eCardsPath);
  if chkTemplates.Checked then CheckPath(eTemplatesPath);
  if chkImages.Checked then CheckPath(eImagesPath);
end;  // TfraMigrateSettings2.ValidateContent 

end.


