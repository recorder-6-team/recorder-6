{===============================================================================
  Unit:        ADOConnStringEditor

  Defines:     TdlgConnStringEditor

  Description:

  Model:

  Last revision information:
    $Revision: 1 $
    $Date: 2/06/04 10:33 $
    $Author: Ericsalmon $

===============================================================================}

unit ADOConnStringEditor;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
  StdCtrls, DB, ADODB, ADOInt;

type
  TdlgConnStringEditor = class(TForm)
    btnBrowse: TButton;
    btnBuild: TButton;
    btnCancel: TButton;
    btnOk: TButton;
    cmbDataLinkFile: TComboBox;
    eConnectionString: TEdit;
    gbConnectionOptions: TGroupBox;
    rbUseConnectionString: TRadioButton;
    rbUseDataLinkFile: TRadioButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SourceChange(Sender: TObject);
  private
    FInitialConnString: String;
    function GetConnectionString: String;
    procedure SetInitialConnString(const Value: String);
  public
    property ConnectionString: String read GetConnectionString;
    property InitialConnString: String read FInitialConnString write SetInitialConnString;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TdlgConnStringEditor
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TdlgConnStringEditor.btnBrowseClick(Sender: TObject);
begin
  cmbDataLinkFile.Text := PromptDataLinkFile(Handle, cmbDataLinkFile.Text);
end;  // TdlgConnStringEditor.btnBrowseClick 

{-------------------------------------------------------------------------------
}
procedure TdlgConnStringEditor.btnBuildClick(Sender: TObject);
begin
  eConnectionString.Text := PromptDataSource(Handle, eConnectionString.Text);
end;  // TdlgConnStringEditor.btnBuildClick 

{-------------------------------------------------------------------------------
}
procedure TdlgConnStringEditor.FormCreate(Sender: TObject);
begin
  GetDataLinkFiles(cmbDataLinkFile.Items);
  InitialConnString := '';
  SourceChange(nil);
end;  // TdlgConnStringEditor.FormCreate 

{-------------------------------------------------------------------------------
}
function TdlgConnStringEditor.GetConnectionString: String;
begin
  if rbUseConnectionString.Checked then
    Result := eConnectionString.Text
  else
  if cmbDataLinkFile.Text <> '' then begin
    if ExtractFilePath(cmbDataLinkFile.Text) = '' then
      Result := CT_FILENAME + DataLinkDir + '\' + cmbDataLinkFile.Text
    else
      Result := CT_FILENAME + cmbDataLinkFile.Text
  end else
    Result := '';
end;  // TdlgConnStringEditor.GetConnectionString 

{-------------------------------------------------------------------------------
}
procedure TdlgConnStringEditor.SetInitialConnString(const Value: String);
var
  lFileName: String;
begin
  FInitialConnString := Value;
  rbUseDataLinkFile.Checked := True;
  if Pos(CT_FILENAME, FInitialConnString) = 1 then
  begin
    lFileName := Copy(FInitialConnString, Length(CT_FILENAME) + 1, MAX_PATH);
    if ExtractFilePath(lFileName) = (DataLinkDir + '\') then
      cmbDataLinkFile.Text := ExtractFileName(lFileName)
    else
      cmbDataLinkFile.Text := lFileName;
  end else begin
    eConnectionString.Text := FInitialConnString;
    rbUseConnectionString.Checked := True;
  end;
  
  SourceChange(nil);
end;  // TdlgConnStringEditor.SetInitialConnString 

{-------------------------------------------------------------------------------
}
procedure TdlgConnStringEditor.SourceChange(Sender: TObject);
  
  const
    ENABLED_COLOUR: array[Boolean] of TColor = (clBtnFace, clWindow);
  
begin
  cmbDataLinkFile.Enabled   := rbUseDataLinkFile.Checked;
  cmbDataLinkFile.Color     := ENABLED_COLOUR[cmbDataLinkFile.Enabled];
  btnBrowse.Enabled         := cmbDataLinkFile.Enabled;
  eConnectionString.Enabled := rbUseConnectionString.Checked;
  eConnectionString.Color   := ENABLED_COLOUR[eConnectionString.Enabled];
  btnBuild.Enabled          := eConnectionString.Enabled;
end;  // TdlgConnStringEditor.SourceChange 

end.

