{-------------------------------------------------------------------------------
  Unit:        ViewRemoteInstructions

  Defines:     TdlgViewRemoteInstructions

  Description: Form to display instructions for setting up a remote server.

  Created:     March 2003

  Last revision information:
    $Revision: 5 $
    $Date: 24/04/03 12:23 $
    $Author: Ericsalmon $

-------------------------------------------------------------------------------}

unit ViewRemoteInstructions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, Printers, TextMessages, ActnList,
  StdActns, ExtActns, Settings, Registry;

type
  TdlgViewRemoteInstructions = class(TForm)
    reInstructions: TRichEdit;
    btnPrint: TBitBtn;
    btnClose: TBitBtn;
    procedure btnPrintClick(Sender: TObject);
  private
    { Private declarations }
    function GetDBPath : string;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetSettings(ASettings : TSettings);
  end;


implementation

{$R *.dfm}

{ TdlgViewRemoteInstructions }

{-------------------------------------------------------------------------------
  Description : Loads the rtf content from a file on the disk.
              Also hides the Print button if no Windows printers installed.
  Created : 26/03/2003 }
constructor TdlgViewRemoteInstructions.Create(AOwner: TComponent);
begin
  inherited;
  btnPrint.Visible := Printer.Printers.Count>0;
end;

{-------------------------------------------------------------------------------
  Description : If any printers available, then print the content of the
      rtf control
  Created : 26/03/2003 }
procedure TdlgViewRemoteInstructions.btnPrintClick(Sender: TObject);
begin
  if Printer.Printers.Count>0 then
    reInstructions.Print('Remote Server Installation Instructions');
end;

{-------------------------------------------------------------------------------
  Description : Once we know what type of install, set up appropriate instructions
  Created : 27/03/2003 }
procedure TdlgViewRemoteInstructions.SetSettings(ASettings: TSettings);
var
  lLineCount : integer;
begin
  reInstructions.Lines.Clear;
  reInstructions.Paragraph.FirstIndent := 4;
  reInstructions.Paragraph.LeftIndent := 10;
  reInstructions.Paragraph.RightIndent := 4;

  reInstructions.Lines.Add(ST_INSTRUCT_TITLE);
  reInstructions.Lines.Add('');
  reInstructions.Lines.Add(ST_INSTRUCT_ATTACH1);
  reInstructions.Lines.Add(ST_INSTRUCT_ATTACH2);
  reInstructions.Lines.Add(ST_INSTRUCT_ATTACH3);
  // locate the start of the SQL to highlight
  lLineCount := reInstructions.Lines.Count;
  if ASettings.NTAuthentication then
    reInstructions.Lines.Add(ST_INSTRUCT_SQL_TRUSTED)
  else
    reInstructions.Lines.Add(ST_INSTRUCT_SQL_NBNUSER);
  reInstructions.SelStart := SendMessage( reInstructions.Handle, EM_LINEINDEX,
                                     lLineCount, 0 );
  reInstructions.SelLength := SendMessage( reInstructions.Handle, EM_LINEINDEX,
                                     reInstructions.Lines.Count, 0 ) - reInstructions.SelStart;
  reInstructions.SelAttributes.Color := clHighlight;
  reInstructions.SelAttributes.Name := 'Courier New';
  reInstructions.Paragraph.FirstIndent := 40;
  if ASettings.InstallMode = imUpgrade then begin
    reInstructions.Lines.Add(Format(ST_INSTRUCT_ATTACH4, [GetDBPath]));
    if ASettings.NTAuthentication then
      reInstructions.Lines.Add('5) ' + ST_INSTRUCT_ATTACH5);
  end
  else if ASettings.NTAuthentication then
      reInstructions.Lines.Add('4) ' + ST_INSTRUCT_ATTACH5);
  // bold the title
  reInstructions.SelStart := 0;
  reInstructions.SelLength := Length(reInstructions.Lines[0]);
  reInstructions.SelAttributes.Size := 12;
  reInstructions.SelAttributes.Style := [fsBold];
  reInstructions.Paragraph.Alignment := taCenter;
  reInstructions.SelLength := 0;
end;


{-------------------------------------------------------------------------------
  Description : Return the folder the access files are in
  Created : 27/3/2003 }
function TdlgViewRemoteInstructions.GetDBPath: string;
begin
  Result := '<database path>';
  with TRegistry.Create do
    try
      if OpenKey('Software\JNCC\Recorder\Settings', False) then begin
        // Extract path to Help folder
        Result := ExtractFilePath(ReadString('Database Path'));
        CloseKey;
      end;
    finally
      Free;
    end;
end;

end.
