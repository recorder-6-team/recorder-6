//==============================================================================
//  Unit:        TextInput
//
//  Implements:  TdlgTextInput
//
//  Description:
//
//  Author:      John van Breda
//  Created:     8 Apr 1999
//
//  Last Revision Details:
//    $Revision: 6 $
//    $Date: 20/06/02 15:00 $
//    $Author: Ericsalmon $
//
//  $History: TextInput.pas $
//  
//  *****************  Version 6  *****************
//  User: Ericsalmon   Date: 20/06/02   Time: 15:00
//  Updated in $/JNCC/Source
//  Replaced BitBtns with ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit TextInput;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, OnlineHelp, ImageListButton;

type
  TdlgTextInput = class(TForm)
    Bevel1: TBevel;
    lblSubject: TLabel;
    eName: TEdit;
    lblInitials: TLabel;
    eInitials: TEdit;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    procedure eNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bbOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    hlpTextInput: TOnlineHelp;
    FChanged:boolean;
  public
    { Public declarations }
  end;

//==============================================================================
implementation

uses
  FormActions;
  
{$R *.DFM}

//==============================================================================
procedure TdlgTextInput.FormShow(Sender: TObject);
begin
  FChanged:=false;
end;

//==============================================================================
procedure TdlgTextInput.eNameChange(Sender: TObject);
begin
  if Trim(eName.Text)<>'' then FChanged:=true;
end;

//==============================================================================
procedure TdlgTextInput.bbOkClick(Sender: TObject);
begin
  if FChanged then ModalResult:=mrOk
              else ModalResult:=mrCancel;
end;

//==============================================================================
procedure TdlgTextInput.FormCreate(Sender: TObject);
begin
  hlpTextInput := TOnlineHelp.Create(Self.Handle);
  OnHelp := hlpTextInput.OnHelpReplacement;
  HelpContext := IDH_AUTHORINPUT;
end;

//==============================================================================
procedure TdlgTextInput.FormDestroy(Sender: TObject);
begin
  hlpTextInput.Free;
end;

//==============================================================================
end.
