{===============================================================================
  Unit:        BasePage

  Defines:     TBasePage

  Description: Base class for steps followed during the install.

  Created:     March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 3/07/09 11:57 $
    $Author: Ericsalmon $

===============================================================================}

unit BasePage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Settings, ExtCtrls, StdCtrls;

type
  TBasePageClass = class of TBasePage;

  TForcePageEvent = procedure (Sender: TObject; APageClass: TBasePageClass) of object;

  TBasePage = class (TFrame)
    pnlTitle: TPanel;
    lblTitle: TLabel;
    pnlContinue: TPanel;
    lblClickNext: TLabel;
  private
    FLoading: Boolean;
    FOnChangedContent: TNotifyEvent;
    FOnForceNextPage: TForcePageEvent;
    FSettings: TSettings;
  protected
    procedure ChangedContent;
    procedure ForceNextPage(APageClass: TBasePageClass);
    function GetConfirmCancel: Boolean; virtual;
    function GetHasNext: Boolean; virtual;
    function GetHasPrevious: Boolean; virtual;
    function GetIsFinal: Boolean; virtual;
    function GetNext: TBasePageClass; virtual;
    function GetNextCaption: String; virtual;
    function GetPrevious: TBasePageClass; virtual;
    function GetResourceImage: String; virtual;
    procedure LoadContent; virtual;
    procedure SaveContent; virtual;
    property Loading: Boolean read FLoading;
  public
    constructor Create(AOwner: TComponent; ASettings: TSettings); reintroduce; virtual;
    destructor Destroy; override;
    procedure Cancel; virtual;
    function Execute: Boolean; virtual;
    procedure ValidateContent; virtual;
    property ConfirmCancel: Boolean read GetConfirmCancel;
    property HasNext: Boolean read GetHasNext;
    property HasPrevious: Boolean read GetHasPrevious;
    property IsFinal: Boolean read GetIsFinal;
    property Next: TBasePageClass read GetNext;
    property NextCaption: String read GetNextCaption;
    property OnChangedContent: TNotifyEvent read FOnChangedContent write FOnChangedContent;
    property OnForceNextPage: TForcePageEvent read FOnForceNextPage write FOnForceNextPage;
    property ResourceImage: String read GetResourceImage;
    property Previous: TBasePageClass read GetPrevious;
    property Settings: TSettings read FSettings;
  end;
  
//==============================================================================
implementation

{$R *.dfm}

{-==============================================================================
    TBasePage
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBasePage.Create(AOwner: TComponent; ASettings: TSettings);
begin
  inherited Create(AOwner);
  FSettings := ASettings;
  FLoading := True;
  try
    LoadContent;
  finally
    FLoading := False;
  end;
end;  // TBasePage.Create 

{-------------------------------------------------------------------------------
}
destructor TBasePage.Destroy;
begin
  SaveContent;
  inherited;
end;  // TBasePage.Destroy 

{-------------------------------------------------------------------------------
}
procedure TBasePage.Cancel;
begin
  // So that individual pages can take relevant action if setup cancelled.
end;  // TBasePage.Cancel 

{-------------------------------------------------------------------------------
}
procedure TBasePage.ChangedContent;
begin
  if Assigned(FOnChangedContent) then
    FOnChangedContent(Self);
end;  // TBasePage.ChangedContent 

{-------------------------------------------------------------------------------
}
function TBasePage.Execute: Boolean;
begin
  Result := False;
end;  // TBasePage.Execute 

{-------------------------------------------------------------------------------
}
procedure TBasePage.ForceNextPage(APageClass: TBasePageClass);
begin
  if Assigned(FOnForceNextPage) then
    FOnForceNextPage(Self, APageClass);
end;  // TBasePage.ForceNextPage 

{-------------------------------------------------------------------------------
}
function TBasePage.GetConfirmCancel: Boolean;
begin
  Result := True;
end;  // TBasePage.GetConfirmCancel

{-------------------------------------------------------------------------------
}
function TBasePage.GetHasNext: Boolean;
begin
  Result := False;
end;  // TBasePage.GetHasNext

{-------------------------------------------------------------------------------
}
function TBasePage.GetHasPrevious: Boolean;
begin
  Result := False;
end;  // TBasePage.GetHasPrevious

{-------------------------------------------------------------------------------
}
function TBasePage.GetIsFinal: Boolean;
begin
  Result := False;
end;  // TBasePage.GetIsFinal 

{-------------------------------------------------------------------------------
}
function TBasePage.GetNext: TBasePageClass;
begin
  Result := nil;
end;  // TBasePage.GetNext 

{-------------------------------------------------------------------------------
}
function TBasePage.GetNextCaption: String;
begin
  Result := '&Next >';
end;  // TBasePage.GetNextCaption

{-------------------------------------------------------------------------------
}
function TBasePage.GetPrevious: TBasePageClass;
begin
  Result := nil;
end;  // TBasePage.GetPrevious

{-------------------------------------------------------------------------------
}
function TBasePage.GetResourceImage: String;
begin
  Result := '';
end;  // TBasePage.GetResourceImage

{-------------------------------------------------------------------------------
}
procedure TBasePage.LoadContent;
begin
  // Override as and when required.
end;  // TBasePage.LoadContent

{-------------------------------------------------------------------------------
}
procedure TBasePage.SaveContent;
begin
  // Override as and when required.
end;  // TBasePage.SaveContent

{-------------------------------------------------------------------------------
}
procedure TBasePage.ValidateContent;
begin
  // Override as and when required.
end;  // TBasePage.ValidateContent

end.







