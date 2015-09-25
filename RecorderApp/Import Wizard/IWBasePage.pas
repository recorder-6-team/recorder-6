{===============================================================================
  Unit:        IWBasePage

  Defines:     TBasePage

  Description: Base class for pages of the Import Wizard (IW) addin.

  Model:       ImportWizard

  Last revision information:
    $Revision: 11 $
    $Date: 20/03/08 10:55 $
    $Author: Ericsalmon $

===============================================================================}

unit IWBasePage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, IWSettings, Htmlview, BaseFormUnit, IWConstants, StrUtils;

type
  TBasePageClass = class of TBasePage;

  TValidateContentEvent = procedure (Sender: TObject; var DoValidation: Boolean) of object;
  TForcePageEvent = procedure (Sender: TObject; APageClass: TBasePageClass) of object;
  TChangedHTMLEvent = procedure (Sender: TObject; AErrors: TStrings) of object;
  {-----------------------------------------------------------------------------
    Base class for all frames in the Import Wizard.  The Import Wizard consists of a single
    form into which a number of frames are loaded in sequence one at a time.  These frames
    represent the pages of the wizard.
    The base class declares abstract behaviour for the frames, including the ability to
    specify which frame preceeds and succeeds them so that the Next and Previous buttons
    are automated.
  }
  TBasePage = class(TFrame)
  private
    FCancelled: Boolean;
    FLoadingPage: Boolean;
    FOnChangedContent: TNotifyEvent;
    FOnChangedHTML: TChangedHTMLEvent;
    FOnDoValidateContent: TValidateContentEvent;
    FOnForcedNextPage: TForcePageEvent;
    FSettings: TdmIWSettings;
    function GetContainerForm: TBaseForm;
  protected
    procedure ChangedContent;
    procedure ChangedHTML(AErrors: TStrings);
    function DoValidateContent: Boolean;
    procedure ForceNextPage(APageClass: TBasePageClass);
    function GetBorderWidth: Integer; virtual;
    function GetHasNext: Boolean; virtual;
    function GetHasPrevious: Boolean; virtual;
    function GetHTMLFileName: string; virtual;
    function GetHTMLImageName: string; virtual;
    function GetIsFinal: Boolean; virtual;
    function GetNext: TBasePageClass; virtual;
    function GetNextCaption: String; virtual;
    function GetPrevious: TBasePageClass; virtual;
    function GetSkip: Boolean; virtual;
    procedure LoadContent; virtual;
  public
    constructor Create(AOwner: TComponent; ASettings: TdmIWSettings); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure Cancel; virtual;
    function Execute: Boolean; virtual;
    procedure RefreshContent; virtual;
    procedure RefreshTermLists; virtual;
    procedure RegisterDragDropComponents; virtual;
    procedure UnRegisterDragDropComponents; virtual;
    procedure UpdateMapWindowSelector; virtual;
    procedure ValidateContent; virtual;
    procedure SaveContent; virtual;
    procedure SaveCurrentSettings; virtual;
    property BorderWidth: Integer read GetBorderWidth;
    property Cancelled: Boolean read FCancelled;
    property ContainerForm: TBaseForm read GetContainerForm;
    property HasNext: Boolean read GetHasNext;
    property HasPrevious: Boolean read GetHasPrevious;
    property HtmlFileName: string read GetHTMLFileName;
    property HtmlImageName: string read GetHTMLImageName;
    property IsFinal: Boolean read GetIsFinal;
    property LoadingPage: Boolean read FLoadingPage;
    property Next: TBasePageClass read GetNext;
    property NextCaption: String read GetNextCaption;
    property OnChangedContent: TNotifyEvent read FOnChangedContent write FOnChangedContent;
    property OnChangedHTML: TChangedHTMLEvent read FOnChangedHTML write FOnChangedHTML;
    property OnDoValidateContent: TValidateContentEvent read FOnDoValidateContent write
        FOnDoValidateContent;
    property OnForcedNextPage: TForcePageEvent read FOnForcedNextPage write FOnForcedNextPage;
    property Previous: TBasePageClass read GetPrevious;
    property Settings: TdmIWSettings read FSettings;
    property Skip: Boolean read GetSkip;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  IWResourceStrings;

{-==============================================================================
    TBasePage
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBasePage.Create(AOwner: TComponent; ASettings: TdmIWSettings);
begin
  inherited Create(AOwner);
  FLoadingPage := True;
  FSettings := ASettings;
  LoadContent;
  FLoadingPage := False;
end;  // TBasePage.Create

{-------------------------------------------------------------------------------
}
destructor TBasePage.Destroy;
begin
  inherited;
end;  // TBasePage.Destroy 

{-------------------------------------------------------------------------------
}
procedure TBasePage.Cancel;
begin
  FCancelled := True;
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
function TBasePage.DoValidateContent: Boolean;
begin
  if Assigned(FOnDoValidateContent) then
    FOnDoValidateContent(Self, Result)
  else
    Result := False;
end;  // TBasePage.DoValidateContent 

{-------------------------------------------------------------------------------
  Return true if frame does not require user input and should move on to next frame
      automatically after it has finished executing.
}
function TBasePage.Execute: Boolean;
begin
  Result := False;
end;  // TBasePage.Execute 

{-------------------------------------------------------------------------------
}
procedure TBasePage.ForceNextPage(APageClass: TBasePageClass);
begin
  if Assigned(FOnForcedNextPage) then
    FOnForcedNextPage(Self, APageClass);
end;  // TBasePage.ForceNextPage 

{-------------------------------------------------------------------------------
}
function TBasePage.GetBorderWidth: Integer;
begin
  Result := 4;
end;  // TBasePage.GetBorderWidth 

{-------------------------------------------------------------------------------
}
function TBasePage.GetContainerForm: TBaseForm;
begin
  Result := TBaseForm(GetParentForm(Self));
end;  // TBasePage.GetContainerForm 

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
  Result := ResStr_Next;
end;  // TBasePage.GetNextCaption 

{-------------------------------------------------------------------------------
}
function TBasePage.GetPrevious: TBasePageClass;
begin
  Result := nil;
end;  // TBasePage.GetPrevious 

{-------------------------------------------------------------------------------
}
function TBasePage.GetSkip: Boolean;
begin
  Result := False;
end;  // TBasePage.GetSkip 

{-------------------------------------------------------------------------------
}
procedure TBasePage.LoadContent;
begin
  // Do nothing in base class, override when needed.
end;  // TBasePage.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TBasePage.RefreshContent;
begin
  // Do nothing in base class, override when needed.
end;  // TBasePage.RefreshContent

{-------------------------------------------------------------------------------
}
procedure TBasePage.RefreshTermLists;
begin
  // Do nothing in base class, override when needed.
end;  // TBasePage.RefreshTermLists

{-------------------------------------------------------------------------------
}
procedure TBasePage.RegisterDragDropComponents;
begin
  // Do nothing in base class, override when needed.
end;  // TBasePage.RegisterDragDropComponents 

{-------------------------------------------------------------------------------
}
procedure TBasePage.SaveContent;
begin
  // Do nothing in base class, override when needed.
end;  // TBasePage.SaveContent 

{-------------------------------------------------------------------------------
}
procedure TBasePage.UnRegisterDragDropComponents;
begin
  // Do nothing in base class, override when needed.
end;  // TBasePage.UnRegisterDragDropComponents 

{-------------------------------------------------------------------------------
}
procedure TBasePage.UpdateMapWindowSelector;
begin
  // Do nothing in base class, override when needed.
end;  // TBasePage.UpdateMapWindowSelector

{-------------------------------------------------------------------------------
}
procedure TBasePage.ValidateContent;
begin
  // Do nothing in base class, override when needed.
end;  // TBasePage.ValidateContent

{-------------------------------------------------------------------------------
  Update the HTML with a new list of error bullet points
}
procedure TBasePage.ChangedHTML(AErrors: TStrings);
begin
  if Assigned(FOnChangedHTML) then
    FOnChangedHTML(Self, AErrors);
end;

function TBasePage.GetHTMLFileName: string;
begin
  if LeftStr(Classname, 4)='Tfra' then
    Result := RightStr(Classname, Length(Classname)-4)
  else
    Result := Classname;
  Result := Result + '.htm';
end;

procedure TBasePage.SaveCurrentSettings;
begin
  //Do nothing in base class, override when page reguires to set properties of
  //settings class before it is saved to a template
end;  // TBasePage.SaveCurrentSettings

function TBasePage.GetHTMLImageName: string;
begin
  Result := '';
end;

end.





