unit TaskProgress;  { TTaskProgressBar component. }
{.CDK.REGLINK=InHouseReg.pas}  { Registration file is InHouseReg.pas. }
{ Created 09/06/99 08:24:10 }
{ Eagle Software CDK, Version 4.02 Rev. M }

{ Progress bar with intelligent task management }
{ Modified 09/06/99 09:08:47 by the CDK, Version 4.02 Rev. M }
{ Modified 09/06/99 09:09:45 by the CDK, Version 4.02 Rev. M }

interface

uses
  Windows, 
  SysUtils, 
  Messages, 
  Classes, 
  Graphics, 
  Controls,
  Forms,
  Dialogs, 
  Menus,
  StdCtrls,
  ExtCtrls,
  ComCtrls;

type
  { Simple object to allow the current embedded tasks position to be stored on
    the task stack }
  TTaskRange = class(TObject)
  private
    FEnd : integer;
    FStart : integer;
    FLastTask : TTaskRange;
    function GetBarPosEnd: integer;
    function GetBarPosStart: integer;
  public
    constructor Create( const iStart, iEnd : integer; iLastTask : TTaskRange );
    property TaskStart : integer read FStart;
    property TaskEnd : integer read FEnd;
    property BarPosStart : integer read GetBarPosStart;
    property BarPosEnd : integer read GetBarPosEnd;
  end;

  ETaskProgressBarError = class(Exception);

  TTaskProgressBar = class(TProgressBar)
  private
    { Private declarations }
    FTaskStack: TList;
    FTaskPosition: Integer;
    FRaiseErrors: boolean;
  protected
    { Protected declarations }
    function GetTaskPosition: Integer; virtual;
    procedure SetTaskPosition(newValue: Integer); virtual;
    procedure SetRaiseErrors(const Value: boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EmbedTask(const iStart, iEnd : integer): Integer;
    procedure FinishTask(const iIndex : integer);
    function FinishAndEmbedNext(const iIndex, iStart, iEnd : integer) : integer;
    procedure Reset;
  published
    { Published properties and events }
    property TaskPosition: Integer read GetTaskPosition write SetTaskPosition default 0;  { Published }
    property RaiseErrors: boolean read FRaiseErrors write SetRaiseErrors;
  end;  { TTaskProgressBar }

implementation

resourcestring
  ResStr_InvalidTaskRange = 'The task cannot be embedded - range invalid.';
  ResStr_InvalidTaskFinish= 'Task cannot be finished - index was invalid.';

{ Perform a complete reset of the task bar }
procedure TTaskProgressBar.Reset;
var
  i : integer;
begin
  for i := 0 to FTaskStack.Count-1 do
    TTaskRange(FTaskStack[i]).Free;
  FTaskStack.Clear;
  TaskPosition := 0;
end;


function TTaskProgressBar.GetTaskPosition: Integer;
{ Returns the value of data member FTaskPosition. }
begin
  { CDK: Add query/calculation code here and/or modify result below. }
  GetTaskPosition := FTaskPosition;
end;  { GetTaskPosition }



procedure TTaskProgressBar.SetTaskPosition(newValue: Integer);
{ Sets data member FTaskPosition to newValue. }
begin
  FTaskPosition := newValue;
  if FTaskStack.Count > 0 then
    with TTaskRange(FTaskStack[FTaskStack.Count-1]) do
      Position := ((GetBarPosEnd - GetBarPosStart) * FTaskPosition) div 100 +
                  GetBarPosStart
  else
    Position := FTaskPosition;
end;  { SetTaskPosition }



{ Embeds a task into the progress bar, and returns the index of the task }
function TTaskProgressBar.EmbedTask(const iStart, iEnd : integer): Integer;  { public }
var
  lNewTask : TTaskRange;
begin
  if (iStart < 0) or (iStart > 100) or (iEnd < 0) or (iEnd > 100) then
    raise ETaskProgressBarError.Create(ResStr_InvalidTaskRange);
  if FTaskStack.Count > 0 then
    lNewTask := TTaskRange.Create(iStart, iEnd, FTaskStack[FTaskStack.Count-1])
  else
    lNewTask := TTaskRange.Create(iStart, iEnd, nil);
  FTaskStack.Add(lNewTask);
  { Set task position to start of task }
  TaskPosition := 0;
  Result := FTaskStack.Count-1;
end;  { EmbedTask }



procedure TTaskProgressBar.FinishTask(const iIndex : integer);  { public }
begin
  { CDK: Add method implementation code here. }
  if FRaiseErrors then begin
    { raise an error if task freeing has gotten out of sync }
    if iIndex <> FTaskStack.Count-1 then
      raise ETaskProgressBarError.Create( ResStr_InvalidTaskFinish );
    { Remove the current stacked item }
    TTaskRange(FTaskStack[FTaskStack.Count-1]).Free;
    FTaskStack.Delete(FTaskStack.Count-1);
  end else begin
    { don't bother raising error }
    if iIndex = FTaskStack.Count-1 then begin
      { Remove the current stacked item }
      TTaskRange(FTaskStack[FTaskStack.Count-1]).Free;
      FTaskStack.Delete(FTaskStack.Count-1);
    end;
  end;
end;  { FinishTask }


{ Procedure to combine FinishTask and embedTask into one operation.  Return
    result is the new embedded task index }
function TTaskProgressBar.FinishAndEmbedNext(const iIndex, iStart,
  iEnd: integer): integer;
begin
  FinishTask( iIndex );
  Result := EmbedTask( iStart, iEnd );
end;


destructor TTaskProgressBar.Destroy;
var
  i : integer;
begin
  if FTaskStack <> nil then
    for i := 0 to FTaskStack.Count-1 do
      TTaskRange(FTaskStack[i]).Free;
  FTaskStack.Free;
  inherited Destroy;
end;  { Destroy }



constructor TTaskProgressBar.Create(AOwner: TComponent);
{ Creates an object of type TTaskProgressBar, and initializes properties. }
begin
  inherited Create(AOwner);
  { Initialize properties with default values: }
  FTaskPosition := 0;
  FTaskStack := TList.Create;
  FRaiseErrors := False;
end;  { Create }


//==============================================================================
{ TTaskRange }
//==============================================================================


constructor TTaskRange.Create(const iStart, iEnd: integer; iLastTask : TTaskRange);
begin
  inherited Create;
  FStart := iStart;
  FEnd := iEnd;
  FLastTask := iLastTask;
end;


function TTaskRange.GetBarPosEnd: integer;
begin
  if FLastTask = nil then
    Result := FEnd
  else
    Result := ((FLastTask.BarPosEnd - FLastTask.BarPosStart) *
              FEnd) div 100 + FLastTask.BarPosStart;
end;


function TTaskRange.GetBarPosStart: integer;
begin
  if FLastTask = nil then
    Result := FStart
  else
    Result := ((FLastTask.BarPosEnd - FLastTask.BarPosStart) *
              FStart) div 100 + FLastTask.BarPosStart;
end;




procedure TTaskProgressBar.SetRaiseErrors(const Value: boolean);
begin
  FRaiseErrors := Value;
end;

end.

