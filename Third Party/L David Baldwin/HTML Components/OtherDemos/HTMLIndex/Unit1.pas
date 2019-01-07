
{Thanks to Peter Jones for this example of how to make an HTML expanding
 index.}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Htmlview, HTMLIndex, Buttons, StdCtrls;

type
  TForm1 = class(TForm)
    Viewer: THTMLViewer;
    Panel1: TPanel;
    IndexBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ViewerHotSpotClick(Sender: TObject; const SRC: String;
      var Handled: Boolean);
    procedure IndexBtnClick(Sender: TObject);

  private
    Index:THTMLIndex;
    procedure InitIndex;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation



{$R *.DFM}
procedure TForm1.InitIndex;
begin
  Index:=THTMLIndex.create(Viewer);
  with Index do
    begin
      AddHeading('Heading 1');
        AddChild(0,'Item 1','/TestFile.htm');
        AddChild(0,'Item 2','/TestFile.htm');
        AddChild(0,'Item 3','/TestFile.htm');

        
      AddHeading('Heading 2');
        AddChild(1,'Item 1','/TestFile.htm');
        AddChild(1,'Item 2','/TestFile.htm');
        AddChild(1,'Item 3','/TestFile.htm');


      LoadIndex;
    end;
end;
procedure TForm1.FormCreate(Sender: TObject);
begin
  InitIndex;
   Viewer.ServerRoot:= ExtractFilePath(ParamStr(0));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Index.free;

end;

procedure TForm1.ViewerHotSpotClick(Sender: TObject; const SRC: String;
  var Handled: Boolean);
var
  n:integer;
begin
  if src[1]='*' then
    begin
      handled:=true;
      n:=StrToInt(copy(src,2,length(src)));
      Index.ToggleExpanded(n);
    end
  else handled:=false;
end;

procedure TForm1.IndexBtnClick(Sender: TObject);
begin
  Index.LoadIndex;
end;

end.
