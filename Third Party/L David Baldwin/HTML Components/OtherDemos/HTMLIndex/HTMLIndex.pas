unit HTMLIndex;
{------------------ Expanding index for THTMLViewer --------------------
}
interface
uses
  Windows, Classes, Htmlview;
  type
    TInfo=class
      private
        FText           :string;
        FLevel          :integer;
        FURL            :string;
      public
        constructor create(const Text, URL:string; const level:integer);
        property Text:string read FText write FText;
        property URL:string read FURL write FURL;
        property level:integer read FLevel write FLevel;
    end;

    TChildItem=class
      private
        FText           :string;
        FURL            :string;
      public
        constructor create(const Text, URL:string);
    end;

    THeading=class
      private
         FText          :string;
         FID:integer;
         FChildList     :TList;
         FExpanded      :boolean;
         procedure ClearChildren;
      public
        constructor create(const Title:string);
        destructor destroy; override;
    end;

    THTMLIndex=class
      private
        FList           :TList;
        FViewer         :THTMLViewer;
        procedure ClearList;
        function GetExpanded(const index:integer):boolean;
        procedure SetExpanded(const index:integer; const value:boolean);
        function IsAllCollapsed:boolean;
      public
        constructor create(AViewer:THTMLViewer);
        destructor destroy; override;
        procedure AddHeading(const title:string);
        procedure AddChild(const nHeading:integer;
                           const Text, URL:string);
        property Expanded[const index:integer]:boolean
                          read GetExpanded write SetExpanded;
        procedure LoadIndex;
        procedure ToggleExpanded(const nIndex:integer);
    end;

implementation
uses
  SysUtils;  
const
  header='<html>' +
          '<body >' +
          '<table border="0" width="100%" cellspacing="0" cellpadding="0">' +
          '<tr>' +
          '<td width="100%" bgcolor="#D7D7D7"><font color="#000000"><strong>&nbsp; Index</strong></font></td>'+
          '</tr></table><body><br>';
  headerEx='<html>' +
           '<body >' +
           '<table border="0" width="100%" cellspacing="0" cellpadding="0">' +
           '<tr>' +
           '<td width="100%" bgcolor="#D7D7D7"><font color="#000000"><strong>&nbsp; Index</strong></font></td>'+
           '</tr>'+
           '<tr>'+
           '<td> <img src="Help.gif" width="24" height="27" border="0"> &nbsp;'+
           'Click on one of the book symbols or headings to expand the list of topics.</td></tr>'+
           '</table><body><br>';
//*************************************************************************
//TInfo 
constructor TInfo.create(const Text, URL:string; const level:integer);
begin
  FText:=text;
  FURL:=URL;
  FLevel:=level;
end;
//*************************************************************************
//TChildItem
constructor TChildItem.create(const Text, URL:string);
begin
  inherited create;
  FText:=text;
  FURL:=URL;
end;
//*************************************************************************
//THeading
constructor THeading.create(const Title:string);
begin
  inherited create;
  FChildlist:=TList.create;
  FText:=Title;
end;
//*************************************************************************
destructor THeading.destroy;
begin
  ClearChildren;
  FChildList.free;
  inherited destroy;
end;
//*************************************************************************
procedure THeading.ClearChildren;
var
  temp:TChildItem;
begin
  with FChildList do
    while count>0 do
      begin
        temp:=items[0];
        temp.free;
        delete(0);
      end;
end;
//*************************************************************************
//THTMLIndex

constructor THTMLIndex.create(AViewer:THTMLViewer);
begin
  inherited create;
  FList:=TList.create;
  FViewer:=AViewer;
end;
//*************************************************************************
destructor THTMLIndex.destroy;
begin
  ClearList;
  FList.free;
  inherited destroy;
end;
//*************************************************************************
procedure THTMLIndex.ClearList;
var
  temp:THeading;
begin
  with FList do
    while count>0 do
      begin
        temp:=items[0];
        temp.free;
        delete(0);
      end;
end;
//*************************************************************************
procedure THTMLIndex.AddHeading(const title:string);
var
  temp:THeading;
begin
  temp:=THeading.create(title);
  temp.FID:=FList.count;
  FList.add(temp);
end;
//*************************************************************************
procedure THTMLIndex.AddChild(const nHeading:integer;
                              const Text, URL:string);
var
  AHeading:THeading;
  temp:TChildItem;
begin
  if (nHeading>=0) and (nHeading<FList.count) then
    begin
      AHeading:=FList.items[nHeading];
      temp:=TChildItem.create(text,URL);
      AHeading.FChildList.add(temp);
    end;
end;
//*************************************************************************
function THTMLIndex.GetExpanded(const index:integer):boolean;
begin
  if (index>=0) and (index<FList.count) then
    begin
      result:=THeading(FList.items[index]).FExpanded;
    end
  else result:=false;
end;
//*************************************************************************
procedure THTMLIndex.SetExpanded(const index:integer; const value:boolean);
begin
  if (index>=0) and (index<FList.count) then
    begin
      THeading(FList.items[index]).FExpanded:=value;
    end
end;
//*************************************************************************
procedure THTMLIndex.ToggleExpanded(const nIndex:integer);
var
  temp:THeading;
begin
  if (nIndex>=0) and (nIndex<FList.count) then
    begin
      temp:=FList.items[nIndex];
      temp.FExpanded:=not temp.FExpanded;
      LoadIndex;
    end
end;
//*************************************************************************
function THTMLIndex.IsAllCollapsed:boolean;
var
  j:integer;
begin
  result:=true;
  for j:=0 to FList.count-1 do
    if THeading(FList.items[j]).FExpanded then
      begin
        result:=false;
        exit;
      end;
end;
//*************************************************************************
procedure THTMLIndex.LoadIndex;
var
  s:string;
  j,k:integer;
  AHead:THeading;
  AChild:TChildItem;
begin
  if IsAllCollapsed then s:=HeaderEx
  else s:=header;
  s:=s+ '<table border="0" width="100%">';
  for j:=0 to FList.count-1 do
    begin
      AHead:=FList.items[j];

      if AHead.FExpanded then
        begin
          s:=s+'<tr>' +
             '<td><a href="*'+   IntToStr(j) +
              '"><img src="MinusBook.gif" width="29" height="15" border="0"> &nbsp;<strong>' +
              '<font color="#000000">'+AHead.FText +'</font></a></strong></td></tr>';
          if (AHead.FChildList.count>0) then
            begin
              for k:=0 to AHead.FChildList.count-1 do
                begin

                  AChild:=AHead.FChildList.items[k];

                  s:=s+ '<tr><td><blockquote><p><img src="topic.gif" width="12" height="15"><a href="' +
                         AChild.FURL + '"> &nbsp;' +
                         AChild.FText +  '</a></p></blockquote></td></tr>';
                end;
            end;

        end
      else begin
          s:=s+'<tr>' +
             '<td><a href="*'+   IntToStr(j) +
              '"><img src="PlusBook.gif" width="29" height="15" border="0"> &nbsp;<strong>' +
              '<font color="#000000">'+AHead.FText +'</font></a></strong></td></tr>';
        end;

    end;
  s:=s+'</table></body></html>';

  FViewer.LoadFromBuffer(pChar(s),Length(s));

end;


end.
