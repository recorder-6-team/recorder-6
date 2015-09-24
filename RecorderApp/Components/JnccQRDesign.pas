unit JnccQRDesign;

interface

uses
  SysUtils, Classes, StdCtrls, Forms, Db, QRDesign, QRDTools;

type
  TJnccQRepDesigner = class(TQRepDesigner)
  public
    procedure RefreshDatafieldListbox; override;
  end;


procedure Register;

//==============================================================================
implementation

//==============================================================================
procedure Register;
begin
  RegisterComponents('JNCC', [TJnccQRepDesigner]);
  RegisterComponents('JNCC', [TEventScrollBox, TRulerPanel]);
end;  // Register

//==============================================================================
procedure TJnccQRepDesigner.RefreshDatafieldListbox;
var
    DS        :TDataset;
    lField    :TField;
    Y       :Integer;
  //----------------------------------------------------------------------------
  function Convert(const iText:string):string;
  var lIdx,lLen:integer;
  begin
    Result:=iText;
    if Result<>'' then begin
      Result:=LowerCase(Result);
      lLen:=Length(Result);
      // First convert '_' to ' '
      lIdx:=Pos('_',Result);
      while lIdx>0 do begin
        Result[lIdx]:=' ';
        lIdx:=Pos('_',Result);
      end;  // while
      // Now change first letter of each 'word' to uppercase
      Result[1]:=UpCase(Result[1]);
      for lIdx:=1 to lLen do
        if (Result[lIdx]=' ') and (lIdx+1<=lLen) then
          Result[lIdx+1]:=UpCase(Result[lIdx+1]);
    end;  // if
  end;  // Convert
  //----------------------------------------------------------------------------
begin
  if DatafieldListbox=nil then Exit;
  if not ShowDatafieldListbox then
  begin
    TForm(DatafieldListbox.Owner).Visible:=False;
    Exit;
  end;
 { Simplified the original code here as we only ever use 1 dataset. }
  TListbox(DatafieldListbox).Items.Clear;
  DS:=Self.QReport.Dataset;
  for Y:=1 to DS.FieldCount do begin
    lField:=DS.Fields[Y-1];
    if lField.Visible and (Pos('SPATIAL_REF_SYSTEM',UpperCase(lField.FieldName))=0) then
      TListbox(DatafieldListbox).Items.AddObject(Convert(lField.DisplayLabel),DS);
  end;  // for Y
  TForm(DatafieldListbox.Owner).Visible:=TListbox(DatafieldListbox).Items.Count>0;
end;




//==============================================================================
end.

