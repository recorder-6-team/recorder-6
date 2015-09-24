unit LCDMAPDigiForm1Impl1;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,  Dialogs,
  ActiveX, AxCtrls, LCDAMPDigiProject_TLB, StdVcl,Recorder2000_TLB,StrUtils,Variants,
  StdCtrls, ExtCtrls;

type
  TLCDMAPDigiForm1 = class(TActiveForm, ILCDMAPDigiForm1,IRecorderAddin,INewAction,IDialog)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Button2: TButton;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
   { Private declarations }
   {My declarations }
    FInputFile : string;
    FOutputFile : string;
    FNodesPerRegion : Array[0..100] of Integer;
    { Private declarations }
    FEvents: ILCDMAPDigiForm1Events;
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure PaintEvent(Sender: TObject);
    function  GetOutPutLine(LineInput: widestring): string;
    function  Check_Input_File: boolean;
    function  Check_Output_File: boolean;
    function  GetNodesCount : integer;
  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Get_Active: WordBool; safecall;
    function Get_AlignDisabled: WordBool; safecall;
    function Get_AutoScroll: WordBool; safecall;
    function Get_AutoSize: WordBool; safecall;
    function Get_AxBorderStyle: TxActiveFormBorderStyle; safecall;
    function Get_Caption: WideString; safecall;
    function Get_Color: OLE_COLOR; safecall;
    function Get_DoubleBuffered: WordBool; safecall;
    function Get_DropTarget: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Font: IFontDisp; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_KeyPreview: WordBool; safecall;
    function Get_PixelsPerInch: Integer; safecall;
    function Get_PrintScale: TxPrintScale; safecall;
    function Get_Scaled: WordBool; safecall;
    function Get_ScreenSnap: WordBool; safecall;
    function Get_SnapBuffer: Integer; safecall;
    function Get_Visible: WordBool; safecall;
    function Get_VisibleDockClientCount: Integer; safecall;
    procedure _Set_Font(var Value: IFontDisp); safecall;
    procedure Set_AutoScroll(Value: WordBool); safecall;
    procedure Set_AutoSize(Value: WordBool); safecall;
    procedure Set_AxBorderStyle(Value: TxActiveFormBorderStyle); safecall;
    procedure Set_Caption(const Value: WideString); safecall;
    procedure Set_Color(Value: OLE_COLOR); safecall;
    procedure Set_DoubleBuffered(Value: WordBool); safecall;
    procedure Set_DropTarget(Value: WordBool); safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    procedure Set_Font(const Value: IFontDisp); safecall;
    procedure Set_HelpFile(const Value: WideString); safecall;
    procedure Set_KeyPreview(Value: WordBool); safecall;
    procedure Set_PixelsPerInch(Value: Integer); safecall;
    procedure Set_PrintScale(Value: TxPrintScale); safecall;
    procedure Set_Scaled(Value: WordBool); safecall;
    procedure Set_ScreenSnap(Value: WordBool); safecall;
    procedure Set_SnapBuffer(Value: Integer); safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    {My protected }
    {IRecorderAddin}
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    {INewAction}
    function Get_ActionCaption: WideString; safecall;
    function Get_Hint: WideString; safecall;
    function Get_DimmedImageFilename: WideString; safecall;
    function Get_DisabledImageFileName: WideString; safecall;
    function Get_ParentMenu: WideString; safecall;
    function Get_CanAddToToolbar: WordBool; safecall;
    {IDialog}
    function Get_Width: Integer; safecall;
    function Get_Height: Integer; safecall;
    function DoOk: WordBool; safecall;
    procedure DoCancel; safecall;

  public
    { Public declarations }
    procedure Initialize; override;

  end;

implementation

uses ComObj, ComServ;

{$R *.DFM}

{ TLCDMAPDigiForm1 }
Const
  FixedMifText = 'Version 450' +    chr(13) + chr(10) +
  'Charset "WindowsLatin1"  ' +    chr(13) + chr(10) +
  'Delimiter "," ' +  chr(13) + chr(10) +
  'CoordSys Earth Projection 8, 79, "m", -2, 49, 0.9996012717, 400000, -100000 Bounds' +
  ' (-7845061.1011, -15524202.1641) (8645061.1011, 4470074.53373)' +  chr(13) + chr(10) +
  'Columns 1 ' +  chr(13) + chr(10) +
  'Integer '+  chr(13) + chr(10) +
  'Data ' +  chr(13) + chr(10) +
  'Region 1 ' +  chr(13) + chr(10);
   CRLF = chr(13) + chr(10);
procedure TLCDMAPDigiForm1.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
  { Define property pages here.  Property pages are defined by calling
    DefinePropertyPage with the class id of the page.  For example,
      DefinePropertyPage(Class_LCDMAPDigiForm1Page); }
end;

procedure TLCDMAPDigiForm1.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ILCDMAPDigiForm1Events;
  inherited EventSinkChanged(EventSink);
end;

procedure TLCDMAPDigiForm1.Initialize;
begin
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnPaint := PaintEvent;
end;

function TLCDMAPDigiForm1.Get_Active: WordBool;
begin
  Result := Active;
end;

function TLCDMAPDigiForm1.Get_AlignDisabled: WordBool;
begin
  Result := AlignDisabled;
end;

function TLCDMAPDigiForm1.Get_AutoScroll: WordBool;
begin
  Result := AutoScroll;
end;

function TLCDMAPDigiForm1.Get_AutoSize: WordBool;
begin
  Result := AutoSize;
end;

function TLCDMAPDigiForm1.Get_AxBorderStyle: TxActiveFormBorderStyle;
begin
  Result := Ord(AxBorderStyle);
end;

function TLCDMAPDigiForm1.Get_Caption: WideString;
begin
  Result := WideString(Caption);
end;

function TLCDMAPDigiForm1.Get_Color: OLE_COLOR;
begin
  Result := OLE_COLOR(Color);
end;

function TLCDMAPDigiForm1.Get_DoubleBuffered: WordBool;
begin
  Result := DoubleBuffered;
end;

function TLCDMAPDigiForm1.Get_DropTarget: WordBool;
begin
  Result := DropTarget;
end;

function TLCDMAPDigiForm1.Get_Enabled: WordBool;
begin
  Result := Enabled;
end;

function TLCDMAPDigiForm1.Get_Font: IFontDisp;
begin
  GetOleFont(Font, Result);
end;

function TLCDMAPDigiForm1.Get_HelpFile: WideString;
begin
  Result := WideString(HelpFile);
end;

function TLCDMAPDigiForm1.Get_KeyPreview: WordBool;
begin
  Result := KeyPreview;
end;

function TLCDMAPDigiForm1.Get_PixelsPerInch: Integer;
begin
  Result := PixelsPerInch;
end;

function TLCDMAPDigiForm1.Get_PrintScale: TxPrintScale;
begin
  Result := Ord(PrintScale);
end;

function TLCDMAPDigiForm1.Get_Scaled: WordBool;
begin
  Result := Scaled;
end;

function TLCDMAPDigiForm1.Get_ScreenSnap: WordBool;
begin
  Result := ScreenSnap;
end;

function TLCDMAPDigiForm1.Get_SnapBuffer: Integer;
begin
  Result := SnapBuffer;
end;

function TLCDMAPDigiForm1.Get_Visible: WordBool;
begin
  Result := Visible;
end;

function TLCDMAPDigiForm1.Get_VisibleDockClientCount: Integer;
begin
  Result := VisibleDockClientCount;
end;

procedure TLCDMAPDigiForm1._Set_Font(var Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TLCDMAPDigiForm1.ActivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnActivate;
end;

procedure TLCDMAPDigiForm1.ClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnClick;
end;

procedure TLCDMAPDigiForm1.CreateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnCreate;
end;

procedure TLCDMAPDigiForm1.DblClickEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDblClick;
end;

procedure TLCDMAPDigiForm1.DeactivateEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDeactivate;
end;

procedure TLCDMAPDigiForm1.DestroyEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnDestroy;
end;

procedure TLCDMAPDigiForm1.KeyPressEvent(Sender: TObject; var Key: Char);
var
  TempKey: Smallint;
begin
  TempKey := Smallint(Key);
  if FEvents <> nil then FEvents.OnKeyPress(TempKey);
  Key := Char(TempKey);
end;

procedure TLCDMAPDigiForm1.PaintEvent(Sender: TObject);
begin
  if FEvents <> nil then FEvents.OnPaint;
end;

procedure TLCDMAPDigiForm1.Set_AutoScroll(Value: WordBool);
begin
  AutoScroll := Value;
end;

procedure TLCDMAPDigiForm1.Set_AutoSize(Value: WordBool);
begin
  AutoSize := Value;
end;

procedure TLCDMAPDigiForm1.Set_AxBorderStyle(
  Value: TxActiveFormBorderStyle);
begin
  AxBorderStyle := TActiveFormBorderStyle(Value);
end;

procedure TLCDMAPDigiForm1.Set_Caption(const Value: WideString);
begin
  Caption := TCaption(Value);
end;

procedure TLCDMAPDigiForm1.Set_Color(Value: OLE_COLOR);
begin
  Color := TColor(Value);
end;

procedure TLCDMAPDigiForm1.Set_DoubleBuffered(Value: WordBool);
begin
  DoubleBuffered := Value;
end;

procedure TLCDMAPDigiForm1.Set_DropTarget(Value: WordBool);
begin
  DropTarget := Value;
end;

procedure TLCDMAPDigiForm1.Set_Enabled(Value: WordBool);
begin
  Enabled := Value;
end;

procedure TLCDMAPDigiForm1.Set_Font(const Value: IFontDisp);
begin
  SetOleFont(Font, Value);
end;

procedure TLCDMAPDigiForm1.Set_HelpFile(const Value: WideString);
begin
  HelpFile := String(Value);
end;

procedure TLCDMAPDigiForm1.Set_KeyPreview(Value: WordBool);
begin
  KeyPreview := Value;
end;

procedure TLCDMAPDigiForm1.Set_PixelsPerInch(Value: Integer);
begin
  PixelsPerInch := Value;
end;

procedure TLCDMAPDigiForm1.Set_PrintScale(Value: TxPrintScale);
begin
  PrintScale := TPrintScale(Value);
end;

procedure TLCDMAPDigiForm1.Set_Scaled(Value: WordBool);
begin
  Scaled := Value;
end;

procedure TLCDMAPDigiForm1.Set_ScreenSnap(Value: WordBool);
begin
  ScreenSnap := Value;
end;

procedure TLCDMAPDigiForm1.Set_SnapBuffer(Value: Integer);
begin
  SnapBuffer := Value;
end;

procedure TLCDMAPDigiForm1.Set_Visible(Value: WordBool);
begin
  Visible := Value;
end;

procedure TLCDMAPDigiForm1.DoCancel;
begin

end;

function TLCDMAPDigiForm1.DoOk: WordBool;
var
 InputVar: textfile;
 OutPutVar: textfile;
 LineInput: widestring;
 OutPutLine: widestring;
 Segment : integer;
begin
   if (FInputFile <> '') and (FoutputFile <> '') then
     begin
       Segment := 1;
       AssignFile(InputVar, FInputFile);
       Reset (InputVar);
       AssignFile(OutPutVar, FOutputFile);
       Rewrite (OutPutVar);
       write (OutPutVar, FixedMifText,' ',inttostr(FNodesPerRegion[Segment] ),CRLF );
        While not eof(InputVar) do
        begin
         ReadLN(InputVar, LineInput);

         if LineINput = '-1' then
          begin
            Segment  := Segment + 1;
            OutPutLine := 'Region 1' +   CRLF + ' ' + inttostr(FNodesPerRegion[Segment]) + CRLF;
          end

          else
           begin
            OutPutLine := GetOutPutLine(LineInput);
           end;
           write (OutPutVar, OutPutLine );
       end;
         messagedlg('Complete -  click cancel to exit or change files to process another boundary',mtConfirmation,[mbOK],0);
     end
     else
        messagedlg('You must select both files',mtConfirmation,[mbOK],0);

     Closefile (InputVar);
     Closefile (OutPutVar);


end;

function TLCDMAPDigiForm1.Get_ActionCaption: WideString;
begin
     result := 'DMAP Digitizer Convert V1';
end;

function TLCDMAPDigiForm1.Get_CanAddToToolbar: WordBool;
begin
     result := False;
end;

function TLCDMAPDigiForm1.Get_Description: WideString;
begin
    result := 'DMAP Digitizer to R6';
end;

function TLCDMAPDigiForm1.Get_DimmedImageFilename: WideString;
begin
     result:= 'Default.bmp' ;
end;

function TLCDMAPDigiForm1.Get_DisabledImageFileName: WideString;
begin
     result:= 'Default.bmp' ;
end;

function TLCDMAPDigiForm1.Get_Height: Integer;
begin
     result := 373;
end;

function TLCDMAPDigiForm1.Get_Hint: WideString;
begin
     result := 'DMAP Digitizer';
end;

function TLCDMAPDigiForm1.Get_ImageFileName: WideString;
begin
     result:= 'Default.bmp' ;
end;

function TLCDMAPDigiForm1.Get_Name: WideString;
begin
     result := 'DMAP Digitizer To R6 Format V6.22';
end;

function TLCDMAPDigiForm1.Get_ParentMenu: WideString;
begin
      result:= 'Tools' ;
end;

function TLCDMAPDigiForm1.Get_Width: Integer;
begin
     result := 303;
end;

procedure TLCDMAPDigiForm1.Install(const iInstalledFilePath: WideString);
begin

end;

procedure TLCDMAPDigiForm1.Button1Click(Sender: TObject);
begin
  with OpenDialog1 do
    begin
      Title := 'DMAP digitizer file';
      Filter := 'Digitizer file(*.bdy)|*.bdy';
      FileName := '';
    end;
  OpenDialog1.Execute;
  FInputFile := OpenDialog1.FileName;
  Check_Input_File();
  label2.Caption := FinputFile;
end;

procedure TLCDMAPDigiForm1.Button2Click(Sender: TObject);
begin
  with OpenDialog1 do
    begin
      Title := 'Convert to Map Info file';
      Filter := 'MapInfo file(*.mif)|*.mif';
      FileName := '';
    end;
  OpenDialog1.Execute;
  FOutputFile := OpenDialog1.FileName;
  Check_Output_File();
  label4.Caption := FOutputFile;
end;
function TLCDMAPDigiForm1.Check_Input_File: boolean;
var
CheckFileVar : textfile;

begin
  If (FInputFile <> '') then
    begin
      Try
        AssignFile(CheckFilevar, FInputFile);
        Reset (CheckFileVar);
        Closefile (CheckFileVar);
        if  GetNodesCount() = 0 then
          begin
             Result := false;
             FInputFile := '';
             messagedlg('File may be in wrong format or has no data',mtConfirmation,[mbOK],0);
          end;

        except
        begin
          Result := false;
          FInputFile := '';
            messagedlg('File may be in use',mtConfirmation,[mbOK],0);
       end;
     end;  // end of try
  end

  else
     messagedlg('You must select a file',mtConfirmation,[mbOK],0);

end;

function TLCDMAPDigiForm1.Check_Output_File: boolean;
var
CheckFileVar : textfile;

begin
  If (FOutputFile <> '') then
    begin
    
      if ansipos('.',FOutPutFile) = 0 then FOutputFile := FOutputFile + '.mif';

      if UPPERCASE(AnsiRightStr(FOutputFile,4)) = '.MIF' then
        Begin
          Try
            AssignFile(CheckFilevar, FOutputFile);
            Rewrite(CheckFileVar);
            Closefile (CheckFileVar);
            Result := true;
          except
          begin
            Result := false;
            FOutputFile := '';
            messagedlg('File may be in use or you do not have permission to write to the folder',mtConfirmation,[mbOK],0);
          end;
         end;  // end of try
      end
      else
         Begin
           Result := false;
            FOutputFile := '';
            messagedlg('File must have extension .mif',mtConfirmation,[mbOK],0);
         end;
  end
  else
     messagedlg('You must select an output file',mtConfirmation,[mbOK],0);
end;
function TLCDMAPDigiForm1.GetOutPutLine(LineInput: widestring): string;
Var
FirstRef : real;
SecondRef : real;
PositionFirstComma : integer;
begin
  PositionFirstComma :=  Ansipos(',',LineInput);
  if PositionFirstComma <> 0 then
  begin
     Try

       FirstRef := strtofloat(AnsiLeftStr(LineInput,PositionFirstComma-1)) * 1000;
       SecondRef := strtofloat(AnsiRightStr(LineInput,length(LineInput)- PositionFirstComma )) * 1000;
       Result := floattostrF(FirstRef,ffGeneral,6,6) + ' ' + floattostrF(SecondRef,ffGeneral,7,6) + CRLF;
     except
       Result := '';

     end;




  end
  else
     Result := 'Invalid GR';




end;
function TLCDMAPDigiForm1.GetNodesCount : integer;
var
Inputvar : textFile;
X : integer;
Y : integer;
InputOfLine : widestring;
begin
   AssignFile(InputVar, FInputFile);
   Reset (InputVar);
   X := 0;
   Y := 0;
   While not eof(InputVar) do
        begin
         ReadLN(InputVar, InputOfLine);
         if InputOfLine <> '-1' then
           begin
               X := X + 1;
           end
         else
            begin
               Y := Y + 1;
               Try
                 FNodesPerRegion[Y] := X;
               except
                 Result := 0;
               end;
               X := 0;

            end;
   end;
   Result := Y;
   Closefile (InputVar);
end;

initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TLCDMAPDigiForm1,
    Class_LCDMAPDigiForm1,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
