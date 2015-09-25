{===============================================================================
  Unit:        FixedWidths

  Defines:     TfraFixedWidths

  Description: Allow user to set width of fields in a fixed-width file.

  Model:       ImportWizard

  Last revision information:
    $Revision: 15 $
    $Date: 5/05/09 15:55 $
    $Author: Ericsalmon $

===============================================================================}

unit FixedWidths;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IWBasePage, HtmlView, Grids, StdCtrls, ExtCtrls, IWSettings, SMI2TXT,
  SMIBase, DB, DBGrids;

type
  {-----------------------------------------------------------------------------
    Wizard page displayed when a fixed width text file is imported.  This allows the user to 
    determine the width of each column by dragging and dropping separators in the data.
  }
  TfraFixedWidths = class (TBasePage)
    lblArrowLines: TLabel;
    lblAddingBreak: TLabel;
    lblRemovingBreak: TLabel;
    lblMovingBreak: TLabel;
    pbBreaks: TPaintBox;
    sbBreakHorz: TScrollBar;
    sbBreakVert: TScrollBar;
    procedure pbBreaksMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, 
        Y: Integer);
    procedure pbBreaksMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbBreaksMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: 
        Integer);
    procedure pbBreaksPaint(Sender: TObject);
    procedure sbBreakHorzScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos:
        Integer);
    procedure sbBreakVertScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos:
        Integer);
    procedure pbBreaksClick(Sender: TObject);
  private
    FBreaks: TList;
    FBreakShiftX: Integer;
    FBreakShiftY: Integer;
    FFileContent: TStringList;
    FInDownMouse: Integer;
    FMouseDown: Boolean;
    FLastFieldLength: Integer;
    FLastFieldIndex: Integer;
    FMaxLineLength: Integer;
    FReloadRequired, FReloading: boolean;
    FSMTxtImport: TSMImportFromText;
    function GetBreak(X: Integer; IsAdd: Boolean): Integer;
    function ValidatePage: boolean;
    function CreateFieldMappings: TStringList;
    procedure ImportData;
    procedure SMImportErrorEvent(Sender: TObject; Error: Exception;
      var Abort: Boolean);
    procedure SMImportAfterRecordEvent(Sender: TObject; var Abort: Boolean);
    procedure SMImportGetCellParams(Sender: TObject; Field: TField;
      var Value: Variant);
    procedure CreateDataStructure;
  protected
    function GetHasNext: Boolean; override;
    function GetHasPrevious: Boolean; override;
    function GetNext: TBasePageClass; override;
    function GetPrevious: TBasePageClass; override;
    procedure LoadContent; override;
    function GetHtmlImageName: String; override;
  public
    constructor Create(AOwner: TComponent; ASettings: TdmIWSettings); override;
    procedure SaveContent; override;
    procedure ValidateContent; override;
  end;

//==============================================================================
implementation

{$R *.dfm}

uses
  FileSelect, ColumnTypes, FastColumnTypes, IWConstants, MaintBar, GeneralFunctions,
  DatabaseAccessADO, OnlineHelp;

const
  DEFAULT_FIELD_SIZE = 100;
  RECORDS_TO_PREVIEW = 20;
  LAST_IMPORT_FIELD_WIDTH = 1000;
  FIELD_NAME_PREFIX = 'Field';

resourcestring
  ResStr_ValidationFailed =
    'Validation on the page failed - see the help in the left panel for details.';  
  ResStr_FailedToImportData =
    'The data import failed. Please check that the properties are correctly set and that the import file is not empty.';
  ResStr_NoBreaksAdded =  'no breaks have been added.';

function CompareIntegers(APointer1, APointer2: Pointer): Integer;
begin
  Result := (Integer(APointer1)) - (Integer(APointer2));
end;  // CompareIntegers

{-==============================================================================
    TfraFixedWidths
===============================================================================}
{-------------------------------------------------------------------------------
}
function TfraFixedWidths.GetBreak(X: Integer; IsAdd: Boolean): Integer;
var
  i, j, lPos, lCharWidth: Integer;
  lIsFound: Boolean;
begin
  Result := -1;
  
  lCharWidth := pbBreaks.Canvas.TextWidth('W');
  lPos := X div lCharWidth;
  if IsAdd and (lPos = 0) then Exit;
  
  lIsFound := False;
  for i := 0 to FBreaks.Count-1 do
  begin
    j := LongInt(FBreaks[i]) - FBreakShiftX;
    if (j = lPos) then
    begin
      lIsFound := True;
      Result := i;
      Break
    end;
  end;
  
  if IsAdd and not lIsFound then
  begin
    if (FBreaks.Count = 0) then
      i := 0;
    FBreaks.Insert(i, TObject(FBreakShiftX + lPos));
    pbBreaks.Repaint;
  end;
end;  // TfraFixedWidths.GetBreak 

{-------------------------------------------------------------------------------
}
function TfraFixedWidths.GetHasNext: Boolean;
begin
  Result := ValidatePage;
end;  // TfraFixedWidths.GetHasNext 

{-------------------------------------------------------------------------------
}
function TfraFixedWidths.GetHasPrevious: Boolean;
begin
  Result := True;
end;  // TfraFixedWidths.GetHasPrevious 

{-------------------------------------------------------------------------------
}
function TfraFixedWidths.GetNext: TBasePageClass;
begin
  if Settings.UseOldImportWizard then
    Result := TfraColumnTypes
  else
    Result := TfraFastColumnTypes;
end;  // TfraFixedWidths.GetNext 

{-------------------------------------------------------------------------------
}
function TfraFixedWidths.GetPrevious: TBasePageClass;
begin
  Result := TfraFileSelect;
end;  // TfraFixedWidths.GetPrevious

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.LoadContent;
var
  lFieldNames: string;
  i, lLength: Integer;
begin
  inherited;
  FFileContent := TStringList.Create;
  //call SMI2TXT method to load 20 records of the specified file into
  //the display box
  //nb do not have text qualifier or record separator for fixed width
  LoadTextFile(True, True, lFieldNames, FFileContent,
               Settings.SourceDataFile, Settings.TextRecordSeparator,
               #0, #0, Settings.ImportRowsFrom,
               Settings.ImportRowsFrom + RECORDS_TO_PREVIEW, 20, 1, 0);
  sbBreakVert.Max := FFileContent.Count;

  FMaxLineLength := 0;
  for i := 0 to FFileContent.Count - 1 do begin
    lLength := Length(FFileContent[i]);
    if FMaxLineLength < lLength then FMaxLineLength := lLength;
  end;

  //if we are coming back to the page then there should be a record of the
  //breaks previously set by user
  if Assigned(Settings.FixedWidthBreaks) then
    FBreaks := Settings.FixedWidthBreaks
  else begin
    Settings.FixedWidthBreaks := TList.Create;
    FBreaks := Settings.FixedWidthBreaks;
  end;

  FInDownMouse := -1;
end;  // TfraFixedWidths.LoadContent 

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.pbBreaksMouseDown(Sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
begin
  FInDownMouse := GetBreak(X, False);
  FMouseDown := True;
  
  if (ssDouble in Shift) then begin
    if (FInDownMouse > -1) then begin
      FBreaks.Delete(FInDownMouse);
      pbBreaks.Repaint;
      FInDownMouse := -1
    end
  end else
  if (FInDownMouse < 0) then
    GetBreak(X, True);
end;  // TfraFixedWidths.pbBreaksMouseDown

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.pbBreaksMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
    Integer);
var
  lNew, lPos, lCharWidth: Integer;
begin
  if FMouseDown and (FInDownMouse > -1) then
  begin
    lCharWidth := pbBreaks.Canvas.TextWidth('W');
    lPos := X div lCharWidth;
    if (lPos = 0) then exit;
  
    lNew := FBreakShiftX + lPos;
    if LongInt(FBreaks[FInDownMouse]) <> lNew then
    begin
      FBreaks[FInDownMouse] := TObject(lNew);
      pbBreaks.Repaint;
    end;
  end
end;  // TfraFixedWidths.pbBreaksMouseMove 

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.pbBreaksMouseUp(Sender: TObject; Button: TMouseButton; Shift: 
    TShiftState; X, Y: Integer);
begin
  FMouseDown := False;
end;  // TfraFixedWidths.pbBreaksMouseUp 

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.pbBreaksPaint(Sender: TObject);
var
  lRect: TRect;
  i, j, k, lCharWidth, lCharHeight, lSampleWidth, lSampleHeight: Integer;
  lBreak: LongInt;
  lString: String;
begin
  with pbBreaks, Canvas do
  begin
    lCharWidth := TextWidth('W');
    lCharHeight := TextHeight('Wg');
  
    lRect := ClientRect;
    lRect.Top := lRect.Top + lCharHeight + 15;
    Brush.Color := clBlack;
    FrameRect(lRect);
  
    InflateRect(lRect, -1, -1);
  
    lSampleWidth := (lRect.Right - lRect.Left) div lCharWidth;
    lSampleHeight := (lRect.Bottom - lRect.Top) div lCharHeight;
    sbBreakHorz.LargeChange := lSampleWidth;
    sbBreakVert.LargeChange := lSampleHeight;
    // Keep max scroll in synch with available width.
    sbBreakHorz.Max := Max(0, FMaxLineLength - lSampleWidth);
    if sbBreakHorz.Position = sbBreakHorz.Max then FBreakShiftX := sbBreakHorz.Max;

    Brush.Color := clWhite;
    Pen.Color := clBlack;
    FillRect(lRect);

    {draw a text sample}
    for i := 0 to lSampleHeight - 1 do
    begin
      if i + FBreakShiftY < FFileContent.Count then
        lString := Copy(FFileContent[i + FBreakShiftY], FBreakShiftX + 1, lSampleWidth)
      else
        lString := '';
      TextOut(lRect.Left, lRect.Top + i * lCharHeight, lString);
    end;

    {draw the ruler}
    Brush.Color := Color;
    MoveTo(lRect.Left, lRect.Top - 8);
    LineTo(lRect.Right, lRect.Top - 8);
    for i := 0 to lSampleWidth - 1 do
    begin
      j := i * lCharWidth;
      k := i + FBreakShiftX;
      if k mod 5 = 0 then
      begin
        MoveTo(j, lRect.Top - 13);
        LineTo(j, lRect.Top - 8);

        if (k <> 0) and (k mod 10 = 0) then begin
          lString := IntToStr(k);
          k := TextWidth(lString);
          TextOut(j - k div 2, lRect.Top - lCharHeight - 15, lString);
        end
      end else begin
        MoveTo(j, lRect.Top - 10);
        LineTo(j, lRect.Top - 8);
      end
    end;

    {draw the arrows for breaks}
    for i := 0 to FBreaks.Count - 1 do
    begin
      lBreak := LongInt(FBreaks[i]);
      if (FBreakShiftX <= lBreak) and
         (lBreak <= FBreakShiftX + lSampleWidth) then
      begin
        j := ((lBreak - FBreakShiftX) mod lSampleWidth) * lCharWidth;
        if FMouseDown and (i = FInDownMouse) then
          Pen.Style := psDot
        else
          Pen.Style := psSolid;

        {arrow above}
        Brush.Color := clRed;
        Pen.Color := clRed;
        Polygon([Point(j, lRect.Top - 7),
                 Point(j - 3, lRect.Top - 3),
                 Point(j + 3, lRect.Top - 3)]);
        {line}
        MoveTo(j, lRect.Top - 3);
        LineTo(j, lRect.Bottom);
  
        Pen.Style := psSolid;
      end;
    end;
  end;
end;  // TfraFixedWidths.pbBreaksPaint 

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.SaveContent;
begin
  inherited;
  FFileContent.Free;
  //FBreaks references Settings.FixedWidthBreaks which is freed by settings
end;  // TfraFixedWidths.SaveContent

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.ImportData;
var
  lCursor: TCursor;
  lMappings: TStringList;
begin
  if ValidatePage then begin
    //free existing dataset if existing in settings object
    Settings.ResetImportedData;
    lCursor := HourglassCursor;
    try
      FLastFieldLength := DEFAULT_FIELD_SIZE;
      FReloadRequired  := False;
      FReloading       := False;

      FBreaks.Sort(@CompareIntegers);

      try
        CreateDataStructure;

        FSMTxtImport := TSMImportFromText.Create(nil);
        lMappings := nil;
        try
          with FSMTxtImport do begin
            AnimatedStatus := false;
            DataSet := Settings.ImportedData;
            FieldDelimiter := fdNone;
            Fixed := true;
            lMappings := CreateFieldMappings;
            Mappings := lMappings;
            Options := [soExtendedStatistic, soSkipEmptyRow];
            RecordSeparatorCustom := Settings.TextRecordSeparator;
            RowFieldNames := 1;
            if Settings.ImportOnlySpecifiedRows then begin
              RowFirst := Settings.ImportRowsFrom;
              RowLast := Settings.ImportRowsTo;
            end;
            SourceFileName := Settings.SourceDataFile;
            TextQualifier := tqNone;
            TrimSpaces := tsTrimRight;
            OnErrorEvent := SMImportErrorEvent;
            OnAfterRecordEvent := SMImportAfterRecordEvent;
            OnGetCellParams := SMImportGetCellParams;

            Execute;
            if Settings.ImportedData.Active then begin
              //check if we need to reload because of long fields
              if FReloadRequired then begin
                FReloading := True;
                // Before loading again, clear the previous lot, or we get multiple copies.
                dmDatabase.ExecuteSql(
                    'DELETE FROM ' + Settings.ImportedData.TableName);
                Execute;
              end;
            end else begin
              //import did not work, settings probably wrong - tell user
              //and don't go anywhere!
              Settings.ResetImportedData;
              ContainerForm.ValidateValue(False, ResStr_FailedToImportData);
            end;
          end;
        finally
          FSMTxtImport.Free;
          if Assigned(lMappings) then
            lMappings.Free;
        end;
      except
        on Exception do begin
          Settings.ResetImportedData;
          raise;
        end;
      end;
    finally
      DefaultCursor(lCursor);
      frmMain.SetStatus('');
      frmMain.SetProgress(0);
    end;
  end else
    ContainerForm.ValidateValue(false, ResStr_ValidationFailed);
end;     

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.CreateDataStructure;
var
  I, lPos: integer;
  lSql: String;
begin
  Settings.ImportedData.Close;

  lSql := 'CREATE TABLE ' + Settings.ImportedData.TableName + '(';
        
  lPos := 1;
  for I := 0 to FBreaks.Count - 1 do 
  begin
    lSql := lSql + FIELD_NAME_PREFIX + IntToStr(I)
        + ' VARCHAR(' + IntToStr(Integer(FBreaks[i])) + ') NULL, ';
    Settings.FirstRow.Add(
        FIELD_NAME_PREFIX + IntToStr(i) + '='
        + Copy(FFileContent[0], lPos, Integer(FBreaks[I]) - lPos + 1));
    lPos := Integer(FBreaks[I]) + 1;
  end;
  //final field does not have a break specified for it so we do not know how
  //long it is
  FLastFieldIndex := FBreaks.Count;
  lSql := lSql + FIELD_NAME_PREFIX + IntToStr(FLastFieldIndex)
      + ' VARCHAR(' + IntToStr(FLastFieldLength) + ') NULL, ';
  Settings.FirstRow.Add(FIELD_NAME_PREFIX + IntToStr(FLastFieldIndex) + '=' +
                        Copy(FFileContent[0], lPos, Length(FFileContent[0])));

  lSql := lSql + '"' + FLD_ROWID + '" INT IDENTITY PRIMARY KEY)';
  dmDatabase.ExecuteSql(lSql);
  Settings.ImportedData.Open;
end;  // TfraFixedWidths.CreateDataStructure;

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.sbBreakHorzScroll(Sender: TObject; ScrollCode: TScrollCode; var
    ScrollPos: Integer);
var
  i, lCharWidth, lSampleWidth: Integer;
  R: TRect;
begin
  with pbBreaks do
  begin
    R := ClientRect;
    R.Top := R.Top + 20;
    lCharWidth := Canvas.TextWidth('W');
  end;
  
  lSampleWidth := (R.Right-R.Left) div lCharWidth;
  
  case ScrollCode of
    scLineUp:   i := FBreakShiftX - 1;
    scLineDown: i := FBreakShiftX + 1;
    scPageUp:   i := FBreakShiftX - lSampleWidth;
    scPageDown: i := FBreakShiftX + lSampleWidth;
    scTop:      i := 0;
    scBottom:   i := sbBreakHorz.Max;
    scPosition,
    scEndScroll,
    scTrack:    i := sbBreakHorz.Min + ScrollPos;
  else
    i := FBreakShiftX;
  end;

  if (i < 0) then
    i := 0;
  if (i > sbBreakHorz.Max) then
    i := sbBreakHorz.Max;
  if (i <> FBreakShiftX) then
  begin
    sbBreakHorz.Position := i;
    FBreakShiftX := i;
    pbBreaks.Repaint;
  end;
end;  // TfraFixedWidths.sbBreakHorzScroll 

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.sbBreakVertScroll(Sender: TObject; ScrollCode: TScrollCode; var 
    ScrollPos: Integer);
var
  i, lCharHeight, lSampleHeight: Integer;
  R: TRect;
begin
  with pbBreaks do
  begin
    R := ClientRect;
    R.Top := R.Top + 20;
    lCharHeight := Canvas.TextHeight('Wg');
    InflateRect(R, -1, -1);
  end;
  
  lSampleHeight := (R.Bottom - R.Top) div lCharHeight;
  
  sbBreakVert.Max := FFileContent.Count;
  case ScrollCode of
    scLineUp:   i := FBreakShiftY - 1;
    scLineDown: i := FBreakShiftY + 1;
    scPageUp:   i := FBreakShiftY - lSampleHeight;
    scPageDown: i := FBreakShiftY + lSampleHeight;
    scTop:      i := 0;
    scBottom:   i := sbBreakVert.Max;
    scPosition,
    scEndScroll,
    scTrack:    i := sbBreakVert.Min + ScrollPos;
  else
    i := FBreakShiftY;
  end;
  
  if (i < 0) then
    i := 0;
  if (i > sbBreakVert.Max) then
    i := sbBreakVert.Max;
  if (i <> FBreakShiftY) then
  begin
    sbBreakVert.Position := i;
    FBreakShiftY := i;
    pbBreaks.Repaint;
  end;
end;  // TfraFixedWidths.sbBreakVertScroll

{-------------------------------------------------------------------------------
}
function TfraFixedWidths.ValidatePage: boolean;
var
  lErrors: TStringList;
begin
  lErrors := TStringList.Create;
  try
    if FBreaks.Count > 0 then
      Result := true
    else begin
      lErrors.Add(ResStr_NoBreaksAdded); 
      Result := false;
    end;
    ChangedHTML(lErrors);
  finally
    lErrors.Free;
  end;
end;  // TfraFixedWidths.ValidatePage

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.ValidateContent;
begin
  ImportData;
end;  // TfraFixedWidths.ValidateContent

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.SMImportAfterRecordEvent(Sender: TObject;
  var Abort: Boolean);
begin
  //if we are reloading then we have done one load (=50%) + what we are up to
  if FReloading then
    frmMain.SetProgress(50 + (Round(FSMTxtImport.Statistic.TotalAdded /
                                    FSMTxtImport.Statistic.TotalCount * 50)))
  //if we need to reload then we can on go to 50% on this load
  else if FReloadRequired then
    frmMain.SetProgress(Round(FSMTxtImport.Statistic.TotalAdded /
                              FSMTxtImport.Statistic.TotalCount * 50))
  else
    frmMain.SetProgress(Round(FSMTxtImport.Statistic.TotalAdded /
                              FSMTxtImport.Statistic.TotalCount * 100));;
end;  // TfraFixedWidths.SMImportAfterRecordEvent

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.SMImportErrorEvent(Sender: TObject;
  Error: Exception; var Abort: Boolean);
begin
  Abort := true;
  frmMain.SetStatus('');
  frmMain.SetProgress(0);
  raise Error;
end;  // TfraFixedWidths.SMImportErrorEvent

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.SMImportGetCellParams(Sender: TObject; Field: TField;
  var Value: Variant);
begin
  if not FReloading and Assigned(Field) then
    if Field.Index = FLastFieldIndex then
      if Length(VarToStr(Value)) > FLastFieldLength then begin
        FLastFieldLength := Length(VarToStr(Value));
        FReloadRequired := true;
      end;
end;  // TfraFixedWidths.SMImportGetCellParams

{-------------------------------------------------------------------------------
}
function TfraFixedWidths.CreateFieldMappings: TStringList;
var
  i, lStart, lEnd: integer;
  lMappings: TStringList;
begin
  lMappings := TStringList.Create;
  Result := lMappings;

  lStart := 1;
  lEnd := 0;
  for i := 0 to FBreaks.Count - 1 do begin
    lEnd := integer (FBreaks[i]);
    //add mappings in required format - [FieldName]=Text[FromIndex]-[ToIndex]
    lMappings.Add(FIELD_NAME_PREFIX + IntToStr(i) + '=Text' + IntToStr(lStart) + '-' +
      IntToStr(lEnd));
    lStart := lEnd + 1;
  end;
  //add last field which is not specified by a break
  lEnd := lEnd + LAST_IMPORT_FIELD_WIDTH;
  lMappings.Add(FIELD_NAME_PREFIX + IntToStr(FBreaks.Count) + '=Text' +
    IntToStr(lStart) + '-' + IntToStr(lEnd));
end;  // TfraFixedWidths.CreateFieldMappings

{-------------------------------------------------------------------------------
}
procedure TfraFixedWidths.pbBreaksClick(Sender: TObject);
begin
  inherited;
  ChangedContent;
end;  // TfraFixedWidths.pbBreaksClick

{-------------------------------------------------------------------------------
}
function TfraFixedWidths.GetHtmlImageName: String;
begin
  Result := 'Settings.jpg';
end;

{-------------------------------------------------------------------------------
  Constructor
}
constructor TfraFixedWidths.Create(AOwner: TComponent; ASettings:
    TdmIWSettings);
begin
  inherited;
  HelpContext := IDH_IWFIXEDWIDTHS;
end;

end.
