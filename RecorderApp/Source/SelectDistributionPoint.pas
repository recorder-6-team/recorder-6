//==============================================================================
//  Unit:        SelectDistributionPoint
//
//  Implements:  TdlgSelectDistributionPoint
//               TPointOnMap
//
//  Description:
//
//  Author:      John van Breda
//  Created:     21 March 2001
//
//  Last Revision Details:
//    $Revision: 21 $
//    $Date: 26/03/08 18:05 $
//    $Author: Ericsalmon $
//
//  $History: SelectDistributionPoint.pas $
//  
//  *****************  Version 21  *****************
//  User: Ericsalmon   Date: 26/03/08   Time: 18:05
//  Updated in $/JNCC/Development/Build/Source
//  VI16723. Fixed hardcoded "Internal Map" string problem. Also changed
//  hardcoded gird column titles to individual resource strings.
//  
//  *****************  Version 20  *****************
//  User: Ericsalmon   Date: 20/02/08   Time: 16:21
//  Updated in $/JNCC/Development/Build/Source
//  
//  *****************  Version 19  *****************
//  User: Ericsalmon   Date: 22/03/07   Time: 16:17
//  Updated in $/JNCC/Development/Build/Source
//  VI 13237. Spatial_Ref value stored in DB in english format. Displayed
//  using current locale.
//  
//  *****************  Version 18  *****************
//  User: Ericsalmon   Date: 28/01/04   Time: 18:39
//  Updated in $/JNCC/Development/Build/Source
//  Development.
//  
//  *****************  Version 17  *****************
//  User: Johnvanbreda Date: 26/06/03   Time: 9:18
//  Updated in $/JNCC/Development/Build/Source
//  IR676
//  Drag onto map fixed, by controlling ParseSQL flag on qryAllPurpose
//  properly.  Put in a lot of try..finally blocks to ensure the flag is
//  true by default at all times.
//  
//  *****************  Version 16  *****************
//  User: Ericsalmon   Date: 23/05/03   Time: 15:44
//  Updated in $/JNCC/Development/Build/Source
//  Allowed vertical resizing o the form.
//  
//  *****************  Version 15  *****************
//  User: Pollyshaw    Date: 17/02/03   Time: 11:41
//  Updated in $/JNCC/Source
//  Made syntax SQL Server compliant
//  
//  *****************  Version 14  *****************
//  User: Pollyshaw    Date: 22/01/03   Time: 14:18
//  Updated in $/JNCC/Source
//  IR 289:
//  Fixed Drawing
//  
//  *****************  Version 13  *****************
//  User: Ericsalmon   Date: 21/01/03   Time: 16:36
//  Updated in $/JNCC/Source
//  IR 338 fixed.
//  
//==============================================================================

unit SelectDistributionPoint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DataClasses, ExtCtrls, ImageListButton, SpatialRefFuncs, Grids;

type
  TPointOnMap=class(TObject)
  private
    FDataTable: String;
    FKey: TKeyString;
  public
    constructor Create(iTable:String; iKey:TKeyString);
    property DataTable : string read FDataTable;
    property Key : TKeyString read FKey;
  end;

  TdlgSelectDistributionPoint = class(TForm)
    lblPleaseSelect: TLabel;
    Bevel1: TBevel;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    sgPoints: TStringGrid;
    procedure sgPointsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgPointsDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FData:TList;
    procedure PopulateStringGrid;
    procedure SetStringGridColumns;
    procedure ReadSampleData(iiListIndex: integer);
    procedure ReadEventData(iiListIndex: integer);
    procedure ReadTaxonOccurrenceData(iiListIndex: integer);
    procedure ReadBiotopeOccurrenceData(iiListIndex: integer);
  public
    { Public declarations }
    procedure PopulateGrid(iData:TList);
  end;

var
  dlgSelectDistributionPoint: TdlgSelectDistributionPoint;

//==============================================================================
implementation

uses
  FormActions, GeneralFunctions, GeneralData, ApplicationSettings, Constants;

{$R *.DFM}

//==============================================================================
{ Public method that populates the list box using a list of TPointOnMaps }
procedure TdlgSelectDistributionPoint.PopulateGrid(iData: TList);
begin
  FData:=iData;
  SetStringGridColumns;
  PopulateStringGrid;
end;


{ Description : Initialises the column titles and widths
  Created : 06/11/2002 }
procedure TdlgSelectDistributionPoint.SetStringGridColumns;
begin
  SetGridColumnTitles(
      sgPoints,
      [ResStr_PointType, ResStr_Survey, ResStr_Date,
       ResStr_Locality, ResStr_SampleType, ResStr_OccurrenceOf]);
end;


{ Description : Populates data into the string grid.
  Created : 06/11/2002 }
procedure TdlgSelectDistributionPoint.PopulateStringGrid;
var
  liListIndex : integer;
begin
  sgPoints.RowCount := FData.Count + 1;
  for liListIndex:=0 to FData.Count-1 do begin
    sgPoints.Cells[0,liLIstIndex+1] := ReadableFormat(
                               TPointOnMap(FData.Items[liListIndex]).DataTable);
    if SameText(TPointOnMap(FData.Items[liListIndex]).DataTable, TN_SAMPLE) then
      ReadSampleData(liListIndex)
    else
    if SameText(TPointOnMap(FData.Items[liListIndex]).DataTable, TN_SURVEY_EVENT) then
      ReadEventData(liListIndex)
    else
    if SameText(TPointOnMap(FData.Items[liListIndex]).DataTable, TN_TAXON_OCCURRENCE) then
      ReadTaxonOccurrenceData(liListIndex)
    else
    if SameText(TPointOnMap(FData.Items[liListIndex]).DataTable, TN_BIOTOPE_OCCURRENCE) then
      ReadBiotopeOccurrenceData(liListIndex);
  end; // for
end;


{ Description : Loads a sample (identified by iiListIndex) into a row of the
     string grid
  Created : 06/11/2002 }
procedure TdlgSelectDistributionPoint.ReadSampleData(iiListIndex : integer);
const
  SQL_SAMPLE_POINT_DETAIL = 'SELECT S.ITEM_NAME AS SurveyName, SM.VAGUE_DATE_START, ' +
           'SM.VAGUE_DATE_END, SM.VAGUE_DATE_TYPE, LN.ITEM_NAME as LocName, ' +
           'SM.SPATIAL_REF, SM.SPATIAL_REF_SYSTEM, SM.LAT, SM.LONG, ST.SHORT_NAME As SampleType ' +
           'FROM (((SAMPLE AS SM ' +
           'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY=SM.SURVEY_EVENT_KEY) ' +
           'INNER JOIN SURVEY AS S ON S.SURVEY_KEY=SE.SURVEY_KEY) ' +
           'LEFT JOIN SAMPLE_TYPE AS ST ON ST.SAMPLE_TYPE_KEY=SM.SAMPLE_TYPE_KEY) ' +
           'LEFT JOIN LOCATION_NAME AS LN ON LN.LOCATION_KEY = SM.LOCATION_KEY ' +
           'WHERE SM.SAMPLE_KEY=''%S''';
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := Format(SQL_SAMPLE_POINT_DETAIL,
                [TPointOnMap(FData.Items[iiListIndex]).Key]);
    Open;
    try
      sgPoints.Cells[1,iiListIndex+1] := FieldByName('SurveyName').AsString;
      sgPoints.Cells[2,iiListIndex+1] := FieldByName('VAGUE_DATE_START').Text;
      sgPoints.Cells[3,iiListIndex+1] := FieldByName('LocName').AsString + ' (' +
                                         LocaliseSpatialRef(FieldByName('Spatial_Ref').Text) + ')';
      sgPoints.Cells[4,iiListIndex+1] := FieldByName('SampleType').AsString;
    finally
      Close;
    end;
  end;
end;


{ Description : Loads an event (identified by iiListIndex) into a row of the
     string grid
  Created : 06/11/2002 }
procedure TdlgSelectDistributionPoint.ReadEventData(iiListIndex : integer);
const
  SQL_SAMPLE_POINT_DETAIL = 'SELECT S.ITEM_NAME AS SurveyName, SE.VAGUE_DATE_START, ' +
           'SE.VAGUE_DATE_END, SE.VAGUE_DATE_TYPE, LN.ITEM_NAME as LocName, ' +
           'SE.SPATIAL_REF, SE.SPATIAL_REF_SYSTEM, SE.LAT, SE.LONG ' +
           'FROM (SURVEY_EVENT AS SE ' +
           'INNER JOIN SURVEY AS S ON S.SURVEY_KEY=SE.SURVEY_KEY) ' +
           'LEFT JOIN LOCATION_NAME AS LN ON LN.LOCATION_KEY = SE.LOCATION_KEY ' +
           'WHERE SE.SURVEY_EVENT_KEY=''%S''';
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := Format(SQL_SAMPLE_POINT_DETAIL,
                [TPointOnMap(FData.Items[iiListIndex]).Key]);
    Open;
    try
      sgPoints.Cells[1,iiListIndex+1] := FieldByName('SurveyName').AsString;
      sgPoints.Cells[2,iiListIndex+1] := FieldByName('VAGUE_DATE_START').Text;
      sgPoints.Cells[3,iiListIndex+1] := FieldByName('LocName').AsString + ' (' +
                                         LocaliseSpatialRef(FieldByName('Spatial_Ref').Text) + ')';
    finally
      Close;
    end;
  end;
end;


{ Description : Loads a taxon occ (identified by iiListIndex) into a row of the
     string grid
  Created : 06/11/2002 }
procedure TdlgSelectDistributionPoint.ReadTaxonOccurrenceData(iiListIndex : integer);
const
  SQL_TXOCC_POINT_DETAIL = 'SELECT S.ITEM_NAME AS SurveyName, SM.VAGUE_DATE_START, ' +
           'SM.VAGUE_DATE_END, SM.VAGUE_DATE_TYPE, LN.ITEM_NAME as LocName, ' +
           'SM.SPATIAL_REF, SM.SPATIAL_REF_SYSTEM, SM.LAT, SM.LONG, ' +
           'TD.TAXON_LIST_ITEM_KEY, ST.SHORT_NAME As SampleType ' +
           'FROM (((((SAMPLE AS SM ' +
           'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY=SM.SURVEY_EVENT_KEY) ' +
           'INNER JOIN SURVEY AS S ON S.SURVEY_KEY=SE.SURVEY_KEY) ' +
           'INNER JOIN TAXON_OCCURRENCE AS TXO ON TXO.SAMPLE_KEY=SM.SAMPLE_KEY) ' +
           'INNER JOIN TAXON_DETERMINATION AS TD ON TD.TAXON_OCCURRENCE_KEY=TXO.TAXON_OCCURRENCE_KEY) ' +
           'LEFT JOIN SAMPLE_TYPE AS ST ON ST.SAMPLE_TYPE_KEY=SM.SAMPLE_TYPE_KEY) ' +
           'LEFT JOIN LOCATION_NAME AS LN ON LN.LOCATION_KEY = SM.LOCATION_KEY ' +
           'WHERE TXO.TAXON_OCCURRENCE_KEY=''%S'' ' +
           'AND TD.PREFERRED=1';
begin
  with dmGeneralData.qryAllPurpose do begin
    SQL.Text := Format(SQL_TXOCC_POINT_DETAIL,
                [TPointOnMap(FData.Items[iiListIndex]).Key]);
    Open;
    try
      sgPoints.Cells[1,iiListIndex+1] := FieldByName('SurveyName').AsString;
      sgPoints.Cells[2,iiListIndex+1] := FieldByName('VAGUE_DATE_START').Text;
      sgPoints.Cells[3,iiListIndex+1] := FieldByName('LocName').AsString + ' (' +
                                         LocaliseSpatialRef(FieldByName('Spatial_Ref').Text) + ')';
      sgPoints.Cells[4,iiListIndex+1] := FieldByName('SampleType').AsString;
      // use list item to get taxon name, and store as object
      sgPoints.Objects[5,iiListIndex+1] := dmGeneralData.GetTaxonNamesObject(
                                        FieldByName('TAXON_LIST_ITEM_KEY').AsString);
    finally
      Close;
    end;
  end;
end;


{ Description : Loads a biotope occ (identified by iiListIndex) into a row of the
     string grid
  Created : 06/11/2002 }
procedure TdlgSelectDistributionPoint.ReadBiotopeOccurrenceData(iiListIndex : integer);
const
  SQL_TXOCC_POINT_DETAIL = 'SELECT S.ITEM_NAME AS SurveyName, SM.VAGUE_DATE_START, ' +
           'SM.VAGUE_DATE_END, SM.VAGUE_DATE_TYPE, LN.ITEM_NAME as LocName, ' +
           'SM.SPATIAL_REF, SM.SPATIAL_REF_SYSTEM, SM.LAT, SM.LONG, ' +
           'BD.BIOTOPE_LIST_ITEM_KEY, ST.SHORT_NAME As SampleType ' +
           'FROM (((((SAMPLE AS SM ' +
           'INNER JOIN SURVEY_EVENT AS SE ON SE.SURVEY_EVENT_KEY=SM.SURVEY_EVENT_KEY) ' +
           'INNER JOIN SURVEY AS S ON S.SURVEY_KEY=SE.SURVEY_KEY) ' +
           'INNER JOIN BIOTOPE_OCCURRENCE AS BO ON BO.SAMPLE_KEY=SM.SAMPLE_KEY) ' +
           'INNER JOIN BIOTOPE_DETERMINATION AS BD ON BD.BIOTOPE_OCCURRENCE_KEY=BO.BIOTOPE_OCCURRENCE_KEY) ' +
           'LEFT JOIN SAMPLE_TYPE AS ST ON ST.SAMPLE_TYPE_KEY=SM.SAMPLE_TYPE_KEY) ' +
           'LEFT JOIN LOCATION_NAME AS LN ON LN.LOCATION_KEY = SM.LOCATION_KEY ' +
           'WHERE BO.BIOTOPE_OCCURRENCE_KEY=''%S'' ' +
           'AND BD.PREFERRED=1';
begin
  with dmGeneralData.qryAllPurpose do begin
    ParseSQL := false;
    try
      SQL.Text := Format(SQL_TXOCC_POINT_DETAIL,
                  [TPointOnMap(FData.Items[iiListIndex]).Key]);
      Open;
      try
        sgPoints.Cells[1,iiListIndex+1] := FieldByName('SurveyName').AsString;
        sgPoints.Cells[2,iiListIndex+1] := FieldByName('VAGUE_DATE_START').Text;
        sgPoints.Cells[3,iiListIndex+1] := FieldByName('LocName').AsString + ' (' +
                                           LocaliseSpatialRef(FieldByName('Spatial_Ref').Text) + ')';
        sgPoints.Cells[4,iiListIndex+1] := FieldByName('SampleType').AsString;
        // use list item to get biotope name as text
        sgPoints.Cells[5,iiListIndex+1] := dmGeneralData.GetBiotopeCodeName(
                                          FieldByName('BIOTOPE_LIST_ITEM_KEY').AsString);
      finally
        Close;
      end;
    finally
      ParseSQL := True;
    end;
  end;
end;              

{ Description : DrawCell handler.  Facilitates multiline and italic text
  Created : 06/11/2002 }
procedure TdlgSelectDistributionPoint.sgPointsDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  lstOutputText: String;
  lDrawrect :TRect;
  liTextHeight : integer; // RowHeight required for the text
  liPrefNAmeTop : integer;
  lTaxonNames : TTaxonNames;

    // retrieve the text into lstOutputText, plus the taxon names object if required
    procedure GetTextToOutput;
    begin
      // if a taxon name object for the cell, use it to obtain text
      if Assigned(TStringGrid(Sender).Objects[ACol, ARow]) then begin
        lTaxonNames := TTaxonNames(TStringGrid(Sender).Objects[ACol, ARow]);
        if AppSettings.DisplayTaxonCommonNames then begin
          lstOutputText:= lTaxonNames.CommonName;
          if lTaxonNames.CNItalic then
            (Sender As TStringgrid).Canvas.Font.Style := [fsItalic];
        end else begin
          lstOutputText:= lTaxonNames.TaxonName;
          if lTaxonNames.TNItalic then
            (Sender As TStringgrid).Canvas.Font.Style := [fsItalic];
        end
      end else begin
        // standard text in the cell
        lstOutputText:= (Sender As TStringgrid).Cells[ ACol, ARow ];
        lTaxonNames := nil;
      end;
    end;

    // find the height of the row required to display the text
    procedure MeasureTextHeight;
    begin
      // adjust draw rect to where we want our text to begin
      lDrawRect.Top := Rect.Top + 2;
      lDrawRect.Left := Rect.Left + 2;
      lDrawRect.Right := Rect.Right -2;
      lDrawRect.Bottom := lDrawRect.Top;
      // measure the text - this doesn't draw it
      DrawText((Sender As TStringgrid).canvas.handle,
              PChar(lstOutputText), Length(lstOutputText), lDrawrect,
              dt_calcrect or dt_wordbreak or dt_left );
      liTextHeight := (lDrawrect.bottom - lDrawrect.top) + 2;
      // Allow extra space for latin name if it is going to be displayed
      if Assigned(lTaxonNames) and AppSettings.DisplayTaxonCommonNames then begin
        if lTaxonNames.CommonName <> lTaxonNames.TaxonName then begin
          // when working out extra space for preferred name, set correct font style
          if lTaxonNames.TNItalic then
            (Sender As TStringgrid).Canvas.Font.Style := [fsItalic]
          else
            (Sender As TStringgrid).Canvas.Font.Style := [];
          // previous bottom of rect is where latin name should start
          liPrefNameTop := Rect.Top + liTextHeight;
          lDrawRect.Right := Rect.Right -2;
          lDrawRect.Bottom := lDrawRect.Top;

          DrawText((Sender As TStringgrid).canvas.handle,
              PChar('(' + lTaxonNames.TaxonName + ')'), Length(lTaxonNames.TaxonName)+2, lDrawrect,
              dt_calcrect or dt_wordbreak or dt_left );
          liTextHeight := (lDrawrect.bottom - lDrawrect.top) + 2 + liTextHeight;
          // and set font back again
          if lTaxonNames.CNItalic then
            (Sender As TStringgrid).Canvas.Font.Style := [fsItalic]
          else
            (Sender As TStringgrid).Canvas.Font.Style := [];
        end;
      end;
    end;

begin
  (Sender As TStringgrid).Canvas.Font.Style := [];
  GetTextToOutput;
  If Length(lstOutputText) > 0 then begin
    MeasureTextHeight;
    // adjust rowheight if required - if so the grid gets invalidated so don't bother drawing
    if liTextHeight > (Sender As TStringgrid).RowHeights[ARow] Then
      (Sender As TStringgrid).RowHeights[ARow] := liTextHeight
    else begin
      // actually draw the text
      (Sender As TStringgrid).canvas.fillrect(rect);
      lDrawRect.Top := Rect.Top + 2;
      lDrawRect.Left := Rect.Left + 2;
      lDrawRect.Right := Rect.Right - 2;
      lDrawRect.Bottom := Rect.Top + liTextHeight;
      DrawText((Sender As TStringgrid).canvas.handle,
                Pchar(lstOutputText), Length(lstOutputText), lDrawrect,
                dt_wordbreak or dt_left);
      if Assigned(lTaxonNames) and AppSettings.DisplayTaxonCommonNames then begin
        if lTaxonNames.CommonName <> lTaxonNames.TaxonName then begin
          // draw preferred name underneath in brackets
          lDrawRect.Top := liPrefNameTop;
          lDrawRect.Right := Rect.Right -2;
          lDrawRect.Bottom := liTextHeight + lDrawRect.Top;
          if lTaxonNames.TNItalic then
            (Sender As TStringgrid).Canvas.Font.Style := [fsItalic]
          else
            (Sender As TStringgrid).Canvas.Font.Style := [];
          DrawText((Sender As TStringgrid).canvas.handle,
            PChar('(' + lTaxonNames.TaxonName + ')'), Length(lTaxonNames.TaxonName)+2, lDrawrect,
            dt_wordbreak or dt_left );
        end;
      end;
    end;
  end;
end;

{ TPointOnMap }

{ Constructor stores the relevant bits of data }
constructor TPointOnMap.Create(iTable: String; iKey: TKeyString);
begin
  FDataTable:=iTable;
  FKey:=iKey;
end;

{ Description : double click the grid opens the item
  Created : 06/11/2002 }
procedure TdlgSelectDistributionPoint.sgPointsDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TdlgSelectDistributionPoint.FormResize(Sender: TObject);
var
  lMin, lMax: Integer;
begin
  with sgPoints do begin
    ColWidths[5] := 101;

    // Check the horizontal scrollbar is visible, or not.
    // That would be because column width is too big.
    GetScrollRange(sgPoints.Handle, SB_HORZ, lMin, lMax);
    // Adjust width of Title column accordingly so that it disappears.
    if lMin <> lMax then
      ColWidths[5] := 101 - GetSystemMetrics(SM_CYHSCROLL);
  end;
end;

end.
