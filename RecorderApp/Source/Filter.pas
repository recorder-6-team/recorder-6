//==============================================================================
//  Unit:        Filter
//
//  Implements:  TdlgFilter
//               TFilter
//               TReportField
//
//  Description: Allows user to apply one simple filter criteria to record sets.
//
//               TFilter and TReportField
//               Helper classes.
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon 07/02/2002
//               Datasets properties (DatabaseName, SessionName, Connection) set
//               via dmDatabase.
//
//  Last Revision Details:
//    $Revision: 39 $
//    $Date: 17/09/08 17:18 $
//    $Author: Qingsun $
//
//  $History: Filter.pas $
//  
//  *****************  Version 39  *****************
//  User: Qingsun      Date: 17/09/08   Time: 17:18
//  Updated in $/JNCC/Development/Build/Source
//  
//  *****************  Version 38  *****************
//  User: Davidkelly   Date: 13/12/07   Time: 8:07
//  Updated in $/JNCC/Development/Build/Source
//  Added a missing semicolon.
//  
//  *****************  Version 37  *****************
//  User: Rickyshrestha Date: 12/12/07   Time: 16:58
//  Updated in $/JNCC/Development/Build/Source
//  Changes some hardcoded strings to resourcestring
//  ResStr_NoSpatialReference 
//    ResStr_CriterionMissing 
//    ResStr_SecondCriterionMissing 
//    ResStr_ValidYesNo 
//    ResStr_InvalidVagueDate 
//    ResStr_InvalidReference 
//    ResStr_EqualDates ;
//    ResStr_ContainFirstDate
//    ResStr_SecondBeforeFirstDate 
//  
//  *****************  Version 36  *****************
//  User: Ericsalmon   Date: 22/03/07   Time: 16:17
//  Updated in $/JNCC/Development/Build/Source
//  VI 13237. Spatial_Ref value stored in DB in english format. Displayed
//  using current locale.
//  
//  *****************  Version 35  *****************
//  User: Johnvanbreda Date: 5/09/06    Time: 16:21
//  Updated in $/JNCC/Development/Build/Source
//  IR 12523
//  Fixed filters against text fields
//  
//  *****************  Version 34  *****************
//  User: Ericsalmon   Date: 8/12/05    Time: 10:41
//  Updated in $/JNCC/Development/Build/Source
//  CCN129. Remember applied filters.
//  
//  *****************  Version 33  *****************
//  User: Ericsalmon   Date: 26/08/04   Time: 12:01
//  Updated in $/JNCC/Development/Build/Source
//  Forced nil on freed objects, trying to prevent AVs when closing app
//  while forms are still loading.
//  
//  *****************  Version 32  *****************
//  User: Pollyshaw    Date: 27/01/03   Time: 10:33
//  Updated in $/JNCC/Source
//  IR 362 : Changed Grid Square filter: previously 'S' was being left.
//  
//  *****************  Version 31  *****************
//  User: Pollyshaw    Date: 16/01/03   Time: 15:52
//  Updated in $/JNCC/Source
//  IR 280: fixed filtering on vague dates.
//  
//  *****************  Version 30  *****************
//  User: Ericsalmon   Date: 27/12/02   Time: 15:00
//  Updated in $/JNCC/Source
//  
//  *****************  Version 29  *****************
//  User: Johnvanbreda Date: 20/12/02   Time: 10:02
//  Updated in $/JNCC/Source
//  IR 22
//  Fixed locaiton grid square filters
//  
//  *****************  Version 28  *****************
//  User: Johnvanbreda Date: 5/12/02    Time: 16:24
//  Updated in $/JNCC/Source
//  Interim checkin
//  
//  *****************  Version 27  *****************
//  User: Ericsalmon   Date: 4/12/02    Time: 16:01
//  Updated in $/JNCC/Source
//  Removed USE_TITAN compiler directive and D4-specific code.
//  
//  *****************  Version 26  *****************
//  User: Michaelbailey Date: 19/11/02   Time: 10:16
//  Updated in $/JNCC/Source
//  Incident Report : IR EHS6 - 1    Message box to indicate when the
//  spatial reference is not part of the current system.
//  
//  *****************  Version 25  *****************
//  User: Michaelbailey Date: 18/11/02   Time: 14:52
//  Updated in $/JNCC/Source
//  CCN84   Revised functionality
//  
//  *****************  Version 24  *****************
//  User: Michaelbailey Date: 11/11/02   Time: 14:27
//  Updated in $/JNCC/Source
//  CCN No: EHS6  -  New Location Filter implemented.  Filters all
//  locations centred within a specifiable OS grid square.
//  
//  *****************  Version 23  *****************
//  User: Ericsalmon   Date: 20/06/02   Time: 10:44
//  Updated in $/JNCC/Source
//  Replaced BitBtns with ImageListButtons
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit Filter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  ExtCtrls, StdCtrls, Buttons, DataClasses, JNCCDatasets, VagueDate, OnlineHelp,
  GeneralData, Constants, ImageListButton, ADODB, DatabaseAccessADO;

type
  TCondition = (cdEqual, cdNotEqual, cdContains, cdBetween,
                cdGreater, cdLessThan, cdStarts, cdInSquare);
  TUnion = (uNotSet, uInclude, uOmit, uAlways);

  {-----------------------------------------------------------------------------
   Record holding the essential info required to rebuild a filter.
  }
  TFilterCriteria = packed record
    TableName         : String;
    FilteredFieldKey  : String;
    Condition         : String;
    Criteria1         : String;
    Criteria2         : String;
    AdditionalCriteria: String;
    IsSet             : Boolean;
  end;

  //----------------------------------------------------------------------------
  TBetweenCriteria = record
    Criteria1 : string;
    Criteria2 : string;
  end;

  //----------------------------------------------------------------------------
  TReportField = class(TObject)
  private
    FItemKey    :TKeyString;
    FFilter     :boolean;
    FSelectable :boolean;
    FSQL        :string;
    FDisplayName:string;
    FFieldName  :string;
    FSelected   :boolean;
    FTableName  :string;
    FApplyTo    :Char;
    FFieldType  :string;
    FUnion      :TUnion;
    procedure SetItemKey(const Value: TKeyString);
    function GetTextFilter(ACondition: TCondition; const ACriteria: String): String;
    function GetBooleanFilter(ACondition: TCondition; const ACriteria: String): String;
    function GetNumberFilter(ACondition: TCondition; const ACriteria1, ACriteria2: String): String;
    function GetVagueDateFilter(ACondition: TCondition; const ACriteria1, ACriteria2: String): String;
    function GetGridSquareFilter(const ACriteria: String): String;
    function GetSpatialRefFilter(ACondition: TCondition; const ACriteria: String): String;
  public
    constructor Create;
    constructor Initialise(const AKey: String);
    procedure Assign(Sender: TReportField);
    function SQLFilter(ACondition: TCondition; const ACriteria1, ACriteria2: String): String;
    property ItemKey : TKeyString  read FItemKey write SetItemKey;
    property DisplayName: string read FDisplayName write FDisplayName;
    property TableName: string read FTableName write FTablename;
    property FieldName: string read FFieldName write FFieldName;
    property ApplyTo:Char read FApplyTo write FApplyTo;
    property Selectable: boolean read FSelectable write FSelectable;
    property Filter: boolean read FFilter write FFilter;
    property SQL: string read FSQL write FSQL;
    property Selected: boolean read FSelected write FSelected;
    property Union: TUnion read Funion write Funion;
    property FieldType: string read FFieldType write FFieldType;
  end;  // TReportField

  //----------------------------------------------------------------------------
  TFilter = class
  public
    FieldName   : string;
    Condition   : TCondition;
    Criteria    : string;
    ReportField : TReportField;
    procedure Assign(Sender: TFilter);
    constructor Create;
    destructor Destroy; override;
  end;  // TFilter

  //----------------------------------------------------------------------------
  TdlgFilter = class(TForm)
    Label1: TLabel;
    lbAvailableFields: TListBox;
    rgConditions: TRadioGroup;
    Bevel1: TBevel;
    Label3: TLabel;
    Label2: TLabel;
    eCriteria2: TEdit;
    eCriteria1: TEdit;
    bbAccept: TImageListButton;
    bbCancel: TImageListButton;
    procedure lbAvailableFieldsClick(Sender: TObject);
    procedure bbAcceptClick(Sender: TObject);
    procedure rgConditionsClick(Sender: TObject);
    procedure eCriteria1Exit(Sender: TObject);
    procedure eCriteria2Exit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CriteriaKeyPress(Sender: TObject; var Key: Char);
  private
    hlpFilter: TOnlineHelp;
    FTableName: String;
    FSimpleFilter: String;
    FFilteredField: TReportField;
    function FindFieldByKey(const AItemKey: String): TReportField;
    procedure SetFields;
    function ValidateEntry(AEdit : TEdit) : boolean;
    function ValidateSecondCriteria: boolean;
  public
    // Reintroduce removes the compiler warning, it doesn't change functionalities
    constructor Create(AOwner: TComponent; const ATableName: String); reintroduce; overload;
    constructor Create(AOwner: TComponent; const ATableName: String; ADataFilter: TFilterCriteria); reintroduce; overload;
    destructor Destroy; override;
    property SimpleFilter: String read FSimpleFilter;
    property FilteredField: TReportField read FFilteredField;
  end;

//==============================================================================
// This function is used in the Wizard to Encode the condition string
// into an enumerated type.
// It is not attached to the Complex Dialog Class as it is useful when
// dealing with the return from the COM style dialog.
function EncodeCondition(const iConditionString : string): TCondition;
function CreateFilter(const iFieldName:string; const iCondition:TCondition;
  const iType,iCriteria:string):TFilter;
function SplitBetweenCriteria(const iCriteria : string) : TBetweenCriteria;

//==============================================================================
implementation

{$R *.DFM}

uses
  ApplicationSettings, FormActions, SpatialRefFuncs;

resourcestring
  ResStr_NoSpatialReference = 'The spatial reference is not part of the current system.';
  ResStr_CriterionMissing = 'Criterion is missing';
  ResStr_SecondCriterionMissing = 'Second criterion is missing';
  ResStr_ValidYesNo = 'The entered value must be a valid Yes/No value.';
  ResStr_InvalidVagueDate = 'The Entered Date must be a valid vague date and not be set in the future.';
  ResStr_InvalidReference = '" is not a valid spatial reference.';
  ResStr_EqualDates = 'The First Date cannot contain the Second Date.';
  ResStr_ContainFirstDate = 'The Second Date cannot contain the First Date.';
  ResStr_SecondBeforeFirstDate = 'The Second Date cannot come before the First Date.';
  ResStr_IsEqualTo = 'is Equal to';
  ResStr_IsNotEqualTo = 'is Not equal to';
  ResStr_Contains ='Contains';
  ResStr_StartsWith='Starts with';
  ResStr_IsGreaterThan = 'is Greater than';
  ResStr_IsLessThan ='is Less than';
  ResStr_IsInGridSquare ='is In Grid Square';
  ResStr_Between ='Between';

//==============================================================================
function EncodeCondition( const iConditionString : string) : TCondition;
var lString :string;
begin
  lString := StringReplace(iConditionString, '&','',[rfReplaceAll,rfIgnoreCase]);
  //lString := UpperCase( lString );
  if lString = ResStr_IsEqualTo then
    Result := cdEqual
  else if lString = ResStr_IsNotEqualTo then
    Result := cdNotEqual
  else if lString = ResStr_Contains then
    Result := cdContains
  else if lString = ResStr_StartsWith then
    Result := cdStarts
  else if lString = ResStr_IsGreaterThan then
    Result := cdGreater
  else if lString = ResStr_IsLessThan then
    Result := cdLessThan
  else if lString = ResStr_IsInGridSquare then
    Result := cdInSquare
  else
    Result := cdBetween
end;  // EncodeConditione

//==============================================================================
function CreateFilter(const iFieldName:string; const iCondition:TCondition;
  const iType,iCriteria:string):TFilter;
var lNewFilter:TFilter;
begin
  lNewFilter:=TFilter.Create;
  with lNewFilter do begin
    ReportField := TReportField.Create;
    ReportField.TableName := 'CONSTRAINT';  // Keep uppercase
    ReportField.FieldType := iType;
    ReportField.Union     := uAlways;
    ReportField.FieldName := iFieldName;
    FieldName := iFieldName;
    Condition := iCondition;  // cdEqual or cdNotEqual;
    Criteria  := iCriteria;
  end;
  Result:=lNewFilter;
end;  // CreateFilter

//==============================================================================
function SplitBetweenCriteria(const iCriteria: string): TBetweenCriteria;
var
  lAndPos : integer;
begin
  Result.Criteria1 := '';
  Result.Criteria2 := '';
  lAndPos := Pos('AND', UpperCase(iCriteria));
  if lAndPos <> 0 then begin  // Get the 2 criteria and trim to remove extra spaces
    Result.Criteria1 := Trim(Copy(iCriteria, 1, lAndPos - 1 ));
    Result.Criteria2 := Trim(Copy(iCriteria, lAndPos + 3, Length(iCriteria) - lAndPos - 2));  // 3 being the no. of chars in 'AND'
  end;
end;  // SplitBetweenCriteria

//==============================================================================
//  TFilter
//==============================================================================
constructor TFilter.Create;
begin
  ReportField := TReportField.Create;
end;  // Create

//==============================================================================
destructor TFilter.Destroy;
begin
  ReportField.Free;
  inherited;
end;  // Destroy

//==============================================================================
procedure TFilter.Assign(Sender : TFilter);
begin
  FieldName:= Sender.FieldName;
  Criteria := Sender.Criteria;
  Condition:= Sender.Condition;
  ReportField.Assign(Sender.ReportField);
end;  // Assign

//==============================================================================
{ TReportField }
//------------------------------------------------------------------------------
constructor TReportField.Create;
begin
  inherited Create;
  FSelected := false;
  FUnion := uAlways;
end;  // TReportField.Create

{-------------------------------------------------------------------------------
 Overloaded constructor to create a single field from a key.
}
constructor TReportField.Initialise(const AKey: String);
var
  qryField: TJnccQuery;
begin
  qryField :=TJnccQuery.Create(nil);
  try
    dmDatabase.SetDatabaseLocal([qryField]);
    with  qryField do begin
      SQL.Text := 'SELECT * FROM Usable_Field WHERE Usable_Field_Key = ''' + AKey + ''' ';
      Open;
      if not Eof then begin
        FDisplayName := FieldByName('Field_Description').AsString;
        FTableName   := FieldByName('Table_Name').AsString;
        FFieldName   := FieldByName('Field_Name').AsString;
        FFieldType   := FieldByName('Field_Type').AsString;
        FSQL         := FieldByName('Calculation_SQL').AsString;
      end;
      Close;
    end;
  finally
    qryField.Free;
  end;
end;  // TReportField.Initialise

//------------------------------------------------------------------------------
{ Accessor method allows the item key to be set }
procedure TReportField.SetItemKey(const Value: TKeyString);
begin
  FItemKey := Value;
end;  // TReportField.SetItemKey

//------------------------------------------------------------------------------
//      Procedure Name: TReportField.Assign
//             Purpose: To Copy one TReportFiled to another.
//          Parameters: The TReportField to be copied
//------------------------------------------------------------------------------
procedure TReportField.Assign( Sender : TReportField);
begin
  Self.Filter     :=Sender.Filter;
  Self.Selectable :=Sender.Selectable;
  Self.SQL        :=Sender.SQL;
  Self.DisplayName:=Sender.DisplayName;
  Self.FieldName  :=Sender.FieldName;
  Self.Selected   :=Sender.Selected;
  Self.TableName  :=Sender.TableName;
  Self.ApplyTo    :=Sender.ApplyTo;
  Self.FieldType  :=Sender.FieldType;
  Self.Union      :=Sender.Union;
end;  // TReportField.Assign

{-------------------------------------------------------------------------------
}
function TReportField.SQLFilter(ACondition: TCondition;
  const ACriteria1, ACriteria2: String): String;
begin
  if (FieldType = TYPE_TEXT) or (FieldType = TYPE_RICH_TEXT) then
    Result := GetTextFilter(ACondition, ACriteria1)
  else
  if FieldType = TYPE_BOOLEAN then
    Result := GetBooleanFilter(ACondition, ACriteria1)
  else
  if FieldType = TYPE_SPATIAL_REF then
    Result := GetSpatialRefFilter(ACondition, DelocaliseSpatialRef(ACriteria1))
  else
  if FieldType = TYPE_VAGUE_DATE then
    Result := GetVagueDateFilter(ACondition, ACriteria1, ACriteria2)
  else
    Result := GetNumberFilter(ACondition, ACriteria1, ACriteria2);
end;  // TReportField.SQLFilter

{-------------------------------------------------------------------------------
}
function TReportField.GetTextFilter(ACondition: TCondition; const ACriteria: String): String;
var
  lCriteria: String;
begin
  // Double the quotes characters, if any, so SQL doesn't break
  lCriteria := StringReplace(ACriteria, #39, #39#39, [rfReplaceAll]);

  // Add the filtering condition. For Contains and Starts With, use the
  // ConvertSearchString function on the original text, otherwise, " chars would
  // be doubled again!
  case ACondition of
    // note we use like instead of = or <>, as it works for varchar and text
    cdEqual    : Result := SQL + ' LIKE ''' + lCriteria + '''';
    cdNotEqual : Result := SQL + ' NOT LIKE ''' + lCriteria + '''';
    cdContains : Result := SQL + ' LIKE ''%' + ConvertSearchString(ACriteria) + '%''';
    cdStarts   : Result := SQL + ' LIKE ''' + ConvertSearchString(ACriteria) + '%''';
  end; // case Condition of
end;  // TReportField.GetTextFilter

{-------------------------------------------------------------------------------
}
function TReportField.GetBooleanFilter(ACondition: TCondition; const ACriteria: String): String;
begin
  // Add the filtering condition
  case ACondition of
    cdEqual    : Result := SQL + ' = ' + ACriteria;
    cdNotEqual : Result := SQL + ' <> ' + ACriteria;
  end;
end;  // TReportField.GetBooleanFilter

{-------------------------------------------------------------------------------
}
function TReportField.GetNumberFilter(ACondition: TCondition; const ACriteria1,
  ACriteria2: String): String;
begin
  // Add the filtering condition, =/<>/>/</Between
  case ACondition of
    cdEqual    : Result := SQL + ' = ' + ACriteria1;
    cdNotEqual : Result := SQL + ' <> ' + ACriteria1;
    cdGreater  : Result := SQL + ' > ' + ACriteria1;
    cdLessThan : Result := SQL + ' < ' + ACriteria1;
    cdBetween  : Result := SQL + ' BETWEEN ' + ACriteria1 + ' AND ' + ACriteria2;
  end; // case Condition of
end;  // TReportField.GetNumberFilter

{-------------------------------------------------------------------------------
}
function TReportField.GetVagueDateFilter(ACondition: TCondition; const ACriteria1,
  ACriteria2: String): String;
var
  lStart, lEnd: Integer;
  lType, lFieldStub, lSQL: String;
  lVague1, lVague2: TVagueDate;
begin
  lFieldStub := Copy(FieldName, 1, Pos('_START', FieldName) - 1);
  lSQL       := Copy(SQL, 1, Pos(FieldName, UpperCase(SQL)) - 1);

  lVague1 := StringToVagueDate(ACriteria1);
  lStart  := Trunc(lVague1.StartDate);
  lEnd    := Trunc(lVague1.EndDate);
  lType   := '''' + lVague1.DateTypeString + '''';

  if ACondition = cdBetween then begin
    lVague2 := StringToVagueDate(ACriteria2);
    lEnd    := Trunc(lVague2.EndDate);
  end;

  // Add the filtering condition
  case ACondition of
    cdEqual    : Result := lSQL + lFieldStub + '_START = ' + IntToStr(lStart) + ' AND ' +
                                  lFieldStub + '_END = ' + IntToStr(lEnd) + ' AND ' +
                                  lFieldStub + '_TYPE = ' + lType;
    cdNotEqual : Result := lSQL + 'NOT (' + lFieldStub + '_START = ' + IntToStr(lStart) + ' AND ' +
                                  lFieldStub + '_END = ' + IntToStr(lEnd) + ' AND ' +
                                  lFieldStub + '_TYPE = ' + lType + ') OR ' +
                                  lFieldStub + '_START Is NULL';
    cdGreater  : Result := lSQL + lFieldStub + '_START > ' + IntToStr(lEnd);
    cdLessThan : Result := lSQL + lFieldStub + '_END < ' + IntToStr(lStart);
    cdBetween  : Result := lSQL + lFieldStub + '_START >= ' + IntToStr(lStart) + ' AND ' +
                                  lFieldStub + '_END <= ' + IntToStr(lEnd);
  end; // case Condition of
end;  // TReportField.GetVagueDateFilter

{-------------------------------------------------------------------------------
}
function TReportField.GetGridSquareFilter(const ACriteria: String): String;
var preStr, eStr, nStr, qStr: String;
    n, k: Integer;
begin
  n := (Length(ACriteria) - 1) div 2;
  if Length(ACriteria) mod 2 = 0 then begin
    if AppSettings.SpatialRefSystem = 'OSNI' then
      MessageDlg(ResStr_NoSpatialReference, mtInformation, [mbOK], 0);
    preStr := Copy(ACriteria, 1, 2);
    eStr := Copy(ACriteria, 3, n);
    nStr := Copy(ACriteria, 3 + n, n);
  end else begin
    if AppSettings.SpatialRefSystem = 'OSGB' then
      MessageDlg(ResStr_NoSpatialReference, mtInformation, [mbOK], 0);
    preStr := ACriteria[1];
    eStr := Copy(ACriteria, 2, n);
    nStr := Copy(ACriteria, 2 + n, n);
  end;

  Result := Copy(SQL, 1, LastDelimiter(' ', SQL)); //Remove " Spatial_Ref" from end of string
  qStr := '';
  for k := 1 to 6 - n do begin
    qStr := qStr + '_';  // single character wildcard
    Result := Result + ' (SPATIAL_REF LIKE ''' + preStr + eStr + qStr + nStr + qStr + ''') or';
    Result := Result + ' (SPATIAL_REF LIKE ''' + preStr + ' ' + eStr + qStr + nStr + qStr + ''') or';
  end;
  Result := Result + ' (SPATIAL_REF LIKE ''' + ACriteria + ''') or';
  Result := Result + ' (SPATIAL_REF LIKE ''' + preStr + ' ' + eStr + nStr + ''')';
end;  // TReportField.GetGridSquareFilter

{-------------------------------------------------------------------------------
}
function TReportField.GetSpatialRefFilter(ACondition: TCondition; const ACriteria: String): String;
begin
  if ACondition = cdInSquare then
    Result := GetGridSquareFilter(ACriteria)
  else
    Result := GetTextFilter(ACondition, ACriteria);
end;  // TReportField.GetSpatialRefFilter

//==============================================================================
//==============================================================================
constructor TdlgFilter.Create(AOwner:TComponent; const ATableName:string);
begin
  inherited Create(AOwner);
  FTableName := ATableName;
  FFilteredField := nil;
  FSimpleFilter := '';
  SetFields;
  with lbAvailableFields do
    if Items.Count > 0 then begin
      ItemIndex := 0;
      lbAvailableFieldsClick(nil);
    end;
end;  // Create

constructor TdlgFilter.Create(AOwner: TComponent; const ATableName: String; ADataFilter: TFilterCriteria);
var
  i: Integer;
begin
  Create(AOwner, ATableName);
  if ADataFilter.IsSet then
    if FindFieldByKey(ADataFilter.FilteredFieldKey) <> nil then begin
      eCriteria1.Text := ADataFilter.Criteria1;
      eCriteria2.Text := ADataFilter.Criteria2;

      with rgConditions do
        for i := 0 to Items.Count - 1 do
          if SameText(StringReplace(Items[i], '&', '', [rfReplaceAll]), ADataFilter.Condition) then
          begin
            rgConditions.ItemIndex := i;
            rgConditionsClick(nil);
          end;
    end;
end;

//==============================================================================
procedure TdlgFilter.FormCreate(Sender: TObject);
begin
  //Help Setup
  hlpFilter := TOnlineHelp.Create(Self.Handle);
  OnHelp := hlpFilter.OnHelpReplacement;
  HelpContext := IDH_FILTER;
end;  // FormCreate

//==============================================================================
procedure TdlgFilter.FormDestroy(Sender: TObject);
begin
  hlpFilter.Free;
end;  // FormDestroy

//==============================================================================
destructor TdlgFilter.Destroy;
var lIdx:integer;
begin
  with lbAvailableFields.Items do begin
    for lIdx:=0 to Count-1 do
      TReportField(Objects[lIdx]).Free;
    Clear;
  end;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TdlgFilter.bbAcceptClick(Sender: TObject);
var
  lCondition: TCondition;
begin
  // Set the focus on the Ok button, forcing an OnExit event on the Criteria boxes
  if eCriteria1.Text = '' then begin
    MessageDlg(ResStr_CriterionMissing, mtInformation, [mbOk], 0);
    eCriteria1.SetFocus;
    ModalResult := mrNone;
  end else
  if not ValidateEntry(eCriteria1) then begin
    // Message came up during validation
    eCriteria1.SetFocus;
    ModalResult := mrNone;
  end else
  if eCriteria2.Enabled and (eCriteria2.Text = '') then begin
    MessageDlg(ResStr_SecondCriterionMissing, mtInformation, [mbOk], 0);
    eCriteria2.SetFocus;
    ModalResult := mrNone;
  end else
  if not ValidateSecondCriteria then begin
    eCriteria2.SetFocus;
    ModalResult := mrNone;
  end else begin
    with lbAvailableFields do begin
      FFilteredField := TReportField(Items.Objects[ItemIndex]);
      lCondition := EncodeCondition(rgconditions.Items[rgConditions.ItemIndex]);
      FSimpleFilter := FFilteredField.SQLFilter(lCondition, eCriteria1.Text, eCriteria2.Text);
    end;
    ModalResult := mrOk;
  end;
end;  // bbAcceptClick

//==============================================================================
procedure TdlgFilter.SetFields;
var qryFields:TJnccQuery;
    lField   :TReportField;
begin
  // Populate only if there are any tables to get fields from
  if FTableName<>'' then begin
    qryFields:=TJnccQuery.Create(nil);
    try
      dmDatabase.SetDatabaseLocal([qryFields]);
      with qryFields do begin
        // As there is at least one table, the following statement is ok
        SQL.Text := 'SELECT * FROM Usable_Field WHERE Apply_To = ''F'' '+
                    'AND Table_Name = ''' + FTableName + '''';
        // Open and add fields
        Open;
        while not Eof do begin
          lField:=TReportField.Create;
          lField.ItemKey     := FieldByName('Usable_Field_Key').AsString;
          lField.DisplayName := FieldByName('Field_Description').AsString;
          lField.TableName   := FieldByName('Table_Name').AsString;
          lField.FieldName   := FieldByName('Field_Name').AsString;
          lField.FieldType   := FieldByName('Field_Type').AsString;
          lField.SQL         := FieldByName('Calculation_SQL').AsString;
          lbAvailableFields.Items.AddObject(lField.DisplayName, lField);
          Next;
        end;
        Close;
      end;
      bbAccept.Enabled     := lbAvailableFields.Items.Count > 0;
      rgConditions.Enabled := lbAvailableFields.Items.Count > 0;
    finally
      qryFields.Free;
    end;
  end;
end;  // SetFields

//==============================================================================
procedure TdlgFilter.lbAvailableFieldsClick(Sender: TObject);
var lField     :TReportField;
    lConditions:string;
begin
  inherited;
  with lbAvailableFields do
    lField := TReportField(Items.Objects[ItemIndex]);

  // Set the available conditions according to the type of the selected attribute
  if lField.FieldType = TYPE_TEXT then
    lConditions := ResStr_TextFilterCondition
  else if lField.FieldType = TYPE_SPATIAL_REF then
    if (AppSettings.SpatialRefSystem = 'OSGB') or (AppSettings.SpatialRefSystem = 'OSNI') then
      lConditions := ResStr_SpatialRefFilterConditionOS
    else
      lConditions := ResStr_SpatialRefFilterConditionNON
  else if lField.FieldType = TYPE_RICH_TEXT then
    lConditions := ResStr_RichTextFilterCondition
  else if lField.FieldType = TYPE_BOOLEAN then
    lConditions := ResStr_BooleanFilterCondition
  else  // It's a number, valid for dates too
    lConditions := ResStr_NumberFilterCondition;

  with rgConditions do
    if Items.CommaText<>lConditions then begin
      Items.CommaText:=lConditions;
      ItemIndex:=0;
    end;
end;  // lbAvailableFieldsClick

//==============================================================================
procedure TdlgFilter.rgConditionsClick(Sender: TObject);
begin
  if Pos(ResStr_Between, rgConditions.Items[rgConditions.ItemIndex])>0 then begin
    // Enable the edit box for second of the Between condition
    eCriteria1.Width  :=121;
    eCriteria2.Enabled:=true;
  end else begin
    eCriteria1.Width  :=273;
    eCriteria2.Enabled:=false;
  end;
end;  // rgConditionsClick

//==============================================================================
function TdlgFilter.ValidateEntry(AEdit: TEdit): boolean;
var
  lField: TReportField;
  lValidSpatialRef: TValidSpatialRef;
  lStr: String;
  lPos: TPoint;
begin
  Result := False;
  // If cancelling, don't go any further.
  lPos := bbCancel.ScreenToClient(Mouse.CursorPos);
  if (lPos.X in [0..bbCancel.Width]) and (lPos.Y in [0..bbCancel.Height]) then
    Exit;

  lField := TReportField(lbAvailableFields.Items.Objects[lbAvailableFields.ItemIndex]);

  if lField.FieldType = TYPE_BOOLEAN then begin
    if (CompareText(AEdit.Text,'Yes')<>0) and (CompareText(AEdit.Text,'No')<>0) then begin
      MessageDlg(ResStr_ValidYesNo, mtInformation,[mbOk],0);
      AEdit.SetFocus;
      Exit;
    end;
  end else if lField.FieldType = TYPE_VAGUE_DATE then begin
    if not CheckVagueDate(AEdit.Text) then begin
      MessageDlg(ResStr_InvalidVagueDate,
                 mtInformation, [mbOK], 0);
      AEdit.SetFocus;
      Exit;
    end else
      AEdit.Text := VagueDateToString(StringToVagueDate(AEdit.Text));
  end else if (lField.FieldType = TYPE_SPATIAL_REF) and
              (EncodeCondition(rgconditions.Items[rgConditions.ItemIndex]) = cdInSquare) then begin
    lStr := UpperCase(RemoveSubStrings(AEdit.Text, ' '));
    if lStr[1] in ['A'..'Z'] then
      if lStr[2] in ['A'..'Z'] then
        try
          if (Length(lStr) > 2) and ((Length(lStr) mod 2 = 1) or
             (StrToInt(Copy(lStr, 3, Length(lStr) - 2)) < 0)) then begin
            MessageDlg('"' + AEdit.Text + ResStr_InvalidReference, mtInformation, [mbOK], 0);
            AEdit.SetFocus;
            Exit;
          end else
            lValidSpatialRef := ValidUKSpatialRef(AEdit.Text);
        except
          on EConvertError do begin
            MessageDlg('"' + AEdit.Text + ResStr_InvalidReference, mtInformation, [mbOK], 0);
            AEdit.SetFocus;
            Exit;
          end;
        end
      else
        try
          if (Length(lStr) > 1) and ((Length(lStr) mod 2 = 0) or
             (StrToInt(Copy(lStr, 2, Length(lStr) - 1)) < 0)) then begin
            MessageDlg('"' + AEdit.Text + ResStr_InvalidReference, mtInformation, [mbOK], 0);
            AEdit.SetFocus;
            Exit;
          end else
            lValidSpatialRef := ValidIrishSpatialRef(AEdit.Text);
        except
          on EConvertError do begin
            MessageDlg('"' + AEdit.Text + ResStr_InvalidReference, mtInformation, [mbOK], 0);
            AEdit.SetFocus;
            Exit;
          end;
        end
    else begin
      MessageDlg('"' + AEdit.Text + ResStr_InvalidReference, mtInformation, [mbOK], 0);
      AEdit.SetFocus;
      Exit;
    end;
    if lValidSpatialRef.Valid then
      AEdit.Text := lValidSpatialRef.FormattedSR
    else begin
      MessageDlg(lValidSpatialRef.Error, mtInformation, [mbOK], 0);
      AEdit.SetFocus;
      Exit;
    end;
  end;
  Result := True;
end;  // ValidateEntry

//==============================================================================
procedure TdlgFilter.eCriteria1Exit(Sender: TObject);
begin
  if eCriteria1.Text <> '' then
    ValidateEntry(eCriteria1);
end;  // eCriteria1Exit

//==============================================================================
procedure TdlgFilter.eCriteria2Exit(Sender: TObject);
begin
  if eCriteria2.Text <> '' then
    ValidateSecondCriteria;
end;  // eCriteria2Exit

//==============================================================================
function TdlgFilter.ValidateSecondCriteria:boolean;
var lVague1,lVague2:TVagueDate;
  lField: TReportField;
begin
  Result:=not eCriteria2.Enabled;
  if not Result and ValidateEntry(eCriteria2) then begin
    lField := TReportField(lbAvailableFields.Items.Objects[lbAvailableFields.ItemIndex]);
    if lField.FieldType = TYPE_VAGUE_DATE then begin
      lVague1:=StringToVagueDate(eCriteria1.Text);
      lVague2:=StringToVagueDate(eCriteria2.Text);
      if IsVagueDateInVagueDate(lVague2, lVague1) then begin
        MessageDlg(ResStr_EqualDates, mtInformation, [mbOK], 0);
        eCriteria2.SetFocus;
        Exit;
      end else if IsVagueDateInVagueDate(lVague1, lVague2) then begin
        MessageDlg(ResStr_ContainFirstDate, mtInformation, [mbOK], 0);
        eCriteria2.SetFocus;
        Exit;
      end else if CompareVagueDateToVagueDate(lVague2,lVague1) < 0 then begin
        MessageDlg(ResStr_SecondBeforeFirstDate, mtInformation, [mbOK], 0);
        eCriteria2.SetFocus;
        Exit;
      end;
    end;
    Result:=true;
  end;
end;  // ValidateSecondCriteria

//==============================================================================
procedure TdlgFilter.CriteriaKeyPress(Sender: TObject; var Key: Char);
begin
  if Key='|' then begin
    Beep;
    Key:=#0;
  end;
end;  // CriteriaKeyPress

function TdlgFilter.FindFieldByKey(const AItemKey: String): TReportField;
var
  i: Integer;
begin
  Result := nil;
  with lbAvailableFields do
    for i := 0 to Items.Count - 1 do
      if TReportField(Items.Objects[i]).ItemKey = AItemKey then begin
        ItemIndex := i;
        lbAvailableFieldsClick(nil);
        Result := TReportField(Items.Objects[i]);
        Exit;
      end;
end;

end.
