//==============================================================================
//  Unit:        ComplexFilter
//
//  Implements:  TdlgComplexFilter
//
//  Description:
//
//  Author:      John van Breda
//  Created:     8 Apr 1999
//
//  Last Revision Details:
//    $Revision: 60 $
//    $Date: 20/07/09 11:30 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit ComplexFilter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, ComCtrls, DB, VagueDate, OnlineHelp,
  Filter, ApplicationSettings, Constants, GeneralFunctions, ImageListButton,
  ReportWizardSettings;

type
  TdlgComplexFilter = class(TForm)
    pcComplexFilter   : TPageControl;
    tsConditions      : TTabSheet;
    tsConstraints     : TTabSheet;
    lblRecordCount    : TLabel;
    gbFilterDetails   : TGroupBox;
    Label1            : TLabel;
    Label3            : TLabel;
    lbAvailableFields : TListBox;
    eCriteria1        : TEdit;
    rgConditions      : TRadioGroup;
    cbRecordCount     : TCheckBox;
    sgFilters         : TStringGrid;
    cmbAndOr          : TComboBox;
    Bevel1            : TBevel;
    cbExcConfidential : TCheckBox;
    cbExcUnChecked    : TCheckBox;
    cbExcUnverified   : TCheckBox;
    cbExcZero         : TCheckBox;
    Label2            : TLabel;
    eCriteria2        : TEdit;
    bbOK: TImageListButton;
    bbCancel: TImageListButton;
    bbAdd: TImageListButton;
    bbEdit: TImageListButton;
    bbDelete: TImageListButton;
    bbMoveUp: TImageListButton;
    bbMoveDown: TImageListButton;
    bbFilterAccept: TImageListButton;
    bbFilterCancel: TImageListButton;
    bbClearAll: TImageListButton;
    procedure lbAvailableFieldsClick(Sender: TObject);
    procedure bbAddClick(Sender: TObject);
    procedure bbFilterAcceptClick(Sender: TObject);
    procedure sgFiltersDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure bbDeleteClick(Sender: TObject);
    procedure cmbAndOrChange(Sender: TObject);
    procedure bbFilterCancelClick(Sender: TObject);
    procedure bbEditClick(Sender: TObject);
    procedure bbMoveUpClick(Sender: TObject);
    procedure sgFiltersClick(Sender: TObject);
    procedure bbMoveDownClick(Sender: TObject);
    procedure rgConditionsClick(Sender: TObject);
    procedure pcComplexFilterChanging(Sender: TObject; var AllowChange: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bbOKClick(Sender: TObject);
    procedure bbClearAllClick(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure eCriteria1Exit(Sender: TObject);
    procedure eCriteria2Exit(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pcComplexFilterChange(Sender: TObject);
    procedure CriteriaKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    hlpAddFilter   : TOnlineHelp;
    FFilterList    : TList;
    FOldFilterList : TList;
    FSettings: TReportWizardSettings;
    procedure EnableCriteriaFields;
    procedure SwitchFocus(const tfDetails: boolean);
    procedure CheckButtons;
    procedure DisplayFilterDetails(AFilter: TFilter);
    procedure DisplayRowDetails;
    function ValidateEntry(iEdit : TEdit) : boolean;
    function ValidateSecondCriteria: boolean;
  public
    { Public declarations }
    property FilterList : TList read FFilterList write FFilterList;
    property OldFilterList : TList read FOldFilterList write FOldFilterList;
    constructor Create(Sender: TComponent; const ioFilterList: TList;
        ASettings: TReportWizardSettings); reintroduce; overload;
    destructor Destroy; override ;
    procedure AddCriteria(var ioText :string; const ACriteria : string;
      const ACondition : TCondition);
  end;  // TdlgComplexFilter

//==============================================================================
implementation

{$R *.DFM}

uses
  Maintbar, FormActions, StrUtils;

const
  I_NEW  = 1;
  I_EDIT = 2;

resourcestring
  ResStr_YesNoValue =   'The entered value must be a valid Yes/No value.';
  ResStr_ValidVagueDate = 'The Entered Date must be a valid vague date.';
  ResStr_ValidLongitude = 'Longitude must be between -180 and 180 (East).';
  ResStr_ValidLatitude =  'Latitude must be between -90 and 90 (North).';
  ResStr_ValidNumber =  'The entered value must be a valid number.';
  ResStr_ValidTime =  'The entered value must be a valid time';
  ResStr_NoSameDate = 'The First Date cannot contain the Second Date.';
  ResStr_NoSameDate2 =  'The Second Date cannot contain the First Date.';
  ResStr_InvalidDates = 'The Second Date cannot come before the First Date.';
  ResStr_SecondNumLessThanFirst = 'The Second Number cannot be less than the first Number.';
  ResStr_SecondTimeBeforeFirst =  'The Second Time cannot be before the first Time.';
  ResStr_CannotCloseInEdit =  'You cannot close while in edit mode.';
  ResStr_MissingCriterion = 'Criterion is missing.';




var
  EditMode : byte;

//==============================================================================
function GetConditionGroupIndex( iCondition : TCondition) : integer;
begin
  Result := 0;
  case iCondition of
    cdStarts:   Result:=2;  // Starts with
    cdContains: Result:=3;  // contains
    cdEqual:    Result:=0;  // is equal to
    cdNotEqual: Result:=1;  // not equal
    cdGreater:  Result:=2;  // Greater than
    cdLessThan: Result:=3;  // less than
    // Note that the between is added here but the And between the two dates
    // is put in on the ComplexFilter dialog.
    cdBetween:  Result:=4;  // between
  end; // case Condition of
end;  // GetConditionGroupIndex

//==============================================================================
//  TdlgComplexFilter
constructor TdlgComplexFilter.Create(Sender: TComponent; const ioFilterList: TList;
        ASettings: TReportWizardSettings);
var
  i         : integer;
  iFilter   : TFilter;
  lField    : TReportField;
  RowNumber : integer;
  lLocalText: string;
begin
  inherited Create(Sender);
  FSettings     := ASettings;
  FilterList    := ioFilterList;
  OldFilterList := TList.Create;
  // Copy existing filters
  for i := 0 to FilterList.Count - 1 do
  begin
    // make a copy of the existing filter
    iFilter := TFilter.Create;
    iFilter.Assign(FilterList.Items[i]);
    // and store it on the OldFilter list
    OldFilterList.Add(iFilter);
    // then remove the first copy
    TFilter(FilterList.Items[i]).Free;
  end;
  // Clear the incoming list of pointers
  FilterList.Clear;

  // Initialise the constraints checkboxes
  // Clear all the check states as they are reset later
  cbExcConfidential.Checked := not FSettings.ShowConfidential;
  cbExcUnChecked.Checked    := not FSettings.ShowUnchecked;
  cbExcUnVerified.Checked   := not FSettings.ShowInvalid;
  cbExcZero.Checked         := not FSettings.ShowZeroAbundance;
  SwitchFocus(false); //make everything disabled that should be.
  with sgFilters do
  begin
    // we must have at least one row plus the header
    RowCount := 2;
    sgFilters.Cells[1, 0] := ResStr_Cap_Filters;
    sgFilters.FixedRows   := 1;
    for RowNumber := 1 to OldFilterList.Count do
    begin
      // make a copy of the existing filter
      iFilter := TFilter.Create;
      iFilter.Assign(OldFilterList.Items[RowNumber - 1]);
      // and store it on the OldFilter list
      // Set a local field object to save typing
      lField:=iFilter.Reportfield;
      if lField.DisplayName <> '' then
      begin
        // If this is not the first real filter
        if Cells[1, 1] <> '' then
        // add a new row at the end
          RowCount := RowCount + 1;
        // following code RELIES UPON the "real" filters being at the start
        // we have a real field to display
        lLocalText := lField.DisplayName;
        Objects[1, RowNumber] := iFilter;
        // Add the conditional part
        AddCriteria(lLocalText, iFilter.Criteria, iFilter.Condition);
        Cells[1, RowNumber] := lLocalText;
        if RowNumber > 1 then
        begin
          // cheat a bit here by using the union flag of the report field
          if (lField.Union = uAlways) then
            Cells[0,RowNumber]:= 'AND'
          else
            Cells[0,RowNumber] := 'OR';
        end;
      end; // If we have a real field
    end; // for loop
  end; // with
  cbExcConfidential.Enabled := AppSettings.ConfidentialAccessGranted(AppSettings.UserAccessLevel);
  CheckButtons;
end;  // CreateWithList

//==============================================================================
procedure TdlgComplexFilter.FormCreate(Sender: TObject);
begin
  with sgFilters do begin
    RowCount    := 2;
    Cells[1, 0] := ResStr_Cap_Filters;
    FixedRows   := 1;
  end;
  CheckButtons;

  //Help Setup
  hlpAddFilter:= TOnlineHelp.Create(Self.Handle);
  OnHelp      := hlpAddFilter.OnHelpReplacement;
  HelpContext := IDH_ADDFILTERS;
  pcComplexFilter.HelpContext:= IDH_ADDFILTERSCONDITIONS;
  tsConditions.HelpContext   := IDH_ADDFILTERSCONDITIONS;
  tsConstraints.HelpContext  := IDH_ADDFILTERSCONSTRAINTS;
end;  // FormCreate

//==============================================================================
procedure TdlgComplexFilter.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose:=false;
  if tsConstraints.Enabled then
    CanClose:=true
  else
    MessageDlg(ResStr_CannotCloseInEdit,mtInformation,[mbOk],0);
end;  // FormCloseQuery

//==============================================================================
procedure TdlgComplexFilter.FormDestroy(Sender: TObject);
begin
  hlpAddFilter.Free;
end;  // FormDestroy

//==============================================================================
destructor TdlgComplexFilter.Destroy;
begin
  OldFilterList.Free;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TdlgComplexFilter.AddCriteria(var ioText:string;
  const ACriteria:string; const ACondition:TCondition);
begin
  case ACondition of
    cdStarts:   ioText:= ioText + ' Starts With ' + ACriteria + ' ';      // Starts with
    cdContains: ioText:= ioText + ' Contains ' + ACriteria + ' ';         // contains
    cdEqual:    ioText:= ioText + ' is Equal to ' + ACriteria + ' ';      // is equal to
    cdNotEqual: ioText:= ioText + ' is Not equal to ' + ACriteria + ' ';  // not equal
    cdGreater:  ioText:= ioText + ' is Greater than ' + ACriteria + ' ';  // Greater than
    cdLessThan: ioText:= ioText + ' is Less than ' + ACriteria + ' ';      // less than
    // Note that the between is added here but the And between the two dates is put in on the
    // ComplexFilter dialog.
    cdBetween:  ioText:= ioText + ' Between ' + ACriteria + ' ';           // between
  end; // case iFilter.Condition of
end;  // AddCriteria

//==============================================================================
procedure TdlgComplexFilter.SwitchFocus(const tfDetails:boolean);
var
  iIndex : integer;
  lFilter : TFilter;
begin
  inherited;
  // Change Enabled state to On or Off, opposite from below
  with gbFilterDetails do
    for iIndex:=0 to ControlCount-1 do
      if not (Controls[iIndex] is TLabel) then
        Controls[iIndex].Enabled:=tfDetails;
  // Change Enabled state to Off or ON, opposite from above
  with tsConditions do
    for iIndex:=0 to ControlCount-1 do
      if not (Controls[iIndex] is TGroupBox) and not (Controls[iIndex] is TLabel) then
        Controls[iIndex].Enabled:=not tfDetails;

  // Disable Constraint page if on filter details
  tsConstraints.Enabled:=not tfDetails;

  bbOk.Enabled    :=not tfDetails;
  bbCancel.Enabled:=not tfDetails;

  if tfDetails then
  begin
    cmbAndOr.Visible:=false;
    if EditMode=I_NEW then begin
      lbAvailableFields.ItemIndex:=0;
      lbAvailableFieldsClick(Self);
      eCriteria1.Text   :='';
      eCriteria2.Text   :='';
    end else
      with sgFilters do begin
        lFilter := TFilter(Objects[1,Row]);
        DisplayFilterDetails(lFilter);
      end;
    EnableCriteriaFields;
  end;
end;  // SwitchFocus

//==============================================================================
procedure TdlgComplexFilter.CheckButtons;
begin
  with sgFilters do begin
    bbEdit.Enabled    :=Cells[1,1]<>'';
    bbDelete.Enabled  :=bbEdit.Enabled;
    bbMoveUp.Enabled  :=Row>1;
    bbMoveDown.Enabled:=Row<RowCount-1;
    bbClearAll.Enabled:=bbEdit.Enabled;
  end;
  cmbAndOr.Visible:=false;
end;  // CheckButtons

//==============================================================================
procedure TdlgComplexFilter.bbOKClick(Sender: TObject);
var
  i           : integer;
  lThisFilter : TFilter;
begin
  if sgFilters.Cells[1,1]<>'' then // we have filters
  begin
    // we do not need the objects originally saved as
    // the user has opted for the new list.
    for i:=0 to OldFilterList.Count-1 do
      TFilter(OldFilterList.items[i]).Free;
    OldFilterList.Clear;

    //  Now put new filters from string grid to advanced filter list
    for i := 1 to sgFilters.RowCount -1 do
    begin
      // we could avoid this copying lark but it is easier
      // to follow as it keeps the code "symmetrical"
      // Create a new copy of the filter
      lThisFilter := TFilter.create;
      lThisFilter.Assign( TFilter(sgFilters.Objects[1,i]) );
      // Add this to the external list of filters
      FilterList.Add( lThisFilter );
      // remove the original
      TFilter(sgFilters.Objects[1,i]).Free;
    end;
  end; // we have filters
  // Now let's deal with the constraints form
  FSettings.ShowConfidential   := not cbExcConfidential.Checked;
  FSettings.ShowUnchecked      := not cbExcUnChecked.Checked;
  FSettings.ShowInvalid        := not cbExcUnVerified.Checked;
  FSettings.ShowZeroAbundance  := not cbExcZero.Checked;
  ModalResult := mrOk;
end;  // bbOkClick

//==============================================================================
procedure TdlgComplexFilter.bbClearAllClick(Sender: TObject);
var
  lRow : integer;
  lCol : integer;
begin
  with sgFilters do 
    if Cells[1,1] <> '' then
    begin
      // we have a filter or more
      for lRow:=1 to RowCount-1 do
      begin
        TFilter(Objects[1,lRow]).Free;
        for lCol := 0 to ColCount -1 do
          Cells[lCol,lRow] := '';
      end;
      // reduce to just header and one empty row
      RowCount := 2;
    end;
  CheckButtons;
end;  // bbClearAllClick

//==============================================================================
procedure TdlgComplexFilter.bbCancelClick(Sender: TObject);
var
  i : integer;
  lFilter : TFilter;
begin
  for i := 0 to OldFilterList.Count -1 do begin
    // We could avoid all this copying lark but it
    // keeps the code "symmetrical"
    lFilter := TFilter.Create;
    lFilter.Assign( OldFilterList.Items[i] );
    // add this copy to the external list
    FilterList.Add( lFilter );
    // remove from the internal list
    OldFilterList.Items[i];
  end;
  OldFilterList.Clear;
end;  // bbCancelClick

//==============================================================================
procedure TdlgComplexFilter.bbAddClick(Sender: TObject);
begin
  inherited;
  EditMode:=I_NEW;
  SwitchFocus(true);
end;  // bbAddClick

//==============================================================================
procedure TdlgComplexFilter.bbEditClick(Sender: TObject);
begin
  inherited;
  EditMode:=I_EDIT;
  SwitchFocus(true);
end;  // bbEditClick

//==============================================================================
procedure TdlgComplexFilter.bbDeleteClick(Sender: TObject);
begin
  with sgFilters do begin
// Do not remove this object as it is a shared pointer with OldFilter
//    TFilter(Objects[1,Row]).Free;
    DelLineInGrid(sgFilters);
    Cells[0,1]:='';  // no AND/OR on first filter
  end;
  CheckButtons;
end;  // bbDeleteClick

//==============================================================================
procedure TdlgComplexFilter.lbAvailableFieldsClick(Sender: TObject);
var lField:TReportField;
    lConditions:string;
begin
  inherited;
  with lbAvailableFields do
    lField := TReportField(Items.Objects[ItemIndex]);

  // Set the available conditions according to the tyoe of the selected attribute
  if (lField.FieldType = TYPE_TEXT) or (lField.FieldType = TYPE_SPATIAL_REF) then
    lConditions:=ResStr_TextFilterCondition
  else if lField.FieldType = TYPE_RICH_TEXT then
    lConditions:=ResStr_RichTextFilterCondition
  else if lField.FieldType = TYPE_BOOLEAN then
    lConditions:=ResStr_BooleanFilterCondition
  else if lField.FieldType = TYPE_VAGUE_DATE then
  begin
    if not IsVagueDate(eCriteria1.Text) then eCriteria1.Clear;
    if not IsVagueDate(eCriteria2.Text) then eCriteria2.Clear;
    lConditions:=ResStr_NumberFilterCondition;
  end
  else
  begin
    if not IsInt(eCriteria1.Text) then eCriteria1.Clear;
    if not IsInt(eCriteria2.Text) then eCriteria2.Clear;
    lConditions:=ResStr_NumberFilterCondition;
  end;

  with rgConditions do
    if (Items.CommaText<>lConditions) or (ItemIndex<0) then begin
      Items.CommaText:=lConditions;
      ItemIndex:=0;
    end;
end;  // lbAvailableFieldsClick

//==============================================================================
procedure TdlgComplexFilter.bbFilterAcceptClick(Sender: TObject);
var
  NewFilter : TFilter;
  lFieldObj : TReportField;
begin
  inherited;
  if eCriteria1.Text='' then begin
    MessageDlg(ResStr_MissingCriterion,mtInformation,[mbOk],0);
    eCriteria1.SetFocus;
  end else
  if not ValidateEntry(eCriteria1) then begin
    // Message came up during validation
    eCriteria1.SetFocus;
  end else
  if eCriteria2.Enabled and (eCriteria2.Text='') then begin
    MessageDlg(ResStr_MissingCriterion,mtInformation,[mbOk],0);
    eCriteria2.SetFocus;
  end else
  if not ValidateSecondCriteria then begin
    eCriteria2.SetFocus;
  end else  begin
    NewFilter:=TFilter.Create;
    with NewFilter do begin
      lFieldObj := TReportField(lbAvailableFields.Items.Objects[lbAvailableFields.itemindex]);
      ReportField.Assign(lFieldObj);
       // retain existing And/Or status as they default to AND
      if (EditMode=I_EDIT) and (sgFilters.Cells[0,sgFilters.Row]='OR') then
        ReportField.Union := uOmit;
      FieldName:=lbAvailableFields.Items[lbAvailableFields.ItemIndex];
      Condition:=EncodeCondition(rgConditions.Items[ rgConditions.ItemIndex ]);
      Criteria :=eCriteria1.Text;
      // check if a between range exists
      if (eCriteria2.Enabled) And (eCriteria2.text <> '') then
        Criteria := Criteria + ' And ' + eCriteria2.Text;
    end;

    with sgFilters do begin
      if EditMode=I_NEW then
        if (RowCount=2) and (Cells[1,1]='') then // first filter
          Row:=1
        else begin                               // additional filters
          RowCount:=RowCount+1;
          Row     :=Rowcount-1;
          Cells[0,Row]:='AND';
        end;

      Cells  [1,Row]:=NewFilter.FieldName+' '+
                      dmFormActions.TrimText(rgConditions.Items[rgConditions.ItemIndex])+' '+
                      NewFilter.Criteria;
      Objects[1,Row]:=NewFilter;
    end;
    SwitchFocus(false);
    CheckButtons;
  end;
end;  // bbFilterAcceptClick

//==============================================================================
procedure TdlgComplexFilter.bbFilterCancelClick(Sender: TObject);
begin
  inherited;
  SwitchFocus(false);
  CheckButtons;
end;  // bbFilterCancelClick

//==============================================================================
procedure TdlgComplexFilter.sgFiltersDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  inherited;
  with sgFilters do begin
    if (gdFocused in State) and (Col = 0) and (Row > 1) then
    begin
      cmbAndOr.SetBounds(Rect.Left+Left+2,Rect.Top+Top+1,Rect.Right-Rect.Left-1,cmbAndOr.Height);
      cmbAndOr.ItemIndex:=cmbAndOr.Items.IndexOf(Cells[Col,Row]);
      cmbAndOr.Visible  :=True;
    end;
    if (gdFocused in State) and (Col<>0) and (Row>0) then
      cmbAndOr.Visible:=false;

    Canvas.FillRect(Rect);
    DrawChoppedText(Cells[ACol,ARow],Canvas,Rect,2);
  end;
end;  // sgFiltersDrawCell

//==============================================================================
procedure TdlgComplexFilter.cmbAndOrChange(Sender: TObject);
begin
  with sgFilters do begin
    Cells[0,Row]:=cmbAndOr.Text;
    if CompareText(cmbAndOr.Text,'AND')=0 then
      // This flag set means that we have an 'And' pre-condition
      TFilter(Objects[1,Row]).ReportField.Union := uAlways
    else
      TFilter(Objects[1,Row]).ReportField.Union := uOmit;
  end;
end;  // cmbAndorChange

//==============================================================================
procedure TdlgComplexFilter.bbMoveUpClick(Sender: TObject);
begin
  with sgFilters do begin
    // Don't put anything in cell 0,1
    if Row>2 then
      Cols[0].Exchange(Row,Row-1);
    Cols[1].Exchange(Row,Row-1);
    Row:=Row-1;
  end;
  CheckButtons;
end;  // bbMoveUpClick

//==============================================================================
procedure TdlgComplexFilter.bbMoveDownClick(Sender: TObject);
begin
  with sgFilters do begin
    // Don't put anything in cell 0,1
    if Row>1 then
      Cols[0].Exchange(Row,Row+1);
    Cols[1].Exchange(Row,Row+1);
    Row:=Row+1;
  end;
  CheckButtons;
end;  // bbMoveDownClick

//==============================================================================
procedure TdlgComplexFilter.sgFiltersClick(Sender: TObject);
begin
  CheckButtons;
  DisplayRowDetails;
end;  // sgFiltersClick

//==============================================================================
procedure TdlgComplexFilter.rgConditionsClick(Sender: TObject);
begin
  EnableCriteriaFields;
end;  // rgConditionsClick

//==============================================================================
procedure TdlgComplexFilter.pcComplexFilterChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange:=bbAdd.Enabled;
end;  // pcComplexFilterChanging

//==============================================================================
function TdlgComplexFilter.ValidateEntry(iEdit : TEdit) : boolean;
var lField : TReportField;
begin
  Result := False;
  lField := TReportField(lbAvailableFields.Items.Objects[lbAvailableFields.ItemIndex]);
  if lField.FieldType = TYPE_BOOLEAN then begin
    if (CompareText(iEdit.Text,'Yes')<>0) and (CompareText(iEdit.Text,'No')<>0) then begin
      MessageDlg(ResStr_YesNoValue, mtInformation,[mbOk],0);
      iEdit.SetFocus;
      Exit;
    end;
  end else
  if lField.FieldType = TYPE_VAGUE_DATE then begin
    if not CheckVagueDate(iEdit.Text) then begin
      MessageDlg(ResStr_ValidVagueDate , mtInformation, [mbOK], 0);
      iEdit.SetFocus;
      Exit;
    end else
      iEdit.Text := VagueDateToString(StringToVagueDate(iEdit.Text));
  end else
    if (lField.FieldType = TYPE_NUMBER)
      then
      if CompareText(RightStr(lField.FieldName, 4), 'Time') <>0 then
      //if it's not a time, it should be a number
        try
          iEdit.Text := FloatToStr(StrToFloat(iEdit.Text));
          if  CompareTExt(RightStr(lField.FieldName, 4), 'Long')= 0 then
          begin
            if Abs(StrToFloat(iEdit.Text)) >180 then
            begin
              MessageDlg(ResStr_ValidLongitude, mtInformation, [mbOK], 0);
              iEdit.SetFocus;
              Exit;
            end;
          end
          else if CompareTExt(RightStr(lField.FieldName, 3), 'Lat')= 0 then
            if Abs(StrToFloat(iEdit.Text))>90 then
            begin
              MessageDlg(ResStr_ValidLatitude, mtInformation, [mbOK], 0);
              iEdit.SetFocus;
              Exit;
            end;
        except
          on EConvertError do
          begin
            MessageDlg(ResStr_ValidNumber, mtInformation, [mbOK], 0);
            iEdit.SetFocus;
            Exit;
          end;
        end
      else
        //make sure it's a valid time
        try
          iEdit.Text := TimeToStr(StrToTime(iEdit.Text));
        except
          on EConvertError do
          begin
            MessageDlg(ResStr_ValidTime, mtInformation, [mbOK], 0);
            iEdit.SetFocus;
            Exit;
          end;
        end;



  Result := True;
end;  // ValidateEntry

//==============================================================================
procedure TdlgComplexFilter.eCriteria1Exit(Sender: TObject);
begin
  if eCriteria1.Text <> '' then
    ValidateEntry(eCriteria1);
end;  // eCriteria1Exit

//==============================================================================
procedure TdlgComplexFilter.eCriteria2Exit(Sender: TObject);
begin
  if eCriteria2.Text <> '' then
    ValidateSecondCriteria;
end;  // eCriteria2Exit

//==============================================================================
function TdlgComplexFilter.ValidateSecondCriteria:boolean;
var lVague1,lVague2:TVagueDate;
  lField : TReportField;
begin

  lField := TReportField(lbAvailableFields.Items.Objects[lbAvailableFields.ItemIndex]);
  Result:=not eCriteria2.Enabled;
  if not Result and ValidateEntry(eCriteria2) then begin
    if lField.FieldType = TYPE_VAGUE_DATE then
    begin
      lVague1:=StringToVagueDate(eCriteria1.Text);
      lVague2:=StringToVagueDate(eCriteria2.Text);
      if IsVagueDateInVagueDate(lVague2, lVague1) then begin
        MessageDlg(ResStr_NoSameDate, mtInformation, [mbOK], 0);
        eCriteria2.SetFocus;
        Exit;
      end else if IsVagueDateInVagueDate(lVague1, lVague2) then begin
        MessageDlg(ResStr_NoSameDate2, mtInformation, [mbOK], 0);
        eCriteria2.SetFocus;
        Exit;
      end else if CompareVagueDateToVagueDate(lVague2,lVague1) < 0 then begin
        MessageDlg(ResStr_InvalidDates, mtInformation, [mbOK], 0);
        eCriteria2.SetFocus;
        Exit;
      end;
    end
    else if lField.FieldType = TYPE_NUMBER then
    begin
      if CompareText(RightStr(lField.FieldName, 4), 'Time') <>0 then
      begin
        if StrToFloat(eCriteria2.Text) < StrToFloat(eCriteria1.Text) then begin
          MessageDlg(ResStr_SecondNumLessThanFirst, mtInformation, [mbOK], 0);
          eCriteria2.SetFocus;
          Exit;
        end;
      end
      else
        if StrToTime(eCriteria2.Text) < StrToTime(eCriteria1.Text) then begin
          MessageDlg(ResStr_SecondTimeBeforeFirst, mtInformation, [mbOK], 0);
          eCriteria2.SetFocus;
          Exit;
        end;
    end;
    Result := true;
  end;
end;  // ValidteSecondCriteria

//==============================================================================
procedure TdlgComplexFilter.pcComplexFilterChange(Sender: TObject);
begin
  pcComplexFilter.HelpContext := pcComplexFilter.ActivePage.HelpContext;
end;  // pcComplexFilterChange

//==============================================================================
procedure TdlgComplexFilter.CriteriaKeyPress(Sender: TObject; var Key: Char);
begin
  if Key='|' then begin
    Beep;
    Key:=#0;
  end;
end;  // CriteriaKeyPress

{-------------------------------------------------------------------------------
  Loads the details of a filter into the display controls
}
procedure TdlgComplexFilter.DisplayFilterDetails(AFilter: TFilter);
var
  lBetweenCriteria : TBetweenCriteria;
begin
  lbAvailableFields.ItemIndex:=lbAvailableFields.Items.IndexOf(AFilter.FieldName);
  lbAvailableFieldsClick(nil);
  rgConditions.ItemIndex:=GetConditionGroupIndex(AFilter.Condition);
  if AFilter.Condition = cdBetween then begin
    lBetweenCriteria:= SplitBetweenCriteria(AFilter.Criteria);
    eCriteria1.Text := lBetweenCriteria.Criteria1;
    eCriteria2.Text := lBetweenCriteria.Criteria2;
  end else begin
    eCriteria1.Text   :=AFilter.Criteria;
  end;
end;

{-------------------------------------------------------------------------------
  Click a row in the grid at the bottom updates the controls at the top with
     the details.
}
procedure TdlgComplexFilter.DisplayRowDetails;
begin
  if (sgFilters.Row>0) and (sgFilters.Row<=sgFilters.RowCount) then
    if Assigned(sgFilters.Objects[1,sgFilters.Row]) then
      DisplayFilterDetails(TFilter(sgFilters.Objects[1,sgFilters.Row]));
end;

{-------------------------------------------------------------------------------
  Enable eCiteria1 and eCriteria2 if appropriate.  eCriteria2 is only enabled
     for Between type filters
}
procedure TdlgComplexFilter.EnableCriteriaFields;
begin
  if rgConditions.ItemIndex>-1 then begin
    if Pos('Between',rgConditions.Items[rgConditions.ItemIndex])>0 then begin
      // Enable the edit box for second of the Between condition
      eCriteria1.Width  :=121;
      eCriteria2.Enabled:=true;
    end else begin
      eCriteria1.Width  :=273;
      eCriteria2.Enabled:=false;
    end;
  end;
end;

end.
