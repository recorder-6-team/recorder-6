{===============================================================================
  Unit:        ThamesCommonClasses.pas

  Description: This unit contains classes referred to multiple times in the code.
                They have been put into a separate unit to stop the units
                having circular references.

  Model:       AdvancedReportFiles.mpb

  Created:     June 2004

  Last revision information:
    $Revision: 16 $
    $Date: 2/04/09 16:35 $
    $Author: Pauldavies $
===============================================================================}

unit CRCommonClasses;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, SMIBase, SMI2TXT,
  Forms, Dialogs, XMLIntf, XMLDoc, DB, ExceptionForm, DBClient, Grids;

resourcestring
  ResStr_InvalidEntryCount = 'The entrycount value of %s for condition %s is invalid. '+
     'It must be between 1 & 20 or -1 (for a flexible number of entries.';
type
  ECRClassException = class(TExceptionPath)
  end;
  
  {-----------------------------------------------------------------------------
    This method is used to ensure the messages created by the controls on the grid are not
    prematurely stopped
  }
  TStringGridWithEvents = class(TStringGrid)
    procedure WMCommand(var msg: TWMCommand); message WM_COMMAND;
  end;
  
  TParamDataType = (dtText, dtNumber, dtDate, dtVagueDate,dtDefault,
      dtTrueFalse, dtCSVFile, dtOptionSet, dtLocation, dtTaxon, dtBoundingBox,
      dtGridSquareRange, dtSpatialRef, dtName, dtIndividual, dtOrganisation,
      dtSamplesInPolygon, dtLocationsInPolygon, dtCurrentUserID);

  TOperator = (opEqual, opNotEqual, opLike, opLessThan, opGreaterThan,
                                   opLessThanEqual, opGreaterThanEqual, opIn,
                                   opIsEmpty);

  TCSVDataType = (csvdtText, csvdtNumber, csvdtDate);

  TKeyType = (ktDefault, ktName, ktLocation, ktGridSquareRange, ktTaxon, ktBiotope,
                      ktAdminArea, ktSurvey, ktSurveyEvent, ktSample,
                      ktTaxonOccurrence, ktBiotopeOccurrence, ktSpatialRef, ktSamplesInPolygon,
                      ktAddin, ktLocationsInPolygon);
  TKeyTypes = set of TKeyType;

  TUseRucksack = (urNo, urYes, urOptional);

//  KJG 18/11/2004
//  Represents a Single OrderBy Statement
  TOrderByOption = class(TObject)
  private
    FName: String;
    FValue: String;
  public
    property Name: String read FName write FName ;
    property Value: String read FValue write FValue;
  end;

//  List of Many OrderBy clauses
  TOrderByOptions = class(TList)
  private
    function GetItems(AIndex: Integer): TOrderByOption;
    procedure SetItems(AIndex: Integer; AItem: TOrderByOption);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TOrderByOption): Integer;
    procedure Clear; override;
    function Extract(AItem: TOrderByOption): TOrderByOption;
    function First: TOrderByOption;
    function IndexOf(AItem: TOrderByOption): Integer;
    procedure Insert(AIndex: Integer; AItem: TOrderByOption);
    function Last: TOrderByOption;
    function Remove(AItem: TOrderByOption): Integer;
    property Items[AIndex: Integer]: TOrderByOption read GetItems write SetItems; default;
  end;

  TConditionOption = class(TObject)
  private
    FName: String;
    FValue: String;
  public
    procedure ReadXML(ANode: IXMLNode);
    property Name: String read FName;
    property Value: String read FValue;
  end;

  TConditionOptions = class(TList)
  private
    function GetItems(AIndex: Integer): TConditionOption;
    procedure SetItems(AIndex: Integer; AItem: TConditionOption);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TConditionOption): Integer;
    procedure Clear; override;
    function Extract(AItem: TConditionOption): TConditionOption;
    function First: TConditionOption;
    function IndexOf(AItem: TConditionOption): Integer;
    procedure Insert(AIndex: Integer; AItem: TConditionOption);
    function Last: TConditionOption;
    function Remove(AItem: TConditionOption): Integer;
    property Items[AIndex: Integer]: TConditionOption read GetItems write SetItems; default;
  end;

  TInputParam = class(TObject)
  private
    FEntryCount: Integer;
    FActive: Boolean;
    FDataType: TParamDataType;
    FDefault : String;
    FDescription: String;
    FInputControl: TWinControl;
    FSubstituteText: String;
    FOptions: TConditionOptions;
    FPartialOverlap: Boolean;
    FSelectOneOfGroup: string;
    FValues: TStringList;
    FUseRucksack: TUseRucksack;
    procedure SetActive(const Values: Boolean);
    procedure SetInputControl(const Values: TWinControl);
    function ReadUseRucksack(ANode: IXMLNode): TUseRucksack;
  public
    constructor Create(ADatatype: TParamDataType; const ADescription: string; AOptions:
        TConditionOptions); reintroduce; overload;
    destructor Destroy; override;
    procedure ReadAttributes(ANode: IXMLNode);
    property Active: Boolean read FActive write SetActive;
    property DataType: TParamDataType read FDataType;
    property Description: String read FDescription;
    property Default: String read FDefault;
    property EntryCount: Integer read FEntryCount write FEntryCount;
    property InputControl: TWinControl read FInputControl write SetInputControl;
    property SubstituteText: String read FSubstituteText;
    property Options: TConditionOptions read FOptions;
    property PartialOverlap: Boolean read FPartialOverlap;
    property SelectOneOfGroup: string read FSelectOneOfGroup;
    property Values: TStringList read FValues;
    property UseRucksack: TUseRucksack read FUseRucksack;
  end;

implementation

uses
  CRConstants, GeneralFunctions, CRReportSQL, ResourceStrings;

{-==============================================================================
    TStringGridWithEvents
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TStringGridWithEvents.WMCommand(var msg: TWMCommand);
begin
  if EditorMode and (msg.Ctl = InplaceEditor.Handle) Then
    inherited
  else
    If msg.Ctl <> 0 Then
      msg.result :=
        SendMessage(msg.ctl, CN_COMMAND,
                     TMessage(msg).wparam,
                     TMessage(msg).lparam);
end;  // TStringGridWithEvents.WMCommand

{-==============================================================================
    TConditionOption
===============================================================================}
{-------------------------------------------------------------------------------
  Reads the ConditionOption XML node for the report file. 
}
procedure TConditionOption.ReadXML(ANode: IXMLNode);
begin
  with ANode do begin
    if HasAttribute(AT_NAME) then FName := Attributes[AT_NAME];
    if HasAttribute(AT_VALUE) then FValue := Attributes[AT_VALUE];
    if HasAttribute(AT_DEFAULT) then FValue := Attributes[AT_DEFAULT];

  end;
end;  // TConditionOption.ReadXML

{-==============================================================================
    TConditionOptions
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TConditionOptions.Create;
begin
  inherited Create;
end;  // TConditionOptions.Create

{-------------------------------------------------------------------------------
}
destructor TConditionOptions.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TConditionOptions.Destroy

{-------------------------------------------------------------------------------
}
function TConditionOptions.Add(AItem: TConditionOption): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TConditionOptions.Add

{-------------------------------------------------------------------------------
}
procedure TConditionOptions.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;

  inherited Clear;
end;  // TConditionOptions.Clear

{-------------------------------------------------------------------------------
}
function TConditionOptions.Extract(AItem: TConditionOption): TConditionOption;
begin
  Result := TConditionOption(inherited Extract(AItem));
end;  // TConditionOptions.Extract

{-------------------------------------------------------------------------------
}
function TConditionOptions.First: TConditionOption;
begin
  Result := TConditionOption(inherited First);
end;  // TConditionOptions.First

{-------------------------------------------------------------------------------
}
function TConditionOptions.GetItems(AIndex: Integer): TConditionOption;
begin
  Result := TConditionOption(inherited Items[AIndex]);
end;  // TConditionOptions.GetItems

{-------------------------------------------------------------------------------
}
function TConditionOptions.IndexOf(AItem: TConditionOption): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TConditionOptions.IndexOf

{-------------------------------------------------------------------------------
}
procedure TConditionOptions.Insert(AIndex: Integer; AItem: TConditionOption);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TConditionOptions.Insert

{-------------------------------------------------------------------------------
}
function TConditionOptions.Last: TConditionOption;
begin
  Result := TConditionOption(inherited Last);
end;  // TConditionOptions.Last

{-------------------------------------------------------------------------------
}
function TConditionOptions.Remove(AItem: TConditionOption): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TConditionOptions.Remove

{-------------------------------------------------------------------------------
}
procedure TConditionOptions.SetItems(AIndex: Integer; AItem: TConditionOption);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TConditionOptions.SetItems

//  KJG 18/11/2004 Created these helper classes for the order by parameters
{-==============================================================================
    TOrderByOptions
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TOrderByOptions.Create;
begin
  inherited Create;
end;  // TOrderByOptions.Create

{-------------------------------------------------------------------------------
}
destructor TOrderByOptions.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TOrderByOptions.Destroy

{-------------------------------------------------------------------------------
}
function TOrderByOptions.Add(AItem: TOrderByOption): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TOrderByOptions.Add

{-------------------------------------------------------------------------------
}
procedure TOrderByOptions.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;

  inherited Clear;
end;  // TOrderByOptions.Clear

{-------------------------------------------------------------------------------
}
function TOrderByOptions.Extract(AItem: TOrderByOption): TOrderByOption;
begin
  Result := TOrderByOption(inherited Extract(AItem));
end;  // TOrderByOptions.Extract

{-------------------------------------------------------------------------------
}
function TOrderByOptions.First: TOrderByOption;
begin
  Result := TOrderByOption(inherited First);
end;  // TOrderByOptions.First

{-------------------------------------------------------------------------------
}
function TOrderByOptions.GetItems(AIndex: Integer): TOrderByOption;
begin
  Result := TOrderByOption(inherited Items[AIndex]);
end;  // TOrderByOptions.GetItems

{-------------------------------------------------------------------------------
}
function TOrderByOptions.IndexOf(AItem: TOrderByOption): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TOrderByOptions.IndexOf

{-------------------------------------------------------------------------------
}
procedure TOrderByOptions.Insert(AIndex: Integer; AItem: TOrderByOption);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TOrderByOptions.Insert

{-------------------------------------------------------------------------------
}
function TOrderByOptions.Last: TOrderByOption;
begin
  Result := TOrderByOption(inherited Last);
end;  // TOrderByOptions.Last

{-------------------------------------------------------------------------------
}
function TOrderByOptions.Remove(AItem: TOrderByOption): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TOrderByOptions.Remove

{-------------------------------------------------------------------------------
}
procedure TOrderByOptions.SetItems(AIndex: Integer; AItem: TOrderByOption);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TOrderByOptions.SetItems
{-==============================================================================
    TInputParam
===============================================================================}
{-------------------------------------------------------------------------------
  Object initialisation.
}
constructor TInputParam.Create(ADatatype: TParamDataType; const ADescription: string;
    AOptions: TConditionOptions);
begin
  inherited Create;
  FDataType := ADataType;
  FDescription := ADescription;
  // SelectOneOfGroup parameters can be disabled
  FActive := True;
  if AOptions <> nil then
    FOptions := AOptions
  else
    FOptions := TConditionOptions.Create;
  FValues := TStringList.Create;
  FPartialOverlap := True;
end;  // TInputParam.Create

{-------------------------------------------------------------------------------
  Destructor.
}
destructor TInputParam.Destroy;
begin
  FOptions.Free;
  FValues.Free;
  inherited;
end;  // TInputParam.Destroy

{-------------------------------------------------------------------------------
  Read the parameter's attributes from the XML
}
procedure TInputParam.ReadAttributes(ANode: IXMLNode);
begin
  with ANode do begin
    If HasAttribute(AT_Default) then FDefault :=  Attributes[AT_DEFAULT];
    if HasAttribute(AT_SUBSTITUTEFOR) and
       (not (DataType in [dtSamplesInPolygon, dtLocationsInPolygon])) then
      FSubstituteText := Attributes[AT_SUBSTITUTEFOR];

    if HasAttribute(AT_SELECTONEOFGROUP) then FSelectOneofGroup := Attributes[AT_SELECTONEOFGROUP];

    if HasAttribute(AT_ENTRYCOUNT)
        and (not (DataType in [dtSamplesInPolygon, dtLocationsInPolygon])) then begin
      if not IsInt(Attributes[AT_ENTRYCOUNT]) then
        raise ECRClassException.Create(Format(ResStr_InvalidEntryCount, [
            Attributes[AT_ENTRYCOUNT], Description]));
      EntryCount := StrToInt(Attributes[AT_ENTRYCOUNT]);
      if ((EntryCount<1) or (EntryCount>20)) and (EntryCount<>-1) then
        raise ECRClassException.Create(Format(ResStr_InvalidEntryCount, [
            Attributes[AT_ENTRYCOUNT], Description]));
    end else
      EntryCount := 1;

    if HasAttribute(AT_INCLUDEPARTIALOVERLAP) then
      FPartialOverlap := not SameText(Attributes[AT_INCLUDEPARTIALOVERLAP], 'no');

    FUseRucksack := ReadUseRucksack(ANode);
  end;
end;

{-------------------------------------------------------------------------------
  Accessor method
}
procedure TInputParam.SetActive(const Values: Boolean);
begin
  FActive := Values;
end;

{-------------------------------------------------------------------------------
  Accessor - defines the control (or control group) used on the parameters form
}
procedure TInputParam.SetInputControl(const Values: TWinControl);
begin
  FInputControl := Values;
end;

{-------------------------------------------------------------------------------
  Sets the value of the UseRucksack property from the node's corresponding attribute
}
function TInputParam.ReadUseRucksack(ANode: IXMLNode): TUseRucksack;
var
  AttributeValue: string;
begin
  with ANode do begin
    if HasAttribute(AT_USERUCKSACK) then begin
      AttributeValue := Attributes[AT_USERUCKSACK];
      if      SameText(AttributeValue, 'yes')      then Result := urYes
      else if SameText(AttributeValue, 'no')       then Result := urNo
      else if SameText(AttributeValue, 'optional') then Result := urOptional
      else
        raise EReportSQLException.CreateNonCritical(ResStr_XMLReportDefProblem +
          Format(ResStr_InvalidOperator, [AttributeValue]));
    end else begin
      Result := urNo;
    end;    // if HasAttribute('userucksack')
  end;    // with ANode
end;

end.
