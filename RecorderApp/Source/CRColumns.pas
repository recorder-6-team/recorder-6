{===============================================================================
  Unit:        Columns.pas

  Description: Contains the classes for the columns used in the Report.

  Model:       AdvancedReportFiles.mpb

  Created:     June 2004

  Last revision information:
    $Revision: 5 $
    $Date: 26/07/06 9:41 $
    $Author: Johnvanbreda $
===============================================================================}

unit CRColumns;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, SMIBase, SMI2TXT,
  Forms, Dialogs, XMLIntf, XMLDoc, DB, ExceptionForm, DBClient, Grids;

type
  EReportColumnsException = class(TExceptionPath)
  end;

  TReportColumn = class(TObject)
  private
    FCaption: String;
    FKeyColumn: String;
    FName: String;
    FPosition: Integer;
    FTableName: String;
    FTableField: String;
    FVisible: Boolean;
    FWidth: Integer;
    function GetCanNavigate: Boolean;
    procedure Validate;
  public
    procedure ReadXML(ANode: IXMLNode);
    property CanNavigate: Boolean read GetCanNavigate;
    property Caption: String read FCaption;
    property KeyColumn: String read FKeyColumn;
    property Name: String read FName;
    property Position: Integer read FPosition;
    property TableName: String read FTableName;
    property TableField: String read FTableField;
    property Visible: Boolean read FVisible;
    property Width: Integer read FWidth;
  end;

  TReportColumns = class(TList)
  private
    function GetItems(AIndex: Integer): TReportColumn;
    procedure SetItems(AIndex: Integer; AItem: TReportColumn);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItem: TReportColumn): Integer;
    procedure Clear; override;
    function ColumnByName(const AName: string): TReportColumn;
    function Extract(AItem: TReportColumn): TReportColumn;
    function First: TReportColumn;
    function IndexOf(AItem: TReportColumn): Integer;
    procedure Insert(AIndex: Integer; AItem: TReportColumn);
    function Last: TReportColumn;
    procedure ReadXML(ANode: IXMLNode);
    function Remove(AItem: TReportColumn): Integer;
    property Items[AIndex: Integer]: TReportColumn read GetItems write SetItems; default;
  end;
  
implementation

uses
  CRConstants, CRCommonClasses, ResourceStrings;

{-==============================================================================
    TReportColumns
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TReportColumns.Create;
begin
  inherited Create;

end;  // TReportColumns.Create 

{-------------------------------------------------------------------------------
}
destructor TReportColumns.Destroy;
begin
  Clear;
  inherited Destroy;
end;  // TReportColumns.Destroy 

{-------------------------------------------------------------------------------
}
function TReportColumns.Add(AItem: TReportColumn): Integer;
begin
  Result := inherited Add(Pointer(AItem));
end;  // TReportColumns.Add 

{-------------------------------------------------------------------------------
}
procedure TReportColumns.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
      Items[I].Free;
      Delete(I);
  end;
  
  inherited Clear;
end;  // TReportColumns.Clear 

{-------------------------------------------------------------------------------
  Retrieve a column using its field name.  Raises exception if not found. 
}
function TReportColumns.ColumnByName(const AName: string): TReportColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if CompareText(Items[i].Name, AName)=0 then begin
      Result := Items[i];
      Break;
    end;
  if not Assigned(Result) then
    raise EReportColumnsException.Create(Format(ResStr_ColumnNotFound, [AName]));
end;  // TReportColumns.ColumnByName

{-------------------------------------------------------------------------------
}
function TReportColumns.Extract(AItem: TReportColumn): TReportColumn;
begin
  Result := TReportColumn(inherited Extract(AItem));
end;  // TReportColumns.Extract 

{-------------------------------------------------------------------------------
}
function TReportColumns.First: TReportColumn;
begin
  Result := TReportColumn(inherited First);
end;  // TReportColumns.First 

{-------------------------------------------------------------------------------
}
function TReportColumns.GetItems(AIndex: Integer): TReportColumn;
begin
  Result := TReportColumn(inherited Items[AIndex]);
end;  // TReportColumns.GetItems 

{-------------------------------------------------------------------------------
}
function TReportColumns.IndexOf(AItem: TReportColumn): Integer;
begin
  Result := inherited IndexOf(Pointer(AItem));
end;  // TReportColumns.IndexOf 

{-------------------------------------------------------------------------------
}
procedure TReportColumns.Insert(AIndex: Integer; AItem: TReportColumn);
begin
  inherited Insert(AIndex, Pointer(AItem));
end;  // TReportColumns.Insert 

{-------------------------------------------------------------------------------
}
function TReportColumns.Last: TReportColumn;
begin
  Result := TReportColumn(inherited Last);
end;  // TReportColumns.Last 

{-------------------------------------------------------------------------------
  Reads the ReportColumns XML node for the report file. 
}
procedure TReportColumns.ReadXML(ANode: IXMLNode);
var
  i: Integer;
  lReportColumn: TReportColumn;
begin
  for i := 0 to ANode.ChildNodes.Count - 1 do begin
    if ANode.ChildNodes.Nodes[i].NodeName = EL_REPORTCOLUMN then begin
      lReportColumn := TReportColumn.Create;
      Add(lReportColumn);
      lReportColumn.ReadXML(ANode.ChildNodes.Nodes[i]);
    end
    else
      raise EReportColumnsException.CreateNonCritical(Format(ResStr_ProblemParsingSection,
          ['ReportColumns']));
  end;
end;  // TReportColumns.ReadXML 

{-------------------------------------------------------------------------------
}
function TReportColumns.Remove(AItem: TReportColumn): Integer;
begin
  Result := inherited Remove(Pointer(AItem));
end;  // TReportColumns.Remove 

{-------------------------------------------------------------------------------
}
procedure TReportColumns.SetItems(AIndex: Integer; AItem: TReportColumn);
begin
  inherited Items[AIndex] := Pointer(AItem);
end;  // TReportColumns.SetItems 

{-==============================================================================
    TReportColumn
===============================================================================}
{-------------------------------------------------------------------------------
  Returns true if this column allows the user to navigate into a Recorder screen. 
}
function TReportColumn.GetCanNavigate: Boolean;
begin
  Result := (FKeyColumn<>'') or (FTableName<>'');
end;  // TReportColumn.GetCanNavigate 

{-------------------------------------------------------------------------------
  Reads the ReportColumn XML node for the report file. 
}
procedure TReportColumn.ReadXML(ANode: IXMLNode);
begin
  with ANode do begin
    if HasAttribute(AT_CAPTION) then FCaption := Attributes[AT_CAPTION];
    if HasAttribute(AT_KEYCOLUMN) then FKeyColumn := Attributes[AT_KEYCOLUMN];
    if HasAttribute(AT_NAME) then FName := Attributes[AT_NAME];
    if HasAttribute(AT_POSITION) then FPosition := Attributes[AT_POSITION]
                                 else FPosition := -1;
    if HasAttribute(AT_TABLENAME) then FTableName := Attributes[AT_TABLENAME];
    if HasAttribute(AT_TABLEFIELD) then FTableField := Attributes[AT_TABLEFIELD];
    if HasAttribute(AT_VISIBLE) then FVisible := LowerCase(Attributes[AT_VISIBLE]) = 'true'
                                else FVisible := True;
    if HasAttribute(AT_WIDTH) then FWidth := Attributes[AT_WIDTH]
                              else FWidth := CONST_RESULTS_GRID_DEFAULT_COLUMN_WIDTH;
  end;
  
  Validate;
  
  // Navigable columns must at least specify themselves as the key value to navigate
  if (FKeyColumn='') and (FTableName<>'') then FKeyColumn := FName;
end;  // TReportColumn.ReadXML 

{-------------------------------------------------------------------------------
  Ensure that the Report Column has a name. 
}
procedure TReportColumn.Validate;
begin
  if FName = '' then
    raise EReportColumnsException.CreateNonCritical
              (Format(ResStr_ParameterMissing, ['Name', 'Column']));
end;  // TReportColumn.Validate 

end.
