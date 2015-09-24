{===============================================================================
  Unit:        ReportWizardSettings

  Defines:     TReportWizardSettings

  Description: Report Wizard shared settings

  Model:       None

  Last revision information:
    $Revision: 4 $
    $Date: 18/09/08 10:31 $
    $Author: Qingsun $

===============================================================================}

unit ReportWizardSettings;

interface

uses Classes, Sysutils, XMLDoc, XMLIntf, Contnrs,Constants;

//const
  //LOCATIONS_FOR_OCCURRENCES = ResStr_PlacesForOccurrences;
  //OCCURRENCES_FOR_LOCATIONS = ResStr_OccurrencesForPlaces;

type
  TReportWizardSettings = class
  private
    FPolygonIncludeOverlap: Boolean;
    FShowInvalid: boolean;
    FShowUnchecked: boolean;
    FShowConfidential: boolean;
    FShowZeroAbundance: boolean;
    FFilterSQL: string;
    FSnapshotFile: string;
    FTemplateFile: string;
    FWizardFile: string;
    procedure SetShowConfidential(const Value: boolean);
    procedure SetShowInvalid(const Value: boolean);
    procedure SetShowUnchecked(const Value: boolean);
    procedure SetShowZeroAbundance(const Value: boolean);
    procedure SetFilterSQL(const Value: string);
    procedure DefaultAdditionalFilterState;
    procedure SetSnapshotFile(const Value: string);
    procedure SetTemplateFile(const Value: string);
    function GetStandardReport: boolean;
    procedure SetWizardFile(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveAdditionalFilters(ANode: IXMLNode; APolygonSelection: TObjectList);
    procedure LoadAdditionalFilters(ANode: IXMLNode; APolygonSelection: TObjectList);
    property FilterSQL: string read FFilterSQL write SetFilterSQL;
    property PolygonIncludeOverlap: Boolean read FPolygonIncludeOverlap write FPolygonIncludeOverlap;
    property ShowConfidential: boolean read FShowConfidential write SetShowConfidential;
    property ShowUnchecked: boolean read FShowUnchecked write SetShowUnchecked;
    property ShowInvalid: boolean read FShowInvalid write SetShowInvalid;
    property ShowZeroAbundance: boolean read FShowZeroAbundance write SetShowZeroAbundance;
    property TemplateFile : string read FTemplateFile write SetTemplateFile;
    property SnapshotFile : string read FSnapshotFile write SetSnapshotFile;
    property StandardReport: boolean read GetStandardReport;
    property WizardFile: string read FWizardFile write SetWizardFile;
  end;

implementation

uses
  ReportGenerator;

const
  EL_ADDITIONAL_FILTERS = 'additional_filters';
  EL_CONFIDENTIAL       = 'confidential';
  EL_UNCHECKED          = 'unchecked';
  EL_INVALID            = 'invalid';
  EL_ZERO_ABUNDANCE     = 'zero_abundance';
  EL_POLYGON            = 'polygon';
  EL_POLYGON_OVERLAP    = 'polygonOverlap';

  AT_STATE              = 'state';
  AT_MAP_KEY            = 'mapKey';
  AT_LAYER_KEY          = 'layerKey';
  AT_ID                 = 'id';

{ TReportWizardSettings }

{-------------------------------------------------------------------------------
}
constructor TReportWizardSettings.Create;
begin
  inherited;
  DefaultAdditionalFilterState;
  FFilterSQL := '';
end;

{-------------------------------------------------------------------------------
}
destructor TReportWizardSettings.Destroy;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Set default additional filter state
}
procedure TReportWizardSettings.DefaultAdditionalFilterState;
begin
  FShowConfidential := False;
  FShowInvalid := False;
  FShowUnchecked := False;
  FShowZeroAbundance := False;
end;

{-------------------------------------------------------------------------------
  Read the addition filter state from the XML document.
}
procedure TReportWizardSettings.LoadAdditionalFilters(ANode: IXMLNode; APolygonSelection: TObjectList);
var
  lNode: IXMLNode;
begin
  DefaultAdditionalFilterState;
  lNode := ANode.ChildNodes[EL_ADDITIONAL_FILTERS];
  if Assigned(lNode) then begin
    if Assigned(lNode.ChildNodes[EL_CONFIDENTIAL]) then
      FShowConfidential := lNode.ChildNodes[EL_CONFIDENTIAL].Attributes[AT_STATE]='1';
    if Assigned(lNode.ChildNodes[EL_UNCHECKED]) then
      FShowUnchecked := lNode.ChildNodes[EL_UNCHECKED].Attributes[AT_STATE]='1';
    if Assigned(lNode.ChildNodes[EL_INVALID]) then
      FShowInvalid := lNode.ChildNodes[EL_INVALID].Attributes[AT_STATE]='1';
    if Assigned(lNode.ChildNodes[EL_ZERO_ABUNDANCE]) then
      FShowZeroAbundance := lNode.ChildNodes[EL_ZERO_ABUNDANCE].Attributes[AT_STATE]='1';

    if Assigned(lNode.ChildNodes[EL_POLYGON_OVERLAP]) then
      FPolygonIncludeOverlap := lNode.ChildNodes[EL_POLYGON_OVERLAP].Attributes[AT_STATE] = '1';

    lNode := ANode.ChildNodes[EL_ADDITIONAL_FILTERS].ChildNodes.First;
    while Assigned(lNode) do begin
      if SameText(lNode.NodeName, EL_POLYGON) then
        APolygonSelection.Add(TPolygonInfo.Create(
            lNode.Attributes[AT_MAP_KEY],
            lNode.Attributes[AT_LAYER_KEY],
            StrToInt(lNode.Attributes[AT_ID])));
      lNode := lNode.NextSibling;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Persist the addition filter state to the XML document.
}
procedure TReportWizardSettings.SaveAdditionalFilters(ANode: IXMLNode; APolygonSelection: TObjectList);
var
  lParentNode: IXMLNode;
  lSubNode: IXMLNode;
  i: Integer;
  polygonInfo: TPolygonInfo;
begin
  lParentNode := ANode.AddChild(EL_ADDITIONAL_FILTERS);
  lSubNode := lParentNode.AddChild(EL_CONFIDENTIAL);
  if FShowConfidential then
    lSubNode.Attributes[AT_STATE]:='1'
  else
    lSubNode.Attributes[AT_STATE]:='0';
  lSubNode := lParentNode.AddChild(EL_UNCHECKED);
  if FShowUnchecked then
    lSubNode.Attributes[AT_STATE]:='1'
  else
    lSubNode.Attributes[AT_STATE]:='0';
  lSubNode := lParentNode.AddChild(EL_INVALID);
  if FShowInvalid then
    lSubNode.Attributes[AT_STATE]:='1'
  else
    lSubNode.Attributes[AT_STATE]:='0';
  lSubNode := lParentNode.AddChild(EL_ZERO_ABUNDANCE);
  if FShowZeroAbundance then
    lSubNode.Attributes[AT_STATE]:='1'
  else
    lSubNode.Attributes[AT_STATE]:='0';

  if APolygonSelection.Count > 0 then begin
    lSubNode := lParentNode.AddChild(EL_POLYGON_OVERLAP);
    if FPolygonIncludeOverlap then lSubNode.Attributes[AT_STATE] := '1'
                              else lSubNode.Attributes[AT_STATE] := '0';

    for i := 0 to APolygonSelection.Count - 1 do begin
      polygonInfo := TPolygonInfo(APolygonSelection[i]);

      lSubNode := lParentNode.AddChild(EL_POLYGON);
      lSubNode.Attributes[AT_MAP_KEY]   := polygonInfo.MapKey;
      lSubNode.Attributes[AT_LAYER_KEY] := polygonInfo.LayerKey;
      lSubNode.Attributes[AT_ID]        := polygonInfo.PolygonId;
    end;
  end;
end;

{-------------------------------------------------------------------------------
}
procedure TReportWizardSettings.SetFilterSQL(const Value: string);
begin
  FFilterSQL := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TReportWizardSettings.SetShowConfidential(const Value: boolean);
begin
  FShowConfidential := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TReportWizardSettings.SetShowInvalid(const Value: boolean);
begin
  FShowInvalid := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TReportWizardSettings.SetShowUnchecked(const Value: boolean);
begin
  FShowUnchecked := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TReportWizardSettings.SetShowZeroAbundance(const Value: boolean);
begin
  FShowZeroAbundance := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TReportWizardSettings.SetSnapshotFile(const Value: string);
begin
  FSnapshotFile := Value;
end;

{-------------------------------------------------------------------------------
}
procedure TReportWizardSettings.SetTemplateFile(const Value: string);
begin
  FTemplateFile := Value;
end;

{-------------------------------------------------------------------------------
  Determine if one of the 2 standard reports are being used
}
function TReportWizardSettings.GetStandardReport: boolean;
begin
  Result := (WizardFile = ResStr_PlacesForOccurrences) or (WizardFile = ResStr_OccurrencesForPlaces);
end;

{-------------------------------------------------------------------------------
}
procedure TReportWizardSettings.SetWizardFile(const Value: string);
begin
  FWizardFile := Value;
end;

end.
