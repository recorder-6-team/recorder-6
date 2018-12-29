unit HierarchyNodes;

interface

uses
  Classes, SysUtils, ComCtrls, Graphics, DataClasses, RapTree, GeneralData,
  Recorder2000_TLB, CRCommonClasses;

type
  ENodeObjectError = class(Exception);

  //==============================================================================
  // class holding key and additional data and indication of whether data is system supplied
  TKeyDataSysSupplied=class(TKeyData)
  private
    FSysSupplied : Boolean;
  protected
    procedure SetSystemSupplied(const iSysSupplied: Boolean);
  public
    property SysSupplied : boolean read FSysSupplied write SetSystemSupplied default false;
  end;

  //==============================================================================
  TNodeObject=class(TKeyDataSysSupplied)
  private
    FText : string;
    FHasImage : boolean;
    FHasState : boolean;
    FImageIndex : integer;
    FStateImage : integer;
    FCanDrag : boolean;
    FCanDropOn : boolean;
    FChildrenPopulated : boolean;
    FHint: String;
    FIsFiltered: Boolean;
  protected
    { abstract methods }
//    procedure DrawNodeText(iStyle, iSize); virtual; abstract;
    { Accessor methods }
    procedure SetChildrenPopulated(const iChildrenPopulated: boolean);
    procedure SetCanDropOn(const iCanDropOn: boolean);
    procedure SetCanDrag(const iCanDrag: boolean);
    procedure SetHasImage(const iHasImage: boolean);
    procedure SetHasState(const iHasState: boolean);
    procedure SetImageIndex(const iImageIndex: integer); virtual;
    procedure SetStateImage(const iStateImage: integer); virtual;
    procedure SetText(const iText: string);
  public
    property Text : string read FText write SetText;
    property HasImage : boolean read FHasImage write SetHasImage;
    property HasState : boolean read FHasState write SetHasState;
    property ImageIndex : integer read FImageIndex write SetImageIndex;
    property StateImage : integer read FStateImage write SetStateImage;
    property CanDrag : boolean read FCanDrag write SetCanDrag;
    property CanDropOn : boolean read FCanDropOn write SetCanDropOn;
    property ChildrenPopulated : boolean read FChildrenPopulated write SetChildrenPopulated;
    property Hint: String read FHint write FHint;
    property IsFiltered: Boolean read FIsFiltered write FIsFiltered;

    constructor Create;
    destructor Destroy; override;
  end;

  //==============================================================================
  // Nodes that allow custom reports to be generated
  TReportableNode = class(TNodeObject)
  public
    function ReportKeyType: TKeyType; virtual;
  end;

  //==============================================================================
  TTermListNode = class(TNodeObject)
  private
    FShortName : String;
    FLongName : String;
    FDescription : String;
  public
    property ShortName : String read FShortName write FShortName;
    property LongName : String read FLongName write FLongName;
    property Description : String read FDescription write FDescription;
  end;

  //==============================================================================
	TObservationNode = class(TReportableNode)
  private
    FHasLocation : Boolean;
  protected
    property HasLocation : Boolean read FHasLocation write FHasLocation;
  end;

	TNameNode = class(TReportableNode)
  public
    function ReportKeyType: TKeyType; override;
  end;

  //==============================================================================
  EDictionaryNode = class(ENodeObjectError);

  TDictionaryNode=class(TReportableNode)
  private
    FItalic : boolean;
    FSortCode : LongInt;
    FRankKey : TKeyString;
  protected
    procedure SetItalic(const iItalic: boolean);
    procedure SetSortCode(const iSortCode : LongInt);
    procedure SetRankKey(const iRank: TKeyString); virtual;
  public
    property Italic : boolean read FItalic write SetItalic;
    property SortCode : LongInt read FSortCode write SetSortCode;
    property RankKey : TKeyString read FRankKey write SetRankKey;
  end;

  //==============================================================================
  THTMLDictionaryNode=class(TDictionaryNode)
  protected
    function GetTitle : string; virtual; abstract;
    function GetDetailTitle : string; virtual; abstract;
    function GetAssociatedTitle: string; virtual; abstract;
    function GetAssociatedSourcesTitle: string; virtual; abstract;
    function GetMainSourcesTitle: string; virtual; abstract;
  public
    property Title : string read GetTitle;
    property DetailTitle : string read GetDetailTitle;
    property AssociatedTitle : string read GetAssociatedTitle;
    property AssociatedSourcesTitle : string read GetAssociatedSourcesTitle;
    property MainSourcesTitle : string read GetMainSourcesTitle;
  end;

  //==============================================================================
  TSurveyTagNode = class(TObservationNode)
  public
    constructor Create;
  end;

  //==============================================================================
  TSurveyNode=class(TObservationNode)
  private
    FSelected: boolean;
    FParentKey: TKeyString;
    procedure SetParentKey(const Value: TKeyString);
    procedure SetSelected(const Value: boolean);
  public
    constructor Create;
    function ReportKeyType: TKeyType; override;
    {Next two properties added for use in the Wizard - GAD 27/5/99}
    property Selected: boolean read FSelected write SetSelected;
    property ParentKey: TKeyString read FParentKey write SetParentKey;
  end;

  //==============================================================================
  TEventNode=class(TObservationNode)
  public
    constructor Create;
    function ReportKeyType: TKeyType; override;
    property HasLocation;
  end;

  //==============================================================================
  // base type helper class
  TBaseType=class(TObject)
  private
    FItemKey : TKeyString;
    FImageIndex : integer;
    FShortName: string;
  protected
    procedure SetItemKey(const iItemKey: TKeyString);
    procedure SetImageIndex(const iImageIndex: integer);
  public
    property ItemKey : TKeyString read FItemKey write SetItemKey;
    property ImageIndex : integer read FImageIndex write SetImageIndex;
    property ShortName : string read FShortName write FShortname;
  end;

  //==============================================================================
  // Sample helper class
  TSampleType=class(TBaseType);

  //==============================================================================
  // Taxon helper class
  TTaxonType=class(TBaseType)
  private
    FSequence: Integer;
    FItalicFlag: Boolean;
  protected
    procedure SetSequence(const iValue: Integer);
    procedure SetItalicFlag(const iValue: Boolean);
  public
    property Sequence : Integer read FSequence write SetSequence;
    property ItalicFlag : Boolean read FItalicFlag write SetItalicFlag;
  end;

  //==============================================================================
  // Biotope helper class
  TBiotopeType=class(TBaseType)
  private
    FItalicFlag: Boolean;
  protected
    procedure SetItalicFlag(const iValue: Boolean);
  public
    property ItalicFlag : Boolean read FItalicFlag write SetItalicFlag;
  end;

  //==============================================================================
  // Type helper list class
  TTypeList = class(TList)
  public
    function FindKey(iKey : TKeyString) : TBaseType;
    function GetStrings : TStrings;
    destructor Destroy; override;
  end;

  //==============================================================================
  // Type helper list class
  TSampleTypeList = class(TTypeList);

  //==============================================================================
  // Type helper list class
  TTaxonTypeList = class(TTypeList)
  public
    function GetLowerRanks(iKey: TKeyString): TStrings;
    function GetMaxSequence : Integer;
  end;

  //==============================================================================
  // Type helper list class
  TBiotopeTypeList = class(TTypeList);

  //==============================================================================
  // Enumerated record card type
  TRecordCardType=(rcNone, rcUnchecked, rcChecked);

  TSampleNode=class(TObservationNode)
  private
    FOutstandingCard : TRecordCardType;
    FSampleType : TSampleType;
  protected
    procedure SetOutstandingCard(const iOutstandingCard: TRecordCardType);
    procedure SetSampleType(const iSampleType: TSampleType);
  public
    constructor Create;
    destructor Destroy; override;
    function ReportKeyType: TKeyType; override;
    property OutstandingCard : TRecordCardType read FOutstandingCard write SetOutstandingCard;
    property SampleType : TSampleType read FSampleType write SetSampleType;
    property HasLocation;
  end;

  //==============================================================================
  TOccurrenceNode=class(TObservationNode)
  private
    FChecked   :boolean;
    FParentNode:TFlyNode;   // reference to Sample tree node, so Sample State can be updated
  protected
    procedure SetChecked(const iChecked: boolean);
    procedure SetParentNode(const iNode:TFlyNode);
  public
    constructor Create;
    property Checked : boolean read FChecked write SetChecked;
    property ParentNode:TFlynode read FParentNode write SetParentNode;
    property HasLocation;
  end;

  //==============================================================================
  TTaxonOccNode=class(TOccurrenceNode)
  private
    FConfidential : boolean;
    FZeroAbundance :boolean;
    FVerified :integer;
    FTaxonNameObject : TTaxonNames;
    procedure SetTaxonNameObject(const Value: TTaxonNames);
  protected
    procedure SetConfidential(const Value : boolean);
    procedure SetZeroAbundance (const Value : boolean);
    procedure SetVerified (const Value : integer);

  public
    constructor Create;
    destructor Destroy; override;
    function ReportKeyType: TKeyType; override;
    property TaxonNameObject : TTaxonNames  read FTaxonNameObject write SetTaxonNameObject;
    property Confidential : boolean read FConfidential write SetConfidential;
    property ZeroAbundance :boolean read FZeroAbundance write SetZeroAbundance;
    property Verified :integer read FVerified write SetVerified;
  end;

  //==============================================================================
  TBiotopeOccNode=class(TOccurrenceNode)
  public
    constructor Create;
    function ReportKeyType: TKeyType; override;
  end;

  //==============================================================================
  TAddinLocNode=class(TNodeObject)
  private
    FDeletable: Boolean;
    FEditable: Boolean;
    FIsSubFolder: Boolean;
    FTypeID: Integer;
  public
    property Deletable: Boolean read FDeletable write FDeletable;
    property Editable: Boolean read FEditable write FEditable;
    property IsSubFolder: Boolean read FIsSubFolder write FIsSubFolder;
    property TypeID: Integer read FTypeID write FTypeID;
  end;

  //==============================================================================
  TAddinOccNode=class(TOccurrenceNode)
  private
    FConfidential: Boolean;
    FDeletable: Boolean;
    FEditable: Boolean;
    FIsSubFolder: Boolean;
    FTypeID: Integer;
    procedure SetConfidential(const Value: Boolean);
  public
    property Confidential: Boolean read FConfidential write SetConfidential;
    property Deletable: Boolean read FDeletable write FDeletable;
    property Editable: Boolean read FEditable write FEditable;
    property IsSubFolder: Boolean read FIsSubFolder write FIsSubFolder;
    property TypeID: Integer read FTypeID write FTypeID;
  end;

  //==============================================================================
  TSiteNode=class(TReportableNode)
  private
    FSpatialRef: string;
    FFileCode: string;
    procedure SetFileCode(const Value: string);
    procedure SetSpatialRef(const Value: string);
  public
    constructor Create;
    function ReportKeyType: TKeyType; override;
    property FileCode: string read FFileCode write SetFileCode;
    property SpatialRef: string read FSpatialRef write SetSpatialRef;
  end;

  //==============================================================================
  TFeatureNode=class(TNodeObject)
  public
    constructor Create;
  end;

  //==============================================================================
  ETaxonDictionaryNode = class(EDictionaryNode);
  ERankNotFound = class(ETaxonDictionaryNode);

  TTaxonDictionaryNode=class(THTMLDictionaryNode)
  private
    FAuthority : string;
    FItemName : string;
    FSequence : Integer;
    FTaxonKey : TKeyString;
    FTaxonVersionKey : TKeyString;
    FValidationLevel : LongInt;
    FPrefNameKey : TKeyString;
    FRankKey: TKeyString;
    FDisplayRank: boolean;
    FRankName: string;
    FCheckedCommonName : boolean;
    FTaxonNames : TTaxonNames;
  protected
    function SetBrackets(const iStr : string) : string;
    function GetDetailTitle : string; override;
    function GetCommonName: string;
    function GetTaxonNamesObject: TTaxonNames;
    procedure SetTaxonNamesObject(const Value: TTaxonNames);
    procedure SetAuthority(const iAuthority: string);
    procedure SetItemName(const iItemName: string);
    procedure SetRankKey(const iRank: TKeyString); override;
    procedure SetDisplayRank(const Value: boolean);
    procedure SetRankName(const Value: string);
    procedure SetTaxonKey(const iTaxonKey: TKeyString);
    procedure SetTaxonVersionKey(const iTaxonVersionKey: TKeyString);
    function GetImageIndex : Integer;
    function GetStateImage : Integer;
    procedure SetImageIndex(const Value: integer); override;
    procedure SetStateImage(const Value: integer); override;
    function GetPrefNameKey : TKeyString;
    procedure SetPrefNameKey(const Value: TKeyString);
    function GetTitle : string; override;

    function GetAssociatedTitle: string; override;
    function GetAssociatedSourcesTitle: string; override;
    function GetMainSourcesTitle: string; override;
  public
    constructor Create(AObject: TObject);
    destructor Destroy; override;
    function ReportKeyType: TKeyType; override;
    procedure ClearNamesObject;
    procedure RefreshNamesObjects;
    property Authority : string read FAuthority write SetAuthority;
    property CommonName : string read GetCommonName;
    property TaxonNamesObject : TTaxonNames read GetTaxonNamesObject write SetTaxonNamesObject;
    property ItemName : string read FItemName write SetItemName;
    property Title : string read GetTitle;
    property RankKey : TKeyString read FRankKey write SetRankKey;
    property DisplayRank : boolean read FDisplayRank write SetDisplayRank;
    property RankName : string read FRankName write SetRankName;
    property TaxonKey : TKeyString read FTaxonKey write SetTaxonKey;
    property TaxonVersionKey : TKeyString read FTaxonVersionKey write SetTaxonVersionKey;
    property Sequence : integer read FSequence;
    property PrefNameKey : TKeyString read GetPrefNameKey write SetPrefNameKey;

    property ImageIndex : integer read GetImageIndex write SetImageIndex;
    property StateImage : integer read GetStateImage write SetStateImage;

    property AssociatedTitle : string read GetAssociatedTitle;
    property AssociatedSourcesTitle : string read GetAssociatedSourcesTitle;
    property MainSourcesTitle : string read GetMainSourcesTitle;
  end;

  //==============================================================================
  TBiotopeDictionaryNode=class(THTMLDictionaryNode)
  private
    FOriginalCode : string;
    FShortTerm : string;
    FLongName : string;
    FBiotopeKey : string;
  protected
    procedure SetOriginalCode(const Value: string);
    procedure SetLongName(const Value: string);
    procedure SetShortTerm(const Value: string);
    procedure SetBiotopeKey(const Value: string);

    function GetDetailTitle : string; override;
    function GetTitle : string; override;

    function GetAssociatedTitle: string; override;
    function GetAssociatedSourcesTitle: string; override;
    function GetMainSourcesTitle: string; override;
  public
    function ReportKeyType: TKeyType; override;
    property OriginalCode : string read FOriginalCode write SetOriginalCode;
    property LongName : string read FLongName write SetLongName;
    property ShortTerm : string read FShortTerm write SetShortTerm;
    property BiotopeKey : string read FBiotopeKey write SetBiotopeKey;
    property Title : string read GetTitle;
    property DetailTitle : string read GetDetailTitle;
    property AssociatedTitle : string read GetAssociatedTitle;
    property AssociatedSourcesTitle : string read GetAssociatedSourcesTitle;
    property MainSourcesTitle : string read GetMainSourcesTitle;
  end;

  //==============================================================================
  TAdminAreaDictionaryNode=class(THTMLDictionaryNode)
  private
    FItemName  : string;
    FShortCode : string;
  protected
    procedure SetShortCode(const Value : string);
    procedure SetItemName (const Value : string);
  public
    function ReportKeyType: TKeyType; override;
    property ItemName : string read FItemName write SetItemName;
    property ShortCode : string read FShortCode write SetShortCode;
  end;

  //==============================================================================

const
  { TNodeObject }

  { default values }
  DEF_HASIMAGE = True;
  DEF_HASSTATE = False;
  DEF_CANDRAG = True;
  DEF_CANDROPON = False;
  DEF_IMAGEINDEX = 0;
  DEF_STATEIMAGE = 0;
  DEF_CHILDRENPOPULATED = False;

  { non selections }
  NO_IMAGE = -1;
  NO_STATE = 0;

  { TSurveyTagNode }
  DEF_SURVEY_TAG_IMAGE = 3;

  { TSurveyNode }
  DEF_SURVEY_IMAGE = 0;

  { TEventNode }
  DEF_EVENT_IMAGE = 1;

  { TSampleNode }
  DEF_SAMPLE_IMAGE = 22;
  SAMPLE_STATE_CARD_CHECKED = 3;  // sample state indexes
  SAMPLE_STATE_CARD_UNCHECKED = 4;
  SAMPLE_STATE_CARD_NONE = 5;
  DEF_OUTSTANDINGCARD = rcNone;

  { TOccurrenceNode }
  DEF_CHECKED = False;
  OCC_STATE_CHECKED = 2;   // occurrence state indexes
  OCC_STATE_UNCHECKED = 1;

  { TTaxonOccNode }
  DEF_TAXONOCC_IMAGE = 5;
  DEF_CONFIDENTIAL = False;
  DEF_ZEROABUNDANCE = False;
  DEF_VERIFIED = 0;

  { TBiotopeOccNode }
  DEF_BIOTOPEOCC_IMAGE = 18;

  { TSiteNode }
  DEF_SITE_IMAGE = 0;

  { TFeatureNode }
  DEF_FEATURE_IMAGE = 5;

  { TTaxonDictionaryNode }
  INT_CRITICAL_VALIDATION_LEVEL = 3;

//==============================================================================
implementation

uses
  FormActions;

resourcestring
  ResStr_CannotSetImage = 'You cannot set the ImageIndex property if the HasImage property is false.';
  ResStr_CannotSetState = 'You cannot set the StateImage property if the HasState property is false.';
  ResStr_AssocBiotopes = 'Associated Biotopes';
  ResStr_AssocBiotopeSource = 'Associated Biotope Sources';
  ResStr_TaxonSource =  'Taxon Sources';
  ResStr_AssocTaxonSource = 'Associated Taxon Sources';
  ResStr_AssocTaxa =  'Associated Taxa';
  ResStr_BiotopeSource =  'Biotope Sources';

//==============================================================================
{ TNodeObject }
constructor TNodeObject.Create;
begin
  inherited Create;
  FHasImage          := DEF_HASIMAGE;
  FImageIndex        := DEF_IMAGEINDEX;
  FHasState          := DEF_HASSTATE;
  FStateImage        := NO_STATE;
  FCanDrag           := DEF_CANDRAG;
  FCanDropOn         := DEF_CANDROPON;
  FChildrenPopulated := DEF_CHILDRENPOPULATED;
  FHint              := '';
  FIsFiltered        := False; 
end;

//------------------------------------------------------------------------------
destructor TNodeObject.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TNodeObject.SetChildrenPopulated(const iChildrenPopulated: boolean);
begin
  FChildrenPopulated := iChildrenPopulated;
end;

//------------------------------------------------------------------------------
procedure TNodeObject.SetCanDropOn(const iCanDropOn: boolean);
begin
  FCanDropOn := iCanDropOn;
end;

//------------------------------------------------------------------------------
procedure TNodeObject.SetCanDrag(const iCanDrag: boolean);
begin
  FCanDrag := iCanDrag;
end;

//------------------------------------------------------------------------------
procedure TKeyDataSysSupplied.SetSystemSupplied(const iSysSupplied: Boolean);
begin
  FSysSupplied := iSysSupplied;
end;

//------------------------------------------------------------------------------
procedure TNodeObject.SetHasImage(const iHasImage: boolean);
begin
  FHasImage := iHasImage;
  if not FHasImage then
    FImageIndex := NO_IMAGE
  else
    FImageIndex := DEF_IMAGEINDEX;
end;

//------------------------------------------------------------------------------
procedure TNodeObject.SetHasState(const iHasState: boolean);
begin
  FHasState := iHasState;
  if not FHasState then
    FStateImage := NO_STATE
  else
    FStateImage := DEF_STATEIMAGE;
end;

//------------------------------------------------------------------------------
procedure TNodeObject.SetImageIndex(const iImageIndex: integer);
begin
  if FHasImage then
    FImageIndex := iImageIndex
  else
    Raise ENodeObjectError.Create(ResStr_CannotSetImage);
end;

//------------------------------------------------------------------------------
procedure TNodeObject.SetStateImage(const iStateImage: integer);
begin
  if FHasState then
    FStateImage := iStateImage
  else
    Raise ENodeObjectError.Create(ResStr_CannotSetState);
end;

//------------------------------------------------------------------------------
procedure TNodeObject.SetText(const iText: string);
begin
  FText := iText;
end;

//==============================================================================
{ Survey Tag Node}

constructor TSurveyTagNode.Create;
begin
  inherited Create;
  CanDrag := False;
  ImageIndex := DEF_SURVEY_TAG_IMAGE;
end;

//==============================================================================
{ Survey Node}

constructor TSurveyNode.Create;
begin
  inherited Create;
  CanDrag := False;
  ImageIndex := DEF_SURVEY_IMAGE;
end;

//------------------------------------------------------------------------------
function TSurveyNode.ReportKeyType: TKeyType;
begin
  Result := ktSurvey;
end;

//------------------------------------------------------------------------------
procedure TSurveyNode.SetParentKey(const Value: TKeyString);
begin
  FParentKey := Value;
end;

//------------------------------------------------------------------------------
procedure TSurveyNode.SetSelected(const Value: boolean);
begin
  FSelected := Value;
end;

//==============================================================================
{ Event Node}

constructor TEventNode.Create;
begin
  inherited Create;
  ImageIndex := DEF_EVENT_IMAGE;
end;

//------------------------------------------------------------------------------
function TEventNode.ReportKeyType: TKeyType;
begin
  Result := ktSurveyEvent;
end;

//==============================================================================
{ Sample Node}

constructor TSampleNode.Create;
begin
  inherited Create;
  CanDropOn       := True;
  ImageIndex      := DEF_SAMPLE_IMAGE;
  HasState        := True;
  OutstandingCard := DEF_OUTSTANDINGCARD;
  FSampleType     :=TSampleType.Create;
end;  // Create

//------------------------------------------------------------------------------
destructor TSampleNode.Destroy;
begin
  FSampleType.Free;
  inherited Destroy;
end;  // Destroy

//------------------------------------------------------------------------------
function TSampleNode.ReportKeyType: TKeyType;
begin
  Result := ktSample;
end;

//------------------------------------------------------------------------------
procedure TSampleNode.SetOutstandingCard(const iOutstandingCard: TRecordCardType);
begin
  FOutstandingCard := iOutstandingCard;
  case FOutstandingCard of
    rcNone      : StateImage := SAMPLE_STATE_CARD_NONE;
    rcUnchecked : StateImage := SAMPLE_STATE_CARD_UNCHECKED;
    rcChecked   : StateImage := SAMPLE_STATE_CARD_CHECKED;
  end;
end;  // SetOutstandingCard

//------------------------------------------------------------------------------
procedure TSampleNode.SetSampleType(const iSampleType: TSampleType);
begin
  FSampleType := iSampleType;
end;  // SetSampleType

//==============================================================================
{ Sample Type }

procedure TBaseType.SetItemKey(const iItemKey: TKeyString);
begin
  FItemKey := iItemKey;
end;

//------------------------------------------------------------------------------
procedure TBaseType.SetImageIndex(const iImageIndex: integer);
begin
  FImageIndex := iImageIndex;
end;

//==============================================================================
{ Occurrence Node }

constructor TOccurrenceNode.Create;
begin
  inherited Create;
  HasState := True;
  Checked := DEF_CHECKED;
  ParentNode:=nil;
end;  // Create

//------------------------------------------------------------------------------
procedure TOccurrenceNode.SetChecked(const iChecked: boolean);
var iIndex      :integer;
    tfAllChecked:boolean;
begin
  FChecked := iChecked;
  if FChecked then StateImage := OCC_STATE_CHECKED
              else StateImage := OCC_STATE_UNCHECKED;

  // Update Sample node state if Parent assigned
  if FParentNode<>nil then
    if TSampleNode(FParentNode.Data).OutstandingCard<>rcNone then begin
      tfAllChecked:=FChecked;
      if FParentNode.HasChildren then
        for iIndex:=0 to FParentNode.Count-1 do
          tfAllChecked:=tfAllChecked and TOccurrenceNode(FParentNode.Item[iIndex].Data).Checked;
      if tfAllChecked then
        TSampleNode(FParentNode.Data).OutstandingCard:=rcChecked
      else
        TSampleNode(FParentNode.Data).OutstandingCard:=rcUnchecked;
      FParentNode.StateIndex:=TSampleNode(FParentNode.Data).StateImage;
    end;
end;  // SetChecked

//------------------------------------------------------------------------------
procedure TOccurrenceNode.SetParentNode(const iNode:TFlyNode);
begin
  FParentNode:=iNode;
end;  // SetParentNode

//==============================================================================
{ Taxon Occurrence Node }

constructor TTaxonOccNode.Create;
begin
  inherited Create;
  ImageIndex := DEF_TAXONOCC_IMAGE;
  Confidential := DEF_CONFIDENTIAL;
  ZeroAbundance := DEF_ZEROABUNDANCE;
  FTaxonNameObject := nil;
end;

//------------------------------------------------------------------------------
destructor TTaxonOccNode.Destroy;
begin
  FTaxonNameObject.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TTaxonOccNode.ReportKeyType: TKeyType;
begin
  Result := ktTaxonOccurrence;
end;

//------------------------------------------------------------------------------
procedure TTaxonOccNode.SetConfidential(const Value: boolean);
begin
  FConfidential := Value;
end;

//------------------------------------------------------------------------------
procedure TTaxonOccNode.SetZeroAbundance (const Value: boolean);
begin
  FZeroAbundance := Value;
end;

//------------------------------------------------------------------------------
procedure TTaxonOccNode.SetVerified (const Value: integer);
begin
  FVerified := Value;
end;


//==============================================================================
procedure TTaxonOccNode.SetTaxonNameObject(const Value: TTaxonNames);
begin
  FTaxonNameObject := Value;
end;


//==============================================================================
{ Biotope Occurrence Node }

//------------------------------------------------------------------------------
constructor TBiotopeOccNode.Create;
begin
  inherited Create;
  ImageIndex := DEF_BIOTOPEOCC_IMAGE;
end;

//------------------------------------------------------------------------------
function TBiotopeOccNode.ReportKeyType: TKeyType;
begin
  Result := ktBiotopeOccurrence;
end;

//==============================================================================
{ Site Node }

constructor TSiteNode.Create;
begin
  inherited Create;
  ImageIndex := DEF_SITE_IMAGE;
end;

//------------------------------------------------------------------------------
function TSiteNode.ReportKeyType: TKeyType;
begin
  Result := ktLocation;
end;

{-------------------------------------------------------------------------------
 Accessor
}
procedure TSiteNode.SetFileCode(const Value: string);
begin
  FFileCode := Value;
end;

{-------------------------------------------------------------------------------
 Accessor
}
procedure TSiteNode.SetSpatialRef(const Value: string);
begin
  FSpatialRef := Value;
end;

//==============================================================================
{ Feature Node }

constructor TFeatureNode.Create;
begin
  inherited Create;
  ImageIndex := DEF_FEATURE_IMAGE;
end;

//==============================================================================
{ Dictionary Node }

procedure TTaxonDictionaryNode.SetRankKey(const iRank: TKeyString);
var
  lRankType : TTaxonType;

    // Set up an empty rank for the node
    procedure NullRank;
    begin
      FRankKey := '';
      FImageIndex := -1;
      FStateImage := -1;
      FSequence := 0;
      fItalic := False;
    end;

begin
  if (iRank <> '') then
  begin
    lRankType := TTaxonType(dmFormActions.TaxonTypeList.FindKey(iRank)); // Rank
    if assigned(lRankType) then
    begin
      FRankKey := iRank;
      FImageIndex := lRankType.ImageIndex;
      FStateImage := lRankType.ImageIndex;
      FSequence := lRankType.Sequence;
      FItalic := lRankType.ItalicFlag;
    end
    else
      NullRank;
  end
  else
    NullRank;
end;

//------------------------------------------------------------------------------
procedure TDictionaryNode.SetItalic(const iItalic: boolean);
begin
  FItalic := iItalic;
end;

//------------------------------------------------------------------------------
procedure TDictionaryNode.SetRankKey(const iRank: TKeyString);
begin
  FRankKey := iRank;
end;

//------------------------------------------------------------------------------
procedure TDictionaryNode.SetSortCode(const iSortCode: Integer);
begin
  FSortCode :=iSortCode;
end;

//==============================================================================
{ TTaxonDictionaryNode }

procedure TTaxonDictionaryNode.SetAuthority(const iAuthority: string);
begin
  FAuthority := iAuthority;
end;

//------------------------------------------------------------------------------
function TTaxonDictionaryNode.GetTitle: string;
begin
  Result:=FItemName;
  if FAuthority<>'' then Result:=Result+' '+SetBrackets(FAuthority);
end;

//------------------------------------------------------------------------------
function TTaxonDictionaryNode.SetBrackets(const iStr : string) : string;
begin
  // Make sure there is something to return
  Result:=Trim(iStr);
  {if (Result[1]<>'(') and (Result[Length(Result)]<>')') then
    Result:='('+Result+')';}
end;

//------------------------------------------------------------------------------
procedure TTaxonDictionaryNode.SetItemName(const iItemName: string);
begin
  FItemName := iItemName;
end;

//------------------------------------------------------------------------------
function TTaxonDictionaryNode.GetAssociatedTitle: string;
begin
  Result := ResStr_AssocBiotopes;
end;

//------------------------------------------------------------------------------
function TTaxonDictionaryNode.GetAssociatedSourcesTitle: string;
begin
  Result := ResStr_AssocBiotopeSource;
end;

//------------------------------------------------------------------------------
function TTaxonDictionaryNode.GetMainSourcesTitle: string;
begin
  Result := ResStr_TaxonSource;
end;

//------------------------------------------------------------------------------
procedure TTaxonDictionaryNode.SetTaxonVersionKey(const iTaxonVersionKey: TKeyString);
begin
  FTaxonVersionKey := iTaxonVersionKey;
end;

//------------------------------------------------------------------------------
procedure TTaxonDictionaryNode.SetTaxonKey(const iTaxonKey: TKeyString);
begin
  FTaxonKey := iTaxonKey;
end;

//------------------------------------------------------------------------------
function TTaxonDictionaryNode.GetImageIndex: Integer;
begin
  Result:=FImageIndex;
end;

//------------------------------------------------------------------------------
function TTaxonDictionaryNode.GetStateImage: Integer;
begin
  if FValidationLevel=INT_CRITICAL_VALIDATION_LEVEL then
    Result:= 1
  else
    Result:=-1;
end;

//------------------------------------------------------------------------------
procedure TTaxonDictionaryNode.SetImageIndex(const Value: integer);
begin
  inherited SetImageIndex(Value);
end;

//------------------------------------------------------------------------------
procedure TTaxonDictionaryNode.SetStateImage(const Value: integer);
begin
  inherited SetStateImage(Value);
end;

//------------------------------------------------------------------------------
//==============================================================================
function TTaxonDictionaryNode.GetPrefNameKey: TKeyString;
begin
  Result := FPrefNameKey;
end;

procedure TTaxonDictionaryNode.SetPrefNameKey(const Value: TKeyString);
begin
  FPrefNameKey := Value;
end;

procedure TTaxonDictionaryNode.SetDisplayRank(const Value: boolean);
begin
  FDisplayRank := Value;
end;

procedure TTaxonDictionaryNode.SetRankName(const Value: string);
begin
  FRankName := Value;
end;


//==============================================================================
constructor TTaxonDictionaryNode.Create(AObject: TObject);
begin
  inherited Create;
  FTaxonNames := nil;
  FCheckedCommonName := False; // flag indicating we must fetch on demand
end;


//==============================================================================
function TTaxonDictionaryNode.GetCommonName: string;
begin
  if TaxonNamesObject = nil then
    Result := FItemName
  else
    Result := FTaxonNames.CommonName;
end;

//==============================================================================
function TTaxonDictionaryNode.GetTaxonNamesObject: TTaxonNames;
begin
  if not Assigned(FTaxonNames) then  // build the object only if necessary
    FTaxonNames := dmGeneralData.GetTaxonNamesObject( ItemKey );
  Result := FTaxonNames;
end;  // GetTaxonNamesObject

//------------------------------------------------------------------------------
procedure TTaxonDictionaryNode.SetTaxonNamesObject(const Value: TTaxonNames);
begin
  if Assigned(FTaxonNames) and (Value = nil) then
    FTaxonNames.Free
  else
    FTaxonNames := Value;
end;  // SetTaxonNamesObject

//------------------------------------------------------------------------------
procedure TTaxonDictionaryNode.ClearNamesObject;
begin
  FTaxonNames.Free;
  FTaxonNames := nil;
end;  // ClearNamesObject

//------------------------------------------------------------------------------
procedure TTaxonDictionaryNode.RefreshNamesObjects;
begin
  ClearNamesObject;     // Clear existing stuff first
  GetTaxonNamesObject;  // Now get the new/updated stuff
end;  // RefreshNamesObject

//==============================================================================
function TTaxonDictionaryNode.GetDetailTitle: string;
begin
  Result := CommonName;
end;

//==============================================================================
destructor TTaxonDictionaryNode.Destroy;
begin
  ClearNamesObject;  // if we have created it
  inherited;
end;

//==============================================================================
function TTaxonDictionaryNode.ReportKeyType: TKeyType;
begin
  Result := ktTaxon;
end;

//==============================================================================
{ TBiotopeDictionaryNode }

procedure TBiotopeDictionaryNode.SetOriginalCode(const Value: string);
begin
  FOriginalCode := Value;
end;

//------------------------------------------------------------------------------
function TBiotopeDictionaryNode.GetDetailTitle: string;
begin
  Result := FLongName;
end;

//------------------------------------------------------------------------------
function TBiotopeDictionaryNode.GetTitle: string;
begin
  Result := FText;
end;

//------------------------------------------------------------------------------
procedure TBiotopeDictionaryNode.SetLongName(const Value: string);
begin
  FLongName := Value;
end;

//------------------------------------------------------------------------------
procedure TBiotopeDictionaryNode.SetShortTerm(const Value: string);
begin
  FShortTerm := Value;
end;

//------------------------------------------------------------------------------
function TBiotopeDictionaryNode.GetAssociatedSourcesTitle: string;
begin
  Result := ResStr_AssocTaxonSource;
end;

//------------------------------------------------------------------------------
function TBiotopeDictionaryNode.GetAssociatedTitle: string;
begin
  Result := ResStr_AssocTaxa;
end;

//------------------------------------------------------------------------------
function TBiotopeDictionaryNode.GetMainSourcesTitle: string;
begin
  Result :=  ResStr_BiotopeSource;
end;

//------------------------------------------------------------------------------
procedure TBiotopeDictionaryNode.SetBiotopeKey(const Value: string);
begin
  FBiotopeKey := Value;
end;

//------------------------------------------------------------------------------
function TBiotopeDictionaryNode.ReportKeyType: TKeyType;
begin
  Result := ktBiotope;
end;

{TAdminAreaDictionaryNode}

procedure TAdminAreaDictionaryNode.SetItemName(const Value: string);
begin
  FItemName := Value;
end;

//------------------------------------------------------------------------------

procedure TAdminAreaDictionaryNode.SetShortCode(const Value: string);
begin
  FShortCode := Value;
end;

//------------------------------------------------------------------------------
function TAdminAreaDictionaryNode.ReportKeyType: TKeyType;
begin
  Result := ktAdminArea;
end;

//==============================================================================
{ TTaxonType }

procedure TTaxonType.SetItalicFlag(const iValue: Boolean);
begin
  FItalicFlag := iValue;
end;

procedure TTaxonType.SetSequence(const iValue: Integer);
begin
  FSequence := iValue;
end;

//==============================================================================
{ TTypeList }

destructor TTypeList.Destroy;
var
  lIndex : Integer;
begin
  for lIndex := 0 to Count-1 do
    TBaseType(Items[lIndex]).Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TTypeList.FindKey(iKey: TKeyString) : TBaseType;
var
  lIndex : Integer;
begin
  Result := nil;

  for lIndex := 0 to Count-1 do
  begin
    if TBaseType(Items[lIndex]).ItemKey = iKey then
    begin
      Result := TBaseType(Items[lIndex]);
      Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TTypeList.GetStrings: TStrings;
var
  lIndex : Integer;
  lReturn: TStringList;
begin
  lReturn := TStringList.Create;
  for lIndex :=0 to Count -1 do
    lReturn.AddObject(TBaseType(Items[lIndex]).ShortName,Items[lIndex]);

  Result := lReturn;
end;

//==============================================================================
{ TTaxonTypeList }

function TTaxonTypeList.GetLowerRanks(iKey: TKeyString): TStrings;
var
  lIndex : Integer;
  lCompareSequence : Integer;
  lReturn : TTaxonTypeList;
  lFound : TBaseType;
begin
  lReturn := TTaxonTypeList.Create;
  try
    lFound := FindKey(iKey);
    if assigned(lFound) then
      lCompareSequence := TTaxonType(lFound).Sequence
    else
      lCompareSequence  := -1;   // so that all are selected in loop below

    for lIndex := 0 to Count -1 do
      if TTaxonType(Items[lIndex]).Sequence > lCompareSequence then
        lReturn.Add(Items[lIndex]);

    Result := lReturn.GetStrings;
  except
    if Assigned(lReturn) then lReturn.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------
// returns maximum value of Sequence in list
function TTaxonTypeList.GetMaxSequence: Integer;
var
  lIndex : Integer;
  lReturn : Integer;
begin
  lReturn := 0;

  for lIndex := 0 to Count -1 do
    if TTaxonType(Items[lIndex]).Sequence > lReturn then
      lReturn := TTaxonType(Items[lIndex]).Sequence;

  Result := lReturn;
end;

//==============================================================================
{ TBiotopeType }

procedure TBiotopeType.SetItalicFlag(const iValue: Boolean);
begin
  FItalicFlag := iValue;
end;


{ TAddinOccNode }

procedure TAddinOccNode.SetConfidential(const Value: Boolean);
begin
  FConfidential := Value;
end;

{ TReportableNode }

//------------------------------------------------------------------------------
function TReportableNode.ReportKeyType: TKeyType;
begin
  Result := ktDefault;
end;

{ TNameNode }

//------------------------------------------------------------------------------
function TNameNode.ReportKeyType: TKeyType;
begin
  Result := ktName;
end;

end.
