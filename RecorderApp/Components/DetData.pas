unit DetData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  DataClasses, Db, BseMain, AccMain, taScrollStuff, JNCCDatasets,
  Grids;

type
  TDeterminationType = (dtTaxon, dtBiotope);

  TDeterminationItem = class(TGridDataItem)
  private
    FPreferred: boolean;
    FDate: string;
    FDeterminer: TCaptionAndKey;
    FWork: TCaptionAndKey;
    FDetType: TCaptionAndKey;
    FRole: TCaptionAndKey;
    FDetermination: TCaptionAndKey;
    FComment: TStrings;
    FTaxonOrBiotope : TDeterminationType;
    FComments: TStrings;
  protected
    { Compulsory methods }
    procedure InitFromRecord( iDataset : TDataset ); override;
    procedure WriteToRecord( iDataset : TDataset ); override;
    function GetCell( const iX : integer ) : string; override;
    { Accessor methods }
    procedure SetDate(const Value: string);
    procedure SetDetermination(const Value: TCaptionAndKey);
    procedure SetDeterminer(const Value: TCaptionAndKey);
    procedure SetDetType(const Value: TCaptionAndKey);
    procedure SetPreferred(const Value: boolean);
    procedure SetRole(const Value: TCaptionAndKey);
    procedure SetWork(const Value: TCaptionAndKey);
  public
    constructor CreateNew( aOwner : TCacheDataList );
    constructor CreateFromRecord( aOwner : TCacheDataList; iDataset : TDataset );
    destructor Destroy; override;
    property Determination : TCaptionAndKey read FDetermination write SetDetermination;
    property Determiner : TCaptionAndKey read FDeterminer write SetDeterminer;
    property Role : TCaptionAndKey read FRole write SetRole;
    property Preferred : boolean read FPreferred write SetPreferred;
    property DetType : TCaptionAndKey read FDetType write SetDetType;
    property Date : string read FDate write SetDate;
    property Work : TCaptionAndKey read FWork write SetWork;
    property Comments : TStrings read FComments;
  end;


  { Class to hold a list of determinations in a cache }
  TDeterminationDataList = class(TGridDataList)
  private
    FTaxonOrBiotope : TDeterminationType;
    function GetDetermination(const iIndex: integer): TDeterminationItem;
  protected
    procedure ProcessUpdate; override;
  public
    constructor Create(iDetType : TDeterminationType;
            iDataset: TDataset;
            iStringGrid: TStringGrid);
    property DataItems[ const iIndex : integer ] : TDeterminationItem read GetDetermination;
  end;

  TdmDeterminations = class(TDataModule)
    qryDeterminations: TJNCCQuery;
    qryDetTypes: TJNCCQuery;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmDeterminations : TdmDeterminations;

implementation

uses
  Determinations;

{$R *.DFM}

{ TDeterminationItem }



//==============================================================================
constructor TDeterminationItem.CreateFromRecord(aOwner: TCacheDataList;
  iDataset: TDataset);
begin
  FComment := TStringList.Create;
  inherited CreateFromRecord(aOwner, iDataset);
end;


//==============================================================================
constructor TDeterminationItem.CreateNew(aOwner: TCacheDataList);
begin
  FComment := TStringList.Create;
  inherited CreateNew(AOwner);
end;



//==============================================================================
destructor TDeterminationItem.Destroy;
begin
  FComment.Free;
  inherited Destroy;
end;


//==============================================================================
procedure TDeterminationItem.InitFromRecord(iDataset: TDataset);
begin
  with iDataset do
  begin
    FPreferred := FieldByName('PREFERRED').AsBoolean;
    FDate := FieldByName('VAGUE_DATE_START').AsString;
    FDeterminer := CaptionAndKey(FieldByName('NAME_KEY').AsString,
          FieldByName('FORENAME').AsString + ' ' + FieldByName('SURNAME').AsString);
    FWork := CaptionAndKey(FieldByName('SOURCE_KEY').AsString,
             'IMPLEMENT GET SOURCE STUFF');
    FDetType := CaptionAndKey(FieldByName('DETERMINATION_TYPE_KEY').AsString,
                FieldByName('DETERMINATION_TYPE').AsString); // really SHORT_NAME
    FRole := CaptionAndKey(FieldByName('DETERMINATION_ROLE_KEY').AsString,
                FieldByName('DETERMINATION_ROLE').AsString); // really SHORT_NAME
    FDetermination := CaptionAndKey(FieldByName('TAXON_LIST_ITEM_KEY').AsString,
                      FieldByName('NAME').AsString);
    FComments.Assign(FieldByName('COMMENT'));
  end; // with iDataset
end;


//==============================================================================
function TDeterminationItem.GetCell( const iX: integer ): string;
begin
  case iX of
    0 :
    begin
      if FPreferred then
        Result := 'True'
      else
        Result := 'False';
    end;
    1 :
      Result := FDetermination.Caption;
    2 :
      Result := FDeterminer.Caption;
    3 :
      Result := FRole.Caption;
    4 :
      Result := FDetType.Caption;
    5 :
      Result := FDate;
  else
    Result := 'Not implemented';
  end; // case
end;



//==============================================================================
procedure TDeterminationItem.WriteToRecord(iDataset: TDataset);
begin
  ShowMessage('Need to implement writeToRecord');
end;


//==============================================================================
procedure TDeterminationItem.SetDate(const Value: string);
begin
  FDate := Value;
end;


//==============================================================================
procedure TDeterminationItem.SetDetermination(const Value: TCaptionAndKey);
begin
  FDetermination := Value;
end;


//==============================================================================
procedure TDeterminationItem.SetDeterminer(const Value: TCaptionAndKey);
begin
  FDeterminer := Value;
end;


//==============================================================================
procedure TDeterminationItem.SetDetType(const Value: TCaptionAndKey);
begin
  FDetType := Value;
end;


//==============================================================================
procedure TDeterminationItem.SetPreferred(const Value: boolean);
begin
  FPreferred := Value;
end;


//==============================================================================
procedure TDeterminationItem.SetRole(const Value: TCaptionAndKey);
begin
  FRole := Value;
end;


//==============================================================================
procedure TDeterminationItem.SetWork(const Value: TCaptionAndKey);
begin
  FWork := Value;
end;



//==============================================================================
{ TDeterminationDataList }
//==============================================================================


//==============================================================================
constructor TDeterminationDataList.Create(iDetType : TDeterminationType;
            iDataset: TDataset;
            iStringGrid: TStringGrid);
begin
  if iDetType = dtTaxon then
    inherited Create( dmDeterminations.qryDeterminations,
                      'DETERMINATION_KEY', iStringGrid, TDeterminationItem)
  else
    inherited Create( dmDeterminations.qryDeterminations,
                      'BIOTOPE, DETERMINATION_KEY', iStringGrid,
                      TDeterminationItem);
  FTaxonOrBiotope := iDetType;
end;






{ Accessor method for dataitems property - just a convenient typecast really }
function TDeterminationDataList.GetDetermination(
         const iIndex: integer): TDeterminationItem;
begin
  Result := TDeterminationItem(GetDataItem(iIndex));
end;



//==============================================================================
procedure TDeterminationDataList.ProcessUpdate;
begin
  ShowMessage('Need to implement processupdate');

end;




end.
