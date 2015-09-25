//==============================================================================
//  Unit:        XMLDoc
//
//  Implements:  TAttList
//               TAttribute
//               TComment
//               TContentItem
//               TDocType
//               TElement
//               TEntity
//               TXMLDoc
//               TXMLDocParser
//
//  Description: Document Type Definition classes for XML file parsing/output.
//
//               TAttList
//               Class to hold a single attribute list in an XmL document.
//
//               TAttribute
//               Class to hold an attribute's definition extracted from a DTD
//               Attribute list.
//
//               TComment
//               Class to hold a single comment in an XmL document.
//
//               TContentItem
//               Class which holds part of the content declaration of an
//               element.  hierarchical - can contain references to itself.
//
//               TDocType
//               Class to hold a single DocType directive.
//
//               TElement
//               Class to hold a single element definition for a DTD.
//
//               TEntity
//               Class to hold a single XML entity as parsed from a DTD.
//
//               TXMLDoc
//               Class to interface to a document type definition. Caches items
//               which have already been read in a transparent fashion to the user.
//
//               TXMLDocParser
//               Derivative of TDocParser which handles entities.
//
//  Author:      John van Breda
//  Created:     21 June 1999
//
//  Changes:
//
//  Last Revision Details:
//    $Revision: 12 $
//    $Date: 30/05/08 9:10 $
//    $Author: Johnvanbreda $
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

{ CONSTRAINTS - datasize is a special attribute name for blob sizes
                attribute fieldname indicates a special element
                value is a special attribute name
                entities can be internal, only TXT external entities supported
                    in which case the entity file must be in the same dir as the
                    current XML file.
                FieldNames cannot start with '_'
                '-' cannot be used as separator for any vague dates
                Fields containing word TIME are treated as TIME rather than DATE
                Any CDATA[] content must have a 6 character hex value prefix
                indicating the size of the file in bytes.
                Primary key fields must be explicit tags, not attributes for the
                  table level tag.
                All key fields must be strings.
                All occurrence tables (and only occurrence tables) must end
                 with the word OCCURRENCE)
                Data import validation through COM cannot be on join tables
                If an element is contained within more than one other element,
                   it will be output as a top level element. }

unit JNCCXMLDoc;

interface

uses
  Classes, Sysutils, Forms, Controls, Graphics, XMLTypes, MarkupDocs, Windows, 
  JNCCDatasets, db, FileOutput, DataClasses, TaskProgress, ComCtrls, Messages,
  GeneralFunctions, Relationships_ADO, ADODB, DatabaseAccessADO;

const
  DATA_SIZE = 'datasize'; // special attribute name for length of blobs
  KEY_TYPE = 'type';
  VALUE_TYPE = 'value'; // special attribute name where a key or free text can be specified
  FREE_TERM = 'free_term';
  FIELD_NAME = 'specifier';
  TERMLISTS = 'termlists';
  TIME = 'TIME'; // must be in a field name for it to output the time

type
  { TAttribute
   Class to hold an attribute's definition extracted from a DTD Attribute list }
  EAtrributeError = class(EDocItemError);

  TAttribute = class(TObject)
  private
    FDataType: TAttrType;
    FUsage: TAttrUsage;
    FName : string;
    FDefault : string;
    FEnumerations : TStringList;  //  only instantiated for enumerated attributes
  public
    constructor Create( const iName : string; const iDataType : TAttrType;
            iEnumerations : TStringList; const iUsage :TAttrUsage;
            const iDefault : string );
    destructor Destroy; override;
    property DataType : TAttrType read FDataType;
    property Enumerations : TStringList read FEnumerations;
    property Usage : TAttrUsage read FUsage;
    property Name : string read FName;
    property DefaultValue : string read FDefault;
  end;

  { Forward declaration for TContentItem so that elements can refer to them }
  TContentItem = class;

  { TElement
   Class to hold a single element definition for a DTD }

  EElementError = class(EDocItemError);

  TContentModel = (cmAny, cmEmpty, cmMixed);

  TElement = class(TDirective)
  private
    FContentModel : TContentModel;      // This is the item we are being output within
    FMixedContent : TContentItem;       // These are the ones we contain
    FContentItem  : TContentItem;
    FIsTable : boolean;
    FIsMultipleContent : boolean; // ie content model is not plain PCDATA
    FFieldName : string;
    function GetStringPairValue(const iNameValue: string): string;
    procedure OutputMeasurementTypeContext(iDataset: TDataSet;
      iIndent: integer);
  protected
    procedure BuildMixedContent( var ioRow, ioPos : integer );
    procedure OutputTag(const iRecord: TDataset; const iIndent: integer; iFromExportFilter: Boolean);
    procedure DoMixedOutput( const iRecord : TDataset; const iIndent : integer;
        iFromExportFilter: Boolean );
    procedure OutputAttributes( const iRecord : TDataset );
    procedure OutputGenericDates( iRecord : TDataset; const iIndent : integer );
    procedure OutputSpecialDateAttributes( const iFieldName : string );
    function TryOutputSpecialElement: boolean;
    procedure DoContentOutput( const iIndent : integer; iFromExportFilter: Boolean);
    procedure DoTermlistOutput( const iIndent : integer );
    procedure OutputTermListDescription(const iListname: string;
                    const iIndent: integer);
    procedure OutputTermItem( const iIndex, iIndent : integer);
    procedure CheckForLinkedTerms(iDataset: TDataset);
    procedure OutputTermJustTag(iDataset: TDataset; const Creator: string);
    procedure OutputAdditionalFields(iDataset : TDataset; iiIndent : integer);
    function OutputElsewhere( const iFieldName : string;
                              iRecord : TDataset ): boolean;
    procedure ReadContent( var ioRow, ioPos : integer );
    function FindTableElementName( iDataset : TDataset ) : string;
    function ReadRecordFromKeyItem( iList: TKeyList;
                               const iIndex : integer): TRecordIdentifier;
    procedure OutputSingleRecordContent( iRecord : TRecordIdentifier;
              const iIndent : integer; iFromExportFilter: Boolean);
    procedure OutputElementByContentModel(const iRecord: TDataset;
      const iIndent: integer; iFromExportFilter: Boolean);
  public
    constructor CreateFromStrings( iParent : TAbstractDocItem;
                iDocument : TMarkupDocument; var ioRow, ioPos : integer );
    destructor Destroy; override;
    procedure SetField( const iFieldName : string );
    procedure DoOutput( const iRecord : TDataset; const iIndent : integer;
                        iContentItem : TContentItem; iFromExportFilter: Boolean);
    property ContentModel : TContentModel read FContentModel;
    property IsTable : boolean read FIsTable;
    property MixedContent : TContentItem read FMixedContent;
  end;

  { TContentItem, class which holds part of the content declaration of an
     element.  hierarchical - can contain references to itself  }
 
  TListRelationship = ( lrUnknown, lrEitherOr, lrOrdered  );
  TItemRule = ( irOne, irOneOptional, irManyOptional, irOneToMany );

  EContentItemError = class(EDocumentError);
  EContentValidationError = class(EContentItemError);
  EPCDataContent = class(Exception); // internal - not for reporting errors to user

  TContentItem = class
  private
    FItemRule : TItemRule;
    FContainedContentList : TList;
    FListRelationship : TListRelationship;
    FText : string;
    FDocument : TDocument;
    FParentItem : TObject; // The item it is contained within - element or contentitem
    function GetContentCount: integer;
    function GetContentItem(const iIndex: integer): TContentItem;
    procedure ReadContentWord( var ioRow, ioPos: integer);
    function GetNestedDataset(iCurrentRec: TDataset;
      const iName: string;var oReverse : boolean): TDataset;
    procedure CheckRecordCountAgainstContentRule( iDataset : TDataset );
    procedure CheckCountAgainstContentRule( iCount : integer );
    procedure HandleSpecialTables( var ioTableName : string; iRecord : TDataset );
    function HandleMetadata( const iElementName : string ): boolean;
    function ReadListSeparator( var ioRow, ioPos : integer ): boolean;
    procedure OutputMultipleContent( iRecord : TDataset; const iIndent : integer;
        iFromExportFilter: Boolean);
    procedure OutputSingleContent( iRecord : TDataset; const iIndent : integer;
        iFromExportFilter: Boolean);
    procedure HandlePCDataContent( iRecord : TDataset );
    function HandleOptimisations( const iRecord: TDataset;
            const iIndent: integer) : boolean;
  public
    constructor Create( iDocument : TDocument; var ioRow, ioPos : integer);
    destructor Destroy; override;
    function NeedAtLeastOne: boolean;
    function NoMoreThanOne: boolean;
    procedure DoOutput( const iRecord : TDataset; const iIndent : integer;
              iParentItem : TObject; iFromExportFilter: Boolean );
    procedure OutputTableContent( iElement : TElement;
              iRecord: TDataset; const iIndent: integer; iFromExportFilter: Boolean);
    function ContainsElement( const iName : string ): boolean;
    function FirstOutputField: string;
    function ValidateTag( const iTag : TTag; const iContainedTagNo : integer ): integer;
    procedure CheckNotFoundIsOk( const iContentIndex : integer );
    property Text : string read FText;
    property ItemRule : TItemRule read FItemRule;
    property ListRelationship : TListRelationship read FListRelationship;
    property ContainedContentCount : integer read GetContentCount;
    property ContainedContent[ const iIndex : integer ] : TContentItem read GetContentItem;
    property ParentItem : TObject read FParentItem;
  end;

  { TEntity
    Class to hold a single XML entity as parsed from a DTD }
  EEntityError = class(EDocItemError);

  TEntity = class(TDirective)
  private
    FTextStrings : TStringList;
    FParameter : boolean; // is it a parameter entity? Ie inside DTD only
    FEntityType : TEntityType;  // internal, external etc
  protected
    procedure CheckIfParameterEntity( var ioRow, ioPos : integer );
    procedure ReadExternalEntity( var ioRow, ioPos : integer );
  public
    constructor CreateFromStrings( iParent : TAbstractDocItem;
                iDocument : TMarkupDocument; var ioRow, ioPos : integer );
    destructor Destroy; override;
    property TextStrings : TStringList read FTextStrings;
    property Parameter : boolean read FParameter;
    property EntityType : TEntityType read FEntityType;
  end;

  { TComment
   Class to hold a single comment in an XmL document }

  ECommentError = class(EDocItemError);

  TComment = class(TDirective)
  private
    FText : string;
  protected
  public
    constructor CreateFromStrings( iParent : TAbstractDocItem;
                iDocument : TMarkupDocument; var ioRow, ioPos : integer );
    property Text : string read FText;
  end;

  { TAttList
   Class to hold a single attribute list in an XmL document }

  EAttListError = class(EDocItemError);
  EDataNeededError = class(EXMLError);


  TAttList = class(TDirective)
  private
    FAttributes : TList;
    FDocument : TDocument;
    function GetAttributeCount: integer;
    function GetAttributes(const iIndex: integer): TAttribute;
  protected
    function ReadEnumerations(var ioRow, ioPos: integer): TStringList;
    procedure ReadAttributes( var ioRow, ioPos : integer );
    function ReadDataType(var ioRow, ioPos : integer): TAttrType;
    function ReadUsage(var ioRow, ioPos : integer): TAttrUsage;
    function ReadDefault( var ioRow, ioPos : integer;
                          iEnumerations : TStringList): string;
    function GetAttributeData( iRecord : TDataset; iAttribute : TAttribute )
                                                              : string;
    function FindFieldName(iRecord : TDataset): string;
    function FindSpecifierFromFields(iRecord: TDataset;
          const iMatchString: string; var oSpecifier: string): boolean;
    procedure CheckAttributeAgainstData( iAttribute : TAttribute;
                                         var ioData : string );
    function CheckValidNMToken( const iToken : string ): boolean;
    procedure DoDatasetSourceOutput;
    procedure DoDatasetCopyOriginOutput;
    procedure StoreAttForeignKeys(iRecord: TDataset; iAttr : TAttribute );
  public
    constructor CreateFromStrings( iParent : TAbstractDocItem;
                iDocument : TMarkupDocument; var ioRow, ioPos : integer );
    destructor Destroy; override;
    function FindAttribute( const iName : string ): TAttribute;
    procedure DoOutput( const iRecord: TDataset ) overload;
    property AttributeCount : integer read GetAttributeCount;
    property Attributes[ const iIndex : integer ] : TAttribute read GetAttributes;
  end;


  { TDocType
   Class to hold a single DocType directive }

  EDocTypeError = class(EDocItemError);

  TDocType = class(TDirective)
  private
    FNotation : string;
    FDTDFile : string;
    FExternalID : string; // system or public
  protected
    procedure ReadFile( var ioRow, ioPos : integer );
  public
    constructor CreateFromStrings( iParent : TAbstractDocItem;
                iDocument : TMarkupDocument; var ioRow, ioPos : integer );
    procedure ProcessExternalDoc;
    property Notation : string read FNotation;
    property DTDFile : string read FDTDFile;
  end;


  { TXMLDoc
   Class to interface to a document type definition. Caches items which
      have already been read in a transparent fashion to the user.  }

  EXMLDocError = class(EXMLError);
  EDTDNotComplete = class(EXMLDocError);

  TDocItemClass = class of TAbstractDocItem;

  TXMLDoc = class(TMarkupDocument)
  private
    FCdataStreams : TList;
    FOldDateFormat : string;
    FOldTimeFormat : string;
    FFileStream : TFileStream;
    FBlockStream : TMemoryStream;
    FCurrentBlockStart : Int64;
    FPercentStart : integer;
    FPercentEnd : integer;
    procedure CreateStreams( const iFileName : string );
    procedure SurveyEventFoundInStream(iFileStrings: TStrings;
      const iTagTest: string);
    function CDataFoundInStream(iFileStrings: TStrings;
      const iTagTest: string): boolean;
  protected
    procedure CreateDocParser; override;
    function ReadNewStream(iStream: TStream; iSize : integer): integer;
    procedure LoadFile(iFileStrings : TStrings;
                                    const iFileName : string); override;
    function GetItemByName( const iName : string; iClass :
                            TDocItemClass ) : TAbstractDocItem;
    procedure InitDateTimeFormats;
    procedure ResetDateTimeFormats;
  public
    constructor Create( const iFileName : string; iOutputter : TFileOutput;
                iProgressBar : TTaskProgressBar );
    destructor Destroy; override;
    procedure OutputField(iField : TField; iAllowBlank : Boolean;
              iAsString : boolean);
    function GetCData(const iIndex: integer): TMemoryStream;
    function SearchForContainedContentByName( const iName : string ):TElement;
    function DocTypeElement : TElement;
    function GetElementByName( const iName : string ): TElement;
    function GetAttlistByName( const iName : string ): TAttlist;
    function GetEntityByName( const iName : string ): TEntity;
    function GetTagByName( const iName : string ) : TTag;
    function ProcessDocument( iMustBeXML : boolean ): TTag; override;
    procedure PopulateErrorReport(iRichEdit: TRichEdit; const iRow : integer;
              const iPosition: Int64);
    property PercentStart : integer read FPercentStart; // progress bar
    property PercentEnd : integer read FPercentEnd;
    property CurrentBlockStart : int64 read FCurrentBlockStart;
  end;


  { TXMLDocParser - derivative of TDocParser which handles entities }

  TXMLDocParser = class(TDocParser)
  private
    FDocument : TXMLDoc;
    procedure SpliceEntity( const iRow, iPos : integer; iEntity : TEntity );
  public
    constructor Create( iDocument : TXMLDoc );
    function GetNextWord( const iRow, iPos : integer ) : string; override;
    function GetChar( const iRow, iPos : integer) : char; override;
  end;

//==============================================================================
implementation

uses
  DataOutput, XMLData, GeneralData, MainTBar, VersionInfo, FastStrings,
  ApplicationSettings, XMLErrorReport, StreamHtml2Rtf, GenFuncs;

const
  VALID_NMTOKEN_CHARS = ['a'..'z','A'..'Z','0'..'9','.','-','_',':'];

resourcestring
  {Error messages}
  ResStr_ItemNotFound = 'The item could not be found : ';
  ResStr_DTDProblem = 'Problem occurred during Document Type Definition reading.  Items read : ';
  ResStr_WrongParent = 'A markup is nested incorrectly.  ';
  ResStr_ContentUknown = 'The content type is unknown for element ';
  ResStr_MixedListTypes = 'Element Content lists cannot mix | and , symbols';
  ResStr_ContentWrong = 'The content is missing for an element.';
  ResStr_AttlistNotEnded = 'An Attlist is not correctly terminated.';
  ResStr_AttributeStructure = 'The attribute definition is incorrect';
  ResStr_EntityStructure = 'The entity definition is incorrect';
  ResStr_UnknownKeyword  = 'The following keyword is not recognised : ';
  ResStr_AttlistNeedsData = 'An attlist specifies required data which is not available : ';
  ResStr_ElementNeedsData = 'An element specifies required data which is not available : ';
  ResStr_ContentModelFailure = 'An internal error has occurred relating to the content model of an attlist';
  ResStr_GetOutputError = 'Failure during DoOutput for : ';
  ResStr_ContentRuleBroken = 'The content rule has been broken for element content item %s in table %s';
  ResStr_InvalidChar = 'An invalid character has been found in the definition of ';
  ResStr_OrOutputFailed = 'None of the elements could be output for the optional list for element ';
  ResStr_DefaultInvalid = 'The default value for an attribute list is invalid : ';
  ResStr_AttsForElement = 'Problem occurred processing attributes for element ';
  ResStr_ItemDataMissing = 'Content item needs data which is not available : ';
  ResStr_CircularReference = 'An circular reference has occurred doing output for element ';
  ResStr_InvalidNotation = 'The XML import document is not NBN notation and cannot be imported';
  ResStr_FieldNameAttrInvalid = 'A specifier attribute has been located in an attlist with no valid fields : ';
  ResStr_ContentList = 'The content model is not listed correctly, a separator could not be detected between two items.';
  ResStr_ItemRequired = 'A markup is required according to the content model : ';
  ResStr_ContentIncorrect = 'Content is incorrect according to the content model for ';
  ResStr_TableProblem = 'Problem occurred during table element output : ';
  ResStr_NoCData = 'A problem occurred trying to read CDATA information : ';
  ResStr_InvalidStream = 'An invalid binary data section has been encountered. ';
  ResStr_AttributeCantOutput = 'The attribute cannot be output because its data is invalid :';
  ResStr_WriteMisMatch = 'Stream write size mismatch';
  ResStr_ExtIdInvalid = 'DocType declarations must be SYSTEM or PUBLIC';
  ResStr_DocTypeFile = 'DocType does not have an external reference in correct location';
  ResStr_NoTerms = 'There are no terms to output';
  ResStr_RecordMissing = 'Output failed because a record is referred to which does not exist in ';
  ResStr_CannotOpen = 'Cannot open file ';
  ResStr_EmptyFile = 'The file is empty';

  ResStr_RecordIgnored =  'Record in table %s ignored as not specified by DTD';
  ResStr_RecordDropped =  'Record in table %s dropped from main list of output items';
  ResStr_ProbableProblem = 'Probable problem with stream size specifier.';
  ResStr_IncorrectPosTermString = 'Termination string not in correct position.';
  ResStr_AttListHasInvalidElement = 'Attlist %s has no valid element.';
  ResStr_ContainsAttlistDir = '%s contains an ATTLIST directive.';
  ResStr_AttributeTypeNotSupported = 'Attribute type %s not supported : %s';
  ResStr_AttlistErr = 'Attlist %s : ';
  ResStr_ContentRolledBack =  'Content item %s was rolled back for key %s in table %s';
  ResStr_ContentRecRolledBack = 'Content item (record) %s was rolled back for key %s in table %s';
  ResStr_NoDataAvailable =  'No data available for %s';
  ResStr_SourceBlank =  'Source blank for record';

  ResStr_RelationshipNotCoped = 'RELATIONSHIP NUMBER NOT COPED WITH : %s relationship(s) from table %s' +
                                ' to table %s';

  ResStr_OnlyOne =  'there should be one %s not %s';
  ResStrMaxOne =  'there can be a maximum of one %s not %s';
  ResStr_AtLeastOne =   'there must be at least one %s';
  ResStr_OneOutputRestricted =  'there should be one %s but output of this item was restricted';
  ResStr_AtLeastOneRestricted = 'there should be at least one %s but output of these items were restricted';
  ResStr_CommUnrecognised = '(# command unrecognised).';
  ResStr_ElementContModel = 'Element content model - %s is not a valid Element.';
  ResStr_EntityNotInside =  'Entity is not inside DOCTYPE directive';
  ResStr_ContainsEntityDir = '%s contains an ENTITY directive.';
  ResStr_PublicExternalNotSupp =  '.  Public external entities not supported.';


//==============================================================================
  { TElement }
//==============================================================================

{ Constructor for TElement.  Reads an element from the string at the position
     indicated }
constructor TElement.CreateFromStrings( iParent : TAbstractDocItem;
            iDocument : TMarkupDocument; var ioRow, ioPos : integer );
begin
  if not (iParent is TDocType) then // elements must be declared inside a DOCTYPE
    raise EElementError.Create(ResStr_WrongParent, ioRow, TXMLDoc(iDocument).CurrentBlockStart);
  inherited CreateFromStrings( iParent, iDocument );
  FFieldName := '';  // default - no fieldname attribue
  { Check it is an element }
  ConfirmType('ELEMENT', ioRow, ioPos );
  { Read the element name }
  FName := iDocument.DocParser.GetNextWord( ioRow, ioPos );
  { Need to find out if the element refers to a table - special handling here to
       cope with elements which refer to a table but have a different name }
  FIsTable := (dmNBNXML.CheckIsTable(FName));
  FIsMultipleContent := True; // default value - if we find PCDATA set it to false
  Inc(ioPos, Length(FName));
  ReadContent( ioRow, ioPos );
  { Move to the end of the element definition }
  iDocument.DocParser.SearchFor(stFindWords, ['>'], ioRow, ioPos);
end;



{ Destructor for TElement - cleanup lists }
destructor TElement.Destroy;
begin
  FMixedContent.Free;
  inherited Destroy;
end;



{ ReadContent - picks up the content model definition for an element for the
     DTD.  Content model can be ANY, EMPTY, or otherwise in which case the
     model is read by creating a content item which recurses into the sub-items
     of the content model. }
procedure TElement.ReadContent(var ioRow, ioPos: integer);
var
  lstContent : string;
begin
  { Read the content stuff }
  FDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos);
  lstContent := FDocument.DocParser.GetNextWord( ioRow, ioPos );
  if lstContent = 'ANY' then
  begin
    FContentModel := cmAny;
    Inc(ioPos, Length(lstContent)-1);
  end
  else
    if lstContent = 'EMPTY' then
    begin
      FContentModel := cmEmpty;
      FIsMultipleContent := False;
      Inc(ioPos, Length(lstContent)-1);
    end
    else
    begin
      FContentModel := cmMixed;
      BuildMixedContent( ioRow, ioPos );
      if (FMixedContent.ContainedContentCount = 1) and
         (FMixedContent.ContainedContent[0].text = '#PCDATA') then
        FIsMultipleContent := False;

    end;
end;



{ Locates the element name for the current dataset.  For example, if we are
    outputting a key field element, then the actual table element name is
    returned.  Usually, just uses GetDatasetTable, but if not returned then
    checks to see if the dataset might be a special table element and returns
    that. }
function TElement.FindTableElementName(iDataset: TDataset): string;
begin
  Result := ''; // gets rid of annoying compiler message
  Result := dmNBNXML.GetDatasetTable( iDataset );
  try
    { Check the element exists }
    TXMLDoc(FDocument).GetElementByName( Result );
  except
    on EDTDNotComplete do
      Result := dmNBNXML.GetSpecialTableElement( iDataset );
  end; // try..except
end;



{ The element does not have ony of the standard keywords, so it is probably
     mixed content.  Try and parse this out here }
procedure TElement.BuildMixedContent(var ioRow, ioPos: integer);
begin
  FDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
  FMixedContent := TContentItem.Create( FDocument, ioRow, ioPos );
  { Decrease the position by one.  This is because the content item creation
    advances to the next character it needs to process, but the main parser
    works on the basis that it finishes on the character preceding }
  Dec(ioPos);
end;



{ Do the output for an element (standard version).  Takes the current dataset
     and indentation as parameters.  Calls recursively in to output each
     of the contained elements.  EDataNeeded errors are allowed to pass out
     if the element data cannot be output. }
procedure TElement.DoOutput( const iRecord : TDataset; const iIndent : integer;
                             iContentItem : TContentItem; iFromExportFilter: Boolean);
begin
  Application.ProcessMessages;
  if Assigned(FDocument.Outputter) and FDocument.Outputter.Cancelled then
    raise ECancellation.CreateNonCritical(ResStr_Cancelled);
  TXMLDoc(FDocument).ElementStack.Add(FName);
  try
    try
      { If the contentitem isn't nil, we are already in the process of outputting
           the element so we must have a circular reference }
      if FContentItem <> nil then
        raise EXMLError.Create(ResStr_CircularReference + FName);
      FContentItem := iContentItem; // the item which called us
      { Handle some special cases }
      if FName = GENERIC_DATE then
        OutputGenericDates( iRecord, iIndent )
      else
      begin
        FDocument.Outputter.NewLine( iIndent );
        FDocument.Outputter.OutputText('<' + FName);
        OutputAttributes( iRecord );
        OutputElementByContentModel( iRecord, iIndent, iFromExportFilter);
      end;
    finally // make sure we clean up the element stack
      TXMLDoc(FDocument).ElementStack.Delete(TXMLDoc(FDocument).ElementStack.Count-1);
      FContentItem := nil;
    end; // try..finally
  except
    on EDataNeededError do  { element's data not available }
      { Try and output from the SPECIAL_XML_ELEMENTS table before we give up
              and rollback }
      if not TryOutputSpecialElement then
         raise;
  end; // try..except
end;



{ Manages the output of a single element (and all contents) according to the
    content model for the element }
procedure TElement.OutputElementByContentModel( const iRecord : TDataset;
          const iIndent : integer; iFromExportFilter: Boolean);
var
  lTaskIndex : integer;
begin
  case FContentModel of
    cmAny: // ANY content triggers the main bulk of data output
    begin
      lTaskIndex := frmMain.ProgressBar.EmbedTask(0,80);
      try
        DoContentOutput( iIndent + 1, iFromExportFilter);
      finally
        frmMain.ProgressBar.FinishTask(lTaskIndex);
      end; // try
    end;
    cmMixed:
      if FName = TERMLISTS then
      begin
        lTaskIndex := frmMain.ProgressBar.EmbedTask(80,100);
        try
          DoTermlistOutput( iIndent );
        finally
          frmMain.ProgressBar.FinishTask(lTaskIndex);
        end;
      end
      else
        OutputTag( iRecord, iIndent + 1, iFromExportFilter );
    cmEmpty:     // use special XML end tag to indicate it is an empty tag
      FDocument.Outputter.OutputText('/>');
  end; // case
end;



{ Dump out the attribute information for the current element }
procedure TElement.OutputAttributes( const iRecord : TDataset );
begin
  try
    { Need to catch data not available errors - if the element is not required
          then don't worry }
    TXMLDoc(FDocument).GetAttlistByName(FName).DoOutput(iRecord);
  except
    on E:EDataNeededError do
      raise EDataNeededError.Create(ResStr_AttsForElement + FName, E);
    on EDTDNotComplete do { nothing - no attlist to process } ;
  end;// try
end;




{ Handle the date* declaration, where all dates are output in a record except
     for the metadata. }
procedure TElement.OutputGenericDates(iRecord: TDataset; const iIndent : integer);
var i : integer;
    lOutputCount : integer; // count of dates we find to output
    lFileSize : Longint; // allow rollback of file output
begin
  lOutputCount := 0;
  { Record the current file size so we don't accidentally rollback any further }
  lFileSize := FDocument.Outputter.FileSize;
  { Search for date fields }
  for i := 0 to iRecord.Fields.Count-1 do
  begin
    with iRecord.Fields[i] do
    begin
      if (not OutputElsewhere(FieldName, iRecord)) and (DataType = ftInteger) and
         (Text<>'') then
      begin
        { Make sure we ignore vague date end field - just use normal dates or
          vague start dates }
        if not dmNBNXML.NameContainsSpecifier(FieldName, ['VAGUE'], False) or
           dmNBNXML.NameContainsSpecifier(FieldName, ['VAGUE_DATE_START'],
                                                     False) then
        begin
          FDocument.Outputter.NewLine(iIndent);
          FDocument.Outputter.OutputText('<' + FName );
          OutputSpecialDateAttributes( FieldName );
          FDocument.Outputter.OutputText('>' + Text);
          FDocument.Outputter.OutputText('</' + FName + '>');
          { Record the file size, so we can undo the next carriage return for
            the last element }
          lFileSize := FDocument.Outputter.FileSize;
          Inc(lOutputCount);
          if FContentItem<>nil then
            if   (FContentItem.ItemRule = irOne) or
                 (FContentItem.ItemRule = irOneOptional) then
              break;  // from for loop - we only want one item
        end; // if field needs output
      end; // if date field
    end; // with
  end; // for
  { Remove the last carriage return - we don't want it to duplicate }
  FDocument.Outputter.FileSize := lFileSize;
  { Check that we comply with the content rules }
  if ((FContentItem.ItemRule = irOneToMany) and (lOutputCount = 0)) or
     ((FContentItem.ItemRule = irOne) and (lOutputCount <> 1)) then
    raise EXMLError.Create(Format(ResStr_ContentRuleBroken,
          [FContentItem.Text, dmNBNXML.GetDatasetTable(iRecord)]));
end;


{ Outputs the type and format specifier for a date field, according to the name.
     The field name can be used to determine if it is a vague date field or
     otherwise, and whether it is a from, to or normal date type }
procedure TElement.OutputSpecialDateAttributes(const iFieldName: string);
begin
  if SmartPos('VAGUE_DATE_START', iFieldName) <> 0 then
  begin
    FDocument.Outputter.OutputText(' ' + FIELD_NAME + '="' +
           Copy(iFieldName, 1, Length(iFieldName) - Length('_VAGUE_DATE_START')) +
           '"');
    FDocument.Outputter.OutputText(' format="vague"');  // vague date
  end else
  begin
    { normal date }
    FDocument.Outputter.OutputText(' ' + FIELD_NAME + '="' + iFieldName + '"');
    FDocument.Outputter.OutputText(' format="ddmmyyyy"');
  end;
end;


{ Checks if a field is output elsewhere in the element, or output anywhere in
     the attlist.  If so, returns true.  Useful for doing things like
     finding out which dates to output for a date* construct. }
function TElement.OutputElsewhere(const iFieldName: string;
                                        iRecord : TDataset): boolean;
var
  i : integer;
  lAttlist : TAttlist;
  lElement : TElement;
  ltfFound : boolean;
begin
  ltfFound := False;
  try
    { First check the Attlist that is associated with the current table }
    lAttlist := TXMLDoc(FDocument).GetAttlistByName(FindTableElementName(iRecord));
    for i := 0 to lAttlist.AttributeCount-1 do
    begin
      if Uppercase(lAttlist.Attributes[i].Name) = Uppercase(iFieldName) then
      begin
        ltfFound := True;
        break; // from loop, no need to continue
      end;
    end;
  except
    on EDTDNotComplete do ;// nothing - no attlist present to check
  end;
  { then check the ContentModel for a sub-element that outputs this field }
  try
    if not ltfFound then
    begin
      lElement := TXMLDoc(FDocument).GetElementByName
                  (FindTableElementName( iRecord ));
      if lElement.MixedContent.ContainsElement(iFieldName) then
        ltfFound := True;
    end; // if not found
  except
    on EDTDNotComplete do ;// nothing - no element present to check
  end;
  Result := ltfFound;                             
end;





{ Calls the DoMixedoutput method, providing appropriate tags around the output }
procedure TElement.OutputTag(const iRecord : TDataset; const iIndent : integer;
    iFromExportFilter:Boolean);
begin
  FDocument.Outputter.OutputText('>');
  DoMixedOutput( iRecord, iIndent, iFromExportFilter );
  { For record elements, we need a line feed before the end tag }
  if FIsMultipleContent then
    FDocument.Outputter.NewLine( iIndent-1 );
  FDocument.Outputter.OutputText('</' + FName + '>');
end;



{ Extract sub-element stuff from a record stringlist (name value pairs).  Calls
     the relevent element's DoOutput method to recursively build the results
     string.  Content model must be mixed to call this!  If PCData is
     encountered then the relevant text or blob information is streamed out. }
procedure TElement.DoMixedOutput(const iRecord : TDataset;
          const iIndent : integer; iFromExportFilter: Boolean);
begin
  if FContentModel <> cmMixed then
    raise EXMLError.Create(ResStr_ContentModelFailure);
  FMixedContent.DoOutput(iRecord, iIndent, Self, iFromExportFilter );
end;



{ Before we decide the data is missing for an element, we'll have a look in the
    SPECIAL_XML_ELEMENT table to see if there is some text to output.  This
    is how most of the metadata is handled }
function TElement.TryOutputSpecialElement: boolean;
var
  lSpecialElement : TSpecialElementItem;
begin
  Result := True; // default
  lSpecialElement := dmNBNXML.FindSpecialElement( FName, METADATA );
  if lSpecialElement = nil then // does it exist?
  begin
    { Handle some special cases }
    if FName='version' then
      FDocument.Outputter.OutputText(Application.Title + ' ' + GetFileVersion)
    else if FName='dataset_copy_origin' then
      FDocument.Outputter.OutputText(AppSettings.SiteID)
    else if FName='date_exported' then
      FDocument.Outputter.OutputText(FormatDateTime('dd mmm yyyy',now))
    else
      Result := False;
  end else
  begin
    FDocument.Outputter.OutputText(lSpecialElement.Data);
  end;
  if Result then // we succesfully output some data
    FDocument.Outputter.OutputText('</' + FName + '>');
end;



{ For an element with model ANY, (normally the content element), output all the
     records in the ouput key list as nested elements.  This procedure therefore
     kicks off the main bulk of the processing }
procedure TElement.DoContentOutput(const iIndent: integer; iFromExportFilter: Boolean);
var
  i : integer;
  lRecord : TRecordIdentifier;
  lNewProgress : integer;
  lTopTableKeyList : TKeyList;
  lListCount : integer;
  lOriginalItemCount: integer;
  lFileSize : integer; // allow rollback of file output
begin
  FDocument.outputter.outputText('>');  // close the content tag
  i := 0;
  { Use while, not for, as the itemcount can increase as we find new data to
      output }
  dmNBNXML.KeyList.SortTableWithOccsTogether;
  lOriginalItemCount := dmNBNXML.KeyList.Header.ItemCount;
  while i < dmNBNXML.KeyList.Header.ItemCount do
  begin
    Application.ProcessMessages;
    if Assigned(FDocument.Outputter) and FDocument.Outputter.Cancelled then
      raise ECancellation.CreateNonCritical(ResStr_Cancelled);

    lFileSize := FDocument.Outputter.FileSize;
    try
      { If we are outputting a checklist as a secondary data item, then don't
          include all its child list items }
      dmNBNXML.RestrictListItems := (i>=lOriginalItemCount);
      dmNBNXML.ClearRestrictedData; // new restricted data set per item
      lTopTableKeyList := dmNBNXML.GetContainerElementListForTable( i );
      if lTopTableKeyList=nil then // can output single data item as is
      begin
        lRecord := ReadRecordFromKeyItem(dmNBNXML.KeyList, i-1);
        OutputSingleRecordContent(lRecord, iIndent, iFromExportFilter);
      end else
        for lListCount := 0 to lTopTableKeyList.Header.ItemCount-1 do
        begin
          lRecord := ReadRecordFromKeyItem(lTopTableKeyList, lListCount);
          OutputSingleRecordContent(lRecord, iIndent, iFromExportFilter);
        end;
      { Make sure progress bar only progresses - difficult as the total count
           can change! }
      lNewProgress := i * 100 div dmNBNXML.KeyList.Header.ItemCount;
      if lNewProgress > frmMain.ProgressBar.TaskPosition then
        frmMain.ProgressBar.TaskPosition := lNewProgress;
    except
      on E:EDTDNotComplete Do begin // DTD doesn't specify a table we join to - ignore it
        dmNBNXML.Log(Format(ResStr_RecordIgnored, [lRecord.TableName]), E);
        FDocument.Outputter.FileSize := lFileSize;
      end;
      on E:EDataNeededError do begin
        dmNBNXML.Log(Format(ResStr_RecordDropped,[lRecord.TableName]), E);
        FDocument.Outputter.FileSize := lFileSize;
      end;
    end; // try..except
  end; // while
  FDocument.Outputter.NewLine(iIndent-1);
  FDocument.Outputter.OutputText('</' + FName + '>');
end;


{ Reads a record identifier structure from a single item on a key list.  Copes
     with MIXED or normal items }
function TElement.ReadRecordFromKeyItem( iList: TKeyList;
                               const iIndex : integer): TRecordIdentifier;
begin
  if iList.Header.TableName = MIXED_DATA then // if mixed, table is stored in key2
  begin
    Result.TableName := iList.Items[iIndex].KeyField2;
    Result.Key2 := '';
  end
  else
  begin
    Result.TableName := iList.Header.TableName;
    Result.Key2 := iList.Items[iIndex].KeyField2;
  end;
  Result.Key1 := iList.Items[iIndex].KeyField1;
  Result.KeyField := '';
end;



{ Initiates the output for an element which represents a single record from the
     output key list }
procedure TElement.OutputSingleRecordContent(iRecord: TRecordIdentifier;
          const iIndent : integer; iFromExportFilter: Boolean);
var
  lDataset : TDataset;
begin
  // For already output data, avoid creating the dataset
  if (iRecord.Key2 <> '') or (not dmNBNXML.AlreadyOutput(iRecord.TableName, iRecord.Key1)) then
  begin
    lDataset := dmNBNXML.GetQueryObject(iRecord);
    try
      { Don't output restricted data, unless we have to }
      if (not dmNBNXML.RestrictedData(lDataset, iFromExportFilter))then
      begin
        (*****)
        dmNBNXML.RememberOutput(lDataset);
        TXMLDoc(FDocument).GetElementByName(
                  Lowercase(iRecord.TableName)).
                  DoOutput(lDataset, iIndent, nil, iFromExportFilter);
      end;
    finally
      dmNBNXML.ReleaseQueryObject(lDataset);
    end; // try..finally
  end; // if
end;



{ Initiates the output of termlist information at the end of the XML file.  This
     is handled in a different fashion to normal elements - the DTD is tightly
     defined so there is no need for a flexible, recursive output.  Therefore
     information output can be hard coded }
procedure TElement.DoTermlistOutput(const iIndent: integer);
var
  i : integer;
  lCurrentTermTable : string;
begin
  if dmNBNXML.TermsToOutput.Count = 0 then
    raise EDataNeededError.Create(ResStr_NoTerms);
  lCurrentTermTable := '';
  FDocument.Outputter.OutputText('>');
  with dmNBNXML.TermsToOutput do
  begin
    Application.ProcessMessages;
    if Assigned(FDocument.Outputter) and FDocument.Outputter.Cancelled then
      raise ECancellation.CreateNonCritical(ResStr_Cancelled);
    { Output terms in table order.  Don't set sorted to True, as we need to be
       able to append items to the list later on without resorting the order }
    Sort;
    i := 0;
    { Use while loop as the items to process may increase as we go }
    while i < Count do
    begin
      frmMain.ProgressBar.TaskPosition := i * 100 div Count;
      { Skip item in loop if system_supplied }
      if (not dmGeneralData.IsSystemSupplied(Names[i], GetStringPairValue(dmNBNXML.TermsToOutput[i]))) or
          AppSettings.CanExportSystemSupplied then
      begin
        if Names[i] <> lCurrentTermTable then
        begin
          if lCurrentTermTable <> '' then
          begin
            FDocument.Outputter.NewLine( iIndent + 1);
            FDocument.Outputter.OutputText('</termlist>');
            { DO WE HAVE TO OUTPUT A SOURCE AND DESCRIPTION HERE ?}
          end;
          lCurrentTermTable := Names[i];
          FDocument.Outputter.NewLine( iIndent + 1);
          FDocument.Outputter.OutputText( '<termlist term_list_name="'
                                          + lCurrentTermTable + '" type="nbn">' );
          OutputTermListDescription( lCurrentTermTable, iIndent+2 );
        end;
        OutputTermItem( i, iIndent + 2 );
      end; // not system_supplied
      Inc(i);
    end;
  end;
  if lCurrentTermTable <> '' then
  begin
    FDocument.Outputter.NewLine( iIndent + 1);
    FDocument.Outputter.OutputText('</termlist>');
  end;
  FDocument.Outputter.NewLine(iIndent);
  FDocument.Outputter.OutputText('</' + TERMLISTS + '>');
end;



{ Outputs the description tag for each termlist, reading the information from
     the TERM_LIST table.  If not present, then no tag is put in (as its
     optional).  }
procedure TElement.OutputTermListDescription( const iListname : string;
                                              const iIndent : integer );
var
  lDescription : string;
begin
  lDescription := dmNBNXML.GetTermlistDescription( iListName );
  if lDescription <> '' then // skip blank description
  begin
    FDocument.Outputter.NewLine(iIndent);
    FDocument.Outputter.OutputText('<description>' + lDescription +
                                                   '</description>');
  end;
end;


{ Output a single termlist item }
procedure TElement.OutputTermItem(const iIndex, iIndent: integer);
var
  lRecord : TRecordIdentifier;
  lDataset : TDataset;
  lstPrimaryKey : string;
begin
  { Find the data we need to output }
  lRecord.TableName := dmNBNXML.TermsToOutput.Names[iIndex];
  lRecord.Key1 := GetStringPairValue( dmNBNXML.TermsToOutput[iIndex] );
  lRecord.Key2 := '';
  lRecord.KeyField := '';
  lDataset := dmNBNXML.GetQueryObject(lRecord);
  try
    FDocument.Outputter.NewLine( iIndent);
    lstPrimaryKey := dmDatabase.GetPrimaryKey(lRecord.TableName, False);
    OutputTermJustTag(lDataset, Copy(lstPrimaryKey, 1, 8));
    with FDocument.Outputter do
    begin
      NewLine(iIndent+1);
      OutputText('<term_key type="nbn">' + lDataset.FieldByName(lstPrimaryKey).AsString +
                            '</term_key>');
      NewLine(iIndent+1);
      OutputText('<term>' + SafeText(lDataset.FieldByName(TERM_SHORT).AsString) + '</term>');
      if lDataset.FieldByName(TERM_FULL).AsString <> '' then
      begin
        NewLine(iIndent+1);
        OutputText('<long_name>' + SafeText(lDataset.FieldByName(TERM_FULL).AsString) +
                                 '</long_name>');
      end;
      if lDataset.FieldByName('DESCRIPTION').AsString <> '' then
      begin
        NewLine(iIndent+1);
        OutputText('<term_definition>');
        OutputBlob( lDataset.FieldByName('DESCRIPTION') );
        OutputText('</term_definition>');
      end;
      OutputAdditionalFields(lDataset, iIndent+1);
      FDocument.Outputter.NewLine( iIndent);
      FDocument.Outputter.OutputText( '</termlist_item>');
    end;
  finally
    dmNBNXML.ReleaseQueryObject(lDataset);
  end; //try..finally
end;


{ Return the value part of a name=value pairing, for a given index in the
     stringlist }
function TElement.GetStringPairValue( const iNameValue : string ) : string;
begin
  { Can't use values to read key, as name part is not unique }
  Result := Copy( iNameValue, SmartPos('=', iNameValue) + 1, 255);
end;


{ OutputTermJustTag outputs the opening Tag of a single term item - including
    he attributes.  Eg <termlist_item entered_by="1" entry_date="01/01/1999"> }
procedure TElement.OutputTermJustTag(iDataset: TDataset; const Creator: string);
begin
  FDocument.Outputter.OutputText( '<termlist_item ');
  if iDataset.FieldByName('SYSTEM_SUPPLIED_DATA').AsBoolean then
    FDocument.Outputter.OutputText('system_supplied_data="Yes" ');  // we can omit default of "No"
  FDocument.Outputter.OutputText( 'entered_by="' +
                    iDataset.FieldByName('ENTERED_BY').AsString + '" ' );
  { Note we must not output time part of the string }
  FDocument.Outputter.OutputText( 'entry_date="' +
               DateToStr(iDataset.FieldByName('ENTRY_DATE').AsDateTime) + '"' );
  { Changed by is optional }
  if iDataset.FieldList.IndexOf('CHANGED_BY')<>-1 then
  begin
    if iDataset.FieldByName('CHANGED_BY').AsString <> '' then
    begin
      FDocument.Outputter.OutputText( ' changed_by="' +
                    iDataset.FieldByName('CHANGED_BY').AsString + '" ' );
      FDocument.Outputter.OutputText( 'changed_date="' +
              DateToStr(iDataset.FieldByName('CHANGED_DATE').AsDateTime) + '"' );
    end;
  end;
  if iDataset.FieldByName('CUSTODIAN').AsString <> Creator then
    FDocument.Outputter.OutputText(' custodian="' + iDataset.FieldByName('CUSTODIAN').AsString + '"');
  FDocument.Outputter.OutputText( '>');
end;



{ Procedure To output any specialised fields that come with a termlist.  Also
    adds any linked terms to the termlist output by looking for relationships
    to the additional field. }
procedure TElement.OutputAdditionalFields(iDataset : TDataset; iiIndent : integer);
var
  i : integer;
begin
  // measurement type is a special case - must output context into additional
  // fields element
  if Uppercase(dmNBNXML.GetDatasetTable(iDataset)) = 'MEASUREMENT_TYPE' then
    OutputMeasurementTypeContext(iDataset, iiIndent)
  else begin
    for i := 0 to iDataset.Fields.Count-1 do
    begin
      { Look for any fields not already covered }
      if (CompareText(iDataset.Fields[i].FieldName,
          dmDatabase.GetPrimaryKey(dmNBNXML.GetDatasetTable(iDataset), False))<>0) and
         (CompareText(iDataset.Fields[i].FieldName, 'SHORT_NAME')<>0) and
         (CompareText(iDataset.Fields[i].FieldName, 'LONG_NAME')<>0) and
         (CompareText(iDataset.Fields[i].FieldName, 'DESCRIPTION')<>0) and
         (CompareText(iDataset.Fields[i].FieldName, 'ENTERED_BY')<>0) and
         (CompareText(iDataset.Fields[i].FieldName, 'ENTRY_DATE')<>0) and
         (CompareText(iDataset.Fields[i].FieldName, 'CHANGED_BY')<>0) and
         (CompareText(iDataset.Fields[i].FieldName, 'CHANGED_DATE')<>0) and
         (CompareText(iDataset.Fields[i].FieldName, 'RECORDING_CARD')<>0) and
         (CompareText(iDataset.Fields[i].FieldName, 'SYSTEM_SUPPLIED_DATA')<>0) then
      begin // we have an extra field to output
        { If we have any additional fields, then we build the relationships list
            for the current table }
        if iDataset.fields[i].Text<>'' then
        begin
          FDocument.Outputter.NewLine( iiIndent );
          FDocument.Outputter.OutputText('<additional_field specifier="' +
                              iDataset.Fields[i].FieldName + '">');
          TXMLDoc(FDocument).OutputField(iDataset.Fields[i], true, false);
          FDocument.Outputter.OutputText('</additional_field>');
        end;
      end;
    end;
  end; // for
  { Add any linked terms to the list to output }
  CheckForLinkedTerms(iDataset);
end;


// Description : measurement type context must be included as a special element for measurement
//       types
// Created : 12/09/02
procedure TElement.OutputMeasurementTypeContext(iDataset : TDataSet; iIndent : integer);
var
  lstTypeKey : string;
begin
  lstTypeKey := iDataset.FieldByName('measurement_type_key').AsString;
  with dmGeneralData.qryAllPurpose do begin
    Sql.Text := 'select measurement_context_key from measurement_type_context ' +
             'where measurement_type_key=''' + lstTypeKey + '''';
    Open;
    try
      // loop through each context and output it
      while not EOF do begin
        FDocument.Outputter.NewLine( iIndent);
        FDocument.Outputter.OutputText('<additional_field specifier="' +
                            'measurement_context_key">');
        TXMLDoc(FDocument).OutputField(FieldByName('measurement_context_key'), true, false);
        FDocument.Outputter.OutputText('</additional_field>');
        Next;
      end;
    finally
      Close;
    end;
  end;
end;


{ CheckForLinkedTerms looks for a relationship current termlist is a detail
     table.  If so, then ensures that the master termlist table also goes out. }
procedure TElement.CheckForLinkedTerms(iDataset : TDataset);
var
  i : integer;
  lRelationships : TRelationshipArray;
  lTermString : string;
begin
  lRelationships := dmDatabase.Relationships.FindDependencies(
                        dmNBNXML.GetDatasetTable(iDataset));
  { Add the extra linked terms to the output list, if required }
  for i:=0 to High(lRelationships) do begin
    lTermString := lRelationships[i].MasterTable + '=' +
         iDataset.FieldByName(lRelationships[i].Fields[0].DetailName).AsString;
    if dmNBNXML.TermsToOutput.IndexOf(lTermString) = -1 then
      dmNBNXML.TermsToOutput.Add(lTermString);
  end;
end;



{ Called by an attlist when it outputs a field.  Determines that the element
     should access a database field with '_' + fieldname prefixed onto it }
procedure TElement.SetField(const iFieldName: string);
begin
  FFieldName := iFieldName;
end;



//==============================================================================
{ TXMLDoc }
//==============================================================================



constructor TXMLDoc.Create( const iFileName : string; iOutputter : TFileOutput;
                iProgressBar : TTaskProgressBar );
begin
  InitDateTimeFormats;
  try
    dmNBNXML := TdmNBNXML.Create(Self);
    { List to hold any blocks of CDATA in the file which can't be treated as strings }
    FCdataStreams := TList.Create;
    inherited Create( iFilename, iOutputter, iProgressBar );
  except
    { Catch all errors just so we know where they occurred.  MarkupExceptions
         use a different form to display the error. }
    On E:TMarkupException do
    begin
      if E.Row <> -1 then
      begin
        frmXMLReport := TfrmXMLReport.Create(nil, E, Self);
        try
          frmXMLReport.ShowModal;
        finally
          frmXMLReport.Release;
        end; // try.. finally
        raise;
      end else
        raise EXMLDocError.Create(ResStr_DTDProblem + IntToStr(DocItemCount), E);
    end; // exception handler
    on E:Exception do
      raise EXMLDocError.Create(ResStr_DTDProblem + IntToStr(DocItemCount), E);
  end; // try..except
end;


{ Set up date formats for consistency }
procedure TXMLDoc.InitDateTimeFormats;
begin
  FOldDateFormat := ShortDateFormat;
  ShortDateFormat := DATE_FORMAT;
  FOldTimeFormat := LongTimeFormat;
  LongTimeFormat := TIME_FORMAT;
end;


{ Reset old date time formats }
procedure TXMLDoc.ResetDateTimeFormats;
begin
  ShortDateFormat := FOldDateFormat;
  LongTimeFormat  := FOldTimeFormat;
end;


{ Destructor - just cleanup stuf including the list of restricted data lists }
destructor TXMLDoc.Destroy;
var i : integer;
begin
  if FCdataStreams <> nil then
    for i := 0 to FCdataStreams.Count-1 do
      TMemoryStream(FCdataStreams[i]).Free;
  FCdataStreams.Free;
  dmNBNXML.Free;
  ResetDateTimeFormats;
  FFileStream.Free;
  FBlockStream.Free;
  inherited Destroy;
end;


{ locate the element within the DTD which matches the DocType declaration.  If
     not found, an exception is raised.  This element is the one we must start
     processing at }
function TXMLDoc.DocTypeElement: TElement;
var
  i : integer;
  lDocTypeName : string;
begin
  Result := nil; // default - never returned
  lDocTypeName := '';
  { Find the name }
  for i := 0 to DocItemCount-1 do
  begin
    if DocItems[i] is TDocType then
    begin
      lDocTypeName := TDocType(DocItems[i]).Name;
      break; // from loop - found the DocType
    end;
  end;
  { Find the matching element }
  for i := 0 to DocItemCount-1 do
  begin
    if (DocItems[i] is TElement) and (lDocTypeName <> '') then
      if TElement(DocItems[i]).Name = lDocTypeName then // found
      begin
        Result := TElement(DocItems[i]);
        break; // from for loop as we have found our result
      end;
  end; // for
  if Result=nil then
    raise EDTDNotComplete.Create(ResStr_ItemNotFound + lDocTypeName);
end;



{ Return any DocItem from the document, according to the Name and DocItemClass
     requested }
function TXMLDoc.GetItemByName(const iName: string;
                            iClass: TDocItemClass): TAbstractDocItem;
var
  lIndex : integer;
begin
  lIndex := FDocItems.IndexOf(iClass.ClassName + '=' + iName);
  if lIndex = -1 then
    raise EDTDNotComplete.CreateNonCritical(ResStr_ItemNotFound + iName)
  else
    Result := TAbstractDocItem(FDocItems.Objects[lIndex]);
end;


{ Function to return an attlist by name.  Uses the protected GetItemByName
    function, and typecasts the result }
function TXMLDoc.GetAttlistByName(const iName: string): TAttlist;
begin
  Result := TAttlist( GetItemByName( iName, TAttlist ) );
end;


{ Function to return an element by name.  Uses the protected GetItemByName
    function, and typecasts the result }
function TXMLDoc.GetElementByName(const iName: string): TElement;
begin
  Result := TElement( GetItemByName( iName, TElement ) );
end;


{ Function to return an entity by name.  Uses the protected GetItemByName
    function, and typecasts the result }
function TXMLDoc.GetEntityByName(const iName: string): TEntity;
begin
  Result := TEntity( GetItemByName( iName, TEntity ) );
end;


{ Returns a tag by name.  This will locate the first tag only - really only for
    finding unique tags such as content or metadata }
function TXMLDoc.GetTagByName(const iName: string): TTag;
begin
  Result := TTag( GetItemByName( iName, TTag ) );
end;


{ Override TMarkupDocument.ProcessDocument to provide an error handler to
    display the XML error report }
function TXMLDoc.ProcessDocument(iMustBeXML: boolean): TTag;
begin
  try
    Result := inherited ProcessDocument(iMustBeXML);
  except
    On E:TMarkupException do
    begin
      if E.Row <> -1 then
      begin
        frmXMLReport := TfrmXMLReport.Create(nil, E, Self);
        try
          frmXMLReport.ShowModal;
        finally
          frmXMLReport.Release;
        end; // try.. finally
        raise;
      end else
        raise EXMLDocError.Create(ResStr_DTDProblem + IntToStr(DocItemCount), E);
    end; // exception handler
  end; // try..except
end;


{ Procedure to display the document in a rich edit, with a particular line
     highlighted }
procedure TXMLDoc.PopulateErrorReport(iRichEdit: TRichEdit; const iRow : integer;
              const iPosition: Int64);
var
  lOldCursor : TCursor;
  lStartRow, lEndRow : integer;
  i : integer;
  lFileStrings : TStringList;
begin
  lOldCursor := HourglassCursor;
  lFileStrings := TStringlist.Create;
  try
    FFileStream.Position := iPosition;
    if iPosition>-1 then
      LoadFile(lFileStrings, FileName)
    else
      lFileStrings.Assign(DocParser.DocStrings);

    iRichEdit.Font.Color := clGrayText;
    iRichEdit.SelAttributes.Color := clGrayText;
    iRichEdit.SelAttributes.Style := [];
    if iRow < 250 then        // up to 250 lines before error
      lStartRow := 0
    else
      lStartRow := iRow - 250;
    if iRow > lFileStrings.Count-250 then
      lEndRow := lFileStrings.Count-1
    else
      lEndRow := iRow + 250;
    for i := lStartRow to lEndRow do
      iRichEdit.Lines.Add(lFileStrings[i]);
    { Locate the start of the error }
    iRichEdit.SelStart := SendMessage( iRichEdit.Handle, EM_LINEINDEX,
                                       iRow-lStartRow, 0 );
    iRichEdit.SelLength := Length(iRichEdit.Lines[iRow-lStartRow]);
    { Highlight the error }
    iRichEdit.SelAttributes.Color := clHighlight;
    iRichEdit.SelAttributes.Style := [fsBold];
    { scroll the error into the visible region }
    SendMessage( iRichEdit.Handle, EM_LINESCROLL, 0, 0 - iRichedit.Lines.Count );
    SendMessage( iRichEdit.Handle, EM_LINESCROLL, 0, iRow );
    iRichEdit.SelLength := 0;
  finally
    lFileStrings.Free;
    DefaultCursor(lOldCursor);
  end; // try..finally
end;


{ SearchForContainedContentByName identifies if a element name forms part of the
     content model of any other elements.  The element is only included if there
     is a relationship between the two elements where the container (the one to
     be returned) is the master side of the master-detail relationship.  Returns
     a single element (or nil) and raises an exception if more than one element
     is found. }
function TXMLDoc.SearchForContainedContentByName( const iName: string ): TElement;
var
  i : integer;
  lElementList : TStringlist;
begin
  lElementList := TStringList.Create;
  try
    { Loop through the doc items }
    for i := 0 to DocItemCount-1 do
    begin
      { Only look at the elements }
      if DocItems[i] is TElement then
      begin
        if TElement(DocItems[i]).MixedContent <> nil then // ie not ANY or EMPTY content
          if TElement(DocItems[i]).MixedContent.ContainsElement( iName ) then // element found
            lElementList.Add(TElement(DocItems[i]).Name);
      end; // with
    end;
    if lElementList.Count = 1 then
      Result := GetElementByName(lElementList[0])
    else // none, or more than one.
      Result := nil;
  finally
    lElementList.Free;
  end; // try..finally
end;





{ Overriden method to make sure we get the right kind of DocParser }
procedure TXMLDoc.CreateDocParser;
begin
  FDocParser := TXMLDocParser.Create(Self);
end;


{ Function to read a CDATA stream from the source stream.  The length (a hex string)
    is supplied as a parameter.  The new stream is added to the data streams
    list and its index (to be inserted into the source file) is returned }
function TXMLDoc.ReadNewStream( iStream: TStream; iSize : integer): integer;
var
  lNewStream : TMemoryStream;
  lHTMLStream : TMemoryStream;
  lCloseString : array [0..1] of char;
  lHTMLOpenTagString : array [0..5] of char;
  lChar : char;
begin
  lNewStream := TMemoryStream.Create;
  try
    if iSize <> 0 then // trap this case because CopyFrom reads entire stream if you ask for 0 bytes
      if lNewStream.CopyFrom( iStream, iSize )<>iSize then
        raise EStreamError.Create(ResStr_WriteMisMatch);
  except on E:EStreamError do
    raise EXMLDocError.Create(ResStr_InvalidStream + ResStr_ProbableProblem, E);
  end;
  { Check the stream terminates correctly }
  iStream.Read( lCloseString, 2 );
  if lCloseString <> ']]' then
    raise EXMLDocError.Create(ResStr_InvalidStream + ResStr_IncorrectPosTermString);
  { move to stream start and check for HTML conversions }
  lNewStream.Seek(0, soFromBeginning);
  lNewStream.Read( lHTMLOpenTagString, 6 );
  if CompareText( lHTMLOpenTagString, '<html>' )=0 then
  begin { Create a new stream to hold RTF, translate HTML into it }
    lNewStream.Seek(0, soFromBeginning);
    lHTMLStream := lNewStream;
    lNewStream := TMemoryStream.Create;
    StreamHTMLToRTF( lHTMLStream, lNewStream, 0 );
    lHTMLStream.Free;
  end;
  Result := FcdataStreams.Add( lNewStream );
  { Read until we find the '>' }
  iStream.Read( lChar, 1 );
  while lChar <> '>' do
    iStream.Read( lChar, 1 );
end;



{ Overriden means of reading files.  Needed for XML since the file may not be
     a valid string list if <![CDATA[..]> is used.  Loads info into a temporary
     memory stream, the outputs it char by char into the Filestrings.  When
     binary information is encountered, it is loaded into a stream, and the
     stream is stored on a list.  The index of the stream is written into the
     FileStrings so that the stream can be accessed later. }
     { CCN Change - only loads 1 survey event at a time, otherwise we use too
         much memory }
procedure TXMLDoc.LoadFile(iFileStrings: TStrings; const iFileName: string);
var
  lNextChar : char;
  lTempPosition : integer;
  lTagTest : array [0..13] of char;
const
  BLOCK_SIZE = $FFFF; // arbitrary amount of data to read in one go
begin
  { For first time use, set up streams to read file with }
  if FFileStream=nil then
    CreateStreams( iFileName );
  { Cast to int64 to avoid overflow - check to avoid DivByZero}
  if FFileStream.Size > 0 then
    fPercentStart := Int64(FFileStream.Position) * 100 div Int64(FFileStream.Size)
  else
    raise EXMLDocError.CreateNonCritical(ResStr_EmptyFile);
  iFileStrings.Clear; // reset the string list
  iFileStrings.Add('');
  FRow := 0;
  FPos := 1;
  FCurrentBlockStart := FFileStream.Position; // record current block start point
  { while there is any data left to read }
  while FFileStream.Size - FFileStream.Position>0 do
  begin
    FBlockStream.CopyFrom( FFileStream,
               Min(BLOCK_SIZE, FFileStream.Size - FFileStream.Position) );
    FBlockStream.Seek(0, soFromBeginning);
    while (FBlockStream.Position < FBlockStream.Size-15) or { cannot use last 15 bytes in case of overlap }
          ((FFileStream.Size = FFileStream.Position) and
           (FBlockStream.Position < FBlockStream.Size)) do
    begin
      FBlockStream.Read( lNextChar, 1 );
      case lNextChar of
        #13 :{ Add a new blank line for the carriage return so we can ignore #10 }
          iFileStrings.Add('');
        #10 : ; // do nothing - see above
        '<' :
        begin
          lTempposition := FBlockStream.Position; // remember so we can skip back
          FBlockStream.Read( lTagTest, 14);
          if Copy(lTagTest, 1, 8) = '![CDATA[' then  // read the stream, write its index into the file
          begin
            if not CDataFoundInStream( iFileStrings, lTagTest ) then
            begin
              FBlockStream.Position := lTempposition-1;
              break; // the stream overlapped a block boundary, so start it again
            end;
          end else
          if lTagTest='/survey_event>' then // closing a survey event, so stop
          begin
            SurveyEventFoundInStream( iFileStrings, lTagTest );
            Exit; // finished reading for now - got 1 survey_event to process
          end else
          begin
            { Not CDATA or survey_event so wind back and treat as normal }
            FBlockStream.Position := lTempposition;
            iFileStrings[iFileStrings.Count-1] := iFileStrings[iFileStrings.Count-1]
                                                   + lNextChar;
          end;
        end;
      else
        iFileStrings[iFileStrings.Count-1] := iFileStrings[iFileStrings.Count-1]
                                           + lNextChar;
      end; // case
    end; // while reading from blockstream
    { Allow for the unread overlap allowance }
    FFileStream.Position := FFileStream.Position - (FBlockStream.Size - FBlockStream.Position);
    FBlockStream.Clear;

  end; // while
  FPercentEnd := 100; // if we got this far, we must be at the end
end;


{ Procedure to create the file stream to load the file from, and the block
    stream which acts as a buffer to improve performance }
procedure TXMLDoc.CreateStreams( const iFileName : string );
begin
  FBlockStream := TMemoryStream.Create;
  try
    FFileStream := TFileStream.Create(iFileName,fmOpenRead);
  except
    on EFopenError do
      raise EXMLDocError.CreateNonCritical(ResStr_CannotOpen + iFileName);
  end;
end;



procedure TXMLDoc.SurveyEventFoundInStream( iFileStrings : TStrings; const iTagTest : string );
begin
  iFileStrings[iFileStrings.Count-1] := iFileStrings[iFileStrings.Count-1]
                                         + '<' + iTagTest;
  { Move the filestream position to the right place for next time }
  FFileStream.Position := FFileStream.Position - FBlockStream.Size + FBlockStream.Position;
  { Set for progress bar.  Must cast to Int64 so we don't overflow! }
  fPercentEnd := Int64(FFileStream.Position) * 100 div Int64(FFileStream.Size);
  FBlockStream.Clear;
end;


{ When CDATA section found in XML file, must read it as a stream rather than
     text, as it could contain anything.  We insert the stream's index into
     the XML file so we can relocate the stream during parsing.
     Returns true if loop should continue or false if it should break. }
function TXMLDoc.CDataFoundInStream( iFileStrings : TStrings; const iTagTest : string ): boolean;
var
  lStreamSize : integer; // gives 2Gb limit - should be OK!
begin
  Result := true; // default - OK to continue
  { Check for stream size specifier - part of NBN standard for using CDATA }
  try
    lStreamSize := StrToInt('$' + Copy(iTagTest, 9, 6));
  except on E:Exception do
    raise EXMLDocError.Create(ResStr_InvalidStream + 'Problem reading 32 bit size specifier.',E);
  end;
  if (lStreamSize+3) > (FBlockStream.Size - FBlockStream.Position) then // 3 allows fo ]]> at end of stream
  begin
    { Our stream overlaps a chunk boundary, so wind back and load next chunk }
    Result := false; // break from while loop through FBlockStream
  end else
    { Insert the index into the XML file }
    iFileStrings[iFileStrings.Count-1] := iFileStrings[iFileStrings.Count-1] +
         '<![CDATA[' +
         IntToStr(ReadNewStream( FBlockStream, lStreamSize )) + ']]>'
end;


{ Return a memory stream by index value the value written into the CDATA
      block in the parsed stringlist }
function TXMLDoc.GetCData(const iIndex: integer): TMemoryStream;
begin
  if iIndex < FCdataStreams.Count then
    Result := TMemoryStream(FCdataStreams[iIndex])
  else
    raise EXmlDocError.Create(ResStr_NoCData + IntToStr(iIndex));
end;


{ Procedure to output a field (blob or otherwise) into the document.  If
    allowBlank is false, then an exception is raised if the field contents
    is blank.  The iAsString parameter is used to indicate that the field
    data must be read as string, not text (therby bypassing the get and set
    text methods }
procedure TXMLDoc.OutputField(iField: TField; iAllowBlank: Boolean; iAsString : boolean);
var
  lFieldText : string;
begin
  { Blob streams need to be marked up as such so the parser knows to 'jump' it }
  if (iField.IsBlob) then // anything- memos, bitmaps
    Outputter.OutputBlob( TBlobField(iField) )
  else
  begin
    if iAsString then
      lFieldText := iField.AsString
    else
      lFieldText := iField.Text;
    if (lFieldText='') and (iAllowBlank=false) then
      raise EDataNeededError.Create(ResStr_ItemDataMissing + iField.FieldName);
    if iField.DataType <> ftDateTime then
      Outputter.OutputText(Outputter.SafeText(lFieldText))
    else
    begin
      if lFieldText <> '' then
      begin
        { If the fieldname contains TIME, output as a time, otherwise output as date }
        if SmartPos(TIME,iField.FieldName)<>0 then
          Outputter.OutputText (TimeToStr(iField.AsDateTime))
        else
          Outputter.OutputText (DateToStr(iField.AsDateTime));
      end;
    end;
  end;
end;



//==============================================================================
{ TAttribute }
//==============================================================================

{ Create the attribute and store all the parameters for future use }
constructor TAttribute.Create( const iName : string; const iDataType : TAttrType;
            iEnumerations : TStringList; const iUsage :TAttrUsage;
            const iDefault : string );
begin
  inherited Create;
  FName := iName;
  FDataType := iDataType;
  FEnumerations := iEnumerations;
  Fusage := iUsage;
  FDefault := iDefault;
end;


{ destructor - cleanup lists }
destructor TAttribute.Destroy;
begin
  FEnumerations.Free;
  inherited Destroy;
end;



//==============================================================================
{ TComment }
//==============================================================================

{ Create a comment by reading it from the current position in the file.  Reads
     the comment into FText, and finishes when '-->' is found }
constructor TComment.CreateFromStrings(iParent : TAbstractDocItem;
    iDocument: TMarkupDocument; var ioRow, ioPos: integer);
var
  lOldRow, lOldPos : integer;
  i : integer;
begin
  inherited CreateFromStrings( iParent, iDocument );
  lOldRow := ioRow;
  lOldPos := ioPos;
  { Skip to comment end }
  iDocument.DocParser.SearchFor(stFindWords, ['-->'], ioRow, ioPos);
  FText := '';
  for i := lOldRow to ioRow do
    if i = lOldRow then // first row
      FText := FText + Copy(iDocument.DocParser.DocStrings[i], lOldPos, High(integer))
    else
      if i = ioRow then // last row
        FText := FText + Copy(iDocument.DocParser.DocStrings[i], 1, ioPos)
      else // all other rows
        FText := FText + iDocument.DocParser.DocStrings[i];
  inc(ioPos, 3);
end;



//==============================================================================
{ TDocType }
//==============================================================================

{ Create from Strings.  Reads a doc type instance from the document location.
     Locates the name of the document type (this is always the first word after
     <!DOCTYPE) and moves the position to the character after the [ marker.
     Puts the instance onto the stack as it remains open. }
constructor TDocType.CreateFromStrings(iParent : TAbstractDocItem;
    iDocument: TMarkupDocument; var ioRow, ioPos: integer);
begin
  inherited CreateFromStrings( iParent, iDocument );
  { Initialise stuff for safety }
  FNotation := '';
  FDTDFile := '';
  { Confirm where looking at a doctype directive }
  ConfirmType('DOCTYPE', ioRow, ioPos);
  FName := iDocument.DocParser.GetNextWord( ioRow, ioPos );
  Inc(ioPos, Length(FName)+1);
  iDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos );
  { Record that we are a doc type and we are open! }
  iDocument.MarkupStack.Push(Self);
  { Read SYSTEM or PUBLIC (optional) }
  if iDocument.DocParser.GetChar(ioRow, ioPos) <> '"' then
  begin
    FExternalID := iDocument.DocParser.GetNextWord( ioRow, ioPos );
    if (FExternalID <> 'SYSTEM') and (FEXternalID <> 'PUBLIC') then
      raise EDocTypeError.Create(ResStr_ExtIdInvalid, ioRow, TXMLDoc(iDocument).CurrentBlockStart);
    Inc(ioPos, Length(FExternalID)+1);
    iDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos );
  end;
  { read the file location }
  if iDocument.DocParser.GetChar(ioRow, ioPos) = '"' then
    ReadFile( ioRow, ioPos )
  else
    raise EDocTypeError.Create(ResStr_DocTypeFile, ioRow, TXMLDoc(iDocument).CurrentBlockStart);
  iDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos );
  if iDocument.DocParser.GetChar(ioRow, ioPos) = '[' then
    FCloseChar := ']'
  else
  begin
    FCloseChar := '>';
    Dec(ioPos);
  end;
end;


{ Reads the external DTD declaration.  This file is processed only when the
    DOCTYPE element is closed }
procedure TDocType.ReadFile(var ioRow, ioPos: integer);
begin
  if FDocument.DocParser.GetChar(ioRow, ioPos) = '"' then
  begin
    Inc(ioPos);
    { Do we have a notation specification? }
    if FDocument.DocParser.GetChar(ioRow, ioPos) = '-' then
    begin
      { Read the notation }
      FNotation := FDocument.DocParser.GetAllUpTo('"', ioRow, ioPos);
      { Locate the next '"' }
      FDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
    end;
    if FDocument.DocParser.GetChar(ioRow, ioPos) = '"' then
    begin
      { Read remote file location }
      Inc(ioPos);
      { Read the remote file }
      FDTDFile := FDocument.DocParser.GetAllUpTo('"', ioRow, ioPos);
    end;
  end;
  if FNotation <> NBN_NOTATION then
    raise EDocTypeError.Create(ResStr_InvalidNotation, ioRow, TXMLDoc(FDocument).CurrentBlockStart);
  { Record the lines we have processed in this document for progress bar }
  FDocument.ProcessedLines := ioRow;
end;


{ Actually do the processing of an external DTD }
procedure TDocType.ProcessExternalDoc;
var
  lFileStrings : TStringList;
begin
  { Note - there is no try..finally to free lFilestrings.  This is because an
       exception probably means we will need to display the error report dialog
       which requires the file }
  lFileStrings := TStringlist.Create;
  lFileStrings.LoadFromFile(AppSettings.DTDPath + DTD_FILE);
  { Tell the document that we are embedding the external DTD }
  FDocument.EmbedDocument(lFileStrings);
  { Process it }
  FDocument.ProcessDocument(False);
  { And finish off }
  FDocument.FinishEmbeddedDoc;
  lFileStrings.Free;
end;



//==============================================================================
{ TAttList }
//==============================================================================

{ Create an attlist by reading it from the current position in the file.  The
     attribute definition and all the attributes are read. }
constructor TAttList.CreateFromStrings(iParent : TAbstractDocItem;
            iDocument: TMarkupDocument; var ioRow, ioPos: integer);
begin
  { NOTE - If attlist already exists, need to add items to the existing one }
  if iDocument.MarkupStack.PeekTop is TDocType then
  begin
    inherited CreateFromStrings( iParent, iDocument );
    FDocument := iDocument;
    FAttributes := TList.Create;
    { Confirm we're looking at an Attlist }
    ConfirmType('ATTLIST', ioRow, ioPos);
    FName := iDocument.DocParser.GetNextWord( ioRow, ioPos );
    try
      { Check the element name is already declared }
      TXMLDoc(iDocument).GetElementByName( FName );
    except
      on E:EDTDNotComplete do
        raise EAttListError.Create(ResStr_CreateProblem + Format(ResStr_AttListHasInvalidElement,[FName]),
                                   E, ioRow, TXMLDoc(iDocument).CurrentBlockStart);
    end; // try
    Inc(ioPos, Length(FName)+1);
    ReadAttributes( ioRow, ioPos );
    iDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
    if iDocument.DocParser.GetChar( ioRow, ioPos ) <> '>' then
      raise EAttlistError.Create(ResStr_AttlistNotEnded, ioRow, TXMLDoc(iDocument).CurrentBlockStart);
  end
  else
    raise EAttListError.Create(ResStr_WrongParent +
          Format(ResStr_ContainsAttlistDir, [iDocument.MarkupStack.PeekTop.ClassName]),
          ioRow, TXMLDoc(iDocument).CurrentBlockStart);
end;



{ Destructor - cleanup attribute list }
destructor TAttList.Destroy;
var
  i : integer;
begin
  if FAttributes <> nil then // safety check
    for i := 0 to FAttributes.Count-1 do
      TAttribute(FAttributes[i]).Free;
  FAttributes.Free;
  inherited Destroy;
end;


{ Construct a string for all attributes according to a record string list }
procedure TAttList.DoOutput(const iRecord : TDataset);
var
  lstOutput : string;
  i : integer;
  lstData : string;
  lCurrentAttribute : TAttribute;
begin
  if Name='dataset_source' then
    DoDatasetSourceOutput
  else
  if Name='dataset_copy_origin' then
    DoDatasetCopyOriginOutput
  else
    try
      lstOutput := '';
      for i := 0 to FAttributes.Count-1 do
      begin
        lCurrentAttribute := TAttribute(FAttributes[i]);
        try
          { Fixed attributes - just use default otherwise try and read it from the db }
          try
            lstData := GetAttributeData( iRecord, lCurrentAttribute );
          except
            on E:EDatabaseError do // need proper class
              { Not present - Check Required status }
              raise EDataNeededError.Create(ResStr_AttlistNeedsData + lCurrentAttribute.Name
                          + ' in the ' + Name + ' Attlist.', E)
          end; // try..except
          if (lstData = '') then
          begin
            { Not present - Check Required status }
            raise EDataNeededError.Create(ResStr_AttlistNeedsData + lCurrentAttribute.Name
                    + ' in the ' + Name + ' Attlist.');
          end; //
          CheckAttributeAgainstData( lCurrentAttribute, lstData );
          { Build the output string for the attribute - we can assume it is implied now }
          if lstData <> '' then
            lstOutput := lstOutput + ' ' + lCurrentAttribute.Name + '="' + lstData + '"';
        except
          on EDataNeededError do // don't worry if not required
            if lCurrentAttribute.Usage = auRequired then
              raise;
        end; // try..except
        StoreAttForeignKeys( iRecord, lCurrentAttribute );
      end; // for loop
      FDocument.Outputter.OutputText( lstOutput );
    except
      on EDataNeededError do
        { don't handle these like other exceptions - allow EDataNeededError to
                pass through }
        raise;
      on E:Exception do
        raise EXMLError.Create(ResStr_GetOutputError + 'Attlist ' + FName , E)
    end; // try exception
end;


{ For attributes which represent foreign keys, store the item for output later }
procedure TAttlist.StoreAttForeignKeys( iRecord : TDataset; iAttr : TAttribute );
var
  lFieldName : string;
begin
  lFieldName := iAttr.Name;
  { Check that the fieldname is altered according to special xml elements table }
  dmNBNXML.HandleSpecialFields( lFieldName, iRecord );
  if iRecord.FieldList.IndexOf(lFieldName)<>-1 then
    dmNBNXML.StoreIfForeignKey(iRecord, lFieldName);
end;


{ Special case output for Dataset_source element }
procedure TAttList.DoDatasetSourceOutput;
begin
  FDocument.Outputter.OutputText(' entered_by="UNKNOWN" entry_date="UNKNOWN" ' +
              'completeness="partial" system_supplied_data="No"');
end;


{ Special case output for Dataset_source element }
procedure TAttList.DoDatasetCopyOriginOutput;
begin
  FDocument.Outputter.OutputText(' created_by="UNKNOWN" create_date="UNKNOWN"');
end;


{ Read the data from the dataset according to the current attribute.  Checks if
     the attribute name is one of our special 'keyword' attributes, and handles
     them accordingly. }
function TAttList.GetAttributeData(iRecord: TDataset;
                                            iAttribute : TAttribute): string;
var lFieldName : string;
begin
  if iAttribute.Usage = auFixed then
    Result := iAttribute.DefaultValue
  else
  begin
    { Look up the data for the attribute }
    if iAttribute.Name = DATA_SIZE then
      { Use our attlist name to find the field whose size we need }
      Result := IntToStr(iRecord.FieldByName( FName ).Size)
    else if iAttribute.Name = KEY_TYPE then
      Result := 'nbn'  // we always output nbn data, not free_terms CHECK OTHER TYPES
    else if iAttribute.Name = VALUE_TYPE then // hopefully SCRAP THIS
      Result := 'key'
    else if iAttribute.Name = FIELD_NAME then
    begin
      Result := FindFieldName( iRecord );
      { Pass information to the element so it knows what to output }
      TXMLDoc(FDocument).GetElementByName(FName).SetField(Result);
    end
    else
    begin
      lFieldName := iAttribute.Name;
      { Check that the fieldname is altered according to special xml elements table }
      dmNBNXML.HandleSpecialFields( lFieldName, iRecord );
      { For date/time fields, only output the date unless the field name
          specifically contains the word TIME }
      if iRecord.FieldByName( lFieldName ).DataType = ftDateTime then
      begin
        if iRecord.FieldByName( lFieldName ).IsNull then    // don't return 1899 (year zero!)
          Result := ''
        else if SmartPos(TIME, lFieldName)=0 then
          Result := DateToStr(iRecord.FieldByName( lFieldName ).AsDateTime)
        else
          Result := TimeToStr(iRecord.FieldByName( lFieldName ).AsDateTime);
      end else
        Result := iRecord.FieldByName( lFieldName ).Text;
    end;
  end; // usage not fixed
end;


{ Raises an EDataNeededError if the attribute's data is not valid }
procedure TAttList.CheckAttributeAgainstData(iAttribute: TAttribute;
  var ioData: string);
var
  lIndex : integer;
begin
  case iAttribute.DataType of
    atCData : ; // no check - anthing goes
    atNMTOKEN :
      if not CheckValidNMToken( ioData ) then
        raise EDataNeededError.Create(ResStr_AttributeCantOutput + iAttribute.Name
                  + ', ' + ioData);
    atEnumerated :
    begin
      { First check the data is OK }
      lIndex := iAttribute.Enumerations.IndexOf(ioData);
      if lIndex = -1 then
        raise EDataNeededError.Create(ResStr_AttributeCantOutput + iAttribute.Name
              + ', ' + ioData);
      { Now use the data off the list so we get the correct case according to
        DTD}
      ioData := iAttribute.Enumerations[lIndex];
    end;
  end; // case
end;



{Returns true if the input string is a valid XML NMToken}
function TAttList.CheckValidNMToken(const iToken: string): boolean;
var
  i : integer;
begin
  Result := True; // default
  for i := 1 to Length(iToken) do
    if not (iToken[i] in VALID_NMTOKEN_CHARS) then
    begin
      Result := False;
      Break; // from loop
    end; // if
end;


{ Read the list of attributes specified by an Attlist directive }
procedure TAttList.ReadAttributes(var ioRow, ioPos: integer);
var
  lName : string;
  lDataType : TAttrType;
  lUsage : TAttrUsage;
  lAttribute : TAttribute;
  lDefault : string;
  lEnumerations : TStringList;
begin
  FDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
  { Loop until we hit the attlist end }
  while FDocument.DocParser.GetChar(ioRow, ioPos) <> '>' do
  begin
    { read name }
    lName := FDocument.DocParser.GetNextWord(ioRow, ioPos);
    Inc(ioPos, Length(lName)+1);
    { read data type }
    FDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
    if FDocument.DocParser.GetChar( ioRow, ioPos ) = '(' then
    begin
      lDataType := atEnumerated;
      lEnumerations := ReadEnumerations( ioRow, ioPos );
    end
    else
    begin
      lEnumerations := nil; // not enumerated attribute
      lDataType := ReadDataType( ioRow, ioPos );
    end;
    { Read usage or default }
    lUsage := ReadUsage( ioRow, ioPos );
    if lUsage = auHasDefault then
      lDefault := ReadDefault( ioRow, ioPos, lEnumerations )
    else
      lDefault := '';
    lAttribute := TAttribute.Create( lName, lDataType, lEnumerations,
                                     lUsage, lDefault );
    FAttributes.Add(lAttribute);
    FDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
  end;
end;



{ Reads the next word from an attribute definition and converts it to a data
     type }
function TAttList.ReadDataType(var ioRow, ioPos: integer): TAttrType;
var
  lTempText : string;
begin
  lTempText := FDocument.DocParser.GetNextWord(ioRow, ioPos);
  if lTempText = 'CDATA' then
    result := atCDATA
  else if lTempText = 'NMTOKEN' then
    result := atNMTOKEN
  else
    Raise EAttlistError.Create(Format(ResStr_AttributeTypeNotSupported,[lTempText,lTempText]),
                               ioRow, TXMLDoc(FDocument).CurrentBlockStart);
  Inc(ioPos, Length(lTempText)+1);
end;


{ Reads a default string from the attribute list for a single attribute.  Checks
     that the default value is one of the possible enumerations ( if an
     enumerated type is used ).  If the iEnumerations stringlist is nil, then
     the datatype is not enumerated so any valid string can be used. }
function TAttList.ReadDefault(var ioRow, ioPos: integer;
           iEnumerations: TStringList): string;
begin
  FDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
  if FDocument.DocParser.GetChar( ioRow, ioPos ) = '"' then
  begin
    Inc(ioPos);
    { Read up to the close quote }
    Result := FDocument.DocParser.GetAllUpTo('"', ioRow, ioPos);
    if iEnumerations <> nil then
      if iEnumerations.IndexOf(Result) = -1 then
        { enumerated value not found }
        raise EAttlistError.Create(ResStr_DefaultInvalid + Result, ioRow,
              TXMLDoc(FDocument).CurrentBlockStart);
  end
  else
    Result := '';
end;


function TAttList.ReadUsage(var ioRow, ioPos: integer): TAttrUsage;
var
  lTempText : string;
begin
  FDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
  if FDocument.DocParser.GetChar( ioRow, ioPos ) = '"' then
    Result := auHasDefault
  else
  begin
    if FDocument.DocParser.GetChar( ioRow, ioPos ) <> '#' then
      raise EAttlistError.Create( ResStr_AttributeStructure, ioRow, TXMLDoc(FDocument).CurrentBlockStart );
    Inc(ioPos);
    { Check the usage specifier is immediately after the '#'}
    if not FDocument.DocParser.IsPartOfWord(
                           FDocument.DocParser.GetChar( ioRow, ioPos )) then
      raise EAttlistError.Create( ResStr_AttributeStructure, ioRow, TXMLDoc(FDocument).CurrentBlockStart );
    lTempText := FDocument.DocParser.GetNextWord(ioRow, ioPos);
    if lTempText = 'REQUIRED' then
      Result := auRequired
    else if lTempText = 'IMPLIED' then
      Result := auImplied
    else if lTempText = 'FIXED' then
      Result := auFixed
    else
      raise EAttListError.Create(ResStr_UnknownKeyword + lTempText, ioRow, TXMLDoc(FDocument).CurrentBlockStart);
    Inc(ioPos, Length(lTempText));
  end;
end;



{ When a bracket is encoutered for an attribute datatype, we must read the
     contained strings into a list of possible values.  This is therefore
     for reading enumerated attributes.  Must enter this procedure on the '(' }
function TAttList.ReadEnumerations(var ioRow, ioPos: integer): TStringList;
var
  lList : TStringList;
  lTempWord : string;
  lEndWordPos, lEndWordRow : integer;
begin
  { Create a list to hold all possible values }
  lList := TStringList.Create;
  { skip past the bracket }
  Inc(ioPos);
  FDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos);
  { Loop until the close bracket }
  while FDocument.DocParser.GetChar(ioRow, ioPos) <> ')' do
  begin
    lEndWordPos := ioPos;
    lEndWordRow := ioRow;
    { Locate the end of the enumeration string - it could contain spaces so
      must use a different technique }
    FDocument.DocParser.SearchFor(stFindWords, ['|', ')'], lEndWordRow, lEndWordPos);
    lTempWord := FDocument.DocParser.StripSpacesFromEnd(
                   FDocument.DocParser.GetAllUpTo
                   (FDocument.DocParser.GetChar(lEndWordRow, lEndWordPos),
                   ioRow, ioPos));
    lList.Add(lTempWord);
    Dec(ioPos); // so we can check the dividing character }
    if FDocument.DocParser.GetChar(ioRow, ioPos) = '|' then // more to come
    begin
      { Go to start of next word }
      Inc(ioPos);
      FDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos);
    end else if FDocument.DocParser.GetChar(ioRow, ioPos) <> ')' then
      raise EAttlistError.Create(ResStr_InvalidChar + Format(ResStr_AttlistErr,[FName]) +
                             FDocument.DocParser.GetChar(ioRow, ioPos), ioRow,
                             TXMLDoc(FDocument).CurrentBlockStart);
  end;
  Inc(ioPos); // skip final bracket
  Result := lList;
end;


{ Accessor method - Return a count of all attributes in the list }
function TAttList.GetAttributeCount: integer;
begin
  Result := FAttributes.Count;
end;


{ Accessor method - return an attribute by index }
function TAttList.GetAttributes(const iIndex: integer): TAttribute;
begin
  Result := TAttribute(FAttributes[iIndex]);
end;



{ For a FIELDNAME attribute, tries to locate a suitable field name.  Eg,
      the attribute occurrence contains an occurrence_key element, the field we
      want is biotope_occurrence_key, the field_name is biotope.  Basically, we
      need to search for _Name at the end of every field name, then return the
      bit before the _Name.  Raises an exception if no valid fieldname found. }
function TAttList.FindFieldName(iRecord : TDataset): string;
var
  i : integer;
  ltfFound : boolean;
  lTestFieldName : String;
  lSpecialSpecifiers : TStringArray;
  lElement : TElement;
  lSpecialElement : TSpecialElementItem;
begin
  SetLength(lSpecialSpecifiers, 0); // avoid compiler warning
  lElement := TXMLDoc(FDocument).GetElementByName(FName);
  { Read the actual element name as our first 'best guess' at a field specifier }
  if lElement.ContentModel = cmEMPTY then
    lTestFieldName := FName
  else
  begin
    lTestFieldName := lElement.MixedContent.FirstOutputField;
    if lTestFieldName = '#PCDATA' then // must be top-level pcdata
      lTestFieldName := FName;
  end;
  { First try to locate our real element name }
  if FindSpecifierFromFields( iRecord, lTestFieldName, Result ) then
    ltfFound := True
  else
  begin
    ltfFound := False;
    { if no luck, try and locate one of the special substrings as defined in
      special xml elements }
    lSpecialElement := dmNBNXML.FindSpecialElement(lowercase(FName), TRANSLATE_FIELD);
    { If we find a special xml element record, parse out the possible field names
        specifiers and add then search on those }
    if lSpecialElement <> nil then
    begin
      lSpecialSpecifiers := dmNBNXML.ParseSemiColonString(
                          lSpecialElement.Data );
      for i := 0 to High(lSpecialSpecifiers) do
        if FindSpecifierFromFields( iRecord, lSpecialSpecifiers[i], Result ) then
        begin
          ltfFound := True;
          break;
        end;
    end; // lSpecialElement <> nil
  end;
  if not ltfFound then
    raise EAttlistError.Create(ResStr_FieldNameAttrInvalid + FName);
end;


{ Look through the fields in the dataset for a field which finishes with the
    specified substring.  If found, returns true and the oSpecifier is set to
    the specifier.  For example, FindSpecifierFromFields( record, 'key',
        lSpecifier ); would set oSpecifier to 'survey' if a field 'survey_key'
        was found. }
function TAttlist.FindSpecifierFromFields( iRecord : TDataset;
                        const iMatchString : string; var oSpecifier : string): boolean;
var
  i : integer;
  lFullFieldName : string;
  ltfFound : boolean;
  lTextExtract : string;
begin
  ltfFound := False; // default
  for i := 0 to iRecord.Fields.Count-1 do
  begin
    lFullFieldName := Uppercase(iRecord.Fields[i].FieldName);
    { Have we got an exact name match? }
    if (CompareText(lFullFieldName, iMatchString)=0) then
    begin
      oSpecifier := ''; // specifier can be blank
      ltfFound := True;
    end
    else
    begin
      { Extract the last bit from the field name so we can test for a match }
      lTextExtract := Copy(lFullFieldName, Length(lFullFieldName) -
                      Length(iMatchString),
                      Length('_' + iMatchString));
      { Have we got a field name like '*_Name'  }
      if (CompareText(lTextExtract, '_' + iMatchString)=0) then
      begin
        { Not exact match so extract the initial portion as a specifier}
        oSpecifier := Copy(lFullFieldName, 1,
                      Length(lFullFieldName) - Length('_' + iMatchString) );
        ltfFound := True;
      end;
    end;
  end;
  Result := ltfFound;
end;


{ Function to facilitate locating a particular attribute when inputting data }
function TAttList.FindAttribute(const iName: string): TAttribute;
var
  i : integer;
begin
  Result := nil; // default is not found
  for i := 0 to AttributeCount-1 do
    if TAttribute(FAttributes[i]).Name = iName then
    begin
      Result := TAttribute(FAttributes[i]);
      break; // from loop, no need to go on
    end;
end;




//==============================================================================
{ TContentItem }
//==============================================================================


{ Constructor - prepare the list incase any contained content is nested in this
     item NEED TO DECOMPOSE METHOD }
constructor TContentItem.Create( iDocument : TDocument; var ioRow, ioPos : integer);
var
  ltfFinished : boolean;
begin
  inherited Create;
  FDocument := iDocument;  // store the document so we can access the parser
  FContainedContentList := TList.Create;
  iDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
  if iDocument.DocParser.GetChar( ioRow, ioPos ) <> '(' then
  begin
    ReadContentWord(ioRow, ioPos);
    FListRelationship := lrOrdered;
  end
  else
  begin
    { Bracketed, so add other stuff to our contained content list }
    ltfFinished := False;
    FListRelationship := lrUnknown;
    while not ltfFinished do
    begin
      Inc(ioPos);
      iDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
      FContainedContentList.Add(TContentItem.Create(iDocument, ioRow, ioPos));
      iDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
      ltfFinished := ReadListSeparator( ioRow, ioPos );
    end; // while not finished
  end; // if
  { Get the items's required specifier }
  case iDocument.DocParser.GetChar( ioRow, ioPos ) of
    '?' : FItemRule := irOneOptional;
    '*' : FItemRule := irManyOptional;
    '+' : FItemRule := irOneToMany;
  else
    begin
      FItemRule := irOne;
      { We didn't find any specifier, so advancing a character is unecessary }
      Dec(ioPos);
    end;
  end; // case;
  { Advance the character (or undo the Dec if no specifier was found) }
  Inc(ioPos);
end;


{ Read a single separator in a content model list.  Checks that the separator
     does not contradict any previous separator in the same list.  Returns true
     if the end of the list is encountered. }
function TContentItem.ReadListSeparator(var ioRow,
  ioPos: integer): boolean;
begin
  Result := False; // default
  case FDocument.DocParser.GetChar( ioRow, ioPos ) of
  ')' :
    begin
      Result := True;
      if FListRelationship = lrUnknown then
        FListRelationship := lrOrdered;
      Inc(ioPos);
    end;
  '|' :
    if FListRelationship <> lrOrdered then
      FListRelationship := lrEitherOr
    else
      raise EContentItemError.Create(ResStr_MixedListTypes, ioRow, TXMLDoc(FDocument).CurrentBlockStart);
  ',' :
    if FListRelationship <> lrEitherOr then
      FListRelationship := lrOrdered
    else
      raise EContentItemError.Create(ResStr_MixedListTypes, ioRow, TXMLDoc(FDocument).CurrentBlockStart);
  else
    raise EContentItemError.Create(ResStr_ContentList, ioRow, TXMLDoc(FDocument).CurrentBlockStart);
  end; // case
end;



{ Destructor - clean up the nested content items list }
destructor TContentItem.Destroy;
var
  i : integer;
begin
  for i := 0 to ContainedContentCount-1 do
    TContentItem(FContainedContentList[i]).Free;
  FContainedContentList.Free;
  inherited Destroy;
end;

{ Accessor method for the ContainedContentCount.  May well be zero if the item
     is not in parentheses }
function TContentItem.GetContentCount: integer;
begin
  Result := FContainedContentList.Count;
end;


{ Accessor method allowing access to the content items.  Applicable only for
     nested items (ie wrapped in parentheses) }
function TContentItem.GetContentItem(const iIndex: integer): TContentItem;
begin
  Result := TContentItem(FContainedContentList[iIndex]);
end;




{ Read the output recursively for the content items }
procedure TContentItem.DoOutput( const iRecord : TDataset; const iIndent : integer;
                                 iParentItem : TObject; iFromExportFilter: Boolean );
var
    lDocSize : longint; // size of document before we output, in case of rollback
begin
  FParentItem := iParentItem;  // remember who we are output within
  lDocSize := FDocument.Outputter.FileSize;      // record in case of rollback
  try
    { Are we a single content item, or do we contain a list of other content items }
    if FText <> '' then
      OutputSingleContent( iRecord, iIndent, iFromExportFilter)
    else // iterate through lists of content
      OutputMultipleContent( iRecord, iIndent, iFromExportFilter );
  except
    on E:EDataNeededError do // no data to output
    begin
      { Roll back everything we added }
      FDocument.Outputter.FileSize := lDocSize;
      { If we must have at least one piece of data for this item then allow
           exception to continue }
      if NeedAtLeastOne then
        raise
{$IFDEF DEBUG}
      else                                    
        { Record the rollback for good measure }
        dmNBNXML.Log(Format(ResStr_ContentRolledBack, [FText, iRecord.FieldByName(dmDatabase.GetPrimaryKey(
                dmNbnXML.GetDatasetTable(iRecord), false)).AsString, dmNbnXML.GetDatasetTable(iRecord)]), E)
{$ENDIF};
    end; //on DataNeededError
  end;
end;



{ When a content item is encountered which reflects a table type element,
     locate the nested table's dataset and output the content item multiple
     times for each record in the dataset.  For example, output multiple
     survey_events within a survey }
procedure TContentItem.OutputTableContent( iElement : TElement; iRecord: TDataset;
            const iIndent: integer; iFromExportFilter: Boolean);
var
  lRecord : TDataset;
  lReverse : boolean; // is the new table nested in the first table or the other way round?
  lOutputCount : integer; // count of records output to check against content model
  lDocSize : longint; // size of document before we output a record, in case of rollback
begin
  lOutputCount := 0;
  try
    if not HandleOptimisations( iRecord, iIndent ) then begin
      lRecord := GetNestedDataset( iRecord, FText, lReverse );
      try
        { Validate that we intend to output the correct number of records }
        CheckRecordCountAgainstContentRule( lRecord );
        if not dmNBNXML.IsRestrictedDictListItem(dmNBNXML.GetDatasetTable(lRecord)) then
        begin
          with TADOQuery(lRecord) do
          begin
            First;
            { Loop through the records returned by a query }
            while not EOF do
            begin
              Application.ProcessMessages;
              if Assigned(FDocument.Outputter) and FDocument.Outputter.Cancelled then
                raise ECancellation.CreateNonCritical(ResStr_Cancelled);
                
              lDocSize := FDocument.Outputter.FileSize;      // record in case of rollback
              try
                { Note, we don't restrict data which the original table is nested
                     inside.  For example, don't restrict Checklist inside an
                     identification }
                if (not dmNBNXML.RestrictedData(lRecord, iFromExportFilter)) or (lReverse) then
                begin
                  iElement.DoOutput(lRecord, iIndent, Self, iFromExportFilter);
                  { Don't record termlist outputs - we are just inserting key values! }
                  if not dmNBNXML.IsTermTable(lRecord) then
                    dmNBNXML.RememberOutput(lRecord)
                  else // remember we need to put the record in the termlist section
                    dmNBNXML.StoreTerm(lRecord);
                  Inc(lOutputCount);
                end;
              except
                on E:EDataNeededError do begin
                  // rollback the dropped record
                  FDocument.Outputter.FileSize := lDocSize;
                  {$IFDEF DEBUG}
                  dmNBNXML.Log(Format(ResStr_ContentRecRolledBack, [FText, lRecord.FieldByName(dmDatabase.GetPrimaryKey(
                          dmNbnXML.GetDatasetTable(lRecord), false)).AsString, dmNbnXML.GetDatasetTable(lRecord)]), E);
                  {$ENDIF}
                end;
              end;
              Next;
            end; // while
          end;  // with
        end; // if not IsRestrictedDictListItem
        { Validate that we actually output the correct number of records }
        CheckCountAgainstContentRule(lOutputCount);
      finally
        dmNBNXML.ReleaseQueryObject(lRecord);
      end; // try..finally
    end;
  except // catch all exceptions, and turn into data needed errors
    on E:Exception do
      raise EDataNeededError.Create(ResStr_TableProblem + FText, E);
  end; // try..exception
end;


{ For some foreign keys, it is not necessary to open the destination table as
    we can just output the key.  Returns true if one of these cases is found.
    Includes term list keys, identifications and blank sources.  Also looks
    for tables we have no records in, so we can not bother opening them.
    The aim of the game here is to minimise table opening }
function TContentItem.HandleOptimisations( const iRecord: TDataset;
            const iIndent: integer) : boolean;
var
  lTableName : string;
  lIdentifierType : string;
begin
  Result := True;
  lTableName := dmNBNXML.GetDatasetTable(iRecord);
  if dmNBNXML.IsTermListTable( FText ) then begin
    if iRecord.FieldByName(dmDatabase.GetPrimaryKey(FText, False)).IsNull then
      raise EDataNeededError.Create(Format(ResStr_NoDataAvailable, [FText]));
    FDocument.Outputter.NewLine(iIndent);
    FDocument.Outputter.OutputText('<' + FText + '>');
    FDocument.Outputter.NewLine(iIndent+1);
    FDocument.Outputter.OutputText('<' + FText + '_key type="nbn">' +
              iRecord.FieldByName(dmDatabase.GetPrimaryKey(FText, false)).AsString + '</' + FText + '_key>');
    FDocument.Outputter.NewLine(iIndent);
    FDocument.Outputter.OutputText('</' + FText + '>');
    dmNBNXML.StoreTerm(FText, iRecord.FieldByName(dmDatabase.GetPrimaryKey(FText, False)).AsString);
  end else if dmNBNXML.IsTableUnused( lTableName + '=' + FText ) then
    raise EDataNeededError.Create(Format(ResStr_NoDataAvailable,[FText]))
  else if (FText='identification') then begin
    FDocument.Outputter.NewLine(iIndent);
    { Extract tyype from table name - up to underscore e.g. in taxon_determination }
    lIdentifierType := Lowercase(Copy(lTableName, 1, SmartPos('_', lTableName)-1));
    FDocument.Outputter.OutputText('<' + FText + ' specifier="' + lIdentifierType + '">');
    FDocument.Outputter.NewLine(iIndent+1);
    FDocument.Outputter.OutputText('<identification_key type="nbn">' +
              iRecord.FieldByName(lIdentifierType + '_list_item_key').AsString + '</identification_key>');
    FDocument.Outputter.NewLine(iIndent);
    FDocument.Outputter.OutputText('</' + FText + '>');
    dmNBNXML.RememberOutput(lIdentifierType + '_list_item',
                    iRecord.FieldByName(lIdentifierType + '_list_item_key').AsString);
  end else if (FText='source') and (iRecord.FieldByName('source_key').IsNull) then
    raise EDataNeededError.Create(ResStr_SourceBlank)
  else
    Result := False; // not handled
end;



{ When a content item contains a list of other content items in parentheses,
     iterates through the list and outputs each nested content item in
     sequence.  Checks the requirements according to the list being ordered
     or either or.  Execptions raised when problems encountered. }
procedure TContentItem.OutputMultipleContent(iRecord: TDataset;
  const iIndent: integer; iFromExportFilter: Boolean);
var
  lItemOutputSucceeded : boolean;
  i : integer;
  lContentDescription : string;
begin
  lItemOutputSucceeded := False;
  { Prepare a descriptive string for the content item for any error reports }
  if FParentItem is TElement then
    lContentDescription := TElement(FParentItem).Name
  else if TContentItem(FParentItem).FParentItem is TElement then
    lContentDescription := TElement(TContentItem(FParentItem).FParentItem).Name
  else
    lContentDescription := 'unknown'; // default
  for i := 0 to FContainedContentList.Count-1 do
  begin
    Application.ProcessMessages;
    if Assigned(FDocument.Outputter) and FDocument.Outputter.Cancelled then
      raise ECancellation.CreateNonCritical(ResStr_Cancelled);
    
    try
      TContentItem(FContainedContentList[i]).DoOutput( iRecord, iIndent, Self, iFromExportFilter );
      lItemOutputSucceeded := True;
      if FListRelationship = lrEitherOr then // we only need one item for either/or lists
        break; // from for loop
    except
      on E:EPCDataContent do
      begin
        if not (FParentItem is TElement) then
          raise;
        HandlePCDataContent( iRecord );
      end;
      on E:EDataNeededError do // allow a failure to continue for an either/or list
        if (FListRelationship = lrOrdered) then
          raise
        else
          dmNBNXML.Log('Option not output for ' + lContentDescription, E);
    end; // try.except
  end; // for
  if (not lItemOutputSucceeded) and (FListRelationship = lrEitherOr) then
    raise EDataNeededError.Create(ResStr_OrOutputFailed + lContentDescription)
end;


{ When a content item refers to a single text string ( ie a single element ),
    does the output of that element }
procedure TContentItem.OutputSingleContent(iRecord: TDataset;
  const iIndent: integer; iFromExportFilter: Boolean);
var
  lOutputElement : TElement;
begin
  { For PCDATA, we throw an exception which is caught later and the relevant
       field is output }
  if FText = '#PCDATA' then
    Raise EPCDATAContent.Create('');
  lOutputElement := TXMLDoc(FDocument).GetElementByName(FText);
  if lOutputElement.IsTable and (FText <> TERMLISTS)  then
    OutputTableContent( lOutputElement, iRecord, iIndent, iFromExportFilter)
  else
    lOutputElement.DoOutput(iRecord, iIndent, Self, iFromExportFilter);
end;


{ Handles output of a content item when PCDATA is encountered }
procedure TContentItem.HandlePCDataContent(iRecord: TDataset);
var
  lFieldName : String;
  lIsSpatialRef : boolean;
const
  SPATIAL_REF_FIELD = 'spatial_ref';
begin
  lFieldName := TElement(FParentItem).Name;
  { Record that the field is a spatial ref, as we need to look at string data
  not text so we can output correct format, ie not always current preferred}
  lIsSpatialRef := Copy(lFieldName, Length(lFieldName)-Length(SPATIAL_REF_FIELD)+1, 255) = SPATIAL_REF_FIELD;
  try
    { First check we have such a field }
    if iRecord.FieldDefs.IndexOf(lFieldName) = -1 then
    begin
      { Try the fieldname attribute to construct a fieldname }
      if TXMLDoc(FDocument).GetElementByName(lFieldName).
                                           FFieldName <> '' then
        lFieldName := TElement(TXMLDoc(FDocument).
                   GetElementByName(lFieldName)).FFieldName + '_' +
                   lFieldName
      else
        { Check for fields which have element names different to the field name }
        dmNBNXML.HandleSpecialFields( lFieldName, iRecord );
      { If still no joy, then fail }
      if iRecord.FieldDefs.IndexOf(lFieldName) = -1 then
        raise EDataNeededError.Create(ResStr_ElementNeedsData + lFieldName);
    end;
    TXMLDoc(FDocument).OutputField(iRecord.FieldByName(lFieldName),
                                           False, lIsSpatialRef);
    dmNBNXML.StoreIfForeignKey(iRecord, lFieldName);
  except
    on E:EDataNeededError do
      { First try output as metadata element }
      if not HandleMetadata (lFieldName) then
        raise;
  end;
end;


{ Locate the nested dataset which applies to this element, using the 'parent'
     dataset as a source of information (ie to find out the key value to
     filter on.)  The oDirection output parameter is normally returned false,
     but is set to True if the dataset returned is not really nested, (ie the
     relationship is detail->master, not master->detail). }
function TContentItem.GetNestedDataset(iCurrentRec: TDataset;
         const iName : string; var oReverse : boolean): TDataset;
var
  lTableName : string;
  lRecordInfo : TRecordIdentifier;
  lRelationships : TRelationshipArray;
  lCurrentRecordTable : string;
begin
  oReverse := False; // default
  lTableName := iName;
  lCurrentRecordTable := dmNBNXML.GetDatasetTable(iCurrentRec);
  HandleSpecialTables( lTableName, iCurrentRec );
  { Find relationships between the current and nested tables }
  lRelationships :=dmDatabase.Relationships.FindRelationsBetweenTables
                  (lCurrentRecordTable, lTableName);
  { NEED TO HANDLE 2 RELATIONSHIPS }
  if High(lRelationships) < 0 then
  begin
    { no nested tables, but try the other way round in case the DTD specifies
        'wrong way round' }
    lRelationships :=dmDatabase.Relationships.FindRelationsBetweenTables
                  (lTableName, lCurrentRecordTable);
    if High(lRelationships) < 0 then
      raise EXMLError.Create(Format(ResStr_RelationshipNotCoped, [IntToStr(High(lRelationships)+1), lCurrentRecordTable, lTableName]));
    lRecordInfo.KeyField := lRelationships[0].Fields[0].MasterName;
    lRecordInfo.Key1 := iCurrentRec.FieldByName
                        (lRelationships[0].Fields[0].DetailName).AsString;
    oReverse := True; // relationship is in reverse
  end
  else
  begin
    lRecordInfo.KeyField := lRelationships[0].Fields[0].DetailName;
    lRecordInfo.Key1 := iCurrentRec.FieldByName(dmDatabase.GetPrimaryKey(
                        lCurrentRecordTable, False)).AsString;
  end;
  lRecordInfo.Key2 := '';
  lRecordInfo.TableName := lTableName;
  Result := dmNBNXML.GetQueryObject(lRecordInfo);
end;


{ Checks if we are looking for a nested table for one of the special table
     types.  For example, element determination may mean taxon_determination
     or biotope_determination depending on which occurrence we are outputting.
     Looks in the SPECIAL_XML_ELEMENT table to find the required information. }
procedure TContentItem.HandleSpecialTables(var ioTableName: string;
              iRecord : TDataset);
var
  lSpecialElement : TSpecialElementItem;
begin
  lSpecialElement := dmNBNXML.FindSpecialElement( ioTableName, TRANSLATE_TABLE );
  if lSpecialElement <> nil then
    dmNBNXML.ReplaceSpecialTable( ioTableName,
                                      lSpecialElement.Data,
                                      dmNBNXML.GetDatasetTable( iRecord ) );
end;



{ For elements we have failed to output as a field, we will check for a metadata
     element in SPECIAL_XML_ELEMENTS.  If found, it is output and True is
     returned. }
function TContentItem.HandleMetadata(const iElementName: string): boolean;
var
  lSpecialElement : TSpecialElementItem;
begin
  lSpecialElement := dmNBNXML.FindSpecialElement( iElementName, METADATA );
  if lSpecialElement <> nil then
  begin
    { Record found, so output data }
    FDocument.Outputter.OutputText(lSpecialElement.Data);
    Result := True
  end // if record found
  else
    Result := False;
end;


{ Raises an exception if the dataset does not have the right number of records
      to meet the requirements of the content item.  }
procedure TContentItem.CheckRecordCountAgainstContentRule(iDataset: TDataset);
begin
  { Ensure our count is correct }
  iDataset.Last;
  Case FItemRule of
    irOne:
      { For queries, there must be one record.  Tables must have a record available }
      if ((iDataset is TADOQuery) and (iDataset.RecordCount <> 1)) or
         ((iDataset is TADOTable) and (iDataset.RecordCount = 0)) then
        raise EDataNeededError.Create(ResStr_ContentRuleBroken + Format(ResStr_OnlyOne,[FText,IntToStr(iDataset.RecordCount)]));
    irOneOptional:
      { Tables - no need to test as we can use current record for one }
      if (iDataset is TADOQuery) and (iDataset.RecordCount > 1) then
        raise EDataNeededError.Create( ResStr_ContentRuleBroken + Format(ResStrMaxOne, [FText, IntToStr(iDataset.RecordCount)]));
    irManyOptional:
      { No rules in this case! } ;
    irOneToMany:
      if (iDataset.RecordCount = 0) then
        raise EDataNeededError.Create( ResStr_ContentRuleBroken + Format(ResStr_AtLeastOne, [FText]));
  end; // case
  if iDataset.RecordCount=0 then
      raise EDataNeededError.Create(ResStr_RecordMissing +
                    dmNBNXML.GetDatasetTable(iDataset));
end;


{ Raises an exception if the value passed in does not have the right number of
     records to meet the requirements of the content item.  }
procedure TContentItem.CheckCountAgainstContentRule(iCount: integer);
begin
  Case FItemRule of
    irOne:
      if iCount<>1 then
        raise EDataNeededError.Create( ResStr_ContentRuleBroken + ResStr_OneOutputRestricted);
    irOneOptional:  // should never get this far but check anyway
      if iCount>1 then
      raise EDataNeededError.Create( ResStr_ContentRuleBroken + Format(ResStrMaxOne, [FText, IntToStr(iCount)]));
    irManyOptional:
      { No rules in this case! } ;
    irOneToMany:
      if iCount<1 then
      raise EDataNeededError.Create(ResStr_ContentRuleBroken +
                                Format(ResStr_AtLeastOneRestricted, [FText]));
  end;
end;


{ Reads a word from the content item's text - used where a content item does
     not contain further nested content items.  The word should be either
     #PCDATA or a valid element name }
procedure TContentItem.ReadContentWord(var ioRow, ioPos: integer);
begin
  { Not bracketed info, so just read the word }
  FText := FDocument.DocParser.GetNextWord( ioRow, ioPos );
  if Length(FText) = 0 then
  begin
    { Check for command words which start # }
    if FDocument.DocParser.GetChar( ioRow, ioPos ) = '#' then // could be a valid command
    begin
      Inc(ioPos);
      { Try the word after the # }
      FText := FDocument.DocParser.GetNextWord( ioRow, ioPos );
      if FText <> 'PCDATA' then
        raise EContentItemError.Create(ResStr_ContentUknown +
              ResStr_CommUnrecognised, ioRow, TXMLDoc(FDocument).CurrentBlockStart);
      { Add the # back which GetNextWord does not pick up }
      if Length(FText) > 0 then
      begin
        FText := '#' + FText;
        { We advanced a character to skip the #, so we must go back one for consistency }
        Dec(ioPos);
      end;
    end;
    if Length(FText) = 0 then
      { The word found was blank - the first character must have been a
           non-text character such as >, so the content specification is missing }
      raise EContentItemError.Create(ResStr_ContentWrong, ioRow, TXMLDoc(FDocument).CurrentBlockStart);
  end;
  { Test content elements are valid and declared }
  if FText <> '#PCDATA' then try
    TXMLDoc(FDocument).GetElementByName( FText );
  except
    on E:EDTDNotComplete do
      raise EElementError.Create(ResStr_CreateProblem + Format(ResStr_ElementContModel, [FText]),
                                E, ioRow, TXMLDoc(FDocument).CurrentBlockStart);
  end; // try
  Inc(ioPos, Length(FText));
end;


{ Recursive procedure to identify if a content item contains an element name
    (anywhere beneath it in the content model - eg could be nested in brackets}
function TContentItem.ContainsElement(const iName: string): boolean;
var
  i : integer;
begin
  Result := False; // default not found
  { Are we a text or nested content item }
  if FText <> '' then
    Result := (uppercase(iName) = uppercase(FText))
  else
  begin
    { nested - need to try everything we contain }
    for i :=  0 to ContainedContentCount-1 do
    begin
      if TContentItem(FContainedContentList[i]).ContainsElement(iName) then
      begin
        Result := True; // success!
        break; // from loop - no need to continue recursing
      end;
    end; // for
  end;
end;


{ Iterative call to locate a name which will be output inside the content item.
     For example, (#PCDATA) returns '#PCDATA' as nothing available,
     (((Bob|Bill)|Ben),James) will return Bob by recuring into the content
     items.  Used to locate a field when FIELDNAME attributes are used }
function TContentItem.FirstOutputField: string;
begin
  if FText <> '' then
    Result := FText
  else
    { Use the first contained content item which must exist }
    Result := TContentItem(FContainedContentList[0]).FirstOutputField;
end;


{ Validates that a tag contains appropriate content according to the item's
     content model.  Used to validate import files.  Recursive calls are made
     into the contained content where necessary.  The return value is the item
     no within the tag which we successfully validated up to.
     Exceptions are raised where validation fails. }
function TContentItem.ValidateTag(const iTag: TTag;
        const iContainedTagNo: integer): integer;
var
  lContentIndex : integer;
  lItemsOk, lPrevOKCount : integer;
  lCurrentTagNo, lSuccessTagNo : integer;
  ltfFinished : boolean;
  lCurrentContent : TContentItem;

    procedure ThrowException;
    begin
      raise EContentValidationError.Create(ResStr_ContentIncorrect + Text,
            iTag.DocRow, TXMLDoc(FDocument).CurrentBlockStart);
    end;

begin
  lCurrentTagNo := iContainedTagNo;
  lSuccessTagNo := iContainedTagNo - 1; { last successful tag }
  lItemsOk := 0;
  ltfFinished := False;
  while not ltfFinished do
  begin
    Application.ProcessMessages;
    if Assigned(FDocument.Outputter) and FDocument.Outputter.Cancelled then
      raise ECancellation.CreateNonCritical(ResStr_Cancelled);

    lPrevOKCount := lItemsOK;
    lContentIndex := 0;
    if ContainedContentCount > 1 then
    begin
      while (lContentIndex < ContainedContentCount) do
      begin
        lCurrentContent := TContentItem(FContainedContentList[lContentIndex]);
        try
          { Recurse }
          lCurrentTagNo := lCurrentContent.ValidateTag(iTag, lCurrentTagNo);
          Inc(lContentIndex);
          Inc(lItemsOk);
          lSuccessTagNo := lCurrentTagNo-1;
          if ListRelationship = lrEitherOr then
            break; // from while loop - only want one item
        except
          on EContentValidationError do
          begin
            if (ListRelationship = lrOrdered) and lCurrentContent.NeedAtLeastOne then
              raise;
            { Ok - could be an optional list and were in the wrong option }
            Inc(lContentIndex);
          end;
        end; // try
      end; // while
      { If not enough content found }
      if ((lContentIndex < ContainedContentCount) and (ListRelationship<>lrEitherOr)) or
           ((ListRelationship=lrEitherOr) and (lItemsOK<>1)) then
        ThrowException;
    end else if ContainedContentCount = 1 then
    begin
      lCurrentTagNo := TContentItem(FContainedContentList[lContentIndex]).
                             ValidateTag(iTag, lCurrentTagNo);
      Inc(lItemsOK);
      lSuccessTagNo := lCurrentTagNo-1;
    end
    else
    begin
      if (lCurrentTagNo >= iTag.NestedTags.Count) then
      begin
        if ((FText<>'#PCDATA'))  then // NOTE- CURRENTLY ALLOWS BLANK TAGS
          ltfFinished := True;
      end else
      begin
        if (FText <> TTag(iTag.NestedTags[lCurrentTagNo]).Name) then
          ltfFinished := True;
      end;
      if not ltfFinished then
      begin
        Inc(lItemsOK);
        Inc(lCurrentTagNo);
        lSuccessTagNo := lCurrentTagNo-1;
      end;
    end;
    { If all tags checked, or we have a valid tag and we only want one }
    if ((lCurrentTagNo >= iTag.NestedTags.Count) or ((lItemsOK>0) and NoMoreThanOne)) or
       (lPrevOKCount = lItemsOK) then
      ltfFinished := True;
  end; // while not ltfFinished loop

  { Validate the number of repetitions }
  if (lItemsOK = 0) and NeedAtLeastOne then
    ThrowException;
  Result := lSuccessTagNo+1;
end;


{ Returns true if a content item specifies that at least one item is required }
function TContentItem.NeedAtLeastOne: boolean;
begin
  Result := (ItemRule=irOne) or (ItemRule=irOneToMany);
end;


{ Returns true if a content item specifies that at least one item is required }
function TContentItem.NoMoreThanOne: boolean;
begin
  Result := (ItemRule=irOne) or (ItemRule=irOneOptional);
end;



{ If a content item is not contained in the imported file for a markup, raise
    an exception if this breaks the content rules }
procedure TContentItem.CheckNotFoundIsOk(const iContentIndex: integer);
begin
  if (ListRelationship = lrOrdered) and
                ((TContentItem(FContainedContentList[iContentIndex]).ItemRule = irOne) or
                (TContentItem(FContainedContentList[iContentIndex]).ItemRule = irOneToMany)) then
    { We should have found it }
    raise EXMLError.Create(ResStr_ItemRequired + TContentItem(FContainedContentList[iContentIndex]).Text);
end;



//==============================================================================
{ TEntity }
//==============================================================================

{ Load the entity name and text from the document strings }
constructor TEntity.CreateFromStrings(iParent: TAbstractDocItem;
  iDocument: TMarkupDocument; var ioRow, ioPos: integer);
begin
  if iDocument.MarkupStack.PeekTop is TDocType then
  begin
    inherited CreateFromStrings( iParent, iDocument );
    { Confirm we're looking at an Entity }
    ConfirmType('ENTITY', ioRow, ioPos);
    CheckIfParameterEntity( ioRow, ioPos );
    FName := iDocument.DocParser.GetNextWord( ioRow, ioPos );
    Inc(ioPos, Length(FName)+1);
    iDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
    FTextStrings := TStringList.Create;
    if iDocument.DocParser.GetChar( ioRow, ioPos ) = '"' then
    begin
      Inc(ioPos);
      FTextStrings.Add(iDocument.DocParser.GetAllUpTo('"', ioRow, ioPos));
      FEntityType := etInternal;
    end else
      ReadExternalEntity( ioRow, ioPos );
  end
  else { Nested inside wrong parent so raise appropriate error }
    if iDocument.MarkupStack.PeekTop = nil then
      raise EEntityError.Create( ResStr_WrongParent + ResStr_EntityNotInside)
    else
      raise EEntityError.Create(ResStr_WrongParent +
            Format(ResStr_ContainsEntityDir, [iDocument.MarkupStack.PeekTop.ClassName]),
                  ioRow, TXMLDoc(iDocument).CurrentBlockStart);
end;


{ Destructor - just cleanup stringlist }
destructor TEntity.Destroy;
begin
  FTextStrings.Free;
  inherited Destroy;
end;


{ Checks the next character to see if it is a parameter entity and sets the
     appropriate flag.  Winds on to the next valid char }
procedure TEntity.CheckIfParameterEntity(var ioRow, ioPos: integer);
begin
  FDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
  if FDocument.DocParser.GetChar( ioRow, ioPos ) = '%' then
  begin
    FParameter := True;
    { Wind on to next char }
    Inc(ioPos);
    FDocument.DocParser.SearchFor( stNonSpace, ioRow, ioPos );
  end else // not a parameter entity
    FParameter := False;
end;


{ If the entity content is not in quotes, then it should be an external entity
     so try and read it. }
procedure TEntity.ReadExternalEntity(var ioRow, ioPos: integer);
var
  lNextWord : string;
begin
  lNextWord := FDocument.DocParser.GetNextWord( ioRow, ioPos );
  if lNextWord = 'SYSTEM' then
    FEntityType := etExternSystem
  else if lNextWord = 'PUBLIC' then
    raise EEntityError.Create(ResStr_EntityStructure +
                              ResStr_PublicExternalNotSupp, ioRow,
                              TXMLDoc(FDocument).CurrentBlockStart)
  else
    raise EEntityError.Create(ResStr_EntityStructure, ioRow, TXMLDoc(FDocument).CurrentBlockStart);
  Inc(ioPos, Length(lNextWord));
  FDocument.DocParser.SearchFor(stNonSpace, ioRow, ioPos);
  if FDocument.DocParser.GetChar( ioRow, ioPos ) = '"' then
  begin
    Inc(ioPos);
    FTextStrings.LoadFromFile(ExtractFilePath(FDocument.FileName) +
                             FDocument.DocParser.GetAllUpTo('"', ioRow, ioPos));
  end else
    raise EEntityError.Create(ResStr_EntityStructure, ioRow, TXMLDoc(FDocument).CurrentBlockStart);
end;


//==============================================================================
{ TXMLDocParser }
//==============================================================================

{ TXMLDocParser constructor allows the markup document document to be attached
    to the parser }
constructor TXMLDocParser.Create(iDocument: TXMLDoc);
begin
  inherited Create(iDocument.FileStrings);
  FDocument := iDocument;
end;


{ Reimplementation of getchar, with an initial check in case the char is at the
     start of one of the entities.  If so, the text is replaced and the first
     char in the entity is returned }
function TXMLDocParser.GetChar(const iRow, iPos: integer): char;
var
  lChar : char;
  lEntity : TEntity;
begin
  lChar := inherited GetChar(iRow, iPos);
  { Is it a parameter entity inside the DTD, or a normal entity anywhere? }
  if ((lChar = '%') and (FDocument.MarkupStack.PeekTop is TDocType)) or
                          (lChar = '&') then
  begin
    { Check the next word immediately follows the % }
    if IsPartOfWord(GetChar( iRow, iPos+1 )) then
    begin
      try
        lEntity := FDocument.GetEntityByName(GetNextWord(iRow, iPos+1));
        if (lEntity.Parameter = (lChar = '%')) and // parameter entity must start %, otherwise &
                   (GetChar( iRow, iPos + length(lEntity.Name)+1 )=';') then // correctly terminated, eg %123;
        begin
          { Splice the entity text in place }
          SpliceEntity( iRow, iPos, lEntity );
          { Now the file has changed, refetch the char }
          lChar := inherited GetChar(iRow, iPos);
        end; // if correctly terminated entity reference
      except
        on EDTDNotComplete do ; // nothing - no such entity so leave text as is
      end; // try..except
    end;  // if
  end;
  Result := lChar;
end;



{ Reimplementation of GetNextWord, with an intital check in case the char is at the
     start of one of the entities.  If so, the text is replaced and the first
     word in the entity is returned }
function TXMLDocParser.GetNextWord(const iRow, iPos: integer): string;
var
  lRow, lPos : integer; // temporary values so we can change them
begin
  lRow := iRow;
  lPos := iPos;
  SearchFor( stNonSpace, lRow, lPos );
  { Use the GetChar method to do the dirty work but ignore result }
  GetChar(lRow, lPos);
  result := inherited GetNextWord( lRow, lPos );
end;


{ Inserts an entity's text representation into the document at the point
  specified }
procedure TXMLDocParser.SpliceEntity(const iRow, iPos: integer;
  iEntity: TEntity);
var
  lRowCount : integer;
  lEndText : string; // text in the current line after the splice point
begin
  lEndText := Copy( DocStrings[iRow],
                    iPos + Length('%' + iEntity.Name + ';'),
                    High(Integer));
  DocStrings[iRow] := Copy(DocStrings[iRow], 1, iPos-1);
  for lRowCount := 0 to iEntity.TextStrings.Count-1 do
  begin
    DocStrings[lRowCount+iRow] := DocStrings[lRowCount+iRow]
                                          + iEntity.TextStrings[lRowCount];
    { Only insert a line if we have more text to insert }
    if lRowCount < iEntity.TextStrings.Count-1 then
      DocStrings.Insert(lRowCount + iRow + 1, '');
  end; // for
  { Stick the last bit of text back on }
  DocStrings[iEntity.TextStrings.Count+iRow-1] :=
             DocStrings[iEntity.TextStrings.Count+iRow-1] + lEndText;
end;





















































end.









