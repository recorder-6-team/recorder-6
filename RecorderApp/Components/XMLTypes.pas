{ Types and definitions for use by XML parsing/output classes, and some
     abtract base classes }
unit XMLTypes;

interface

uses
  Classes, SysUtils, ExceptionForm, MarkupDocs, DataClasses;

const
  XML_VERSION = '1.0';
  FORMAT_NAME = 'nbndata';
  NBN_NOTATION = '-//NBN//Exchange Format//EN';
  REMOTE_DTD = 'www.somewhere.com/nbndata.dtd';

  { Name of the content tag }
  CONTENT_TAG = 'content';

  TERM_SHORT = 'SHORT_NAME';
  TERM_FULL = 'LONG_NAME'; // term list field names so we can detect termlists

  GENERIC_DATE = 'date';

  DATE_FORMAT = 'dd/mm/yyyy';
  TIME_FORMAT = 'hh-mm-ss';

  DTD_FILE = 'nbndata.dtd';
  INPUT_FILE = 'exportstart.xml';

  { indicate that information should be added at the end of a document }
  APPEND = -1;

  { Constant values for the TYPE column in SPECIAL_XML_ELEMENT table }
  METADATA = 'M';
  TRANSLATE_TABLE = 'T';
  TRANSLATE_FIELD = 'F';
  IDENTIFICATION = 'I';

type

  { Type to define the usage of each attribute in a DTD attribute list }
  TAttrUsage = (auImplied, auFixed, auRequired, auHasDefault);


  { Data types for attributes in an XML attlist.  There may be more required! }
  TAttrType = (atCDATA, atNMTOKEN, atEnumerated);


  { When defining contained elements for an element, symbols define the
       requirements for how many there should be }
  TElementSymbol = (esNotSpecified, esAny, esOneToMany, esOptional);


  { When defining contained elements, they may be grouped with a specification
       of how the group interacts }
  TElementRelationship = (erOr, erAllOrdered, erAllAnyOrder);


  { Generic XML error classes - we'll derive more specific ones later }
  EXMLError = class(TExceptionPath);
  ECancellation = class(EXMLError);

  TEntityType = (etInternal, etExternSystem, etExternPublic);


  { a structure to identify 1 record in 1 dataset }
  TRecordIdentifier = record
    TableName : string;
    Key1 : TKeyString;
    Key2 : TKeyString;
    KeyField : string[30]; // if blank, assume primary key
  end;


implementation





end.
