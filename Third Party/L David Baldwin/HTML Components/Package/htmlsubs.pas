
{Version 9.03}
{*********************************************************}
{*                     HTMLSUBS.PAS                      *}
{*              Copyright (c) 1995-2002 by               *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$i htmlcons.inc}

{
This module is comprised mostly of the various Section object definitions.
As the HTML document is parsed, it is divided up into sections.  Some sections
are quite simple, like TParagraphSpace.  Others are more complex such as
TSection which can hold a complete paragraph.

The HTML document is then stored as a list, TSectionList, of the various
sections.

Closely related to TSectionList is TCell.  TCell holds the list of sections for
each cell in a Table (the ThtmlTable section).  In this way each table cell may
contain a document of it's own.

The Section objects each store relevant data for the section such as the text,
fonts, images, and other info needed for formating.

Each Section object is responsible for its own formated layout.  The layout is
done in the DrawLogic method.  Layout for the whole document is done in the
TSectionList.DoLogic method which essentially just calls all the Section
DrawLogic's.  It's only necessary to call TSectionList.DoLogic when a new
layout is required (when the document is loaded or when its width changes).

Each Section is also responsible for drawing itself (its Draw method).  The
whole document is drawn with the TSectionList.Draw method.
}

unit Htmlsubs;
{$R HTML32.Res}

interface
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,      
  Forms, Dialogs, StdCtrls, ExtCtrls, HTMLUn2, HTMLGif2, mmSystem, StyleUn;

const
  MaxCols = 200;  {number columns allowed in table}
                                                   
type
  ThvPanel = Class(TPanel)  
  public
    FVisible: boolean;
    procedure SetVisible(Value: boolean);
    property Visible: boolean read FVisible write SetVisible default True;
  end;

  TGetBitmapEvent = procedure(Sender: TObject; const SRC: string;
                    var Bitmap: TBitmap; var Color: TColor) of Object;
  TGetImageEvent = procedure(Sender: TObject; const SRC: string;
                    var Stream: TMemoryStream) of Object;
  TFormSubmitEvent = procedure(Sender: TObject; const Action, Target, EncType, Method: string;
                    Results: TStringList) of Object;
  TPanelCreateEvent = procedure(Sender: TObject; const AName, AType, SRC: string; 
                        Panel: ThvPanel) of Object;
  TPanelDestroyEvent = procedure(Sender: TObject; Panel: ThvPanel) of Object;
  TPanelPrintEvent = procedure(Sender: TObject; Panel: ThvPanel; const Bitmap: TBitmap) of Object;
  TObjectClickEvent = procedure(Sender, Obj: TObject; const OnClick: string) of Object;
  ThtObjectEvent = procedure(Sender, Obj: TObject; const Attribute: string) of Object;   
  TExpandNameEvent = procedure(Sender: TObject; const SRC: string; var Result: string) of Object;
  guResultType = set of (guUrl, guControl, guTitle);  
  TCell = Class;
  TBlockCell = Class;
  TCellBasic = Class;
  TSectionList = Class;
  TSection = Class;

  TFontObj = class(TObject)   {font information}
  private
    Section: TSection;
    FVisited, FHover: boolean;
    Title: string;
    FYValue: integer;  
    Active: boolean;  
    procedure SetVisited(Value: boolean);
    procedure SetHover(Value: boolean);
    function GetURL: string;
    procedure SetAllHovers(List: TList; Value: boolean);
    procedure CreateFIArray;
    {$ifndef NoTabLink}
    procedure EnterEvent(Sender: TObject);  
    procedure ExitEvent(Sender: TObject);  
    procedure CreateTabControl(TabIndex: integer);     
    procedure AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AssignY(Y: integer);   
    {$endif}
  public
    Pos : integer;        {0..Len  Index where font takes effect}
    TheFont : TMyFont;
    FIArray: TFontInfoArray;
    FontHeight,       {tmHeight+tmExternalLeading}
    tmHeight, tmMaxCharWidth,
    Overhang, Descent : integer;
    SScript: AlignmentType;
    UrlTarget: TUrlTarget;
    TabControl: TWinControl;     
    constructor Create(ASection: TSection; F: TMyFont; Position: integer);
    constructor CreateCopy(ASection: TSection; T: TFontObj);
    destructor Destroy; override;
    procedure ReplaceFont(F: TMyFont);
    procedure ConvertFont(FI: ThtFontInfo);  
    procedure FontChanged;
    function GetOverhang : integer;
    function GetHeight(var Desc: integer): integer;

    property URL: string read GetURL;     
    property Visited: boolean read FVisited Write SetVisited;
    property Hover: boolean read FHover Write SetHover;
    property YValue: integer read FYValue;  
  end;

  TFontList = class(TFreeList)  {a list of TFontObj's}
  Public
    constructor CreateCopy(ASection: TSection; T: TFontList);
    function GetFontAt(Posn : integer; var OHang : integer) : TMyFont;
    function GetFontCountAt(Posn, Leng : integer) : integer;
    function GetFontObjAt(Posn : integer;
                      var Index : integer) : TFontObj;
    procedure Decrement(N: integer; ParentSectionList: TSectionList);
  end;

  TImageFormControlObj = class;

  TFloatingObj = class(TIDObject)
  protected
    Pos : integer;        {0..Len  index of image position}
    ImageHeight,          {does not include VSpace}
    ImageWidth: integer;
    ObjAlign: AlignmentType;
    Indent: integer;
    HSpaceL, HSpaceR, VSpaceT, VSpaceB:  integer;  {horizontal, vertical extra space}
    SpecWidth: integer;   {as specified by <img or panel> tag}
    PercentWidth: boolean;           {if width is percent}
    ImageTitle:  string;    
    FAlt: string;          {the alt= attribute}  

    function GetYPosition: integer; override;
  public
    DrawYY: integer;
    DrawXX: integer;  
    constructor CreateCopy(T: TFloatingObj);
    procedure DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
                  FO: TFontObj; AvailableWidth: integer);  virtual; abstract;
    property Alt: string read FAlt;   
  end;

  TPanelObj = class(TFloatingObj)
  private
    fMasterList:TSectionList;
  public
    ShowIt: boolean;
    Panel, OPanel: ThvPanel;  
    OSender: TObject;         
    PanelPrintEvent: TPanelPrintEvent;  
    Ratio: Double;
    constructor Create(AMasterList: TSectionList; Position: integer;
                    L: TAttributeList; ACell: TCellBasic);
    constructor CreateCopy(AMasterList: TSectionList; T: TPanelObj);
    destructor Destroy; override;
    procedure DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
                  FO: TFontObj; AvailableWidth: integer);  override;
    procedure Draw(ACanvas: TCanvas; X1, Y1: integer);
  end;

  HoverType = (hvOff, hvOverUp, hvOverDown);   

  TImageObj = class(TFloatingObj)   {inline image info}
  private
    FBitmap: TBitmap;
    FHover: HoverType;
    FHoverImage: boolean;
    function GetBitmap: TBitmap;
    procedure SetHover(Value: HoverType); 
  public
    SpecHeight: integer;   {as specified by <img or panel> tag}
    ObjHeight, ObjWidth: integer;   {width as drawn}
    ImageKnown: boolean;      {know size of image}
    Source: String;    {the src= attribute}
    NoBorder: boolean;        {set if don't want blue border}
    Image: TPersistent;  {bitmap possibly converted from GIF, Jpeg, etc or animated GIF}
    OrigImage: TPersistent;  {same as above unless swapped}   
    Mask: TBitmap;    {Image's mask if needed for transparency}
    ParentSectionList: TSectionList;
    Transparent: Transparency;    {None, Lower Left Corner, or Transp GIF}
    IsMap, UseMap: boolean;
    HasBlueBox: boolean;          {Link box drawn around image}
    MapName: String;
    MyFormControl: TImageFormControlObj;  {if an <INPUT type=image}
    MyCell: TCellBasic;
    Swapped: boolean;   {image has been replaced}  
    Missing: boolean;   {waiting for image to be downloaded}  

    constructor Create(MasterList: TSectionList; Position: integer; L: TAttributeList);
    constructor SimpleCreate(MasterList: TSectionList; const AnURL: string);
    constructor CreateCopy(AMasterList: TSectionList; T: TImageObj);
    destructor Destroy; override;
    procedure ProcessProperties(Prop: TProperties);
    procedure DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
                  FO: TFontObj; AvailableWidth: integer);  override;
    procedure DoDraw(Canvas: TCanvas; XX, Y: Integer; ddImage: TPersistent; ddMask: TBitmap);
    procedure Draw(Canvas: TCanvas; X: integer; TopY, YBaseline: integer; FO: TFontObj);
    function InsertImage(const UName: String; var Reformat: boolean): boolean;

    property Bitmap: TBitmap read GetBitmap;
    property Hover: HoverType read FHover write SetHover;  
    procedure ReplaceImage(NewImage: TStream);   
  end;

  TImageObjList = class(TFreeList)  {a list of TImageObj's and TPanelObj's}
  Public
    constructor CreateCopy(AMasterList: TSectionList; T: TImageObjList);
    function FindImage(Posn: integer): TFloatingObj;
    function GetHeightAt(Posn: integer; var AAlign: AlignmentType;
         var FlObj: TFloatingObj) : Integer;
    function GetWidthAt(Posn: integer; var AAlign: AlignmentType;
           var HSpcL, HSpcR: integer; var FlObj: TFloatingObj) : integer;
    function GetImageCountAt(Posn: integer): integer;
    function PtInImage(X: integer; Y: integer; var IX, IY, Posn: integer;
                var AMap, UMap: boolean; var MapItem: TMapItem;
                var ImageObj: TImageObj): boolean;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
    procedure Decrement(N: integer);
  end;

  IndentManager = class(IndentManagerBasic)
    procedure Update(Y: integer; Img: TFloatingObj);
    procedure UpdateBlock(Y: integer; IW: integer; IH: integer; Justify: AlignmentType);
    end;

  TFormControlObj = class;
  TRadioButtonFormControlObj = class;

  ThtmlForm = class(TObject)
  private
    procedure AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  Public
    MasterList: TSectionList;
    Method: string[4];
    Action, Target, EncType: String;
    ControlList: TFreeList;
    NonHiddenCount: integer;
    constructor Create(AMasterList: TSectionList; L : TAttributeList);
    destructor Destroy; override;
    procedure DoRadios(Radio: TRadioButtonFormControlObj);
    procedure InsertControl(Ctrl: TFormControlObj);
    procedure ResetControls;
    function GetFormSubmission: TStringList;
    procedure SubmitTheForm(const ButtonSubmission: string);
    procedure SetFormData(SL: TStringList);
    procedure SetSizes(Canvas: TCanvas);
    procedure ControlKeyPress(Sender: TObject; var Key: char);
  end;

  TFormControlObj = class(TIDObject)
  private
    FYValue: integer;
    Active: boolean;
    function GetControl: TWinControl; virtual;
  protected
    procedure DoOnChange;  virtual;
    procedure SaveContents; virtual;
    function GetYPosition: integer; override;
  public
    Pos : integer;        {0..Len  index of control position}
    MasterList: TSectionList;
    MyForm: ThtmlForm;
    Value, FName, FID: String;
    FormAlign: AlignmentType;
    HSpaceL, HSpaceR: integer;
    FHeight, FWidth: integer;
    FControl: TWinControl;
    ShowIt: boolean;
    OnClickMessage: String;
    OnFocusMessage: String;
    OnBlurMessage: String;
    OnChangeMessage: String;

    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    constructor CreateCopy(T: TFormControlObj);
    destructor Destroy; override;
    procedure ProcessProperties(Prop: TProperties); virtual;
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); virtual;
    procedure ResetToValue; virtual;
    function GetSubmission(Index: integer; var S: string): boolean; virtual;
    procedure SetData(Index: integer; const V: String); virtual;
    procedure SetDataInit; virtual;
    procedure SetHeightWidth(Canvas: TCanvas); virtual;
    procedure EnterEvent(Sender: TObject);    {these two would be better private}
    procedure ExitEvent(Sender: TObject);
    procedure FormControlClick(Sender: TObject);

    property TheControl: TWinControl read GetControl;  {the Delphi control, TButton, TMemo, etc}
    property Name: string read FName;  {Name given to control}
    property ID: string read FID;      {ID attribute of control}    
    property YValue: integer read FYValue;
  end;

  TImageFormControlObj = class(TFormControlObj)
  private
    MyImage: TImageObj;
  public
    XPos, YPos, XTmp, YTmp: integer;   {click position}
    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    procedure ProcessProperties(Prop: TProperties); override;
    procedure ImageClick(Sender: TObject);
    function GetSubmission(Index: integer; var S: string): boolean; override;
  end;

  THiddenFormControlObj = class(TFormControlObj)
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;     
  end;

  TEditFormControlObj = class(TFormControlObj)
  private
    EnterContents: string;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    EditSize: integer;
    constructor Create(AMasterList: TSectionList; Position: integer;
                L: TAttributeList; const Typ: string; Prop: TProperties);
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure ResetToValue; override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
  end;

  WhichType = (Submit, ResetB, Button);

  TButtonFormControlObj = class(TFormControlObj)
  public
    Which: WhichType;
    constructor Create(AMasterList: TSectionList; Position: integer;
                L: TAttributeList; const Typ: string; Prop: TProperties);
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure ButtonClick(Sender: TObject);
    procedure SetHeightWidth(Canvas: TCanvas); override;
  end;

  TRadioButtonFormControlObj = class(TFormControlObj)
  private
  private
    WasChecked: boolean;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
    function GetControl: TWinControl; override;
  public
    IsChecked: boolean;
    RButton: TRadioButton;  
    MyCell: TCellBasic;
    constructor Create(AMasterList: TSectionList; Position: integer;
                L: TAttributeList; ACell: TCellBasic);
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure RadioClick(Sender: TObject);
    procedure ResetToValue; override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;     
  end;

  TCheckBoxFormControlObj = class(TFormControlObj)
  private
    WasChecked: boolean;
  public
    IsChecked: boolean;
    constructor Create(AMasterList: TSectionList; Position: integer; L: TAttributeList);
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure ResetToValue; override;
    function GetSubmission(Index: integer; var S: string): boolean; override;
    procedure SetData(Index: integer; const V: String); override;     
    procedure SetDataInit; override;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  end;

  LineRec = class(TObject)  {holds info on a line of text}
    Start: PWideChar;
    SpaceBefore, SpaceAfter,
    LineHt,                 {total height of line}
    LineImgHt,              {top to bottom including any floating image}
    Ln,                     {# chars in line}
    Descent,
    LineIndent : integer;
    DrawXX, DrawWidth: integer;
    DrawY: integer;
    Spaces, Extra:  integer;
    end;

  TSectionBase = class(TIDObject)   {abstract base for document sections}
  protected
    function GetYPosition: integer; override;
  public
    ParentSectionList: TSectionList;   {what list it's in}
    SectionHeight: integer;            {pixel height of section}
    DrawHeight: integer;               {floating image may overhang}
    StartCurs: integer;
    Len: integer;
    ZIndex: integer;
    ContentTop, ContentBot, ContentLeft: integer;
    DrawTop, DrawBot, YDraw: integer;

    constructor Create(AMasterList: TSectionList);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); virtual;
    procedure CopyToClipboard; virtual;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; virtual;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  virtual;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType; virtual;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; virtual;
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer; virtual;
    function FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer; virtual;
    function FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer; virtual;
    function FindSourcePos(DocPos: integer): integer; virtual;
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; virtual;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; virtual;
    function GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean; virtual;
    procedure SetParent(List: TSectionList);
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); virtual;
    procedure AddSectionsToList; virtual;
    end;

  TBlock = class(TSectionBase)
    MargArray: TMarginArray;
    MyCell: TBlockCell;
    EmSize, ExSize, FGColor: integer;
    BorderStyle: BorderStyleType;
    FloatLR: AlignmentType;   {ALeft or ARight if floating}
    ClearAttr: ClearAttrType;
    IsListBlock: boolean;
    PRec: PtPositionRec;
    Positioning: PositionType;  {posStatic, posAbsolute, posRelative}
    Visibility: VisibilityType;
    BottomAuto: boolean;
    BreakBefore, BreakAfter, KeepIntact: boolean;
    DisplayNone: boolean;
    Converted: boolean;

    MargArrayO: TVMarginArray;
    OwnerCell: TCellBasic;
    TagClass: string;       {debugging aid}
    NewWidth: integer;
    ClearAddon: integer;
    Indent: integer;
    NeedDoImageStuff: boolean;
    BGImage: TImageObj;
    TiledImage, TiledMask, FullBG: TBitmap;
    TopP, LeftP: integer;
    DrawList: TList;
    NoMask: boolean;
    ClientContentBot: integer;
    BlockTitle: string;  
    MyRect: TRect;       

    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic; Attributes: TAttributeList);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    destructor Destroy; override;
    procedure CollapseMargins;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer; virtual;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X , XRef, YRef : integer) : integer;  override;
    procedure DrawBlock(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, Y, XRef, YRef : integer);
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType; override;
    function FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer; override;
    function GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; override;
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; override;
    function FindSourcePos(DocPos: integer): integer; override;
    procedure CopyToClipboard; override;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; override;
    procedure DrawSort;
    procedure DrawTheList(Canvas: TCanvas; ARect: TRect; ClipWidth, X,
                          XRef, YRef :integer);
    procedure AddSectionsToList; override;
    procedure FormTree(Indent: string; var Tree: string);
    end;

  ListTypeType = (None, Ordered, Unordered, Definition, liAlone);

  ThtmlTable = class;

  TTableBlock = class(TBlock)
  public
    Table: ThtmlTable;
    WidthAttr: integer;
    AsPercent: boolean;
    HeightAttr: integer;
    HtAsPercent: boolean;
    BkColor: TColor;
    BkGnd: boolean;
    HSpace, VSpace: integer;
    Justify: JustifyType;

    constructor Create(Master: TSectionList; Prop: TProperties;
    	AnOwnerCell: TCellBasic; ATable: ThtmlTable; TableAttr: TAttributeList;
        TableLevel: integer);  
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer; override;
    procedure AddSectionsToList; override;
    end;

  THRBlock = class(TBlock)
  public
    Align: JustifyType;
    MyHRule: TSectionBase;
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    function FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer; override;
    end;

  TBlockLI = class(TBlock)
    private
      ListType: ListTypeType;
      ListNumb: integer;
      ListStyleType: ListBulletType;

      ListFont: TFont;
      Image: TImageObj;
      FirstLineHt: integer;
    public
    constructor Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic;
                Sy: Symb; APlain: boolean; AIndexType: char;
                AListNumb, ListLevel: integer; Attributes: TAttributeList);  
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    destructor destroy; override;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    end;

  TBodyBlock = class(TBlock)
    constructor Create(Master: TSectionList; Prop: TProperties;
          AnOwnerCell: TCellBasic; Attributes: TAttributeList); 
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType; override;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    end;

  IntArray = array[0..MaxCols] of integer;
  TCellObj = Class;

  TCellList = class(TFreeList)  {a list of TCellObj's to form a table row}
  public
    RowHeight: integer;
    SpecRowHeight: integer;   
    RowSpanHeight: integer;   {height of largest rowspan}
    BkGnd: boolean;
    BkColor: TColor;
    BkImage: string;
    APRec: PtPositionRec;
    BreakBefore, BreakAfter, KeepIntact: boolean;   

    constructor Create(Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: TSectionList; T: TCellList);
    procedure InitializeRow;
    function DrawLogic1(Canvas : TCanvas; const Widths : IntArray; Span,
          CellSpacing: integer; var More: boolean): integer;
    procedure DrawLogic2(Canvas : TCanvas; Y: integer;
              CellSpacing: integer; var Curs: integer);
    function Draw(Canvas: TCanvas; MasterList: TSectionList; const ARect: TRect;
         const Widths : IntArray; X: integer; Y, YOffset: integer;
         CellSpacing : integer; Border: boolean; Light, Dark: TColor; Rgn: THandle;
         MyRow: integer) : integer;  
    procedure Add(CellObj: TCellObj);
    end;

  TColObj = Class
    colWidth: integer;
    colAsPercent: boolean;
    colAlign: string;
    colVAlign: AlignmentType;
    end;

  ThtmlTable = class(TSectionBase)
  private
    procedure DrawTable(Canvas: TCanvas; const ARect: TRect;
      IMgr: IndentManager; X, Y: Integer);
  public
    Rows: TFreeList;   {a list of TCellLists}
    Caption: TCellObj; {holds the caption info}
    ListsProcessed,
    Border,                   {if has a border}
    TopCaption: boolean;      {if caption is on top (vs. bottom)}
    Indent,                   {table indent}
    CaptionIndent: integer;   {indent of caption}
    Float: boolean;           {if floating}
    NumCols,                  {Number columns in table}
    TableWidth,               {width of table}
    CaptionWidth: integer;    {width of caption}
    WidthAttr: integer;    {Width attribute as entered}
    AsPercent: boolean;    {if it's a percent}
    HeightAttr: integer;   {Height attribute as entered}
    HtAsPercent: boolean;  {if it's a percent}
    ProposedHeight: integer;
    UseAbsolute: boolean;  {width entries are considered absolute}
    CaptionHeight,            {height of caption itself}
    TableHeight: integer;     {height of table itself, not incl caption}
    CellPadding, CellSpacing: integer;
    CaptionMinWidth: integer; {minimum width caption can be shrunk to}
    Widths,                   {holds column widths}
    Percents: IntArray;       {percent widths of columns}
    HSpace, VSpace:  integer; {horizontal, vertical extra space}
    BorderColorLight, BorderColorDark: TColor;
    EndList: boolean;     {marker for copy}
    DrawX: integer;
    DrawY: integer;
    BkGnd, BdrOn: boolean;
    BkColor, BdrColor: TColor;
    MyCell: TCellBasic;
    ColInfo: TFreeList;

    constructor Create(Master: TSectionList;Attr: TAttributeList;
                    Prop: TProperties; ACell: TCellBasic);
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    destructor Destroy; override;
    procedure DoColumns(Width: integer; AsPercent: boolean;
                   VAlign: AlignmentType; const Align: string);   
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    procedure AddDummyCells;
    procedure GetMinMaxAbs(Canvas: TCanvas; var TotalMinWidth,
         TotalMaxWidth: integer; var MinWidths, MaxWidths: IntArray);
    procedure GetWidthsAbs(Canvas: TCanvas; TablWidth: integer; Specified: boolean;
                var MinWidths, MaxWidths: IntArray);
    procedure GetWidths(Canvas: TCanvas; var TotalMinWidth, TotalMaxWidth: integer;
              var MinWidths, MaxWidths: IntArray; TheWidth: integer);
    procedure xxx(const MaxWidths, MinWidths: IntArray; TheWidth: integer);
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType; override;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; override;
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer; override;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; override;
    function GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    function FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindSourcePos(DocPos: integer): integer; override;    
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; override;    
    procedure CopyToClipboard; override;
    end;

  XArray = array[0..300] of integer;
  PXArray = ^XArray;

  IndexObj = class
    Pos: integer;
    Index: integer;
    end;

  TSection = class(TSectionBase)
  {TSection holds <p>, <li>, many other things, and the base for lists}
  private
    SectionNumber: integer;   
    ThisCycle: integer;     
    function GetIndexObj(I: integer): IndexObj;
    property PosIndex[I: integer]: IndexObj read GetIndexObj;
  public
    BuffS: WideString; {holds the text for the section}
    Buff: PWideChar; {same as above}
    Brk: string;        
    XP: PXArray;
    BuffSize: integer;       {buffer may be larger}
    Fonts : TFontList;   {List of FontObj's in this section}
    Images: TImageObjList;   {list of TImageObj's, the images in section}
    FormControls: TList;      {list of TFormControls in section}
    SIndexList: TFreeList;    {list of Source index changes}
    Lines : TFreeList;   {List of LineRecs,  info on all the lines in section}
    Justify: JustifyType; {Left, Centered, Right}
    ClearAttr: ClearAttrType;
    LineHeight: integer;
    DrawX, DrawWidth: integer;
    AnchorName: boolean;
    StoredMin, StoredMax: integer;
    FirstLineIndent: integer;
    FLPercent: integer;   

    constructor Create(AMasterList: TSectionList;  L: TAttributeList;
                Prop: TProperties; AnURL: TUrlTarget; ACell: TCellBasic; FirstItem: boolean); 
    constructor CreateCopy(AMasterList: TSectionList; T: TSectionBase); override;
    destructor Destroy; override;
    procedure CheckFree;
    procedure Finish;
    procedure AddChar(C: WideChar; Index: integer); virtual;
    procedure AddTokenObj(T : TokenObj); virtual;
    procedure ProcessText; virtual;     
    procedure Allocate(N : integer);
    function AddImage(L: TAttributeList; ACell: TCellBasic; Index: integer): TImageObj;
    procedure AddPanel(AMasterList: TSectionList; L: TAttributeList; ACell: TCellBasic; Index: integer);
    function AddFormControl(Which: Symb; AMasterList: TSectionList;
         L: TAttributeList; ACell: TCellBasic; Index: integer;
         Prop: TProperties): TFormControlObj;  
    procedure ChangeFont(Prop: TProperties);
    procedure HRef(Sy: Symb; List: TSectionList; AnURL: TUrlTarget;
              Attributes: TAttributeList; Prop: TProperties);
    function FindCountThatFits(Canvas: TCanvas; Width: integer; Start: PWideChar; Max: integer): integer;
    function FindCountThatFits1(Canvas: TCanvas; Start: PWideChar; Max: integer; X, Y: integer; IMgr: IndentManager;
                    var ImgHt: integer; NxImages: TList) : integer;
    function FindTextWidth(Canvas: TCanvas; Start: PWideChar; N: integer; RemoveSpaces: boolean): integer;
    function FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: integer): integer;
    function DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer; override;
    function Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;  override;
    procedure CopyToClipboard; override;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType; override;
    function PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean; override;   
    function FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer; override;
    function FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer; override;
    function FindSourcePos(DocPos: integer): integer; override;
    function FindDocPos(SourcePos: integer; Prev: boolean): integer; override;    
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean; override;
    function GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean; override;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); override;
    end;

  TDrawList = class(TFreeList)
    procedure AddImage(Obj: TImageObj; Canvas: TCanvas; X: integer; TopY,
              YBaseline: integer; FO: TFontObj);
    procedure DrawImages;
    end;

  TCellBasic = class(TFreeList)   {a list which holds sections and blocks}
  public
    MasterList: TSectionList;  {the TSectionList that holds the whole document}
    YValue: integer;   {vertical position at top of cell}
    IMgr: IndentManager;
    StartCurs: integer;
    Len: integer;
    BkGnd: boolean;
    BkColor: TColor;
    tcContentBot, tcDrawTop, tcDrawBot: integer;
    OwnersTag: string;

    constructor Create(Master: TSectionList);
    constructor CreateCopy(AMasterList: TSectionList; T: TCellBasic);
    procedure Add(Item: TSectionBase);
    function CheckLastBottomMargin: boolean;
    procedure CopyToClipboard;
    function DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight: integer;
                  var ScrollWidth: integer; var Curs: integer): integer; virtual;
    procedure MinMaxWidth(Canvas: TCanvas; var Min, Max: integer); virtual;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y, XRef, YRef :integer): integer; virtual;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer; var UrlTarg: TUrlTarget;
        var FormControl: TImageFormControlObj; var ATitle: string): guResultType; virtual;
    function PtInObject(X: integer; Y: integer;  var Obj: TObject;
              var IX, IY: integer): boolean;
    function FindCursor(Canvas: TCanvas; X: Integer; Y: integer;
                var XR: integer; var YR: integer; var Ht: integer;
                var Intext: boolean): integer;
    function FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
    function FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
    function FindSourcePos(DocPos: integer): integer;
    function FindDocPos(SourcePos: integer; Prev: boolean): integer;
    function CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
    function GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
    procedure AddSectionsToList;
    procedure FormTree(Indent: string; var Tree: string);
    end;

  TCell = class(TCellBasic)
    DrawYY: integer;     

    constructor Create(Master: TSectionList);
    constructor CreateCopy(AMasterList: TSectionList; T: TCellBasic);
    destructor Destroy; override;
    function DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight: integer;
                  var ScrollWidth: integer; var Curs: integer): integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y, XRef, YRef:integer): integer; override;
    end;

  TBlockCell = class(TCellBasic)
    CellHeight: integer;

    function DoLogicX(Canvas: TCanvas; X, Y: integer; XRef, YRef, Width, AHeight: integer;
                  var ScrollWidth: integer; var Curs: integer): integer;
    end;

  TSectionList = class(TCell)    {a list of all the sections--holds document}
  Private
    procedure AdjustFormControls;

  Public
    ShowImages,              {set if showing images}
    Printing: boolean;       {set if printing -- also see IsCopy}
    YOff: integer;           {marks top of window that's displayed}
    YOffChange: boolean;     {when above changes}
    NoPartialLine: boolean;  {set when printing if no partial line allowed
                              at page bottom}
    SelB, SelE: integer;
    PreFontName : string[lf_FaceSize+1];  {<pre>, <code> font for document}
    LinkVisitedColor, LinkActiveColor,
    HotSpotColor: TColor;
    PrintTableBackground: boolean;
    PrintMonoBlack: boolean;      
    TheOwner: TWinControl;        {the viewer that owns this document}
    PPanel: TWinControl;          {the viewer's PaintPanel}
    GetBitmap: TGetBitmapEvent;   {for OnBitmapRequest Event}
    GetImage: TGetImageEvent;     {for OnImageRequest Event}
    ExpandName: TExpandNameEvent;
    ObjectClick: TObjectClickEvent;
    ObjectFocus: ThtObjectEvent;
    ObjectBlur: ThtObjectEvent;
    ObjectChange: ThtObjectEvent;
    BackGround: TColor;

    OnBackgroundChange: TNotifyEvent;
    BackgroundBitmap: TBitmap;
    BackgroundMask: TBitmap;
    BackgroundPRec: PtPositionRec; 
    BitmapName: String;      {name of background bitmap}
    BitmapLoaded: boolean;   {if background bitmap is loaded}
    htmlFormList: TFreeList;
    AGifList: TList;      {list of all animated Gifs}
    SubmitForm: TFormSubmitEvent;
    ScriptEvent: TScriptEvent;
    PanelCreateEvent: TPanelCreateEvent;
    PanelDestroyEvent: TPanelDestroyEvent;
    PanelPrintEvent: TPanelPrintEvent;  
    CB: SelTextCount;
    PageBottom: integer;
    PageShortened: boolean;
    MapList: TFreeList;    {holds list of client maps, TMapItems}
    Timer: TTimer;      {for animated GIFs}
    FormControlList:  TList;   {List of all TFormControlObj's in this SectionList}
    PanelList: TList;    {List of all TPanelObj's in this SectionList}
    MissingImages: TStringList;  {images to be supplied later}
    ControlEnterEvent: TNotifyEvent;
    LinkList: TList;    {List of links (TFontObj's)}
    ActiveLink: TFontObj;
    LinksActive: boolean;
    ActiveImage: TImageObj;
    ShowDummyCaret: boolean;
    Styles: TStyleList;    {the stylesheet}
    DrawList: TDrawList;
    FirstLineHtPtr: PInteger;
    IDNameList: TIDNameList;
    PositionList: TList;
    BitmapList: TStringBitmapList;
    SectionCount: integer;
    CycleNumber: integer;     
    ProgressStart: integer;   
    IsCopy: boolean;         {set when printing or making bitmap/metafile}
    TabOrderList: TStringList;
    FirstPageItem: boolean;

    constructor Create(Owner, APaintPanel: TWinControl);
    constructor CreateCopy(T: TSectionList);
    procedure Clear;
    procedure ClearLists;
    destructor Destroy; override;
    procedure CheckGIFList(Sender: TObject);
    procedure HideControls;
    procedure SetYOffset(Y: integer);
    function GetSelLength: integer;
    procedure CopyToClipboardA(Leng: integer);
    function GetSelTextBuf(Buffer: PWideChar; BufSize: integer): integer;
    procedure SetFonts(const Name, PreName: string; ASize: integer;
              AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor;
              LnksActive: boolean; LinkUnderLine: boolean; ACharSet: TFontCharSet;
              MarginHeight, MarginWidth: integer);
    procedure SetBackground(ABackground: TColor);
    procedure SetBackgroundBitmap(Name: String; const APrec: PtPositionRec);
    function GetBackgroundBitmap: TBitmap;
    function FindSectionAtPosition(Pos: integer;
             var TopPos: integer; var Index: integer): TSectionBase;
    procedure CancelActives;
    function GetURL(Canvas: TCanvas; X: integer; Y: integer; var UrlTarg: TUrlTarget;
          var FormControl: TImageFormControlObj; var ATitle: string): guResultType; override;
    procedure LButtonDown(Down: boolean);   
    function GetTheBitmap(const BMName: String; var Transparent: Transparency;
               var AMask: TBitmap; var FromCache, Delay: boolean): TPersistent;
    function DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight: integer;
                  var ScrollWidth: integer; var Curs: integer): integer; override;
    function Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y, XRef, YRef :integer): integer; override;
    procedure InsertImage(const Src: string; Stream: TMemoryStream; var Reformat: boolean);
    function GetFormcontrolData: TFreeList;     
    procedure SetFormcontrolData(T: TFreeList); 
    function FindDocPos(SourcePos: integer; Prev: boolean): integer;   
  end;

  TCellObj = class(TObject)  {holds a TCell and some other information}
    ColSpan, RowSpan,      {column and row spans for this cell}
    Wd: integer;  {total width (may cover more than one column)}
    Ht,           {total height (may cover more than one row)}
    VSize: integer;     {Actual vertical size of contents}
    SpecHt: integer;    {Height as specified}
    YIndent: integer;   {Vertical indent}
    VAlign: AlignmentType;  {Top, Middle, or Bottom}
    WidthAttr: integer;   {Width attribute (percentage or absolute)}
    AsPercent: boolean;   {it's a percent}
    EmSize, ExSize: integer;
    PRec: PtPositionRec;
    PadTop, PadRight, PadBottom, PadLeft: integer;
    BrdTop, BrdRight, BrdBottom, BrdLeft: integer;   
    HzSpace, VrSpace: integer;   
    BorderStyle: BorderStyleType;  
    Cell: TCell;

    NeedDoImageStuff: boolean;
    BGImage: TImageObj;
    TiledImage, TiledMask, FullBG: TBitmap;   
    MargArray: TMarginArray;
    MargArrayO: TVMarginArray;
    NoMask: boolean;
    BreakBefore, BreakAfter, KeepIntact: boolean;

    constructor Create(Master: TSectionList; AVAlign: AlignmentType;
                Attr: TAttributeList; Prop: TProperties);
    constructor CreateCopy(AMasterList: TSectionList; T: TCellObj);
    destructor Destroy; override;
    private
    procedure InitializeCell(TablePadding: integer; const BkImageName: string;
                                           const APRec: PtPositionRec);
    procedure Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacing: integer;
               Border: boolean; Light, Dark: TColor; Rgn: THandle);
    procedure DrawLogic2(Canvas: TCanvas; Y, CellSpacing: integer;
                var Curs: integer);
    end;

const
  ImageSpace = 3;   {extra space for left, right images}
  ListIndent = 35;    

var
  CurrentStyle: TFontStyles;  {as set by <b>, <i>, etc.}
  CurrentForm: ThtmlForm;

implementation
         
uses
  {$ifdef Delphi6_Plus}
  Variants,
  {$endif}
  HTMLView, ReadHTML, HTMLSbs1;

type
  TSectionClass = Class of TSectionBase;
  EProcessError = class(Exception);

  TFormRadioButton = class(TRadioButton)  
    private
      IDName: string;
      procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    end;

  TFormCheckBox = class(TCheckBox)  
    private
      procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    end;

  TTabControl = class(TWinControl)   
  private
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;  
    protected
      property OnEnter;
      property OnExit;
      property TabStop;
      property OnKeyUp;
    end;

procedure IndentManager.Update(Y: integer; Img: TFloatingObj);
{Given a new floating image, update the edge information.  Fills  Img.Indent,
 the distance from the left edge to the upper left corner of the image}
var
  IH, IW: integer;
  IR: IndentRec;
  LIndent: integer;
begin
if Assigned(Img) then
  begin
  IW := Img.ImageWidth + Img.HSpaceL + Img.HSpaceR;
  IH := Img.ImageHeight + Img.VSpaceT + Img.VSpaceB;
  if (Img.ObjAlign = ALeft) then
    begin
    IR := IndentRec.Create;
    with IR do
      begin
      LIndent := LeftIndent(Y);    
      Img.Indent := LIndent-LfEdge+Img.HSpaceL;
      X := LIndent-LfEdge + IW;
      YT := Y;
      YB := Y + IH;
      L.Add(IR);
      end;
    end
  else if (Img.ObjAlign = ARight) then
    begin
    IR := IndentRec.Create;
    with IR do
      begin
      X := RightSide(Y) - IW;    
      Img.Indent := X + Img.HSpaceL;
      YT := Y;
      YB := Y + IH;
      R.Add(IR);
      end;
    end;
  end;
end;

procedure IndentManager.UpdateBlock(Y: integer; IW: integer; IH: integer;
                           Justify: AlignmentType);
{For a floating block, update the edge information. }
var
  IR: IndentRec;
begin
  IR := IndentRec.Create;
  if (Justify = ALeft) then
    begin
    with IR do
      begin
      X := -LfEdge + IW;
      YT := Y;
      YB := Y + IH;
      Float := True;
      L.Add(IR);
      end;
    end
  else if (Justify = ARight) then
    begin
    with IR do
      begin
      X := RightSide(Y) - IW;
      YT := Y;
      YB := Y + IH;
      Float := True;
      R.Add(IR);
      end;
    end;
end;

constructor TFontObj.Create(ASection: TSection; F: TMyFont; Position: integer);
begin
inherited Create;
Section := ASection;
TheFont := F;
Pos := Position;
UrlTarget := TUrlTarget.Create;
FontChanged;
end;

{$ifndef NoTabLink}
procedure TFontObj.EnterEvent(Sender: TObject);  
var
  List: TList;
  I, J: integer;
begin
Active := True;
{Make adjacent fonts in this link active also}
List := Section.ParentSectionList.LinkList;
I := List.IndexOf(Self);
if I >= 0 then
  for J := I+1 to List.Count-1 do
    if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
      TFontObj(List[J]).Active := True
    else Break;
Section.ParentSectionList.ControlEnterEvent(Self);
end;

procedure TFontObj.ExitEvent(Sender: TObject);  
var
  List: TList;
  I, J: integer;
begin
Active := False;
{Make adjacent fonts in this link inactive also}
List := Section.ParentSectionList.LinkList;
I := List.IndexOf(Self);
if I >= 0 then
  for J := I+1 to List.Count-1 do
    if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
      TFontObj(List[J]).Active := False
    else Break;
Section.ParentSectionList.PPanel.Invalidate;
end;

procedure TFontObj.AssignY(Y: integer);
var
  List: TList;
  I, J: integer;
begin
if UrlTarget.Url = '' then Exit;
if Assigned(TabControl) then
  FYValue := Y
else
  begin  {Look back for the TFontObj with the TabControl}
  List := Section.ParentSectionList.LinkList;
  I := List.IndexOf(Self);
  if I >= 0 then
    for J := I-1 downto 0 do
      if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
        begin
        if Assigned(TFontObj(List[J]).TabControl) then
          begin
          TFontObj(List[J]).FYValue := Y;
          break;
          end;
        end
      else Break;
  end;
end;

procedure TFontObj.AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Viewer: ThtmlViewer;
begin
Viewer := ThtmlViewer(Section.ParentSectionList.TheOwner);
if (Key = vk_Return) then
  begin
  Viewer.Url := UrlTarget.Url;
  Viewer.Target := UrlTarget.Target;
  Viewer.UrlAction;
  end
else  {send other keys to ThtmlViewer}
  Viewer.KeyDown(Key, Shift);
end;

procedure TFontObj.CreateTabControl(TabIndex: integer);     
var
  PntPanel: TPaintPanel;
  I, J: integer;
  List: TList;
begin
if Assigned(TabControl) then
  Exit;
  {Look back for the TFontObj with the TabControl}
List := Section.ParentSectionList.LinkList;
I := List.IndexOf(Self);
if I >= 0 then
  for J := I-1 downto 0 do
    if (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) then
      if Assigned(TFontObj(List[J]).TabControl) then
        Exit;

PntPanel := TPaintPanel(Section.ParentSectionList.PPanel);
TabControl := TTabControl.Create(PntPanel);
with TTabControl(TabControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Width := 1;
  Height := 1;
  TabStop := True;
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnKeyDown := Self.AKeyDown;
  end;
TabControl.Parent := PntPanel;

if TabIndex > 0 then
  {Adding leading 0's to the number string allows it to be sorted numerically,
   and the Count takes care of duplicates}
  with Section.ParentSectionList.TabOrderList do
    AddObject(Format('%.5d%.3d', [TabIndex, Count]), Self);
end;
{$endif}

procedure TFontObj.CreateFIArray;
begin
if not Assigned(FIArray) then
  FIArray := TFontInfoArray.Create;
end;

procedure TFontObj.ReplaceFont(F: TMyFont);
begin
TheFont.Free;
TheFont := F;
FontChanged;
end;

procedure TFontObj.ConvertFont(FI: ThtFontInfo);
begin
  with TheFont, FI do
    begin
    Name := iName;
    Size := iSize;
    Style := iStyle;
    bgColor := ibgColor;
    Color := iColor;
    CharSet:= ICharSet;
    FontChanged;
    end;
end;

constructor TFontObj.CreateCopy(ASection: TSection; T: TFontObj); 
begin
inherited Create;
Section := ASection;
Pos := T.Pos;
SScript := T.SScript;
TheFont := TMyFont.Create;
TheFont.Assign(T.TheFont);
if Assigned(T.FIArray) then
  ConvertFont(T.FIArray.Ar[LFont]);
UrlTarget := TUrlTarget.Create;
UrlTarget.Copy(T.UrlTarget);
FontChanged;
end;

destructor TFontObj.Destroy;
begin
FIArray.Free;
TheFont.Free;
UrlTarget.Free;
TabControl.Free;   
inherited Destroy;
end;

procedure TFontObj.SetVisited(Value: boolean);    
begin
if Value <> FVisited then
  begin
  FVisited := Value;
  if Value then
    if Hover then
      ConvertFont(FIArray.Ar[HVFont])
    else
      ConvertFont(FIArray.Ar[VFont])
  else
    if Hover then
      ConvertFont(FIArray.Ar[HLFont])
    else
      ConvertFont(FIArray.Ar[LFont]);
  FontChanged;
  end;
end;

procedure TFontObj.SetHover(Value: boolean);
begin
if Value <> FHover then
  begin
  FHover := Value;
  if Value then
    if FVisited then
      ConvertFont(FIArray.Ar[HVFont])
    else ConvertFont(FIArray.Ar[HLFont])
  else
    if FVisited then
      ConvertFont(FIArray.Ar[VFont])
    else ConvertFont(FIArray.Ar[LFont]);
  FontChanged;
  end;
end;

procedure TFontObj.SetAllHovers(List: TList; Value: boolean);
{Set/Reset Hover on this item and all adjacent item with the same URL}
var
  I, J: integer;
begin
SetHover(Value);
I := List.IndexOf(Self);
if I >= 0 then
  begin
  J := I+1;
  while (J < List.Count) and (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) do
    begin
    TFontObj(List[J]).Hover := Value;
    Inc(J);
    end;
  J := I-1;
  while (J >= 0) and (Self.UrlTarget.ID = TFontObj(List[J]).UrlTarget.ID) do
    begin
    TFontObj(List[J]).Hover := Value;
    Dec(J);
    end;
  end;
end;

function TFontObj.GetURL: string;
begin
try                            
  Result := UrlTarget.Url;
except
  Result := '';
  {$ifdef DebugIt}
  ShowMessage('Bad TFontObj, htmlsubs.pas, TFontObj.GetUrl');
  {$endif}
  end;
end;

procedure TFontObj.FontChanged;
var
  Save: THandle;
  tm : TTextmetric;
  DC: HDC;
  SaveCharset: TFontCharset;
begin
DC := GetDC(0);
{if this is a Symbol Charset, then keep it as a symbol character set}  
SaveCharset := TheFont.Charset;
TheFont.Charset := Default_Charset;
Save := SelectObject(DC, TheFont.Handle);
GetTextMetrics(DC, tm);
SelectObject(DC, Save);
if tm.tmCharset = Symbol_Charset then
  TheFont.Charset := Symbol_Charset
else TheFont.Charset := SaveCharset;

Save := SelectObject(DC, TheFont.Handle);
GetTextMetrics(DC, tm);
tmHeight := tm.tmHeight;
tmMaxCharWidth := tm.tmMaxCharWidth;
FontHeight := tm.tmHeight + tm.tmExternalLeading;
Descent := tm.tmDescent;
if fsItalic in TheFont.Style then  {estimated overhang}
  Overhang := tm.tmheight div 10
else Overhang := 0;
SelectObject(DC, Save);
ReleaseDC(0, DC);
end;

function TFontObj.GetOverhang: integer;
begin
Result := Overhang;
end;

function TFontObj.GetHeight(var Desc: integer): integer;
begin
Desc := Descent;
Result := FontHeight;
end;

constructor TFontList.CreateCopy(ASection: TSection; T: TFontList);
var
  I: integer;
begin
inherited create;
for I := 0 to T.Count-1 do
  Add(TFontObj.CreateCopy(ASection, TFontObj(T.Items[I])));
end;

function TFontList.GetFontAt(Posn : integer;
                var OHang : integer) : TMyFont;
{given a character index, find the font that's effective there}
var
  I, PosX: integer;
  F : TFontObj;
begin
I := 0;
PosX := 0;
while (I < Count)  do
  begin
  PosX := TFontObj(Items[I]).Pos;
  Inc(I);
  if PosX >= Posn then Break;
  end;
Dec(I);
if PosX > Posn then Dec(I);
F := TFontObj(Items[I]);
OHang := F.Overhang;   
Result := F.TheFont;
end;

function TFontList.GetFontCountAt(Posn, Leng : integer) : integer;
{Given a position, return the number of chars before the font changes}
var
  I, PosX : integer;
begin
I := 0;
PosX := 0;
while I < Count do
  begin
  PosX := TFontObj(Items[I]).Pos;
  if PosX >= Posn then Break;
  Inc(I);
  end;
if PosX = Posn then Inc(I);
if I = Count then
  Result := Leng-Posn
else
  Result := TFontObj(Items[I]).Pos - Posn;
end;

{----------------TFontList.GetFontObjAt}
function TFontList.GetFontObjAt(Posn : integer;
                      var Index : integer) : TFontObj;
{Given a position, returns the FontObj which applies there and the index of
 the FontObj in the list}
var
  PosX: integer;
begin
Index := 0;
PosX := 0;
while (Index < Count)  do
  begin
  PosX := TFontObj(Items[Index]).Pos;
  Inc(Index);
  if PosX >= Posn then Break;
  end;
Dec(Index);
if PosX > Posn then Dec(Index);
Result := TFontObj(Items[Index]);
end;

{----------------TFontList.Decrement}
procedure TFontList.Decrement(N: integer; ParentSectionList: TSectionList);   
{called when a character is removed to change the Position figure}
var
  I, J: integer;
  FO, FO1: TFontObj;
begin
I := 0;
while I < Count do
  begin
  FO := TFontObj(Items[I]);
  if FO.Pos > N then
      Dec(FO.Pos);
  if (I > 0) and (TFontObj(Items[I-1]).Pos = FO.Pos) then
    begin
    FO1 := TFontObj(Items[I-1]);
    J := ParentSectionList.LinkList.IndexOf(FO1);
    if J >=0 then
      ParentSectionList.LinkList.Delete(J);
    FO1.Free;
    Delete(I-1);
    end
  else Inc(I);
  end;
end;

{----------------TImageObj.Create}
constructor TImageObj.Create(MasterList: TSectionList; Position: integer; L: TAttributeList);
var
  I: integer;
  S: string;
  NewSpace: integer;
  T: TAttribute;
begin
inherited Create;
ParentSectionList := MasterList;
Pos := Position;
ObjAlign := ABottom;   {default}
NewSpace := -1;
for I := 0 to L.Count-1 do
  with TAttribute(L[I]) do
    case Which of
      SrcSy: Source := Name;
      AltSy:
        begin
        FAlt := Name;
        ImageTitle := FAlt;    {use Alt as default Title}
        end;
      IsMapSy:  IsMap := True;
      UseMapSy:
        begin
        UseMap := True;
        S := Trim(Uppercase(Name));
        if (Length(S) > 1) and (S[1] = '#') then
          System.Delete(S, 1, 1);
        MapName := S;
        end;
      AlignSy:
        begin
        S := UpperCase(Name);
        if S = 'TOP' then ObjAlign := ATop
        else if (S = 'MIDDLE') or (S = 'ABSMIDDLE') then ObjAlign := AMiddle
        else if S = 'LEFT' then ObjAlign := ALeft
        else if S = 'RIGHT' then ObjAlign := ARight;
        end;
      BorderSy: NoBorder := Value = 0;
      TranspSy: Transparent := LLCorner;
      HeightSy: SpecHeight := Intmax(1, Value); {spec ht of 0 becomes 1}
      WidthSy: if System.Pos('%', Name) = 0 then
                   SpecWidth := Value
               else if (Value > 0) and (Value <=100) then
                 begin
                 SpecWidth := Value;
                 PercentWidth := True;
                 end;
      HSpaceSy:  NewSpace := IntMin(40, Abs(Value)); 
      VSpaceSy:  VSpaceT := IntMin(40, Abs(Value));
      ActiveSy:  FHoverImage := True;
      NameSy: ParentSectionList.IDNameList.AddObject(Name, Self);  
      end;
if L.Find(TitleSy, T) then
  ImageTitle := T.Name;   {has higher priority than Alt loaded above}
if L.TheID <> '' then
  ParentSectionList.IDNameList.AddObject(L.TheID, Self);

if NewSpace >= 0 then   
  HSpaceL := NewSpace
else if ObjAlign in [ALeft, ARight] then
  HSpaceL := ImageSpace   {default}
else HSpaceL := 0;
HSpaceR := HSpaceL;
VSpaceB := VSpaceT;
end;

constructor TImageObj.SimpleCreate(MasterList: TSectionList; const AnURL: string);
begin
inherited Create;
ParentSectionList := MasterList;
ObjAlign := ABottom;   {default}
Source := AnURL;
NoBorder := True;
end;

procedure TImageObj.ProcessProperties(Prop: TProperties);
var
  MargArrayO: TVMarginArray;
  MargArray: TMarginArray;
  Align: AlignmentType;
  EmSize, ExSize: integer;
begin
if Prop.GetVertAlign(Align) then
  ObjAlign := Align;
if Prop.GetFloat(Align) and (Align <> ANone) then
  begin
  if HSpaceR = 0 then
    begin  {default is different for Align = left/right}
    HSpaceR := ImageSpace;
    HSpaceL := ImageSpace;
    end;
  ObjAlign := Align;
  end;
if ImageTitle = '' then  {a Title attribute will have higher priority than inherited}
  ImageTitle := Prop.PropTitle;    
Prop.GetVMarginArray(MargArrayO);
EmSize := Prop.EmSize;
ExSize := Prop.ExSize;
ConvInlineMargArray(MargArrayO, 200, 200, EmSize, ExSize, MargArray);

if MargArray[MarginLeft] <> IntNull then
  HSpaceL := MargArray[MarginLeft];
if MargArray[MarginRight] <> IntNull then
  HSpaceR := MargArray[MarginRight];
if MargArray[MarginTop] <> IntNull then
  VSpaceT := MargArray[MarginTop];
if MargArray[MarginBottom] <> IntNull then
  VSpaceB := MargArray[MarginBottom];

if MargArray[Width] <> IntNull then
  begin
  if MargArray[Width] = Auto then
    SpecWidth := 0
  else
    SpecWidth := MargArray[Width];
  PercentWidth := False;
  end;
if MargArray[Height] <> IntNull then
  begin
  if MargArray[Height] = Auto then
    SpecHeight := 0
  else
    SpecHeight := MargArray[Height];
  end;

if Prop.GetVertAlign(Align) then
  ObjAlign := Align;
if Prop.GetFloat(Align) and (Align <> ANone) then
  ObjAlign := Align;
end;

constructor TImageObj.CreateCopy(AMasterList: TSectionList; T: TImageObj);
begin
inherited CreateCopy(T);
ParentSectionList := AMasterList;
ImageKnown := T.ImageKnown;
ObjHeight := T.ObjHeight;
ObjWidth := T.ObjWidth;
SpecHeight := T.SpecHeight;
SpecWidth := T.SpecWidth;
PercentWidth := T.PercentWidth;
NoBorder := T.NoBorder;
FAlt := T.FAlt;
Image := T.Image;  
Mask := T.Mask;
IsMap := T.IsMap;
Transparent := T.Transparent;
FBitmap := Nil;
end;

destructor TImageObj.Destroy;
begin
if not ParentSectionList.IsCopy then
  begin
  if (Source <> '') and Assigned(OrigImage) then  
    ParentSectionList.BitmapList.DecUsage(Source);
  if Swapped and (Image <> OrigImage) then   
    begin      {not in cache}
    Image.Free;
    Mask.Free;
    end;
  if (OrigImage is TGifImage) and TGifImage(OrigImage).IsCopy then
    OrigImage.Free;
  FBitmap.Free;
  end;
inherited Destroy;
end;

function TImageObj.GetBitmap: TBitmap;    
begin
Result := Nil;
if Image = ErrorBitmap then Exit;
if (Image is TGifImage) then
  Result := TGifImage(Image).Bitmap
else if (Image is TBitmap) then
  begin
  if Assigned(FBitmap) then
    Result := FBitmap
  else
    begin
    FBitmap := TBitmap.Create;
    FBitmap.Assign(TBitmap(Image));
    if ColorBits = 8 then            
      FBitmap.Palette := CopyPalette(ThePalette);
    Result := FBitmap;
    end;  
  end;
end;

procedure TImageObj.SetHover(Value: HoverType);    
begin
if (Value <> FHover) and FHoverImage and (Image is TGifImage) then   
  with TGifImage(Image) do
    begin
    if Value <> hvOff then
      case NumFrames of
        2: CurrentFrame := 2;
        3: if Value = hvOverDown then
            CurrentFrame := 3
           else CurrentFrame := 2;
      else
        begin
        Animate := True;
        ParentSectionList.AGifList.Add(Image);
        end;
      end
    else
      begin
      Animate := False;
      CurrentFrame := 1;
      ParentSectionList.AGifList.Remove(Image);
      end;
    FHover := Value;
    ParentSectionList.PPanel.Invalidate;
    end;
end;    

{----------------TImageObj.ReplaceImage}
procedure TImageObj.ReplaceImage(NewImage: TStream);   
var
  TmpImage: TPersistent;
  NonAnimated: boolean;
  AMask: TBitmap;
  Stream: TMemoryStream;
  Tmp: TGifImage;
  I: integer;
begin
Transparent := NotTransp;
AMask := Nil;
TmpImage := Nil;
Stream := TMemoryStream.Create;
try
  Stream.LoadFromStream(NewImage);
  if Assigned(Stream) and (Stream.Memory <> Nil) and (Stream.Size >= 1) then
    begin
    NonAnimated := True;
    if KindOfImage(Stream.Memory) in [GIF, Gif89] then
      TmpImage := CreateAGifFromStream(NonAnimated, Stream);
    if Assigned(TmpImage) then
      begin
      if NonAnimated then
        begin     {else already have animated GIF}
        Tmp := TGifImage(TmpImage);
        TmpImage := TBitmap.Create;
        TmpImage.Assign(Tmp.MaskedBitmap);
        if Tmp.IsTransparent then
          begin
          AMask := TBitmap.Create;
          AMask.Assign(Tmp.Mask);
          Transparent := TGif;
          end;
        Tmp.Free;
        end;
      end
    else
      TmpImage := GetImageAndMaskFromStream(Stream, Transparent, AMask);
    end;
finally
  Stream.Free;
  end;
if Assigned(TmpImage) then
  begin
  if not Swapped then
    begin
    {OrigImage is left in cache and kept}
    if (Image is TGifImage) then
      ParentSectionList.AGifList.Remove(Image);
    Swapped := True;
    end
  else   {swapped already}
    begin
    if (Image is TGifImage) then
      begin
      ParentSectionList.AGifList.Remove(Image);
      end;
    Image.Free;     
    FreeAndNil(Mask);
    end;
  FreeAndNil(FBitmap);
  Image := TmpImage;
  if (Image is TGifImage) then
    begin
    if not FHoverImage then
      begin
      TGifImage(Image).Animate := True;
      ParentSectionList.AGifList.Add(Image);
      end
    else
      begin
      TGifImage(Image).Animate := False;
      SetHover(hvOff);
      end;
    end;
  Mask := AMask;
  if Missing then
    begin    {if waiting for image, no longer want it}
    with ParentSectionList.MissingImages do
      for I := 0 to count-1 do
        if Objects[I] = Self then
          begin
          Delete(I);
          break;
          end;
    Missing := False;
    end;
  ParentSectionList.PPanel.Invalidate;
  end;
end;

{----------------TImageObj.InsertImage}
function TImageObj.InsertImage(const UName: string; var Reformat: boolean): boolean;
var
  TmpImage: TPersistent;
  FromCache, IsAniGIF, Delay: boolean;
begin
Result := False;
Reformat := False;
if (Image = DefBitmap) then
  begin
  Result := True;
  TmpImage := ParentSectionList.GetTheBitmap(UName, Transparent, Mask, FromCache, Delay);
  if not Assigned(TmpImage) then
    Exit;
  IsAniGIF := TmpImage is TGifImage;

  if IsAniGIF then
    begin
    if FromCache then   {it would be}
      Image := TGifImage.CreateCopy(TGifImage(TmpImage))  {it's in Cache already, make copy}
    else
      Image := TmpImage;
    if not FHoverImage then
      begin
      ParentSectionList.AGifList.Add(Image);
      TGifImage(Image).Animate := True;
      if Assigned(ParentSectionList.Timer) then
        ParentSectionList.Timer.Enabled := True;
      end
    else TGifImage(Image).Animate := False;
    end
  else Image := TmpImage;
  OrigImage := Image;
  Missing := False;    

  if not ImageKnown then
    Reformat := True;  {need to get the dimensions}
  end;
end;

{----------------TImageObj.DrawLogic}
procedure TImageObj.DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
              FO: TFontObj; AvailableWidth: integer);
{calculate the height and width}
var
  TmpImage: TPersistent;
  ImHeight, ImWidth: integer;
  ViewImages, FromCache: boolean;
  AltWidth, AltHeight: integer;
  Rslt: string;

begin
ViewImages := ParentSectionList.ShowImages;

TmpImage := Image;
if ViewImages and not Assigned(TmpImage) then
  begin
  if Source <> '' then
    with SectionList do
      begin
      if not Assigned(GetBitmap) and not Assigned(GetImage) then
        Source := (TheOwner as ThtmlViewer).HTMLExpandFilename(Source)
      else if Assigned(ExpandName) then
        begin
        ExpandName(TheOwner, Source, Rslt);
        Source := Rslt;
        end;
      if MissingImages.IndexOf(Uppercase(Source)) = -1 then
        TmpImage := ParentSectionList.GetTheBitmap(Source, Transparent, Mask, FromCache, Missing)
      else Missing := True;  {already in list, don't request it again}  
      end;
  if not Assigned(TmpImage) then
    begin
    if Missing then  
      begin
      Image := DefBitmap;
      TmpImage := DefBitmap;
      ParentSectionList.MissingImages.AddObject(Source, Self); {add it even if it's there already}
      end
    else
      begin
      Image := ErrorBitmap;
      TmpImage := ErrorBitmap;
      Mask := ErrorBitmapMask;
      Transparent := LLCorner;
      end;
    end
  else if TmpImage is TGifImage then
    begin
    if FromCache then
      begin  {it's in Cache already, make copy}
      Image := TGifImage.CreateCopy(TGifImage(TmpImage));
      TmpImage := Image;
      end
    else
      Image := TmpImage;
    OrigImage := Image;   
    if not FHoverImage then
      ParentSectionList.AGifList.Add(Image)
    else TGifImage(Image).Animate := False;
    end
  else
    begin
    Image := TBitmap(TmpImage);
    OrigImage := Image;  
    end;
  end;
if not ViewImages then
  TmpImage := DefBitMap;

if TmpImage is TGifImage then
  begin
  ImHeight := TGifImage(TmpImage).Height;
  ImWidth := TGifImage(TmpImage).Width;
  end
else
  begin
  ImHeight := TBitmap(TmpImage).Height;
  ImWidth := TBitmap(TmpImage).Width;
  end;

if not ImageKnown then
  if not ((Image = ErrorBitmap) or (TmpImage = DefBitmap)) then
    begin
    if PercentWidth then
      begin
      ObjWidth := MulDiv(AvailableWidth, SpecWidth, 100);
      if SpecHeight <> 0 then ObjHeight := SpecHeight
        else ObjHeight := MulDiv(ObjWidth, ImHeight, ImWidth);  
      end
    else if (SpecWidth <> 0) and (SpecHeight <> 0) then
      begin       {Both width and height specified}
      ObjHeight := SpecHeight;
      ObjWidth := SpecWidth;
      ImageKnown := True;
      end
    else if SpecHeight <> 0 then
      begin
      ObjHeight := SpecHeight;
      ObjWidth := MulDiv(SpecHeight, ImWidth, ImHeight);
      ImageKnown := True;
      end
    else if SpecWidth <> 0 then
      begin
      ObjWidth := SpecWidth;
      ObjHeight := MulDiv(SpecWidth, ImHeight, ImWidth);
      ImageKnown := True;
      end
    else
      begin       {neither height and width specified}
      ObjHeight := ImHeight;
      ObjWidth := ImWidth;
      ImageKnown := True;
      end;
    end
  else {don't know the image yet}
    if (SpecHeight <> 0) and (SpecWidth <> 0) then
      begin       {Both width and height specified}
      ObjHeight := SpecHeight;
      ObjWidth := SpecWidth;
      ImageKnown := True;   {do know the image size}
      end
    else
      begin       {neither height and width specified}
      ObjHeight := ImHeight;
      ObjWidth := ImWidth;
      end;

if (not ViewImages or (TmpImage = ErrorBitmap) or (Image = DefBitmap))
       and Not ImageKnown then
  begin
  Canvas.Font.Name := 'Arial';{use same font as in Draw}
  Canvas.Font.Size := 8;      {should be option?}
  if FAlt <> '' then
    begin
    AltWidth := Canvas.TextWidth(FAlt) + 2;
    AltHeight := Canvas.TextHeight(FAlt);
    end
  else
    begin
    AltHeight := 0;
    AltWidth := 0;
    end;
  ObjWidth := IntMax(ObjWidth, 16+8 + AltWidth);
  ObjHeight := IntMax(ObjHeight, IntMax(16+8, AltHeight));
  end;

ImageHeight := ObjHeight;
ImageWidth := ObjWidth;

HasBlueBox := not NoBorder and Assigned(FO) and (FO.URLTarget.Url <> '');

if HasBlueBox then
  begin
  Inc(ImageHeight, 2);      {extra pixel top and bottom for rectangle}
  Inc(ImageWidth, 2);
  end;
end;

{----------------TImageObj.DoDraw}
procedure TImageObj.DoDraw(Canvas: TCanvas; XX: integer; Y: integer;
               ddImage: TPersistent; ddMask: TBitmap);
{Y relative to top of display here}
var
  DC: HDC;
  Img: TBitmap;
  W, H: integer;
  BMHandle: HBitmap;
  PrintTransparent: boolean;

begin
if (ddImage is TGifImage) and not ParentSectionList.IsCopy then
  with TGifImage(ddImage) do
    begin
    ShowIt := True;
    Visible := True;
    Draw(Canvas, ParentSectionList, MyCell, XX, Y, ObjWidth, ObjHeight);
    Exit;
    end;
DC := Canvas.Handle;
try
  if not ParentSectionList.IsCopy then
    begin
    if ((Transparent <> NotTransp) or (ddImage = ErrorBitmap)) and Assigned(ddMask) then  
      if ddImage = ErrorBitmap then
        FinishTransparentBitmap(DC, TBitmap(ddImage), Mask, XX, Y,
                    TBitmap(ddImage).Width, TBitmap(ddImage).Height)
      else
        FinishTransparentBitmap(DC, TBitmap(ddImage), Mask, XX, Y, ObjWidth, ObjHeight)
    else
      begin
      Img := TBitmap(ddImage);
      if (ddImage = DefBitMap) or (ddImage = ErrorBitmap) then
        BitBlt(DC, XX, Y, Img.Width, Img.Height, Img.Canvas.Handle, 0, 0, SRCCOPY)
      else
        begin
        SetStretchBltMode(DC, ColorOnColor);
        StretchBlt(DC, XX, Y, ObjWidth, ObjHeight, Img.Canvas.Handle, 0, 0, Img.Width, Img.Height, SRCCOPY);
        end;
      end;
    end
  else
    begin       {printing}
    if ddImage is TGifImage then
      with TGifImage(ddImage) do
        begin
        ddMask := Mask;
        if Assigned(ddMask) then Transparent := TGif;
        ddImage := MaskedBitmap;
        TBitmap(ddImage).Palette := CopyPalette(ThePalette);
        TBitmap(ddImage).HandleType := bmDIB;
        end;
    if (ddImage = DefBitMap) or (ddImage = ErrorBitmap) then
      begin
      W := TBitmap(ddImage).Width;
      H := TBitmap(ddImage).Height;
      end
    else
      begin
      W := ObjWidth;
      H := ObjHeight;
      end;

    PrintTransparent := ((Transparent <> NotTransp) or (ddImage = ErrorBitmap))
                     and Assigned(ddMask);
    if PrintTransparent then
      PrintTransparentBitmap3(Canvas, XX, Y, W, H, TBitmap(ddImage), ddMask, 0, TBitmap(ddImage).Height)
    else
      begin   {printing, not transparent}
      BMHandle := TBitmap(ddImage).Handle;
      PrintBitmap(Canvas, XX, Y, W, H, BMHandle);
      end;
    end;
except
  end;
end;

{----------------TImageObj.Draw}
procedure TImageObj.Draw(Canvas: TCanvas; X: integer; TopY, YBaseline: integer;
                                 FO: TFontObj);
var
  TmpImage: TPersistent;
  TmpMask: TBitmap;
  MiddleAlignTop: integer;
  ViewImages: boolean;
  SubstImage: boolean;
  Ofst: integer;

begin
with ParentSectionList do
  begin
  ViewImages := ShowImages;
  Dec(TopY, YOff);
  Dec(YBaseLine, YOff);
  end;
if ViewImages then
  begin
  TmpImage := Image;
  if Image is TBitmap then
    TmpMask := Mask
  else TmpMask := Nil;
  end
else
  begin
  TmpImage := DefBitMap;
  TmpMask := Nil;
  end;
SubstImage := not ViewImages or (TmpImage = ErrorBitmap) or (TmpImage = DefBitmap); {substitute image}

with Canvas do
  begin
  Brush.Style := bsClear;
  Font.Size := 8;
  Font.Name := 'Arial';        {make this a property?}
  if SubstImage then Ofst := 4 else Ofst := 0;
  if ObjAlign = AMiddle then
    MiddleAlignTop := YBaseLine+FO.Descent-(FO.tmHeight div 2)-((ImageHeight-VSpaceT+VSpaceB) div 2)
  else MiddleAlignTop := 0;   {not used}

  DrawXX := X;
  case ObjAlign of
      ALeft, ARight, ATop: DrawYY := TopY+VSpaceT;
      AMiddle: DrawYY := MiddleAlignTop;
      ABottom, ABaseline: DrawYY := YBaseLine-ImageHeight-VSpaceB;
      end;
  if HasBlueBox then
    begin
    Inc(DrawXX, 1);
    Inc(DrawYY, 1);
    end;

  if not SubstImage or (ObjHeight >= 16+8) and (ObjWidth >= 16+8) then
    DoDraw(Canvas, DrawXX+Ofst, DrawYY+Ofst, TmpImage, TmpMask);
  Inc(DrawYY, ParentSectionList.YOff);
  SetTextAlign(Canvas.Handle, TA_Top);
  if SubstImage and not HasBlueBox then
    begin
    Font.Color := FO.TheFont.Color;   
    {calc the offset from the image's base to the alt= text baseline}
    case ObjAlign of
      ATop, ALeft, ARight:
        begin
        if FAlt <> '' then
          WrapText(Canvas, X+24, TopY+Ofst+VSpaceT, X+ObjWidth-2, TopY+ObjHeight-1+VSpaceT, FAlt);
        RaisedRect(ParentSectionList, Canvas, X, TopY+VSpaceT,
                                      X+ObjWidth-1, TopY+ObjHeight-1+VSpaceT, False);
        end;
      AMiddle:
        begin   {MiddleAlignTop is always initialized}
        if FAlt <> '' then
          WrapText(Canvas, X+24, MiddleAlignTop+Ofst, X+ObjWidth-2,
              MiddleAlignTop+ObjHeight-1, FAlt);
        RaisedRect(ParentSectionList, Canvas, X, MiddleAlignTop,
                         X+ObjWidth-1, MiddleAlignTop+ObjHeight-1, False);
        end;
      ABottom, ABaseline:
        begin
        if FAlt <> '' then
          WrapText(Canvas, X+24, YBaseLine-ObjHeight+Ofst-VSpaceB, X+ObjWidth-2,
                   YBaseLine-VSpaceB-1, FAlt);
        RaisedRect(ParentSectionList, Canvas, X, YBaseLine-ObjHeight-VSpaceB,
                                      X+ObjWidth-1, YBaseLine-VSpaceB-1, False);
        end;
      end;
    end;
  if HasBlueBox then
    begin
    Pen.Color := FO.TheFont.Color;
    Font.Color := Pen.Color;
    if (FAlt <> '') and SubstImage then  {output Alt message}
      case ObjAlign of
        ALeft, ARight, ATop:
          WrapText(Canvas, X+24, TopY+Ofst+VSpaceT, X+ObjWidth-2, TopY+ObjHeight+VSpaceT-1, FAlt);
        AMiddle:
          WrapText(Canvas, X+24, MiddleAlignTop+Ofst, X+ObjWidth-2,
              MiddleAlignTop+ObjHeight-1, FAlt);
        ABottom, ABaseline:
          WrapText(Canvas, X+24, YBaseLine-ObjHeight+Ofst-VSpaceB, X+ObjWidth-2,
                   YBaseLine-VSpaceB-1, FAlt);
        end;
    case ObjAlign of   {draw blue box}
      ALeft, ARight, ATop: Rectangle(X, TopY+VSpaceT, X+ImageWidth, TopY+VSpaceT+ImageHeight);
      AMiddle: Rectangle(X, MiddleAlignTop, X+ImageWidth, MiddleAlignTop + ImageHeight);
      ABottom, ABaseline: Rectangle(X, YBaseLine-ImageHeight-VSpaceB, X+ImageWidth, YBaseLine-VSpaceB);
      end;
    end;
  if Assigned(MyFormControl) and MyFormControl.Active or FO.Active then    
    begin
    Canvas.Brush.Color := clWhite;
    case ObjAlign of   {draw focus box}
      ALeft, ARight, ATop:
          Canvas.DrawFocusRect(Rect(X-2, TopY+VSpaceT-2, X+ImageWidth+2, TopY+VSpaceT+ImageHeight+2));
      AMiddle:
          Canvas.DrawFocusRect(Rect(X-2, MiddleAlignTop-2, X+ImageWidth+2, MiddleAlignTop + ImageHeight+2));
      ABottom, ABaseline:
          Canvas.DrawFocusRect(Rect(X-2, YBaseLine-ImageHeight-VSpaceB-2, X+ImageWidth+2, YBaseLine-VSpaceB+2));
      end;
    end;
  end;
end;

{----------------TImageObjList.CreateCopy}
constructor TImageObjList.CreateCopy(AMasterList: TSectionList; T: TImageObjList);
var
  I: integer;
  Item: TObject;  
begin
inherited create;
for I := 0 to T.Count-1 do
  begin
  Item := T.Items[I];
  if Item is TImageObj then
    Add(TImageObj.CreateCopy(AMasterList, TImageObj(Item)))
  else Add(TPanelObj.CreateCopy(AMasterList, TPanelObj(Item)));
  end;
end;

function TImageObjList.FindImage(Posn: integer): TFloatingObj;  
{find the image at a given character position}
var
  I: integer;
begin
for I := 0 to Count-1 do
  if TFloatingObj(Items[I]).Pos = Posn then  
    begin
    Result := Items[I];
    Exit;
    end;
Result := Nil;
end;

function TImageObjList.GetHeightAt(Posn: integer; var AAlign: AlignmentType;
         var FlObj: TFloatingObj) : Integer;
begin
FLObj := FindImage(Posn);
if Assigned(FLObj) then
  begin
  Result := FLObj.ImageHeight+FLObj.VSpaceT+FLObj.VSpaceB;
  AAlign := FLObj.ObjAlign;
  end
else Result := -1;
end;

function TImageObjList.GetWidthAt(Posn: integer; var AAlign: AlignmentType;
                var HSpcL, HSpcR: integer; var FlObj: TFloatingObj) : integer;
begin
FLObj := FindImage(Posn);
if Assigned(FLObj) then
  begin
  Result := FLObj.ImageWidth;
  AAlign := FLObj.ObjAlign;
  HSpcL := FLObj.HSpaceL;    
  HSpcR := FLObj.HSpaceR;    
  end
else Result := -1;
end;

function TImageObjList.GetImageCountAt(Posn: integer): integer;
{Return count of chars before the next image.  0 if at the image, 9999 if no
 images after Posn}
var
  I, Pos: integer;
begin
if Count = 0 then
  begin
  Result := 9999;
  Exit;
  end;
I := 0;
while I < count do
  begin
  Pos := TFloatingObj(Items[I]).Pos;
  if Pos >= Posn then break;
  Inc(I);
  end;
if I = Count then Result := 9999
else
  Result := TFloatingObj(Items[I]).Pos - Posn;
end;

{----------------TImageObjList.Decrement}
procedure TImageObjList.Decrement(N: integer);
{called when a character is removed to change the Position figure}
var
  I: integer;
begin
for I := 0 to Count-1 do
  with TImageObj(Items[I]) do
    if Pos > N then
      Dec(Pos);
end;

{----------------TImageObjList.PtInImage}
function TImageObjList.PtInImage(X: integer; Y: integer; var IX, IY, Posn: integer;
                var AMap, UMap: boolean; var MapItem: TMapItem;
                var ImageObj: TImageObj): boolean;
var
  I, J, LimX, LimY: integer;
  LIY: integer;
  Obj: TObject;   
begin
Result := False;
for I := 0 to Count-1 do
  begin
  Obj := Items[I];   
  if Obj is TImageObj then
    with TImageObj(Obj) do
      begin
      IX := X-DrawXX;    {these are actual image, box if any is outside}
      LIY := Y - DrawYY;
      if HasBlueBox then begin LimX := ImageWidth-2; Limy := ImageHeight-2; end
        else begin LimX := ImageWidth; Limy := ImageHeight; end;
      if (IX >= 0) and (IX < LimX) and (LIY >= 0) and (LIY < LimY) then
        begin
        IY := LIY;
        Result := True;
        AMap := IsMap;
        Posn := Pos;
        UMap := False;
        ImageObj := TImageObj(Obj);   
        if UseMap then
          with ParentSectionList.MapList do
            for J := 0 to Count-1 do
              begin
              MapItem := Items[J];
              if MapItem.MapName = MapName then
                begin
                UMap := True;
                Exit;
                end;
              end;
        Exit;
        end;
      end;
  end;
end;

function TImageObjList.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
var
  I, LimX, LimY: integer;
  LIY: integer;
  Item: TObject;   
begin
Result := False;
for I := 0 to Count-1 do
  begin
  Item := Items[I]; 
  if Item is TImageObj then
    with TImageObj(Item) do
      begin
      IX := X-DrawXX;    {these are actual image, box if any is outside}
      LIY := Y - DrawYY;
      if HasBlueBox then begin LimX := ImageWidth-2; Limy := ImageHeight-2; end
        else begin LimX := ImageWidth; Limy := ImageHeight; end;
      if (IX >= 0) and (IX < LimX) and (LIY >= 0) and (LIY < LimY) then
        begin
        IY := LIY;
        Result := True;
        Obj := Item;  
        Exit;
        end;
      end;
  end;
end;

{----------------ThtmlForm.Create}
constructor ThtmlForm.Create(AMasterList: TSectionList; L : TAttributeList);
var
  I: integer;
begin
inherited Create;
MasterList := AMasterList;
AMasterList.htmlFormList.Add(Self);
Method := 'Get';
if Assigned(L) then
  for I := 0 to L.Count-1 do
    with TAttribute(L[I]) do
      case Which of
        MethodSy: Method := Name;
        ActionSy: Action := Name;
        TargetSy: Target := Name;
        EncTypeSy: EncType := Name;
        end;
ControlList := TFreeList.Create;
end;

destructor ThtmlForm.Destroy;
begin
ControlList.Free;
inherited Destroy;
end;

procedure ThtmlForm.InsertControl(Ctrl: TFormControlObj);
begin
ControlList.Add(Ctrl);
if not (Ctrl is THiddenFormControlObj) then Inc(NonHiddenCount);
end;

procedure ThtmlForm.DoRadios(Radio: TRadioButtonFormControlObj);
var
  S: string;
  Ctrl: TFormControlObj;
  I: integer;
begin
if Radio.FName <>'' then
  begin
  S := Radio.FName;
  for I := 0 to ControlList.Count-1 do
    begin
    Ctrl := TFormControlObj(ControlList.Items[I]);
    if  (Ctrl is TRadioButtonFormControlObj) and (Ctrl <> Radio) then
      if CompareText(Ctrl.FName, S) = 0 then
        begin
        TRadioButtonFormControlObj(Ctrl).RButton.Checked := False;
        TRadioButtonFormControlObj(Ctrl).DoOnchange;   
        end;
    end;
  end;
end;

procedure ThtmlForm.AKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);  
var
  S: string;
  Ctrl: TFormControlObj;
  I: integer;
  List: TList;
begin
if (Key in [vk_up, vk_down, vk_left, vk_right]) and (Sender is TFormRadioButton) then
  begin
  S := TFormRadioButton(Sender).IDName;
  List:= TList.Create;
  try
    for I := 0 to ControlList.Count-1 do
      begin
      Ctrl := TFormControlObj(ControlList.Items[I]);
      if  (Ctrl is TRadioButtonFormControlObj) and
                (CompareText(Ctrl.FName, S) = 0) then
          List.Add(TRadioButtonFormControlObj(Ctrl).RButton);
      end;
    I := List.IndexOf(Sender);
    if I >= 0 then
      begin
      if (Key in [vk_up, vk_left]) then
        begin
        if I > 0 then
          Dec(I);
        end
      else if I < List.Count-1 then
        Inc(I);
      TFormRadioButton(List.Items[I]).SetFocus;
      end;
  finally
    List.Free;
    end;
  end
else  {send other keys to ThtmlViewer}
  ThtmlViewer(MasterList.TheOwner).KeyDown(Key, Shift);
end;

procedure ThtmlForm.ResetControls;
var
  I: integer;
begin
for I := 0 to ControlList.Count-1 do
  TFormControlObj(ControlList.Items[I]).ResetToValue;
end;

procedure ThtmlForm.ControlKeyPress(Sender: TObject; var Key: char);
begin
if (Sender is TEdit) then
  if (Key = #13) then
    begin
    SubmitTheForm('');
    Key := #0;
    end;
end;

function ThtmlForm.GetFormSubmission: TStringList;
var                                                    
  I, J: integer;
  S: string;
begin
Result := TStringList.Create;
for I := 0 to ControlList.Count-1 do
  with  TFormControlObj(ControlList.Items[I]) do
    begin
    J := 0;
    while GetSubmission(J, S) do
      begin
      if S <> '' then
        Result.Add(S);
      Inc(J);
      end;
    end;
end;

procedure ThtmlForm.SubmitTheForm(const ButtonSubmission: string);
var
  I, J: integer;
  SL: TStringList;
  S: string;                              
begin
if Assigned(MasterList.SubmitForm) then
  begin
  SL := TStringList.Create;
  for I := 0 to ControlList.Count-1 do
    with  TFormControlObj(ControlList.Items[I]) do
      begin
      J := 0;
      while GetSubmission(J, S) do
        begin
        if S <> '' then
          SL.Add(S);
        Inc(J);
        end;
      end;
  if ButtonSubmission <> '' then
    SL.Add(ButtonSubmission);
  MasterList.SubmitForm(MasterList.TheOwner, Action, Target, EncType, Method, SL);
  end;
end;

procedure ThtmlForm.SetFormData(SL: TStringList);    
var
  I, J, K, Index: integer;
  Value: string;
  FormControl: TFormControlObj;
begin
for I := 0 to ControlList.Count-1 do
  begin
  FormControl := TFormControlObj(ControlList[I]);
  FormControl.SetDataInit;
  Index := 0;
  for J := 0 to SL.Count-1 do
    if CompareText(FormControl.FName, SL.Names[J]) = 0 then
      begin
      K := Pos('=', SL[J]);
      if K > 0 then
        begin
        Value := Copy(SL[J], K+1, Length(SL[J])-K);
        FormControl.SetData(Index, Value);
        Inc(Index);
        end;
      end;
  end;
end;

procedure ThtmlForm.SetSizes(Canvas: TCanvas);
var
  I: integer;
begin
for I := 0 to ControlList.Count-1 do
  TFormControlObj(ControlList.Items[I]).SetHeightWidth(Canvas);
end;

{----------------TFormControlObj.Create}
constructor TFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
var
  I: integer;
begin
inherited Create;
Pos := Position;
MasterList := AMasterList;
if not Assigned(CurrentForm) then   {maybe someone forgot the <form> tag}
  CurrentForm := ThtmlForm.Create(AMasterList, Nil);
AMasterList.FormControlList.Add(Self);
MyForm := CurrentForm;
for I := 0 to L.Count-1 do
  with TAttribute(L[I]) do
    case Which of
      ValueSy: Self.Value := Name;
      NameSy: Self.FName := Name;
      IDSy: FID := Name;
      OnClickSy: OnClickMessage := Name;
      OnFocusSy: OnFocusMessage := Name;  
      OnBlurSy: OnBlurMessage := Name;
      OnChangeSy: OnChangeMessage := Name;
      TabIndexSy:
          if Value > 0 then
            {Adding leading 0's to the number string allows it to be sorted numerically,
             and the Count takes care of duplicates}
            with AMasterList.TabOrderList do
              AddObject(Format('%.5d%.3d', [Value, Count]), Self);  
      end;

if L.TheID <> '' then
  MasterList.IDNameList.AddObject(L.TheID, Self);
FormAlign := ABottom;     {ABaseline set individually}
MyForm.InsertControl(Self);
end;

constructor TFormControlObj.CreateCopy(T: TFormControlObj);
begin
inherited Create;
System.Move(T.Pos, Pos, DWord(@FControl)-DWord(@Pos));
end;

destructor TFormControlObj.Destroy;
begin
if Assigned(FControl) then {hidden controls are Nil}
  begin
  TPaintPanel(MasterList.PPanel).RemoveControl(FControl);
  FControl.Free;
  end;
inherited Destroy;
end;

function TFormControlObj.GetYPosition: integer;
begin
Result := YValue;
end;

procedure TFormControlObj.ProcessProperties(Prop: TProperties);
var
  MargArrayO: TVMarginArray;
  MargArray: TMarginArray;
  Align: AlignmentType;
  EmSize, ExSize: integer;
begin
Prop.GetVMarginArray(MargArrayO);
EmSize := Prop.EmSize;
ExSize := Prop.ExSize;
ConvInlineMargArray(MargArrayO, 200, 200, EmSize, ExSize, MargArray);

if MargArray[MarginLeft] <> IntNull then
  HSpaceL := MargArray[MarginLeft];
if MargArray[MarginRight] <> IntNull then
  HSpaceR := MargArray[MarginRight];
{Vertical margins not implemented yet}
if MargArray[Width] > 0 then     {excludes IntNull and Auto}
    FWidth := MargArray[Width];
if MargArray[Height] > 0 then
    FHeight:= MargArray[Height];
if Prop.GetVertAlign(Align) then
  FormAlign := Align;
end;

procedure TFormControlObj.EnterEvent(Sender: TObject);
{Once form control entered, insure all form controls are tab active}
{$ifndef FastRadio}
var
  I: integer;
{$endif}
begin
if MasterList.IsCopy then Exit;
Active := True;
MasterList.PPanel.Invalidate;
MasterList.ControlEnterEvent(Self);
{$ifndef FastRadio}
with MasterList.FormControlList do
  begin
  for I := 0 to Count-1 do
    with  TFormControlObj(Items[I]) do
      if not ShowIt and Assigned(FControl) then
        begin
        FControl.Show;   {makes it tab active}
        FControl.Left := -4000; {even if it can't be seen}
        end;
  end;
{$endif}
if Assigned(MasterList.ObjectFocus) and (OnFocusMessage <> '') then
  MasterList.ObjectFocus(MasterList.TheOwner, Self, OnFocusMessage);
if OnChangeMessage <> '' then
  SaveContents;
end;

procedure TFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
end;

procedure TFormControlObj.ExitEvent(Sender: TObject);
begin
{$ifndef FastRadio}
MasterList.AdjustFormControls;
{$endif}
Active := False;
if OnChangeMessage <> '' then
  DoOnChange;
if Assigned(MasterList.ObjectBlur) and (OnBlurMessage <> '') then
  MasterList.ObjectBlur(MasterList.TheOwner, Self, OnBlurMessage);
MasterList.PPanel.Invalidate;
end;

procedure TFormControlObj.DoOnChange;
begin
end;

function TFormControlObj.GetControl: TWinControl;
begin
Result := FControl;
end;

procedure TFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
begin end;

procedure TFormControlObj.ResetToValue;
begin end;

function TFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
Result := False;
end;

procedure TFormControlObj.SetDataInit;
begin
end;

procedure TFormControlObj.SetData(Index: integer; const V: String);
begin
end;

procedure TFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
end;

procedure TFormControlObj.FormControlClick(Sender: TObject);  
begin
if Assigned(MasterList.ObjectClick) then
  MasterList.ObjectClick(MasterList.TheOwner, Self, OnClickMessage);
end;

constructor TImageFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
var
  PntPanel: TPaintPanel;
begin
inherited Create(AMasterList, Position, L);
XPos := -1;   {so a button press won't submit image data}

PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TButton.Create(PntPanel);
with TButton(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Width := 1;
  Height := 1;
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnClick := ImageClick;
  end;
FControl.Parent := PntPanel;
end;

procedure TImageFormControlObj.ProcessProperties(Prop: TProperties);
begin
inherited ;
if FHeight > 0 then
  MyImage.SpecHeight := FHeight;
if FWidth > 0 then
  MyImage.SpecWidth := FWidth;
MyImage.ObjAlign := FormAlign;
end;

procedure TImageFormControlObj.ImageClick(Sender: TObject); 
begin
if FControl.CanFocus then
  FControl.SetFocus;     
FormControlClick(Self);   
XPos := XTmp; YPos := YTmp;
MyForm.SubmitTheForm('');
end;

function TImageFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
Result := False;
if (Index <= 1) and (XPos >= 0) then
  begin
  S := '';
  if FName <> '' then S := FName+'.';
  if Index = 0 then S := S+'x='+IntToStr(XPos)
  else
    begin  {index = 1}
    S := S+'y='+IntToStr(YPos);
    XPos := -1;
    end;
  Result := True;
  end;
end;

{----------------THiddenFormControlObj.GetSubmission}
function THiddenFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
Result := Index = 0;
if Result then
  S := FName+'='+Value;
end;

procedure THiddenFormControlObj.SetData(Index: integer; const V: String);      
begin
Value := V;
end;

{----------------TEditFormControlObj.Create}
constructor TEditFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; const Typ: string; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TPaintPanel;
  I: integer;
  Tmp: TMyFont;
begin
inherited Create(AMasterList, Position, L);
EditSize := 15;     
if L.Find(SizeSy, T) then
  begin
  if T.Value > 0 then EditSize := T.Value
  else
    begin    {see if it's comma delimited list}
    I := IntMin(System.Pos(',', T.Name), System.Pos(' ', T.Name));
    if I > 1 then EditSize := StrToIntDef(copy(T.Name, 1, I-1), 20);
    end;
  end;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TEdit.Create(PntPanel);
with TEdit(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Width := 120;
  Height := 20;
  Text := Value;
  Tmp := Prop.GetFont;
  Font.Assign(Tmp);
  Tmp.Free;
  if L.Find(MaxLengthSy, T) then
    MaxLength := T.Value;
  if Typ = 'password' then
    PassWordChar := '*';
  OnKeyPress := MyForm.ControlKeyPress;   
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  OnClick := FormControlClick;
  end;
FControl.Parent := PntPanel;
end;

procedure TEditFormControlObj.ResetToValue;
begin
TEdit(FControl).Text := Value;
end;

procedure TEditFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  H2: integer;
begin
with TEdit(FControl) do
  begin
  Canvas.Font := Font;
  H2 := Abs(Font.Height);
  FormControlRect(Canvas, X1, Y1, X1+Width, Y1+Height, False, MasterList.PrintMonoBlack); 
  SetTextAlign(Canvas.handle, TA_Left);
  SetBkMode(Canvas.Handle, Transparent);
  Canvas.Brush.Style := bsClear; 
  Canvas.TextRect(Rect(X1+4, Y1, X1+Width-8, Y1+Height), X1+4,
             Y1+(Height-H2)div 2, Text);
  end;
end;

function TEditFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
if Index = 0 then
  begin
  Result := True;
  S := FName+'='+TEdit(FControl).Text;
  end
else Result := False;
end;

procedure TEditFormControlObj.SetData(Index: integer; const V: String);      
begin
TEdit(FControl).Text := V;
end;

procedure TEditFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
with TEdit(FControl) do
  begin
  Canvas.Font := Font;
  if FWidth >= 10 then
    Width := FWidth
  else Width := Canvas.TextWidth('0')*EditSize+5;
  Height := IntMax(Height, FHeight);
  end;
end;

procedure TEditFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
EnterContents := TEdit(FControl).Text;
end;

procedure TEditFormControlObj.DoOnChange;
begin
if TEdit(FControl).Text <> EnterContents then
  if Assigned(MasterList.ObjectChange) then
    MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TButtonFormControlObj.Create}
constructor TButtonFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; const Typ: string;
            Prop: TProperties);
var
  PntPanel: TPaintPanel;
  Tmp: TMyFont;
begin
inherited Create(AMasterList, Position, L);
if Typ = 'submit' then
  begin
  Which := Submit;
  if Value = '' then
    Value := 'Submit';
  end
else if Typ = 'reset' then
  begin
  Which := ResetB;
  if Value = '' then
    Value := 'Reset';
  end
else
  begin
  Which := Button;
  if Value = '' then
    Value := 'Button';
  end;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TButton.Create(PntPanel);
with TButton(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Tmp := Prop.GetFont;  
  Font.Assign(Tmp);
  Tmp.Free;
  OnClick := ButtonClick;
  Caption := Value;
  OnEnter := EnterEvent;    
  OnExit := ExitEvent;    
  end;
FControl.Parent := PntPanel;
end;

procedure TButtonFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  H2: integer;
begin
with TButton(FControl) do
  begin
  Canvas.Brush.Style := bsClear;
  Canvas.Font := Font;
  FormControlRect(Canvas, X1, Y1, X1+Width, Y1+Height, True, MasterList.PrintMonoBlack);
  H2 := Canvas.TextHeight('A');
  SetTextAlign(Canvas.handle, TA_Center+TA_Top);
  Canvas.TextRect(Rect(X1, Y1, X1+Width, Y1+Height), X1+(Width div 2),
             Y1+(Height-H2)div 2, Value);
  end;
end;

procedure TButtonFormControlObj.ButtonClick(Sender: TObject);
var
  S: string;
begin
FormControlClick(Self);
if Which = ResetB then
  MyForm.ResetControls
else if Which = Submit then
  if FName = '' then
    MyForm.SubmitTheForm('')
  else
    begin
    S := FName;
    MyForm.SubmitTheForm(S+'='+Value);
    end;       
end;

procedure TButtonFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
with TButton(FControl) do
  begin
  Canvas.Font := Font;
  if FHeight >= Canvas.TextHeight('A') then
    Height := FHeight
  else Height := Canvas.TextHeight('A')+8;
  if FWidth >= 10 then
    Width := FWidth
  else Width := Canvas.TextWidth(Caption)+20;
  end;
end;

{----------------TCheckBoxFormControlObj.Create}
constructor TCheckBoxFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList);
var
  T: TAttribute;
  PntPanel: TPaintPanel;
begin
inherited Create(AMasterList, Position, L);
if Value = '' then Value := 'on';
FormAlign := ABaseline;
if L.Find(CheckedSy, T) then IsChecked := True;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TFormCheckBox.Create(PntPanel);    
with TFormCheckBox(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  Width := 13;
  Height := 13;
  Checked := IsChecked;
  OnClick := FormControlClick;
  OnKeyDown := MyForm.AKeyDown;  
  OnEnter := EnterEvent;
  OnExit := ExitEvent;
  end;
FControl.Parent := PntPanel;
end;

procedure TCheckBoxFormControlObj.ResetToValue;
begin
TCheckBox(FControl).Checked := IsChecked;
end;

procedure TCheckBoxFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  x, y: integer;
begin
with TCheckBox(FControl) do
  begin
  FormControlRect(Canvas, X1, Y1, X1+Width, Y1+Height, False, MasterList.PrintMonoBlack);
  if Checked then
    with Canvas do
      begin
      Pen.Color := clBlack;
      x := X1+3; y := Y1+Height div 2;
      MoveTo(x, y);
      LineTo(x+2, y+2);
      LineTo(x+6, y-2);
      end;
  end;
end;

function TCheckBoxFormControlObj.GetSubmission(Index: integer; var S: string): boolean;
begin
if (Index = 0) and TCheckBox(FControl).Checked then
  begin
  Result := True;
  S := FName+'='+Value;
  end
else Result := False;
end;

procedure TCheckBoxFormControlObj.SetDataInit;   
begin
TCheckBox(FControl).Checked := False;  {not checked unless later data says so}
end;

procedure TCheckBoxFormControlObj.SetData(Index: integer; const V: String);
begin
if CompareText(V, Value) = 0 then
  TCheckBox(FControl).Checked := True;
end;

procedure TCheckBoxFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
WasChecked := TCheckBox(FControl).Checked;
end;

procedure TCheckBoxFormControlObj.DoOnChange;
begin
if TCheckBox(FControl).Checked <> WasChecked then
  if Assigned(MasterList.ObjectChange) then
    MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TRadioButtonFormControlObj.Create}
constructor TRadioButtonFormControlObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; ACell: TCellBasic);
var
  T: TAttribute;
  PntPanel: TPaintPanel;
begin
inherited Create(AMasterList, Position, L);
MyCell := ACell;
PntPanel := TPaintPanel(AMasterList.PPanel);
FControl := TPanel.Create(PntPanel);
FormAlign := ABaseline;
if L.Find(CheckedSy, T) then IsChecked := True;
{Use a TPanel to isolate RadioButton action}
with TPanel(FControl) do
  begin
  Top := -400;   {so will be invisible until placed}
  if Screen.PixelsPerInch > 100 then   
    begin
    Width := 16;
    Height := 16;
    end
  else
    begin
    Width := 13;
    Height := 14;
    end;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  end;
RButton := TFormRadioButton.Create(FControl);  
TFormRadioButton(RButton).IDName := Self.FName;
RButton.Checked := IsChecked;
RButton.Parent := FControl;
RButton.OnClick := RadioClick;
RButton.OnEnter := EnterEvent;    
RButton.OnExit := ExitEvent;
RButton.OnKeyDown := MyForm.AKeyDown;
if Screen.PixelsPerInch <= 100 then   
  RButton.Top := -1;
FControl.Parent := PntPanel;
end;

function TRadioButtonFormControlObj.GetControl: TWinControl;
begin
Result := RButton;
end;

procedure TRadioButtonFormControlObj.RadioClick(Sender: TObject);
begin
MyForm.DoRadios(Self);
FormControlClick(Self);    
end;

procedure TRadioButtonFormControlObj.ResetToValue;
begin
RButton.Checked := IsChecked;
end;

procedure TRadioButtonFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  OldStyle: TPenStyle;
  OldWidth, XW, YH, XC, YC: integer;
  OldColor, OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  MonoBlack: boolean;    
begin
with Canvas do
  begin
  XW := X1+14;
  YH := Y1+14;
  OldStyle := Pen.Style;
  OldWidth := Pen.Width;
  OldBrushStyle := Brush.Style;
  OldBrushColor := Brush.Color;
  Brush.Color := clWhite;
  Pen.Color := clWhite;
  Ellipse(X1, Y1, XW, YH);

  MonoBlack := MasterList.PrintMonoBlack and (GetDeviceCaps(Handle, BITSPIXEL) = 1) and  
          (GetDeviceCaps(Handle, PLANES) = 1);
  Pen.Style := psInsideFrame;
  if MonoBlack then
    begin
    Pen.Width := 1;
    Pen.Color := clBlack;
    end
  else
    begin
    Pen.Width := 2;
    Pen.Color := clBtnShadow;
    end;
  Arc(X1, Y1, XW, YH, XW, Y1, X1, YH);
  if not MonoBlack then
    Pen.Color := clSilver;
  Arc(X1, Y1, XW, YH, X1, YH, XW, Y1);
  if RButton.Checked then
    begin
    Pen.Color := clBlack;
    OldColor := Brush.Color;
    Brush.Color := clBlack;
    Brush.Style := bsSolid;
    XC := X1+7;
    YC := Y1+7;
    Ellipse(XC-2, YC-2, XC+2, YC+2);
    Brush.Color := OldColor;
    end;
  Pen.Width := OldWidth;
  Pen.Style := OldStyle;
  Brush.Color := OldBrushColor;
  Brush.Style := OldBrushStyle;
  end;
end;

function TRadioButtonFormControlObj.GetSubmission(Index: integer;
             var S: string): boolean;
begin
if (Index = 0) and RButton.Checked then
  begin
  Result := True;
  S := FName+'='+Value;
  end
else Result := False;
end;

procedure TRadioButtonFormControlObj.SetData(Index: integer; const V: String);
begin
if CompareText(V, Value) = 0 then
  RButton.Checked := True;
end;

procedure TRadioButtonFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
WasChecked := RButton.Checked;
end;

procedure TRadioButtonFormControlObj.DoOnChange;
begin
if RButton.Checked <> WasChecked then
  if Assigned(MasterList.ObjectChange) then
    MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{----------------TCellBasic.Create}
constructor TCellBasic.Create(Master: TSectionList);
begin
inherited Create;
MasterList := Master;
end;

{----------------TCellBasic.CreateCopy}
constructor TCellBasic.CreateCopy(AMasterList: TSectionList; T: TCellBasic);
var
  I: integer;
  Tmp, Tmp1: TSectionBase;
begin
inherited Create;
MasterList := AMasterList;
OwnersTag := T.OwnersTag;
for I := 0 to T.Count-1 do
  begin
  Tmp := T.Items[I];
  Tmp1 := TSectionClass(Tmp.ClassType).CreateCopy(AMasterList, Tmp);
  Add(Tmp1);
  end;
end;

{----------------TCellBasic.Add}
procedure TCellBasic.Add(Item: TSectionBase);
begin
if Assigned(Item) then
  begin
  if (Item is TSection) and Assigned(TSection(Item).XP) then   {XP not assigned if printing}
    begin
    TSection(Item).ProcessText;
    if not (Item is TPreFormated) and (TSection(Item).Len = 0)
         and not TSection(Item).AnchorName and (TSection(Item).ClearAttr = clrNone) then
      begin
      TSection(Item).CheckFree;
      Item.Free;  {discard empty TSections that aren't anchors}
      Exit;
      end;
    end;
  inherited Add(Item);
  Item.SetParent(MasterList);
  end;
end;

function TCellBasic.CheckLastBottomMargin: boolean;   
{Look at the last item in this cell.  If its bottom margin was set to Auto,
 set it to 0}
var
  TB: TObject;
  I: integer;
  Done: boolean;
begin
Result := False;  
I := Count-1;  {find the preceding block that isn't absolute positioning}
Done := False;
while (I >= 0) and not Done do
  begin
  TB := Items[I];
  if (TB is TBlock) and (TBlock(TB).Positioning <> PosAbsolute) then
    Done := True
  else Dec(I);
  end;
if I >= 0 then
  begin
  TB := Items[I];
  if (TB is TBlock) then
    with TBlock(TB) do
      if BottomAuto then
        begin
        MargArray[MarginBottom] := 0;
        Result := True;   
        end;
  if (TB is TBlockLI) then
    Result := TBlockLI(TB).MyCell.CheckLastBottomMargin;  
  end;
end;

{----------------TCellBasic.GetURL}
function TCellBasic.GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType;
{Y is absolute}
var
  I: integer;
  SB: TSectionBase;
begin
Result := [];
FormControl := Nil;
UrlTarg := Nil;
for I := 0 to Count-1 do
  begin
  SB := TSectionBase(Items[I]);
  with SB do
    begin
    if (Y >= DrawTop) and (Y < DrawBot) then
        begin
        Result := GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
        if Result <> [] then
          Exit;     
        end;
    end;
  end;
end;

{----------------TCellBasic.PtInObject}
function TCellBasic.PtInObject(X: integer; Y: integer;  var Obj: TObject;  
              var IX, IY: integer): boolean;
{Y is absolute}
var
  I: integer;
begin
Result := False;
Obj := Nil;
for I := 0 to Count-1 do
  with TSectionBase(Items[I]) do
    begin
    if (Y >= DrawTop) and (Y < DrawBot) then
        begin
        Result := PtInObject(X, Y, Obj, IX, IY);
        if Result then
          Exit;
        end;
    end;
end;

{----------------TCellBasic.FindCursor}
function TCellBasic.FindCursor(Canvas: TCanvas; X: Integer; Y: integer;
                var XR: integer; var YR: integer; var Ht: integer;
                var Intext: boolean): integer;
var
  I: integer;
  SB: TSectionBase;
begin
Result := -1;
for I := 0 to Count-1 do
  begin
  SB := TSectionBase(Items[I]);
  with SB do
    begin
    if (Y >= DrawTop) and (Y < DrawBot) then
      Result := TSectionBase(Items[I]).FindCursor(Canvas, X, Y, XR, YR, Ht, InText);
    if Result >= 0 then
      Break;
    end;
  end;
end;

procedure TCellBasic.AddSectionsToList;
var
  I: integer;
begin
for I := 0 to Count-1 do
  TSectionBase(Items[I]).AddSectionsToList;
end;

{----------------TCellBasic.FindString}
function TCellBasic.FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
var
  I: integer;
begin
Result := -1;
for I := 0 to Count-1 do
  begin
  Result := TSectionBase(Items[I]).FindString(From, ToFind, MatchCase);
  if Result >= 0 then
    Break;
  end;
end;

{----------------TCellBasic.FindStringR}
function TCellBasic.FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
var
  I: integer;
begin
Result := -1;
for I := Count-1 downto 0 do
  begin
  Result := TSectionBase(Items[I]).FindStringR(From, ToFind, MatchCase);
  if Result >= 0 then
    Break;
  end;
end;

{----------------TCellBasic.FindSourcePos}
function TCellBasic.FindSourcePos(DocPos: integer): integer;
var
  I: integer;
begin
Result := -1;
for I := 0 to Count-1 do
  begin
  Result := TSectionBase(Items[I]).FindSourcePos(DocPos);
  if Result >= 0 then
    Break;
  end;
end;

procedure TCellBasic.FormTree(Indent: string; var Tree: string);
var
  I: integer;
  Item: TSectionBase;
begin
for I := 0 to Count-1 do
  begin
  Item := Items[I];
  if Item is TBlock then
    TBlock(Item).FormTree(Indent, Tree)
  else if Item is TSection then
    Tree := Tree + Indent + Copy(TSection(Item).BuffS, 1, 10)+^M^J
  else
    Tree := Tree + Indent + '----'^M^J;
  end;
end;

{----------------TCellBasic.FindDocPos}
function TCellBasic.FindDocPos(SourcePos: integer; Prev: boolean): integer;
var
  I: integer;
begin
Result := -1;
if not Prev then
  for I := 0 to Count-1 do
    begin
    Result := TSectionBase(Items[I]).FindDocPos(SourcePos, Prev);
    if Result >= 0 then
      Break;
    end
else  {Prev, iterate backwards}
  for I := Count-1 downto 0 do
    begin
    Result := TSectionBase(Items[I]).FindDocPos(SourcePos, Prev);
    if Result >= 0 then
      Break;
    end
end;

{----------------TCellBasic.CursorToXY}
function TCellBasic.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
var
  I: integer;
begin
Result := False;
for I := 0 to Count-1 do
  begin
  Result := TSectionBase(Items[I]).CursorToXY(Canvas, Cursor, X, Y);
  if Result then Break;
  end;
end;

{----------------TCellBasic.GetChAtPos}   

function TCellBasic.GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
var
  I: integer;
begin
Result := False;
if (Pos >= StartCurs) and (Pos <= StartCurs+Len) then
  for I := 0 to Count-1 do
    begin
    Result := TSectionBase(Items[I]).GetChAtPos(Pos, Ch, Obj);
    if Result then Break;
    end;
end;

{----------------TCellBasic.CopyToClipboard}
procedure TCellBasic.CopyToClipboard;
var
  I: integer;                    
  SLE, SLB: integer;
begin
if not Assigned(MasterList) then Exit;  {dummy cell}
SLB := MasterList.SelB;
SLE := MasterList.SelE;
if SLE <= SLB then Exit;   {nothing to do}

for I := 0 to Count-1 do
  with TSectionBase(Items[I]) do
    begin
    if (SLB >= StartCurs + Len) then Continue;
    if (SLE <= StartCurs) then Break;
    CopyToClipboard;
    end;
end;

{----------------TCellBasic.DoLogic}
function TCellBasic.DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight: integer;
                 var ScrollWidth: integer; var Curs: integer): integer;
{Do the entire layout of the cell or document.  Return the total document
 pixel height}
var
  I, Sw, TheCount: integer;
  H, Tmp: integer;
  SB: TSectionBase;
begin
  YValue := Y;
  StartCurs := Curs;
  H := 0;
  ScrollWidth := 0;
  TheCount := Count;
  I := 0;
  while I < TheCount do
    begin
    try
      SB :=  TSectionBase(Items[I]);
      Tmp := SB.DrawLogic(Canvas, 0, Y+H, 1000, 1000, Width, AHeight, IMgr, Sw, Curs);
      H := H+Tmp;
      ScrollWidth := IntMax(ScrollWidth, Sw);
      Inc(I);
    except
      on E:EProcessError do
        begin
        MessageDlg(E.Message, mtError, [mbOK], 0);
        TSectionBase(Items[I]).Free;
        Delete(I);
        Dec(TheCount);
        end;
      end;
    end;
  Len := Curs - StartCurs;
  Result := H;
 end;

{----------------TCellBasic.MinMaxWidth}
procedure TCellBasic.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
{Find the Width the cell would take if no wordwrap, Max, and the width if wrapped
 at largest word, Min}
var
  I, Mn, Mx: integer;
begin
Max := 0; Min := 0;
for I := 0 to Count-1 do
  begin
  TSectionBase(Items[I]).MinMaxWidth(Canvas, Mn, Mx);
  Max := IntMax(Max, Mx);
  Min := IntMax(Min, Mn);
  end;
end;

{----------------TCellBasic.Draw}
function TCellBasic.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                            Y, XRef, YRef : integer): integer;
{draw the document or cell.  Note: individual sections not in ARect don't bother
 drawing}
var
  I: integer;
  H: integer;
begin
  H := Y;
  for I := 0 to Count-1 do
    begin
    H := TSectionBase(Items[I]).Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
    end;
  Result := H;
end;

{----------------TBlock.Create}
constructor TBlock.Create(Master: TSectionList; Prop: TProperties;
                AnOwnerCell: TCellBasic; Attributes: TAttributeList);   
var
  Clr: ClearAttrType;
  S: string;
begin
inherited Create(Master);
OwnerCell := AnOwnerCell;
MyCell := TBlockCell.Create(Master);
MyCell.OwnersTag := Prop.PropTag;
DrawList := TList.Create;

Prop.GetVMarginArray(MargArrayO);
if Prop.GetClear(Clr) then
  ClearAttr := Clr;
if not Prop.GetFloat(FloatLR) then
  FloatLR := ANone;
BorderStyle := Prop.GetBorderStyle;
FGColor := Prop.Props[Color];
EmSize := Prop.EmSize;
ExSize := Prop.ExSize;
DisplayNone := Prop.DisplayNone;
BlockTitle := Prop.PropTitle;     
if not (Self is TBodyBlock) and Prop.GetBackgroundImage(S) and (S <> '') then
  begin  {body handles its own image}
  BGImage := TImageObj.SimpleCreate(Master, S);
  Prop.GetBackgroundPos(EmSize, ExSize, PRec);
  end;

Positioning := Prop.GetPosition;
Visibility := Prop.GetVisibility;
Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);
if Positioning <> posStatic then
  begin
  ZIndex := 10*Prop.GetZIndex;
  if (Positioning = posAbsolute) and (ZIndex = 0) then
    ZIndex := 1;  {abs on top unless otherwise specified}
  end;
if (FloatLR in [ALeft, ARight]) and (ZIndex = 0) then
  ZIndex := 1;
TagClass := Prop.PropTag+'.'+Prop.PropClass;
if not (Self is TTableBlock) then
  CollapseMargins;
if Assigned(Attributes) and (Attributes.TheID <> '') then
  Master.IDNameList.AddObject(Attributes.TheID, Self);   
end;

procedure TBlock.CollapseMargins;
{adjacent vertical margins need to be reduced}
var
  TopAuto, Done: boolean;
  TB: TSectionBase;
  LastMargin, Negs, I: integer;
  Tag: string;
begin
ConvVertMargins(MargArrayO, 400,  {height not known at this point}
   EmSize, ExSize, MargArray, TopAuto, BottomAuto);
if Positioning = posAbsolute then
  begin
  if TopAuto then
    MargArray[MarginTop] := 0;
  end
else if FloatLR in [ALeft, ARight] then  {do nothing}
else
  with OwnerCell do
    begin
    I := Count-1;  {find the preceding block that isn't absolute positioning}
    Done := False;
    while (I >= 0) and not Done do
      begin
      TB := TSectionBase(Items[I]);
      if ((TB is TBlock) and (TBlock(TB).Positioning <> PosAbsolute))
             or not (TB is TBlock) then   {allow for a TSection}
        Done := True
      else Dec(I);
      end;
    Tag := OwnerCell.OwnersTag;   
    if I < 0 then
      begin {no previous non absolute block, remove any Auto paragraph space}
      if TopAuto then
        begin
        if (Tag = 'li')  then
          begin
          MargArray[MarginTop] := 0;
          end
        else if (Tag <> 'default') and (Tag <> 'body') then
          MargArray[MarginTop] := 0;
        end;
      end
    else
      begin
      TB := Items[I];
      if (TB is TTableBlock) and (TTableBlock(TB).FloatLR in [ALeft, ARight])
            and TopAuto then
        MargArray[MarginTop] := 0
      else if (TB is TBlock) then
        begin
        LastMargin := TBlock(TB).MargArray[MarginBottom];
        TBlock(TB).MargArray[MarginBottom] := 0;
        if LastMargin >= 0 then  {figure out how many are negative}
          if MargArray[MarginTop] >=0 then
            Negs := 0
          else Negs := 1
        else
          if MargArray[MarginTop] >=0 then
            Negs := 1
          else Negs := 2;
        case Negs of
          0: MargArray[MarginTop] := IntMax(MargArray[MarginTop], LastMargin);
          1: MargArray[MarginTop] := MargArray[MarginTop] + LastMargin;
          2: MargArray[MarginTop] := IntMin(MargArray[MarginTop], LastMargin);
          end;
        end
      else if (Tag = 'li') and TopAuto
           and ((Pos('ul.', TagClass)=1) or (Pos('ol.', TagClass)=1)) then  
        MargArray[MarginTop] := 0;  {removes space from nested lists}
      end;
    end;
end;

{----------------TBlock.CreateCopy}
constructor TBlock.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  TT: TBlock;
begin
inherited CreateCopy(AMasterList, T);
TT := T as TBlock;
System.Move(TT.MargArray, MargArray, DWord(@Converted)-DWord(@MargArray)+Sizeof(Converted));
MyCell := TBlockCell.CreateCopy(AMasterList, TT.MyCell);
DrawList := TList.Create;
TagClass := TT.TagClass;
if Assigned(TT.BGImage) and AMasterlist.PrintTableBackground then
  BGImage := TImageObj.CreateCopy(AMasterList, TT.BGImage);
MargArrayO := TT.MargArrayO;
end;

destructor TBlock.Destroy;
begin
BGImage.Free;
TiledImage.Free;
TiledMask.Free;
FullBG.Free;
MyCell.Free;
DrawList.Free;
inherited;
end;

procedure TBlock.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
var
  MinCell, MaxCell: integer;
  LeftSide, RightSide, AutoCount: integer;
begin
MyCell.MinMaxWidth(Canvas, MinCell, MaxCell);
ConvMargArray(MargArrayO, 0, 400, EmSize, ExSize, BorderStyle, AutoCount, MargArray);
if MargArray[MarginLeft] = Auto then
  MargArray[MarginLeft] := 0;
if MargArray[MarginRight] = Auto then
  MargArray[MarginRight] := 0;
if MargArray[Width] = Auto then
  MargArray[Width] := 0;
LeftSide := MargArray[MarginLeft]+MargArray[BorderLeftWidth]+MargArray[PaddingLeft];
RightSide := MargArray[MarginRight]+MargArray[BorderRightWidth]+MargArray[PaddingRight];
Min := IntMax(MinCell, MargArray[Width]) + LeftSide + RightSide;
if MargArray[Width] > 0 then
  Max := Min
else
  Max := IntMax(MaxCell, MargArray[Width]) + LeftSide + RightSide;
end;

{----------------TBlock.GetURL}
function TBlock.GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj; var ATitle: string): guResultType;
begin
if DisplayNone then
  Result := []
else
  begin
  if (BlockTitle <> '') and PtInRect(MyRect, Point(X, Y-ParentSectionList.YOFF)) then
    begin
    ATitle := BlockTitle;
    Include(Result, guTitle);
    end;
  Result := MyCell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
  end;
end;   

{----------------TBlock.FindString}
function TBlock.FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
begin
if DisplayNone then
  Result := -1
else Result := MyCell.FindString(From, ToFind, MatchCase);
end;

{----------------TBlock.FindStringR}
function TBlock.FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
begin
if DisplayNone then
  Result := -1
else Result := MyCell.FindStringR(From, ToFind, MatchCase);
end;

{----------------TBlock.FindCursor}
function TBlock.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer;
var
  I: integer;         
begin
if DisplayNone then  
  Result := -1
else
  begin          {check this in z order}
  Result := -1;
  with DrawList do
    for I := Count-1 downto 0 do
      with TSectionBase(Items[I]) do
        begin
        if (Y >= DrawTop) and (Y < DrawBot) then
            begin
            Result := FindCursor(Canvas, X, Y, XR, YR, CaretHt, Intext);
            if Result>= 0 then
              Exit;
            end;
        end;
  end;
end;

procedure TBlock.AddSectionsToList;
begin
MyCell.AddSectionsToList;
end;

{----------------TBlock.PtInObject}
function TBlock.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
{Y is absolute}
var
  I: integer;
begin
if DisplayNone then
  Result := False
else
  begin          {check this in z order}
  Result := False;
  Obj := Nil;
  with DrawList do
    for I := Count-1 downto 0 do
      with TSectionBase(Items[I]) do
        begin
        if (Y >= DrawTop) and (Y < DrawBot) then
            begin
            Result := PtInObject(X, Y, Obj, IX, IY);
            if Result then
              Exit;
            end;
        end;
  end;
end;

{----------------TBlock.GetChAtPos}
function TBlock.GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
begin
if DisplayNone then  
  Result := False
else Result := MyCell.GetChAtPos(Pos, Ch, Obj);
end;

function TBlock.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
begin
if DisplayNone then  
  Result := False
else Result := MyCell.CursorToXY(Canvas, Cursor, X, Y);
end;             

function TBlock.FindDocPos(SourcePos: integer; Prev: boolean): integer;
begin
if DisplayNone then  
  Result := -1
else Result := MyCell.FindDocPos(SourcePos, Prev);
end;

function TBlock.FindSourcePos(DocPos: integer): integer;
begin
if DisplayNone then  
  Result := -1
else Result := MyCell.FindSourcePos(DocPos);
end;

procedure TBlock.CopyToClipboard;
begin
if not DisplayNone then
  begin
  MyCell.CopyToClipboard;
  if Pos('p.', TagClass) = 1 then
    ParentSectionList.CB.AddTextCR('', 0);    
  end;
end;

{----------------TBlock.FindWidth}
function TBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer;
var
  Marg2: integer;
  MinWidth, MaxWidth: integer;

  function BordPad: integer;
  begin
  Result := MargArray[BorderLeftWidth]+MargArray[BorderRightWidth]+
             MargArray[PaddingLeft]+MargArray[PaddingRight];
  end;

  procedure CalcWidth;
  begin
  if Positioning = posAbsolute then
    MargArray[Width] := IntMax(MinWidth,
        IntMin(MaxWidth, AWidth-BordPad-MargArray[MarginLeft]-MargArray[MarginRight]-LeftP))
  else
    MargArray[Width] := IntMax(MinWidth,
        AWidth-BordPad-MargArray[MarginLeft]-MargArray[MarginRight]);
  end;

  procedure CalcMargRt;
  begin
  MargArray[MarginRight] := IntMax(0, AWidth-BordPad-MargArray[MarginLeft]-MargArray[Width]);
  end;

  procedure CalcMargLf;
  begin
  MargArray[MarginLeft] := IntMax(0, AWidth-BordPad-MargArray[MarginRight]-MargArray[Width]);
  end;

begin
MyCell.MinMaxWidth(Canvas, MinWidth, MaxWidth);
case AutoCount of
  0: begin
     MargArray[Width] := IntMax(MinWidth, MargArray[Width]);    
     end;
  1: if MargArray[Width] = Auto then
       CalcWidth
     else
       begin
       MargArray[Width] := IntMax(MargArray[Width], MinWidth);
       if MargArray[MarginRight] = Auto then
          if (FloatLR in [ALeft, ARight]) then  
            MargArray[MarginRight] := 0
          else CalcMargRt
       else CalcMargLf;
       end;
  2: if MargArray[Width] = Auto then
       begin
       if MargArray[MarginLeft] = Auto then
         MargArray[MarginLeft] := 0
       else MargArray[MarginRight] := 0;
       CalcWidth;
       end
     else
       begin
       MargArray[Width] := IntMax(MargArray[Width], MinWidth);
       Marg2 := IntMax(0, AWidth-MargArray[Width]-BordPad);
       MargArray[MarginLeft] := Marg2 div 2;
       MargArray[MarginRight] := Marg2 div 2;
       end;
  3: begin
     MargArray[MarginLeft] := 0;
     MargArray[MarginRight] := 0;
     CalcWidth;
     end;
  end;
Result := MargArray[Width];
end;

procedure DoImageStuff(Canvas: TCanvas; IW, IH: integer; BGImage: TImageObj; PRec: PtPositionRec;
  var TiledImage, TiledMask: TBitmap; var NoMask: boolean);
{Set up for the background image. Allow for tiling, and transparency
  BGImage is the image
  PRec describes the location and tiling
  IW, IH, the width and height of the background
}
var                            
  I, OW, OH, X, XX, Y, X2, Y2: integer;
  P: array[1..2] of integer;
  TheBitmap, TheMask: TBitmap;
begin
if (BGImage.Image is TBitmap) then
  begin
  TheBitmap := TBitmap(BGImage.Image);
  TheMask := BGImage.Mask;
  end
else
  begin
  TheBitmap := TGifImage(BGImage.Image).MaskedBitmap;
  TheMask := TGifImage(BGImage.Image).Mask;
  end;

NoMask := not Assigned(TheMask) and PRec[1].RepeatD and PRec[2].RepeatD;

if not Assigned(TiledImage) then
  TiledImage := TBitmap.Create;
TiledImage.Palette := CopyPalette(ThePalette);
TiledImage.Height := IH;
TiledImage.Width := IW;
PatBlt(TiledImage.Canvas.Handle, 0, 0, IW, IH, Blackness);

if not NoMask then
  begin
  if not Assigned(TiledMask) then
    TiledMask := TBitmap.Create;
  TiledMask.Monochrome := True;
  TiledMask.Height := IH;
  TiledMask.Width := IW;
  if not Assigned(TheMask) then
    PatBlt(TiledMask.Canvas.Handle, 0, 0, IW, IH, Whiteness);
  end;

OW := BGImage.Bitmap.Width;
OH := BGImage.Bitmap.Height;

{compute the location and tiling of BGImage in the background}
P[1] := 0;  P[2] := 0;
for I := 1 to 2 do
  with PRec[I] do
    begin
    case PosType of
      pTop:
        P[I] := 0;
      pCenter:
        if I = 1 then
          P[1] := IW div 2 - OW div 2
        else P[2] := IH div 2 - OH div 2;
      pBottom:
        P[I] := IH - OH;
      pLeft:
        P[I] := 0;
      pRight:
        P[I] := IW - OW;
      PPercent:
        if I = 1 then
          P[1] := ((IW-OW) * Value) div 100
        else P[2] := ((IH-OH) * Value) div 100;
      pDim:
        P[I] := Value;
      end;
    if I=1 then
      P[1] := Intmin(IntMax(P[1], -OW), IW)
    else P[2] := Intmin(IntMax(P[2], -OH), IH);
    end;

X := P[1];
Y := P[2];
if PRec[2].RepeatD then
  begin
  while Y > 0 do
    Dec(Y, OH);
  Y2 := IH;
  end
else Y2 := Y;
if PRec[1].RepeatD then
  begin
  while X > 0 do
    Dec(X, OW);
  X2 := IW;
  end
else X2 := X;

repeat  {tile BGImage in the various dc's}
  XX := X;
  repeat
    TiledImage.Canvas.Draw(XX, Y, TheBitmap);
    if Assigned(TheMask) then
      TiledMask.Canvas.Draw(XX, Y, TheMask)
    else if not NoMask then
      PatBlt(TiledMask.Canvas.Handle, XX, Y, OW, OH, Blackness); 
    Inc(XX, OW);
  until XX >= X2;
  Inc(Y, OH);
until Y >= Y2;
end;

{----------------TBlock.DrawLogic}
function TBlock.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
         var MaxWidth: integer; var Curs: integer): integer;
var
  ScrollWidth, YClear: integer;
  LIndex, RIndex: integer;
  SaveID: TObject;
  XL, TotalWidth, MiscWidths: integer;
  AutoCount: integer;

  function GetClearSpace: integer;
  var
    CL, CR: integer;
  begin
  Result := 0;
  if (ClearAttr <> clrNone) then
    begin  {may need to move down past floating image}
    IMgr.GetClearY(CL, CR);
    case ClearAttr of
      clLeft:  Result := IntMax(0, CL-Y-1);
      clRight:  Result := IntMax(0, CR-Y-1);
      clAll: Result := IntMax(CL-Y-1, IntMax(0, CR-Y-1));
      end;
    end;
  end;

begin
if DisplayNone then
  begin
  SectionHeight := 0;   
  DrawHeight := 0;
  ContentBot := 0;
  DrawBot := 0;
  MaxWidth := 0;
  Result := 0;
  Exit;
  end;
YDraw := Y;
ClearAddOn := GetClearSpace;
StartCurs := Curs;
MaxWidth := AWidth;

ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, BorderStyle, AutoCount, MargArray);

TopP := MargArray[TopPos];
LeftP := MargArray[LeftPos];
if Positioning = PosRelative then
  begin
  if TopP = Auto then
    TopP := 0;
  if LeftP = Auto then
    LeftP := 0;
  end
else if  Positioning = PosAbsolute then
  begin
  if TopP = Auto then
    TopP := Y;
  if LeftP = Auto then
    LeftP := X;
  end;

if Positioning = posAbsolute then
  begin
  X := LeftP;
  Y := TopP+YRef;
  end;

NewWidth := FindWidth(Canvas, AWidth, AHeight, AutoCount);

MyCell.IMgr := IMgr;
SaveID := IMgr.CurrentID;
IMgr.CurrentID := Self;

MiscWidths := MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth]
        +MargArray[MarginRight]+MargArray[PaddingRight]+MargArray[BorderRightWidth];
TotalWidth := MiscWidths + NewWidth;

YClear := Y+ClearAddon;
if MargArray[MarginTop] > 0 then
  DrawTop := YClear
else DrawTop := YClear + MargArray[MarginTop]; {Border top}
if FloatLR = ALeft then
  begin
  Indent := IntMax(X, IMgr.LeftIndent(YClear)) + MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth]-X;
  end
else if FloatLR = ARight then
  Begin
  Indent := IntMin(AWidth, IMgr.RightSide(YClear)-X)- (MargArray[MarginRight]+MargArray[PaddingRight]+MargArray[BorderRightWidth]) - NewWidth;
  end
else
  begin
  Indent := MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth];
  end;

X := X + Indent;
if IsListBlock then
  begin
  XL := IMgr.LeftIndent(YClear);
  if X < XL+35 then
    begin
    Indent := Indent + 35 - (X-XL);
    NewWidth := IntMax(0, NewWidth - (35 - (X-XL)));
    X := XL + 35;
    end;
  end;
ContentTop := Y+ClearAddon+MargArray[MarginTop]+MargArray[PaddingTop]+MargArray[BorderTopWidth];

LIndex := IMgr.SetLeftIndent(X, ContentTop);
RIndex := IMgr.SetRightIndent(X+NewWidth, ContentTop);

ContentLeft := X;

if Positioning = posRelative then
  MyCell.DoLogicX(Canvas, X, ContentTop+TopP, XRef, ContentTop+TopP, NewWidth, MargArray[Height], ScrollWidth, Curs)
else if Positioning = posAbsolute then
  MyCell.DoLogicX(Canvas, X, ContentTop, XRef+LeftP+MargArray[MarginLeft]+MargArray[BorderLeftWidth],
     YRef+TopP+MargArray[MarginTop]+MargArray[BorderTopWidth], NewWidth, MargArray[Height], ScrollWidth, Curs)
else MyCell.DoLogicX(Canvas, X, ContentTop, XRef, YRef, NewWidth, MargArray[Height], ScrollWidth, Curs);

Len := Curs-StartCurs;

if Positioning in [posAbsolute, posRelative] then
  MaxWidth := ScrollWidth+MiscWidths-MargArray[MarginRight]+LeftP
else MaxWidth := ScrollWidth+MiscWidths;

if Positioning = posRelative then
  ClientContentBot := IntMax(ContentTop, MyCell.tcContentBot-TopP)
else ClientContentBot := IntMax(ContentTop, MyCell.tcContentBot);
if ClientContentBot - ContentTop < MargArray[Height] then
  ClientContentBot := ContentTop + MargArray[Height];
ContentBot := ClientContentBot + MargArray[PaddingBottom]+
              MargArray[BorderBottomWidth]+MargArray[MarginBottom];
DrawBot := IntMax(ClientContentBot, MyCell.tcDrawBot) + MargArray[PaddingBottom]
           +MargArray[BorderBottomWidth];

Result := ContentBot-Y;

if Assigned(BGImage) and ParentSectionList.ShowImages then
  begin
  BGImage.DrawLogic(ParentSectionList, Canvas, Nil, 100);
  if (BGImage.Image = ErrorBitmap) then   
    begin
    BGImage.Free;
    BGImage := Nil;
    end
  else
    begin
    BGImage.ImageKnown := True;  {won't need reformat on InsertImage}
    NeedDoImageStuff := True;
    end;
  end;
SectionHeight := Result;
IMgr.FreeLeftIndentRec(LIndex);   
IMgr.FreeRightIndentRec(RIndex);
IMgr.CurrentID := SaveID;
if (FloatLR in [ALeft, ARight]) or (Positioning = posAbsolute) then
  begin
  if Positioning = posAbsolute then
    DrawHeight := 0
  else DrawHeight := SectionHeight;
  if FloatLR = ALeft then
    IMgr.UpdateBlock(Y, X+NewWidth + MargArray[MarginRight]+
            MargArray[PaddingRight]+MargArray[BorderRightWidth], DrawBot-Y, FloatLR)
  else if FloatLR = ARight then
    IMgr.UpdateBlock(Y, TotalWidth, DrawBot-Y, FloatLR);
  SectionHeight := 0;
  Result := 0;
  end
else
  begin
  DrawHeight := IMgr.ImageBottom - Y;  {in case image overhangs}
  if DrawHeight < SectionHeight then
    DrawHeight := SectionHeight;
  end;
if DrawList.Count = 0 then  
  DrawSort;
end;

{----------------TBlock.DrawSort}
procedure TBlock.DrawSort;             
var
  I, ZeroIndx, EndZeroIndx, SBZIndex: integer;
  SB: TSectionBase;

  procedure InsertSB(I1, I2: integer);
  var
    J: integer;
    Inserted: boolean;
  begin
  Inserted := False;
  for J := I1 to I2-1 do
    if SBZIndex < TSectionBase(DrawList[J]).ZIndex then
      begin
      DrawList.Insert(J, SB);
      Inserted := True;
      Break;
      end;
  if not Inserted then
    DrawList.Insert(I2, SB);
  end;

begin
ZeroIndx := 0;
EndZeroIndx := 0;
for I := 0 to MyCell.Count-1 do
  begin
  SB := TSectionBase(MyCell.Items[I]);
  SBZIndex := SB.ZIndex;
  if SBZIndex < 0 then
    begin
    InsertSB(0, ZeroIndx);
    Inc(ZeroIndx);
    Inc(EndZeroIndx);
    end
  else if SBZIndex = 0 then  {most items go here}
    begin
    DrawList.Insert(EndZeroIndx, SB);
    Inc(EndZeroIndx);
    end
  else
    InsertSB(EndZeroIndx, DrawList.Count);
  end;
end;

{----------------TBlock.Draw1}
function TBlock.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  Y, YO: integer;
begin
if DisplayNone then
  begin
  Result := 0;
  Exit;
  end;
Y := YDraw;
Result := Y+SectionHeight;

with ParentSectionList do
  if Printing and (Positioning <> posAbsolute) then
    if BreakBefore and not FirstPageItem then     
      begin
      if ARect.Top + YOff < YDraw+MargArray[MarginTop] then  {page-break-before}
        begin
        if YDraw+MargArray[MarginTop] < PageBottom then
          PageBottom := YDraw+MargArray[MarginTop];
        Exit;
        end;
      end
    else if KeepIntact then
      begin
      {if we're printing and block won't fit on this page and block will fit on
       next page, then don't do block now}
      YO := Y - YOff;
      if (YO > ARect.Top) and (Y+DrawHeight > PageBottom) and
            (DrawHeight-MargArray[MarginTop] < ARect.Bottom - ARect.Top) then
        begin
        if Y+MargArray[MarginTop] < PageBottom then
          PageBottom := Y+MargArray[MarginTop];
        Exit;
        end;
      end
    else if BreakAfter then
      if ARect.Top + YOff < Result then    {page-break-after}
        if Result < PageBottom then
          PageBottom := Result;

if Visibility <> viHidden then
  if Positioning = posRelative then   {for debugging}
      DrawBlock(Canvas, ARect, IMgr, X+LeftP, Y+TopP, XRef, YRef)
  else if Positioning = posAbsolute then
    DrawBlock(Canvas, ARect, IMgr, XRef+LeftP, YRef+TopP, XRef, YRef)
  else if FloatLR in [ALeft, ARight] then
    DrawBlock(Canvas, ARect, IMgr, X, Y, XRef, YRef)
  else DrawBlock(Canvas, ARect, IMgr, X, Y, XRef, YRef);
end;

{----------------TBlock.DrawBlock}
procedure TBlock.DrawBlock(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, Y, XRef, YRef : integer);
var
  YOffset: integer;
  XR, YB, BL, BT, BR, BB, PL, PT, PR, PB, RefX, TmpHt: integer;
  BorderRegion, PaddingRegion: THandle;
  Brush: TBrush;
  SaveID: TObject;
  ImgOK, HasBackgroundColor: boolean;
  IT, IH, FT: integer;

  procedure InitFullBg(W, H: integer);
  begin
  if not Assigned(FullBG) then
    begin
    FullBG := TBitmap.Create;
    if ParentSectionList.IsCopy then
      begin
      FullBG.HandleType := bmDIB;
      if ColorBits <= 8 then
        FullBG.Palette := CopyPalette(ThePalette);
      end;
    end;
  FullBG.Height := IntMax(H, 2);;
  FullBG.Width := IntMax(W, 2);
  end;

begin
YOffset := ParentSectionList.YOff;

case FLoatLR of
  ALeft, ARight: RefX := X+Indent-(MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth]);
  else
    RefX := X;
  end;

X := X+Indent;

XR := RefX + MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth]
        + NewWidth + MargArray[MarginRight]+
            MargArray[PaddingRight]+MargArray[BorderRightWidth];  {current right edge}
if Positioning = posRelative then
  YB := ContentBot - YOffset + TopP
else if FLoatLR in [ALeft, ARight] then
  YB := DrawBot + MargArray[MarginBottom] - YOffset
else YB := ContentBot - YOffset;

BL := RefX + MargArray[MarginLeft];   {Border left and right}
BR := XR - MargArray[MarginRight];
PL := BL + MargArray[BorderLeftWidth]; {Padding left and right}
PR := BR - MargArray[BorderRightWidth];

BT := Y + ClearAddon + MargArray[MarginTop] - YOffset;   {Border Top and Bottom}
BB := YB - MargArray[MarginBottom];
PT := BT + MargArray[BorderTopWidth]; {Padding Top and Bottom}
PB := BB - MargArray[BorderBottomWidth];

IT := IntMax(0, Arect.Top-2-PT);
FT := IntMax(PT, ARect.Top-2);    {top of area drawn, screen coordinates}
IH := IntMin(PB-FT, Arect.Bottom-FT); {height of area actually drawn}

MyRect := Rect(BL, BT, BR, BB);   
if (BT <= ARect.Bottom) and (BB >= ARect.Top) then       
  begin
  HasBackgroundColor := MargArray[BackgroundColor] <> clNone;

  if BorderStyle <> bssNone then
    {Region defined by outside of Border}
    BorderRegion := CreateRectRgn(BL, BT, BR, BB)
  else BorderRegion := 0;
  PaddingRegion := 0;

  try
    try
      if NeedDoImageStuff and (BGImage.Image <> DefBitmap) then
        begin
        if FloatLR in [ALeft, ARight] then
          TmpHt := DrawBot-ContentTop+MargArray[PaddingTop]+MargArray[PaddingBottom]
        else TmpHt := ClientContentBot-ContentTop+MargArray[PaddingTop]+MargArray[PaddingBottom];

        DoImageStuff(Canvas, MargArray[PaddingLeft]+NewWidth+MargArray[PaddingRight],
             TmpHt, BGImage, PRec, TiledImage, TiledMask, NoMask);
        if ParentSectionList.IsCopy then
          TiledImage.HandleType := bmDIB;
        NeedDoImageStuff := False;
        end;

      ImgOK := Not NeedDoImageStuff and Assigned(BGImage) and (BGImage.Bitmap <> DefBitmap)
                   and ParentSectionList.ShowImages;

      if HasBackgroundColor and
          (not ParentSectionList.Printing or ParentSectionList.PrintTableBackground) then
        begin   {color the Padding Region}
        Canvas.Brush.Color := MargArray[BackgroundColor] or $2000000;
        Canvas.Brush.Style := bsSolid;
        if ParentSectionList.IsCopy and ImgOK then
          begin
          InitFullBG(PR-PL, IH);
          FullBG.Canvas.Brush.Color := MargArray[BackgroundColor] or $2000000;
          FullBG.Canvas.Brush.Style := bsSolid;
          FullBG.Canvas.FillRect(Rect(0, 0, PR-PL, IH));
          end
        else Canvas.FillRect(Rect(PL, FT, PR, FT+IH));
        end;

      if ImgOK then
        begin
        if not ParentSectionList.IsCopy then
          if NoMask then
            BitBlt(Canvas.Handle, PL, FT, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SrcCopy)
          else
            begin
            InitFullBG(PR-PL, IH);
            BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, Canvas.Handle, PL, FT, SrcCopy);
            BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SrcInvert);
            BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
            BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SRCPaint);
            BitBlt(Canvas.Handle, PL, FT, PR-PL, IH, FullBG.Canvas.Handle, 0, 0, SRCCOPY);
            end
        else if NoMask then
          PrintBitmap(Canvas, PL, FT, PR-PL, IH, TiledImage.Handle)     
        else if HasBackgroundColor then
          begin
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SrcInvert);
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
          BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SRCPaint);
          PrintBitmap(Canvas, PL, FT, PR-PL, IH, FullBG.Handle);
          end
        else
          PrintTransparentBitmap3(Canvas, PL, FT, PR-PL, IH, TiledImage, TiledMask, IT, IH);
        end;

      if BorderRegion <> 0 then
        begin
        PaddingRegion := CreateRectRgn(PL, PT, PR, PB);
        CombineRgn(BorderRegion, BorderRegion, PaddingRegion, RGN_DIFF);
        Brush := TBrush.Create;
        if MargArray[BorderColor] <> clNone then
          Brush.Color := MargArray[BorderColor]
        else Brush.Color := FGColor;
        Brush.Style := bsSolid;
        FillRgn(Canvas.Handle, BorderRegion, Brush.Handle);
        Brush.Free;
         end;
    finally
      if BorderRegion <> 0 then
        DeleteObject(BorderRegion);
      if PaddingRegion <> 0 then
        DeleteObject(PaddingRegion);
     end;
  except
    end;
  end;

SaveID := IMgr.CurrentID;
Imgr.CurrentID := Self;
if Positioning = posRelative then
  DrawTheList(Canvas, ARect, NewWidth, X,
      RefX+MargArray[MarginLeft]+MargArray[BorderLeftWidth]+MargArray[PaddingLeft],
      Y+MargArray[MarginTop]+MargArray[BorderTopWidth]+MargArray[PaddingTop])
else if Positioning = posAbsolute then
  DrawTheList(Canvas, ARect, NewWidth, X,
      RefX+MargArray[MarginLeft]+MargArray[BorderLeftWidth],
      Y+MargArray[MarginTop]+MargArray[BorderTopWidth])
else DrawTheList(Canvas, ARect, NewWidth, X, XRef, YRef);
Imgr.CurrentID := SaveID;
end;

procedure TBlock.DrawTheList(Canvas: TCanvas; ARect: TRect; ClipWidth, X,
                          XRef, YRef :integer);
{draw the list sorted by Z order.}
var
  I: integer;
  SB: TSectionBase;
begin
with DrawList do
  for I := 0 to Count-1 do
    begin
    SB := TSectionBase(Items[I]);
    SB.Draw1(Canvas, ARect, MyCell.IMgr, X, XRef, YRef);
    end;
end;

procedure TBlock.FormTree(Indent: string; var Tree: string);
var
  MyIndent: string;
  TM, BM: string;
begin
MyIndent := Indent + '   ';
TM := IntToStr(MargArray[MarginTop]);
BM := IntToStr(MargArray[MarginBottom]);
Tree := Tree+Indent+TagClass+'  '+TM+'  '+BM+^M^J;
MyCell.FormTree(MyIndent, Tree);
end;

{----------------TTableBlock.Create}
constructor TTableBlock.Create(Master: TSectionList; Prop: TProperties;
    	AnOwnerCell: TCellBasic; ATable: ThtmlTable; TableAttr: TAttributeList;
        TableLevel: integer);
var
  I, AutoCount: integer;
  Percent: boolean;   
begin
inherited Create(Master, Prop, AnOwnerCell, TableAttr);  
Table := ATable;
Justify := NoJustify;   

for I := 0 to TableAttr.Count-1 do
  with TAttribute(TableAttr[I]) do
    case Which of
      AlignSy:
        if CompareText(Name, 'CENTER') = 0 then
          Justify := Centered                      
        else if CompareText(Name, 'LEFT') = 0 then
          begin
          if FloatLR = ANone then
            FloatLR := ALeft;
          end
        else if CompareText(Name, 'RIGHT') = 0 then
          begin
          if FloatLR = ANone then
            FloatLR := ARight;
          end;
      BGColorSy:
        BkGnd := ColorFromString(Name, False, BkColor);
      BackgroundSy:
        if not Assigned(BGImage) then
          begin
          BGImage := TImageObj.SimpleCreate(Master, Name);
          PRec[1].PosType := pDim;
          PRec[1].Value := 0;
          PRec[1].RepeatD := True;
          PRec[2] := PRec[1];
          end;
      HSpaceSy: HSpace := IntMin(40, Abs(Value));
      VSpaceSy: VSpace := IntMin(200, Abs(Value));
      WidthSy:
        if Pos('%', Name) > 0 then
          begin
          if (Value > 0) and (Value <= 100) then WidthAttr := Value*10;
          AsPercent := True;
          end
        else WidthAttr := Value;
      HeightSy:
        if (Pos('%', Name) > 0) then
          begin
          if (Value > 0) and (Value <= 110) and (TableLevel = 1) then
            begin
            HeightAttr := Value*10;
            HtAsPercent := True;
            end;
          end
        else HeightAttr := Value;
      end;

{need to see if width is defined in style}   
Percent := (VarType(MargArrayO[Width]) = VarString) and (Pos('%', MargArrayO[Width]) > 0);
ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, bssNone, AutoCount, MargArray);
if MargArray[Width] > 0 then
  if Percent then
    begin
    if MargArray[Width] < 100 then
      begin
      AsPercent := True;
      WidthAttr := MargArray[Width] * 10;
      end;
    end
  else
    begin
    WidthAttr := MargArray[Width];
    AsPercent := False;
    end;                

CollapseMargins;
Table.Float := FloatLR in [ALeft, ARight];
if Table.Float and (ZIndex = 0) then
  ZIndex := 1;
end;

constructor TTableBlock.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  TT: TTableBlock;
  Item: TObject;
begin
inherited;
TT := T as TTableBlock;
System.Move(TT.WidthAttr, WidthAttr, DWord(@Justify)-DWord(@WidthAttr)+Sizeof(Justify));
Item := MyCell.Items[0];
Table := Item as ThtmlTable;
end;

procedure TTableBlock.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
begin
if not AsPercent then         
  Table.WidthAttr := WidthAttr
else Table.WidthAttr := 0;
inherited MinMaxWidth(Canvas, Min, Max);
if (WidthAttr > 0) and not AsPercent then
  begin
  Min := IntMax(Min, WidthAttr);
  Max := Min;
  end;
end;

{----------------TTableBlock.FindWidth}
function TTableBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer;
var
  LeftSide, RightSide, SWidth: integer;
  Min, Max, Allow: integer;
begin
if MargArray[MarginLeft] = Auto then
  MargArray[MarginLeft] := 0;
if MargArray[MarginRight] = Auto then
  MargArray[MarginRight] := 0;

if FloatLR in [ALeft, ARight] then
  begin
  if MargArray[MarginLeft] = 0 then
    MargArray[MarginLeft] := HSpace;
  if MargArray[MarginRight] = 0 then
    MargArray[MarginRight] := HSpace;
  if MargArray[MarginTop] = 0 then
    MargArray[MarginTop] := VSpace;
  if MargArray[MarginBottom] = 0 then
    MargArray[MarginBottom] := VSpace;
  end;

if BkGnd and (MargArray[BackgroundColor] = clNone) then
  MargArray[BackgroundColor]  := BkColor;
Table.BkGnd := (MargArray[BackgroundColor] <> clNone) and not Assigned(BGImage);
Table.BkColor := MargArray[BackgroundColor];   {to be passed on to cells}

LeftSide := MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth];
RightSide := MargArray[MarginRight]+MargArray[PaddingRight]+MargArray[BorderRightWidth];
SWidth := MargArray[Width];
if SWidth = Auto then
  SWidth := 0;

Table.WidthAttr := 0;
Table.AsPercent := False;
if SWidth > 0 then
  begin
  Result := SWidth;    
  Table.WidthAttr := Result;   
  Table.AsPercent := False;
  end
else if WidthAttr > 0 then
  begin
  if AsPercent then
    Result := MulDiv(AWidth, WidthAttr, 1000)
  else
    begin
    Result := WidthAttr;
    Table.WidthAttr := WidthAttr;    
    end;
  Table.MinMaxWidth(Canvas, Min, Max);
  Result := IntMax(Min, Result);
  Table.WidthAttr := Result;
  end
else
  begin
  Table.MinMaxWidth(Canvas, Min, Max);
  Allow := AWidth - LeftSide - RightSide;
  if Max <= Allow then
    Result := Max               
  else if Min >= Allow then
    Result := Min                    
  else Result := Allow;
  end;
MargArray[Width] := Result;

if (Result < AWidth) and                     
     (MargArray[MarginLeft] = 0) and (MargArray[MarginRight] = 0) then
  case Justify of
    Centered:
      MargArray[MarginLeft] := (AWidth - Result) div 2;
    Right:
      MargArray[MarginLeft] := (AWidth - Result);      
  end;

if (MargArray[Height] = 0) and (HeightAttr > 0) then
  if HtAsPercent then
    MargArray[Height] := MulDiv(AHeight, HeightAttr, 1000)
  else MargArray[Height] := HeightAttr;
Table.ProposedHeight := MargArray[Height];
end;

function TTableBlock.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer;
           IMgr: IndentManager; var MaxWidth: integer; var Curs: integer): integer;
var
  X1, Tmp: integer;
begin
if not (FloatLR in [ALeft, ARight]) then
  begin
  Tmp := X;
  X := IntMax(Tmp, IMgr.LeftIndent(Y));
  X1 := IntMin(Tmp+AWidth, IMgr.RightSide(Y));
  AWidth := X1 - X;
  end;
Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, IMgr, MaxWidth, Curs);
end;

function TTableBlock.Draw1(Canvas: TCanvas; const ARect: TRect;
         IMgr: IndentManager; X, XRef, YRef : integer) : integer;
begin
if not (FloatLR in [ALeft, ARight]) then
  X := IntMax(X, IMgr.LeftIndent(YDraw));
Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);
end;

procedure TTableBlock.AddSectionsToList;
begin   {Sections in Table not added only table itself}
ParentSectionList.PositionList.Add(Table);
end;

constructor THRBlock.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
begin
inherited;
Align := (T as THRBlock).Align;
end;

{----------------THRBlock.FindWidth}
function THRBlock.FindWidth(Canvas: TCanvas; AWidth, AHeight, AutoCount: integer): integer;
var
  LeftSide, RightSide, SWidth: integer;
  Diff: integer;
begin
LeftSide := MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth];
RightSide := MargArray[MarginRight]+MargArray[PaddingRight]+MargArray[BorderRightWidth];
SWidth := MargArray[Width];

if SWidth > 0 then
   Result := IntMin(SWidth, AWidth - LeftSide - RightSide)
else Result := IntMax(15, AWidth - LeftSide - RightSide);
MargArray[Width] := Result;
{note that above could be inherited; if LeftSide and Rightside were fields
of TBlock}

if Align <> Left then
  begin
  Diff := AWidth-Result-LeftSide-RightSide;
  if Diff > 0 then
    case Align of
      Centered:  Inc(MargArray[MarginLeft], Diff div 2);
      Right:  Inc(MargArray[MarginLeft], Diff);
      end;
  end;
if not ParentSectionList.IsCopy then
  THorzline(MyHRule).VSize := MargArray[StyleUn.Height];
end;

{----------------TBlockLI.Create}
constructor TBlockLI.Create(Master: TSectionList; Prop: TProperties; AnOwnerCell: TCellBasic;
                Sy: Symb; APlain: boolean; AIndexType: char; AListNumb,
                ListLevel: integer; Attributes: TAttributeList);    
var
  Tmp: ListBulletType;
  S: string;
  TmpFont: TMyFont;
begin
inherited Create(Master, Prop, AnOwnerCell, Attributes);  
case Sy of
  UlSy, DirSy, MenuSy:
    begin
    ListType := Unordered;
    if APlain then
      ListStyleType := lbNone
    else 
      case ListLevel Mod 3 of               
        1: ListStyleType := lbDisc;
        2: ListStyleType := lbCircle;
        0: ListStyleType := lbSquare;
        end;
    end;
  OLSy:
      begin
      ListType := Ordered;
      case AIndexType of
        'a': ListStyleType := lbLowerAlpha;
        'A': ListStyleType := lbUpperAlpha;
        'i': ListStyleType := lbLowerRoman;
        'I': ListStyleType := lbUpperRoman;
        else ListStyleType := lbDecimal;
        end;
      end;
  DLSy: ListType := Definition;
  else
    begin
    ListType := liAlone;
    ListStyleType := lbDisc;
    if (VarType(MargArrayO[MarginLeft]) in varInt) and
         ((MargArrayO[MarginLeft] = IntNull) or (MargArrayO[MarginLeft] = 0)) then
      MargArrayO[MarginLeft] := 16;  
    end;
  end;
ListNumb := AListNumb;

Tmp := Prop.GetListStyleType;
if Tmp <> lbBlank then     
  ListStyleType := Tmp;
ListFont := TMyFont.Create;
TmpFont := Prop.GetFont;
ListFont.Assign(TmpFont);
TmpFont.Free;

S := Prop.GetListStyleImage;
if S <> '' then
  Image := TImageObj.SimpleCreate(Master, S);
end;

constructor TBlockLI.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  TT: TBlockLI;
begin
inherited CreateCopy(AMasterList, T);
TT := T as TBlockLI;
ListType := TT.ListType;
ListNumb := TT.ListNumb;
ListStyleType := TT.ListStyleType;
if Assigned(TT.Image) then
  Image := TImageObj.CreateCopy(AMasterList, TT.Image);
ListFont := TMyFont.Create;
ListFont.Assign(TT.ListFont);
end;

destructor TBlockLI.Destroy;
begin
ListFont.Free;
Image.Free;
inherited;
end;

function TBlockLI.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
begin
if Assigned(Image) then
  begin
  Image.DrawLogic(ParentSectionList, Canvas, Nil, 100);
  if (Image.Image = ErrorBitmap) then
    begin
    Image.Free;
    Image := Nil;
    end;
  end;
ParentSectionList.FirstLineHtPtr := @FirstLineHt;
FirstLineHt := 0;
try
  Result := inherited DrawLogic(Canvas, X, Y, XRef, YRef, AWidth, AHeight, IMgr, MaxWidth, Curs);
finally
  ParentSectionList.FirstLineHtPtr := Nil;
  end;
end;

{----------------TBlockLI.Draw}
function TBlockLI.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;

const
  MaxRoman = 20;
  LowRoman: array[1..MaxRoman] of string[5] = ('i', 'ii', 'iii', 'iv', 'v', 'vi',
     'vii', 'viii', 'ix', 'x', 'xi', 'xii', 'xiii', 'xiv', 'xv', 'xvi', 'xvii',
     'xviii', 'xix', 'xx');
  HighRoman: array[1..MaxRoman] of string[5] = ('I', 'II', 'III', 'IV', 'V', 'VI',
     'VII', 'VIII', 'IX', 'X', 'XI', 'XII', 'XIII', 'XIV', 'XV', 'XVI', 'XVII',
     'XVIII', 'XIX', 'XX');
var
  NStr : string[7];
  BkMode, TAlign: integer;
  PenColor, BrushColor: TColor;
  PenStyle: TPenStyle;
  BrushStyle: TBrushStyle;
  YB, AlphaNumb: integer;

  procedure Circle(X, Y, Rad: integer);
  begin
  Canvas.Ellipse(X, Y-Rad, X+Rad, Y);
  end;

begin
Result := inherited Draw1(Canvas, ARect, IMgr, X, XRef, YRef);

X := X+Indent;  

if FirstLineHt > 0 then
  begin
  YB := FirstLineHt-ParentSectionList.YOff;
  if (YB < ARect.Top-50) or (YB > ARect.Bottom+50) then   
    Exit;
  if Assigned(Image) and (Image.Image <> DefBitmap) and ParentSectionList.ShowImages then
    begin
    Image.DoDraw(Canvas, X-16, YB-Image.ObjHeight, Image.Image, Image.Mask);
    end

  else if not (ListType in [None, Definition]) then
    begin
    if ListStyleType in [lbDecimal, lbLowerAlpha, lbLowerRoman, lbUpperAlpha, lbUpperRoman] then
      begin
      AlphaNumb := IntMin(ListNumb-1, 25);
      case ListStyleType of
        lbLowerAlpha: NStr := chr(ord('a')+AlphaNumb);
        lbUpperAlpha: NStr := chr(ord('A')+AlphaNumb);
        lbLowerRoman: NStr := LowRoman[IntMin(ListNumb, MaxRoman)];
        lbUpperRoman: NStr := HighRoman[IntMin(ListNumb, MaxRoman)];
        else NStr := IntToStr(ListNumb);
        end;
      Canvas.Font := ListFont;
      NStr := NStr+'.';
      BkMode := SetBkMode(Canvas.Handle, Transparent);
      TAlign := SetTextAlign(Canvas.Handle, TA_BASELINE);
      Canvas.TextOut(X-10-Canvas.TextWidth(NStr), YB, NStr);
      SetTextAlign(Canvas.Handle, TAlign);
      SetBkMode(Canvas.Handle, BkMode);
      end
    else if (ListStyleType in [lbCircle, lbDisc, lbSquare]) then
      with Canvas do
        begin
        PenColor := Pen.Color;
        PenStyle := Pen.Style;
        Pen.Color := ListFont.Color;
        Pen.Style := psSolid;
        BrushStyle := Brush.Style;
        BrushColor := Brush.Color;
        Brush.Style := bsSolid;
        Brush.Color := ListFont.Color;
        case ListStyleType of
          lbCircle:
             begin
             Brush.Style := bsClear;
             Circle(X-16, YB, 7);
             end;
          lbDisc:
             Circle(X-15, YB-1, 5);   
          lbSquare: Rectangle(X-15, YB-6, X-10, YB-1);
          end;
        Brush.Color := BrushColor;
        Brush.Style := BrushStyle;
        Pen.Color := PenColor;
        Pen.Style := PenStyle;
        end;
    end;
  end;
end;

{----------------TBodyBlock.Create}
constructor TBodyBlock.Create(Master: TSectionList; Prop: TProperties;
                   AnOwnerCell: TCellBasic; Attributes: TAttributeList);   
var
  PRec: PtPositionRec;
  Image: string;
  Val: TColor;
begin
inherited;
Prop.GetBackgroundPos(0, 0, PRec);
  if Prop.GetBackgroundImage(Image) and (Image <> '') then
    Master.SetBackgroundBitmap(Image, PRec);
Val := Prop.GetBackgroundColor;
if Val <> clNone then
  Master.SetBackGround(Val or $2000000);
end;

{----------------TBodyBlock.GetURL}
function TBodyBlock.GetURL(Canvas: TCanvas; X: integer; Y: integer;
     var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
     var ATitle: string): guResultType;
begin
if (BlockTitle <> '') then
  begin
  ATitle := BlockTitle;
  Include(Result, guTitle);
  end;
Result := MyCell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
end;

{----------------TBodyBlock.DrawLogic}
function TBodyBlock.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
         var MaxWidth: integer; var Curs: integer): integer;
var
  ScrollWidth: integer;
  Lindex, RIndex, AutoCount: integer;
  SaveID: TObject;
  ClientContentBot: integer;
begin
YDraw := Y;
StartCurs := Curs;
ConvMargArray(MargArrayO, AWidth, AHeight, EmSize, ExSize, BorderStyle, AutoCount, MargArray);

NewWidth := IMgr.Width - (MargArray[MarginLeft]+MargArray[PaddingLeft]+
            MargArray[BorderLeftWidth] + MargArray[MarginRight]+
            MargArray[PaddingRight]+MargArray[BorderRightWidth]);

X := MargArray[MarginLeft]+MargArray[PaddingLeft]+MargArray[BorderLeftWidth];
DrawTop := MargArray[MarginTop];

MyCell.IMgr := IMgr;
SaveID := IMgr.CurrentID;
Imgr.CurrentID := Self;
LIndex := IMgr.SetLeftIndent(X, Y);
RIndex := IMgr.SetRightIndent(X+NewWidth, Y);

ContentTop := Y+MargArray[MarginTop]+MargArray[PaddingTop]+MargArray[BorderTopWidth];
ContentLeft := X;
MyCell.DoLogicX(Canvas, X, ContentTop, 0, 0, NewWidth,
    AHeight-MargArray[MarginTop]-MargArray[MarginBottom], ScrollWidth, Curs);

Len := Curs-StartCurs;

ClientContentBot := IntMax(ContentTop, MyCell.tcContentBot);
ContentBot := ClientContentBot + MargArray[PaddingBottom]+
              MargArray[BorderBottomWidth]+MargArray[MarginBottom];
DrawBot := IntMax(ClientContentBot, MyCell.tcDrawBot) + MargArray[PaddingBottom]
           +MargArray[BorderBottomWidth];              

MyCell.tcDrawTop := 0;
MyCell.tcContentBot := 999000;

Result := DrawBot+MargArray[MarginBottom]-Y;
SectionHeight := Result;
IMgr.FreeLeftIndentRec(LIndex);   
IMgr.FreeRightIndentRec(RIndex);
DrawHeight := IMgr.ImageBottom - Y;  {in case image overhangs}
Imgr.CurrentID := SaveID;
if DrawHeight < SectionHeight then
  DrawHeight := SectionHeight;
MaxWidth := IntMax(IMgr.Width, IntMax(ScrollWidth, NewWidth)+MargArray[MarginLeft]+MargArray[MarginRight]);
if DrawList.Count = 0 then  
  DrawSort;
end;

{----------------TBodyBlock.Draw}
function TBodyBlock.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  SaveID: TObject;
  Y: integer;
begin
Y := YDraw;
Result := Y+SectionHeight;

X := IMgr.LfEdge+MargArray[MarginLeft]+MargArray[BorderLeftWidth]+MargArray[PaddingLeft];
SaveID := IMgr.CurrentID;
Imgr.CurrentID := Self;
DrawTheList(Canvas, ARect, NewWidth, X, IMgr.LfEdge, 0);
Imgr.CurrentID := SaveID;
end;

{----------------TSectionList}
constructor TSectionList.Create(Owner, APaintPanel: TWinControl);
begin
inherited Create(Self);
TheOwner := Owner;
PPanel := APaintPanel;
IDNameList := TIDNameList.Create(Self);
htmlFormList := TFreeList.Create;
AGifList := TList.Create;
MapList := TFreeList.Create;
FormControlList := TList.Create;
MissingImages := TStringList.Create;
MissingImages.Sorted := False;
LinkList := TList.Create;
PanelList := TList.Create;
Styles := TStyleList.Create(Self);   
DrawList := TDrawList.Create;
PositionList := TList.Create;
TabOrderList := TStringList.Create;   
TabOrderList.Sorted := True;
TabOrderList.Duplicates := dupAccept;  
end;

{----------------TSectionList.CreateCopy}
constructor TSectionList.CreateCopy(T: TSectionList);
begin
PrintTableBackground := T.PrintTableBackground;
BitmapList := T.BitmapList;     {same list}
IsCopy := True;    
inherited CreateCopy(Self, T);
System.Move(T.ShowImages, ShowImages, DWord(@Background)-Dword(@ShowImages)+Sizeof(integer));
BitmapName := '';
BackgroundBitmap := Nil;
BackgroundMask := Nil;
BitmapLoaded := False;
htmlFormList := TFreeList.Create;    {no copy of list made}
AGifList := TList.Create;
Timer := Nil;
MapList := TFreeList.Create;
MissingImages := TStringList.Create;
PanelList := TList.Create;
DrawList := TDrawList.Create;
end;

destructor TSectionList.Destroy;
begin
Clear;
IDNameList.Free;
htmlFormList.Free;
MapList.Free;
AGifList.Free;
Timer.Free;
FormControlList.Free;
MissingImages.Free;
LinkList.Free;
PanelList.Free;
Styles.Free;
DrawList.Free;
PositionList.Free;
TabOrderList.Free;   
inherited Destroy;
end;

function TSectionList.GetURL(Canvas: TCanvas; X: integer; Y: integer;
             var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
             var ATitle: string): guResultType;
var
  OldLink: TFontObj;
  OldImage: TImageObj;
begin
OldLink := ActiveLink;
OldImage := ActiveImage;
ActiveLink := Nil;
ActiveImage := Nil;
Result := inherited GetUrl(Canvas, X, Y, UrlTarg, FormControl, ATitle);
if LinksActive and (ActiveLink <> OldLink) then
  begin
  if OldLink <> Nil then
    OldLink.SetAllHovers(LinkList, False);
  if ActiveLink <> Nil then 
    ActiveLink.SetAllHovers(LinkList, True);
  PPanel.Invalidate;
  end;
if (ActiveImage <> OldImage) then
  begin
  if OldImage <> Nil then
    OldImage.Hover := hvOff;  
  end;
if ActiveImage <> Nil then        
  if Word(GetKeyState(VK_LBUTTON)) and $8000 <> 0 then
    ActiveImage.Hover := hvOverDown
  else
    ActiveImage.Hover := hvOverUp;
end;

procedure TSectionList.LButtonDown(Down: boolean);   
{called from htmlview.pas when left mouse button depressed}
begin
if ActiveImage <> Nil then        
  begin
  if Down then
    ActiveImage.Hover := hvOverDown
  else
    ActiveImage.Hover := hvOverUp;
  PPanel.Invalidate;
  end;
end;

procedure TSectionList.CancelActives;
begin
if Assigned(ActiveLink) or Assigned(ActiveImage) then
  PPanel.Invalidate;
if Assigned(ActiveLink) then
  begin
  ActiveLink.SetAllHovers(LinkList, False);
  ActiveLink := Nil;
  end;
if Assigned(ActiveImage) then
  begin
  ActiveImage.Hover := hvOff;  
  ActiveImage := Nil;
  end;
end;

procedure TSectionList.CheckGIFList(Sender: TObject);
var
  I: integer;
begin
if IsCopy then Exit;
for I := 0 to AGifList.Count-1 do
    with TGifImage(AGifList.Items[I]) do
      if ShowIt then
        begin
        CheckTime(PPanel);     
        end;
Timer.Interval := 40;
end;

procedure TSectionList.HideControls;  
var
  I, J: integer;
begin
  {After next Draw, hide all formcontrols that aren't to be shown}
  for I := 0 to htmlFormList.Count-1 do
    with ThtmlForm(htmlFormList.Items[I]) do
      for J := 0 to ControlList.Count-1 do
        with  TFormControlObj(ControlList.Items[J]) do
          ShowIt := False;
  for I := 0 to PanelList.Count-1 do
    TPanelObj(PanelList[I]).ShowIt := False;   {same for panels}
end;

procedure TSectionList.SetYOffset(Y: integer);
begin
YOff := Y;
YOffChange := True;
HideControls;
end;

procedure TSectionList.Clear;
begin
if not IsCopy then
  begin
  IDNameList.Clear;
  PositionList.Clear;
  end;
BackgroundBitmap := Nil;
BackgroundMask := Nil;
if BitmapLoaded and (BitmapName <> '') then
  BitmapList.DecUsage(BitmapName);
BitmapName := '';
BitmapLoaded := False;
htmlFormList.Clear;
if Assigned(FormControlList) then
  FormControlList.Clear;
AGifList.Clear;
Timer.Free;
Timer := Nil;
SelB := 0;
SelE := 0;
MapList.Clear;
MissingImages.Clear;
if Assigned(LinkList) then
  LinkList.Clear;
ActiveLink := Nil;
ActiveImage := Nil;
PanelList.Clear;
if not IsCopy then
  Styles.Clear;
if Assigned(TabOrderList) then
  TabOrderList.Clear;   
inherited Clear;
end;

procedure TSectionList.ClearLists;
{called from DoBody to clear some things when starting over}
begin
PanelList.Clear;
if Assigned(FormControlList) then
  FormControlList.Clear;
end;

{----------------TSectionList.GetSelLength:}
function TSectionList.GetSelLength: integer;
var
  I: integer;
begin
Result := 0;
if SelE <= SelB then Exit;   {nothing to do}
CB := SelTextCount.Create;
try
  for I := 0 to Count-1 do
    with TSectionBase(Items[I]) do
      begin
      if (SelB >= StartCurs + Len) then Continue;
      if (SelE <= StartCurs) then Break;
      CopyToClipboard;
      end;
  Result := CB.Terminate;
finally
  CB.Free;
  end;
end;

{----------------TSectionList.CopyToClipboard}
procedure TSectionList.CopyToClipboardA(Leng: integer);    
var
  I: integer;
  SB: TSectionBase;
begin
if SelE <= SelB then Exit;   {nothing to do}
try
  CB := ClipBuffer.Create(Leng);    
  for I := 0 to Count-1 do
    begin
    SB := TSectionBase(Items[I]);
    with SB do
      begin
      if (SelB >= StartCurs + Len) then Continue;
      if (SelE <= StartCurs) then Break; 
      CopyToClipboard;
      end;
    end;
  CB.Terminate;
finally
  CB.Free;
  end;
end;

{----------------TSectionList.GetSelTextBuf}
function TSectionList.GetSelTextBuf(Buffer: PWideChar; BufSize: integer): integer;

var
  I: integer;
begin
if BufSize >= 1 then
  begin
  Buffer[0] := #0;
  Result := 1;
  end
else Result := 0;
if SelE <= SelB then Exit;   {nothing to do}
CB := SelTextBuf.Create(Buffer, BufSize);  
try
  for I := 0 to Count-1 do
    with TSectionBase(Items[I]) do
      begin
      if (SelB >= StartCurs + Len) then Continue;
      if (SelE <= StartCurs) then Break;
      CopyToClipboard;
      end;
  Result := CB.Terminate;
finally
  CB.Free;
  end;
end;

{----------------TSectionList.DoLogic}
function TSectionList.DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight: integer;
              var ScrollWidth: integer; var Curs: integer): integer;
var
  I, J: integer;
begin
Inc(CycleNumber);
if Assigned(Timer) then Timer.Enabled := False;
for I := 0 to htmlFormList.Count-1 do
  ThtmlForm(htmlFormList.Items[I]).SetSizes(Canvas);
SetTextJustification(Canvas.Handle, 0, 0);

{set up the tab order for form controls according to the TabIndex attributes}
if Assigned(TabOrderList) and (TabOrderList.Count > 0) then
  with TabOrderList do
    begin
    J := 0;  {tab order starts with 0}
    for I := 0 to Count-1 do    {list is sorted into proper order}
      begin
      if Objects[I] is TFormControlObj then
        begin
        with Objects[I] as TFormControlObj do
          if Assigned(FControl) then
            begin
            FControl.TabOrder := J;
            Inc(J);
            end;
        end
      else if Objects[I] is TFontObj then
        begin
        with Objects[I] as TFontObj do
          if Assigned(TabControl) then
            begin
            TabControl.TabOrder := J;
            Inc(J);
            end;
        end;
      end;
    TabOrderList.Clear;   {only need do this once}
    end;

Result := inherited DoLogic(Canvas, Y, Width, AHeight, ScrollWidth, Curs);

for I := 0 to AGifList.Count-1 do
  with TGifImage(AGifList.Items[I]) do
    begin
    CurrentFrame := 1;   {required for dtDoNothing and background}
    Animate := False;    {starts iteration count from 1}
    if not Self.IsCopy then
      Animate := True;
    end;
if not IsCopy and not Assigned(Timer) then
  begin
  Timer := TTimer.Create(TheOwner as ThtmlViewer);
  Timer.Interval := 50;
  Timer.OnTimer := CheckGIFList;
  end;
if Assigned(Timer) then Timer.Enabled := AGifList.Count >= 1;
AdjustFormControls;
if not IsCopy and (PositionList.Count = 0) then
  begin
  AddSectionsToList;
  end;
end;

procedure TSectionList.AdjustFormControls;
var
  I: integer;

  {$ifndef FastRadio}
  function ActiveInList: boolean; {see if active control is a form control}
  var
    Control: TWinControl;
    I: integer;
  begin
  with FormControlList do
    begin
    Result := False;
    Control := Screen.ActiveControl;
    for I := 0 to Count-1 do
      with  TFormControlObj(Items[I]) do
        if FControl = Control then
          begin
          Result := True;
          Break;
          end;
    end;
  end;
  {$endif}

begin
if IsCopy or (FormControlList.Count = 0) then Exit;
with FormControlList do
  {$ifndef FastRadio}
  if not ActiveInList then
    begin  {if none of the formcontrols are active, turn off tabs for those off screen}
    for I := 0 to Count-1 do
      with  TFormControlObj(Items[I]) do
        if not ShowIt and Assigned(FControl) then
          FControl.Hide;   {hides and turns off tabs}
    end
  else
  {$endif}
    for I := 0 to Count-1 do
      with  TFormControlObj(Items[I]) do
        if not ShowIt and Assigned(FControl) then
          begin
          FControl.Show;   {turns on tabs}
          FControl.Left := -4000;  {but it still can't be seen}
          end;
end;

{----------------TSectionList.Draw}
function TSectionList.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                          Y, XRef, YRef :integer): integer;
var
  OldPal: HPalette;
  I: integer;
begin
PageBottom := ARect.Bottom + YOff;
PageShortened := False;
FirstPageItem := True;     

if Assigned(Timer) then Timer.Enabled := False;
for I := 0 to AGifList.Count-1 do
  with TGifImage(AGifList.Items[I]) do
    begin
    ShowIt := False;
    end;
if (ColorBits <= 8) then   
  begin
  OldPal := SelectPalette(Canvas.Handle, ThePalette, True);
  RealizePalette(Canvas.Handle);
  end
else OldPal := 0;
DrawList.Clear;
try
  Result := inherited Draw(Canvas, ARect, ClipWidth, X, Y, XRef, YRef);
  DrawList.DrawImages;
finally
  if OldPal <> 0 then     
    SelectPalette(Canvas.Handle, OldPal, True);
  end;
if YOffChange then
  begin
  AdjustFormControls;
  {Hide all TPanelObj's that aren't displayed}
  for I := 0 to PanelList.Count-1 do
    with TPanelObj(PanelList[I]) do
      if not ShowIt then
        Panel.Hide;
  YOffChange := False;
  end;
if Assigned(Timer) then Timer.Enabled := AGifList.Count >= 1;
end;

procedure TSectionList.SetFonts(const Name, PreName: String; ASize: integer;
          AColor, AHotSpot, AVisitedColor, AActiveColor, ABackground: TColor;
          LnksActive: boolean; LinkUnderLine: boolean; ACharSet: TFontCharSet;
          MarginHeight, MarginWidth: integer);
begin
Styles.Initialize(Name, PreName, ASize, AColor, AHotspot, AVisitedColor,
         AActiveColor, LinkUnderLine, ACharSet, MarginHeight, MarginWidth);
InitializeFontSizes(ASize);   
PreFontName := PreName;
HotSpotColor := AHotSpot;
LinkVisitedColor := AVisitedColor;
LinkActiveColor := AActiveColor;
LinksActive := LnksActive;
SetBackground(ABackground);
end;

procedure TSectionList.SetBackground(ABackground: TColor);
begin
Background := ABackground;
if Assigned(OnBackGroundChange) then
  OnBackgroundChange(Self);
end;

procedure TSectionList.SetBackgroundBitmap(Name: String; const APrec: PtPositionRec);
begin
BackgroundBitmap := Nil;
BitmapName := Name;
BitmapLoaded := False;
BackgroundPRec := APrec;  
end;

{----------------TSectionList.InsertImage}
procedure TSectionList.InsertImage(const Src: string; Stream: TMemoryStream;
               var Reformat: boolean); 
var
  UName: string;
  I, J: integer;
  Pair: TBitmapItem;
  NonAnimated, Rformat: boolean;
  Image: TPersistent;
  AMask: TBitmap;
  Tr, Transparent: Transparency;
  Obj: TObject;
  Tmp: TGifImage;
begin
Image := Nil;  AMask := Nil;
Reformat := False;
UName := Trim(Uppercase(Src));
I := BitmapList.IndexOf(UName);  {first see if the bitmap is already loaded}
J := MissingImages.IndexOf(UName); {see if it's in missing image list}
if (I = -1) and (J >= 0) then
  begin
  Transparent := NotTransp;
  if Assigned(Stream) and (Stream.Memory <> Nil) and (Stream.Size >= 1) then
    begin
    NonAnimated := True;
    if KindOfImage(Stream.Memory) in [GIF, Gif89] then
      Image := CreateAGifFromStream(NonAnimated, Stream);
    if Assigned(Image) then
      begin
      if NonAnimated then
        begin     {else already have animated GIF}
        Tmp := TGifImage(Image);
        Image := TBitmap.Create;
        Image.Assign(Tmp.MaskedBitmap);
        if Tmp.IsTransparent then
          begin
          AMask := TBitmap.Create;
          AMask.Assign(Tmp.Mask);
          Transparent := TGif;
          end;
        Tmp.Free;
        end;
      end
    else
      Image := GetImageAndMaskFromStream(Stream, Transparent, AMask);
    end;
  if Assigned(Image) then  {put in Cache}
    try
      if Assigned(AMask) then Tr := Transparent
        else Tr := NotTransp;
      Pair := TBitmapItem.Create(Image, AMask, Tr);
      try
        BitmapList.AddObject(UName, Pair);  {put new bitmap in list}
        BitmapList.DecUsage(UName);    {this does not count as being used yet}
      except
        Pair.Mask := Nil;
        Pair.MImage:= Nil;
        Pair.Free;
      end;
    except  {accept inability to create}
    end;
  end;
if (I >= 0) or Assigned(Image) then  {a valid image in the Cache}
  begin
  while J >= 0 do
    begin
    Obj := MissingImages.Objects[J];
    if (Obj = Self) and not IsCopy then
      BitmapLoaded := False  {the background image, set to load}
    else if (Obj is TImageObj) then
      begin
      TImageObj(Obj).InsertImage(UName, Rformat);
      Reformat := Reformat or Rformat;
      end;
    MissingImages.Delete(J);
    J := MissingImages.IndexOf(UName);
    end;
  end;
end;

{----------------TSectionList.GetTheBitmap}
function TSectionList.GetTheBitmap(const BMName: String; var Transparent: Transparency;
         var AMask: TBitmap; var FromCache, Delay: boolean): TPersistent;
{Note: bitmaps and Mask returned by this routine are on "loan".  Do not destroy
 them}
{Transparent may be set to NotTransp or LLCorner on entry but may discover it's
 TGif here}

{$ifdef ShareWare}
const
  OneTime: boolean = False;
{$endif}

var
  UName: string;
  Ext: string[10];
  I: integer;
  Pair: TBitmapItem;
  Tr: Transparency;
  NonAnimated: boolean;
  Stream: TMemoryStream;
  Color: TColor;
  Tmp: TGifImage;

  function LoadImageFromFile(const FName: string; var AMask: TBitmap;
                var Transparent: Transparency): TPersistent;
  var
    Tmp: TGifImage;
  begin   {look for the image file}
  Result := Nil;
  AMask := Nil;
  Ext := ExtractFileExt(FName);
  NonAnimated := True;
  if (CompareText(Ext, '.gif')=0) then    
    Result := CreateAGif(FName, NonAnimated);
    if Assigned(Result) then
      begin
      if NonAnimated then
        begin     {else already have animated GIF}
        Tmp := TGifImage(Result);
        Result := TBitmap.Create;
        Result.Assign(Tmp.MaskedBitmap);
        if Tmp.IsTransparent then
          begin
          AMask := TBitmap.Create;
          AMask.Assign(Tmp.Mask);
          Transparent := TGif;
          end
        else if Transparent = LLCorner then
          AMask := GetImageMask(TBitmap(Result), False, 0);
        Tmp.Free;
        end;
      end
    else
      Result := GetImageAndMaskFromFile(BMName, Transparent, AMask);
  end;

begin
{$ifdef ShareWare}
{$Include DemoVers.inc}
{$endif}
AMask := Nil;
Delay := False;
FromCache := False;
if BMName <> '' then
  begin
  UName := Trim(Uppercase(BMName));
  I := BitmapList.IndexOf(UName);  {first see if the bitmap is already loaded}
  if I > -1 then
    begin         {yes, handle the case where the image is already loaded}
    Result := BitmapList.GetImage(I);
    FromCache := True;
    if Result is TBitmap then
      with  BitmapList.Objects[I] as TBitmapItem do
        begin
        if Transp = TGif then
          Transparent := TGif   {it's a transparent GIF}
        else if Transp = Tpng then
          Transparent := TPng
        else if Transparent = LLCorner then
          begin
          if not Assigned (Mask) then  {1st bitmap may not have been marked transp}
            Mask := GetImageMask(TBitmap(MImage), False, 0);
          if Assigned(Mask) then Transp := LLCorner;
          end;
        AMask := Mask;
        end;
    Exit;
    end;

  {The image is not loaded yet, need to get it}
  Result := Nil;
  if Assigned(GetBitmap) then
    begin {the OnBitmapRequest event}
    Color := -1;
    GetBitmap(TheOwner, BMName, TBitmap(Result), Color);
    if Assigned(Result) then
      if Color <> -1 then
        begin
        AMask := GetImageMask(TBitmap(Result), True, Color);
        Transparent := TGif;
        end
      else if (Transparent = LLCorner) then
        AMask := GetImageMask(TBitmap(Result), False, 0);
    end
  else if Assigned(GetImage) then
    begin    {the OnImageRequest}
    Stream := Nil;
    GetImage(TheOwner, BMName, Stream);
    if Stream = WaitStream then
      Delay := True
    else if not Assigned(Stream) then
      Result := LoadImageFromFile(ThtmlViewer(TheOwner).HTMLExpandFilename(BMName), AMask, Transparent)  
    else if Assigned(Stream) and (Stream.Memory <> Nil) and (Stream.Size >= 1) then
      begin
      NonAnimated := True;
      if KindOfImage(Stream.Memory) in [GIF, Gif89] then
        Result := CreateAGifFromStream(NonAnimated, Stream);
      if Assigned(Result) then
        begin
        if NonAnimated then
          begin     {else already have animated GIF}
          Tmp := TGifImage(Result);
          Result := TBitmap.Create;
          Result.Assign(Tmp.MaskedBitmap);
          if Tmp.IsTransparent then
            begin
            AMask := TBitmap.Create;
            AMask.Assign(Tmp.Mask);
            Transparent := TGif;
            end
          else if Transparent = LLCorner then
            AMask := GetImageMask(TBitmap(Result), False, 0);
          Tmp.Free;
          end;
        end
      else
        Result := GetImageAndMaskFromStream(Stream, Transparent, AMask);
      end;
    end
  else
    Result := LoadImageFromFile(BMName, AMask, Transparent);  
  if Assigned(Result) then  {put in Image List for use later also}
    try     
      if Assigned(AMask) then Tr := Transparent
        else Tr := NotTransp;
      Pair := TBitmapItem.Create(Result, AMask, Tr);
      try
        BitmapList.AddObject(UName, Pair);  {put new bitmap in list}
      except
        Pair.Mask := Nil;
        Pair.MImage:= Nil;
        Pair.Free;
      end;
    except  {accept inability to create}
    end;
  end
else Result := Nil;
end;

{----------------TSectionList.FindSectionAtPosition}
function TSectionList.FindSectionAtPosition(Pos: integer;
             var TopPos: integer; var Index: integer): TSectionBase;
var
  I: integer;
begin
with PositionList do
  for I := Count-1 downto 0 do
    if TSectionBase(Items[I]).YPosition <= Pos then
      begin
      Result := TSectionBase(Items[I]);
      TopPos := Result.YPosition;
      Index := I;
      Exit;
      end;
Result := Nil;
end;

function TSectionList.GetBackgroundBitmap: TBitmap;
var
  Mask: TBitmap;
  Dummy1: Transparency;
  TmpResult: TPersistent;
  FromCache, Delay: boolean;
  Rslt: string;
begin
if ShowImages and not BitmapLoaded and (BitmapName <> '') then
  begin
  if not Assigned(BackgroundBitmap) then
    begin
    Dummy1 := NotTransp;
    if not Assigned(GetBitmap) and not Assigned(GetImage) then
      BitmapName := (TheOwner as ThtmlViewer).HTMLExpandFilename(BitmapName)
    else if Assigned(ExpandName) then
      begin
      ExpandName(TheOwner, BitmapName, Rslt);
      BitmapName := Rslt;
      end;
    TmpResult := GetTheBitmap(BitmapName, Dummy1, Mask, FromCache, Delay); {might be Nil}
    if TmpResult is TBitmap then
      begin
      BackgroundBitmap := TBitmap(TmpResult);
      BackgroundMask := Mask;
      end
    else if TmpResult is TGifImage then
      begin
      BackgroundBitmap := TGifImage(TmpResult).MaskedBitmap;
      BackgroundMask := TGifImage(TmpResult).Mask;
      end
    else
      begin
      BackgroundBitmap := Nil;
      if Delay then
        MissingImages.AddObject(BitmapName, Self);
      end;
    BitmapLoaded := True;
    end;
  end;
Result := BackgroundBitmap;
end;

{----------------TSectionList.GetFormcontrolData}
function TSectionList.GetFormcontrolData: TFreeList;     
var
  I: integer;
begin
if htmlFormList.Count > 0 then
  begin
  Result := TFreeList.Create;
  for I := 0 to htmlFormList.Count-1 do
    Result.Add(ThtmlForm(htmlFormList[I]).GetFormSubmission);
  end
else Result := Nil;
end;

procedure TSectionList.SetFormcontrolData(T: TFreeList); 
var
  I: integer;
begin
try
  for I := 0 to T.Count-1 do
    if htmlFormList.Count > I then
      ThtmlForm(htmlFormList[I]).SetFormData(TStringList(T[I]));
except end;
end;

{----------------TSectionList.FindDocPos}
function TSectionList.FindDocPos(SourcePos: integer; Prev: boolean): integer;   
begin
Result := inherited FindDocPos(SourcePos, Prev);
if Result < 0 then    {if not found return 1 past last char}  
  Result := Len+1;
end;

{----------------TCellObj.Create}
constructor TCellObj.Create(Master: TSectionList; AVAlign: AlignmentType;
             Attr: TAttributeList; Prop: TProperties);
var
  I, AutoCount: integer;
  Color: TColor;
  BackgroundImage: string;
  Percent: boolean;
  Algn: AlignmentType;  
begin
inherited Create;
Cell := TCell.Create(Master);
ColSpan := 1;
RowSpan := 1;
VAlign := AVAlign;
if Assigned(Attr) then
  for I := 0 to Attr.Count-1 do
    with TAttribute(Attr[I]) do
      case Which of
        ColSpanSy:
          if Value > 1 then ColSpan := Value;
        RowSpanSy:
          if Value > 1 then RowSpan := Value;
        WidthSy:
          if Pos('%', Name) > 0 then
            begin
            if (Value > 0) and (Value <= 100) then
              begin
              WidthAttr := Value*10;
              AsPercent := True;
              end;
            end
          else if (Value > 0) then
            WidthAttr := Value;
        HeightSy: SpecHt := Value;
        BGColorSy:
          Cell.BkGnd := ColorFromString(Name, False, Cell.BkColor);  
        BackgroundSy: BackgroundImage := Name;
        end;

if Assigned(Prop) then
  begin    {Caption does not have Prop}
  if Prop.GetVertAlign(Algn) and  (Algn in [Atop, AMiddle, ABottom]) then 
    Valign := Algn;
  Prop.GetVMarginArray(MargArrayO);
  EmSize := Prop.EmSize;
  ExSize := Prop.ExSize;
  Percent := (VarType(MargArrayO[Width]) = VarString) and (Pos('%', MargArrayO[Width]) > 0);
  ConvMargArray(MargArrayO, 100, 0, EmSize, ExSize, bssNone, AutoCount, MargArray);
  if MargArray[Width] > 0 then
    if Percent then
      begin
      if MargArray[Width] < 100 then
        begin
        AsPercent := True;
        WidthAttr := MargArray[Width] * 10;
        end;
      end
    else
      begin
      WidthAttr := MargArray[Width];
      AsPercent := False;       
      end;
  if MargArray[Height] > 0 then
    SpecHt := MargArray[Height];
  Color := Prop.GetBackgroundColor;
  if Color <> clNone then
    begin
    Cell.BkGnd := True;
    Cell.BkColor := Color;
    end;
  Prop.GetBackgroundImage(BackgroundImage);  {'none' will change string to empty}
  if BackgroundImage <> '' then
    begin
    BGImage := TImageObj.SimpleCreate(Master, BackgroundImage);
    Prop.GetBackgroundPos(EmSize, ExSize, PRec);
    end;

  BorderStyle := Prop.GetBorderStyle;
  {In the following, Padding widths in percent aren't accepted}
  ConvMargArrayForCellPadding(MargArrayO, EmSize, ExSize, MargArray);
  PadTop := MargArray[PaddingTop];
  PadRight := MargArray[PaddingRight];
  PadBottom := MargArray[PaddingBottom];
  PadLeft := MargArray[PaddingLeft];

  if BorderStyle <> bssNone then
    begin
    BrdTop := MargArray[BorderTopWidth];     
    BrdRight := MargArray[BorderRightWidth];
    BrdBottom := MargArray[BorderBottomWidth];
    BrdLeft := MargArray[BorderLeftWidth];
    end;
  Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);   
  end;
end;

constructor TCellObj.CreateCopy(AMasterList: TSectionList; T: TCellObj);
begin
inherited create;
Cell := TCell.CreateCopy(AMasterList, T.Cell);
Move(T.ColSpan, ColSpan, DWord(@Cell)-DWord(@ColSpan));

if AMasterList.PrintTableBackground then
  begin
  Cell.BkGnd := T.Cell.BkGnd;
  Cell.BkColor := T.Cell.BkColor;
  end
else
  Cell.BkGnd := False;
if Assigned(T.BGImage) and AMasterList.PrintTableBackground then
  BGImage := TImageObj.CreateCopy(AMasterList, T.BGImage);
MargArrayO := T.MargArrayO;
MargArray := T.MargArray;     
end;

destructor TCellObj.Destroy;
begin
Cell.Free;
BGImage.Free;
TiledImage.Free;
TiledMask.Free;
FullBG.Free;
inherited Destroy;
end;

{----------------TCellObj.InitializeCell}
procedure TCellObj.InitializeCell(TablePadding: integer; const BkImageName: string;
                                           const APRec: PtPositionRec);
begin
if PadTop < 0 then
  PadTop := TablePadding;
if PadRight < 0 then
  PadRight := TablePadding;
if PadBottom < 0 then
  PadBottom := TablePadding;
if PadLeft < 0 then
  PadLeft := TablePadding;
HzSpace := PadLeft+BrdLeft+BrdRight+PadRight;   
VrSpace := PadTop+BrdTop+BrdBottom+PadBottom;

if (BkImageName <> '') and not Assigned(BGImage) then
  begin
  BGImage := TImageObj.SimpleCreate(Cell.MasterList, BkImageName);
  PRec := APrec;
  end;
end;

{----------------TCellObj.DrawLogic2}
procedure TCellObj.DrawLogic2(Canvas : TCanvas; Y, CellSpacing: integer; var Curs: integer);
var
  Dummy: integer;
  Tmp: integer;
begin
if Cell.Count > 0 then
  begin
  Tmp := Ht - VSize - (VrSpace+CellSpacing);
  case VAlign of
    ATop: YIndent := 0;
    AMiddle: YIndent := Tmp div 2;
    ABottom, ABaseline: YIndent := Tmp;
    end;
  Dummy := 0;  
  Cell.DoLogic(Canvas, Y+PadTop+BrdTop+CellSpacing+YIndent, Wd-(HzSpace+CellSpacing), Dummy,
                       Dummy, Curs);
  end;
if Assigned(BGImage) and Cell.MasterList.ShowImages then   
  begin
  BGImage.DrawLogic(Cell.MasterList, Canvas, Nil, 100);
  if BGImage.Image = ErrorBitmap then
    begin
    BGImage.Free;
    BGImage := Nil;
    end
  else
    begin
    BGImage.ImageKnown := True;  {won't need reformat on InsertImage}
    NeedDoImageStuff := True;
    end;
  end;
end;

{----------------TCellObj.Draw}
procedure TCellObj.Draw(Canvas: TCanvas; const ARect: TRect; X, Y, CellSpacing: integer;
           Border: boolean; Light, Dark: TColor; Rgn: THandle);
var
  YO: integer;
  ARgn: THandle;
  AddOn: integer;
  BL, BT, BR, BB, PL, PT, PR, PB: integer;
  BorderRegion, PaddingRegion: THandle;
  Brush: TBrush;
  ImgOK: boolean;
  IT, IH, FT: integer;

  procedure InitFullBg(W, H: integer);
  begin
  if not Assigned(FullBG) then
    begin
    FullBG := TBitmap.Create;
    if Cell.MasterList.IsCopy then
      begin
      FullBG.HandleType := bmDIB;
      if ColorBits <= 8 then
        FullBG.Palette := CopyPalette(ThePalette);
      end;
    end;
  FullBG.Height := IntMax(H, 2);
  FullBG.Width := IntMax(W, 2);       
  end;

begin
YO := Y - Cell.MasterList.YOff;

BL := X + CellSpacing;   {Border left and right}
BR := X + Wd;
PL := BL + BrdLeft; {Padding left and right}
PR := BR - BrdRight;

BT := Y - Cell.MasterList.YOff + Cellspacing;   {Border Top and Bottom}
BB := BT + Ht - CellSpacing;
PT := BT + BrdTop; {Padding Top and Bottom}
PB := BB - BrdBottom;

IT := IntMax(0, Arect.Top-2-PT);
FT := IntMax(PT, ARect.Top-2);         {top of area drawn, screen coordinates}
IH := IntMin(PB-FT, Arect.Bottom-FT);  {height of area actually drawn}

if not (BT <= ARect.Bottom) and (BB >= ARect.Top) then
  Exit;

if BorderStyle <> bssNone then
  {Region defined by outside of Border}
  BorderRegion := CreateRectRgn(BL, BT, BR, BB)
else BorderRegion := 0;
PaddingRegion := 0;

try
  if NeedDoImageStuff and (BGImage.Image <> DefBitmap) then
    begin
    DoImageStuff(Canvas, Wd-CellSpacing, Ht-CellSpacing,
         BGImage, PRec, TiledImage, TiledMask, NoMask);
    if Cell.MasterList.IsCopy then
      TiledImage.HandleType := bmDIB;
    NeedDoImageStuff := False;
    end;

  ImgOK := Not NeedDoImageStuff and Assigned(BGImage) and (BGImage.Bitmap <> DefBitmap)
             and Cell.MasterList.ShowImages;

  if Cell.BkGnd then
    begin
    Canvas.Brush.Color := Cell.BkColor or $2000000;

    Canvas.Brush.Style := bsSolid;
    if Cell.MasterList.IsCopy and ImgOK then
      begin
      InitFullBG(PR-PL, IH);
      FullBG.Canvas.Brush.Color := Cell.BkColor or $2000000;
      FullBG.Canvas.Brush.Style := bsSolid;
      FullBG.Canvas.FillRect(Rect(0, 0, PR-PL, IH));
      end
    else if BorderRegion = 0 then
      Canvas.FillRect(Rect(PL, FT, PR, FT+IH))
    else
      Canvas.FillRect(Rect(PL, FT, PR+1, FT+IH+1));  
    end;
  if ImgOK then
    begin
    if not Cell.MasterList.IsCopy then
      if NoMask then
        BitBlt(Canvas.Handle, PL, FT, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SrcCopy)
      else
        begin
        InitFullBG(PR-PL, IH);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, Canvas.Handle, PL, FT, SrcCopy);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SrcInvert);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
        BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SRCPaint);
        BitBlt(Canvas.Handle, PL, FT, PR-PL, IH, FullBG.Canvas.Handle, 0, 0, SRCCOPY);
        end
    else if NoMask then
      PrintBitmap(Canvas, PL, FT, PR-PL, IH, TiledImage.Handle)
    else if Cell.BkGnd then
      begin
      InitFullBG(PR-PL, IH);
      BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SrcInvert);
      BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledMask.Canvas.Handle, 0, IT, SRCAND);
      BitBlt(FullBG.Canvas.Handle, 0, 0, PR-PL, IH, TiledImage.Canvas.Handle, 0, IT, SRCPaint);
      PrintBitmap(Canvas, PL, FT, PR-PL, IH, FullBG.Handle);
      end
    else
      PrintTransparentBitmap3(Canvas, PL, FT, PR-PL, IH, TiledImage, TiledMask, IT, IH);
    end;
except
  end;

try
  Cell.Draw(Canvas, ARect, Wd-HzSpace-CellSpacing, X+PadLeft+BrdLeft+CellSpacing,
                Y+PadTop+BrdTop+YIndent, 1000, 1000);  
  Cell.DrawYY := Y;
  if BorderRegion <> 0 then
    begin
    PaddingRegion := CreateRectRgn(PL, PT, PR, PB);
    CombineRgn(BorderRegion, BorderRegion, PaddingRegion, RGN_DIFF);
    Brush := TBrush.Create;
    if MargArray[BorderColor] <> clNone then
      Brush.Color := MargArray[BorderColor]
    else Brush.Color := clBlack;
    Brush.Style := bsSolid;
    FillRgn(Canvas.Handle, BorderRegion, Brush.Handle);
    Brush.Free;
    end
  else if Border and (Cell.Count > 0) then   
    if (Light = clBtnHighLight) and (Dark = clBtnShadow) then
      RaisedRect(Cell.MasterList, Canvas, X+CellSpacing-1, YO+CellSpacing-1,
                                  X+Wd, YO+Ht, False)
    else
      RaisedRectColor(Cell.MasterList, Canvas, X+CellSpacing-1, YO+CellSpacing-1,
                                  X+Wd, YO+Ht, Light, Dark, False);
except
  end;

if BorderRegion <> 0 then
  DeleteObject(BorderRegion);
if PaddingRegion <> 0 then
  DeleteObject(PaddingRegion);

if Rgn <> 0 then
  begin
  if Border then
    AddOn := 1
  else
    AddOn := 0;
  ARgn := CreateRectRgn(X+CellSpacing-AddOn, IntMax(YO+CellSpacing-AddOn, TopLim),
                        X+Wd+AddOn, IntMin(YO+Ht+AddOn, BotLim));
  CombineRgn(Rgn, Rgn, ARgn, RGN_DIFF);
  DeleteObject(ARgn);
  end;
end;

{----------------TSectionBase.Create}
constructor TSectionBase.Create(AMasterList: TSectionList);
begin
inherited Create;
ParentSectionList := AMasterList;
ContentTop := 999999999;  {large number in case it has Display: none; }
end;

constructor TSectionBase.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
begin
inherited Create;
ParentSectionList := AMasterList;
SectionHeight := T.SectionHeight;
ZIndex := T.ZIndex;
end;

procedure TSectionBase.CopyToClipboard;
begin
end;

function TSectionBase.GetYPosition: integer;
begin
Result := ContentTop;
end;

{----------------TSectionBase.DrawLogic}
function TSectionBase.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
begin
StartCurs := Curs;
Result := SectionHeight;
DrawHeight := SectionHeight;
MaxWidth := IMgr.Width;
ContentTop := Y;
DrawTop := Y;
YDraw := Y; 
ContentBot := Y+SectionHeight;
DrawBot := Y+DrawHeight;
end;

function TSectionBase.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  Y: integer;
begin
Y := YDraw;
Result := Y+SectionHeight;
end;

function TSectionBase.GetURL(Canvas: TCanvas; X: integer; Y: integer;
     var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
     var ATitle: string): guResultType;
begin
Result := [];
end;

function TSectionBase.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
begin
Result := False;
end;

function TSectionBase.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer;
begin
Result := -1;
end;

function TSectionBase.FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
begin
Result := -1;
end;

function TSectionBase.FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
begin
Result := -1;
end;

function TSectionBase.FindSourcePos(DocPos: integer): integer;
begin
Result := -1;
end;

function TSectionBase.FindDocPos(SourcePos: integer; Prev: boolean): integer; 
begin
Result := -1;
end;

function TSectionBase.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
begin
Result := False;
end;

function TSectionBase.GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
begin
Result := False;
end;

procedure TSectionBase.SetParent(List: TSectionList);
begin
ParentSectionList := List;
end;

procedure TSectionBase.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
begin
Min := 0;  Max := 0;
end;

procedure TSectionBase.AddSectionsToList;
begin
ParentSectionList.PositionList.Add(Self);
end;

{----------------TCellList.Create}
constructor TCellList.Create(Attr: TAttributeList; Prop: TProperties);
var
  I: integer;
  Color: TColor;
begin
inherited Create;
if Assigned(Attr) then
  for I := 0 to Attr.Count-1 do
    with TAttribute(Attr[I]) do
      case Which of
      BGColorSy:
        BkGnd := ColorFromString(Name, False, BkColor);
      BackgroundSy:
        BkImage := Name;
      HeightSy:     
        SpecRowHeight := Value;
      end;
if Assigned(Prop) then
  begin
  Color := Prop.GetBackgroundColor;
  if Color <> clNone then
    begin
    BkGnd := True;
    BkColor := Color;
    end;
  Prop.GetBackgroundImage(BkImage);  {'none' will change string to empty}
  if BkImage <> '' then
    Prop.GetBackgroundPos(Prop.EmSize, Prop.ExSize, APRec);
  Prop.GetPageBreaks(BreakBefore, BreakAfter, KeepIntact);   
  end;
end;

{----------------TCellList.CreateCopy}
constructor TCellList.CreateCopy(AMasterList: TSectionList; T: TCellList);
var
  I: integer;
begin
inherited create;
BreakBefore := T.BreakBefore;  
BreakAfter := T.BreakAfter;
KeepIntact := T.KeepIntact;   
for I := 0 to T.Count-1 do
  if Assigned(T.Items[I]) then
    Add(TCellObj.CreateCopy(AMasterList, TCellObj(T.Items[I])))
  else Add(Nil);
end;

procedure TCellList.Add(CellObj: TCellObj);   
begin
inherited Add(CellObj);
if Assigned(CellObj) then
  begin
  BreakBefore := BreakBefore or CellObj.BreakBefore;
  BreakAfter := BreakAfter or CellObj.BreakAfter;
  KeepIntact := KeepIntact or CellObj.KeepIntact ;
  if SpecRowHeight > 0 then          
    CellObj.SpecHt := IntMax(SpecRowHeight, CellObj.SpecHt);
  end;
end;

{----------------TCellList.InitializeRow}
procedure TCellList.InitializeRow;
var
  I: integer;
begin
if BkGnd then
  for I := 0 to Count-1 do
    with TCellObj(Items[I]).Cell do     
      if not BkGnd then
        begin
        BkGnd := True;
        BkColor := Self.BkColor;
        end;
end;

{----------------TCellList.DrawLogic1}
function TCellList.DrawLogic1(Canvas : TCanvas; const Widths : IntArray; Span,
           CellSpacing: integer; var More: boolean): integer;
{Find vertical size of each cell, Row height of this row.  But final Y position
 is not known at this time.}
var
  I, J, Dummy: integer;
  DummyCurs, H, TmpSize: integer;
  CellObj: TCellObj;
begin
H := 0;
DummyCurs := 0;
More := False;
for I := 0 to Count-1 do
  begin
  CellObj := TCellObj(Items[I]);
  if Assigned(CellObj) then     
    with CellObj do
      if ColSpan > 0 then  {skip the dummy cells}
        begin
        Wd := 0;
        for J := I to ColSpan+I-1 do
          Inc(Wd, Widths[J]);   {accumulate column widths}
        if Span = RowSpan then
          begin
          Dummy := 0;
          VSize := Cell.DoLogic(Canvas, 0, Wd-HzSpace-CellSpacing, Dummy,
                   Dummy, DummyCurs);
          if VSize + VrSpace > SpecHt then
            TmpSize := VSize + VrSpace
          else TmpSize := SpecHt;
          if TmpSize > H  then H := TmpSize;
          end
        else if RowSpan > Span then More := True;
        end;
  end;
Result := H;
end;

{----------------TCellList.DrawLogic2}
procedure TCellList.DrawLogic2(Canvas : TCanvas; Y: integer; 
          CellSpacing: integer; var Curs: integer);
{Calc Y indents. Set up Y positions of all cells.}
var
  I: integer;
  CellObj: TCellObj;
begin
for I := 0 to Count-1 do
  begin
  CellObj := TCellObj(Items[I]);
  if Assigned(CellObj) then
    CellObj.DrawLogic2(Canvas, Y, CellSpacing, Curs);
  end;
end;

{----------------TCellList.Draw}
function TCellList.Draw(Canvas: TCanvas; MasterList: TSectionList; const ARect: TRect;
     const Widths : IntArray; X: integer; Y, YOffset: integer;
     CellSpacing : integer; Border: boolean; Light, Dark: TColor; Rgn: THandle;
     MyRow: integer) : integer;  
var
  I, Spacing: integer;
  YO: integer;
  CellObj: TCellObj;
begin
YO := Y - YOffset;
Result := RowHeight+Y;
Spacing := CellSpacing div 2;

with MasterList do   {check CSS page break properties}    
  if Printing then
    if BreakBefore then
      begin
      if YO > ARect.Top then  {page-break-before}
        begin
        if Y+Spacing < PageBottom then
          begin
          PageShortened := True;
          PageBottom := Y+Spacing;    
          end;
        Exit;
        end;
      end
    else if KeepIntact then
      begin
      {Try to fit this RowSpan on a page by itself}
      if (YO > ARect.Top) and (Y+RowSpanHeight > PageBottom) and
            (RowSpanHeight < ARect.Bottom - ARect.Top) then
        begin
        if Y+Spacing < PageBottom then
          begin
          PageShortened := True;
          PageBottom := Y+Spacing;
          end;
        Exit;
        end
      else if  (YO > ARect.Top) and (Y+RowHeight > PageBottom) and
            (RowHeight < ARect.Bottom - ARect.Top) then
        begin
        if Y+Spacing < PageBottom then
          begin
          PageShortened := True;
          PageBottom := Y+Spacing;
          end;
        Exit;
        end;
      end
    else if BreakAfter then
      if ARect.Top + YOff < Result then    {page-break-after}
        if Result+Spacing < PageBottom then
          begin
          PageShortened := True;
          PageBottom := Result+Spacing;    
          end;

{Regardless of the CSS break properties, if we're printing and
 we're 2/3 down page and this row won't fit on this page but will fit on the
 next page, then don't print now}
with MasterList do
  if Printing and
        (YO > ARect.Top + ((ARect.Bottom - ARect.Top)*2) div 3) and
        (Y + RowSpanHeight > PageBottom) and
        (RowSpanHeight < ARect.Bottom - ARect.Top) then
    begin
    if MyRow = 0 then Spacing := 0 else Spacing := CellSpacing div 2;
    if Y + Spacing < PageBottom then
      begin
      PageShortened := True;
      PageBottom := Y + Spacing;
      end;
    Exit;
    end;

if (YO+RowSpanHeight >= ARect.Top) and (YO < ARect.Bottom) and
       (not MasterList.Printing or (Y < MasterList.PageBottom)) then
  for I := 0 to Count-1 do
    begin
    CellObj := TCellObj(Items[I]);
    if Assigned(CellObj) then
      CellObj.Draw(Canvas, ARect, X, Y, CellSpacing, Border, Light, Dark, Rgn);
    X := X + Widths[I];
    end;
end;

{----------------ThtmlTable.Create}
constructor ThtmlTable.Create(Master: TSectionList;Attr: TAttributeList;
            Prop: TProperties; ACell: TCellBasic);
var
  I: integer;
begin
inherited Create(Master);
MyCell := ACell;
Rows := TFreeList.Create;
Caption := TCellObj.Create(Master, ATop, Nil, Nil);
TopCaption := True;
CellPadding := 1;
CellSpacing := 2;
BorderColorLight := clBtnHighLight;   
BorderColorDark := clBtnShadow;   
for I := 0 to Attr.Count-1 do
  with TAttribute(Attr[I]) do
    case Which of
      BorderSy:
        Border := Value > 0;   {Border=0 is no border}
      CellSpacingSy:
        if Value >= 0 then CellSpacing := IntMin(Value, 40);
      CellPaddingSy:
        if Value >= 0 then CellPadding := IntMin(Value, 50);
      BorderColorSy:
        BdrOn := ColorFromString(Name, False, BdrColor);
      BorderColorLightSy:
        ColorFromString(Name, False, BorderColorLight);  
      BorderColorDarkSy:
        ColorFromString(Name, False, BorderColorDark);  
      end;
if Border then
  begin           
  Inc(CellSpacing, 2);   {includes border lines}
  CellSpacing := IntMax(1, CellSpacing);
  end;
if BdrOn then    
  begin
  if BorderColorLight = clBtnHighLight then
    BorderColorLight := BdrColor;
  if BorderColorDark = clBtnShadow then
    BorderColorDark := BdrColor;
  end;
end;

{----------------ThtmlTable.CreateCopy}
constructor ThtmlTable.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  I: integer;
begin
inherited CreateCopy(AMasterList, T);
Rows := TFreeList.Create;
for I := 0 to ThtmlTable(T).Rows.Count-1 do
  Rows.Add(TCellList.CreateCopy(AMasterList, TCellList(ThtmlTable(T).Rows.Items[I])));

Caption := TCellObj.CreateCopy(AMasterList, ThtmlTable(T).Caption);
Move((T as ThtmlTable).ListsProcessed, ListsProcessed,
        DWord(@EndList)-DWord(@ListsProcessed));

if AMasterList.PrintTableBackground then         
  begin
  BkGnd := ThtmlTable(T).BkGnd;
  BdrOn := ThtmlTable(T).BdrOn;
  BkColor := ThtmlTable(T).BkColor;
  BdrColor := ThtmlTable(T).BdrColor;
  end
else
  begin
  BkGnd := False;
  BdrOn := False;
  end;
end;

{----------------ThtmlTable.Destroy}
destructor ThtmlTable.Destroy;
begin
Rows.Free;
Caption.Free;
inherited Destroy;
end;

{----------------ThtmlTable.DoColumns}
procedure ThtmlTable.DoColumns(Width: integer; AsPercent: boolean;
              VAlign: AlignmentType; const Align: string);   
{add the <col> info to the ColInfo list}
var
  Col: TColObj;
begin
Col := TColObj.Create;
with Col do
  begin
    ColWidth := Width;
    ColAsPercent := AsPercent;
    colVAlign := VAlign;
    colAlign := Align;
  end;
if not Assigned(colInfo) then
  colInfo := TFreeList.Create;
ColInfo.Add(Col);
end;

{----------------ThtmlTable.AddDummyCells}
procedure ThtmlTable.AddDummyCells;
var
  Cl, Rw, K, RowCount: integer;
  AnyAbsolute: boolean;
  Row: TCellList;
  CellObj: TCellObj;

  function DummyCell(RSpan: integer): TCellObj;
  begin
  Result := TCellObj.Create(ParentSectionList, ATop, Nil, Nil);
  Result.ColSpan := 0;
  Result.RowSpan := RSpan;
  end;

Begin
RowCount := Rows.Count;
if not ListsProcessed then
  begin   {put dummy cells in rows to make up for ColSpan > 1}
  NumCols := 0;
  AnyAbsolute := False;
  for Rw := 0 to RowCount-1 do
    begin
    with TCellList(Rows[Rw]) do
      begin
      InitializeRow;
      for Cl := Count-1 downto 0 do
        with TCellObj(Items[Cl]) do
          begin
          InitializeCell(CellPadding, BkImage, APRec);
          if WidthAttr > 0 then
            begin
            if not AsPercent then AnyAbsolute := True;
            end;
          if Self.BkGnd and not Cell.BkGnd then    {transfer bgcolor to cells if no Table image}
            begin
            Cell.BkGnd := True;
            Cell.BkColor := Self.BkColor;
            end;
          for K := 1 to ColSpan-1 do
            if RowSpan > 1 then
              TCellList(Rows[Rw]).Insert(Cl+K, DummyCell(RowSpan)) {these could be
                Nil also except they're needed for expansion in the next section}
            else
              TCellList(Rows[Rw]).Insert(Cl+K, DummyCell(1));  
          end;
      end;
    NumCols := IntMax(NumCols, TCellList(Rows[Rw]).Count);  {temporary # cols}
    end;

  {Absolute calc only if  some absolute widths entered}
  UseAbsolute := AnyAbsolute;  

  {put dummy cells in cols to make up for RowSpan > 1}
  for Cl := 0 to NumCols-1 do
    for Rw := 0 to RowCount-1 do
      with TCellList(Rows[Rw]) do
        if Count > Cl then
          if Assigned(Items[Cl]) then
            with TCellObj(Items[Cl]) do
              begin
              RowSpan := IntMin(RowSpan, RowCount-Rw);  {practical limit}
              if RowSpan > 1 then
                for K := Rw+1 to Rw+RowSpan-1 do
                  begin  {insert dummy cells in following rows if RowSpan > 1}
                  while TCellList(Rows[K]).Count < Cl do {add padding if row is short}
                     TCellList(Rows[K]).Add(DummyCell(0));
                  TCellList(Rows[K]).Insert(Cl, DummyCell(0));
                  end;
              end;

  NumCols := 0;  {find the number of columns}
  for Rw := 0 to RowCount-1 do
    begin
    NumCols := IntMax(NumCols, TCellList(Rows[Rw]).Count);
    end;
  if NumCols > MaxCols then
    Raise EProcessError.Create('Table has too many Columns');

  {add the width info from the <col> tags to the cells}
  if Assigned(colInfo) then
    begin
    AnyAbsolute := False;
    for Rw := 0 to RowCount-1 do
      begin
      Row := TCellList(Rows[Rw]);
      for Cl := 0 to IntMin(Row.Count-1, NumCols-1) do     
        begin
        CellObj := TCellObj(Row[Cl]);
        with CellObj do
          begin
          if Cl < colInfo.Count then
            with TColObj(colInfo[Cl]) do
              begin
              if colWidth > 0 then
                begin
                WidthAttr := colWidth;
                AsPercent := colAsPercent;
                end;
              end;
          if not AsPercent then
            AnyAbsolute := True;
          end;
        end;
      end;
    UseAbsolute := AnyAbsolute;
    FreeAndNil(colInfo);    {no longer needed}
    end;

  ListsProcessed := True;
  end;    {if not ListsProcessed}
end;

{----------------ThtmlTable.GetMinMaxAbs}
procedure ThtmlTable.GetMinMaxAbs(Canvas: TCanvas; var TotalMinWidth,
     TotalMaxWidth: integer; var MinWidths, MaxWidths: IntArray);
var
  I, J, Min, Max, N, Span, Addon, D: integer;
  More: boolean;
  CellObj: TCellObj;

Begin
FillChar(MinWidths, Sizeof(MinWidths), 0);
FillChar(MaxWidths, Sizeof(MaxWidths), 0);
Span := 1;
More := True;
while More do
  begin
  More := False;
  for J := 0 to Rows.Count-1 do
    with TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then
          begin
          CellObj := TCellObj(Items[I]);  
          with CellObj do
            begin
            More := More or (CellObj.ColSpan > Span); {set if need another iteration}
            if ColSpan = Span then
              begin
              Cell.MinMaxWidth(Canvas, Min, Max);
              Addon := CellSpacing + CellObj.HzSpace;
              Inc(Min, Addon);
              Inc(Max, Addon);
              if Span = 1 then
                begin
                if not AsPercent and (CellObj.WidthAttr > 0) then
                  begin
                  Max := IntMax(Min, WidthAttr+Addon);
                  end;
                MinWidths[I] := Intmax(MinWidths[I], Min);
                MaxWidths[I] := Intmax(MaxWidths[I], Max);
                end
              else
                begin
                TotalMinWidth := 0;  TotalMaxWidth := 0;
                for N := I to I+ColSpan-1 do
                  begin   {find the current totals for the span}
                  Inc(TotalMaxWidth, MaxWidths[N]);
                  Inc(TotalMinWidth, MinWidths[N]);
                  end;
                if not AsPercent and (WidthAttr > 0) then
                  begin
                  Min := IntMax(Min, WidthAttr{+Cellspacing}); 
                  Max := IntMax(Min, WidthAttr{+Cellspacing});
                  end;
                if (TotalMinWidth < Min) then
                  if TotalMinWidth > 0 then
                    begin
                    D := Min - TotalMinWidth;
                    for N := I to I+ColSpan-1 do  {increase the sub widths to match the span}
                      MinWidths[N] := MinWidths[N]+MulDiv(MinWidths[N], D, TotalMinWidth);
                    end
                  else MinWidths[I] := Min;  {this for multiple empty cols}
                if (TotalMaxWidth < Max) then
                  if TotalMaxWidth > 0 then
                    begin     {increase the sub widths to match the span}
                    D := Max - TotalMaxWidth;
                    for N := I to I+ColSpan-1 do  {increase the sub widths to match the span}
                      MaxWidths[N] := MaxWidths[N]+MulDiv(MaxWidths[N], D, TotalMaxWidth);
                    end
                  else MaxWidths[I] := Max;
                end;
              end;
            end;
          end;
      end;
  Inc(Span);
  end;

{Find the total min and max width}
TotalMaxWidth := 0;  TotalMinWidth := 0;
for I := 0 to NumCols-1 do
  begin
  Inc(TotalMaxWidth, MaxWidths[I]);
  Inc(TotalMinWidth, MinWidths[I]);
  end;

end;

{----------------ThtmlTable.GetWidthsAbs}
procedure ThtmlTable.GetWidthsAbs(Canvas: TCanvas; TablWidth: integer;
            Specified: boolean; var MinWidths, MaxWidths: IntArray);
var
  N, D, W, dd, TotalMinWidth, TotalMaxWidth: integer;

Begin
GetMinMaxAbs(Canvas, TotalMinWidth, TotalMaxWidth, MinWidths, MaxWidths);

if TotalMinWidth > TablWidth then  {use the minimum column widths, table will expand}
  Move(MinWidths, Widths, Sizeof(MinWidths))
else if (TotalMaxWidth <= TablWidth) and not Specified then  
  {use the max column widths, table will be smaller}
  Move(MaxWidths, Widths, Sizeof(MaxWidths))
else  {make table fit}
  begin
  D := TotalMaxWidth - TotalMinWidth;
  W := TablWidth - TotalMinWidth;
  if D > 0 then  {expand only those columns with some slop in them}
    begin
    for  N := 0 to NumCols-1 do
      begin
      dd := MaxWidths[N] - MinWidths[N];  {some dd's may be 0}
      Widths[N] := MinWidths[N] + MulDiv(dd, W, D);
      end;
    end
  else  {no adjustable columns, will have to expand them all}
    for N := 0 to NumCols-1 do
      Widths[N] := MinWidths[N] + MulDiv(MinWidths[N], W, TotalMinWidth);
  end;
end;

{----------------ThtmlTable.GetWidths}
procedure ThtmlTable.GetWidths(Canvas: TCanvas; var TotalMinWidth, TotalMaxWidth: integer;
          var MinWidths, MaxWidths: IntArray; TheWidth: integer);  
var
  I, J, Min, Max, N, Span, Addon, Distributable, TotalPC,
  ExcessMin, ExcessMax, NonPC, PCWidth, NewTotalPC, MaxSum: integer;
  More: boolean;

Begin
{Find the max and min widths of each column}
FillChar(MaxWidths, Sizeof(MaxWidths), 0);
FillChar(MinWidths, Sizeof(MinWidths), 0);
FillChar(Percents, Sizeof(Percents), 0);
Span := 1;
More := True;
while More do
  begin
  More := False;
  for J := 0 to Rows.Count-1 do
    with TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then  
          with TCellObj(Items[I]) do
            begin
            PCWidth := 0;
            if WidthAttr > 0 then
              if AsPercent then PCWidth := WidthAttr
              else if TheWidth > 0 then
                PCWidth := IntMin(1000, MulDiv(WidthAttr, 1000, TheWidth));
            More := More or (ColSpan > Span); {set if need another iteration}
            if ColSpan = Span then
              begin
              Cell.MinMaxWidth(Canvas, Min, Max);
              Addon := CellSpacing + HzSpace;
              Inc(Min, Addon);
              Inc(Max, Addon);
              if Span = 1 then
                begin
                MaxWidths[I] := IntMax(MaxWidths[I],  Max);
                MinWidths[I] := IntMax(MinWidths[I],  Min);
                Percents[I] := Intmax(Percents[I], PCWidth);  {collect percents}
                end
              else
                begin
                TotalMaxWidth := 0;  TotalMinWidth := 0;
                TotalPC := 0;  NonPC := 0;
                for N := I to I+ColSpan-1 do
                  begin   {Total up the pertinant column widths}
                  Inc(TotalMaxWidth, MaxWidths[N]);
                  Inc(TotalMinWidth, MinWidths[N]);
                  if Percents[N] > 0 then
                    Inc(TotalPC, Percents[N])  {total percents}
                  else Inc(NonPC);      {count of cell with no percent}
                  end;
                ExcessMin := Min - TotalMinWidth;
                ExcessMax := Max - TotalMaxWidth;
                if (PCWidth > 0) or (TotalPC > 0) then
                  begin   {manipulate for percentages}
                  if NonPC > 0 then
                    {find the extra percentages to divvy up}
                    Distributable := IntMax(0, (PCWidth-TotalPC) div NonPC)
                  else Distributable := 0;
                  if (NonPC = 0) and (PCWidth > TotalPC) then
                    begin
                    for  N := I to I+ColSpan-1 do  {stretch percentages to fit}
                      Percents[N] := MulDiv(Percents[N], PCWidth, TotalPC);
                    end
                  else if Distributable > 0 then    {spread colspan percentage excess over the unspecified cols}
                    for N := I to I+ColSpan-1 do
                      if Percents[N] = 0 then Percents[N] := Distributable;
                  NewTotalPC := IntMax(TotalPC, PCWidth);
                  if ExcessMin > 0 then
                    begin
                    if NonPC > 0 then  {split excess over all cells}
                      begin
                      {proportion the distribution so cells with large MaxWidth get more}
                      MaxSum := 0;
                      for  N := I to I+ColSpan-1 do
                        Inc(MaxSum, MaxWidths[N]);   
                      for  N := I to I+ColSpan-1 do    
                        Inc(MinWidths[N], MulDiv(ExcessMin, MaxWidths[N], MaxSum));
                      end
                    else
                      for  N := I to I+ColSpan-1 do
                        Inc(MinWidths[N], (MulDiv(ExcessMin, Percents[N], NewTotalPC)));
                    end;
                  if ExcessMax > 0 then
                    begin
                    if NonPC > 0 then  {split excess over non-specified cells}
                      begin
                      Distributable := ExcessMax div NonPC;
                      for  N := I to I+ColSpan-1 do
                        if Percents[N] = 0 then
                          Inc(MaxWidths[N], Distributable);
                      end
                    else
                      for  N := I to I+ColSpan-1 do
                        Inc(MaxWidths[N], (MulDiv(ExcessMax, Percents[N], NewTotalPC)));
                    end;
                  end
                else
                  begin  {no width dimensions entered}
                  if ExcessMin > 0 then
                    for  N := I to I+ColSpan-1 do
                      if TotalMinWidth = 0 then
                        MinWidths[N] := Min div ColSpan
                      else  {split up the widths in proportion to widths already there}
                        MinWidths[N] := MulDiv(Min, MinWidths[N], TotalMinWidth);
                  if ExcessMax > 0 then
                    for  N := I to I+ColSpan-1 do
                      if TotalMaxWidth = 0 then
                        MaxWidths[N] := Max div ColSpan
                      else   {split up the widths in proportion to widths already there}
                        MaxWidths[N] := MulDiv(Max, MaxWidths[N], TotalMaxWidth);
                  end;
                end;
              end;
            end;
      end;
  Inc(Span);
  end;

TotalMaxWidth := 0;  TotalMinWidth := 0;
for I := 0 to NumCols-1 do
  begin
  Inc(TotalMaxWidth, MaxWidths[I]);
  Inc(TotalMinWidth, MinWidths[I]);
  end;
end;

{----------------ThtmlTable.MinMaxWidth}
procedure ThtmlTable.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
var
  MaxWidths, MinWidths: IntArray;
  Mn, Dummy: integer;
begin
AddDummyCells;     {in case it hasn't been done}
if UseAbsolute and (WidthAttr = 0) then
  GetMinMaxAbs(Canvas, Mn, Max, MinWidths, MaxWidths)
else if not AsPercent then
  GetWidths(Canvas, Mn, Max, MinWidths, MaxWidths, WidthAttr)
else
  GetWidths(Canvas, Mn, Max, MinWidths, MaxWidths, 0);

Inc(Mn, CellSpacing);
Inc(Max, CellSpacing);
if not AsPercent then
  begin
  Mn := IntMax(Mn, WidthAttr);
  Max := IntMax(Max, WidthAttr);
  end;
Caption.Cell.MinMaxWidth(Canvas, CaptionMinWidth, Dummy);
Min := IntMax(CaptionMinWidth, Mn);  {caption may be wider than table}
Max := IntMax(CaptionMinWidth, Max);
end;

procedure ThtmlTable.xxx(const MaxWidths, MinWidths: IntArray; TheWidth: integer);
{Divide up the table into columns.  TheWidth is the specified width of the table.
 At this point, it is known that everything will fit into TheWidth. Percents are
 being used}
var
  I, W, PCNotMinWid, TotalWid, Unsp, UnspDiff, Delta, Addon, Count: integer;
  UseMin: array[0..MaxCols] of boolean;
  NoChange: boolean;
begin
FillChar(UseMin, Sizeof(UseMin), False);
PCNotMinWid := 0;  TotalWid := 0;  Unsp := 0; UnspDiff := 0;
{First calculate everything assuming the data entered is perfectly correct}
for I := 0 to NumCols - 1 do
  begin
  if Percents[I] > 0 then
    begin
    W := MulDiv(TheWidth, Percents[I], 1000);  {width based on percentage}
    if W > MinWidths[I] then
      begin
      Widths[I] := W;
      Inc(PCNotMinWid, Percents[I]);
      end
    else
      begin   {percent is too small, use Min width}
      Widths[I] := MinWidths[I];
      UseMin[I] := True;
      end;
    end
  else
    begin    {no percent}
    Widths[I] := MinWidths[I];
    Inc(Unsp);     {an unspecified column}
    Inc(UnspDiff, MaxWidths[I]-MinWidths[I]); {total max-min for unspecified cols}
    end;
  Inc(TotalWid, Widths[I]);
  end;

Delta := TotalWid - TheWidth;    {see what the error is}
if Delta < 0 then     {table is too small}
  begin
  if Unsp > 0 then
    begin
    if (UnspDiff > 0) and (UnspDiff >= Abs(Delta) div 4) then    
      {increase the unspecified columns widths prop to Max, Min unless the difference is trivial}
      begin
      for I := 0 to NumCols-1 do
        if (Percents[I] = 0) then
          Inc(Widths[I], MulDiv(-Delta, MaxWidths[I] - MinWidths[I], UnspDiff));
      end
    else
      begin  {increase the unspecified columns widths uniformly}
      Addon := -Delta div Unsp;
      for I := 0 to NumCols - 1 do
        if (Percents[I] = 0) then
          Inc(Widths[I], Addon);
      end;
    end
  else
    begin            {no unspecified widths, increase the specified columns which are not minimum}
    for I := 0 to NumCols - 1 do
      if (Percents[I] > 0) and not UseMin[I] then
        Inc(Widths[I], MulDiv(-Delta, Percents[I], PCNotMinWid));
    end;
  end
else if Delta > 0 then    {calculated table is too large}
  begin
  Count := 0;
  {make one or more trial run to see what happens when shrinking the columns
   that can be shrunck.  May hit another MinWidth situation}
  repeat
    NoChange := True;
    for I := 0 to NumCols - 1 do
      if (Percents[I] > 0) and not UseMin[I] then
        begin
        W := Widths[I] - MulDiv(Delta, Percents[I], PCNotMinWid);
        if W < MinWidths[I] then
          begin    {new width is smaller than MinWidth, make adustments}
          UseMin[I] := True;
          NoChange := False;
          Dec(PCNotMinWid, Percents[I]);
          Dec(Delta, Widths[I]-MinWidths[I]);
          Widths[I] := MinWidths[I];
          end;
        end;
    Inc(Count);
  until NoChange or (Count >= 4);   {count guards against endless loop}
  for I := 0 to NumCols - 1 do  {now actually change the widths}
    if (Percents[I] > 0) and not UseMin[I] then
      Dec(Widths[I], MulDiv(Delta, Percents[I], PCNotMinWid));
  end;

TotalWid := 0;     {fix up any round off errors}
for I := 0 to NumCols - 1 do
  Inc(TotalWid, Widths[I]);
Delta := TotalWid-TheWidth;     {round off error}
if Delta > 0 then
  begin
  for I := 0 to NumCols-1 do
    if not UseMin[I] then
      begin
      Dec(Widths[I], Delta);   {remove extra from first non minimum}
      Break;
      end;
  end
else Inc(Widths[0], -Delta);   {tack it on anywhere}
end;

{----------------ThtmlTable.DrawLogic}
function ThtmlTable.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
Label
  GotWidths;
type
  HeightArray = array[0..16000] of integer;
var
  I, J, K, N, Span,
  TotalMaxWidth, TotalMinWidth, D, W, DS,
  Total, TotalPC, Residual, NewResidual, W1, W2, NewTotal, LastNewTotal: integer;
  More, Mr, HasPercents, UsesPercents, Done: boolean;
  MaxWidths, MinWidths: IntArray;
  NewWidth, Dummy: integer;
  Heights: ^HeightArray;
  OwnerWidth: integer;
  H, TotalHt, Addon, Sum: integer;
  Specified: boolean;
  AddedOn: integer;
  TopY: integer;
  FirstLinePtr: PInteger;

Begin
YDraw := Y;
TopY := Y;
ContentTop := Y;
DrawTop := Y;
StartCurs := Curs;
If Assigned(ParentSectionList.FirstLineHtPtr) and        {used for List items}
    (ParentSectionList.FirstLineHtPtr^ = 0) then
      FirstLinePtr := ParentSectionList.FirstLineHtPtr  {save for later}
else FirstLinePtr := Nil;

OwnerWidth := IMgr.RightSide(Y) - IMgr.LeftIndent(Y);
if WidthAttr > 0 then
  begin
  Specified := True;
  if AsPercent then
    NewWidth := MulDiv(OwnerWidth, WidthAttr, 1000)
  else NewWidth := WidthAttr;
  end
else
  begin
  Specified := False;
  NewWidth := OwnerWidth;
  end;
Dec(NewWidth, CellSpacing);

AddDummyCells;

{Figure the width of each column}
if UseAbsolute and not Specified then  
  begin
  GetWidthsAbs(Canvas, NewWidth, Specified, MinWidths, MaxWidths); {fills in Widths array}
  GoTo GotWidths;
  end
else
  GetWidths(Canvas, TotalMinWidth, TotalMaxWidth, MinWidths, MaxWidths, NewWidth);

if (TotalMinWidth >= NewWidth) then
  begin   {table won't fit, use minimun widths}
  Move(MinWidths, Widths, Sizeof(IntArray));
  GoTo GotWidths;
  end;

if Specified then
  begin
  xxx(MaxWidths, MinWidths, NewWidth);
  GoTo GotWidths;
  end;

TotalPC := 0;   {see if any percentage widths entered}
for I := 0 to NumCols-1 do
  Inc(TotalPC, Percents[I]);
UsesPercents := (TotalPc > 0) and (TotalPc <= 1000) {ignore ridiculous values}
                 or (WidthAttr > 0);

if UsesPercents then
  begin {find the largest width that will accomodate the %'s}
  Residual := 0; W1 := 0; W2 := 0;
  for I := 0 to NumCols-1 do
    if Percents[I] > 0 then  {a percent has been entered}
      W1 := IntMax(W1, MulDiv(MaxWidths[I], 1000, Percents[I])) {look for maximum}
    else
      Inc(Residual, MaxWidths[I]);  {accumlate the cols which have no percent}
  if TotalPC < 1000 then
    W2 := MulDiv(Residual, 1000, 1000-TotalPC)
  else if Residual > 0 then W2 := 30000
  else W2 := 0;
  Total := IntMax(W1, W2);
  if Total <= NewWidth then
    begin  {a fit is found using percents and maxwidths}
    if WidthAttr > 0 then
      Total := NewWidth;    {don't try to make it smaller than NewWidth}  
    NewResidual := MulDiv(Total, 1000-TotalPC, 1000);
    for I := 0 to NumCols-1 do
      if Percents[I] > 0 then   {figure widths to fit this situation}
        Widths[I] := MulDiv(Total, Percents[I], 1000)
      else if Residual > 0 then
        Widths[I] := MulDiv(MaxWidths[I], NewResidual, Residual)
      else Widths[I] := 0;    {this is an table syntax error condition}
    GoTo GotWidths;
    end;

  Done := False;
  LastNewTotal := $FFFFFFF;  
  repeat  {with the above possibilites taken care of, we can assume the final
           width will = NewWidth}
    HasPercents := False;
    Total := 0;  Residual := 0;
    for I := 0 to NumCols-1 do
      begin
      if Percents[I] > 0 then
        begin
        W := MulDiv(NewWidth, Percents[I], 1000); {a Percent's width based on NewWidth}
        if W < MinWidths[I] then  {but it must be > MinWidth}
          begin   {eliminate the percentage value as not achievable}
          Percents[I] := 0;
          Inc(Residual, MinWidths[I]);  {and put it in the residuals}
          end
        else
          begin
          HasPercents := True;   {still valid percents}
          Inc(Total, W);
          end;
        end
      else Inc(Residual, MinWidths[I]);
      end;
    if not HasPercents then Break;  {no percents are achievable}
    if Total+Residual <= NewWidth then
      begin  {a solution with at least some percentages can be found}
      Done := True;
      TotalMaxWidth := 0;  TotalMinWidth := 0;  {recalc these}
      for I := 0 to NumCols-1 do
        begin
        if Percents[I] > 0 then
          begin
          MinWidths[I] := MulDiv(NewWidth, Percents[I], 1000);
          MaxWidths[I] := MinWidths[I];  {this fixes the width thru later calculations}
          end;
        Inc(TotalMaxWidth, MaxWidths[I]);
        Inc(TotalMinWidth, MinWidths[I]);
        end;
      end
    else  {it doesn't fit screen, reduce percentages and try again}
      begin
      NewTotal := NewWidth-Residual;  {percent items must fit this}
      while LastNewTotal <= NewTotal do    
        Dec(NewTotal);
      LastNewTotal := NewTotal;
      for I := 0 to NumCols-1 do
        if Percents[I] > 0 then
          Percents[I] := MulDiv(Percents[I], NewTotal, Total);  
      end;
  until Done;
  end;

D := TotalMaxWidth - TotalMinWidth;
if (TotalMaxWidth <= NewWidth) or (D = 0) then
  Move(MaxWidths, Widths, Sizeof(IntArray))
else
  begin
  W := NewWidth - TotalMinWidth;
  for I := 0 to NumCols-1 do
    begin
    ds := MaxWidths[I] - MinWidths[I];
    Widths[I] := MinWidths[I] + MulDiv(ds, W, D);
    end;
  end;

GotWidths:

{Find Table Width}
TableWidth := CellSpacing;
for I := 0 to NumCols-1 do
  Inc(TableWidth, Widths[I]);
Caption.Cell.MinMaxWidth(Canvas, CaptionMinWidth, Dummy);
CaptionWidth := IntMax(TableWidth, CaptionMinWidth); {make sure caption fits}

GetMem(Heights, Rows.Count * Sizeof(integer));
try
  {Find the height of each row allowing for RowSpans}
  FillChar(Heights^, Rows.Count*Sizeof(integer), 0);
  Span := 1;
  More := True;
  while More do
    begin
    More := False;
    for J := 0 to Rows.Count-1 do
      with TCellList(Rows[J]) do
        begin
        if J+Span > Rows.Count then Break;  {otherwise will overlap}
        H := DrawLogic1(Canvas, Widths, Span, CellSpacing, Mr) + CellSpacing;
        More := More or Mr;
        if Span = 1 then
          Heights^[J] := H
        else
          begin
          TotalHt := 0;  {sum up the height so far for the rows involved}
          for K := J to J+Span-1 do
            Inc(TotalHt, Heights^[K]);
          if H > TotalHt then   {apportion the excess over the rows}
            begin
            Addon := ((H-TotalHt) div Span);
            AddedOn := 0;       
            for K := J to J+Span-1 do
              begin
              Inc(Heights^[K], Addon);
              Inc(AddedOn, Addon);   
              end;
            Inc(Heights^[J+Span-1], (H-TotalHt)-AddedOn); {make up for round off error}
            end;
          end;
        end;
    Inc(Span);
    end;

  if TopCaption then
    begin         {layout the caption}
    SectionHeight := Caption.Cell.DoLogic(Canvas, Y, CaptionWidth, Dummy, Dummy, Curs);
    CaptionHeight := SectionHeight;
    Inc(Y, SectionHeight);
    end
  else SectionHeight := 0;

  TotalHt := 0;
  if Rows.Count > 0 then     
    begin
    for J := 0 to Rows.Count-1 do
      Inc(TotalHt, Heights^[J]);
    if TotalHt < ProposedHeight-CellSpacing then
      begin
      Addon := (ProposedHeight-TotalHt-CellSpacing) div Rows.Count;
      Sum := 0;
      for J := 0 to Rows.Count-2 do
        begin
        Inc(Heights^[J], Addon);
        Inc(Sum, Heights^[J]);
        end;
      Heights^[Rows.Count-1] := ProposedHeight-CellSpacing-Sum;
      end;
    end;

  TableHeight := SectionHeight;
  for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      begin
      RowHeight := Heights^[J];
      RowSpanHeight := 0;
      Inc(SectionHeight, Heights^[J]);
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then  
          with TCellObj(Items[I]) do
            begin   {find the actual height, Ht, of each cell}
            Ht := 0;
            for K := J to J+RowSpan-1 do
              Inc(Ht, Heights^[K]);
            if RowSpanHeight < Ht then RowSpanHeight := Ht;
            end;
      DrawLogic2(Canvas, Y, CellSpacing, Curs);
      Inc(Y, RowHeight);
      end;
  Inc(SectionHeight, CellSpacing);
  TableHeight := SectionHeight-TableHeight;
Finally
  FreeMem(Heights, Rows.Count * Sizeof(integer));
  end;

if not TopCaption then
  begin
  CaptionHeight := Caption.Cell.DoLogic(Canvas, TopY+TableHeight, 
                   CaptionWidth, Dummy, Dummy, Curs);
  Inc(SectionHeight, CaptionHeight);
  end;

{figure the indents, CaptionWidth is = or larger than TableWidth}
CaptionIndent := 0;
if TableWidth < CaptionWidth then
  Indent := CaptionIndent + (CaptionWidth-TableWidth) div 2; {table indent} 

Len := Curs-StartCurs;
MaxWidth := CaptionWidth;
Result := SectionHeight;
DrawHeight := Result;
ContentBot := TopY+SectionHeight;
DrawBot := TopY+DrawHeight;
try
  If Assigned(FirstLinePtr) then
    FirstLinePtr^ := YDraw+SectionHeight;
except
  end;
end;

{----------------ThtmlTable.Draw}
function ThtmlTable.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  YO, YOffset, Y: integer;
begin
Y := YDraw;
Result := Y+SectionHeight;
if Float then
  Y := Y + VSpace;
YOffset := ParentSectionList.YOff;
YO := Y - YOffset;
{if we're printing and
 we're 2/3 down page and table won't fit on this page and table will fit on
 next page, then don't do table now}
with ParentSectionList do
  begin
  if Printing and
        (YO > ARect.Top + ((ARect.Bottom - ARect.Top)*2) div 3) and
        (Y+DrawHeight > PageBottom) and
        (DrawHeight < ARect.Bottom - ARect.Top) then
    begin
    if Y < PageBottom then
      PageBottom := Y;
    Exit;
    end;
  end;

if (YO+DrawHeight >= ARect.Top) and (YO < ARect.Bottom) and
       (not ParentSectionList.Printing or (Y < ParentSectionList.PageBottom)) then
  DrawTable(Canvas, ARect, IMgr, X, Y);
end;

procedure ThtmlTable.DrawTable(Canvas: TCanvas; const ARect: TRect; IMgr: IndentManager; X: integer; Y: integer);
var
  I, XX: integer;
  YY, YTable, YOffset: integer;
  Rgn: THandle;
begin
YOffset := ParentSectionList.YOff;
XX := X+Indent;   {for the table}
YY := Y;
DrawX := XX;
DrawY := YY;
if TopCaption then
  YY := Caption.Cell.Draw(Canvas, ARect, CaptionWidth, XX+CaptionIndent-Indent, YY, 1000,1000);  
YTable := YY;
if BdrOn then
  begin
  Rgn:= CreateRectRgn(XX, IntMax(Arect.Top-1, YTable-YOffset),
           XX+TableWidth, IntMin(ARect.Bottom, YTable+TableHeight-YOffset));
  end
else Rgn := 0;
for I := 0 to Rows.Count-1 do
  YY := TCellList(Rows.Items[I]).Draw(Canvas, ParentSectionList, ARect, Widths,
          XX, YY, YOffset, CellSpacing, Border, BorderColorLight, BorderColorDark,
          Rgn, I);   
if Rgn <> 0 then
  begin
  Canvas.Brush.Color := BdrColor or $2000000;
  FillRgn(Canvas.Handle, Rgn, Canvas.Brush.Handle);
  DeleteObject(Rgn);
  end;
if Border then
  if (BorderColorLight = clBtnHighLight) and (BorderColorDark = clBtnShadow) then     
    RaisedRect(ParentSectionList, Canvas, XX, YTable-YOffset, XX+TableWidth-1,
                                       YY+CellSpacing-YOffset-1, True)
  else
    RaisedRectColor(ParentSectionList, Canvas, XX, YTable-YOffset, XX+TableWidth-1,
                 YY+CellSpacing-YOffset-1, BorderColorLight, BorderColorDark, True);
if not TopCaption then
  Caption.Cell.Draw(Canvas, ARect, CaptionWidth, XX+CaptionIndent-Indent,
                        YTable+TableHeight, 1000,1000);  
end;

{----------------ThtmlTable.GetURL}
function ThtmlTable.GetURL(Canvas: TCanvas; X: integer; Y: integer;
         var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
         var ATitle: string): guResultType;
var
  CaptionOK, TableOK: boolean;

  function GetTableURL(X: integer; Y: integer): guResultType;
  var
    I, J, XX: integer;
    CellObj: TCellObj;
  begin
  for J := 0 to Rows.Count-1 do
    begin
    XX := DrawX;
    with  TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        begin
        CellObj := TCellObj(Items[I]);
        if Assigned(CellObj) then   
          with CellObj do
            begin
            if (X >=XX) and (X < XX+Wd)
                 and (Y >= Cell.DrawYY) and (Y < Cell.DrawYY+Ht) then
              begin
              Result := Cell.GetUrl(Canvas, X, Y, UrlTarg, FormControl, ATitle);
              Exit;
              end;
            end;
        Inc(XX, Widths[I]); 
        end;
      end;
    end;
  Result := [];
  end;

begin
Result := [];
if (Y >= ContentTop) and (Y < ContentBot) then
  begin
  TableOK := (X >= DrawX) and (X <= TableWidth+DrawX);
  CaptionOK := (X >= DrawX+CaptionIndent-Indent) and (X <= DrawX+CaptionWidth+CaptionIndent-Indent);
  if TopCaption then
    if Y < ContentTop+CaptionHeight then
      begin
      if CaptionOK then
        Result := Caption.Cell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
      end
    else
      begin
      if TableOK then
        Result := GetTableURL(X, Y);
      end
  else
    if Y < ContentTop+TableHeight then
      begin
      if TableOK then
        Result := GetTableURL(X, Y);
      end
    else
      begin
      if CaptionOK then
        Result := Caption.Cell.GetURL(Canvas, X, Y, UrlTarg, FormControl, ATitle);
      end;
  end;
end;

function ThtmlTable.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;    
var
  CaptionOK, TableOK: boolean;

  function GetTableObj(X: integer; Y: integer): boolean;
  var
    I, J, XX: integer;
  begin
  for J := 0 to Rows.Count-1 do
    begin
    XX := DrawX;
    with  TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        begin
        if Assigned(Items[I]) then  
          with TCellObj(Items[I]) do
            begin
            if (X >=XX) and (X < XX+Wd)
                 and (Y >= Cell.DrawYY) and (Y < Cell.DrawYY+Ht) then
              begin
              Result := Cell.PtInObject(X, Y, Obj, IX, IY);
              Exit;
              end;
            end;
        Inc(XX, Widths[I]); 
        end;
      end;
    end;
  Result := False;
  end;

begin
Result := False;
if (Y >= ContentTop) and (Y < ContentBot) then
  begin
  TableOK := (X >= DrawX) and (X <= TableWidth+DrawX);
  CaptionOK := (X >= DrawX+CaptionIndent-Indent) and (X <= DrawX+CaptionWidth+CaptionIndent-Indent);
  if TopCaption then
    if Y < ContentTop+CaptionHeight then
      begin
      if CaptionOK then
        Result := Caption.Cell.PtInObject(X, Y, Obj, IX, IY);
      end
    else
      begin
      if TableOK then
        Result := GetTableObj(X, Y);
      end
  else
    if Y < ContentTop+TableHeight then
      begin
      if TableOK then
        Result := GetTableObj(X, Y);
      end
    else
      begin
      if CaptionOK then
        Result := Caption.Cell.PtInObject(X, Y, Obj, IX, IY);
      end;
  end;
end;

{----------------ThtmlTable.FindCursor}
function ThtmlTable.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer;
var
  CaptionOK, TableOK: boolean;

  function GetTableCursor(X: integer; Y: integer; var XR: integer;
           var YR: integer; var CaretHt: integer; var Intext: boolean): integer;
  var
    I, J, XX: integer;
  begin
  for J := 0 to Rows.Count-1 do
    begin
    XX := DrawX;
    with  TCellList(Rows[J]) do
      begin
      for I := 0 to Count-1 do
        begin
        if Assigned(Items[I]) then   
          with TCellObj(Items[I]) do
            begin
            if (X >=XX) and (X < XX+Wd)
                 and (Y >= Cell.DrawYY) and (Y < Cell.DrawYY+Ht) then
              begin
              Result := Cell.FindCursor(Canvas, X, Y, XR, YR, CaretHt, InText);
              Exit;
              end;
            end;
        Inc(XX, Widths[I]);  
        end;
      end;
    end;
  Result := -1;
  end;

begin
Result := -1;
if ( Y >= ContentTop) and (Y < ContentBot) then
  begin
  TableOK := (X >= DrawX) and (X <= TableWidth+DrawX);
  CaptionOK := (X >= DrawX+CaptionIndent-Indent) and (X <= DrawX+CaptionWidth+CaptionIndent-Indent);
  if TopCaption then
    if Y < ContentTop + CaptionHeight then
      begin
      if CaptionOK then
        Result := Caption.Cell.FindCursor(Canvas, X, Y, XR, YR, CaretHt, InText);
      end
    else
      begin
      if TableOK then
        begin
        Result := GetTableCursor(X, Y, XR, YR, CaretHt, InText);
        end;
      end
  else
    if Y < ContentTop+TableHeight then
      begin
      if TableOK then
        begin
        Result := GetTableCursor(X, Y, XR, YR, CaretHt, InText);
        end;
      end
    else
      begin
      if CaptionOK then
        begin
        Result := Caption.Cell.FindCursor(Canvas, X, Y,
                  XR, YR, CaretHt, InText);
        Inc(YR, TableHeight);
        end;
      end;
  end;
end;

function ThtmlTable.CursorToXY(Canvas: TCanvas; Cursor: integer;
                                var X: integer; var Y: integer): boolean;
{note: returned X value is not correct here but it isn't used}
var
  I, J: integer;
begin
Result := False;
if (Len = 0) or (Cursor > StartCurs + Len) then Exit;
if TopCaption then
  begin
  Result := Caption.Cell.CursorToXy(Canvas, Cursor, X, Y);
  if Result then Exit;
  end;
for J := 0 to Rows.Count-1 do
  with  TCellList(Rows[J]) do
    for I := 0 to Count-1 do
      if Assigned(Items[I]) then  
        with TCellObj(Items[I]) do
          begin
          Result := Cell.CursorToXy(Canvas, Cursor, X, Y);
          if Result then Exit;
          end;
if not TopCaption then
  Result := Caption.Cell.CursorToXy(Canvas, Cursor, X, Y);
end;

{----------------ThtmlTable.GetChAtPos}
function ThtmlTable.GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
var
  I, J: integer;
begin
Result := False;
if (Len = 0) or (Pos < StartCurs) or (Pos > StartCurs + Len) then Exit;

Result := Caption.Cell.GetChAtPos(Pos, Ch, Obj);
if Result then Exit;

for J := 0 to Rows.Count-1 do
  with  TCellList(Rows[J]) do
    for I := 0 to Count-1 do
      if Assigned(Items[I]) then  
        with TCellObj(Items[I]) do
          begin
          Result := Cell.GetChAtPos(Pos, Ch, Obj);
          if Result then Exit;
          end;
end;

{----------------ThtmlTable.FindString}
function ThtmlTable.FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;

var
  I, J: integer;
begin
Result := -1;
if TopCaption then
  begin
  Result := Caption.Cell.FindString(From, ToFind, MatchCase);
  if Result >= 0 then Exit;
  end;
for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
            begin
            Result := Cell.FindString(From, ToFind, MatchCase);
            if Result >= 0 then Exit;
            end;
if not TopCaption then
  Result := Caption.Cell.FindString(From, ToFind, MatchCase);
end;

{----------------ThtmlTable.FindStringR}
function ThtmlTable.FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;

var
  I, J: integer;
begin
Result := -1;
if not TopCaption then
  begin
  Result := Caption.Cell.FindStringR(From, ToFind, MatchCase);
  if Result >= 0 then Exit;
  end;
for J := Rows.Count-1 downto 0 do
    with  TCellList(Rows[J]) do
      for I := Count-1 downto 0 do
        if Assigned(Items[I]) then
          with TCellObj(Items[I]) do
            begin
            Result := Cell.FindStringR(From, ToFind, MatchCase);
            if Result >= 0 then Exit;
            end;
if TopCaption then
  Result := Caption.Cell.FindStringR(From, ToFind, MatchCase);
end;

{----------------ThtmlTable.FindSourcePos}
function ThtmlTable.FindSourcePos(DocPos: integer): integer;   
var
  I, J: integer;
begin
Result := -1;
if TopCaption then
  begin
  Result := Caption.Cell.FindSourcePos(DocPos);
  if Result >= 0 then Exit;
  end;
for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then  
          with TCellObj(Items[I]) do
            begin
            Result := Cell.FindSourcePos(DocPos);
            if Result >= 0 then Exit;
            end;
if not TopCaption then
  Result := Caption.Cell.FindSourcePos(DocPos);
end;

{----------------ThtmlTable.FindDocPos}
function ThtmlTable.FindDocPos(SourcePos: integer; Prev: boolean): integer;   
var
  I, J: integer;
  TC: TCellObj;
begin
if not Prev then
  begin
  Result := Caption.Cell.FindDocPos(SourcePos, Prev);
  if Result >= 0 then Exit;

  for J := 0 to Rows.Count-1 do
    if Assigned(Rows.Items[J]) then  
      with  TCellList(Rows[J]) do
        for I := 0 to Count-1 do
          begin
          TC := TCellObj(Items[I]);
          if Assigned(TC) then
            begin
            Result := TC.Cell.FindDocPos(SourcePos, Prev);
            if Result >= 0 then Exit;
            end;
          end;
  end
else  {Prev , iterate in reverse}
  begin
  for J := Rows.Count-1 downto 0 do
      with  TCellList(Rows[J]) do
        for I := Count-1 downto 0 do
          if Assigned(Items[I]) then  
          begin
          TC := TCellObj(Items[I]);
          if Assigned(TC) then
            begin
            Result := TC.Cell.FindDocPos(SourcePos, Prev);
            if Result >= 0 then Exit;
            end;
          end;
  Result := Caption.Cell.FindDocPos(SourcePos, Prev);
  end;
end;

{----------------ThtmlTable.CopyToClipboard}
procedure ThtmlTable.CopyToClipboard;
var
  I, J: integer;
begin
if TopCaption then
  Caption.Cell.CopyToClipboard;

for J := 0 to Rows.Count-1 do
    with  TCellList(Rows[J]) do
      for I := 0 to Count-1 do
        if Assigned(Items[I]) then  
          with TCellObj(Items[I]) do
            Cell.CopyToClipboard;
if not TopCaption then
  Caption.Cell.CopyToClipboard;
end;

{----------------TSection.Create}
constructor TSection.Create(AMasterList: TSectionList; L: TAttributeList;
            Prop: TProperties; AnURL: TUrlTarget; ACell: TCellBasic; FirstItem: boolean); 
var
  FO : TFontObj;
  T: TAttribute;
  S: string;
  Clr: ClearAttrType;
  Percent: boolean;    
begin
inherited Create(AMasterList);
Buff := PWideChar(BuffS);
Len := 0;
BuffSize := 0;
Fonts := TFontList.Create;

FO := TFontObj.Create(Self, Prop.GetFont, 0);
FO.Title := Prop.PropTitle;   
if Assigned(AnURL) and (Length(AnURL.Url) > 0) then
  begin
  FO.CreateFIArray;
  Prop.GetFontInfo(FO.FIArray);
  FO.ConvertFont(FO.FIArray.Ar[LFont]);
  FO.UrlTarget.Copy(AnUrl);
  ParentSectionList.LinkList.Add(FO);
  {$ifndef NoTabLink}
  FO.CreateTabControl(AnUrl.TabIndex);    
  {$endif}
  end;

Fonts.Add(FO);

LineHeight := Prop.GetLineHeight(Abs(FO.TheFont.Height));
if FirstItem then   
  begin
  FirstLineIndent := Prop.GetTextIndent(Percent);  
  if Percent then
    FLPercent := IntMin(FirstLineIndent, 90);   
  end;

Images := TImageObjList.Create;
FormControls := TFormControlList.Create;

if Assigned(L) then
  begin
  if L.Find(ClearSy, T) then
    begin
    S := LowerCase(T.Name);
    if (S = 'left') then ClearAttr := clLeft
    else if (S = 'right') then ClearAttr := clRight
    else ClearAttr := clAll;
    end;
  if L.TheID <> '' then
    ParentSectionList.IDNameList.AddObject(L.TheID, Self);
  end;
if Prop.GetClear(Clr) then
  ClearAttr := Clr;

Lines := TFreeList.Create;
if Prop.Props[TextAlign] = 'right' then
  Justify := Right
else if Prop.Props[TextAlign] = 'center' then
  Justify := Centered
else if Prop.Props[TextAlign] = 'justify' then   
  Justify := FullJustify
else Justify := Left;
end;

{----------------TSection.CreateCopy}
constructor TSection.CreateCopy(AMasterList: TSectionList; T: TSectionBase);
var
  TT: TSection;
  I: integer;
begin
inherited CreateCopy(AMasterList, T);
TT := T as TSection;
Len := TT.Len;
BuffSize := TT.BuffSize;
BuffS := TT.BuffS;
SetLength(BuffS, Length(BuffS));
Buff := PWideChar(BuffS);
Brk := TT.Brk;   
Fonts := TFontList.CreateCopy(Self, TT.Fonts);
Images := TImageObjList.CreateCopy(AMasterList, TT.Images);
FormControls := TFormControlList.Create;
for I := 0 to TT.FormControls.Count-1 do
  FormControls.Add(TT.FormControls[I]);
Lines := TFreeList.Create;
Justify := TT.Justify;
ClearAttr := TT.ClearAttr;
LineHeight := TT.LineHeight;
FirstLineIndent := TT.FirstLineIndent;
FLPercent := TT.FLPercent;  
end;

{----------------TSection.Destroy}
destructor TSection.Destroy;
begin
if Assigned(XP) then
  Freemem(XP);
Fonts.Free;
Images.Free;
FormControls.Free;
SIndexList.Free;
Lines.Free;
inherited Destroy;
end;

procedure TSection.CheckFree;    
var
  I, J: integer;
begin
if Assigned(ParentSectionList) then
  begin
  {Check to see that there isn't a TFontObj in LinkList}
  if Assigned(ParentSectionList.LinkList) then
    for I := 0 to Fonts.Count-1 do
      begin
      J := ParentSectionList.LinkList.IndexOf(Fonts[I]);
      if J >=0 then
        ParentSectionList.LinkList.Delete(J);
      end;
  {Remove Self from IDNameList if there}
  if Assigned(ParentSectionList.IDNameList) then   
    with ParentSectionList.IDNameList do
      begin
      I := IndexOfObject(Self);
      if I > -1 then
        Delete(I);
      end;
  end;
end;

{----------------TSection.AddChar}
procedure TSection.AddChar(C: WideChar; Index: integer);
var
  Tok: TokenObj;
begin
Tok := TokenObj.Create;
Tok.AddUnicodeChar(C, Index);  
AddTokenObj(Tok);
Tok.Free;
end;

function TSection.GetIndexObj(I: integer): IndexObj;
begin
Result := SIndexList[I];
end;

{----------------TSection.Finish}
procedure TSection.Finish;
{complete some things after all information added}
var
  Last, I: integer;
  IO: IndexObj;
begin
Buff := PWideChar(BuffS);
Len := Length(BuffS);
Brk := Brk+'y';
if Len > 0 then
  begin
  if Assigned(XP) then  {XP = Nil when printing}
    begin
    Last := 0;   {to prevent warning msg}
    SIndexList := TFreeList.Create;
    for I := 0 to Len-1 do
      begin
      if (I = 0) or (XP^[I] <> Last+1) then
        begin
        IO := IndexObj.Create;
        IO.Pos := I;
        IO.Index := XP^[I];
        SIndexList.Add(IO);
        end;
      Last := XP^[I];
      end;
    FreeMem(XP);
    XP := Nil;
    end;
  end;
if Len > 0 then
  begin           
  Inc(ParentSectionList.SectionCount);
  SectionNumber := ParentSectionList.SectionCount;
  end;
end;

{----------------TSection.AddTokenObj}
procedure TSection.AddTokenObj(T : TokenObj);
var
  L, I : integer;
  C: char;
  St: WideString;
begin
if T.Leng = 0 then Exit;

L := Len+T.Leng;
if BuffSize < L+1 then Allocate(L + 500);  {L+1 so there is always extra for font at end}
case PropStack.Last.GetTextTransform of
  txUpper:
    St := WideUpperCase1(T.S);
  txLower:
    St := WideLowerCase1(T.S);
  else
    St := T.S;
  end;
BuffS := BuffS+St;
Buff := PWideChar(BuffS);
Move(T.I[1], XP^[Len], T.Leng*Sizeof(integer));
if NoBreak or (Self is TPreformated) then
  C := 'n'
else C := 'y';
for I := 1 to T.Leng do
  Brk := Brk+C;
Len := L;
end;

{----------------TSection.ProcessText}
Procedure TSection.ProcessText;
var
  I: integer;

  Procedure Remove(I: integer);
  begin
  Move(XP^[I], XP^[I-1], ((Length(BuffS))-I)*Sizeof(integer));   
  System.Delete(BuffS, I, 1);
  System.Delete(Brk, I, 1);
  TFormControlList(FormControls).Decrement(I-1);
  TFontList(Fonts).Decrement(I-1, ParentSectionList);
  TImageObjList(Images).Decrement(I-1);
  end;

begin
while (Length(BuffS) > 0) and (BuffS[1] = ' ') do
  Remove(1);
I := WidePos('  ', BuffS);
while I > 0 do
  begin
  if Brk[I] = 'n' then
    Remove(I)
  else
    Remove(I + 1);
  I := WidePos('  ', BuffS);
  end;

{After floating images at start, delete an annoying space}
for I := Length(BuffS)-1 downto 1 do
  if (BuffS[I] = ImgPan) and (Images.FindImage(I-1).ObjAlign in [ALeft, ARight])
               and (BuffS[I+1] = ' ') then
    Remove(I+1);

I := WidePos(WideString(' '+#8), BuffS);  {#8 is break char}
while I > 0 do
  begin
  Remove(I);
  I := WidePos(WideString(' '+#8), BuffS);
  end;

  I := WidePos(WideString(#8+' '), BuffS);
while I > 0 do
  begin
  Remove(I+1);
  I := WidePos(WideString(#8+' '), BuffS);
  end;

I := WidePos(nbsp, BuffS);
while I > 0 do
  begin
  BuffS[I] := ' ';   {subst space}
  Brk[I] := 'n';     {no break}
  I := WidePos(nbsp, BuffS);
  end;

Finish;
end;

{----------------TSection.Allocate}
procedure TSection.Allocate(N : integer);
begin
if BuffSize < N then
  begin
  ReAllocMem(XP, N*Sizeof(integer));
  BuffSize := N;
  end;
end;

{----------------TSection.ChangeFont}
procedure TSection.ChangeFont(Prop: TProperties);
var
  FO: TFontObj;
  LastUrl: TUrlTarget;
  NewFont: TMyFont;
  Align: AlignmentType;
begin
FO := TFontObj(Fonts[Fonts.Count-1]);
LastUrl := FO.UrlTarget;
NewFont := Prop.GetFont;
If FO.Pos = Len then
  FO.ReplaceFont(NewFont)  {fontobj already at this position, modify it}
else
  begin
  FO := TFontObj.Create(Self, NewFont, Len);
  FO.URLTarget.Copy(LastUrl);
  Fonts.Add(FO);
  end;
FO.Title := Prop.PropTitle;   
if LastUrl.Url <> '' then
  begin
  FO.CreateFIArray;
  Prop.GetFontInfo(FO.FIArray);
  FO.ConvertFont(FO.FIArray.Ar[LFont]);
  if ParentSectionList.LinkList.IndexOf(FO) = -1 then
    ParentSectionList.LinkList.Add(FO);
  end;
if Prop.GetVertAlign(Align) and (Align in [ASub, ASuper]) then  
  FO.SScript := Align
else FO.SScript := ANone;
end;

{----------------------TSection.HRef}
procedure TSection.HRef(Sy: Symb; List: TSectionList; AnURL: TUrlTarget;
          Attributes: TAttributeList; Prop: TProperties);
var
  FO: TFontObj;
  NewFont: TMyFont;
  Align: AlignmentType;
begin
FO := TFontObj(Fonts[Fonts.Count-1]);
NewFont := Prop.GetFont;
If FO.Pos = Len then
  FO.ReplaceFont(NewFont)  {fontobj already at this position, modify it}
else
  begin
  FO := TFontObj.Create(Self, NewFont, Len);
  Fonts.Add(FO);
  end;

if Sy = HRefSy then
  begin
  FO.CreateFIArray;
  Prop.GetFontInfo(FO.FIArray);
  FO.ConvertFont(FO.FIArray.Ar[LFont]);
  if ParentSectionList.LinkList.IndexOf(FO) = -1 then
    ParentSectionList.LinkList.Add(FO);
  {$ifndef NoTabLink}
  FO.CreateTabControl(AnUrl.TabIndex);
  {$endif}
  end
else if Assigned(FO.FIArray) then    
  begin
  FO.FIArray.Free;
  FO.FIArray := Nil;
  end;
FO.UrlTarget.Copy(AnUrl);
if Prop.GetVertAlign(Align) and (Align in [ASub, ASuper]) then  
  FO.SScript := Align
else FO.SScript := ANone;
end;

function TSection.AddImage(L: TAttributeList; ACell: TCellBasic; Index: integer): TImageObj;
begin
Result := TImageObj.Create(ParentSectionList, Len, L);
Result.MyCell := ACell;
Images.Add(Result);
AddChar(ImgPan, Index);    {marker for image}
end;

procedure TSection.AddPanel(AMasterList: TSectionList; L: TAttributeList;
                     ACell: TCellBasic; Index: integer);
var
  PO: TPanelObj;
begin
PO := TPanelObj.Create(AMasterList, Len, L, ACell);
Images.Add(PO);
AddChar(ImgPan, Index);    {marker for panel}
end;

{----------------TSection.AddFormControl}
function TSection.AddFormControl(Which: Symb; AMasterList: TSectionList;
         L: TAttributeList; ACell: TCellBasic; Index: integer;
         Prop: TProperties): TFormControlObj;  
var
  T: TAttribute;
  FCO: TFormControlObj;
  S: string[20];
  IO: TImageObj;

  procedure GetEditFCO;
  begin
  FCO := TEditFormControlObj.Create(AMasterList, Len, L, S, Prop);
  end;

begin
S := '';
if Which = InputSy then
  begin
  if L.Find(TypeSy, T) then
    begin
    S := LowerCase(T.Name);
    if (S = 'text') or (S = 'password') then
      GetEditFCO
    else if (S = 'submit') or (S = 'reset') or (S = 'button') then
      FCO := TButtonFormControlObj.Create(AMasterList, Len, L, S, Prop)
    else if S = 'radio' then
      FCO := TRadioButtonFormControlObj.Create(AMasterList, Len, L, ACell)
    else if S = 'checkbox' then
      FCO := TCheckBoxFormControlObj.Create(AMasterList, Len, L)
    else if S = 'hidden' then
      FCO := THiddenFormControlObj.Create(AMasterList, Len, L)
    else if S = 'image' then
      FCO := TImageFormControlObj.Create(AMasterList, Len, L)
    else
      GetEditFCO;
    end
  else
    GetEditFCO;
  end
else if Which = SelectSy then
  begin
  if L.Find(MultipleSy, T) or L.Find(SizeSy, T) and (T.Value > 1) then
    FCO := TListBoxFormControlObj.Create(AMasterList, Len, L, Prop)
  else
    FCO := TComboFormControlObj.Create(AMasterList, Len, L, Prop);
  end
else
  FCO := TTextAreaFormControlObj.Create(AMasterList, Len, L, Prop);
if S = 'image' then
  begin
  IO := AddImage(L, ACell, Index);  {leave out of FormControlList}
  IO.MyFormControl := TImageFormControlObj(FCO);
  TImageFormControlObj(FCO).MyImage := IO;
  end
else if S <> 'hidden' then
  begin
  FormControls.Add(FCO);
  AddChar(FmCtl, Index);    {marker for FormControl}  
  end;
Result := FCO;
end;

{----------------TSection.FindCountThatFits}
function TSection.FindCountThatFits(Canvas: TCanvas; Width: integer; Start: PWideChar; Max: integer): integer;
{Given a width, find the count of chars (<= Max) which will fit allowing for
 font changes.  Line wrapping will be done later}
var
  Cnt, XX, I, J, J1, J2, J3, OHang, Tmp : integer;
  Picture: boolean;
  Align: AlignmentType;
  HSpcL, HSpcR: integer;
  FLObj: TFloatingObj;
  Extent: integer;
const
  OldStart: PWideChar = nil;
  OldResult: integer = 0;
  OldWidth: integer = 0;

begin
if (Width = OldWidth) and (Start = OldStart) then
  begin
  Result := OldResult;
  Exit;
  end;
OldStart := Start;
OldWidth := Width;
Cnt := 0;
XX := 0;
while True do
  begin
  Canvas.Font := Fonts.GetFontAt(Start-Buff, OHang);
  J1 := Fonts.GetFontCountAt(Start-Buff, Len);
  J2 := Images.GetImageCountAt(Start-Buff);
  J3 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J2 = 0 then
    begin
    Tmp:= Images.GetWidthAt(Start-Buff, Align, HSpcL, HSpcR, FlObj);
    if not (Align in [ALeft, ARight]) then
      XX := XX + Tmp + HSpcL + HSpcR;
    I := 1;  J := 1;
    Picture := True;
    if XX > Width then break;
    end
  else if J3 = 0 then
    begin
    XX := XX + TFormControlList(FormControls).GetWidthAt(Start-Buff, HSpcL, HSpcR);
    XX := XX + HSpcL + HSpcR;
    I := 1;  J := 1;
    Picture := True;
    if XX > Width then break;
    end
  else
    begin
    Picture := False;
    J := IntMin(J1, J2);
    J := IntMin(J, J3);
    I := FitText(Canvas.Handle, Start, J, Width-XX, Extent);
    end;
  if Cnt+I >= Max then      {I has been initialized}
    begin
    Cnt := Max;
    Break;
    end
  else Inc(Cnt, I);

  if not Picture then
    begin
    if (I < J) or (I = 0) then   
      Break;
    XX := XX + Extent;
    end;

  Inc(Start, I);
  end;
Result := Cnt;
OldResult := Result;    
end;

{----------------TSection.FindCountThatFits1}
function TSection.FindCountThatFits1(Canvas: TCanvas; Start: PWideChar; Max: integer; X, Y: integer; IMgr: IndentManager;
                var ImgHt: integer; NxImages: TList) : integer;
{Given a width, find the count of chars (<= Max) which will fit allowing for
 font changes.  Line wrapping will be done later}
var
  Cnt, XX, I, J, J1, J2, J3, X1, X2,
    OHang, ImgWidth, Width : integer;
  Picture: boolean;
  Align: AlignmentType;
  ImageAtStart: boolean;
  FlObj: TFloatingObj;
  HSpcL, HSpcR: integer;
  BrChr, TheStart: PWideChar;
  Font, LastFont: TFont;
  SaveX: integer;
  FoundBreak: boolean;

begin
LastFont := Nil;
TheStart := Start;
ImageAtStart := True;
ImgHt := 0;

BrChr := StrScanW(TheStart, BrkCh); {see if a break char}
if Assigned(BrChr) and (BrChr-TheStart < Max) then
  begin
  Max := BrChr-TheStart;
  if Max = 0 then
    begin
    Result := 1;
    Exit;     {single character fits}
    end;
  FoundBreak := True;
  end
else FoundBreak := False;

Cnt := 0;
X1 := Imgr.LeftIndent(Y);
if Start = Buff then
  Inc(X1, FirstLineIndent);  
X2 := IMgr.RightSide(Y);   
Width := X2-X1;

if (Start = Buff) and (Images.Count = 0) and (FormControls.Count = 0) then   
  if Max * TFontObj(Fonts[0]).tmMaxCharWidth <= Width then  {try a shortcut}
    begin  {it will all fit}
    Result := Max;
    if FoundBreak then
      Inc(Result);
    Exit;
    end;

XX := 0;
while True do
  begin
  Font := Fonts.GetFontAt(Start-Buff, OHang);
  if Font <> LastFont then   {may not have to load font}  
    Canvas.Font := Font;
  LastFont := Font;
  J1 := IntMin(Fonts.GetFontCountAt(Start-Buff, Len), Max-Cnt);   
  J2 := Images.GetImageCountAt(Start-Buff);
  J3 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J2 = 0 then
    begin   {next is an image}
    I := 1;  J := 1;
    Picture := True;
    ImgWidth := Images.GetWidthAt(Start-Buff, Align, HSpcL, HSpcR, FlObj);
    if Align in [ALeft, ARight] then
      begin
      FlObj.DrawYY := Y;     {approx y position}
      if ImageAtStart then
        begin         
        Inc(XX, ImgWidth + FlObj.HSpaceL + FlObj.HSpaceR);
        if XX <= Width then   {it fits}
          begin
          IMgr.Update(Y, FlObj);
          ImgHt := IntMax(ImgHt, FlObj.ImageHeight + FlObj.VSpaceT + FlObj.VSpaceB);
          end
        else if Cnt > 0 then
          Break    {One or more do fit, this one doesn't}
        else
          begin   {first image doesn't fit}
          if IMgr.GetNextWiderY(Y) > Y then
            Break;   {wider area below, it might fit there}
          {Can't move it down, might as well put it here}
          IMgr.Update(Y, FlObj);
          ImgHt := IntMax(ImgHt, FlObj.ImageHeight + FlObj.VSpaceT + FlObj.VSpaceB);
          Cnt := 1;
          Break;
          end;
        end
      else
        NxImages.Add(FlObj);    {save it for the next line}
      end
    else
      begin
      Inc(XX, ImgWidth+HSpcL+HSpcR);
      ImageAtStart := False;
      end;
    if XX > Width then break;
    end
  else if J3 = 0 then
    begin
    XX := XX + TFormControlList(FormControls).GetWidthAt(Start-Buff, HSpcL, HSpcR);
    XX := XX + HSpcL + HSpcR;
    I := 1;  J := 1;
    Picture := True;
    ImageAtStart := False;
    if XX > Width then break;
    end
  else
    begin
    Picture := False;
    J := IntMin(J1, J2);
    J := IntMin(J, J3);
    I := FitText(Canvas.Handle, Start, J, Width-XX, SaveX);
    end;
  if Cnt+I >= Max then
    begin
    Cnt := Max;
    Break;
    end
  else Inc(Cnt, I);

  if not Picture then
    begin
    if I < J then Break;
    XX := XX + SaveX;   
    ImageAtStart := False;
    end;

  Inc(Start, I);
  end;
Result := Cnt;
if FoundBreak and (Cnt = Max) then   
  Inc(Result);
end;

function WrapChar(C: WideChar): boolean;
begin
Result := Ord(C) >= $3000;
end;

{----------------TSection.MinMaxWidth}
procedure TSection.MinMaxWidth(Canvas: TCanvas; var Min, Max: integer);
{Min is the width the section would occupy when wrapped as tightly as possible.
 Max, the width if no wrapping were used.}
var
  I, Indx, FloatMin: integer;
  P, P1: PWideChar;
  Obj: TObject;

  function FindTextWidthB(Canvas: TCanvas; Start: PWideChar; N: integer; RemoveSpaces: boolean): integer;
  begin
  Result := FindTextWidth(Canvas, Start, N, RemoveSpaces);
  if (Start = Buff) then
    if (FLPercent = 0) then   {not a percent}
      Inc(Result, FirstLineIndent)
    else
      Result := (100 * Result) div (100 - FLPercent);
  end;

begin
if (StoredMin > 0) and (Images.Count = 0) then
  begin
  Min := StoredMin;
  Max := StoredMax;
  Exit;
  end;
Min := 0;
Max := 0;
if Len = 0 then Exit;

for I := 0 to Images.Count-1 do     {call drawlogic for all the images}
  begin
  Obj := Images[I];
  with TFloatingObj(Obj) do
    begin
    DrawLogic(Self.ParentSectionList, Canvas, Fonts.GetFontObjAt(Pos, Indx), 0);
    if not PercentWidth then
      if ObjAlign in [ALeft, ARight] then
        begin
        Max := Max + ImageWidth + HSpaceL + HSpaceR;
        Brk[Pos+1] := 'y';    {allow break after floating image}
        end
      else Min := IntMax(Min, ImageWidth);
    end;
  end;
FloatMin := Max;

for I := 0 to FormControls.Count-1 do     {get Min for form controls}  
  begin
  Obj := FormControls[I];
  if Obj is TFormControlObj then
    with TFormControlObj(FormControls[I]) do
      Min := IntMax(Min, FControl.Width + HSpaceL + HSpaceR);
  end;

Max := 0;
P := Buff;
P1 := StrScanW(P, BrkCh); {look for break char}
while Assigned(P1) do
  begin
  Max := IntMax(Max, FindTextWidthB(Canvas, P, P1-P, False));
  P:= P1+1;
  P1 := StrScanW(P, BrkCh);
  end;
P1 := StrScanW(P, #0); {look for the end}
Max := IntMax(Max, FindTextWidthB(Canvas, P, P1-P, False)) + FloatMin;

P := Buff;
while P^ = ' ' do Inc(P);
P1 := P;
I := P1-Buff+1;
while P^ <> #0 do
  {find the next string of chars that can't be wrapped}
  begin
  if WrapChar(P1^) and  (Brk[I]='y') then
    begin
    Inc(P1);
    Inc(I);
    end
  else
    begin
    repeat
      begin
      Inc(P1);
      Inc(I);
      end;
    until (P1^=#0) or
          (((P1^ in [WideChar(' '), WideChar('-'), WideChar('?'), ImgPan, FmCtl, BrkCh]) or WrapChar(P1^))
               and (Brk[I]='y'));
    if P1^ in [WideChar('-'), WideChar('?')] then
      begin
      Inc(P1);
      Inc(I);
      end;
    end;
  Min := IntMax(Min, FindTextWidthB(Canvas, P, P1-P, True));
  while (P1^ in [WideChar(' '), ImgPan, FmCtl, BrkCh]) do
    begin
    Inc(P1);
    Inc(I);
    end;
  P := P1;
  end;

Min := IntMax(FloatMin, Min);
StoredMin := Min;
StoredMax := Max;
end;

{----------------TSection.FindTextWidth}
function TSection.FindTextWidth(Canvas: TCanvas; Start: PWideChar; N: integer; RemoveSpaces: boolean): integer;
{find actual line width of N chars starting at Start.  If RemoveSpaces set,
 don't count spaces on right end}
var
  I, J, J1, OHang, Wid, HSpcL, HSpcR: integer;
  Align: AlignmentType;
  FlObj: TFloatingObj;
begin
Result := 0;
if RemoveSpaces then
  while ((Start + N - 1)^ in [WideChar(' '), BrkCh]) do
    Dec(N);    {remove spaces on end}
while N > 0 do
  begin
  J := Images.GetImageCountAt(Start-Buff);
  J1 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J = 0 then  {it's and image}
    begin
    Wid := Images.GetWidthAt(Start-Buff, Align, HSpcL, HSpcR, FlObj);
    {Here we count floating images as 1 char but do not include their width,
      This is required for the call in FindCursor}
    if not (Align in [ALeft, ARight]) then
      begin
      Result := Result + Wid + HSpcL + HSpcR;  
      end;
    Dec(N);   {image counts as one char}
    Inc(Start);
    end
  else if J1 = 0 then
    begin
    Result := Result + TFormControlList(FormControls).GetWidthAt(Start-Buff, HSpcL, HSpcR);
    Result := Result + HSpcL + HSpcR;
    Dec(N);   {control counts as one char}
    Inc(Start);
    end
  else
    begin
    Canvas.Font := Fonts.GetFontAt(Start-Buff, OHang);
    I := IntMin(J, J1);
    I := IntMin(I, IntMin(Fonts.GetFontCountAt(Start-Buff, Len), N));
    Assert(I > 0, 'I less than or = 0 in FindTextWidth');
    Inc(Result, GetXExtent(Canvas.Handle, Start, I) + OHang);
    if I = 0 then
      Break;
    Dec(N, I);
    Inc(Start, I);
    end;
  end;
end;

{----------------TSection.FindTextWidthA}
function TSection.FindTextWidthA(Canvas: TCanvas; Start: PWideChar; N: integer): integer;
{find actual line width of N chars starting at Start.}
var
  I, J, J1, OHang, Wid, HSpcL, HSpcR: integer;
  Align: AlignmentType;
  FlObj: TFloatingObj;
  Font: TMyFont;
begin
Result := 0;
while N > 0 do
  begin
  J := Images.GetImageCountAt(Start-Buff);
  J1 := TFormControlList(FormControls).GetControlCountAt(Start-Buff);
  if J = 0 then  {it's an image}
    begin
    Wid := Images.GetWidthAt(Start-Buff, Align, HSpcL, HSpcR, FlObj);
    {Here we count floating images as 1 char but do not include their width,
      This is required for the call in FindCursor}
    if not (Align in [ALeft, ARight]) then
      begin
      Result := Result + Wid + HSpcL + HSpcR;  
      end;
    Dec(N);   {image counts as one char}
    Inc(Start);
    end
  else if J1 = 0 then
    begin
    Result := Result + TFormControlList(FormControls).GetWidthAt(Start-Buff, HSpcL, HSpcR);
    Result := Result + HSpcL + HSpcR;
    Dec(N);   {control counts as one char}
    Inc(Start);
    end
  else
    begin
    Font := Fonts.GetFontAt(Start-Buff, OHang);  
    Canvas.Font := Font;
    I := IntMin(J, J1);
    I := IntMin(I, IntMin(Fonts.GetFontCountAt(Start-Buff, Len), N));
    Inc(Result, GetXExtent(Canvas.Handle, Start, I) - OHang);
    if I = 0 then
      Break;
    Dec(N, I);
    Inc(Start, I);
    end;
  end;
end;

{----------------TSection.DrawLogic}
function TSection.DrawLogic(Canvas : TCanvas; X, Y, XRef, YRef, AWidth, AHeight: integer; IMgr: IndentManager;
             var MaxWidth: integer; var Curs: integer): integer;
{returns height of the section}
var
  PStart, P, Last: PWideChar;
  Max, N, NN, Width, I, Indx, ImgHt: integer;
  Finished: boolean;
  LR : LineRec;
  NxImages: TList;
  Tmp, Tmp1: integer;
  Obj: TFloatingObj;
  TopY, AccumImgBot, TotHt: integer;

  function GetClearSpace(ClearAttr: ClearAttrType): integer;
  var
    CL, CR: integer;
  begin
  Result := 0;
  if (ClearAttr <> clrNone) then
    begin  {may need to move down past floating image}
    IMgr.GetClearY(CL, CR);
    case ClearAttr of
      clLeft:  Result := IntMax(0, CL-Y-1);
      clRight:  Result := IntMax(0, CR-Y-1);
      clAll: Result := IntMax(CL-Y-1, IntMax(0, CR-Y-1));
      end;
    end;
  end;

  procedure LineComplete(NN : integer);
  var
    I, J, DHt, Desc, Tmp, TmpRt, Cnt, Index, H, SB, SA : integer;
    FO : TFontObj;
    Align: AlignmentType;
    FormAlign: AlignmentType;
    NoChar: boolean;
    P: PWideChar;
    FCO: TFormControlObj;
    FlObj: TFloatingObj;

    function FindSpaces: integer;   
    var
      I: integer;
    begin
    Result := 0;
    for I := 0 to NN-2 do  {-2 so as not to count end spaces}
      if (PStart+I)^ = ' ' then
        Inc(Result);
    end;

  begin
  DHt := 0;    {for the fonts on this line get the maximum height}
  Cnt := 0;
  Desc := 0;
  P := PStart;
  if (NN = 1) and (P^ = BrkCh) then
    NoChar := False
  else
    begin
    NoChar := True;
    for I := 0 to NN-1 do
      begin
      if not (P^ in [FmCtl, ImgPan, BrkCh]) then
        begin   {check for the no character case}
        NoChar := False;
        Break;
        end;
      Inc(P);
      end;
    end;

  if not NoChar then
    repeat
      FO := Fonts.GetFontObjAt(PStart-Buff+Cnt, Index);
      Tmp := FO.GetHeight(Desc);
      DHt := IntMax(DHt, Tmp);
      LR.Descent := IntMax(LR.Descent, Desc);
      J := Fonts.GetFontCountAt(PStart-Buff+Cnt, Len);
      Inc(Cnt, J);
    until Cnt >= NN;

  SB := 0;
  SA := 0;  {space before and after}
  Cnt := 0;   {if images, then maybe they add extra space}
  repeat
    Cnt := Cnt + Images.GetImageCountAt(PStart-Buff+Cnt);
    if Cnt < NN then
      begin
      H := Images.GetHeightAt(PStart-Buff+Cnt, Align, FlObj);
      FlObj.DrawYY := Y;     {approx y dimension}
      if (FLObj is TImageObj) and Assigned(TImageObj(FLObj).MyFormControl) then
         TImageObj(FLObj).MyFormControl.FYValue := Y;    
      case Align of
        ATop: SA := IntMax(SA, H - DHt);
        AMiddle:
             begin
             if DHt = 0 then
               begin
               DHt := Fonts.GetFontObjAt(PStart-Buff, Index).GetHeight(Desc);
               LR.Descent := Desc;
               end;
             Tmp := (H - DHt) div 2;
             SA := IntMax(SA, Tmp);
             SB := IntMax(SB, (H - DHt - Tmp));
             end;
        ABottom, ABaseline: SB := IntMax(SB, H - (DHt - LR.Descent));
        end;
      end;
    Inc(Cnt);  {to skip by the image}
  until Cnt >= NN;

  Cnt := 0;   {now check on form controls}
  repeat
    Cnt := Cnt + TFormControlList(FormControls).GetControlCountAt(PStart-Buff+Cnt);
    if Cnt < NN then
      begin
      H := TFormControlList(FormControls).GetHeightAt(PStart-Buff+Cnt, FormAlign);
      case FormAlign of
        ATop:
          SA := IntMax(SA, H-Dht);
        AMiddle:
          begin
          SA := IntMax(SA, (H-DHt)div 2);
          SB := IntMax(SB, H - ((H-DHt)div 2));
          end;
        ABaseline:
          SB := IntMax(SB, H-(DHt-LR.Descent));
        ABottom:
          SB := IntMax(SB, H-DHt);
        end;
      FCO := TFormControlList(FormControls).FindControl(PStart-Buff+Cnt);
      if Assigned(FCO) then
        FCO.FYValue := Y;
      end;
    Inc(Cnt);  {to skip by the control}
  until Cnt >= NN;

  TotHt := SA+DHt+SB;     
  if TotHt < LineHeight then
    begin
    Inc(SA, (LineHeight-TotHt) div 2);
    Inc(SB, (LineHeight-TotHt) div 2);
    end;

  {$ifndef NoTabLink}
  if not ParentSectionList.IsCopy then
    begin
    Cnt := 0;   {now check URLs}   
    repeat
      FO := Fonts.GetFontObjAt(PStart-Buff+Cnt, Index);
      FO.AssignY(Y);
      Cnt := Cnt + Fonts.GetFontCountAt(PStart-Buff+Cnt, Len);
    until Cnt >= NN;
    end;
  {$endif}

  LR.Start := PStart;
  LR.LineHt := DHt;
  LR.Ln := NN;
  TmpRt := IMgr.RightSide(Y);
  Tmp := IMgr.LeftIndent(Y);
  if PStart = Buff then
    Tmp := Tmp + FirstLineIndent;   
  if Justify = Left then
    LR.LineIndent := Tmp-X
  else if Justify = Centered then
    LR.LineIndent := (TmpRt + Tmp - FindTextWidth(Canvas, PStart, NN, True)) div 2 -X
  else if Justify = Right then
    LR.LineIndent := TmpRt - X - FindTextWidth(Canvas, PStart, NN, True)
  else
    begin    {Justify = FullJustify}
    LR.LineIndent := Tmp-X;
    if not Finished then
      begin
      LR.Extra := TmpRt - Tmp - FindTextWidth(Canvas, PStart, NN, True);
      LR.Spaces := FindSpaces;
      end;
    end;
  LR.DrawWidth := TmpRt-Tmp;   
  LR.SpaceBefore := LR.SpaceBefore + SB;
  LR.SpaceAfter := SA;
  Lines.Add(LR);
  Inc(PStart, NN);
  SectionHeight := SectionHeight +DHt + SA + LR.SpaceBefore;
  Tmp := DHt +SA + SB;
  Inc(Y, Tmp);
  LR.LineImgHt := IntMax(Tmp, ImgHt);
  for I := 0 to NxImages.Count-1 do
    begin
    IMgr.Update(Y, TFloatingObj(NxImages[I]));  {update Image manager and Image}
    {include images in Line height}
    with TFloatingObj(NxImages[I]) do
      Tmp1 := ImageHeight + VSpaceT + VSpaceB;
    LR.LineImgHt := IntMax(LR.LineImgHt, Tmp+Tmp1);
    AccumImgBot := IntMax(AccumImgBot, Y + Tmp1);
    end;
  NxImages.Clear;
  end;

begin         {TSection.DrawLogic}
YDraw := Y;
AccumImgBot := 0;
TopY := Y;
ContentTop := Y;
DrawTop := Y;
StartCurs := Curs;
PStart := Buff;
Last := Buff + Len - 1;
SectionHeight := 0;
Lines.Clear;
if (Len = 0) then
  begin
  Result := GetClearSpace(ClearAttr);
  DrawHeight := Result;
  SectionHeight := Result;
  ContentBot := Y+Result;
  DrawBot := ContentBot;
  MaxWidth := 0;
  DrawWidth := 0;
  Exit;
  end;
if FLPercent <> 0 then
  FirstLineIndent := (FLPercent * AWidth) div 100;  {percentage calculated}
Finished := False;
DrawWidth := IMgr.RightSide(Y) - X;
Width := IntMin(IMgr.RightSide(Y)-IMgr.LeftIndent(Y), AWidth);
MaxWidth := Width;
for I := 0 to Images.Count-1 do     {call drawlogic for all the images}
  begin
  Obj := TFloatingObj(Images[I]);
  Obj.DrawLogic(Self.ParentSectionList, Canvas, Fonts.GetFontObjAt(Obj.Pos, Indx), Width);
  MaxWidth := IntMax(MaxWidth, Obj.ImageWidth);  {HScrollBar for wide images}
  end;
for I := 0 to FormControls.Count-1 do
  with TFormControlObj(FormControls[I]) do
    if Assigned(FControl) then
      MaxWidth := IntMax(MaxWidth, FControl.Width);
NxImages := TList.Create;
while not Finished do
  begin
  Max := Last - PStart + 1;
  if Max <= 0 then Break;
  LR := LineRec.Create;     {a new line}
  if (Lines.Count = 0) then
    begin  {may need to move down past floating image}
    Tmp := GetClearSpace(ClearAttr);
    if Tmp > 0 then
      begin
      LR.LineHt := Tmp;
      Inc(SectionHeight, Tmp);
      LR.Ln := 0;
      LR.Start := PStart;
      Inc(Y, Tmp);
      Lines.Add(LR);
      LR := LineRec.Create;
      end;
    end;

  ImgHt := 0;
  NN := 0;
  if Self is TPreformated then
    N := Max
  else
    begin
    NN := FindCountThatFits1(Canvas, PStart, Max, X, Y, IMgr, ImgHt, NxImages);
    N := IntMax(NN, 1);   {N = at least 1}
    end;

  AccumImgBot := IntMax(AccumImgBot, Y+ImgHt);
  if NN = 0 then   {if nothing fits, see if we can move down}
    Tmp := IMgr.GetNextWiderY(Y) - Y
  else Tmp := 0;
  if Tmp > 0 then
    begin      {move down where it's wider}
    LR.LineHt := Tmp;
    Inc(SectionHeight, Tmp);
    LR.Ln := 0;
    LR.Start := PStart;
    Inc(Y, Tmp);
    Lines.Add(LR);
    end        {else can't move down or don't have to}
  else if N = Max then
    begin   {Do the remainder}
    Finished := True;
    LineComplete(N);
    end
  else
    begin
    P := PStart + N -1;   {the last char that fits}
    if (P^ in [WideChar(' '), FmCtl, ImgPan]) and (Brk[P - Buff + 1] <> 'n') or (P^ = BrkCh) or WrapChar(P^) then 
      begin  {move past spaces so as not to print any on next line}
      while (N < Max) and ((P+1)^ = ' ') do
        begin
        Inc(P);
        Inc(N);
        end;
      Finished := N >= Max;  
      LineComplete(N);
      end
    else if (N < Max) and ((P+1)^ = ' ') and (Brk[P - Buff + 1] <> 'n') then
      begin
      repeat
        Inc(N);         {pass the space}
        Inc(p);
      until (N >= Max) or ((P+1)^ <> ' ');
      Finished := N >= Max;
      LineComplete(N);
      end
    else if (N < Max) and ((P+1)^ in [FmCtl, ImgPan]) and (Brk[PStart-Buff+N] <> 'n') then {an image or control}
      begin
      Finished := False;
      LineComplete(N);
      end
    else
      begin {non space, wrap it by backing off to previous space or image}
      while (not ((P^ in [WideChar(' '), WideChar('-'), WideChar('?'), FmCtl, ImgPan]) or WrapChar(P^) or WrapChar((P+1)^))   
             or ((Brk[P-Buff+1] = 'n'))) and (P > PStart) do
        Dec(P);
      if (P = PStart) and ((not (P^ in [FmCtl, ImgPan])) or (Brk[PStart-Buff+1] = 'n')) then
        begin {no space found, forget the wrap, write the whole word and any
               spaces found after it}
        P := PStart+N-1;

        while (P <> Last) and not (P^ in [WideChar('-'), WideChar('?')])
                    and not (((P + 1)^ in [WideChar(' '), FmCtl, ImgPan, BrkCh]) or WrapChar((P+1)^)) or (Brk[P - Buff + 2] = 'n') do
          begin
          Inc(P);
          end;
        while (P <> Last) and ((P+1)^ = ' ') do
          begin
          Inc(P);
          end;
        if (P <> Last) and ((P+1)^ = BrkCh) then     
          Inc(P);
        {Line is too long, add spacer line to where it's clear}
        Tmp := IMgr.GetNextWiderY(Y) - Y;
        if Tmp > 0 then
          begin
          LR.LineHt := Tmp;
          Inc(SectionHeight, Tmp);
          LR.Ln := 0;
          LR.Start := PStart;
          Inc(Y, Tmp);
          Lines.Add(LR);
          end
        else
          begin   {line is too long but do it anyway}
          MaxWidth := IntMax(MaxWidth, FindTextWidth(Canvas, PStart, P-PStart+1, True));
          Finished := P = Last;
          LineComplete(P-PStart+1);
          end;
        end
      else
        begin  {found space}
        while (P+1)^ = ' ' do   {this eats nbsp between images}
          Inc(P);
        LineComplete(P-PStart+1);
        end;
      end;
    end;
  end;
NxImages.Free;
Curs := StartCurs + Len;

If Assigned(ParentSectionList.FirstLineHtPtr) and (Lines.Count > 0) then       {used for List items}
  with LineRec(Lines[0]) do
    if (ParentSectionList.FirstLineHtPtr^ = 0) then
      ParentSectionList.FirstLineHtPtr^ := YDraw + LineHt - Descent + SpaceBefore;  

DrawHeight := AccumImgBot - TopY;  {in case image overhangs}
if DrawHeight < SectionHeight then
  DrawHeight := SectionHeight;
Result := SectionHeight;
ContentBot := TopY+SectionHeight;
DrawBot := TopY+DrawHeight;
with ParentSectionList do   
  begin
  if not IsCopy and (SectionNumber mod 50 = 0) and (ThisCycle <> CycleNumber)
          and (SectionCount > 0) then
    ThtmlViewer(TheOwner).htProgress(ProgressStart + ((100-ProgressStart)*SectionNumber) div SectionCount);
  ThisCycle := CycleNumber;  {only once per cycle}
  end;
end;

{----------------TSection.Draw}
function TSection.Draw1(Canvas: TCanvas; const ARect: TRect;
     IMgr: IndentManager; X, XRef, YRef : integer) : integer;
var
  I: integer;
  MySelB, MySelE: integer;
  DC: HDC;
  Ctrl: TFormControlObj;
  YOffset, Y: integer;

  procedure DrawTheText(LR: LineRec; Start: PWideChar; Cnt, Descent: integer);
  var
    I, J, J1, J2, J3, J4, Index, Addon, TopP, LeftT, Tmp : integer;
    Obj: TFloatingObj;
    FO: TFontObj;
    ARect: TRect;
    Inverted, ImageAtStart, NewCP: boolean;
    Color: TColor;
    CP, CP1: TPoint;

    function ChkInversion(Start: PWideChar; var Count: Integer): boolean;
    var
      LongCount, C: integer;
    begin
    Result := False;
    C := Start-Buff;   
    Count := 32000;
    if ParentSectionList.IsCopy then Exit;
    if MySelE < MySelB then Exit;
    if (MySelB <= C) and (MySelE > C) then
      begin
      Result := True;
      LongCount := MySelE - C;
      end
    else if MySelB > C then LongCount := MySelB - C
    else if (MySelB = C) and ParentSectionList.ShowDummyCaret then
      LongCount := 1
    else LongCount := 32000;
    if LongCount > 32000 then Count := 32000
      else Count := LongCount;
    end;

  begin  {Y is at bottom of line here}
  NewCP := True;   
  ImageAtStart := True;
  CP.x := X + LR.LineIndent;
  LR.DrawY := Y-LR.LineHt;
  LR.DrawXX := CP.x;
  while Cnt > 0 do
    begin
    I := 1;
    J1 := Fonts.GetFontCountAt(Start-Buff, Len)-1;
    J2 := Images.GetImageCountAt(Start-Buff)-1;
    J4 := TFormControlList(FormControls).GetControlCountAt(Start-Buff)-1;
    FO := Fonts.GetFontObjAt(Start-Buff, Index);
    Canvas.Font := FO.TheFont;
    if J2 = -1 then
      begin  {it's an image or panel}
      Obj := Images.FindImage(Start-Buff);
      if Obj is TImageObj then
        begin
        if Obj.ObjAlign in [ALeft, ARight] then
          begin
          if ImageAtStart then
            begin
            ParentSectionList.DrawList.AddImage(TImageObj(Obj), Canvas, IMgr.LfEdge+Obj.Indent,
                 Y-LR.LineHt-LR.SpaceBefore, Y-Descent, FO);
            end
          else
            begin {if not at start, draw on next line}
            ParentSectionList.DrawList.AddImage(TImageObj(Obj), Canvas, IMgr.LfEdge+Obj.Indent, Y, Y-Descent, FO);
            end;
          end
        else
          begin
          SetTextJustification(Canvas.Handle, 0, 0);  
          TImageObj(Obj).Draw(Canvas, CP.x+Obj.HSpaceL, Y-LR.LineHt, Y-Descent, FO);
          CP.x := CP.x + Obj.ImageWidth + Obj.HSpaceL + Obj.HSpaceR;
          NewCP := True;
          ImageAtStart := False;
          end;
        end
      else
        begin  {it's a Panel}
        with TPanelObj(Obj) do
          begin
          ShowIt := True;
          if (Obj.ObjAlign in [ALeft, ARight]) then
            begin
            LeftT := IMgr.LfEdge+Obj.Indent;
            if ImageAtStart then
              TopP := Y-LR.LineHt-LR.SpaceBefore-YOffset+VSpaceT
            else
              TopP := Y-YOffset+VSpaceT;
            end
          else
            begin
            LeftT := CP.x+Obj.HSpaceL;
            case Obj.ObjAlign of
              ATop: TopP := Y-YOffset-LR.LineHt;
              AMiddle:  TopP := Y-YOffset - FO.tmHeight div 2 - ImageHeight div 2;
              ABottom, ABaseline:  TopP := Y-YOffset-ImageHeight-Descent;
              else TopP := 0;   {to eliminate warning msg}
              end;
            Inc(CP.x, ImageWidth+Obj.HSpaceL+Obj.HSpaceR);
            NewCP := True;    
            ImageAtStart := False;
            end;
          if ParentSectionList.IsCopy then
            TPanelObj(Obj).Draw(Canvas, LeftT, TopP)
          else
            begin
            Panel.Left := LeftT;
            Panel.Top := TopP;
            if ThvPanel(Panel).FVisible then
              Panel.Show
            else Panel.Hide;
            end;
          DrawXX := LeftT;      
          end;
        end;
      end
    else if J4 = -1 then
      begin  {it's a form control}
      Ctrl := TFormControlList(FormControls).FindControl(Start-Buff);
      if Assigned(Ctrl.FControl) then
        with Ctrl, FControl do
          begin
          ShowIt := True;
          case FormAlign of
             ATop:
               TopP := LR.DrawY - YOffset;
             AMiddle:
               TopP := Y - ((LR.LineHt+Height) div 2) - YOffset;
             ABaseline:
               TopP := Y - Height - Descent -YOffset; {sits on baseline}
             ABottom:
               TopP := Y-Height-YOffset;
             else TopP := Y;    {never get here}
            end;
          if ParentSectionList.IsCopy then
            Ctrl.Draw(Canvas, CP.x+Ctrl.HSpaceL, TopP)
          else
            begin
            Show;
            Left := CP.x+Ctrl.HSpaceL;
            Top := TopP;
            if Ctrl is TRadioButtonFormControlObj then
              with  TRadioButtonFormControlObj(Ctrl) do
                begin
                TRadioButtonFormControlObj(Ctrl).RButton.Show;
                if MyCell.BkGnd then
                  (FControl as TPanel).Color := MyCell.BkColor
                else (FControl as TPanel).Color := ParentSectionList.Background;
                TRadioButtonFormControlObj(Ctrl).RButton.Repaint;
                end;
            if Ctrl.Active and ((Ctrl is TRadioButtonFormControlObj) or
                   (Ctrl is TCheckBoxFormControlObj)) then
                begin
                Canvas.Brush.Color := clWhite;
                if (Ctrl is TRadioButtonFormControlObj) then
                  begin
                  if Screen.PixelsPerInch > 100 then  
                    Canvas.DrawFocusRect(Rect(Left-2, Top-2, Left+18, Top+18))
                  else
                    Canvas.DrawFocusRect(Rect(Left-3, Top-2, Left+16, Top+17));
                  end
                else
                  Canvas.DrawFocusRect(Rect(Left-3, Top-3, Left+16, Top+16));
                end;
            end;
          Inc(CP.x, Width+Ctrl.HSpaceL+Ctrl.HSpaceR);
          NewCP := True;
          end;
      ImageAtStart := False;
      end
    else
      begin
      J := IntMin(J1, J2);
      J := IntMin(J, J4);
      Inverted := ChkInversion(Start, J3);
      J := IntMin(J, J3-1);
      I := IntMin(Cnt, J+1);

      if Inverted then
        begin
        SetBkMode(Canvas.Handle, Opaque);
        Canvas.Brush.Color := Canvas.Font.Color;
        if FO.TheFont.bgColor = clNone then
          begin
          Color := Canvas.Font.Color;
          if Color and $80000000 = $80000000 then
            Color := GetSysColor(Color)
          else Color := Color and $FFFFFF;
          Canvas.Font.Color := Color xor $FFFFFF;
          end
        else Canvas.Font.Color := FO.TheFont.bgColor;
        end
      else if FO.TheFont.BGColor = clNone then
        SetBkMode(Canvas.Handle, Transparent)
      else
        begin
        SetBkMode(Canvas.Handle, Opaque);
        Canvas.Brush.Color := FO.TheFont.BGColor;
        end;
        
      if ParentSectionList.Printing then
        begin
        if ParentSectionList.PrintMonoBlack and    
              (GetDeviceCaps(Canvas.Handle, NumColors) in [0..2]) then
          begin
          Color := Canvas.Font.Color;
          if Color and $80000000 = $80000000 then    
            Color := GetSysColor(Color);
          if Color and $ffffff <> $ffffff then
            Canvas.Font.Color := clBlack;     {Print black}
          end;
        if not ParentSectionlist.PrintTableBackground then
          begin
          Color := Canvas.Font.Color;
          if Color and $80000000 = $80000000 then
            Color := GetSysColor(Color);
          if (Color and $E0E0 = $E0E0) then
            Canvas.Font.Color := $2A0A0A0;   {too near white or yellow, make it gray}
          end;
        end;

      SetTextAlign(Canvas.Handle, TA_BaseLine or TA_UpdateCP);
      {figure any offseet for subscript or superscript}
      with FO do                           
        if SScript = ANone then Addon := 0
          else if SScript = ASuper then Addon := -(FontHeight div 3)
          else Addon := Descent div 2 +1;
      NewCP := NewCP or (Addon <> 0);
      {calc a new CP if required}
      if NewCP then        
        begin
        MoveToEx(Canvas.Handle, CP.x, Y - Descent + Addon - YOffset, Nil);
        NewCP := Addon <> 0;
        end;

      if Self is TPreformated then
        begin   {so will clip in Table cells}
        ARect := Rect(IMgr.LfEdge, Y-LR.LineHt-LR.SpaceBefore-YOffset, IMgr.LfEdge+IMgr.ClipWidth, Y-YOffset+1);
        ExtTextOutW(Canvas.Handle, 0, 0, ETO_CLIPPED, @ARect, Start, I, nil);
        end
      else
        begin
        if (Cnt - I <= 0) and ((Start + I - 1)^ in [WideChar(' '), WideChar(BrkCh)]) then
          Tmp := I-1   {at end of line, don't show space or break}
        else Tmp := I;
        if LR.Spaces = 0 then
          SetTextJustification(Canvas.Handle, 0, 0)
        else
          SetTextJustification(Canvas.Handle, LR.Extra, LR.Spaces);
        if not IsWin95 then    {use TextOutW}
          TextOutW(Canvas.Handle, 0, 0, Start, Tmp)
        else
          begin  {Win95}  
          {Win95 has bug which extends text underline for proportional font in TextOutW.
           Use clipping to clip the extra underline.}
          ARect := Rect(CP.x, Y-LR.LineHt-LR.SpaceBefore-YOffset, CP.x+GetXExtent(Canvas.Handle, Start, Tmp), Y-YOffset+1);
          ExtTextOutW(Canvas.Handle, 0, 0, ETO_CLIPPED, @ARect, Start, Tmp, nil)
          end;
        end;
      {Put in a dummy caret to show character position}
      if ParentSectionList.ShowDummyCaret and not Inverted
                                  and (MySelB = Start-Buff) then
        begin              
        Canvas.Pen.Color := Canvas.Font.Color;
        Tmp := Y - Descent+ FO.Descent + Addon - YOffset;
        Canvas.Brush.Color := clWhite;
        Canvas.Rectangle(CP.x, Tmp, CP.x+1, Tmp-FO.FontHeight);
        end;

      GetCurrentPositionEx(Canvas.Handle, @CP1);

      if FO.Active then  
        begin
        Tmp := Y - Descent+ FO.Descent + Addon - YOffset;
        Canvas.Brush.Color := clWhite;
        Canvas.DrawFocusRect(Rect(CP.x, Tmp-FO.FontHeight, CP1.x+1, Tmp));
        end;
      CP := CP1;
      ImageAtStart := False;
      end;
    Dec(Cnt, I);
    Inc(Start, I);
    end;
  SetTextJustification(Canvas.Handle, 0, 0);    
  end;

  procedure DoDraw(I: integer);
  begin
  with LineRec(Lines[I]) do
    begin
    Inc(Y, LineHt+SpaceBefore);
    DrawTheText(LineRec(Lines[I]), Start, Ln, Descent);
    Inc(Y, SpaceAfter);
    end;
  ParentSectionList.FirstPageItem := False;   
  end;

begin       {TSection.Draw}
Y := YDraw;
DrawX := X;
Result := Y + SectionHeight;
YOffset := ParentSectionList.YOff;

if (Len > 0) and (Y-YOffset+DrawHeight >= ARect.Top) and (Y-YOffset < ARect.Bottom) then
  begin
  DC := Canvas.Handle;
  SetTextAlign(DC, TA_BaseLine);

  MySelB := ParentSectionList.SelB-StartCurs;
  MySelE := ParentSectionList.SelE-StartCurs;
  for I := 0 to Lines.Count-1 do
    with ParentSectionList do
      if Printing then
          with LineRec(Lines[I]) do
            begin
            if (Y + LineImgHt <= PageBottom) then
              begin
              if(Y + LineImgHt > ARect.Top+YOffSet) then
                DoDraw(I)
              else Inc(Y, SpaceBefore + LineHt + SpaceAfter);
              end
            else if (LineImgHt >= ARect.Bottom - ARect.Top) or PageShortened then   
              DoDraw(I)
            else
              begin
              if Y < PageBottom then
                  PageBottom := Y;
              Break;   {Dont' print, don't want partial line}
              end;
            end
      else
        with LineRec(Lines[I]) do    
          if (Y-YOffset+LineImgHt >= ARect.Top) and (Y-YOffset < ARect.Bottom) then  
            DoDraw(I)
          else  {do not completely draw extremely long paragraphs}
            Inc(Y, SpaceBefore + LineHt + SpaceAfter);
  end;
end;

{----------------TSection.CopyToClipboard}
procedure TSection.CopyToClipboard;        
var
  I, Strt, X1, X2: integer;
  MySelB, MySelE: integer;
begin
MySelB := ParentSectionList.SelB - StartCurs;
MySelE := ParentSectionList.SelE - StartCurs;
for I := 0 to Lines.Count-1 do
  with LineRec(Lines.Items[I]) do
    begin
    Strt := Start-Buff;
    if (MySelE <= Strt) or (MySelB > Strt + Ln) then Continue;
    if MySelB-Strt > 0 then X1 := MySelB-Strt
      else X1 := 0;
    if MySelE-Strt < Ln then X2 := MySelE - Strt    
      else X2 := Ln;
    ParentSectionList.CB.AddText(Start+X1, X2-X1);
    end;
ParentSectionList.CB.AddTextCR('', 0);
end;

{----------------TSection.PtInObject}
function TSection.PtInObject(X : integer; Y: integer; var Obj: TObject;
         var IX, IY: integer): boolean;
{Y is distance from start of section}
begin
Result := (Images.Count > 0) and Images.PtInObject(X, Y, Obj, IX, IY); 
end;

{----------------TSection.GetURL}
function TSection.GetURL(Canvas: TCanvas; X: integer; Y: integer;
          var UrlTarg: TUrlTarget; var FormControl: TImageFormControlObj;
          var ATitle: string): guResultType;
 {Y is absolute}
var
  I, L, Index, Width, TotalHt, IX, IY, Posn: integer;
  FO : TFontObj;
  LR: LineRec;
  IMap, UMap: boolean;
  MapItem: TMapItem;
  ImageObj: TImageObj;

  function MakeCopy(UrlTarget: TUrlTarget): TUrlTarget;
  begin
  Result := TUrlTarget.Create;
  Result.Copy(UrlTarget);
  end;

begin
Result := [];
{First, check to see if in an image}
if (Images.Count > 0) and
    Images.PtInImage(X, Y, IX, IY, Posn, IMap, UMap, MapItem, ImageObj) then
  begin
  if ImageObj.ImageTitle <> '' then
    begin
    ATitle := ImageObj.ImageTitle;
    Include(Result, guTitle);
    end
  else if ImageObj.FAlt <> '' then
    begin
    ATitle := ImageObj.FAlt;
    Include(Result, guTitle);
    end;
  ParentSectionList.ActiveImage := ImageObj;
  if Assigned(ImageObj.MyFormControl) then
    begin
    FormControl := ImageObj.MyFormControl;
    Include(Result, guControl);
    FormControl.XTmp := IX;
    FormControl.YTmp := IY;
    end
  else if UMap then
    begin
    if MapItem.GetURL(IX, IY, UrlTarg) then
      Include(Result, guUrl);
    end
  else
    begin
    FO := Fonts.GetFontObjAt(Posn, Index);
    if (FO.UrlTarget.Url <> '') then
      begin   {found an URL}
      Include(Result, guUrl);
      UrlTarg := MakeCopy(FO.UrlTarget);
      ParentSectionList.ActiveLink := FO;
      if IMap then
        UrlTarg.Url := UrlTarg.Url + '?'+IntToStr(IX)+','+IntToStr(IY);
      end;
    end;
  end
else    
  begin
  I := 0;
  LR := Nil;
  with Lines do
    begin
    while I < Count do
      begin
      LR := LineRec(Lines[I]);
      with LR do
        TotalHt := LineHt+SpaceBefore+SpaceAfter;
      if (Y > LR.DrawY) and (Y <= LR.DrawY+TotalHt) then
        Break;
      Inc(I);
      end;
    if I >= Count then Exit;
    end;
  with LR do
    begin
    if X < DrawXX then Exit;
    Width := X - DrawXX;
    if Spaces > 0 then
      SetTextJustification(Canvas.Handle, Extra, Spaces);
    L := FindCountThatFits(Canvas, Width, Start, Ln);
    if Spaces > 0 then
      SetTextJustification(Canvas.Handle, 0, 0);
    if L >= Ln then Exit;
    FO := Fonts.GetFontObjAt(L+(Start-Buff), Index);
    if (FO.UrlTarget.Url <> '') then {found an URL}
      if not ((Start+L)^ in [ImgPan]) then   {an image here would be in HSpace area}
        begin
        Include(Result, guUrl);
        UrlTarg := MakeCopy(FO.UrlTarget);
        ParentSectionList.ActiveLink := FO;
        end;
    if (FO.Title <> '') then  {found a Title}
      if not ((Start+L)^ in [ImgPan]) then   {an image here would be in HSpace area}
        begin
        ATitle := FO.Title;
        Include(Result, guTitle);
        end;
    end;
  end;
end;

{----------------TSection.FindCursor}
function TSection.FindCursor(Canvas: TCanvas; X: integer; Y: integer;
         var XR: integer; var YR: integer; var CaretHt: integer;
         var Intext: boolean): integer;
{Given an X, Y, find the character position and the resulting XR, YR position
 for a caret along with its height, CaretHt.  Coordinates are relative to this
 section}
var
  I, H, L, Width, TotalHt, L1, W, Delta, OHang: integer;
  LR: LineRec;

begin
Result := -1;
if X < DrawX then
  Exit;
I := 0;  H := ContentTop; L1 := 0;
LR := Nil;
with Lines do
  begin
  while I < Count do
    begin
    LR := LineRec(Lines[I]);
    with LR do
      TotalHt := LineHt+SpaceBefore+SpaceAfter;
    if H+TotalHt > Y then Break;
    Inc(H, TotalHt);
    Inc(I);
    Inc(L1, LR.Ln);  {L1 accumulates char count of previous lines}
    end;
  if (I >= Count) then
    Exit;
  end;
with LR do
  begin
  if X > LR.DrawXX + LR.DrawWidth then
    Exit;
  InText := True;   
  CaretHt := LineHt;
  YR := H + SpaceBefore;
  if X < DrawXX then
    begin
    Result := L1+StartCurs;
    InText := False;
    Exit;
    end;
  Width := X-DrawXX;
  if (Justify = FullJustify) and (Spaces > 0) then    
    SetTextJustification(Canvas.Handle, Extra, Spaces);
  L := FindCountThatFits(Canvas, Width, Start, Ln);
  W := FindTextWidth(Canvas, Start, L, False);
  XR := DrawXX + W;
  if L < Ln then
    begin   {check to see if passed 1/2 character mark}
    Canvas.Font := Fonts.GetFontAt(L1+L, OHang);
    Delta := FindTextWidthA(Canvas, Start+L, 1);
    if Width > W+(Delta div 2) then
      begin
      Inc(L);
      Inc(XR, Delta);
      end;
    end
  else InText := False;
  Result := L+L1+StartCurs;
  if Justify = FullJustify then
    SetTextJustification(Canvas.Handle, 0, 0);
  end;
end;

{----------------TSection.FindString}
function TSection.FindString(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
{find the first occurance of the string, ToFind, with a cursor value >= to From.
 ToFind is in lower case if MatchCase is False.  ToFind is known to have a length
 of at least one.
}
var
  P: PWideChar;
  I: integer;
  ToSearch: WideString;

begin
Result := -1;
if (Len = 0) or (From >= StartCurs + Len) then Exit;
if From < StartCurs then I := 0
else I := From-StartCurs;

if MatchCase then
  ToSearch := BuffS
else ToSearch := WideLowerCase1(BuffS);  {ToFind already lower case}

P := StrPosW(PWideChar(ToSearch) + I, PWideChar(ToFind));
if Assigned(P) then
  Result := StartCurs+(P-PWideChar(ToSearch));
end;

{----------------TSection.FindStringR}
function TSection.FindStringR(From: integer; const ToFind: WideString; MatchCase: boolean): integer;
{find the first occurance of the string, ToFind, with a cursor value <= to From.
 ToFind is in lower case if MatchCase is False.  ToFind is known to have a length
 of at least one.
}
var
  P: PWideChar;
  ToFindLen: word;
  ToMatch, ToSearch: WideString;

begin
Result := -1;
if (Len = 0) or (From < StartCurs) then
  Exit;
ToFindLen := Length(ToFind);
if (Len < ToFindLen) or (From-StartCurs+1 < ToFindLen) then
  Exit;

if From >= StartCurs + Len then
  ToSearch := BuffS      {search all of BuffS}
else ToSearch := Copy(BuffS, 1, From-StartCurs);  {Search smaller part}
if not MatchCase then
  ToSearch := WideLowerCase1(ToSearch);  {ToFind already lower case}

{search backwards for the end char of ToFind}
P := StrRScanW(PWideChar(ToSearch), ToFind[ToFindLen]);
while Assigned(P) and (P-PWideChar(ToSearch)+1 >= ToFindLen) do
  begin
  {pick out a string of proper length from end char to see if it matches}
  SetString(ToMatch, P-ToFindLen+1, ToFindLen);
  if WideSameStr1(ToFind, ToMatch) then
    begin  {matches, return the cursor position}
    Result := StartCurs + (P - ToFindLen+1 - PWideChar(ToSearch));
    Exit;
    end;
  {doesn't match, shorten string to search for next search}
  ToSearch := Copy(ToSearch, 1, P-PWideChar(ToSearch));
  {and search backwards for end char again}
  P := StrRScanW(PWideChar(ToSearch), ToFind[ToFindLen]);
  end;
end;

{----------------TSection.FindSourcePos}
function TSection.FindSourcePos(DocPos: integer): integer;
var
  I: integer;
  IO: IndexObj;
begin
Result := -1;
if (Len = 0) or (DocPos >= StartCurs + Len) then Exit;

for I := SIndexList.Count-1 downto 0 do
  begin
  IO := PosIndex[I];
  if IO.Pos <= DocPos-StartCurs then
    begin
    Result := IO.Index + DocPos-StartCurs - IO.Pos;
    break;
    end;
  end;
end;

{----------------TSection.FindDocPos}
function TSection.FindDocPos(SourcePos: integer; Prev: boolean): integer;
{for a given Source position, find the nearest document position either Next or
 previous}
var
  I: integer;
  IO, IOPrev: IndexObj;
begin
Result := -1;
if Len = 0 then Exit;

if not Prev then
  begin
  I:= SIndexList.Count-1;
  IO := PosIndex[I];
  if SourcePos > IO.Index + (Len-1) - IO.Pos then Exit; {beyond this section}

  IOPrev := PosIndex[0];
  if SourcePos <= IOPrev.Index then
    begin  {in this section but before the start of Document text}
    Result := StartCurs;
    Exit;
    end;

  for I := 1 to SIndexList.Count-1 do
    begin
    IO := PosIndex[I];
    if (SourcePos >= IOPrev.Index) and (SourcePos < IO.Index) then
      begin   {between IOprev and IO}
      if SourcePos-IOPrev.Index+IOPrev.Pos < IO.Pos then
        Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index)
      else Result := StartCurs+IO.Pos;
      Exit;
      end;
    IOPrev := IO;
    end;
  {after the last IndexObj in list}
  Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index);
  end
else     {prev  -- we're iterating from the end of TSectionList}
  begin
  IOPrev := PosIndex[0];
  if SourcePos < IOPrev.Index then Exit;   {before this section}

  I:= SIndexList.Count-1;
  IO := PosIndex[I];
  if SourcePos > IO.Index + (Len-1) - IO.Pos then
    begin   {SourcePos is after the end of this section}
    Result := StartCurs + (Len-1);
    Exit;
    end;

  for I := 1 to SIndexList.Count-1 do
    begin
    IO := PosIndex[I];
    if (SourcePos >= IOPrev.Index) and (SourcePos < IO.Index) then
      begin {between IOprev and IO}
      if SourcePos-IOPrev.Index+IOPrev.Pos < IO.Pos then
        Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index)
      else Result := StartCurs+IO.Pos-1;
      Exit;
      end;
    IOPrev := IO;
    end;
  {after the last IndexObj in list}
  Result := StartCurs+IOPrev.Pos+(SourcePos-IOPrev.Index);
  end;
end;

{----------------TSection.CursorToXY}
function TSection.CursorToXY(Canvas: TCanvas; Cursor: integer; var X: integer;
             var Y: integer): boolean;
var
  I, Curs: integer;
  LR: LineRec;
begin
Result := False;
if (Len = 0) or (Cursor > StartCurs + Len) then Exit;

I := 0;
LR := Nil;
Curs := Cursor - StartCurs;
Y := ContentTop;
with Lines do
  begin
  while I < Count do
    begin
    LR := LineRec(Lines[I]);
    with LR do
      begin
      if Curs < Ln then Break;
      Inc(Y, LineHt+SpaceBefore+SpaceAfter);
      Dec(Curs, Ln);
      end;
    Inc(I);
    end;
  if I >= Count then Exit;
  end;
if Assigned(Canvas) then
  begin
  if LR.Spaces > 0 then    
    SetTextJustification(Canvas.Handle, LR.Extra, LR.Spaces);
  X := LR.DrawXX + FindTextWidth(Canvas, LR.Start, Curs, False);
  if LR.Spaces > 0 then   
    SetTextJustification(Canvas.Handle, 0, 0);
  end
else X := LR.DrawXX;    
Result := True;
end;

{----------------TSection.GetChAtPos}
function TSection.GetChAtPos(Pos: integer; var Ch: WideChar; var Obj: TObject): boolean;
begin
Result := False;
if (Len = 0) or (Pos < StartCurs) or (Pos >= StartCurs + Len) then Exit;
Ch := Buff[Pos-StartCurs];
Obj := Self;
Result := True;
end;

{----------------TPanelObj.Create}
constructor TPanelObj.Create(AMasterList: TSectionList;
            Position: integer; L: TAttributeList; ACell: TCellBasic);
var
  PntPanel: TPaintPanel;
  I: integer;
  S, Source, AName, AType: string;
begin
inherited Create;
fMasterList := AMasterList;
Pos := Position;
PntPanel := TPaintPanel(AMasterList.PPanel);
Panel := ThvPanel.Create(PntPanel);
Panel.Left := -4000;
Panel.Parent := PntPanel;
with ThvPanel(Panel) do
  begin
  Top := -4000;
  Height := 20;
  Width := 30;
  BevelOuter := bvNone;
  BorderStyle := bsSingle;
  Color := clWhite;
  FVisible := True;
  Ctl3D := False;
  ParentCtl3D := False;
  ParentFont := False;
  ObjAlign := ABottom;   {default}
  HSpaceL := ImageSpace;  {default}
  for I := 0 to L.Count-1 do
    with TAttribute(L[I]) do
      case Which of
        HeightSy: Height := Value;
        WidthSy:
          if System.Pos('%', Name) = 0 then   
            begin
            SpecWidth := Value;
            Width := Value;
            end
          else
            begin
            Value := IntMax(1, IntMin(Value, 100));
            SpecWidth := Value;
            PercentWidth := True;
            end;
        RatioSy:    
          begin
          try
            Ratio := StrToFloat(Name);
          except
            Ratio := 0.0;
            end;
          end;
        HSpaceSy:  HSpaceL := IntMin(40, Abs(Value));
        VSpaceSy:  VSpaceT := IntMin(40, Abs(Value));
        SrcSy: Source := Name;
        NameSy:
          begin
          AName := Name;   
          try
            Panel.Name := Name;
          except  {duplicate name will be ignored}
            end;
          end;
        AlignSy:
          begin
          S := UpperCase(Name);
          if S = 'TOP' then ObjAlign := ATop
          else if (S = 'MIDDLE') or (S = 'ABSMIDDLE') then ObjAlign := AMiddle
          else if S = 'LEFT' then ObjAlign := ALeft
          else if S = 'RIGHT' then ObjAlign := ARight;
          end;
        AltSy: FAlt := Name;
        TypeSy: AType := Name;
        end;
  HSpaceR := HSpaceL;
  VSpaceB := VSpaceT;
  Caption := '';
  if Assigned(AMasterList.PanelCreateEvent) then
    AMasterList.PanelCreateEvent(AMasterList.TheOwner, AName, AType,
        Source, ThvPanel(Panel));
  ImageWidth := Width;
  ImageHeight := Height;
  end;
AMasterList.PanelList.Add(Self);
end;

constructor TPanelObj.CreateCopy(AMasterList: TSectionList; T: TPanelObj);
begin
inherited CreateCopy(T);
FAlt := T.FAlt;
Panel := ThvPanel.Create(Nil);
with T.Panel do
  Panel.SetBounds(Left, Top, Width, Height);
Panel.FVisible := T.Panel.FVisible;
Panel.Color := T.Panel.Color;
Panel.Parent := AMasterList.PPanel;
SpecWidth := T.SpecWidth;
PercentWidth := T.PercentWidth;
Ratio := T.Ratio;
OPanel := T.Panel;   {save these for printing}
OSender := T.fMasterList.TheOwner;
PanelPrintEvent := T.fMasterList.PanelPrintEvent;
end;

destructor TPanelObj.Destroy;
begin
if Assigned(fMasterList) and Assigned(fMasterList.PanelDestroyEvent) then
  fMasterList.PanelDestroyEvent(fMasterList.TheOwner, ThvPanel(Panel));
Panel.Free;
inherited Destroy;
end;

procedure TPanelObj.DrawLogic(SectionList: TSectionList; Canvas: TCanvas;
                            FO: TFontObj; AvailableWidth: integer);
begin
if PercentWidth and (AvailableWidth > 0) then
  begin
  ImageWidth := MulDiv(AvailableWidth, SpecWidth, 100);
  Panel.Width := ImageWidth;
  end;
if Ratio > 0.0 then
  begin
  ImageHeight := Round(Ratio*ImageWidth);
  Panel.Height := ImageHeight;
  end;
end;

procedure TPanelObj.Draw(ACanvas: TCanvas; X1, Y1: integer);
var
  OldBrushStyle: TBrushStyle;
  OldBrushColor: TColor;
  OldPenColor: TColor;
  Bitmap: TBitmap;
begin
if Panel.FVisible then
  with ACanvas do
    if Assigned(PanelPrintEvent) then  
      begin
      Bitmap := TBitmap.Create;
      try
        Bitmap.Height := ImageHeight;
        Bitmap.Width := ImageWidth;
        PanelPrintEvent(OSender, OPanel, Bitmap);
        PrintBitmap(ACanvas, X1, Y1, ImageWidth, ImageHeight, Bitmap.Handle);
      finally
        Bitmap.Free;
        end;
      end
    else
      begin
      OldBrushStyle := Brush.Style;   {save style first}
      OldBrushColor := Brush.Color;
      OldPenColor := Pen.Color;
      Pen.Color := clBlack;
      Brush.Color := Panel.Color;
      Brush.Style := bsSolid;

      ACanvas.Rectangle(X1, Y1, X1+ImageWidth, Y1+ImageHeight);
      WrapText(ACanvas, X1+15, Y1+15, X1+ImageWidth-15, Y1+ImageHeight-15, FAlt);

      Brush.Color := OldBrushColor;
      Brush.Style := OldBrushStyle;    {style after color as color changes style}
      Pen.Color := OldPenColor;
      end;
end;

{----------------TFloatingObj.CreateCopy}
constructor TFloatingObj.CreateCopy(T: TFloatingObj);
begin
inherited Create;
ImageWidth := T.ImageWidth;
ImageHeight := T.ImageHeight;
Indent := T.Indent;
ObjAlign := T.ObjAlign;
HSpaceL := T.HSpaceL;
HSpaceR := T.HSpaceR;
VSpaceT := T.VSpaceT;
VSpaceB := T.VSpaceB;
Pos := T.Pos;
end;

function TFloatingObj.GetYPosition: integer;
begin
Result := DrawYY;
end;

procedure ThvPanel.SetVisible(Value: boolean);
begin
if Value <> FVisible then
  begin
  FVisible := Value;
  if FVisible then
    Show
  else Hide;
  end;
end;

{----------------TCell.Create}
constructor TCell.Create(Master: TSectionList);
begin
inherited Create(Master);
IMgr := IndentManager.Create;
end;

{----------------TCell.CreateCopy}
constructor TCell.CreateCopy(AMasterList: TSectionList; T: TCellBasic);
begin
inherited CreateCopy(AMasterList, T);
IMgr := IndentManager.Create;
end;

destructor TCell.Destroy;
begin
IMgr.Free;
inherited Destroy;
end;

{----------------TCell.DoLogic}
function TCell.DoLogic(Canvas: TCanvas; Y: integer; Width, AHeight: integer;
                 var ScrollWidth: integer; var Curs: integer): integer;
{Do the entire layout of the cell or document.  Return the total document
 pixel height}
var
  IB: integer;
  LIndex, RIndex: integer;
  SaveID: TObject;
begin
IMgr.Clear;
IMgr.Reset(0, Width);
IMgr.Width := Width;
SaveID := IMgr.CurrentID;
IMgr.CurrentID := Self;

LIndex := IMgr.SetLeftIndent(0, Y);
RIndex := IMgr.SetRightIndent(0+Width, Y);

Result := inherited DoLogic(Canvas, Y, Width, AHeight, ScrollWidth, Curs);

IMgr.FreeLeftIndentRec(LIndex);   
IMgr.FreeRightIndentRec(RIndex);
IB := IMgr.ImageBottom - YValue;   {check for image overhang}
IMgr.CurrentID := SaveID;
if IB > Result then
  Result := IB;
end;

{----------------TCell.Draw}
function TCell.Draw(Canvas: TCanvas; ARect: TRect; ClipWidth, X: integer;
                            Y, XRef, YRef : integer): integer;
{draw the document or cell.  Note: individual sections not in ARect don't bother
 drawing}
begin
IMgr.Reset(X, X+IMgr.Width);
IMgr.ClipWidth := ClipWidth;
DrawYY := Y;   {This is overridden in TCellObj.Draw}
Result := inherited Draw(Canvas, ARect, ClipWidth, X, Y, XRef, YRef);
end;

{ TBlockCell }

function TBlockCell.DoLogicX(Canvas: TCanvas; X, Y, XRef, YRef, Width, AHeight: integer;
  var ScrollWidth, Curs: Integer): integer;
{Do the entire layout of the this cell.  Return the total pixel height}
var
  I, Sw, TheCount: integer;
  H, Tmp: integer;
  SB: TSectionBase;
begin
  YValue := Y;
  StartCurs := Curs;
  H := 0;
  ScrollWidth := 0;
  tcContentBot := 0;
  tcDrawTop := 990000;
  tcDrawBot := 0;
  TheCount := Count;
  I := 0;
  while I < TheCount do
    begin
    SB :=  TSectionBase(Items[I]);
    Tmp := SB.DrawLogic(Canvas, X, Y+H, XRef, YRef, Width, AHeight, IMgr, Sw, Curs);
    H := H+Tmp;
    ScrollWidth := IntMax(ScrollWidth, Sw);
    if (SB is TBlock) and (TBlock(SB).Positioning = posAbsolute) then
    else  tcContentBot := IntMax(tcContentBot, SB.ContentBot);
    tcDrawTop := IntMin(tcDrawTop, SB.DrawTop);
    tcDrawBot := IntMax(tcDrawBot, SB.DrawBot);
    Inc(I);
    end;
  Len := Curs - StartCurs;
  Result := H;
  CellHeight := Result;
 end;


{ TDrawList }

Type
  TImageRec = class(TObject)
    AObj: TImageObj;
    ACanvas: TCanvas;
    AX, AY:  integer;
    AYBaseline: integer;
    AFO: TFontObj;
  end;

procedure TDrawList.AddImage(Obj: TImageObj; Canvas: TCanvas; X, TopY, YBaseline: Integer;
  FO: TFontObj);
var
  IR: TImageRec;
begin
IR := TImageRec.Create;
IR.AObj := Obj;
IR.ACanvas := Canvas;
IR.AX := X;
IR.AY := TopY;
IR.AYBaseline := YBaseline;
IR.AFO := FO;
Add(IR);
end;

procedure TDrawList.DrawImages;
var
  I: integer;
  Item: TObject;
begin
I := 0;
while I < Count do  {note: Count may increase during this operation}
  begin
  Item := Items[I];
  if (Item is TImageRec) then
    with TImageRec(Item) do
      AObj.Draw(ACanvas, AX, AY, AYBaseline, AFO);
  Inc(I);
  end;
end;

procedure TFormRadioButton.WMGetDlgCode(var Message: TMessage);
begin
Message.Result := DLGC_WantArrows;  {else don't get the arrow keys}
end;

procedure TFormCheckBox.WMGetDlgCode(var Message: TMessage);
begin
Message.Result := DLGC_WantArrows;  {this to eat the arrow keys}
end;

procedure TTabControl.WMGetDlgCode(var Message: TMessage);  
begin
Message.Result := DLGC_WantArrows;  {this to eat the arrow keys}
end;

end.




