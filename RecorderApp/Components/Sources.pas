//==============================================================================
//  Unit:        Sources
//
//  Implements:  TSources
//
//  Description: Composite component dealing with document sources, internal
//               or external(file system)
//
//  Author:      John van Breda
//  Created:     8 April 1999
//
//  Changes:     Eric Salmon - 21/03/2002
//               Removed all references to BaseDragForm from the unit. Added
//               parameter to the Init function to provide a link back to the
//               RegisterDropComponent function. The component is completely
//               disassociated from the main source of Recorder2000.
//
//               Eric Salmon - 27/03/2002
//               Changed parameter list of the Init function to accept a
//               database object instead of the database name and session name.
//               Implementation changes concerning the handling of the database
//               object, makes it easier to deal with either TtaDatabase or
//               TADOConnection.
//
//  Last Revision Details:
//    $Revision: 112 $
//    $Date: 16/04/09 9:34 $
//    $Author: Ericsalmon $
//
//  Copyright © Dorset Software Services Ltd, 1999
//
//==============================================================================

unit Sources;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Grids, CompositeComponent, ExtCtrls, Buttons, Registry,
  SourcesData, DropStruct, DropSource, DataClasses, DisplayFuncs, Constants,
  DropTarget, EasyShell, ADODB;

resourcestring
  ResStr_InternalDocuments  = 'Internal documents';
  ResStr_ExternalReferences = 'External references';
  ResStr_GridCommaText      = '"",Document,Original,Type';

type
  ESourcesError = class(Exception);

  TShowSourceEvent = procedure (Sender : TObject; SourceKey : String) of Object;

  TSources = class(TCompositeComponent)
  private
    FdmSources        : TdmSources;
    FAlreadyShown     : boolean;
    FBevel            : TBevel;
    FlblInternal      : TLabel;
    FlblExternal      : TLabel;
    FsgInternalRefs   : TStringGrid;
    FcmbOriginal      : TComboBox;
    FlbExternalRefs   : TListBox;
    FbbFindInternalRef: TBitBtn;
    FbbAddInternalRef : TBitBtn;
    FbbDelInternalRef : TBitBtn;
    FbbViewExternalRef: TBitBtn;
    FbbAddExternalRef : TBitBtn;
    FbbDelExternalRef : TBitBtn;
    FshpInternalRefs  : TShape;
    FshpExternalRefs  : TShape;
    FExternalRefList  : TExternalRefList;
    FInternalRefList  : TInternalRefList;
    FSourceJoinTable  : string;
    FSourcesFromKey   : TKeyString;
    FDatabase         : TADOConnection;
    FUserAccessLevel  : TUserAccessLevel;
    FNextKey          : TGetNextKey;
    FFindInternalEvent: TNotifyEvent;
    FAddInternalEvent : TNotifyEvent;
    FShowSourceEvent  : TShowSourceEvent;

    FRegisterDropComponent: TRegisterDropComponent;
    FSiteID: string;
    FDefaultPath: string;    
    procedure CreateObjects;
    function CheckInternalKey(const AKey: TKeyString): boolean;
    function GetExternalSourceKey: TKeyString;
    function GetInternalSourceKey: TKeyString;

    procedure SetupBevel;
    procedure SetupInternalLabel;
    procedure SetupInternalRefsGrid;
    procedure SetupOriginalComboBox;
    procedure SetupFindInternalRefButton;
    procedure SetupAddInternalRefButton;
    procedure SetupDelInternalRefButton;
    procedure SetupInternalRefsShape;

    procedure SetupExternalLabel;
    procedure SetupExternalRefsListBox;
    procedure SetupViewExternalRefButton;
    procedure SetupAddExternalRefButton;
    procedure SetupDelExternalRefButton;
    procedure SetupExternalRefsShape;
    procedure ShellOrEditExternalRef(Sender:TObject);
    { Event Methods }
    procedure FsgInternalRefsClick(Sender:TObject);
    procedure FsgInternalRefsDblClick(Sender:TObject);
    procedure FsgInternalRefsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FsgInternalRefsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FcmbOriginalChange(Sender: TObject);
    procedure bbFindInternalClick(Sender : TObject);
    procedure bbAddInternalClick(Sender : TObject);
    procedure bbRemoveInternalClick(Sender : TObject);
    procedure AddMultipleInternalRefs(const iKeyList: TKeyList);
    procedure SetFindInternalEvent(const Value: TNotifyEvent);
    procedure SetAddInternalEvent(const Value: TNotifyEvent);
    procedure SetShowSourceEvent(const Value: TShowSourceEvent);

    procedure FlbExternalRefsClick(Sender:TObject);
    procedure DoShellFile(Sender:TObject);
    procedure bbAddExternalClick(Sender: TObject);
    procedure bbRemoveExternalClick(Sender: TObject);
    procedure BevelResize(Sender: TObject);
  protected
    { Other methods }
    procedure SetEditMode(const Value: TEditMode); override;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetSourceJoinTable(const Value: string);
    procedure SetSourcesFromKey(const Value: TKeyString);
  public
    constructor Create( AOwner : TComponent); override;
    destructor Destroy; override;
    procedure RefreshLists;
    procedure ResetKeys(const AKey:TKeyString);
    procedure Post;
    procedure RegisterDragDrop;
    procedure InternalRefsDrop(const Sender: TObject; const iFormat : integer;
      const iSourceData: TKeyList; const iTextStrings : TstringList;
      var ioHandled : boolean);
    procedure ExternalRefsDrop(const Sender: TObject; const iFormat : integer;
      const iSourceData: TKeyList; const iTextStrings : TstringList;
      var ioHandled : boolean);
    procedure Init(const ADatabase: TADOConnection; const iTableName: string;
      const iUAL: TUserAccessLevel; iNextKey: TGetNextKey;
      iRegDropComp: TRegisterDropComponent; const ASiteID: string; ADefaultPath: string);
    procedure SetDatabase(const ADatabase: TADOConnection);
    procedure InternalRefUpdate(KeyList:TKeyList);
    procedure AddInternalRef(const AKey:TKeyString);
    procedure GetInternalSources(var ioSourceList:TEditableKeyList);
    property SourcesFromKey : TKeyString read FSourcesFromKey write SetSourcesFromKey;
    property InternalSourceKey:TKeyString read GetInternalSourceKey;
    property ExternalSourceKey:TKeyString read GetExternalSourceKey;
  published
    property OnClick;
    property OnEnter;
    property OnExit;
    { Event handler for the find/add internal methods }
    property OnFindInternal: TNotifyEvent read FFindInternalEvent
                                          write SetFindInternalEvent;
    property OnAddInternal: TNotifyEvent read FAddInternalEvent
                                         write SetAddInternalEvent;
    property OnShowSource: TShowSourceEvent read FShowSourceEvent
                                            write SetShowSourceEvent;
  end;

//==============================================================================
implementation

{$R *.res}

uses
  GeneralFunctions, ShellApi, ExternalSourceDetails;

resourcestring
  ResStr_TooLate        = 'SourceJoinTable cannot be changed once shown';
  ResStr_NoSourcesJoin  = 'Property SourcesJoinTable uninitialised in TSources component.';
  ResStr_NoKey          = 'Property SourcesFromKey uninitialised in TSources component.';
  ResStr_OwnerNotBase   = 'Cannot register drag drop for TSources on a non TBaseForm';
  ResStr_DMNil          = 'Internal error - sources data module is nil during drop attempt';
  ResStr_RemoevRefIntDoc =  'Are you sure you want to remove the reference to this Internal Document?';
  ResStr_CannotFindFile = 'Could not find the file %s';
  ResStr_RemoveRefToExtDoc =  'Are you sure you want to remove the reference to this External Document?';
  ResStr_DuplicateFile = 'The selected file is already in the list of external sources.';


//==============================================================================
{ TSources }

constructor TSources.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 355;
  Height:= 265;
  // Set Frame
  SetupBevel;
  // Set Title Labels
  SetupInternalLabel;
  SetupExternalLabel;
  // The creation order also determines the tab order at runtime.
  // Set Internal References controls
  SetupInternalRefsShape;
  SetupInternalRefsGrid;
  SetupOriginalComboBox; // for in-place editing
  SetupFindInternalRefButton;
  SetupAddInternalRefButton;
  SetupDelInternalRefButton;
  // Set External References controls
  SetupExternalRefsShape;
  SetupExternalRefsListBox;
  SetupViewExternalRefButton;
  SetupAddExternalRefButton;
  SetupDelExternalRefButton;

  FAlreadyShown := False;
end;  // Create

//==============================================================================
procedure TSources.Init(const ADatabase: TADOConnection; const iTableName: string;
  const iUAL: TUserAccessLevel; iNextKey: TGetNextKey; iRegDropComp: TRegisterDropComponent;
  const ASiteID: string; ADefaultPath: string);
begin
  if FAlreadyShown then
    raise ESourcesError.Create(ResStr_TooLate);
  FDatabase        := ADatabase;
  FSourceJoinTable := iTableName;
  FUserAccessLevel := iUAL;
  FNextKey         := iNextKey;
  FRegisterDropComponent := iRegDropComp;
  FSiteID          := ASiteID;
  FDefaultPath := ADefaultPath;
end;  // Init

//==============================================================================
procedure TSources.SetDatabase(const ADatabase: TADOConnection);
begin
  FDatabase := ADatabase;
  if FAlreadyShown then begin
    FdmSources.UpdateDataSets(FDatabase);
    RefreshLists;
  end;
end;  // SetDatabase

//==============================================================================
{ Free the data module and reference lists if created }
destructor TSources.Destroy;
begin
  if FdmSources<>nil then FdmSources.Free;
  if FExternalRefList<>nil then FExternalRefList.Free;
  if FInternalRefList<>nil then FInternalRefList.Free;
  inherited Destroy;
end;  // Destroy

//==============================================================================
procedure TSources.SetupBevel;
begin
  FBevel := TBevel.Create(Self);
  with FBevel do begin
    Parent:= Self;
    Align := alClient;
    Shape := bsFrame;
    OnResize := BevelResize;
  end;
end;  // SetupBevel

//==============================================================================
procedure TSources.SetupInternalLabel;
begin
  { At design time, the label must be owned by the component so it is not
      streamed in its own right.  At run time, owner is the form so it is
      accessible to com addins }
  if csDesigning in ComponentState then
    FlblInternal := TLabel.Create(Self)
  else
    FlblInternal := TLabel.Create(Owner);
  with FlblInternal do begin
    Name := 'FlblInternalDoc';
    Parent := Self;
    SetBounds(12, 13, 99, 13);
    Caption := ResStr_InternalDocuments + ':';
  end;
end;  // SetupInternalLabel

//==============================================================================
procedure TSources.SetupInternalRefsGrid;
begin
  FsgInternalRefs := TStringGrid.Create(Self);
  with FsgInternalRefs do begin
    Parent:=Self;
    SetBounds(12, 28, Self.Width - 22, 97);
    ColCount  := 4;
    RowCount  := 2;
    FixedCols := 0;
    FixedRows := 1;
    DefaultRowHeight        :=  19;
    ColWidths[COL_ICON]     :=  18;
    ColWidths[COL_DOCUMENT] := 176;
    ColWidths[COL_ORIGINAL] :=  41;
    ColWidths[COL_TYPE]     :=  91;
    FsgInternalRefs.Options := [goFixedVertLine, goFixedHorzLine, goVertLine,
                                goHorzLine, goColSizing];
    Rows[0].CommaText := ResStr_GridCommaText;
    OnClick    := FsgInternalRefsClick;
    OnDblClick := FsgInternalRefsDblClick;
    OnDrawCell := FsgInternalRefsDrawCell;
    OnMouseMove := FsgInternalRefsMouseMove;
    Anchors := [akLeft, akRight, akTop];
  end;
end;  // SetupInternalRefsGrid

//==============================================================================
procedure TSources.SetupOriginalComboBox;
begin
  FcmbOriginal := TComboBox.Create(Self);
  with FcmbOriginal do begin
    Parent      := Self;  // FsgInternalRefs;
    Visible     := False;
    ParentCtl3D := False;
    Ctl3D       := False;
    BevelKind   := bkFlat;
    Style       := csDropDownList;
    OnChange    := FcmbOriginalChange;
  end;
end;  // SetupOriginalComboBox

//==============================================================================
procedure TSources.SetupFindInternalRefButton;
begin
  FbbFindInternalRef := TBitBtn.Create(Self);
  with FbbFindInternalRef do begin
    Parent := Self;
    SetBounds( 273, 127, 24, 23);
    Glyph.LoadFromResourceName(Hinstance, 'BMP_FIND');
    NumGlyphs := 2;
    OnClick:= bbFindInternalClick;
    Anchors := [akRight, akTop];
  end;
end;  // SetupFindInternalRefButton

//==============================================================================
procedure TSources.SetupAddInternalRefButton;
begin
  FbbAddInternalRef := TBitBtn.Create(Self);
  with FbbAddInternalRef do begin
    Parent := Self;
    SetBounds( 297, 127, 24, 23);
    Glyph.LoadFromResourceName(Hinstance, 'BMP_ADD');
    NumGlyphs := 2;
    OnClick := bbAddInternalClick;
    Anchors := [akRight, akTop];
  end;
end;  // SetupAddInternalRefButton

//==============================================================================
procedure TSources.SetupDelInternalRefButton;
begin
  FbbDelInternalRef := TBitBtn.Create(Self);
  with FbbDelInternalRef do begin
    Parent := Self;
    SetBounds( 321, 127, 24, 23);
    Glyph.LoadFromResourceName(Hinstance, 'BMP_DEL');
    NumGlyphs := 2;
    OnClick := bbRemoveInternalClick;
    Anchors := [akRight, akTop];
  end;
end;  // SetupDelInternalRefButton

//==============================================================================
procedure TSources.SetupInternalRefsShape;
begin
  FshpInternalRefs:=TShape.Create(Self);
  with FshpInternalRefs do begin
    Parent  := Self;
    SetBounds(11, 27, Self.Width - 20, 99);
    Tag     := 2;
    Anchors := [akLeft, akRight, akTop];
  end;
end;  // SetupInternalRefsShape

//==============================================================================
procedure TSources.SetupExternalLabel;
begin
  { At design time, the label must be owned by the component so it is not
      streamed in its own right.  At run time, owner is the form so it is
      accessible to com addins }
  if csDesigning in ComponentState then
    FlblExternal := TLabel.Create(Self)
  else
    FlblExternal := TLabel.Create(Owner);
  with FlblExternal do begin
    Name := 'FlblExternalRef';
    Parent := Self;
    SetBounds(12, 144, 96, 13);
    Caption := ResStr_ExternalReferences + ':';
  end;
end;  // SetupExternalLabel

//==============================================================================
procedure TSources.SetupExternalRefsListBox;
begin
  FlbExternalRefs := TListBox.Create(Self);
  with FlbExternalRefs do begin
    Parent := Self;
    SetBounds( 12, 160, Self.Width-22, Self.Height-196);
    OnClick    := FlbExternalRefsClick;
    OnDblClick := ShellOrEditExternalRef;
    Anchors := [akLeft, akRight, akTop];
  end;
end;  // SetupExternalRefsListBox

//==============================================================================
procedure TSources.SetupViewExternalRefButton;
begin
  FbbViewExternalRef := TBitBtn.Create(Self);
  with FbbViewExternalRef do begin
    Parent    :=Self;
    SetBounds( 273, Self.Height-34, 24, 23);
    Caption   :='...';  // ellipses button
    Font.Style:=[fsBold];  // Try to make it more visible.
    OnClick   :=ShellOrEditExternalRef;
    Anchors := [akRight, akBottom];
  end;
end;  // SetupViewExternalRefButton

//==============================================================================
procedure TSources.SetupAddExternalRefButton;
begin
  FbbAddExternalRef := TBitBtn.Create(Self);
  with FbbAddExternalRef do begin
    Parent := Self;
    SetBounds( 297, Self.Height-34, 24, 23);
    Glyph.LoadFromResourceName(Hinstance, 'BMP_ADD');
    NumGlyphs := 2;
    OnClick := bbAddExternalClick;
    Anchors := [akRight, akBottom];
  end;
end;  // SetupAddExternalRefButton

//==============================================================================
procedure TSources.SetupDelExternalRefButton;
begin
  FbbDelExternalRef := TBitBtn.Create(Self);
  with FbbDelExternalRef do begin
    Parent := Self;
    SetBounds( 321, Self.Height-34, 24, 23);
    Glyph.LoadFromResourceName(Hinstance, 'BMP_DEL');
    NumGlyphs := 2;
    OnClick := bbRemoveExternalClick;
    Anchors := [akRight, akBottom];
  end;
end;  // SetupDelExternalRefButton

//==============================================================================
procedure TSources.SetupExternalRefsShape;
begin
  FshpExternalRefs:=TShape.Create(Self);
  with FshpExternalRefs do begin
    Parent   :=Self;
    SetBounds(11,159,Self.Width-20,Self.Height-194);
    Tag      :=2;
    Anchors := [akLeft, akRight, akTop];
  end;
end;  // SetupExternalRefsShape

//==============================================================================
//==============================================================================
procedure TSources.CreateObjects;
var lCursor:TCursor;
begin
  if not FAlreadyShown and not (csDesigning in ComponentState)  then
  begin
    lCursor:=Screen.Cursor;
    Screen.Cursor:=crHourglass;
    try
      FAlreadyShown := True;
      if FSourceJoinTable = '' then
        raise ESourcesError.Create(ResStr_NoSourcesJoin);
      if FSourcesFromKey = '' then
        raise ESourcesError.Create(ResStr_NoKey);
      // Create when displaying for the first time
      FdmSources := TdmSources.Create(Self);

      FdmSources.UpdateDataSets(FDatabase);

      FdmSources.NextKey := FNextKey;
      FdmSources.JoinTable := FSourceJoinTable;
      FExternalRefList := TExternalRefList.Create(FSourcesFromKey,
                                                  FlbExternalRefs.Items,
                                                  FdmSources);
      FExternalRefList.DefaultPath := FDefaultPath;
      FInternalRefList := TInternalRefList.Create(FSourcesFromKey,
                                                  FsgInternalRefs,
                                                  FdmSources);
      // Populate the lists properly
      RefreshLists;
      // Register components for Drag/Drop operations
      RegisterDragDrop;
      // Update Enabled/Disabled state of buttons
      EditMode:=EditMode;
    finally
      Screen.Cursor:=lCursor;
    end;
  end;
end;  // CreateObjects

//==============================================================================
procedure TSources.WMPaint(var Message: TWMPaint);
begin
  if csDestroying in ComponentState then Exit;
  
  if (FSourceJoinTable <> '') and (FSourcesFromKey <> '') then
    // If first time and we have enough information, need to prepare
    // everything before showing   
    CreateObjects
  else
    // Just update enabled state of buttons    
    EditMode := EditMode;
  // Now paint it
  PaintHandler(Message);
end;  // WMPaint

//==============================================================================
{ Accessor method for SourceJoinTable - can't be set after component is running }
procedure TSources.SetSourceJoinTable(const Value: string);
begin
  if FAlreadyShown then
    raise ESourcesError.Create(ResStr_TooLate);
  FSourceJoinTable := Value;
end;  // SetSourceJoinTable

//==============================================================================
procedure TSources.SetSourcesFromKey(const Value: TKeyString);
begin
  FSourcesFromKey := Value;
end;  // SetSourcesFromKey

//==============================================================================
procedure TSources.FsgInternalRefsClick(Sender:TObject);
var
  lCell: TPoint;
  lCustodian: string;
begin
  inherited;
  if (EditMode = emView) or
     ((FInternalRefList <> nil) and (FInternalRefList.ItemCount = 0)) then
  begin
    FbbDelInternalRef.Enabled := false;
    FcmbOriginal.Visible      := false;
  end else
  if FInternalRefList <> nil then begin
    lCustodian := FInternalRefList.GetDataItem(Pred(FsgInternalRefs.Row)).Custodian;
    FbbDelInternalRef.Enabled := (EditMode = emAdd) or
                                 ((EditMode = emEdit) and (FUserAccessLevel > ualAddOnly)
                                 and ((lCustodian = FSiteID) or (lCustodian = '')));
    FcmbOriginal.Visible := false;
  end;

  with FsgInternalRefs do begin
    lCell := ScreenToClient(Mouse.CursorPos);
    MouseToCell(lCell.X, lCell.Y, lCell.X, lCell.Y);
    // When goRowSelect in options, Col always = 0
    if (lCell.X = 0) and (lCell.Y > 0) and (Cells[lCell.X, lCell.Y] <> '') then
      ShellFile(Cells[lCell.X, lCell.Y]);
  end;
end;  // FsgInternalRefsClick

//==============================================================================
procedure TSources.FsgInternalRefsDblClick(Sender:TObject);
begin
  inherited;
  if (FInternalRefList <> nil) then
    with FsgInternalRefs do
      if (FInternalRefList.Count > 0) and (Row > 0) then
      begin
        if Assigned(FShowSourceEvent) then
          FShowSourceEvent(Sender, TInternalRefItem(Objects[0, Row]).ItemKey);
      end;
end;  // FsgInternalRefsClick

{-------------------------------------------------------------------------------
  When the bevelled area is expanded, the contents expand to match it.
}
procedure TSources.BevelResize(Sender: TObject);
var
  availableHeight, halfHeight: Integer;
begin
  availableHeight := FBevel.Height - FlblExternal.Height
                                   - FlblInternal.Height
                                   - FbbAddInternalRef.Height
                                   - FbbAddExternalRef.Height
                                   - 3 * 2  //Spacing below labels
                                   - 8 * 2; //Spacing on top and bottom
  halfHeight := availableHeight div 2;

  FlblInternal.Top        := FBevel.Top + 8;
  FsgInternalRefs.Top     := FlblInternal.Top + FlblInternal.Height + 4;
  FshpInternalRefs.Top    := FsgInternalRefs.Top - 1;
  FsgInternalRefs.Height  := halfHeight - 2; // Allow space for shape to be seen.
  FshpInternalRefs.Height := halfHeight;
  FbbFindInternalRef.Top  := FshpInternalRefs.Top + halfHeight;
  FbbAddInternalRef.Top   := FbbFindInternalRef.Top;
  FbbDelInternalRef.Top   := FbbFindInternalRef.Top;
  FlblExternal.Top        := FbbFindInternalRef.Top + FbbFindInternalRef.Height;
  FlbExternalRefs.Top     := FlblExternal.Top + FlblExternal.Height + 4;
  FlbExternalRefs.Height  := halfHeight - 2;
  FshpExternalRefs.Top    := FlbExternalRefs.Top - 1;
  FshpExternalRefs.Height := halfHeight;
  FbbViewExternalRef.Top  := FshpExternalRefs.Top + halfHeight;
  FbbAddExternalRef.Top   := FbbViewExternalRef.Top;
  FbbDelExternalRef.Top   := FbbViewExternalRef.Top;
end;

//==============================================================================
{ Show combo box when moving into the Original cell }
procedure TSources.FsgInternalRefsDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var lDataItem     :TInternalRefItem;
    tfShowComboBox:boolean;
begin
  inherited;
  lDataItem := nil;
  with FcmbOriginal do
    if Items.Count < 2 then
      Items.CommaText := ResStr_InternalRef_Original + ',' + ResStr_InternalRef_Not_Original;

  with FsgInternalRefs do begin
    if ARow > 0 then
      lDataItem := TInternalRefItem(Objects[0, ARow]);

    if (EditMode <> emView) and (FInternalRefList.ItemCount > 0) then begin
      if ARow > 0 then
        tfShowComboBox := FbbDelInternalRef.Enabled or (lDataItem.SourceKey = '')
      else
        tfShowComboBox := false;

      if tfShowComboBox and (Col = COL_ORIGINAL) then begin
        if (gdFocused in State) then begin
          FcmbOriginal.Left      := Rect.Left + Left + 1;
          FcmbOriginal.Top       := Rect.Top + Top + 1;
          FcmbOriginal.Width     := Rect.Right - Rect.Left + 2;
          FcmbOriginal.ItemIndex := FcmbOriginal.Items.IndexOf(Cells[COL_ORIGINAL, Row]);
          FcmbOriginal.Visible   := True;
        end;
        if (gdfocused in State) and (Col <> COL_ORIGINAL) then FcmbOriginal.Visible := False;
      end else
        FcmbOriginal.Visible := false;
    end;

    Canvas.FillRect(Rect);
    if (ACol = COL_ICON) and (Cells[COL_ICON, ARow] <> '') then
      with TPicture.Create do
        try
          if (CompareText('http', Copy(Cells[COL_ICON, ARow], 1, 4)) = 0) or
             (CompareText('www.', Copy(Cells[COL_ICON, ARow], 1, 4)) = 0) then
          begin
            // Get a fake file to find associated icon.
            if not FileExists(GetWindowsTempDir + '~~R6.html') then
              with TStringList.Create do
                try
                  SaveToFile(GetWindowsTempDir + '~~R6.html');
                finally
                  Free;
                end;
            Icon.Handle := GetFileNameIcon(GetWindowsTempDir + '~~R6.html', SHGFI_SMALLICON);
          end else
            Icon.Handle := GetFileNameIcon(Cells[COL_ICON, ARow], SHGFI_SMALLICON);
          Canvas.Draw(Rect.Left + 1, Rect.Top + 1, Icon);
        finally
          Free;
        end
    else
      DrawChoppedText(Cells[ACol, ARow], Canvas, Rect, 2);
  end;
end;  // sgInternalRefsDrawCell

{-------------------------------------------------------------------------------
}
procedure TSources.FsgInternalRefsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  lCol, lRow: Integer;
begin
  with FsgInternalRefs do begin
    MouseToCell(X, Y, lCol, lRow);
    if (lCol = COL_ICON) and (lRow > 0) then
      if Cells[lCol, lRow] <> '' then begin
        Cursor := crHandPoint;
        Exit;
      end;
    Cursor := crDefault;
  end;
end;

//==============================================================================
{ Accessor method for event property }
procedure TSources.SetFindInternalEvent(const Value: TNotifyEvent);
begin
  FFindInternalEvent := Value;
end;  // SetFindInternalEvent

//==============================================================================
procedure TSources.bbFindInternalClick(Sender: TObject);
begin
  if Assigned(FFindInternalEvent) then begin
    FFindInternalEvent(Self);
    FsgInternalRefsClick(nil);
  end;
end;  // bbFindInternalClick

//==============================================================================
procedure TSources.SetAddInternalEvent(const Value: TNotifyEvent);
begin
  FAddInternalEvent := Value;
end;  // SetAddInternalEvent

//==============================================================================
{Event raised when the user double clicks to show the source documents}
procedure TSources.SetShowSourceEvent(const Value: TShowSourceEvent);
begin
  FShowSourceEvent := Value;
end;


//==============================================================================
{ Event handler for add internal button click }
procedure TSources.bbAddInternalClick(Sender: TObject);
begin
  if Assigned(FAddInternalEvent) then begin
    FAddInternalEvent(Self);
    FsgInternalRefsClick(nil);
  end;
end;  // bbAddInternalClick

//==============================================================================
procedure TSources.bbRemoveInternalClick(Sender: TObject);
begin
  if Assigned(FsgInternalRefs.Objects[0, FsgInternalRefs.Row]) then
    if MessageDlg(ResStr_RemoevRefIntDoc, mtConfirmation,[mbNo, mbYes], 0) = mrYes then
    begin
      FInternalRefList.DeleteItem(FsgInternalRefs.Row);
      FsgInternalRefsClick(nil);
    end;
end;  // bbRemoveInternalClick

//==============================================================================
procedure TSources.FcmbOriginalChange(Sender: TObject);
begin
  with FsgInternalRefs do
    if Objects[0, Row] <> nil then
      TInternalRefItem(Objects[0, Row]).Original := FcmbOriginal.Text;
end;  // FcmbOriginalChange

//==============================================================================
procedure TSources.FlbExternalRefsClick(Sender:TObject);
var
  lCustodian: string;
begin
  if (EditMode = emView) then begin
    FbbViewExternalRef.Enabled := FlbExternalRefs.ItemIndex <> -1;
    FbbDelExternalRef.Enabled  := False;
  end else
  if ((FExternalRefList <> nil) and (FExternalRefList.ItemCount = 0)) then
  begin
    FbbViewExternalRef.Enabled := False;
    FbbDelExternalRef.Enabled  := False;
  end else
  if FExternalRefList <> nil then begin
    if FlbExternalRefs.ItemIndex = -1 then FlbExternalRefs.ItemIndex := 0;
    FbbViewExternalRef.Enabled := True;
    lCustodian := FExternalRefList.DataItems[FlbExternalRefs.ItemIndex].Custodian;
    FbbDelExternalRef.Enabled := (EditMode = emAdd) or
                                 ((EditMode = emEdit)and (FUserAccessLevel > ualAddOnly)
                                 and ((lCustodian = FSiteID) or (lCustodian = '')));
  end;
end;  // FlbExternalRefsClick

//==============================================================================
procedure TSources.DoShellFile(Sender:TObject);
begin
  try
    if FlbExternalRefs.ItemIndex>-1 then
      { Call library code to shell file then try quickview }
      ShellFile(TExternalRefItem(FlbExternalRefs.Items.Objects[FlbExternalRefs.ItemIndex]).FileName);
  except
    on EShellError do // tone down the exception
      MessageDlg(Format(ResStr_CannotFindFile, [FlbExternalRefs.Items[FlbExternalRefs.ItemIndex]]),
                 mtWarning, [mbOk], 0);
  end;
end;  // ShellFile

//==============================================================================
procedure TSources.bbAddExternalClick(Sender: TObject);
var lDataItem : TExternalRefItem;
  i: integer;
begin
  inherited;
  with TdlgExternalSourceDetails.Create(nil) do try
    DefaultPath := FDefaultPath;
    if ShowModal=mrOk then begin
      for i := 0 to FExternalRefList.ItemCount-1 do
        if TExternalRefItem(FExternalRefList.Items[i]).FileName = ePath.Text then begin
          ShowInformation(ResStr_DuplicateFile);
          Exit; // don't add to list
        end;
      if FlbExternalRefs.Items.IndexOf(ePath.Text)=-1 then begin
        lDataItem := TExternalRefItem.CreateNew(FExternalRefList);
        lDataItem.FileName := ePath.Text;
        lDataItem.Title := eTitle.Text;
        FExternalRefList.AddNew(lDataItem);
        FlbExternalRefsClick(nil);
      end;
    end;
  finally
    Free;
  end;
end;  // bbAddExternalClick

//==============================================================================
procedure TSources.bbRemoveExternalClick(Sender: TObject);
begin
  with FlbExternalRefs do
    if ItemIndex<>-1 then
      if MessageDlg(ResStr_RemoveRefToExtDoc, mtConfirmation,[mbNo, mbYes], 0) = mrYes then
      begin
        FExternalRefList.DeleteItem(ItemIndex);
        FlbExternalRefsClick(nil);
      end;
end;  // bbRemoveExternalClick

//==============================================================================
{ Set up drag drop for the contained controls }
procedure TSources.RegisterDragDrop;
begin
  if Assigned(FRegisterDropComponent) then begin
    FRegisterDropComponent(FsgInternalRefs, InternalRefsDrop, ['REFERENCE'],[CF_JNCCDATA]);
    FRegisterDropComponent(FlbExternalRefs, ExternalRefsDrop, ['SOURCE_FILE'], [CF_JNCCDATA, CF_HDROP]);
  end;
end;  // RegisterDragDrop

//==============================================================================
procedure TSources.InternalRefsDrop(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
begin
  if EditMode=emView then
    ioHandled:=true
  else if iFormat=CF_JNCCDATA then begin
    AddMultipleInternalRefs(iSourceData);
    ioHandled:=true;
  end else
    ioHandled:=false;
end;  // InternalRefsDrop

//==============================================================================
procedure TSources.InternalRefUpdate(KeyList: TKeyList);
begin
  if (EditMode<>emView) and (KeyList<>nil) and (KeyList.Header.ItemCount>0) then
    try
      AddMultipleInternalRefs(KeyList);
    finally
      KeyList.Free;
    end;
end;  // InternalRefUpdate

//==============================================================================
procedure TSources.AddInternalRef(const AKey:TKeyString);
var lKeyList:TEditableKeyList;
begin
  if AKey<>'' then begin
	  lKeyList:=TEditableKeyList.Create;
    try
  	  lKeyList.SetTable('REFERENCE');  // Might not be necessary in this particular case
      lKeyList.AddItem(AKey, '');
      AddMultipleInternalRefs(lKeyList);  // Adds only one, but re-use the function
    finally
      lKeyList.Free;
    end;
  end;
end;  // AddInternalRef

//==============================================================================
procedure TSources.ExternalRefsDrop(const Sender: TObject;
  const iFormat : integer; const iSourceData: TKeyList;
  const iTextStrings : TStringList; var ioHandled : boolean);
var
  lFileList : TStringList;
  lDataItem : TExternalRefItem;
  iCount    : integer;
begin
  if Editmode=emView then
    ioHandled := True
  else begin
    if iFormat = CF_JNCCDATA then
    begin
      if FdmSources = nil then
        raise ESourcesError.Create(ResStr_DMNil);
      lFileList := TStringList.Create;
      try
        FdmSources.ConvertKeysToFiles( iSourceData, lFileList);
        for iCount := 0 to lFileList.Count-1 do
          if FlbExternalRefs.Items.IndexOf(lFileList[iCount])=-1 then begin
            lDataItem := TExternalRefItem.CreateNew( FExternalReflist);
            lDataItem.FileName := lFileList[iCount];
            FExternalRefList.AddNew( lDataItem);
          end; // if
      finally
        lFileList.Free;
      end; // try
      ioHandled := True;
    end else
      if iFormat = CF_HDROP then
      begin
        { Add each file dropped }
        for iCount := 0 to iTextStrings.Count-1 do
          if FlbExternalRefs.Items.IndexOf(iTextStrings[iCount])=-1 then begin
            lDataItem := TExternalRefItem.CreateNew( FExternalReflist);
            lDataItem.FileName := iTextStrings[iCount];
            FExternalRefList.AddNew( lDataItem);
          end; // if
        ioHandled := True; // don't use default handling
      end; // if format
  end; // if EditMode
  FlbExternalRefsClick(nil);
end;  // ExternalRefsDrop

//==============================================================================
{ Toggle controls as required for editing }
procedure TSources.SetEditMode(const Value: TEditMode);
var tfOn:boolean;
begin
  inherited SetEditMode(Value);
  tfOn := Value <> emView;
  FcmbOriginal.Visible       := False;
  FbbFindInternalRef.Enabled := tfOn;
  FbbAddInternalRef.Enabled  := tfOn;
  FsgInternalRefsClick(nil);
  FbbAddExternalRef.Enabled  := tfOn;
  FlbExternalRefsClick(nil);
  with FsgInternalRefs do
    if tfOn then Options := Options - [goRowSelect]
            else Options := Options + [goRowSelect];
end;  // SetEditmode

//==============================================================================
procedure TSources.RefreshLists;
begin
  if FAlreadyShown then begin  // Don't refresh if nothing exists yet
    FExternalRefList.RefreshList(FSourcesFromKey);
    FInternalRefList.RefreshList(FSourcesFromKey);
    SetEditMode(EditMode);
  end;
end;  // RefreshLists

//==============================================================================
{ Update any of the lists to the database as required }
procedure TSources.Post;
begin
  if (FExternalRefList<>nil) and (FInternalRefList<>nil) then begin
    if FExternalRefList.Changed then FExternalRefList.Update;
    if FInternalRefList.Changed then FInternalRefList.Update;
    SetEditMode(EditMode);
  end;
end;  // Post;

//==============================================================================
function TSources.CheckInternalKey(const AKey:TKeyString):boolean;
var lCount:integer;
begin
  Result:=false;
  // If list empty then not found
  if FInternalRefList.ItemCount=0 then
    Result:=false
  else  // Check each item in grid with new key
    with FsgInternalRefs do
      for lCount:=FixedRows to RowCount-1 do
        Result:=Result or (TInternalRefItem(Objects[0,lCount]).ItemKey=AKey);
end;  // CheckInternalKey

//==============================================================================
procedure TSources.AddMultipleInternalRefs(const iKeyList:TKeyList);
var iCount   :integer;
    lDataItem:TInternalRefItem;
    lreTemp  :TRichEdit;
    stRef    :string;
begin
  lreTemp:=TRichEdit.Create(Self);
  try
    lreTemp.Parent:=Self;
    lreTemp.Visible:=false;
    for iCount:=0 to iKeyList.Header.ItemCount-1 do begin
      // Find the Item in the list.
      if not CheckInternalKey(iKeyList.Items[iCount].KeyField1) then
        with FdmSources.qryGetRef do begin
          Parameters.ParamByName('KeyParameter').Value := iKeyList.Items[iCount].KeyField1;
          Open;
          // Get first line of the full reference.
          //Should be no need to test for carrige return as references screen now chops them off
          lreTemp.Lines.Assign(FieldByName('Full_Reference'));
          if (Pos(#13,lreTemp.Text) = 0) then
            stRef:=lreTemp.Text
          else
            stRef:=Copy(lreTemp.Text,1,Pos(#13,lreTemp.Text)-1);

          lDataItem:=TInternalRefItem.CreateNew(FInternalRefList);
          lDataItem.SetupProperties(iKeyList.Items[iCount].KeyField1, 'No', stRef,
                                    FieldByName('Reference_Type').AsString,
                                    FieldByName('Original_File').AsString);
          FInternalRefList.AddNew(lDataItem);
          Close;
        end;
    end;
  finally
    lreTemp.Free;
  end;
  FsgInternalRefsClick(nil);
end;  // AddMultipleInternalRefs

//==============================================================================
function TSources.GetExternalSourceKey: TKeyString;
var lIdx:integer;
begin
  // Make sure the lists are created before trying to access them
  CreateObjects;
  Result:='';
  lIdx:=FlbExternalRefs.ItemIndex;
  with FlbExternalRefs.Items do
    if Count>0 then
      if lIdx<>-1 then Result:=TExternalRefItem(Objects[lIdx]).ItemKey
                  else Result:=TExternalRefItem(Objects[0]).ItemKey;
end;  // GetExternalSourceKey

//==============================================================================
function TSources.GetInternalSourceKey: TKeyString;
begin
  // Make sure the lists are created before trying to access them
  CreateObjects;
  Result:='';
  if FInternalRefList.ItemCount>0 then
    with FsgInternalRefs do
      Result:=TInternalRefItem(Objects[0,Row]).ItemKey;
end;  // GetInternalSourceKey

//==============================================================================
procedure TSources.GetInternalSources(var ioSourceList: TEditableKeyList);
var lCount:integer;
begin
  // Make sure the lists are created before trying to access them
  CreateObjects;
  if ioSourceList=nil then ioSourceList:=TEditableKeyList.Create;

  if FInternalRefList.ItemCount>0 then
    with FsgInternalRefs do
      for lCount:=1 to RowCount-1 do
        ioSourceList.AddItem(TInternalRefItem(Objects[0,lCount]).ItemKey,'');
end;  // GetInternalSources

//==============================================================================
// This procedure should only be called when adding a main record where the
// main key is no longer valid, as is the case when 2 users save at the same time
procedure TSources.ResetKeys(const AKey: TKeyString);
begin
  // Make sure the lists are created before trying to access them
  CreateObjects;
  SourcesFromKey:=AKey;
  FInternalRefList.SourcesForKey:=AKey;
  FExternalRefList.SourcesForKey:=AKey;
end;  // ResetKeys

{-------------------------------------------------------------------------------
   Handle double clicks on an external ref item or clicking the ... button. If in edit
   mode, displays the details dialog so you can overwrite the title. Else shells the file.
 }
procedure TSources.ShellOrEditExternalRef(Sender:TObject);
var
  item: TExternalRefItem;
begin
  if FlbExternalRefs.ItemIndex>=0 then begin
    if EditMode=emView then
      DoShellFile(Sender)
    else begin
      with TdlgExternalSourceDetails.Create(nil) do try
        DefaultPath := FDefaultPath;
        item := TExternalRefItem(FlbExternalRefs.Items.Objects[FlbExternalRefs.ItemIndex]);
        ePath.Text := item.FileName;
        eTitle.Text := item.Title;
        if ShowModal=mrOk then begin
          item.FileName := ePath.Text;
          item.Title := eTitle.Text;
        end;
      finally
        Free;
      end;
    end;
  end;
end;

//==============================================================================



end.
