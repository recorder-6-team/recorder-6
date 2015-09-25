//==============================================================================
//  Unit:        GoogleEarthExport
//
//  Implements:  TdlgGoogleEarthExport
//
//  Description: Allows users to select which data from the report will be
//                included in the exported KML file.
//
//  Author:      David Kelly
//  Created:     16 September 2007
//
//  Last Revision Details:
//    $Revision: 12 $
//    $Date: 1/10/09 9:55 $
//    $Author: Simonlewis $
//
//  $History: GoogleEarthExport.pas $
//  
//  *****************  Version 12  *****************
//  User: Simonlewis   Date: 1/10/09    Time: 9:55
//  Updated in $/JNCC/Development/Build/Source
//  Changed the DoExport routine from a procedure to a function; it now
//  returns the file name of the export file, which is used in FilterResult
//  to save metadata.
//
//  *****************  Version 11  *****************
//  User: Johndurman   Date: 15/05/08   Time: 11:45
//  Updated in $/JNCC/Development/Build/Source
//  Fixed code in the FormatForXml method (caused by lack of understanding
//  of Delphi strings)
//  
//  *****************  Version 10  *****************
//  User: Johndurman   Date: 15/05/08   Time: 10:28
//  Updated in $/JNCC/Development/Build/Source
//  Bug fixed having added destructor.
//  
//  *****************  Version 9  *****************
//  User: Johndurman   Date: 13/05/08   Time: 15:46
//  Updated in $/JNCC/Development/Build/Source
//  Destructor added to form to prevent memory leaks from TStringList
//  members.
//  
//  *****************  Version 8  *****************
//  User: Davidkelly   Date: 11/01/08   Time: 11:34
//  Updated in $/JNCC/Development/Build/Source
//  VI 14902: PointNode must use . as the decimal separator, regardless of
//  the locale settings.
//  
//  *****************  Version 7  *****************
//  User: Davidkelly   Date: 10/01/08   Time: 11:54
//  Updated in $/JNCC/Development/Build/Source
//  VI 14687: Description nodes now use FieldByName.Text rather than
//  FieldByName.AsString, which fixes the vague dates problem. New
//  FormatForXml method fixes the invalid character problems.
//  
//  *****************  Version 6  *****************
//  User: Ericsalmon   Date: 13/11/07   Time: 12:17
//  Updated in $/JNCC/Development/Build/Source
//  VI 14018. Fixed google earth export.
//  
//  *****************  Version 5  *****************
//  User: Ericsalmon   Date: 13/11/07   Time: 11:21
//  Updated in $/JNCC/Development/Build/Source
//  VI 14017. Fix for file extension.
//  
//  *****************  Version 4  *****************
//  User: Ericsalmon   Date: 13/11/07   Time: 11:07
//  Updated in $/JNCC/Development/Build/Source
//  VI 14016. Forced folders on save.
//  
//  *****************  Version 3  *****************
//  User: Johnvanbreda Date: 8/11/07    Time: 11:25
//  Updated in $/JNCC/Development/Build/Source
//  Google Earth code review updates
//
//  *****************  Version 2  *****************
//  User: Davidkelly   Date: 21/09/07   Time: 12:14
//  Updated in $/JNCC/Development/Build/Source
//  Made some improvements to the code by refactoring some methods.
//==============================================================================

unit GoogleEarthExport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, CheckLst, DBGrids, ApplicationSettings,
  xmldom, XMLIntf, msxmldom, XMLDoc, Grids, ImageListButton, ExceptionForm, VagueDate, DB;

resourcestring
  ResStr_SelectTitle = 'Please select a title for the placemarks';
  ResStr_SpatialFieldsNotAvailable = 'There are no spatial reference fields in the dataset to export';
  ResStr_CreatingKmlFile = 'Creating KML file...';

type
  EGoogleEarth = class(TExceptionPath);

  TdlgGoogleEarthExport = class(TForm)
    cmbPlacemarkName: TComboBox;
    clbDescriptionFields: TCheckListBox;
    btnOK: TImageListButton;
    btnCancel: TImageListButton;
    Label1: TLabel;
    Label2: TLabel;
    dlgSaveKML: TSaveDialog;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FColumns: TStringList;
    FFields: TStringList;
    procedure AddPlacemark(AGrid:TDBGrid; DocNode: IXMLNode);
    procedure AddDescriptionNode(AGrid: TDBGrid; PlaceNode: IXMLNode);
    procedure AddPointNode(AGrid: TDBGrid; PlaceNode: IXMLNode);
    function FormatForXml(AString: String): String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Populate(ADBGrid: TDBGrid);
    function DoExport(AGrid: TDBGrid): String;
  end;

var
  dlgGoogleEarthExport: TdlgGoogleEarthExport;

implementation

uses FormActions, JNCCDatasets, MainTBar, GeneralFunctions;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Constructor
}
constructor TdlgGoogleEarthExport.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TStringList.Create;
  FFields := TStringList.Create;
end;

{-------------------------------------------------------------------------------
  Constructor
}
destructor TdlgGoogleEarthExport.Destroy;
begin
  FColumns.Free;
  FFields.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Populate the combobox, checklistbox, and hidden stringlists
}
procedure TdlgGoogleEarthExport.Populate(ADBGrid: TDBGrid);
var
  i: Integer;
begin
  with ADBGrid do begin
    for i := 0 to Columns.Count - 1 do begin
      FColumns.Add(Columns[i].Title.Caption);
      FFields.Add(Columns[i].FieldName);
      if Columns[i].Visible then begin
        cmbPlacemarkName.Items.Add(Columns[i].Title.Caption);
        clbDescriptionFields.Items.Add(Columns[i].Title.Caption);
      end;
    end;
  end;
end;
 
{-------------------------------------------------------------------------------
  Show save dialog. Initial directory and filename are standard export names
}
procedure TdlgGoogleEarthExport.btnOKClick(Sender: TObject);
var
  ltfSave: Boolean;
begin
  if cmbPlacemarkName.Text = '' then
    ShowInformation(ResStr_SelectTitle)
  else begin
    with dlgSaveKML do begin
      InitialDir := AppSettings.ReportPath + 'Output\';
      Filter := 'KML Files | *.kml';
      FileName := AppSettings.ReportPath + 'Output\Export.kml';
      ltfSave := Execute;
    end;
    if ltfSave then begin
      // Ensure folders exist.
      if ExtractFilePath(dlgSaveKML.FileName) <> '' then
        ForceDirectories(ExtractFilePath(dlgSaveKML.FileName));
      // Ensure extension is KML.
      if not SameText(ExtractFileExt(dlgSaveKML.FileName), '.kml') then
        dlgSaveKML.FileName := dlgSaveKML.FileName + '.kml';
      ModalResult := mrOK;
    end;
  end;
end;
 
{-------------------------------------------------------------------------------
}
procedure TdlgGoogleEarthExport.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
  
{-------------------------------------------------------------------------------
  Write the data to a KML file. Returns the full path if the file was saved, or
  an empty string otherwise.
}
function TdlgGoogleEarthExport.DoExport(AGrid: TDBGrid): String;
var
  XMLDoc: TXMLDocument;
  RootNode, DocNode: IXMLNode;
  i: Integer;
begin
  Result := '';
  frmMain.SetStatus(ResStr_CreatingKmlFile);
  try
    XMLDoc := TXMLDocument.Create(self);
    XMLDoc.Active := True;
    RootNode := XMLDoc.AddChild('kml');
    RootNode.Attributes['xmlns'] := 'http://earth.google.com/kml/2.0';
    // Document node allows more than one placemark per file
    DocNode := RootNode.AddChild('Document');
    if not gAssistor.HasSpatialFields(AGrid.DataSource.DataSet) then
      raise EGoogleEarth.Create(ResStr_SpatialFieldsNotAvailable);
    // Make sure we start at the top!
    AGrid.DataSource.DataSet.First;
    for i := 0 to AGrid.DataSource.DataSet.RecordCount - 1 do begin
      frmMain.SetProgress(i*100 div AGrid.DataSource.DataSet.RecordCount);
      AddPlacemark(AGrid, DocNode);
      AGrid.DataSource.DataSet.Next;
    end;
    XMLDoc.SaveToFile(dlgSaveKML.FileName);
    FColumns.Free;
    FColumns := nil;
    FFields.Free;
    FFields := nil;
    Result := dlgSaveKML.FileName;
  finally
    frmMain.TaskFinished;
  end;
end;
 
{-------------------------------------------------------------------------------
  Add a placemark to the KML file for this row of the grid
}
procedure TdlgGoogleEarthExport.AddPlacemark(AGrid: TDBGrid; DocNode: IXMLNode);
var
  i: Integer;
  str: string;
  PlaceNode, ANode: IXMLNode;
begin
  i := FColumns.IndexOf(cmbPlacemarkName.Text);
  str := FFields[i];
  PlaceNode := DocNode.AddChild('Placemark');
  ANode := PlaceNode.AddChild('name');
  ANode.Text := FormatForXml(AGrid.DataSource.DataSet.FieldByName(str).Text);
  // Set placemarks to be visible when the file is opened in Google Earth
  ANode := PlaceNode.AddChild('visibility');
  ANode.Text := '1';

  AddDescriptionNode(AGrid, PlaceNode);
  AddPointNode(AGrid, PlaceNode);
end;

{-------------------------------------------------------------------------------
  Loop through the checked items in the list and add them to the description node
}
procedure TdlgGoogleEarthExport.AddDescriptionNode(AGrid: TDBGrid; PlaceNode: IXMLNode);
var
  ANode: IXMLNode;
  i, j: Integer;
  str, lFieldText: string;
begin
  ANode := PlaceNode.AddChild('description');
  for j := 0 to clbDescriptionFields.Items.Count - 1 do begin
    i := FColumns.IndexOf(clbDescriptionFields.Items.Strings[j]);
    str := FFields[i];
    lFieldText := AGrid.DataSource.DataSet.FieldByName(str).Text;
    if clbDescriptionFields.Checked[j] then begin
      if ANode.Text = '' then
        ANode.Text := FormatForXml(clbDescriptionFields.Items.Strings[j] +
            ': ' + lFieldText)
      else
        // Add line breaks between each field in the description
        ANode.Text := ANode.Text + #13#10 + FormatForXml(clbDescriptionFields.Items.Strings[j] +
           ': ' + lFieldText);
    end;
  end;
end; 

{-------------------------------------------------------------------------------
  Add the point node
}
procedure TdlgGoogleEarthExport.AddPointNode(AGrid: TDBGrid; PlaceNode: IXMLNode);
var
  PointNode, ANode: IXMLNode;
  lLongText, lLatText: String;
begin
  PointNode := PlaceNode.AddChild('Point');
  ANode := PointNode.AddChild('coordinates');
  lLongText := StringReplace(AGrid.DataSource.DataSet.FieldByName(
      gAssistor.LongitudeFieldName(AGrid.DataSource.DataSet)).Text,
      DecimalSeparator, '.', [rfReplaceAll]);
  lLatText := StringReplace(AGrid.DataSource.DataSet.FieldByName(
      gAssistor.LatitudeFieldName(AGrid.DataSource.DataSet)).Text,
      DecimalSeparator, '.', [rfReplaceAll]);
  ANode.Text := FormatForXml(lLongText + ',' + lLatText);
end;

{-------------------------------------------------------------------------------
  Remove any invalid characters from the string.
}
function TdlgGoogleEarthExport.FormatForXml(AString: String): String;
var
  lStr: String;
  i: Integer;
begin
  lStr := '';                
  for i := 1 to Length(AString) do
    // Characters not in this set are not allowed in XML, and cause errors if you try
    // to save them there. Strip them out and return what remains.
    if (Ord(AString[i]) >= 32) or (Ord(AString[i]) = 9)
        or (Ord(AString[i]) = 10) or (Ord(AString[i]) = 13) then
      lStr := lStr + AString[i];
  Result := lStr;
end;

end.
