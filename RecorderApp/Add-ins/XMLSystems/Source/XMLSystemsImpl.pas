{===============================================================================
  Unit:        XMLSystemsImpl

  Defines:     TXMLSystems
               TSpatialSystem
               TBaseMapSystem

  Description: Provides support for the native mapping formats required for
               distribution mapping and data entry of German field data.

  Created:     Feb 2007

  Last revision information:
    $Revision: 45 $
    $Date: 9/04/10 15:07 $
    $Author: Andrewkemp $

===============================================================================}
unit XMLSystemsImpl;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  SysUtils, ComObj, ActiveX, XMLSystems1_TLB, StdVcl, Recorder2000_TLB, Classes, Math,
  geodll32, Types, VersionInfo, Windows, Registry, xmldom, XMLIntf, msxmldom, XMLDoc,
  ExceptionForm, SpatialRefFuncs, MSScriptControl_TLB, StrUtils, Dialogs, Contnrs;

resourcestring
  ResStr_FailedToInit = 'Failed to initialise XML Systems addin. Error:'#13#10'%s';
  ResStr_XMLSystemsAddinDescription =
      'Addin providing support for reading XML '
      + 'documents containing spatial reference systems.'#13#10'Version %s';
  ResStr_XMLSystemsAddinName = 'Multiple Spatial Reference System Add-in';
  ResStr_InputRefNotCorrectFormat =
      'The spatial reference "%s" is not of a recognised format for this system.';
  ResStr_RequiredFilesMissing =
      'The addin folder you are installing from does '
      + 'not contain the required GeoDLL deployment files.';
  ResStr_ErrorInScript =
      'An error of type "%s" occurred in the script defining the %s system.';
  ResStr_InvalidCustomMethod =
      'The XML file for reference system %s specifies a custom '
      + 'calculation method with a name %s which is invalid.';
  ResStr_InvalidCoordRefsys =
      'The XML file for reference system %s specifies a %s parameter '
      + 'value of %s which is invalid.';
  ResStr_FailedRegistryAccess = 'Failed to access Recorder 6 registry settings';

type
  EXMLSystemsException = class(TExceptionPath);

  {-----------------------------------------------------------------------------
  }
  TLatLong = record
    Lat, Long: Double;
  end;

  {-----------------------------------------------------------------------------
  }
  TCoord = record
    x, y: Double;
  end;

  {-----------------------------------------------------------------------------
  }
  TScale = class
  private
    FText: String;
    FScale: TCoord;
  public
    constructor Create(const AText, AX, AY: String);
    property Text: String read FText;
    property Scale: TCoord read FScale;
  end;

  {-----------------------------------------------------------------------------
  }
  TGridScale = class(TScale)
  private
    FOffset: TCoord;
  public
    constructor Create(const AText, AX, AY, AOffX, AOffY: String);
    property Offset: TCoord read FOffset;
  end;

  {-----------------------------------------------------------------------------
  }
  TXMLSystems = class(TAutoObject, IXMLSystems, IRecorderAddin, ISpatialReferenceList,
                      IBaseMapFormatList)
  private
    FAddinPath: string;
    FInstallPath: string;
    FLastError: string;
    FSpatialRefInterfaces: TInterfaceList;
    FBaseMapInterfaces: TInterfaceList;
    FSystemsInitialised: Boolean;
    function GetAddinPath: string;
    function GetInstallPath: string;
    function GetRegistrySetting(const AValue: string): string;
    procedure InitialiseSystems;
    property AddinPath: string read GetAddinPath;
    property InstallPath: string read GetInstallPath;
  protected
    {IRecorderAddin}
    function Get_Name: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_ImageFileName: WideString; safecall;
    procedure Install(const iInstalledFilePath: WideString); safecall;
    {ISpatialReferenceList}
    function Get_SystemCount: Integer; safecall;
    function Get_SystemInterface(AIndex: Integer): ISpatialReference; safecall;
    {IBaseMapFormatList}
    function Get_Count: Integer; safecall;
    function Get_SystemBaseMapFormat(AIndex: Integer): IBaseMapFormat; safecall;
  public
    procedure Initialize; override;
  end;

  {-----------------------------------------------------------------------------
  }
  TSpatialSystem = class(TAutoObject, ISpatialReference, ISpatialReference6, INamedSpatialReference)
  private
    FCachedInputLatLong: TLatLong;
    FCachedInputExampleCoords: TCoord;
    FCachedOutputLatLong: TLatLong;
    FCachedOutputExampleCoords: TCoord;
    FLastError: string;
    FName: String;
    FAbbrev: String;
    FEquivalentBaseMapSystem: String;
    FCoordSys: Integer;
    FRefSys: Integer;
    FStripNo: Integer;
    FXOrigin: Double;
    FYOrigin: Double;
    FScriptControl: TScriptControl;
    function CoordToLatLong(ACoords: TCoord): TLatLong;
    function LatLong(ALat, ALong: double): TLatLong;
    function LatLongToCoord(ALatLong: TLatLong): TCoord;
    procedure Configure(xmlDoc: IXMLDocument);
    function SpatialRefToLatLong(const ARef: string): TLatLong;
    function SplitSpatialRef(const ARef: string): TCoord;
  protected
    {INamedSpatialReference}
    function Get_SpatialSystemName: WideString; safecall;
    {ISpatialReference}
    function Get_SpatialRefSystem: WideString; safecall;
    function ConvertToLat(const iSpatialRef: WideString): WideString; safecall;
    function ConvertToLong(const iSpatialRef: WideString): WideString; safecall;
    function ConvertFromLatLong(const iLat: WideString; const iLong: WideString): WideString; safecall;
    function Get_GetLastError: WideString; safecall;
    function ValidSpatialRef(const iSpatialRef: WideString): WordBool; safecall;
    {ISpatialReference6}
    function Get_EquivalentBaseMapSystem: WideString; safecall;
  public
    constructor Create(xmlDoc: IXMLDocument);
    destructor Destroy; override;
  end;

  {-----------------------------------------------------------------------------
  }
  TBaseMapSystem = class(TAutoObject, ISpatialReference, ISpatialReference6, INamedSpatialReference,
                         IBaseMapFormat, IBaseMapScaleList, IBaseMapScaleList_614)
  private
    FCachedInputLatLong: TLatLong;
    FCachedInputExampleCoords: TCoord;
    FCachedOutputLatLong: TLatLong;
    FCachedOutputExampleCoords: TCoord;
    FLastError: string;
    FMapCoordsPerMetre: Double;
    FName: String;
    FAbbrev: String;
    FEquivalentBaseMapSystem: String;
    FBaseMap: String;
    FCoordSys: Integer;
    FRefSys: Integer;
    FStripNo: Integer;
    FDisplayGrids: Boolean;
    FDistributionScales: TObjectList;
    FGridLineScales: TObjectList;
    function CoordToLatLong(ACoords: TCoord): TLatLong;
    function LatLong(ALat, ALong: double): TLatLong;
    function LatLongToCoord(ALatLong: TLatLong): TCoord;
    function Coord(AX, AY: double): TCoord;
    procedure Configure(xmlDoc: IXMLDocument);
    function SpatialRefToLatLong(const ARef: string): TLatLong;
    function SplitSpatialRef(const ARef: string): TCoord;
  protected
    {IBaseMapFormat}
    function MapCoordToLat(iEasting: Double; iNorthing: Double): WideString; safecall;
    function MapCoordToLong(iEasting: Double; iNorthing: Double): WideString; safecall;
    function Get_CanDisplayGrids: WordBool; safecall;
    function Get_MapCoordsPerMetre: Double; safecall;
    function LatLongToMapCoordX(iLat: Double; iLong: Double): Double; safecall;
    function LatLongToMapCoordY(iLat: Double; iLong: Double): Double; safecall;
    {IBaseMapScaleList}
    function Get_GridScaleCount: Integer; safecall;
    function Get_GridScaleCaption(Index: Integer): WideString; safecall;
    function Get_GridScaleX(Index: Integer): Double; safecall;
    function Get_GridScaleY(Index: Integer): Double; safecall;
    function Get_PointSizeCount: Integer; safecall;
    function Get_PointSizeCaption(Index: Integer): WideString; safecall;
    function Get_PointSizeX(Index: Integer): Double; safecall;
    function Get_PointSizeY(Index: Integer): Double; safecall;
    {IBaseMapScaleList}
    function Get_GridOffsetX(Index: Integer): Double; safecall;
    function Get_GridOffsetY(Index: Integer): Double; safecall;
    {INamedSpatialReference}
    function Get_SpatialSystemName: WideString; safecall;
    {ISpatialReference}
    function Get_SpatialRefSystem: WideString; safecall;
    function ConvertToLat(const iSpatialRef: WideString): WideString; safecall;
    function ConvertToLong(const iSpatialRef: WideString): WideString; safecall;
    function ConvertFromLatLong(const iLat: WideString; const iLong: WideString): WideString; safecall;
    function Get_GetLastError: WideString; safecall;
    function ValidSpatialRef(const iSpatialRef: WideString): WordBool; safecall;
    {ISpatialReference6}
    function Get_EquivalentBaseMapSystem: WideString; safecall;
  public
    constructor Create(xmlDoc: IXMLDocument);
    destructor Destroy; override;
  end;

//==============================================================================
implementation

uses
  ComServ, GeneralFunctions, Variants, OleCtrls, TlHelp32;

const
  SIX_MINUTES = 1/10;
  TEN_MINUTES = 1/6;

  COORD_SYSTEM = 6;     // Geographic coordinates (Greenwich) [deg]
  REF_SYSTEM   = 4;
  GEODLL_LICENSEE = 'Delattinia e.V., Büro Merzig';
  GEODLL_UNLOCK_COORDTRANSFORM = '027883454-367867986';
  GEODLL_UNLOCK_USERDEF = '291483875-221295059';

{-------------------------------------------------------------------------------
  Retrieve the application's process id, for debugging purposes.
}
var
  debugOn: Boolean = False;
  logStarted: Boolean = False;
  logIndent: Integer = 0;
  processId: String = '';
  CanonicalFormatSettings: TFormatSettings;

procedure GetProcessId;
var
  snapProcHandle: THandle;
  procEntry: TProcessEntry32;
  res: Boolean;
  exeName: String;
begin
  exeName := ExtractFileName(GetModuleName(0));

  snapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if snapProcHandle <> INVALID_HANDLE_VALUE then
    try
      procEntry.dwSize := SizeOf(procEntry);
      res := Process32First(snapProcHandle, procEntry);
      while res do begin
        if exeName = ExtractFileName(procEntry.szExeFile) then begin
          processId := IntToStr(procEntry.th32ProcessID);
          Exit;
        end;
        res := Process32Next(snapProcHandle, procEntry);
      end;
    finally
      CloseHandle(snapProcHandle);
    end;
end;

{-------------------------------------------------------------------------------
  Log an entry to a text file in the temp folder. Provided as a debugging service.
}
procedure Log(const logText: String);
var
  fileName: String;
  logFile: TextFile;
begin
  if not debugOn then Exit;


  fileName := 'XMLSystems - ' + FormatDateTime('yyyy-mm-dd', Now) + '.log';
  AssignFile(logFile, fileName);
  if not FileExists(fileName) then
    Rewrite(logFile)
  else
    Append(logFile);

  if not logStarted then begin
    WriteLn(logFile, FormatDateTime('hh:nn:ss', Now) + ' [' + processId + ']:  START NEW LOG SESSION ');
    logStarted := True;
  end;

  WriteLn(logFile,
      FormatDateTime('hh:nn:ss', Now)
      + ' [' + processId + ']: '
      + DupeString('|'#9, logIndent)
      + logText);
  Flush(logFile);
  CloseFile(logFile);
end;

{-----------------------------------------------------------------------------
  Checks a GeoDll function call response.
}
procedure Check(AResult: Integer);
var
  lpsz: PChar;
begin
  if AResult = 0 then begin
    Geterrorcode(lpsz);
    raise EXMLSystemsException.Create(Format(ResStr_FailedToInit, [lpsz]));
  end;
end;

{===============================================================================
  TXMLSystems
===============================================================================}
{-------------------------------------------------------------------------------
}
function TXMLSystems.Get_Name: WideString;
begin
  Result := ResStr_XMLSystemsAddinName;
end;

{-------------------------------------------------------------------------------
}
function TXMLSystems.Get_Description: WideString;
begin
  Result := Format(
      ResStr_XMLSystemsAddinDescription,
      [VersionInfo.GetFileVersion(AddinPath + 'XMLSystems.dll')]);
end;

{-------------------------------------------------------------------------------
}
function TXMLSystems.Get_ImageFileName: WideString;
begin
  Result := 'JTM.bmp';
end;

{-------------------------------------------------------------------------------
}
procedure TXMLSystems.Install(const iInstalledFilePath: WideString);
var
  lSrc, lDest, lFilePath: String;

  procedure CopyGeodllFile(const AFile: String);
  begin
    lSrc  := lFilePath + AFile;
    lDest := InstallPath + AFile;
    CopyFile(PChar(lSrc), PChar(lDest), false);
    Log('File "' + lSrc + '" copied to "' + lDest + '".');
  end;

begin
  Log('XMLSystems.Install start.');
  Inc(logIndent);
  lFilePath := ExtractFilePath(iInstalledFilePath);
  if not (FileExists(lFilePath + 'geodll32.dll') and
          FileExists(lFilePath + 'geobin32.bin') and
          FileExists(lFilePath + 'vo27run.dll')) then
    raise EXMLSystemsException.Create(ResStr_RequiredFilesMissing);
  CopyGeodllFile('geodll32.dll');
  CopyGeodllFile('geobin32.bin');
  CopyGeodllFile('vo27run.dll');
  Dec(logIndent);
  Log('XMLSystems.Install end.');
end;

{-------------------------------------------------------------------------------
}
function TXMLSystems.GetAddinPath: string;
begin
  if FAddinPath = '' then
    FAddinPath := GetRegistrySetting('Addin Path');
  Result := FAddinPath;
end;

{-------------------------------------------------------------------------------
}
function TXMLSystems.GetInstallPath: string;
begin
  if FInstallPath = '' then
    FInstallPath := GetRegistrySetting('Installation Path');
  Result := FInstallPath;
end;

{-------------------------------------------------------------------------------
}
function TXMLSystems.GetRegistrySetting(const AValue: string): string;
begin
  with TRegistry.Create do begin
    Rootkey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('Software\Dorset Software\Recorder 6') then
      Result := ReadString(AValue)
    else
      raise EXMLSystemsException.Create(ResStr_FailedRegistryAccess);
  end;
end;

{-------------------------------------------------------------------------------
  Get the number of ISpatialReferences in the list
}
function TXMLSystems.Get_SystemCount: Integer;
begin
  InitialiseSystems;
  Result := FSpatialRefInterfaces.Count;
end;

{-------------------------------------------------------------------------------
  Retrieve the specified ISpatialReference from the list
}
function TXMLSystems.Get_SystemInterface(AIndex: Integer): ISpatialReference;
begin
  InitialiseSystems;
  try
    Result := FSpatialRefInterfaces[AIndex] as ISpatialReference;
  except
    on E:EIntfCastError do
      Result := nil;
  end;
end;

{-------------------------------------------------------------------------------
  Get the number of IBaseMapFormats in the list
}
function TXMLSystems.Get_Count: Integer;
begin
  InitialiseSystems;
  Result := FBaseMapInterfaces.Count;
end;

{-------------------------------------------------------------------------------
  Retrieve the specified IBaseMapFormat
}
function TXMLSystems.Get_SystemBaseMapFormat(AIndex: Integer): IBaseMapFormat;
begin
  InitialiseSystems;
  try
    Result := FBaseMapInterfaces[AIndex] as IBaseMapFormat;
  except on E:EIntfCastError do
    Result := nil;
  end;
end;

{-------------------------------------------------------------------------------
  Initialisation code
}
procedure TXMLSystems.Initialize;
begin
  inherited;
  FLastError := '';
  FSystemsInitialised := false;
end;

{-------------------------------------------------------------------------------
}
procedure TXMLSystems.InitialiseSystems;
var
  lSpatialSystem: TSpatialSystem;
  lBaseMapSystem: TBaseMapSystem;
  lSearchRec: TSearchRec;
  xmlDoc: IXMLDocument;
  node: IXMLNode;
  fileName: String;
begin
  if not FSystemsInitialised then begin
    Log('XMLSystems.InitialiseSystems start.');
    Inc(logIndent);
    xmlDoc := TXMLDocument.Create(nil);
    FSpatialRefInterfaces := TInterfaceList.Create;
    FBaseMapInterfaces    := TInterfaceList.Create;
    Check(SetLanguage(2));

    // Unlock the coordinate transformations module
    Log('Unlock coordinate transformations module.');
    Check(SetUnlockCode(PChar(GEODLL_UNLOCK_COORDTRANSFORM), PChar(GEODLL_LICENSEE)));

    // Unlock the user definitions module
    Log('Unlock user definitions module.');
    Check(SetUnlockCode(PChar(GEODLL_UNLOCK_USERDEF), PChar(GEODLL_LICENSEE)));

    if FindFirst(AddinPath + '*.xml', faAnyFile, lSearchRec) = 0 then
    begin
      repeat
        fileName := AddinPath + lSearchRec.Name;
        xmlDoc.LoadFromFile(fileName);
        node := xmlDoc.DocumentElement;
        Log('File loaded: ' + fileName);

        if node.NodeName = 'spatialrefsystem' then
        begin
          lSpatialSystem := TSpatialSystem.Create(xmlDoc);
          FSpatialRefInterfaces.Add(lSpatialSystem as ISpatialReference);
          Log('Spatial Reference System loaded: ' + lSpatialSystem.FAbbrev);
        end else
        if node.NodeName = 'basemapformat' then
        begin
          lBaseMapSystem := TBaseMapSystem.Create(xmlDoc);
          FSpatialRefInterfaces.Add(lBaseMapSystem as ISpatialReference);
          FBaseMapInterfaces.Add(lBaseMapSystem as IBaseMapFormat);
          Log('Base Map System loaded: ' + lBaseMapSystem.FAbbrev);
        end;
      until FindNext(lSearchRec) <> 0;
      SysUtils.FindClose(lSearchRec);
    end;
    FSystemsInitialised := True;
    Dec(logIndent);
    Log('XMLSystems.InitialiseSystems end.');
  end;
end;

{===============================================================================
  TSpatialSystem
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TSpatialSystem.Create(xmlDoc: IXMLDocument); //filename: WideString);
begin
  Configure(xmlDoc); //filename);
end;

{-------------------------------------------------------------------------------
}
destructor TSpatialSystem.Destroy;
begin
  if Assigned(FScriptControl) then
    FScriptControl.Free;

  inherited;
end;

{-------------------------------------------------------------------------------
  Converts from latitude and longitude to the coordinate system
}
function TSpatialSystem.ConvertFromLatLong(const iLat, iLong: WideString): WideString;
var
  lCoord: TCoord;
  lLatLong: TLatLong;

  {-----------------------------------------------------------------------------
  }
  function EastingNorthingToQqq(const iLong, iLat: Double): WideString;
  var
    y, x: Double;
    yy, xx, q1, q2, q3, y8th, x8th: Integer;
    StrY, StrX: WideString;
  begin
    y    := ((FYOrigin - iLat) / SIX_MINUTES) + 9;
    yy   := Floor(y);
    x    := ((iLong - FXOrigin) * 6) + 1;
    xx   := Floor(x);
    y8th := Floor((y - yy) * 8) + 1;
    x8th := Floor((x - xx) * 8) + 1;
    // Start on 111
    q1 := 1;
    q2 := 1;
    q3 := 1;
    // Work out each shift according to y8th
    if y8th in [5, 6, 7, 8] then q1 := 3;
    if y8th in [3, 4, 7, 8] then q2 := 3;
    if y8th in [2, 4, 6, 8] then q3 := 3;
    // Work out each additional shift according to x8th
    if x8th in [5, 6, 7, 8] then Inc(q1);
    if x8th in [3, 4, 7, 8] then Inc(q2);
    if x8th in [2, 4, 6, 8] then Inc(q3);

    StrY := RightStr('00' + IntToStr(yy), 2);
    StrX := RightStr('00' + IntToStr(xx), 2);
    if (yy < 1) or (xx < 1) or (yy > 99) or (xx > 99) then
      Result := ResStr_BeyondSystemLimits
    else
      Result := Format('%s%s/%d%d%d', [StrY, StrX, q1, q2, q3]);
  end;

  {-----------------------------------------------------------------------------
  }
  function EastingNorthingToQyx(const iLong, iLat: Double): WideString;
  var
    easting, northing: Double;
    yy, xx, q, y, x, y6th, x10th: Integer;
    StrY, StrX: WideString;
  begin
    easting  := ((FYOrigin - iLat)/SIX_MINUTES) + 9;
    yy       := Floor(easting);
    northing := ((iLong - FXOrigin) * 6) + 1;
    xx       := Floor(northing);
    y6th     := Floor((easting - yy) * 6) + 1;
    x10th    := Floor((northing - xx) * 10) + 1;
    // Start on 111
    q := 1;
    y := 1;
    x := 1;
    // Work out each shift according to y6th
    if y6th in [4, 5, 6] then q := 3;
    if y6th in [2, 5] then y := 2;
    if y6th in [3, 6] then y := 3;
    // Work out each additional shift according to x10th
    if x10th in [6,  7, 8, 9, 10] then Inc(q);
    if x10th in [2,  7] then x := 2;
    if x10th in [3,  8] then x := 3;
    if x10th in [4,  9] then x := 4;
    if x10th in [5, 10] then x := 5;

    StrY := RightStr('00' + IntToStr(yy), 2);
    StrX := RightStr('00' + IntToStr(xx), 2);
    if (yy < 1) or (xx < 1) or (yy > 99) or (xx > 99) then
      Result := ResStr_BeyondSystemLimits
    else
      Result := Format('%s%s/%d%d%d', [StrY, StrX, q, y, x]);
  end;

  {-----------------------------------------------------------------------------
  }
  function EastingNorthingToIfbl(const iLong, iLat: Double): WideString;
  var
    easting, northing: Double;
    y, x, y1st, x1st, y2nd, x2nd, y3rd, x3rd, q1, q2: integer;
    letter: WideString;
  begin
    easting := FYOrigin - iLat;
    // This makes sure any coordinate exactly on a line between two letters
    // is put with the smaller letter.
    y := Ceil(easting)-1;
    northing := iLong - FXOrigin;
    x := Floor(northing);
    y1st := Floor(y / 20000);
    x1st := Floor(x / 32000);
    y2nd := Floor((y - (y1st * 20000)) / 4000) + 1;
    x2nd := Floor((x - (x1st * 32000)) / 4000) + 1;
    y3rd := Floor((y - (y1st * 20000) - ((y2nd - 1) * 4000)) / 1000) + 1;
    x3rd := Floor((x - (x1st * 32000) - ((x2nd - 1) * 4000)) / 1000) + 1;
    q1 := 1;
    q2 := 1;
    if y3rd in [3, 4] then q1 := 3;
    if x3rd in [3, 4] then Inc(q1);
    if y3rd in [2, 4] then q2 := 3;
    if x3rd in [2, 4] then Inc(q2);
    // Grid has no row 'I' so numbers past 8 need to be one bigger to get the right letter
    if y1st > 7 then Inc(y1st);
    letter := Chr(y1st + 65);
    if (x < 0) or (y < 0) or (y1st > 25) or (x1st > 9) then
      Result := ResStr_BeyondSystemLimits
    else
      Result := Format('%s%d%d%d%d%d', [letter, x1st, y2nd, x2nd, q1, q2]);
  end;

  {-----------------------------------------------------------------------------
  }
  function EastingNorthingToScriptedSystem(const iXCoord, iYCoord: Double): WideString;
  var
    lVarArray: Variant;
    lCoords: PSafeArray;
  begin
    lVarArray := VarArrayCreate([0, 2], VarVariant);
    lVarArray[0] := iXCoord;
    lVarArray[1] := iYCoord;
    lVarArray[2] := CanonicalFormatSettings.ListSeparator;
    lCoords := PSafeArray(System.TVarData(lVarArray).VArray);
    try
      Result := VarToStr(FScriptControl.Run('CoordsToRef', lCoords));
    except
      on E:Exception do begin
        // If the script errors, show a message box saying why.
        MessageDlg(Format(ResStr_ErrorInScript, [E.Message, FName]), mtError, [mbOK], 0);
        Result := ResStr_ErrorInScript;
      end;
    end;
  end;

  function FormatLatLong(const Value: String; PosChar, NegChar: Char): string;
  var
    lFloatValue: Extended;
  begin
    lFloatValue := StrToFloat(Value, CanonicalFormatSettings);
    if lFloatValue > 0 then
      Result := Value + PosChar
    else
      Result := FloatToStr(Abs(lFloatValue), CanonicalFormatSettings) + NegChar;
  end;

begin
  try
    lLatLong := LatLong(
        StrToFloat(iLat, CanonicalFormatSettings),
        StrToFloat(iLong, CanonicalFormatSettings));
    // Convert to the underlying easting/northing, or lat long projection
    lCoord := LatLongToCoord(lLatLong);
    if SameText(FAbbrev, 'QQQ') then
      Result := EastingNorthingToQqq(lCoord.x, lCoord.y)
    else
    if SameText(FAbbrev, 'QYX') then
      Result := EastingNorthingToQyx(lCoord.x, lCoord.y)
    else
    if SameText(FAbbrev, 'IFBL') then
      Result := EastingNorthingToIfbl(lCoord.x, lCoord.y)
    else
    if Assigned(FScriptControl) then
      Result := EastingNorthingToScriptedSystem(lCoord.x, lCoord.y)
    else
      Result := Format('%s%s %s', [
          FloatToStrF(lCoord.x, ffGeneral, 8, 5, CanonicalFormatSettings),
          CanonicalFormatSettings.ListSeparator,
          FloatToStrF(lCoord.y, ffGeneral, 8, 5, CanonicalFormatSettings)
      ], CanonicalFormatSettings);
    Result := result;
  except
    on E: Exception do begin
      Result := ResStr_BeyondSystemLimits;
      FLastError := E.Message;
    end;
  end;

  if (Result = ResStr_BeyondSystemLimits) or (Result = ResStr_ErrorInScript) then
  begin
    Result := Format(
        'LTLN: %s%s %s',
        [FormatLatLong(iLat, 'N', 'S'),
         CanonicalFormatSettings.ListSeparator,
         FormatLatLong(iLong, 'W', 'E')]);
  end;
end;

{-------------------------------------------------------------------------------
  Converts a spatial reference to a latitude
}
function TSpatialSystem.ConvertToLat(const iSpatialRef: WideString): WideString;
var
  lLatLong: TLatLong;
begin
  try
    lLatLong := SpatialRefToLatLong(iSpatialRef);
    Result := FloatToStr(lLatLong.Lat, CanonicalFormatSettings);
  except
    on E: Exception do begin
      Result := '';
      FLastError := E.Message;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Converts a spatial reference to a longitude
}
function TSpatialSystem.ConvertToLong(const iSpatialRef: WideString): WideString;
var
  lLatLong: TLatLong;
begin
  try
    lLatLong := SpatialRefToLatLong(iSpatialRef);
    Result := FloatToStr(lLatLong.Long, CanonicalFormatSettings);
  except
    on E: Exception do begin
      Result := '';
      FLastError := E.Message;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Converts coordinates in the designated system to latitude and longitude
}
function TSpatialSystem.CoordToLatLong(ACoords: TCoord): TLatLong;
var
  lLat, lLong: double;
  psz1, psz2: pchar;
  lResult : Word;
begin
  if (FCachedInputExampleCoords.x = ACoords.x) and (FCachedInputExampleCoords.y = ACoords.y) then
    Result := FCachedOutputLatLong
  else begin
    if SameText(FAbbrev, 'Bess') then begin
      ACoords.x := ACoords.x / 60;
      ACoords.y := ACoords.y / 100;
    end;

    // calculation not in cache so invoke GeoDLL
    lResult := CoordTrans(
                ACoords.x,
                ACoords.y,
                psz1,
                FCoordSys, FRefSys,
                lLong, lLat, // output
                psz2,
                COORD_SYSTEM, REF_SYSTEM,
                0);
    if lResult = 0 then begin
      Geterrorcode(psz1);
      raise EXMLSystemsException.Create(psz1);
    end;
    Result.Lat  := lLat;
    Result.Long := lLong;
    FCachedInputExampleCoords := ACoords;
    FCachedOutputLatLong := result;
  end;
end;

{-------------------------------------------------------------------------------
  Gets the 4 character abbreviation of the equivalent spatial reference system.
}
function TSpatialSystem.Get_EquivalentBaseMapSystem: WideString;
begin
  Result := FEquivalentBaseMapSystem;
end;

{-------------------------------------------------------------------------------
  Gets the error message of the most recent error
}
function TSpatialSystem.Get_GetLastError: WideString;
begin
  Result := FLastError;
end;

{-------------------------------------------------------------------------------
  The 4 character abbreviation of the spatial reference system
}
function TSpatialSystem.Get_SpatialRefSystem: WideString;
begin
  Result := FAbbrev;
end;

{-------------------------------------------------------------------------------
  The name of the spatial reference system
}
function TSpatialSystem.Get_SpatialSystemName: Widestring;
begin
  Result := FName;
end;

{-------------------------------------------------------------------------------
  Converts a pair of numbers into a latitude/longitude
}
function TSpatialSystem.LatLong(ALat, ALong: double): TLatLong;
begin
  Result.Lat  := ALat;
  Result.Long := ALong;
end;

{-------------------------------------------------------------------------------
  Converts a latitude/longitude into coordinates in the system
}
function TSpatialSystem.LatLongToCoord(ALatLong: TLatLong): TCoord;
var
  lx, ly: double;
  psz1, psz2: pchar;
  lResult : Word;
begin
  if (FCachedInputLatLong.Long = ALatLong.Long) and (FCachedInputLatLong.Lat = ALatLong.Lat) then
    Result := FCachedOutputExampleCoords
  else begin
    // calculation not in cache so invoke GeoDLL
    lResult := CoordTrans(
                ALatLong.Long,
                ALatLong.Lat,
                psz1,
                COORD_SYSTEM, REF_SYSTEM,
                lx, ly, // output
                psz2,
                FCoordSys, FRefSys,
                FStripNo);
    if lResult = 0 then begin
      Geterrorcode(psz1);
      raise EXMLSystemsException.Create(psz1);
    end;
    if SameText(FAbbrev, 'Bess') then begin
      lx := lx * 60;
      ly := ly * 100;
    end;
    Result.x := lx;
    Result.y := ly;
    FCachedInputLatLong := ALatLong;
    FCachedOutputExampleCoords := result;
  end;
end;
  
{-------------------------------------------------------------------------------
  Converts a spatial reference to latitude/longitude
}
function TSpatialSystem.SpatialRefToLatLong(const ARef: string): TLatLong;
var
  lCoords: TCoord;
  lVarArray: Variant;
  lParams: PSafeArray;
  lCoordStr: WideString;
  lListSep: Integer;
begin
  if Assigned(FScriptControl) then begin
    lVarArray := VarArrayCreate([0, 1], VarVariant);
    // first parameter is the ref to convert to coordinates
    lVarArray[0] := ARef;
    // second is the list separator
    lVarArray[1] := CanonicalFormatSettings.ListSeparator;
    lParams := PSafeArray(System.TVarData(lVarArray).VArray);
    try
      lCoordStr := FScriptControl.Run('RefToCoords', lParams);
    except
      on E: Exception do begin
        // If the script errors, we want to show a message box saying why.
        MessageDlg(Format(ResStr_ErrorInScript, [E.Message, FName]), mtError, [mbOK], 0);
        raise EXMLSystemsException.Create(Format(ResStr_ErrorInScript, [E.Message, FName]));
      end;
    end;
    // @todo: Problem
    // When direct inputting an sref, the ref is canonical. When clicking on the map, it is localised.
    lListSep  := Pos(CanonicalFormatSettings.ListSeparator, lCoordStr);
    if lListSep > 0 then begin
      lCoords.x := StrToFloat(LeftStr(lCoordStr, lListSep - 1), CanonicalFormatSettings);
      lCoords.y := StrToFloat(RightStr(lCoordStr, Length(lCoordStr) - lListSep), CanonicalFormatSettings);
    end else
      raise EXMLSystemsException.CreateNonCritical(Format(ResStr_InputRefNotCorrectFormat, [ARef]));
  end else
    lCoords := SplitSpatialRef(ARef);
  Result := CoordToLatLong(lCoords);
end;

{-------------------------------------------------------------------------------
  Splits a spatial reference system into a pair of coordinates
}
function TSpatialSystem.SplitSpatialRef(const ARef: string): TCoord;
var
  lLeftStr, lRightStr: string;
  lSeparatorPos: Integer;

  {-----------------------------------------------------------------------------
   Do an MTB reference up to the 1st quarter (shared between MTBQYX & MTBQQQ),
   getting the bottom left of the square
  }
  function ConvertMTBFirstQuarterToLatLong(const ARef: string) : TCoord;
  var
    yy, xx, q : Integer;
  begin
    //ValidateQqq(ARef);

    // Split the input string to separate all the numbers
    yy := StrtoInt(Copy(ARef, 1, 2));
    xx := StrToInt(Copy(ARef, 3, 2));
    // Set the latitude and longitude
    // Top left cell is yy=09, xx=01
    yy := yy - 09;
    xx := xx - 01;
    // Need to convert to bottom left of lat rather than top left, so move down a cell
    Inc(yy);
    // Each cell is 6 minutes high, = 0.1 degree. Top of grid is 55.1
    Result.y := FYOrigin - yy * SIX_MINUTES;
    // Each cell is 10 minutes wide, = 1/6 degrees. Left of grid is 5.833 = 35/6
    Result.x := FXOrigin + xx * TEN_MINUTES;
    if Length(ARef) >= 6 then begin
      q := StrtoInt(ARef[6]);
      // If in top half move up by half a cell
      if q in [1, 2] then
        Result.y := Result.y + (SIX_MINUTES / 2);
      // If in right half move right by half a cell
      if q in [2, 4] then
        Result.x := Result.x + (TEN_MINUTES / 2);
    end;
  end;

  {-----------------------------------------------------------------------------
  }
  function ConvertQqqToLatLong(const ARef: string) : TCoord;
  var
    q2, q3: integer;
  begin
    Result := ConvertMTBFirstQuarterToLatLong(ARef);
    if Length(ARef) >= 7 then begin
      q2 := StrToInt(ARef[7]);
      if (q2 < 3) then
        result.y := result.y + (SIX_MINUTES / 4);
      if (q2 = 2) or (q2 = 4) then
        result.x := result.x + (TEN_MINUTES / 4);
    end;
    if Length(ARef)>=8 then begin
      q3 := StrtoInt(ARef[8]);
      if (q3 < 3) then
        result.y := result.y + (SIX_MINUTES / 8);
      if (q3 = 2) or (q3 = 4) then
        result.x := result.x + (TEN_MINUTES / 8);
    end;
  end;

  {-----------------------------------------------------------------------------
  }
  function ConvertQyxToLatLong(const ARef: string) : TCoord;
  var
    x, y: integer;
  begin
    Result := ConvertMTBFirstQuarterToLatLong(ARef);
    if Length(ARef) >= 8 then begin
      y := StrToInt(Copy(ARef, 7, 1));
      x := StrtoInt(Copy(ARef, 8, 1));
      // Set the latitude and longitude to 1 degree precision
      result.y := result.y + ((3 - y) * (SIX_MINUTES / 6));
      result.x := result.x + ((x - 1) * (TEN_MINUTES / 10));
    end;
  end;

  {-----------------------------------------------------------------------------
  }
  function ConvertIfblToCoords(const ARef: string) : TCoord;
  var
    x, y, q1, q2: integer;
    letter: Char;
  begin
    // Convert letter to number A=1 etc.
    letter := UpperCase(Copy(ARef, 1, 1))[1];
    y := Ord(letter) - Ord('A');
    // Only do this for < 9 because the grid has has no row 'I'
    if y < 9 then y := y + 1;
    x := StrToInt(Copy(ARef, 2, 1));
    // Get SW corner of 32x20km cell
    Result.y := FYOrigin - (y * 20000);
    Result.x := FXOrigin + (32000 * x);
    if Length(ARef) >= 4 then begin
      // Get SW corner of 4x4km cell
      y := StrToInt(Copy(ARef, 3, 1));
      x := StrToInt(Copy(ARef, 4, 1));
      Result.y := Result.y + ((5 - y) * 4000);
      Result.x := Result.x + ((x - 1) * 4000);
    end;
    if Length(ARef)>=6 then begin
      // Get SW corner of 1x1km cell
      q1 := StrToInt(Copy(ARef, 5, 1));
      if (q1 < 3) then
        Result.y := Result.y + 2000;
      if (q1 = 2) or (q1 = 4) then
        Result.x := Result.x + 2000;

      q2 := strToInt(Copy(ARef, 6, 1));
      if (q2 < 3) then
        Result.y := Result.y + 1000;
      if (q2 = 2) or (q2 = 4) then
        Result.x := Result.x + 1000;
    end;
  end;

begin
  if SameText(FAbbrev, 'QQQ') then
    Result := ConvertQqqToLatLong(ARef)
  else
  if SameText(FAbbrev, 'QYX') then
    Result := ConvertQyxToLatLong(ARef)
  else
  if SameText(FAbbrev, 'IFBL') then
    Result := ConvertIfblToCoords(ARef)
  else begin
    lSeparatorPos := Pos(CanonicalFormatSettings.ListSeparator, ARef);
    if lSeparatorPos = 0 then
      raise EXMLSystemsException.Create(Format(ResStr_InputRefNotCorrectFormat, [ARef]));

    lLeftStr  := Trim(Copy(ARef, 1, lSeparatorPos - 1));
    lRightStr := Trim(Copy(ARef, lSeparatorPos + 1, 255));

    if not (IsFloat(lLeftStr, CanonicalFormatSettings) and IsFloat(lRightStr, CanonicalFormatSettings)) then
      raise EXMLSystemsException.Create(Format(ResStr_InputRefNotCorrectFormat, [ARef]));

    Result.x := StrToFloat(lLeftStr, CanonicalFormatSettings);
    Result.y := StrToFloat(lRightStr, CanonicalFormatSettings);
  end;
end;

{-------------------------------------------------------------------------------
  Checks whether the spatial reference system is valid
}
function TSpatialSystem.ValidSpatialRef(const iSpatialRef: WideString): WordBool;

  {-----------------------------------------------------------------------------
  }
  procedure CheckValidQuarter(quarter: string);
  begin
    if not IsInt(quarter) then
      raise EXMLSystemsException.CreateNonCritical(Format(ResStr_InputRefNotCorrectFormat, [quarter]));
    if ((StrToInt(quarter) < 1) or (StrToInt(quarter) > 4)) then
      raise EXMLSystemsException.CreateNonCritical(Format(ResStr_InputRefNotCorrectFormat, [quarter]));
  end;

  {-----------------------------------------------------------------------------
   Raise an exception if not nnnn, nnnn/n, nnnn/nn or nnnn/nnn
  }
  procedure CheckValidStructure(ARef: string);
  begin
    if not ((((Length(ARef) >= 6) and (Length(ARef) <= 8) and (ARef[5] = '/')) or (Length(ARef) = 4)) and
            IsInt(Copy(ARef, 1, 4))) then
      raise EXMLSystemsException.CreateNonCritical(Format(ResStr_InputRefNotCorrectFormat, [ARef]));
  end;

  {-----------------------------------------------------------------------------
  }
  procedure ValidateQqq(const ARef: string);
  begin
    CheckValidStructure(ARef);
    // Check each quarter is valid
    if Length(ARef) >= 6 then CheckValidQuarter(ARef[6]);
    if Length(ARef) >= 7 then CheckValidQuarter(ARef[7]);
    if Length(ARef) >= 8 then CheckValidQuarter(ARef[8]);
  end;

  {-----------------------------------------------------------------------------
  }
  procedure ValidateQyx(const ARef: string);
  var
    y, x : Integer;
  begin
    CheckValidStructure(ARef);

    // Split the input string to separate all the numbers
    if Length(ARef) >= 6 then
      CheckValidQuarter(ARef[6]);
    if Length(ARef) = 7 then
      // can't have an y and not a x
      raise EXMLSystemsException.CreateNonCritical(Format(ResStr_InputRefNotCorrectFormat, [ARef]));
    if Length(ARef) >= 8 then begin
      y := StrToInt(Copy(ARef, 7, 1));
      x := StrToInt(Copy(ARef, 8, 1));
      if (y < 1) or (y > 3) then
        raise EXMLSystemsException.CreateNonCritical(Format(ResStr_InputRefNotCorrectFormat, [ARef]));
      if (x < 1) or (x > 5) then
        raise EXMLSystemsException.CreateNonCritical(Format(ResStr_InputRefNotCorrectFormat, [ARef]));
    end;
  end;

begin
  Result := true;
  try
    if SameText(FAbbrev, 'QQQ') then
      ValidateQqq(iSpatialRef)
    else
    if SameText(FAbbrev, 'QYX') then
      ValidateQyx(iSpatialRef)
    else
      SpatialRefToLatLong(iSpatialRef);
  except
    on E: Exception do begin
      Result := False;
      FLastError := E.Message;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Reads the coordinate system data from an XML file, and prepares GeoDll to
  use it
}
procedure TSpatialSystem.Configure(xmlDoc: IXMLDocument);
var
  lProjNum: Integer;
  lParam1, lParam2, lParam3, lParam4,
  lParam5, lParam6, lParam7, lParam8: Double;
  node, methodNode: IXMLNode;

  {-----------------------------------------------------------------------------
  }
  procedure ReadAttributes(ADefaultCoordsys, ADefaultRefsys: integer;
      ADefaultXOrigin, ADefaultYOrigin: double);
  begin
    if methodNode.HasAttribute('coordsys') and IsInt(methodNode.Attributes['coordsys']) then
      FCoordSys := methodNode.Attributes['coordsys']
    else
    if methodNode.HasAttribute('coordsys') then
      raise EXMLSystemsException.Create(Format(
          ResStr_InvalidCoordRefsys,
          [FName, 'coordsys', methodNode.Attributes['coordsys']]))
    else
      FCoordSys := ADefaultCoordsys;

    if methodNode.HasAttribute('refsys') and IsInt(methodNode.Attributes['refsys']) then
      FRefSys := methodNode.Attributes['refsys']
    else
    if methodNode.HasAttribute('refsys') then
      raise EXMLSystemsException.Create(Format(
          ResStr_InvalidCoordRefsys,
          [FName, 'refsys', methodNode.Attributes['refsys']]))
    else
      FRefSys := ADefaultRefsys;

    if methodNode.HasAttribute('x-origin') then
      FXOrigin := methodNode.Attributes['x-origin']
    else
      FXOrigin := ADefaultXOrigin;

    if methodNode.HasAttribute('y-origin') then
      FYOrigin := methodNode.attributes['y-origin']
    else
      FYOrigin := ADefaultYOrigin;
  end;

begin
  Log('SpatialSystem.Configure start.');
  Inc(logIndent);
  FCoordSys := 0;
  FRefSys   := 0;
  FStripNo  := 0;
  // Set the x and y origins as the MTB origin for backwards compatibility
  FXOrigin  := 0;
  FYOrigin  := 0;

  xmlDoc.Active := true;
  node          := xmlDoc.DocumentElement;
  FName         := node.Attributes['caption'];
  Log('Attribute "caption": ' + FName);
  FAbbrev       := node.Attributes['code'];
  Log('Attribute "code": ' + FAbbrev);

  if FName = '' then FName := FAbbrev;
  FEquivalentBaseMapSystem := VarToStr(node.Attributes['equivalentbasecode']);
  methodNode := node.ChildNodes['calcmethod'];

  Log('Calc Method Type: "' + methodNode.Attributes['type'] + '".');
  if methodNode.Attributes['type'] = 'standard' then
  begin
    Log('Loading attributes for [standard] type');
    if methodNode.HasAttribute('coordsys') then
      FCoordSys := methodNode.Attributes['coordsys'];
    if methodNode.HasAttribute('refsys') then
      FRefSys := methodNode.Attributes['refsys'];
    if methodNode.HasAttribute('stripno') then
      FStripNo := methodNode.Attributes['stripno'];
  end else
  if methodNode.Attributes['type'] = 'user' then
  begin
    Log('Loading attributes for [user] type');
    lProjNum := StrToInt(methodNode.ChildNodes['projnum'].Text);
    lParam1 := StrToFloat(methodNode.ChildNodes['param1'].Text);
    lParam2 := StrToFloat(methodNode.ChildNodes['param2'].Text);
    lParam3 := StrToFloat(methodNode.ChildNodes['param3'].Text);
    lParam4 := StrToFloat(methodNode.ChildNodes['param4'].Text);
    lParam5 := StrToFloat(methodNode.ChildNodes['param5'].Text);
    lParam6 := StrToFloat(methodNode.ChildNodes['param6'].Text);
    lParam7 := StrToFloat(methodNode.ChildNodes['param7'].Text);
    lParam8 := StrToFloat(methodNode.ChildNodes['param8'].Text);
    Check(SetUserCoordSys1(lProjNum,
                           lParam1, lParam2, lParam3, lParam4,
                           lParam5, lParam6, lParam7, lParam8));
    FCoordSys := 1000;
  end else
  if methodNode.Attributes['type'] = 'custom' then
  begin
    Log('Loading attributes for [custom] type');

    if (methodNode.Attributes['name'] = 'MTBQQQ') or
       (methodNode.Attributes['name'] = 'MTBQYX') then
      ReadAttributes(0, 17, 35/6, 55.1)
    else
    if (methodNode.Attributes['name'] = 'IFBL') then
      ReadAttributes(54, 34, 2012, 258180)
    else
      raise EXMLSystemsException.Create(Format(ResStr_InvalidCustomMethod,
          [FName, methodNode.Attributes['name']]));
  end else
  if methodNode.Attributes['type'] = 'script' then
  begin
    Log('Loading attributes for [script] type');

    FScriptControl := TScriptControl.Create(nil);
    FScriptControl.Language := methodNode.Attributes['language'];
    FScriptControl.AddCode(StringReplace(methodNode.ChildNodes['reftocoords'].Text, '&quot;', '"', [rfReplaceAll]));
    FScriptControl.AddCode(StringReplace(methodNode.ChildNodes['coordstoref'].Text, '&quot;', '"', [rfReplaceAll]));

    if methodNode.HasAttribute('coordsys') then
      FCoordSys := methodNode.Attributes['coordsys']
    else
    if methodNode.HasAttribute('refsys') then
      FRefsys := methodNode.Attributes['refsys'];
  end;
  Dec(logIndent);
  Log('SpatialSystem.Configure end.');
end;

{===============================================================================
  TBaseMapSystem
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TBaseMapSystem.Create(xmlDoc: IXMLDocument);
begin
  FDistributionScales := TObjectList.Create;
  FGridLineScales     := TObjectList.Create;

  Configure(xmlDoc);
end;

{-------------------------------------------------------------------------------
}
destructor TBaseMapSystem.Destroy;
begin
  FDistributionScales.Free;
  FGridLineScales.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Converts from latitude and longitude to the coordinate system
}
function TBaseMapSystem.ConvertFromLatLong(const iLat, iLong: WideString): WideString;
var
  lCoord: TCoord;
begin
  try
    lCoord := LatLongToCoord(LatLong(
        StrToFloat(iLat, CanonicalFormatSettings),
        StrToFloat(iLong, CanonicalFormatSettings)));

    Result := FloatToStr(RoundTo(lCoord.x, -2), CanonicalFormatSettings)
        + CanonicalFormatSettings.ListSeparator + ' '
        + FloatToStr(RoundTo(lCoord.y, -2), CanonicalFormatSettings);
  except
    on E: Exception do begin
      Result := '';
      FLastError := E.Message;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Converts a spatial reference to a latitude
}
function TBaseMapSystem.ConvertToLat(const iSpatialRef: WideString): WideString;
var
  lLatLong: TLatLong;
begin
  try
    lLatLong := SpatialRefToLatLong(iSpatialRef);
    Result := FloatToStr(lLatLong.Lat, CanonicalFormatSettings);
  except
    on E: Exception do begin
      Result := '';
      FLastError := E.Message;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Converts a spatial reference to a longitude
}
function TBaseMapSystem.ConvertToLong(const iSpatialRef: WideString): WideString;
var
  lLatLong: TLatLong;
begin
  try
    lLatLong := SpatialRefToLatLong(iSpatialRef);
    Result := FloatToStr(lLatLong.Long, CanonicalFormatSettings);
  except
    on E: Exception do begin
      Result := '';
      FLastError := E.Message;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Converts coordinates in the designated system to latitude and longitude
}
function TBaseMapSystem.CoordToLatLong(ACoords: TCoord): TLatLong;
var
  lLat, lLong: double;
  psz1, psz2: pchar;
  lResult : Word;
begin
  if (FCachedInputExampleCoords.x = ACoords.x) and (FCachedInputExampleCoords.y = ACoords.y) then
    Result := FCachedOutputLatLong
  else begin
    if SameText(FAbbrev, 'Bess') then begin
      ACoords.x := ACoords.x / 60;
      ACoords.y := ACoords.y / 100;
    end;
    // calculation not in cache so invoke GeoDLL
    lResult := CoordTrans(
                ACoords.x,
                ACoords.y,
                psz1,
                FCoordSys, FRefSys,
                lLong, lLat, // output
                psz2,
                COORD_SYSTEM, REF_SYSTEM,
                0);
    if lResult = 0 then begin
      Geterrorcode(psz1);
      raise EXMLSystemsException.Create(psz1);
    end;
    Result.Lat := lLat;
    Result.Long := lLong;
    FCachedInputExampleCoords := ACoords;
    FCachedOutputLatLong := result;
  end;
end;

function TBaseMapSystem.Get_GetLastError: WideString;
begin
  Result := FLastError;
end;

function TBaseMapSystem.Get_EquivalentBaseMapSystem: WideString;
begin
  Result := FEquivalentBaseMapSystem;
end;

function TBaseMapSystem.Get_SpatialRefSystem: WideString;
begin
  Result := FAbbrev;
end;

{-------------------------------------------------------------------------------
  The name of the spatial reference system
}
function TBaseMapSystem.Get_SpatialSystemName: Widestring;
begin
  Result := FName;
end;

{-------------------------------------------------------------------------------
  Can the system display grids?
}
function TBaseMapSystem.Get_CanDisplayGrids: WordBool;
begin
  Result := true;
end;

{-------------------------------------------------------------------------------
  How many map coords per metre
}
function TBaseMapSystem.Get_MapCoordsPerMetre: Double;
begin
  if FMapCoordsPerMetre > 0 then
    Result := FMapCoordsPerMetre
  else
    Result := -FMapCoordsPerMetre;
end;

{-------------------------------------------------------------------------------
}
function TBaseMapSystem.Get_GridScaleCount: Integer;
begin
  Result := FGridLineScales.Count;
end;

function TBaseMapSystem.Get_GridScaleCaption(Index: Integer): WideString;
begin
  Result := TGridScale(FGridLineScales[Index]).Text;
end;

function TBaseMapSystem.Get_GridScaleX(Index: Integer): Double;
begin
  Result := TGridScale(FGridLineScales[Index]).Scale.x;
end;

function TBaseMapSystem.Get_GridScaleY(Index: Integer): Double;
begin
  Result := TGridScale(FGridLineScales[Index]).Scale.y;
end;

function TBaseMapSystem.Get_GridOffsetX(Index: Integer): Double;
begin
  Result := TGridScale(FGridLineScales[Index]).Offset.x;
end;

function TBaseMapSystem.Get_GridOffsetY(Index: Integer): Double;
begin
  Result := TGridScale(FGridLineScales[Index]).Offset.y;
end;

{-------------------------------------------------------------------------------
}
function TBaseMapSystem.Get_PointSizeCount: Integer;
begin
  Result := FDistributionScales.Count;
end;

function TBaseMapSystem.Get_PointSizeCaption(Index: Integer): WideString;
begin
  Result := TScale(FDistributionScales[Index]).Text;
end;

function TBaseMapSystem.Get_PointSizeX(Index: Integer): Double;
begin
  Result := TScale(FDistributionScales[Index]).Scale.x;
end;

function TBaseMapSystem.Get_PointSizeY(Index: Integer): Double;
begin
  Result := TScale(FDistributionScales[Index]).Scale.y;
end;

{-------------------------------------------------------------------------------
  Converts a pair of numbers into a latitude/longitude
}
function TBaseMapSystem.LatLong(ALat, ALong: double): TLatLong;
begin
  Result.Lat := ALat;
  Result.Long := ALong;
end;

{-------------------------------------------------------------------------------
  Converts a latitude/longitude into coordinates in the system
}
function TBaseMapSystem.LatLongToCoord(ALatLong: TLatLong): TCoord;
var
  lx, ly: double;
  psz1, psz2: pchar;
  lResult : Word;
begin
  if (FCachedInputLatLong.Long=ALatLong.Long) and (FCachedInputLatLong.Lat=ALatLong.Lat) then
    result := FCachedOutputExampleCoords
  else begin
    // calculation not in cache so invoke GeoDLL
    lResult := CoordTrans(
                ALatLong.Long,
                ALatLong.Lat,
                psz1,
                COORD_SYSTEM, REF_SYSTEM,
                lx, ly, // output
                psz2,
                FCoordSys, FRefSys,
                FStripNo);
    if lResult=0 then begin
      Geterrorcode(psz1);
      raise EXMLSystemsException.Create(psz1);
    end;
    if SameText(FAbbrev, 'Bess') then begin
      lx := lx * 60;
      ly := ly * 100;
    end;
    Result.x := lx;
    Result.y := ly;
    FCachedInputLatLong := ALatLong;
    FCachedOutputExampleCoords := result;
  end;
end;

{-------------------------------------------------------------------------------
  Turns a pair of numbers into coordinates in the system
}
function TBaseMapSystem.Coord(AX, AY: double): TCoord;
begin
  Result.x := AX;
  Result.y := AY;
end;
  
{-------------------------------------------------------------------------------
  Gets the system's x coordinate from a latitude/longitude
}
function TBaseMapSystem.LatLongToMapCoordX(iLat, iLong: Double): Double;
var
  lCoord: TCoord;
  lStripNo: Integer;
begin
  try
    lCoord := LatLongToCoord(LatLong(iLat, iLong));
    Result := lCoord.x;
  except
    on E:Exception do begin
      lStripNo := FStripNo;
      FStripNo := 0;
      lCoord   := LatLongToCoord(LatLong(iLat, iLong));
      Result   := lCoord.x;
      FStripNo := lStripNo;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Gets the system's y coordinate from a latitude/longitude
}
function TBaseMapSystem.LatLongToMapCoordY(iLat, iLong: Double): Double;
var
  lCoord: TCoord;
  lStripNo: Integer;
begin
  try
    lCoord := LatLongToCoord(LatLong(iLat, iLong));
    Result := lCoord.y;
  except
    on E:Exception do begin
      lStripNo := FStripNo;
      FStripNo := 0;
      lCoord   := LatLongToCoord(LatLong(iLat, iLong));
      Result   := lCoord.y;
      FStripNo := lStripNo;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Gets the latitude of a pair of map coordinates
}
function TBaseMapSystem.MapCoordToLat(iEasting, iNorthing: Double): WideString;
var
  lLatLong: TLatLong;
begin
  try
    lLatLong := CoordToLatLong(Coord(iEasting, iNorthing));
    Result := FloatToStr(lLatLong.Lat, CanonicalFormatSettings);
    if (SameText(FAbbrev, 'GKr2') and ((iEasting < 2000000) or (iEasting >= 3000000))) or
       (SameText(FAbbrev, 'GKr4') and ((iEasting < 4000000) or (iEasting >= 5000000))) then
      Result := ResStr_BeyondSystemLimits;
  except
    on E: Exception do begin
      Result := '';
      FLastError := E.Message;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Gets the longitude of a pair of coordinates
}
function TBaseMapSystem.MapCoordToLong(iEasting, iNorthing: Double): WideString;
var
  lLatLong: TLatLong;
begin
  try
    lLatLong := CoordToLatLong(Coord(iEasting, iNorthing));
    Result := FloatToStr(lLatLong.Long, CanonicalFormatSettings);
    if (SameText(FAbbrev, 'GKr2') and ((iEasting < 2000000) or (iEasting >= 3000000))) or
       (SameText(FAbbrev, 'GKr4') and ((iEasting < 4000000) or (iEasting >= 5000000))) then
      Result := ResStr_BeyondSystemLimits;
  except
    on E: Exception do begin
      Result := '';
      FLastError := E.Message;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Converts a spatial reference to latitude/longitude
}
function TBaseMapSystem.SpatialRefToLatLong(const ARef: string): TLatLong;
var
  lCoords: TCoord;
begin
  lCoords := SplitSpatialRef(ARef);
  Result := CoordToLatLong(lCoords);
end;

{-------------------------------------------------------------------------------
  Splits a spatial reference system into a pair of coordinates
}
function TBaseMapSystem.SplitSpatialRef(const ARef: string): TCoord;
var
  lPos: Integer;
  lLeftStr, lRightStr: string;
begin
  lPos := Pos(CanonicalFormatSettings.ListSeparator, ARef);
  if lPos = 0 then
    raise EXMLSystemsException.CreateFmt(
        ResStr_InputRefNotCorrectFormat,
        [ARef]);

  lLeftStr  := Trim(Copy(ARef, 1, lPos - 1));
  lRightStr := Trim(Copy(ARef, lPos + 1, 255));

  if not (IsFloat(lLeftStr, CanonicalFormatSettings)
          and IsFloat(lRightStr, CanonicalFormatSettings)) then
    raise EXMLSystemsException.CreateFmt(
        ResStr_InputRefNotCorrectFormat,
        [ARef]);

  Result.x := StrToFloat(lLeftStr, CanonicalFormatSettings);
  Result.y := StrToFloat(lRightStr, CanonicalFormatSettings);
end;

{-------------------------------------------------------------------------------
  Checks whether the spatial reference system is valid
}
function TBaseMapSystem.ValidSpatialRef(const iSpatialRef: WideString): WordBool;
begin
  Result := true;
  try
    SpatialRefToLatLong(iSpatialRef);
  except
    on E: Exception do begin
      Result := False;
      FLastError := E.Message;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Reads the coordinate system data from an XML file, and prepares GeoDll to
  use it
}
procedure TBaseMapSystem.Configure(xmlDoc: IXMLDocument);
var
  i, lProjNum: Integer;
  lParam1, lParam2, lParam3, lParam4,
  lParam5, lParam6, lParam7, lParam8: Double;
  node, methodNode, scaleNode: IXMLNode;
  scale: TScale;
  gridScale: TGridScale;

  {-----------------------------------------------------------------------------
  }
  function ParseValue(const value: String): String;
  begin
    Result := StringReplace(value, '.', DecimalSeparator, [rfReplaceAll]);
  end;

begin
  Log('BaseMapSystem.Configure start.');
  Inc(logIndent);
  FCoordSys := 0;
  FRefSys   := 0;
  FStripNo  := 0;
  FDisplayGrids := True;
  try
    XMLDoc.Active := True;
    node          := XMLDoc.DocumentElement;
    methodNode    := node.ChildNodes['calcmethod'];
    Log('Calc Method Type: "' + methodNode.Attributes['type'] + '".');
    
    if methodNode.Attributes['type'] = 'standard' then
    begin
      Log('Loading attributes for [standard] type');
      if methodNode.HasAttribute('coordsys') then
        FCoordSys := methodNode.Attributes['coordsys'];
      if methodNode.HasAttribute('refsys') then
        FRefSys := methodNode.Attributes['refsys'];
      if methodNode.HasAttribute('stripno') then
        FStripNo := methodNode.Attributes['stripno'];
    end else
    if methodNode.Attributes['type'] = 'user' then
    begin
      Log('Loading attributes for [user] type');
      // Read values for GeoDll call.
      lProjNum := StrToInt(methodNode.ChildNodes['projnum'].Text);
      lParam1 := StrToFloat(methodNode.ChildNodes['param1'].Text);
      lParam2 := StrToFloat(methodNode.ChildNodes['param2'].Text);
      lParam3 := StrToFloat(methodNode.ChildNodes['param3'].Text);
      lParam4 := StrToFloat(methodNode.ChildNodes['param4'].Text);
      lParam5 := StrToFloat(methodNode.ChildNodes['param5'].Text);
      lParam6 := StrToFloat(methodNode.ChildNodes['param6'].Text);
      lParam7 := StrToFloat(methodNode.ChildNodes['param7'].Text);
      lParam8 := StrToFloat(methodNode.ChildNodes['param8'].Text);
      Check(SetUserCoordSys1(lProjNum,
                             lParam1, lParam2, lParam3, lParam4,
                             lParam5, lParam6, lParam7, lParam8));
    end;

    FAbbrev := node.Attributes['code'];

    if node.HasAttribute('caption') then
      FName := node.Attributes['caption']
    else
      FName := FAbbrev;

    FBaseMap := node.Attributes['code'];

    if node.HasAttribute('displaygrids') then
      FDisplayGrids := node.Attributes['displaygrids'];

    if node.ChildNodes.IndexOf('distributionscalelist') > -1 then
    begin
      Log('Node found: distributionscalelist');
      scaleNode := node.ChildNodes['distributionscalelist'];
      for i := 0 to scaleNode.ChildNodes.Count - 1 do
      begin
        scale := TScale.Create(
            scaleNode.ChildNodes[i].Attributes['caption'],
            ParseValue(scaleNode.ChildNodes[i].Attributes['x']),
            ParseValue(scaleNode.ChildNodes[i].Attributes['y']));
        FDistributionScales.Add(scale);
      end;
    end;

    if node.ChildNodes.IndexOf('gridlinescalelist') > -1 then
    begin
      Log('Node found: gridlinescalelist');
      scaleNode := node.ChildNodes['gridlinescalelist'];
      for i := 0 to scaleNode.ChildNodes.Count - 1 do
      begin
        gridScale := TGridScale.Create(
            scaleNode.ChildNodes[i].Attributes['caption'],
            ParseValue(scaleNode.ChildNodes[i].Attributes['x']),
            ParseValue(scaleNode.ChildNodes[i].Attributes['y']),
            ParseValue(VarToStr(scaleNode.ChildNodes[i].Attributes['offsetx'])),
            ParseValue(VarToStr(scaleNode.ChildNodes[i].Attributes['offsety'])));
        FGridLineScales.Add(gridScale);
      end;
    end;

    FMapCoordsPerMetre := StrToFloat(ParseValue(node.ChildNodes['mapcoordspermetre'].Text));
  except
    on E: Exception do
      FLastError := E.Message;
  end;
  Dec(logIndent);
  Log('BaseMapSystem.Configure start.');
end;


{===============================================================================
  TScale
===============================================================================}
constructor TScale.Create(const AText, AX, AY: String);
begin
  FText := AText;
  FScale.X := StrToFloat(AX);
  FScale.Y := StrToFloat(AY);
end;

{===============================================================================
  TGridScale
===============================================================================}
constructor TGridScale.Create(const AText, AX, AY, AOffX, AOffY: String);
begin
  inherited Create(AText, AX, AY);
  if AOffX = '' then FOffset.X := 0
                else FOffset.X := StrToFloat(AOffX);
  if AOffY = '' then FOffset.Y := 0
                else FOffset.Y := StrToFloat(AOffY);
end;


initialization
{$IFDEF DEBUG}
  debugOn   := True;
  GetProcessId;
{$ENDIF}
  GetSpatialRefFormatSettings(CanonicalFormatSettings);

  TAutoObjectFactory.Create(ComServer, TXMLSystems, Class_XMLSystems,
    ciMultiInstance, tmApartment);
end.

