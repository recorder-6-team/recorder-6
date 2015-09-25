{===============================================================================
  Unit:        XmlReportList

  Defines:     TXmlReport
               TXmlReportList

  Description: Implementation of the IXmlReport and IXmlReportList interfaces

  Created:     Oct 2007

  Last revision information:
    $Revision: 2 $
    $Date: 15/09/10 15:08 $
    $Author: Robertjohnson $

===============================================================================}
unit XmlReportList;

interface

uses
  ComObj, Classes, SysUtils, Recorder2000_TLB, CRCommonClasses, ResourceStrings,
  ExceptionForm, StrUtils, Forms;

type
  EReportSQLException = class(TExceptionPath)
  end;

  {-----------------------------------------------------------------------------
    Implementation of IXmlReport.
  }
  TXmlReport = class(TAutoObject, IXmlReport)
  private
    FFilename: String;
    FKeyType: TKeyType;
    FReportTitle: String;
    FReportPath: String;
    FKeyTypeString: String;
  protected
    function Get_ReportTitle: WideString; safecall;
    function Get_ReportPath: WideString; safecall;
  public
    constructor Create(ATitle, AFilename, APath: String; AKeyType: TKeyType;
        AKeyTypeString: String = '');
    property ReportTitle: WideString read Get_ReportTitle;
    property ReportPath: WideString read Get_ReportPath;
    function Execute(const AKeyList: IKeyList): WordBool; safecall;
  end;
          
  {-----------------------------------------------------------------------------
    Implementation of IXmlReportList.
  }
  TXmlReportList = class(TAutoObject, IXmlReportList)
  protected
    FReports: TInterfaceList;
    function Get_ReportCount: Integer; safecall;
    function Get_Report(Index: Integer): IXmlReport; safecall;
  public
    constructor Create(AReportPath, AKeyType: WideString);
    destructor Destroy; override;
    property ReportCount: Integer read Get_ReportCount;
    property Report[Index: Integer]: IXmlReport read Get_Report;
  end;

implementation

uses ApplicationSettings, FilterResult, Maintbar, ComAddinUnit;
   
{===============================================================================
  TXMLReport
===============================================================================} 
{-------------------------------------------------------------------------------
  Constructor
}
constructor TXmlReport.Create(ATitle, AFilename, APath: String; AKeyType: TKeyType;
    AKeyTypeString: String = '');
begin
  FReportTitle := ATitle;
  FFileName := AFileName;
  FReportPath := APath;
  FKeyType := AKeyType;
  FKeyTypeString := AKeyTypeString;
end;  // TXmlReport.Create
     
{-------------------------------------------------------------------------------
}
function TXmlReport.Get_ReportTitle: WideString;
begin
  Result := FReportTitle;
end;  // TXmlReport.Get_ReportTitle
   
{-------------------------------------------------------------------------------
}
function TXmlReport.Get_ReportPath: WideString;
begin
  Result := FReportPath;
end;  // TXmlReport.Get_ReportPath
      
{-------------------------------------------------------------------------------
}
function TXmlReport.Execute(const AKeyList: IKeyList): WordBool;
var
  i, j: Integer;
  lReportKeyType: IReportKeyType;
  lKeys: String;
begin
  try
    if FKeyType <> ktAddin then
      // If it's not a report keytype defined by an addin, then there'll only be one
      // key, so we can run the report as normal using the first and only key in the list
      TfrmFilterResult.CreateAndRun(frmMain, AppSettings.ReportPath + FReportPath, FKeyType, AKeyList.GetKeyItem(0).KeyField1)
    else begin
      // Otherwise find the right addin-defined report keytype and check if it supports
      // multiple values
      for i := 0 to AppSettings.ComAddins.ReportKeyTypeInterfaces.Count - 1 do begin
        lReportKeyType := AppSettings.ComAddins.ReportKeyTypeInterfaces[i] as IReportKeytype;
        if Lowercase(lReportKeyType.Name) = FKeyTypestring then begin
          if lReportKeyType.Multivalue = True then begin
            // If so, convert the keylist to a comma separated string and run the report
            lKeys := AKeyList.GetKeyItem(0).KeyField1;
            for j := 1 to AKeyList.ItemCount - 1 do
              // Note no ' at either end of the string; they get parsed in later
              lKeys := lKeys + ''', ''' + AKeyList.GetKeyItem(j).KeyField1;
            TfrmFilterResult.CreateAndRun(frmMain, AppSettings.ReportPath + FFileName, FKeyType, lKeys);
          end else
            // If not, run the report using the first and only key in the list
            TfrmFilterResult.CreateAndRun(frmMain, AppSettings.ReportPath + FFileName, FKeyType, AKeyList.GetKeyItem(0).KeyField1);
        end;
      end;  // for i := 0 to AppSettings.ComAddins.ReportKeyTypeInterfaces.Count - 1
    end;  // if FKeyType <> ktAddin
  except
    // Handle problems parsing the SQL or XML with a non-critical box so developer can immediately see problem
    on E:EReportSQLException do
      raise EReportSQLException.CreateNonCritical(E.Message);
  end;  // try .. finally
end;  // TXmlReport.Execute

{===============================================================================
  TXMLReportList
===============================================================================}
{-------------------------------------------------------------------------------
  Constructor
}
constructor TXmlReportList.Create(AReportPath, AKeyType: WideString);
var
  lKeyType: TKeyType;
  lXmlReport: TXmlReport;
  i: Integer;
  lKeyTypeString: String;

    function GetReportFileName(AIndex: integer): string;
    begin
      with AppSettings.CRReportIndex.FileTypes do
        Result := LeftStr(Strings[AIndex], Pos(':', Strings[AIndex])-1);
    end;

    function GetReportTitle(AIndex: integer): string;
    begin
      with AppSettings.CRReportIndex.Titles do
        Result := Values[GetReportFileName(AIndex)];
      with AppSettings.CRReportIndex.Templates do
        if Values[GetReportFileName(AIndex)]<>'' then
          Result := Result + ' (' + Values[GetReportFileName(AIndex)] + ')';
    end;

begin
  FReports := TInterfaceList.Create;

  if      AKeyType = 'default' then lKeyType := ktDefault
  else if AKeyType = 'name' then lKeyType := ktName
  else if AKeyType = 'location' then lKeyType := ktLocation
  else if AKeyType = 'gridsquarerange' then lKeyType := ktGridSquareRange
  else if AKeyType = 'spatialref' then lKeyType := ktSpatialRef
  else if AKeyType = 'taxon' then lKeyType := ktTaxon
  else if AKeyType = 'biotope' then lKeyType := ktBiotope
  else if AKeyType = 'admin' then lKeyType := ktAdminArea
  else if AKeyType = 'survey' then lKeyType := ktSurvey
  else if AKeyType = 'event' then lKeyType := ktSurveyEvent
  else if AKeyType = 'sample' then lKeyType := ktSample
  else if AKeyType = 'taxon_occurrence' then lKeyType := ktTaxonOccurrence
  else if AKeyType = 'biotope_occurrence' then lKeyType := ktBiotopeOccurrence
  else lKeyType := ktAddin;

  with AppSettings.CRReportIndex.FileTypes do
    for i := 0 to Count-1 do begin
      // If keytype is not addin defined, we can do with just the enum
      if lKeyType <> ktAddin then begin
        if Ord(lKeyType) = StrToInt(Copy(Strings[i], Pos(':', Strings[i])+1, Length(Strings[i]))) then begin
          lXmlReport := TXmlReport.Create(GetReportTitle(i), GetReportFileName(i),
              AppSettings.CRReportIndex.Paths.Values[
          LeftStr(Strings[i], Pos(':', Strings[i])-1)], lKeyType);
          FReports.Add(lXmlReport as IXmlReport);
        end;
      // Otherwise we need the keytype name as well
      end else begin
        lKeytypeString := Copy(Strings[i], Pos(':', Strings[i])+1, Length(Strings[i]));
        if (LowerCase(AKeyType) = Copy(lKeytypeString, Pos(':', lKeyTypeString)+1, Length(lKeyTypeString))) then begin
          lXmlReport := TXmlReport.Create(GetReportTitle(i), GetReportFileName(i),
              AppSettings.CRReportIndex.Paths.Values[
          LeftStr(Strings[i], Pos(':', Strings[i])-1)], lKeyType, LowerCase(AKeyType));
          FReports.Add(lXmlReport as IXmlReport);
        end;
      end;
    end;
end;  //TXmlReportList.Create

{-------------------------------------------------------------------------------
  Destructor
}
destructor TXmlReportList.Destroy;
begin
  FReports.Free;

  inherited Destroy;
end;  // TXmlReportList.Destroy

{-------------------------------------------------------------------------------
}
function TXmlReportList.Get_ReportCount: Integer;
begin
  Result := FReports.Count;
end;  // TXmlReportList.Get_ReportCount
   
{-------------------------------------------------------------------------------
}
function TXmlReportList.Get_Report(Index: Integer): IXmlReport;
begin
  Result := FReports[Index] as IXmlReport;
end;  // TXmlReportList.Get_Report

end.
