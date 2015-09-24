{===============================================================================
  Unit:         IWTableRule.pas

  Implements:   TUserSuppliedData

  Description:  Data specified by the user on import instead of being read from
                the file.

  Model:        ImportWizardBackend.mpb

  Created:      May 2004

  Last Revision Details:
    $Revision: 5 $
    $Date: 26/01/07 11:31 $
    $Author: Ericsalmon $

  Copyright © Environment and Heritage Service, 2004

===============================================================================}
unit IWUserSuppliedData;

interface

uses
  Classes;

type
  {--------------------------------------------------------------------------
  ---
    Data specified by the user on import instead 
    of being read from the file.
  }
  TUserSuppliedData = class(TObject)
  protected
    FSingleDate: String;
    FSingleDeterminerKey: String;
    FSingleLocationKey: String;
    FSingleLocationName: String;
    FSingleObserverKey: String;
    FSingleSpatialRef: String;
    FSingleSpatialRefQualifier: String;
    FSingleSpatialRefSystem: String;
    FSurveyKey: String;
    FTermKeys: TStringList;
    FUseObserverAsDeterminer: Boolean;
    FUseSpecificDeterminer: Boolean;
    function GetTermKey(const TableName: String): String; virtual;
    procedure SetTermKey(const TableName: String; const Value: String); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property SingleDate: String read FSingleDate write FSingleDate;
    property SingleDeterminerKey: String read FSingleDeterminerKey write
        FSingleDeterminerKey;
    property SingleLocationKey: String read FSingleLocationKey write
        FSingleLocationKey;
    property SingleLocationName: String read FSingleLocationName write
        FSingleLocationName;
    property SingleObserverKey: String read FSingleObserverKey write
        FSingleObserverKey;
    property SingleSpatialRef: String read FSingleSpatialRef write
        FSingleSpatialRef;
    property SingleSpatialRefQualifier: String read FSingleSpatialRefQualifier
        write FSingleSpatialRefQualifier;
    property SingleSpatialRefSystem: String read FSingleSpatialRefSystem write
        FSingleSpatialRefSystem;
    property SurveyKey: String read FSurveyKey write FSurveyKey;
    property TermKey[const TableName: String]: String read GetTermKey write
        SetTermKey;
    property UseObserverAsDeterminer: Boolean read FUseObserverAsDeterminer
        write FUseObserverAsDeterminer;
    property UseSpecificDeterminer: Boolean read FUseSpecificDeterminer write
        FUseSpecificDeterminer;
  end;

//==============================================================================
implementation

uses
  Constants, IWConstants;
  
{-==============================================================================
    TUserSuppliedData
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TUserSuppliedData.Create;
begin
  inherited;
  FTermKeys := TStringList.Create;
  FTermKeys.Values[TN_SAMPLE_TYPE] := ST_KEY_FIELD_OBSERVATION;
  FTermKeys.Values[TN_RECORD_TYPE] := NONE_RECORD_KEY;
end;  // TUserSuppliedData.Create

{-------------------------------------------------------------------------------
}
destructor TUserSuppliedData.Destroy;
begin
  FTermKeys.Free;
  inherited;
end;  // TUserSuppliedData.Destroy

{-------------------------------------------------------------------------------
}
function TUserSuppliedData.GetTermKey(const TableName: String): String;
begin
  if FTermKeys.IndexOfName(TableName) = -1 then
    Result := ''
  else
    Result := FTermKeys.Values[TableName];
end;  // TUserSuppliedData.GetTermKey

{-------------------------------------------------------------------------------
}
procedure TUserSuppliedData.SetTermKey(const TableName: String; const Value:
    String);
begin
  FTermKeys.Values[TableName] := Value;
end;  // TUserSuppliedData.SetTermKey

end.
