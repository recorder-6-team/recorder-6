//==============================================================================
//  Unit:        BiotopeOccurData
//
//  Implements:  TdmBiotopeOccurrences
//
//  Description: Implements data access functionality for the biotope occurrence
//               screen.
//
//  Author:      Eric Salmon
//  Created:     1 Jun 1999
//
//  Last Revision Details:
//    $Revision: 21 $
//    $Date: 20/12/07 11:01 $
//    $Author: Rickyshrestha $
//
//  $History: BiotopeOccurData.pas $
//  
//  *****************  Version 21  *****************
//  User: Rickyshrestha Date: 20/12/07   Time: 11:01
//  Updated in $/JNCC/Development/Build/Source
//  Changed some constants to resourcestring
//  
//  *****************  Version 20  *****************
//  User: Ericsalmon   Date: 7/01/03    Time: 10:42
//  Updated in $/JNCC/Source
//  Cleanup. Use Parameters collection of ADO query objects.
//  
//  *****************  Version 19  *****************
//  User: Ericsalmon   Date: 4/12/02    Time: 15:49
//  Updated in $/JNCC/Source
//  Removed USE_TITAN compiler directive.
//  
//  *****************  Version 18  *****************
//  User: Ericsalmon   Date: 2/12/02    Time: 16:06
//  Updated in $/JNCC/Source
//  
//  *****************  Version 17  *****************
//  User: Ericsalmon   Date: 2/12/02    Time: 15:32
//  Updated in $/JNCC/Source
//  
//  *****************  Version 16  *****************
//  User: Ericsalmon   Date: 2/12/02    Time: 15:25
//  Updated in $/JNCC/Source
//  
//  *****************  Version 15  *****************
//  User: Ericsalmon   Date: 26/06/02   Time: 17:29
//  Updated in $/JNCC/Source                                 
//  JNCC 566: Fixed, Biotope_List_Item_Key is returned for the given
//  Biotope_Key and where BT_CL_Version_To is null.
//
//  Copyright © Dorset Software Services Ltd, 2001
//
//==============================================================================

unit BiotopeOccurData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BaseData, Db, JNCCDatasets, DataClasses, Constants, ADODB;

type
  TdmBiotopeOccurrences = class(TBaseDataModule)
    qryBiotopeOcc: TJNCCQuery;
    dsBiotopeOcc: TDataSource;
    qryBLIKey: TJNCCQuery;
    qryClassificationFromBLI: TJNCCQuery;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DeleteRecord(const ABioOccKey:TKeyString);
    function GetBiotopeListItemKey(const ABiotopeKey: TKeyString): TKeyString;
    function GetClassification(const ABliKey: TKeyString): string;
  end;

//==============================================================================
implementation

{$R *.DFM}

uses
  GeneralData, ApplicationSettings;

//==============================================================================
{ TdmBiotopeOccurrences }

procedure TdmBiotopeOccurrences.DeleteRecord(const ABioOccKey: TKeyString);
begin
  with dmGeneralData do begin
    DelSources('Biotope_Occurrence_Sources','Biotope_Occurrence_Key',ABioOccKey);

    ExecuteSQL('DELETE FROM Biotope_Occurrence_Data WHERE Biotope_Occurrence_Key = '''+ABioOccKey+'''',
               ResStr_DelFail+' - BIOTOPE_OCCURRENCE_DATA');
    ExecuteSQL('DELETE FROM Biotope_Determination WHERE Biotope_Occurrence_Key = '''+ABioOccKey+'''',
               ResStr_DelFail+' - BIOTOPE_DETERMINATION');
    ExecuteSQL('DELETE FROM Biotope_Occurrence WHERE Biotope_Occurrence_Key = '''+ABioOccKey+'''',
               ResStr_DelFail+' - BIOTOPE_OCCURRENCE');
  end;
end;  // DeleteRecord

//==============================================================================
function TdmBiotopeOccurrences.GetBiotopeListItemKey(const ABiotopeKey:TKeyString):TKeyString;
begin
  Result:='';
  with qryBLIKey do begin
    Parameters.ParamByName('KeyParameter').Value:=ABiotopeKey;
    Open;
    First;
    if RecordCount>0 then Result:=FieldByName('Biotope_List_Item_Key').AsString;
    Close;
  end;
end;  // GetBiotopeListItemKey

//==============================================================================
function TdmBiotopeOccurrences.GetClassification(const ABliKey: TKeyString): string;
begin
  Result:='';
  with qryClassificationFromBLI do begin
    Parameters.ParamByName('KeyParameter').Value:=ABliKey;
    Open;
    First;
    if RecordCount>0 then Result:=FieldByName('Short_Name').AsString;
    Close;
  end;
end;  // GetClassification

//==============================================================================
end.
  