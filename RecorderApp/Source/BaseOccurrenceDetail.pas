//==============================================================================
//  Unit: BaseOccurrenceDetail
//
//  Implements: TfrmBaseOccurrenceDetail
//
//  Description: A base class for all occurrence detail pages
//
//  Created: 28/04/2003
//
//  Last Revision Details:
//    $Revision: 4 $
//    $Date: 6/12/07 12:18 $
//    $Author: Davidkelly $
//
//  $History: BaseOccurrenceDetail.pas $
//  
//  *****************  Version 4  *****************
//  User: Davidkelly   Date: 6/12/07    Time: 12:18
//  Updated in $/JNCC/Development/Build/Source
//  VI 14309: CCN 183: Moved the call to DisplayRecord up to
//  Observations.CheckedOccurrence.
//  
//  *****************  Version 3  *****************
//  User: Ericsalmon   Date: 25/02/04   Time: 10:36
//  Updated in $/JNCC/Development/Build/Source
//  Changed SQL to use GetDate function.
//  
//  *****************  Version 2  *****************
//  User: Ericsalmon   Date: 4/02/04    Time: 15:15
//  Updated in $/JNCC/Development/Build/Source
//  Bug fixes.
//  
//  *****************  Version 1  *****************
//  User: Johnvanbreda Date: 28/04/03   Time: 15:49
//  Created in $/JNCC/Development/Build/Source
//  Base form for occurrence detail pages
//
//  Copyright © Dorset Software Services Ltd, 2002
//
//==============================================================================
unit BaseOccurrenceDetail;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseDockedForm, DataClasses;

type
  TfrmBaseOccurrenceDetail = class(TfrmBaseDockedForm)
  protected
    FCheckedState: Boolean;
    procedure DoCheckedStateChange(const ATable, AKey: String; ANewState: Boolean);
  public
    procedure DisplayRecord(const AOccurrenceKey: TKeyString); virtual; abstract;
  end;

//==============================================================================
implementation

uses
  ApplicationSettings, DatabaseAccessADO;
  
{$R *.dfm}

{-------------------------------------------------------------------------------
  Description : Change the occurrence checked state.  The query supplied must
      select the checked, checked_by and checked_date fields.
  Created : 28/04/2003 }
procedure TfrmBaseOccurrenceDetail.DoCheckedStateChange(const ATable, AKey: String;
  ANewState: Boolean);
var
  lUser, lDate: String;
begin
  if ANewState then begin
    lUser := '''' + AppSettings.UserID + '''';
    lDate := 'GetDate()';
  end else begin
    lUser := 'Null';
    lDate := 'Null';
  end;

  dmDatabase.ExecuteSQL(Format('UPDATE %s_Occurrence SET Checked = %d, Checked_By = %s, ' +
                               'Checked_Date = %s WHERE %s_Occurrence_Key = ''%s''',
                               [ATable, Ord(ANewState), lUser, lDate, ATable, AKey]));
end;

end.
