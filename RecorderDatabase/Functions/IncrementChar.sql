if exists (Select * from sysobjects where 
type = 'fn' and uid = user_ID('dbo') and name =
'IncrementChar')
drop function dbo.IncrementChar
go

--Function used in creation of keys for tables
-- Author: Polly Shaw
-- Created on : 6 Jan 2003
    /*
    $History: IncrementChar.sql $
 * 
 * *****************  Version 2  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 14:57
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * Permissions corrected.
 * 
 * *****************  Version 1  *****************
 * User: Pollyshaw    Date: 6/02/03    Time: 14:14
 * Created in $/JNCC/Development/Build/SQL Scripts/Functions
 * Function generating new key for tables
*/

create function dbo.IncrementChar( 
@IncChar  Char) returns Char


begin

declare @Result as Char
if @incChar = '9'  
	set @Result = 'A'
else if @incChar = 'Z'
	set @Result = '0'
else 
	set @Result = Char(Ascii(@incChar) + 1);

return @Result

end
go

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[IncrementChar]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function IncrementChar'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.IncrementChar TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.IncrementChar TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.IncrementChar TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.IncrementChar TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.IncrementChar TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.IncrementChar TO [Dev - JNCC SQL]
	END
GO