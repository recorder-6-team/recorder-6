
if exists (Select * from sysobjects where 
type = 'fn' and uid = user_ID('dbo') and name =
'IncrementKey')
drop function dbo.IncrementKey
go

--Function used in creation of keys for tables
-- Author: Polly Shaw
-- Created on : 6 Jan 2003
    /*
    $History: IncrementKey.sql $
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

create function dbo.IncrementKey(

@OldKey Char(8))

returns Char(8)
as
begin
Declare @ChangeSection as Char(8),
	@iCurrentChar as integer,
	@chNewChar as Char,
	@Result as Char(8)
Set @iCurrentChar = 8
set @ChangeSection = @OldKey
set @chNewChar = dbo.IncrementChar(Substring(@ChangeSection, @iCurrentChar, 1))
set @ChangeSection = Stuff(@ChangeSection, @iCurrentChar,1, @chNewChar)
while (@chNewChar = '0') and (@iCurrentChar >0)
begin
	set @iCurrentChar = @iCurrentChar -1;
	set @chNewChar = dbo.IncrementChar(Substring(@ChangeSection, @iCurrentChar, 1))
	set @ChangeSection = Stuff(@ChangeSection, @iCurrentChar ,1, @chNewChar)

end
if @iCurrentChar = 0
--run out of keys
--can't raise an error, so return same key
--will cause insert error later
set @ChangeSection = @OldKey

return @ChangeSection

end
go

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[IncrementKey]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function IncrementKey'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.IncrementKey TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.IncrementKey TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.IncrementKey TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.IncrementKey TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.IncrementKey TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.IncrementKey TO [Dev - JNCC SQL]
	END
GO
