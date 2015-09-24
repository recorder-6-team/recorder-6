/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE Id = OBJECT_ID(N'[dbo].[ufn_FormatEventOwners]')
	   AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
  DROP FUNCTION [dbo].[ufn_FormatEventOwners]
GO

/*===========================================================================*\
  Description:	Returns the list of survey event owners as a semi-colon 
		separated string

  Parameters:	@Key	survey event key

  Created:	Jan 2004

  Last revision information:
    $Revision: 2 $
    $Date: 20/05/04 14:57 $
    $Author: Anthonysimpson $

\*===========================================================================*/   
CREATE FUNCTION dbo.ufn_FormatEventOwners(@Key char(16))
RETURNS varchar(8000)

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @ItemString varchar(70)
DECLARE @NameKey char(16)
DECLARE @OwnerType varchar(20)

DECLARE csrEventOwner CURSOR
FOR
  SELECT EO.NAME_KEY, OT.SHORT_NAME
	FROM SURVEY_EVENT_OWNER EO
	INNER JOIN SURVEY_EVENT_OWNER_TYPE OT ON OT.SURVEY_EVENT_OWNER_TYPE_KEY = EO.SURVEY_EVENT_OWNER_TYPE_KEY
	WHERE EO.SURVEY_EVENT_KEY=@Key

OPEN csrEventOwner

FETCH NEXT FROM csrEventOwner INTO @NameKey, @OwnerType

IF @@FETCH_STATUS = 0 
  SELECT @ReturnString = dbo.ufn_GetFormattedName(@NameKey) + ' (' + @OwnerType + ')'

WHILE @@FETCH_STATUS = 0
BEGIN
	FETCH NEXT FROM csrEventOwner INTO @NameKey, @OwnerType
	SELECT @ItemString = dbo.ufn_GetFormattedName(@NameKey) + ' (' + @OwnerType + ')'
	IF @@FETCH_STATUS = 0 
		SELECT @ReturnString = @ReturnString + ';' + @ItemString
END

CLOSE csrEventOwner
DEALLOCATE csrEventOwner

RETURN @ReturnString
--****************************************************************************************************

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_FormatEventOwners]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_FormatEventOwners'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_FormatEventOwners TO [Dev - JNCC SQL]
	END
GO
