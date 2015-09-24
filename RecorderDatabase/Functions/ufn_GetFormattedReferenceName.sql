/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedReferenceName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedReferenceName
GO

/*===========================================================================*\
  Description:	Returns a formatted reference.

  Parameters:	@SourceKey

  Created:	

  Last revision information:
    $Revision: 2 $
    $Date: 5/08/04 18:28 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedReferenceName]
	(@SourceKey char(16))
RETURNS varchar(300)
AS
BEGIN

	DECLARE	@FormattedRef varchar(300)

	SELECT	@FormattedRef = Author + 
				' - ' + 
				Cast(dbo.FormatDatePart(Year_Vague_date_Start, Year_Vague_Date_End, Year_Vague_Date_Type, 0) AS varchar(50)) + 
				', ' +
				IsNull(dbo.ufn_RtfToPlainText(Title), '')
	FROM	Reference R
	JOIN	vw_Reference_Authors RA ON RA.Source_Key = R.Source_Key
	WHERE	R.Source_Key = @SourceKey

	RETURN @FormattedRef
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedReferenceName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
BEGIN
   	PRINT 'Setting up security on function ufn_GetFormattedReferenceName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.ufn_GetFormattedReferenceName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.ufn_GetFormattedReferenceName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.ufn_GetFormattedReferenceName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedReferenceName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedReferenceName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.ufn_GetFormattedReferenceName TO [Dev - JNCC SQL]
END
GO

