/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedTerm]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedTerm
GO

/*===========================================================================*\
  Description:	Returns the formatted term for the supplied term, author, common
		term and author

		Null fields are omitted.

  Parameters:	@Term, @Author, @CommonTerm, @CommonAuthor

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/04 11:01 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedTerm]
	(@Term varchar(150),
   @Author varchar(100),
   @CommonTerm varchar(150),
   @CommonAuthor varchar(100))
RETURNS varchar(505)
AS
BEGIN
  DECLARE @FormalName varchar(251)  -- Term (varchar(150)) + space + Author (varchar(100)
  DECLARE @CommonName varchar(251)
	DECLARE	@FormattedTerm varchar(505) -- 2 names + extra space and 2 brackets

	SET @FormattedTerm = ''

  SET @FormalName = @Term
  IF @Author IS NOT NULL
    SET @FormalName = @FormalName + ' ' + @Author

  IF @CommonTerm IS NOT NULL BEGIN
    SET @CommonName = @CommonTerm
    IF @CommonAuthor IS NOT NULL
      SET @CommonName = @CommonName + ' ' + @CommonAuthor
  END

  IF @CommonName IS NOT NULL AND @CommonName<>@FormalName
    SET @FormattedTerm = @CommonName + ' (' + @FormalName + ')'
  ELSE
   SET @FormattedTerm = @FormalName

  RETURN @FormattedTerm
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedTerm]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetFormattedTerm'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetFormattedTerm TO [Dev - JNCC SQL]
	END
GO
