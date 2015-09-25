/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_HtmlToPlainText]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_HtmlToPlainText
GO

/*===========================================================================*\
  Description:	Removes HTML formatting tags from a string and returns the 
		results

  Parameters:	@Dirty (HTML formatted text)

  Created:	Feb 2007

  Last revision information:
    $Revision: 1 $
    $Date: 13/02/07 14:48 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_HtmlToPlainText]	
	(@Dirty varchar(4000))
	Returns varchar(4000)
As

Begin
	Declare @Start int,
		@End int,
		@Length int

	While CharIndex('<', @Dirty) > 0 And CharIndex('>', @Dirty, CharIndex('<', @Dirty)) > 0
	    Begin
		Select @Start = CharIndex('<', @Dirty), 
		  @End = CharIndex('>', @Dirty, CharIndex('<', @Dirty))
		Select @Length = (@End - @Start) + 1
		If @Length > 0
		    Begin
			Select @Dirty = Stuff(@Dirty, @Start, @Length, '')
		    End
	    End

	return @Dirty
End

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_HtmlToPlainText]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_HtmlToPlainText'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_HtmlToPlainText TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_HtmlToPlainText TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_HtmlToPlainText TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_HtmlToPlainText TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_HtmlToPlainText TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_HtmlToPlainText TO [Dev - JNCC SQL]
	END
GO
