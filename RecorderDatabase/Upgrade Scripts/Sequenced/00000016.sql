/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedBiotopeName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedBiotopeName
GO

/*===========================================================================*\
  Description:	Returns the formatted biotope name for the given biotope list item.

  Parameters:	@ListItemKey

  Created:	

  Last revision information:
    $Revision: 1 $
    $Date: 6/07/04 12:40 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedBiotopeName]
	(@ListItemKey char(16))
RETURNS varchar(200)
AS
BEGIN
	DECLARE	@FormattedName varchar(200)

	SELECT		@FormattedName = IsNull(Original_Code + ', ', '') + Short_Term
	FROM 		Biotope B 
	INNER JOIN 	Biotope_List_Item BLI ON B.Biotope_Key = BLI.Biotope_Key 
	WHERE 		BLI.Biotope_List_Item_Key = @ListItemKey

	RETURN @FormattedName
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedBiotopeName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
BEGIN
   	PRINT 'Setting up security on function ufn_GetFormattedBiotopeName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [Dev - JNCC SQL]
END
GO


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
    $Revision: 1 $
    $Date: 6/07/04 12:40 $
    $Author: Johnvanbreda $

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
				IsNull(Cast(Title AS varchar(1000)), '')
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


/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedSpeciesName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedSpeciesName
GO

/*===========================================================================*\
  Description:	Returns the formatted taxon name for the given taxon list item.

  Parameters:	@Key

  Created:	

  Last revision information:
    $Revision: 1 $
    $Date: 6/07/04 12:40 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedSpeciesName]
	(@ListItemKey char(16))
RETURNS varchar(200)
AS
BEGIN
	DECLARE	@FormattedName varchar(200),
		@ActualName varchar(100),
		@PreferredName varchar(100),
		@Authority varchar(100),
		@ActualItalic bit,
		@PreferredItalic bit
	SET @FormattedName = ''

	SELECT 	@ActualName = Actual_Name, 
		@Authority = Authority, 
		@PreferredName = Preferred_Name, 
		@ActualItalic = Actual_Name_Italic,
		@PreferredItalic = Preferred_Name_Italic
	FROM	Index_Taxon_Name
	WHERE	Taxon_List_Item_Key = @ListItemKey

	IF @ActualItalic = 1
		SET @FormattedName = '<i>' + @ActualName + '</i>' + ISNULL(' ' + @Authority, '')
	ELSE
		SET @FormattedName = @ActualName + ISNULL(' ' + @Authority, '')

	IF @ActualName <> @PreferredName
	BEGIN
		SELECT	@Authority = ITN.Authority
		FROM	Taxon_List_Item TLI
		JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLI.Preferred_Name
		WHERE	TLI.Taxon_List_Item_Key = @ListItemKey

		IF @PreferredItalic = 1
			SET @FormattedName = @FormattedName + ' (' + '<i>' + @PreferredName + '</i>' + ISNULL(' ' + @Authority, '') + ')'
		ELSE
			SET @FormattedName = @FormattedName + ' (' + @PreferredName + ISNULL(' ' + @Authority, '') + ')'
	END	

	RETURN @FormattedName
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedSpeciesName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
BEGIN
   	PRINT 'Setting up security on function ufn_GetFormattedSpeciesName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [Dev - JNCC SQL]
END
GO


