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
    $Date: 20/03/08 11:09 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedSpeciesName]
	(@ListItemKey char(16))
RETURNS varchar(200)
AS
BEGIN
	DECLARE	@FormattedName 	VARCHAR(200),
			@ActualName 	VARCHAR(100),
			@PreferredName	VARCHAR(100),
			@Authority 		VARCHAR(100),
			@ActualItalic 	BIT,
			@PreferredItalic BIT,
			@Attribute		VARCHAR(100),
			@RankName		VARCHAR(100)
	SET 	@FormattedName = ''

	SELECT 	@ActualName 	= ITN.Actual_Name, 
			@Authority 		= ITN.Authority, 
			@PreferredName 	= ITN.Preferred_Name, 
			@ActualItalic 	= ITN.Actual_Name_Italic,
			@PreferredItalic= ITN.Preferred_Name_Italic,
			@Attribute		= TV.Attribute,
			@RankName		= TR.Short_Name
	FROM	Index_Taxon_Name	ITN
	JOIN	Taxon_LisT_Item		TLI	ON	TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	JOIN	Taxon_Version		TV	ON	TV.Taxon_Version_Key	= TLI.Taxon_Version_Key
	JOIN	Taxon_Rank			TR	ON	TR.Taxon_Rank_Key		= TLI.Taxon_Rank_Key
	WHERE	ITN.Taxon_List_Item_Key = @ListItemKey

	IF @ActualItalic = 1
		SET @FormattedName = '<i>' + @ActualName + '</i>'
	ELSE
		SET @FormattedName = @ActualName

	SET @FormattedName = @FormattedName + ISNULL(' ' + @Attribute, '') + ISNULL(' ' + @Authority, '') + ISNULL(' [' + @RankName + ']', '')

	IF @ActualName <> @PreferredName
	BEGIN
		SELECT	@Authority = ITN.Authority
		FROM	Taxon_List_Item 	TLI
		JOIN	Index_Taxon_Name 	ITN ON ITN.Taxon_List_Item_Key = TLI.Preferred_Name
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

