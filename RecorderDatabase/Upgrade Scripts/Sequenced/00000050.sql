/*===========================================================================*\
  Increase size of DATA field to cope with Measurement_Unit_Value.
\*===========================================================================*/
ALTER TABLE Location_Data
	ALTER COLUMN Data VARCHAR(20) NOT NULL


/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetPartialTaxonSearchText]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetPartialTaxonSearchText
GO


/*===========================================================================*\
  Description:	Returns the taxon search text formatted for use when matching partial taxon names.
		Up to three individual words are searched for individually, separated by spaces.
		Wildcards are used to create a search pattern for use with the LIKE operator.
		If any search term other than the first begins with a dot, match the end of that word.

  Parameters:	@SearchText

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 11/04/08 14:45 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetPartialTaxonSearchText]
	(@SearchText varchar(100))
RETURNS varchar(110)
AS
BEGIN
	DECLARE	@FormattedSearchText varchar(110)		-- Holds the result
			
	DECLARE @SpacePos INT, 
			@FirstWord	VARCHAR(100),
			@SecondWord VARCHAR(100),
			@RestOfText	VARCHAR(100)

	SET @FormattedSearchText = LTRIM(RTRIM(@SearchText))
	SET @SpacePos = CHARINDEX(' ', @FormattedSearchText)
	IF @SpacePos > 0
	BEGIN
		SET @FirstWord = LEFT(@FormattedSearchText, @SpacePos - 1) + '% '
		SET @RestOfText = LTRIM(SUBSTRING(@FormattedSearchText, @SpacePos + 1, 110))

		SET @SpacePos = CHARINDEX(' ', @RestOfText)
		IF @SpacePos > 0
		BEGIN
			SET @SecondWord = LEFT(@RestOfText, @SpacePos - 1)
			SET @RestOfText = LTRIM(SUBSTRING(@RestOfText, @SpacePos + 1, 110))
			IF LEFT(@SecondWord, 1) = '.'
			BEGIN
				SET @SecondWord = '%' + SUBSTRING(@SecondWord, 2, 110) + ' '
			END
			ELSE
			BEGIN
				SET @SecondWord = @SecondWord + '% '
			END
		END

		IF LEFT(@RestOfText, 1) = '.'
		BEGIN
			SET @RestOfText = '%' + SUBSTRING(@RestOfText, 2, 110)
		END
		ELSE
		BEGIN
			SET @RestOfText = @RestOfText + '%'
		END

		SET @FormattedSearchText = @FirstWord + ISNULL(@SecondWord, '') + @RestOfText
	END
	ELSE
	BEGIN
		SET @FormattedSearchText = @FormattedSearchText + '%'
	END

	RETURN @FormattedSearchText
END

GO	

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	
		@SearchKey	Taxon List key
		@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 11/04/08 14:45 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch]
	@SearchKey 	CHAR(16) = NULL,
	@SearchText VARCHAR(100)
AS
	IF @SearchKey IS NULL
		SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
				dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '')
				+ ISNULL(' - ' + TL.Item_Name, '')                       AS DisplayTerm,
				Actual_Name 
				+ ISNULL(' ' + ITN.Authority, '') 
				+ ISNULL(' - ' + TL.Item_Name, '')                       AS SearchTerm

		FROM	Index_Taxon_Name 	ITN
		JOIN 	Taxon_List_Version 	TLV ON TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
		JOIN 	Taxon_List 			TL 	ON TL.Taxon_List_Key 			= TLV.Taxon_List_Key
		JOIN	Taxon_List_Item 	TLI ON TLI.Taxon_List_Item_Key 		= ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name 						LIKE @SearchText + '%'
		OR		Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText + '%'
		OR 		ITN.Authority 						LIKE @SearchText + '%')
		AND		TLI.Taxon_List_Version_To 			IS NULL
		AND 	TLV.Version >= (
					SELECT 	MAX(Version) FROM Taxon_List_Version 
					WHERE 	Taxon_List_Key = (SELECT Taxon_List_Key FROM Taxon_List_Version WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
					AND		Version_Is_Amendment = 0)
		ORDER BY SearchTerm
	ELSE
		SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
				dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '')            AS DisplayTerm,
				Actual_Name 
				+ ISNULL(' ' + ITN.Authority, '')                        AS SearchTerm

		FROM	Index_Taxon_Name 	ITN
		JOIN 	Taxon_List_Version 	TLV ON	TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key 
										AND TLV.Taxon_List_Key 			= @SearchKey
		JOIN	Taxon_List_Item 	TLI ON 	TLI.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name 						LIKE @SearchText + '%'
		OR		Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText + '%'
		OR 		ITN.Authority 						LIKE @SearchText + '%')
		AND		TLI.Taxon_List_Version_To 			IS NULL
		AND 	TLV.Version >= (
					SELECT 	MAX(Version) FROM Taxon_List_Version
					WHERE 	Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
					AND 	Version_Is_Amendment = 0)
		ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Species_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch_Partial') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch_Partial]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string,
					using partial taxon matching.

  Parameters:	@SearchKey
		@SearchText

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 11/04/08 14:45 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch_Partial]
	@SearchKey 	CHAR(16) = NULL,
	@SearchText VARCHAR(100)
AS
	SET @SearchText = dbo.[ufn_GetPartialTaxonSearchText](@SearchText)

	IF @SearchKey IS NULL
		SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
				dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '') 
				+ ISNULL(' - ' + TL.Item_Name, '')                       AS DisplayTerm, 
				Actual_Name 
				+ ISNULL(' ' + ITN.Authority, '') 
				+ ISNULL(' - ' + TL.Item_Name, '')                       AS SearchTerm

		FROM	Index_Taxon_Name 	ITN
		JOIN 	Taxon_List_Version 	TLV ON TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
		JOIN 	Taxon_List 			TL 	ON TL.Taxon_List_Key 			= TLV.Taxon_List_Key
		JOIN	Taxon_List_Item 	TLI ON TLI.Taxon_List_Item_Key 		= ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name 						LIKE @SearchText
		OR		Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText
		OR 		ITN.Authority 						LIKE @SearchText)
		AND		TLI.Taxon_List_Version_To 			IS NULL
		AND 	TLV.Version >= (SELECT MAX(Version)
			FROM Taxon_List_Version
			WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
			WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
			AND Version_Is_Amendment = 0)
		ORDER BY SearchTerm
	ELSE
		SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
				dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '')            AS DisplayTerm, 
				Actual_Name 
				+ ISNULL(' ' + ITN.Authority, '')                        AS SearchTerm
	
		FROM	Index_Taxon_Name 	ITN
		JOIN 	Taxon_List_Version 	TLV ON 	TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key 
										AND TLV.Taxon_List_Key 			= @SearchKey
		JOIN	Taxon_List_Item 	TLI ON 	TLI.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name 						LIKE @SearchText
		OR		Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText
		OR 		ITN.Authority 						LIKE @SearchText)
		AND		TLI.Taxon_List_Version_To 			IS NULL
		AND 	TLV.Version >= (SELECT MAX(Version)
			FROM Taxon_List_Version
			WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
			WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
			AND Version_Is_Amendment = 0)
		ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch_Partial') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Species_Select_ForSearch_Partial'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 11/04/08 14:45 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
	@SearchKey 	CHAR(16),
	@SearchText VARCHAR(100)
AS
	SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
			dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) 
			+ ISNULL(' - [' + ITN.Abbreviation + ']', '')            AS DisplayTerm, 
			Actual_Name + ' ' 
			+ ISNULL(ITN.Authority, '')                              AS SearchTerm
	FROM	Index_Taxon_Name 	ITN
	JOIN 	Taxon_List_Version 	TLV ON 	TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key 
									AND TLV.Taxon_List_Key 			= @SearchKey
	JOIN	Taxon_List_Item 	TLI ON 	TLI.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key

	WHERE 	(Actual_Name 						LIKE @SearchText + '%'
	OR		Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText + '%'
	OR 		ITN.Authority 						LIKE @SearchText + '%')
	AND		TLI.Taxon_List_Version_To 			IS NULL
	AND 	TLV.Version >= (SELECT MAX(Version)
		FROM Taxon_List_Version
		WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
		WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
		AND Version_Is_Amendment = 0)
	ORDER BY SearchTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Species_Select_ForSearchByList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList_Partial') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearchByList_Partial]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string,
					using partial taxon matching.

  Parameters:	@SearchText

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 11/04/08 14:45 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearchByList_Partial]
	@SearchKey 	CHAR(16),
	@SearchText VARCHAR(100)
AS
	SET @SearchText = dbo.[ufn_GetPartialTaxonSearchText](@SearchText)

	SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
			dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) 
			+ ISNULL(' - [' + ITN.Abbreviation + ']', '')            AS DisplayTerm, 
			Actual_Name + ' ' 
			+ ISNULL(ITN.Authority, '')                              AS SearchTerm

	FROM	Index_Taxon_Name 	ITN
	JOIN 	Taxon_List_Version 	TLV ON 	TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key 
									AND TLV.Taxon_List_Key 			= @SearchKey
	JOIN	Taxon_List_Item 	TLI ON 	TLI.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key

	WHERE 	(Actual_Name 						LIKE @SearchText
	OR		Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText
	OR 		ITN.Authority 						LIKE @SearchText)
	AND		TLI.Taxon_List_Version_To 			IS NULL
	AND 	TLV.Version >= (SELECT MAX(Version)
		FROM Taxon_List_Version
		WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
		WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
		AND Version_Is_Amendment = 0)
	ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList_Partial') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Species_Select_ForSearchByList_Partial'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpeciesFullySpecified_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpeciesFullySpecified_Select]
GO

/*===========================================================================*\
  Description:	Return a species name with checklist name.

  Parameters:	
		@ItemKey
		@Output

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 11/04/08 14:45 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpeciesFullySpecified_Select]
	@ItemKey 	CHAR(16),
	@Output 	VARCHAR(300) OUTPUT
AS
	SELECT	@Output = dbo.ufn_GetFormattedSpeciesName(Taxon_List_Item_Key) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '') 
				+ ' - ' + TL.Item_Name
	FROM	Index_Taxon_Name 	ITN
	JOIN	Taxon_List_Version 	TLV ON TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
	JOIN	Taxon_List 			TL 	ON TL.Taxon_List_Key 			= TLV.Taxon_List_Key
	WHERE	ITN.Taxon_List_Item_Key = @ItemKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpeciesFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_SpeciesFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [Dev - JNCC SQL]
END
GO