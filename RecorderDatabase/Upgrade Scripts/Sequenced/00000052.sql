SET QUOTED_IDENTIFIER ON
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Biotope_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Biotope_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Biotope_Select_ForSearch] 
@SearchText VARCHAR(100)

AS
/*===========================================================================*\
  Description:	Proc. for searching on the Biotope names

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/

SET NOCOUNT ON

	SELECT 		Biotope_List_Item_Key AS Item_Key,
			IsNull(Original_Code + ' - ' + Full_Term, Full_Term) AS DisplayTerm,
			IsNull(Original_Code + ' - ' + Full_Term, Full_Term) AS SearchTerm
	FROM 		Biotope AS B
	INNER JOIN	Biotope_List_Item AS BLI ON BLI.Biotope_Key = B.Biotope_Key
	WHERE 		(Original_Code + ' - ' + Full_Term LIKE @SearchText + '%')
	OR		(Full_Term LIKE @SearchText + '%')
	ORDER BY 	IsNull(Original_Code + ' - ' + Full_Term, Full_Term)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Biotope_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Biotope_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Biotope_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified concept group.

  Parameters:   @ConceptGroup - key of the concept group
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
	@SearchKey char(16),
	@SearchText varchar(100),
	@SearchSize int = NULL
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

    SET @SearchSize =   ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT Concept_Key as Item_Key,
	  CASE WHEN Author_Copy IS NULL THEN
		Item_Name
	  ELSE
		Item_Name + ' ' + Author_Copy
	  END AS DisplayTerm,
	  CASE WHEN Author_Copy IS NULL THEN
		Plaintext
	  ELSE
		Plaintext + ' ' + Author_Copy COLLATE SQL_Latin1_General_CP1_CI_AI
	  END AS SearchTerm,
	  Author_copy,
	  Concept_Rank_Key
	FROM VW_ConceptTerm
	WHERE Concept_Group_Key = @SearchKey
	AND (Plaintext like @SearchText + '%'
	OR Author_Copy like @SearchText + '%')
	AND Is_Current = 1
	ORDER BY SearchTerm, Author_Copy

    SET ROWCOUNT 0    
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Individual_Select_ForNameSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Individual_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of individual names matching a search string.

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Individual_Select_ForNameSearch]
	@SearchText varchar(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT	Name_Key AS Item_Key, 
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Individual
	WHERE 	Forename LIKE @SearchText + '%'
	OR 	(Initials LIKE @SearchText + '%' AND Forename IS NULL)
	OR 	Surname LIKE @SearchText + '%'
	OR 	Title LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Individual_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Individual_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Individual_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LastSurveyEvent_ForLocation_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LastSurveyEvent_ForLocation_Select]
GO

/*===========================================================================*\
  Description:	Returns the last known survey event date for a location.

  Parameters:	@Key	Location  Key

  Created:	Feb 2004

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LastSurveyEvent_ForLocation_Select]
	@Key char(16)
AS
	DECLARE @EventVDS int,
		@EventVDE int,
		@EventVDT varchar(2),
		@SampleVDS int,
		@SampleVDE int,
		@SampleVDT varchar(2)	
		
	SELECT TOP 1
		@EventVDS = Vague_Date_Start, 
		@EventVDE = Vague_Date_End, 
		@EventVDT = Vague_Date_Type
	FROM 	Survey_Event
	WHERE 	Location_Key = @Key
	ORDER BY Vague_Date_End DESC, Vague_Date_Start DESC
	
	SELECT TOP 1 
		@SampleVDS = Vague_Date_Start, 
		@SampleVDE = Vague_Date_End, 
		@SampleVDT = Vague_Date_Type
	FROM 	Sample
	WHERE 	Location_Key = @Key
	ORDER BY Vague_Date_End DESC, Vague_Date_Start DESC
	
	-- Find the latest of the 2 dates
	IF (@SampleVDE > @EventVDE) OR (@EventVDT IS NULL)
		SELECT @SampleVDS AS Vague_Date_Start, @SampleVDE AS Vague_Date_End, @SampleVDT AS Vague_Date_Type
	ELSE 
	IF (@SampleVDE < @EventVDE) OR (@SampleVDT IS NULL)
		SELECT @EventVDS AS Vague_Date_Start, @EventVDE AS Vague_Date_End, @EventVDT AS Vague_Date_Type
	ELSE
	-- end date same, match on start date
	IF (@SampleVDS > @EventVDS) OR (@EventVDT IS NULL)
		SELECT @SampleVDS AS Vague_Date_Start, @SampleVDE AS Vague_Date_End, @SampleVDT AS Vague_Date_Type
	ELSE
		SELECT @EventVDS AS Vague_Date_Start, @EventVDE AS Vague_Date_End, @EventVDT AS Vague_Date_Type
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LastSurveyEvent_ForLocation_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LastSurveyEvent_ForLocation_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LastSurveyEvent_ForLocation_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_AllChildrenForLevel') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_Locations_Select_AllChildrenForLevel
GO

/*===========================================================================*\
  Description:	Returns All locations and features below a given location.

  Parameters:
	@Key	Location key

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Locations_Select_AllChildrenForLevel
	@Key 	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE	@Results TABLE (
		TableName	VARCHAR(20) COLLATE Database_Default,
		ItemKey		CHAR(16) COLLATE Database_Default
	)
	
	INSERT INTO @Results
	SELECT	'Location', Location_Key
	FROM	Location
	WHERE	Location_Key = @Key
	
	-- Gather hierarchy
	WHILE @@RowCount > 0
		INSERT INTO @Results
		SELECT	'Location', Location_Key
		FROM	Location
		JOIN	@Results		ON	ItemKey = Parent_Key
		-- Don't want to loop forever!
		WHERE	Location_Key	NOT IN (SELECT ItemKey FROM @Results)
	
	INSERT INTO @Results
	SELECT	'Location_Feature', Location_Feature_Key
	FROM	Location_Feature
	JOIN	@Results			ON	ItemKey = Location_Key

	SELECT 	ItemKey, TableName
	FROM	@Results
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_AllChildrenForLevel') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Locations_Select_AllChildrenForLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Locations_Select_AllChildrenForLevel TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Locations_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Locations_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of individual names matching a search string.

  Parameters:	@SearchText

  Created:	August 2003
 
  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Locations_Select_ForSearch]
	@SearchText varchar(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT		LN.Location_Key AS Item_Key, 
			LN.Item_Name AS DisplayTerm, 
			LN.Item_Name AS SearchTerm
	FROM		Location_Name AS LN
	WHERE 		LN.Item_Name LIKE @SearchText + '%'
	ORDER BY 	DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Locations_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Name_Select_ForNameSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Name_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of names matching a search string..

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Name_Select_ForNameSearch]
	@SearchText varchar(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT	Name_Key AS Item_Key, 'Individual' AS Type,
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Individual
	WHERE 	Forename LIKE @SearchText + '%'
	OR 	(Initials LIKE @SearchText + '%' AND Forename IS NULL)
	OR 	Surname LIKE @SearchText + '%'
	OR 	Title LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'
UNION
	SELECT	Name_Key AS Item_Key, 'Organisation' AS Type,
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Organisation
	WHERE	Acronym LIKE @SearchText + '%'
	OR	Full_Name LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'

	-- Set the order here for all
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Name_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Name_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Organisation_Select_ForNameSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Organisation_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of organisation names matching a search string.

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Organisation_Select_ForNameSearch]
	@SearchText varchar(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT	Name_Key AS Item_Key, 
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Organisation
	WHERE	Acronym LIKE @SearchText + '%'
	OR	Full_Name LIKE @SearchText + '%'
	OR   Acronym + ', ' + Full_Name LIKE @SearchText + '%'
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Organisation_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Organisation_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Organisation_Select_ForNameSearch TO [Dev - JNCC SQL]
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
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

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
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

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
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

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
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForSurveyTag') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Surveys_Select_ForSurveyTag]
GO

/*===========================================================================*\
  Description:	Retrieve the surveys for a tag

  Parameters:
	@Key		Survey Tag Key

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Surveys_Select_ForSurveyTag]
	@Key 	CHAR(16),
	@Order	BIT = 0
AS
	SET NOCOUNT ON

	DECLARE @Results TABLE (
		Survey_Tag_Key	CHAR(16) COLLATE Database_Default,
		Survey_Tag_Term	VARCHAR(150) COLLATE Database_Default,
		Concept_Key		CHAR(16) COLLATE Database_Default,
		Survey_Key		CHAR(16) COLLATE Database_Default,
		Item_Name		VARCHAR(100) COLLATE Database_Default,
		FullName		VARCHAR(60) COLLATE Database_Default,
		Entered_By		CHAR(16) COLLATE Database_Default,
		Custodian		CHAR(8) COLLATE Database_Default
	)

	CREATE TABLE #Synonyms (
		Item_Key 		CHAR(16) COLLATE Database_Default, 
		Item_Name 		VARCHAR(100) COLLATE Database_Default, 
		Language_Key 	VARCHAR(4) COLLATE Database_Default, 
		Language 		VARCHAR(50) COLLATE Database_Default, 
		Custodian 		CHAR(8) COLLATE Database_Default, 
		Entered_By 		CHAR(16) COLLATE Database_Default
	)
	INSERT INTO #Synonyms EXEC usp_ListSynonyms_Select_ForConcept @Key
	INSERT INTO #Synonyms (Item_Key) VALUES (@Key)

	INSERT INTO @Results
	SELECT		Survey_Tag_Key,
				CT.PlainText,
				CP.Preferred_Concept_Key,
				ST.Survey_Key, 
				SU.Item_Name,
				CASE N.Organisation
					WHEN 0 THEN CASE WHEN I.Forename IS NULL THEN I.Surname ELSE I.Forename + ' ' + I.Surname END
					WHEN 1 THEN O.Full_Name
					ELSE ''
				END,
				S.User_Name_Key, 
				ST.Custodian
	FROM		Survey_Tag				ST
	JOIN		VW_ConceptTerm			CT	ON	CT.Concept_Key	=	ST.Concept_Key
	JOIN		VW_ConceptTermPreferred CP 	ON 	CP.Concept_Key 	= 	ST.Concept_Key
	JOIN		Survey					SU	ON	SU.Survey_Key	=	ST.Survey_Key
	JOIN		Session					S	ON	S.Session_ID	= 	ST.Entered_Session_ID
	JOIN		Name					N	ON	N.Name_Key		=	SU.Run_By
	LEFT JOIN	Individual				I	ON	I.Name_Key		=	N.Name_Key
											AND	N.Organisation	=	0
	LEFT JOIN	Organisation 			O 	ON	O.Name_Key		=	N.Name_Key
											AND	N.Organisation	=	1
	WHERE		ST.Concept_Key				IN	(SELECT Item_Key FROM #Synonyms)

	DROP TABLE #Synonyms

	/* Return list sorted as requested */
	SELECT	R.Survey_Tag_Key,
			R.Concept_Key,
			R.Survey_Key,
			R.Item_Name AS SurveyName,
			R.FullName,
			CASE 
				WHEN ST.Concept_Key <> R.Concept_Key THEN '<' + R.Survey_Tag_Term + '> ' 
				ELSE '' 
			END + R.Item_Name + ' - ' + R.FullName AS ItemName,
			R.Entered_By,
			R.Custodian
	FROM	@Results	R
	JOIN	Survey_Tag	ST	ON	ST.Survey_Tag_Key = R.Survey_Tag_Key
	ORDER BY 
		CASE WHEN @Order = 0 THEN Item_Name ELSE FullName END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForSurveyTag') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Surveys_Select_ForSurveyTag'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTag TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForSurveyTagExport') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Surveys_Select_ForSurveyTagExport]
GO

/*===========================================================================*\
  Description:	Returns all Surveys for a survey tag concept and its children.

  Parameters:
	@Key	Concept Key of tag

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Surveys_Select_ForSurveyTagExport
	@Key 	CHAR(16)
AS
	SET NOCOUNT ON

	-- Doing this once is enough.
	DECLARE	@HierarchyRelationTypeKey CHAR(16)
	SELECT	@HierarchyRelationTypeKey = Hierarchy_Relation_Type_Key
	FROM	Concept_Group	CG
	JOIN	Concept			C	ON	C.Concept_Group_Key = CG.Concept_Group_Key
	WHERE	C.Concept_Key	=	@Key

	-- Need somewhere to put all those keys.
	DECLARE	@Concepts TABLE (ConceptKey CHAR(16) COLLATE Database_Default)

	INSERT INTO @Concepts VALUES (@Key)

	-- Gather all nested concepts.
	WHILE @@RowCount > 0
		INSERT INTO @Concepts
		SELECT		To_Concept_Key
		FROM		Concept_Relation
		JOIN		@Concepts		ON	ConceptKey = From_Concept_Key
		WHERE		To_Concept_Key	NOT IN (SELECT ConceptKey FROM @Concepts)
		AND			Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey

	-- Return all relevant survey keys.
	SELECT 	DISTINCT Survey_Key
	FROM	Survey_Tag
	JOIN	@Concepts	ON	ConceptKey = Concept_Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select_ForSurveyTagExport') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Surveys_Select_ForSurveyTagExport'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select_ForSurveyTagExport TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Survey_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Survey_Select_ForSearch] 
GO

CREATE PROCEDURE [dbo].[usp_Survey_Select_ForSearch] 
@SearchText VARCHAR(100)

AS
/*===========================================================================*\
  Description:	Proc for the search dialog window that searched for survey 
		names and their authors. 

  Parameters:	@SearchText

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/

SET NOCOUNT ON

SELECT 	SV.Survey_Key AS Item_Key, 
	SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) AS DisplayTerm,
	SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) AS SearchTerm
FROM 	Survey AS SV
WHERE 	SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) LIKE @SearchText + '%'
ORDER BY DisplayTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Survey_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Survey_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Survey_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Terms_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Terms_Select_ForSearch]
GO

/*===========================================================================*\
  Description: 	Search proc for Term table.

  Parameters:	@SearchText
		@SearchKey	Language_key

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/08 11:00 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Terms_Select_ForSearch] 
	@SearchText varchar(100),
	@SearchKey varchar(4) = NULL
AS

SET NOCOUNT ON
	-- NB: This proc does want to search on Item_Name (as opposed to 
	-- Plaintext). This is because as we are dealing with terms, we could
	-- have two terms with the same Plaintext, but different Item_Name
	-- i.e. one is italic, one isn't.

	IF @SearchKey IS NOT NULL 
		SELECT 
				T.Term_Key AS Item_Key,
				Item_Name AS DisplayTerm,
				Item_Name AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		PlainText LIKE @SearchText + '%'  -- Still want to filter on PlainText though
		AND 		Language_Key = @SearchKey
		ORDER BY 	Plaintext
	ELSE
		SELECT 
				T.Term_Key AS Item_Key,
				Item_Name AS DisplayTerm,
				Item_Name AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		PlainText LIKE @SearchText + '%'  -- Still want to filter on PlainText though
		ORDER BY 	Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Terms_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Terms_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

