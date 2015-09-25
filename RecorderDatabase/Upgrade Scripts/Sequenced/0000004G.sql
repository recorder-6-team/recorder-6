SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BiotopeDetermination_PreferredType_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_BiotopeDetermination_PreferredType_Get
GO

/*===========================================================================*\
  Description:	Returns the verified value of the given determination type.

  Parameters:	@Key

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_BiotopeDetermination_PreferredType_Get
	@Key char(16),
	@Output char(16) output
AS
	SELECT @Output = Determination_Type_Key
	FROM Biotope_Determination
	WHERE Biotope_Occurrence_Key = @Key AND Preferred = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BiotopeDetermination_PreferredType_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_BiotopeDetermination_PreferredType_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_BiotopeDetermination_PreferredType_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationType_Verified_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_DeterminationType_Verified_Get]
GO

/*===========================================================================*\
  Description:	Returns the verified value of the given determination type.

  Parameters:	@DetTypeKey

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeterminationType_Verified_Get]
	@Key char(16),
	@Output int output
AS
	SELECT @Output = Verified
	FROM Determination_Type
	WHERE Determination_Type_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationType_Verified_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationType_Verified_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationType_Verified_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Language_Find') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Language_Find]
GO

/*===========================================================================*\
  Description:	Finds the language matching the given name. If not found, the
	default language key (en - English) is returned.

  Parameters:

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Language_Find]
	@Key	VARCHAR(4)	OUTPUT,
	@Name	VARCHAR(50)
AS
	SET NOCOUNT ON

	SET 	@Key = NULL

	SELECT	@Key = Language_Key
	FROM	Language
	WHERE	Item_Name  = @Name

	IF @Key IS NULL BEGIN
		SELECT	@Key = Language_Key
		FROM	Language
		WHERE	Item_Name LIKE '%' + @Name + '%'

		IF @Key IS NULL
			SELECT	@Key = Language_Key
			FROM	Language
			WHERE	Language_Key = 'en'
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Language_Find') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Language_Find'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Language_Find TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Language_Find TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Language_Find TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Language_Find TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Language_Find TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Language_Find TO [Dev - JNCC SQL]
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
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Names_Select_AllChildrenForLevel') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_Names_Select_AllChildrenForLevel
GO

/*===========================================================================*\
  Description:	Retrieves keys of all items below a survey/event/sample.

  Parameters:
	@Key	Parent key, either individual or organisation.
	@IsOrg	0 if parent key is individual, 1 if organisation.

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Names_Select_AllChildrenForLevel
	@Key	CHAR(16),
	@IsOrg	BIT
AS
	IF @IsOrg = 0
		SELECT	DISTINCT 'Organisation' AS TableName, O.Name_Key AS ItemKey
		FROM Name_Relation 	NR 
		JOIN Individual 	I 	ON I.Name_Key IN (NR.Name_Key_1, NR.Name_Key_2) 
		JOIN Organisation 	O 	ON O.Name_Key IN (NR.Name_Key_1, NR.Name_Key_2)
		WHERE 	I.Name_Key = @Key
	ELSE
		SELECT	DISTINCT 'Individual' AS TableName, I.Name_Key AS ItemKey
		FROM Name_Relation 	NR 
		JOIN Individual 	I 	ON I.Name_Key IN (NR.Name_Key_1, NR.Name_Key_2) 
		JOIN Organisation 	O 	ON O.Name_Key IN (NR.Name_Key_1, NR.Name_Key_2)
		WHERE 	O.Name_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Names_Select_AllChildrenForLevel') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Names_Select_AllChildrenForLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Observations_Select_AllChildrenForLevel') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_Observations_Select_AllChildrenForLevel
GO

/*===========================================================================*\
  Description:	Retrieves keys of all items below a survey/event/sample.

  Parameters:
	@Key	Depends on the value of @Type
	@Type	Table name of the item for which the children are returned.

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Observations_Select_AllChildrenForLevel
	@Key	CHAR(16),
	@Type	VARCHAR(20)
AS
	-- Retrieve all items for survey tag
	IF @Type = 'Concept' BEGIN
		SELECT	'Survey' AS TableName, SU.Survey_Key AS ItemKey
		FROM	Survey_Tag		ST
		JOIN	Survey			SU	ON	SU.Survey_Key = ST.Survey_Key
		WHERE	ST.Concept_Key 	= 	@Key
		UNION
		SELECT	'Survey_Event' AS TableName, SE.Survey_Event_Key AS ItemKey
		FROM	Survey_Tag		ST
		JOIN	Survey			SU	ON	SU.Survey_Key = ST.Survey_Key
		JOIN	Survey_Event	SE	ON	SE.Survey_Key = SU.Survey_Key
		WHERE	ST.Concept_Key 	= 	@Key
		UNION
		SELECT	'Sample' AS TableName, SA.Sample_Key AS ItemKey
		FROM	Survey_Tag		ST
		JOIN	Survey			SU	ON	SU.Survey_Key 		= ST.Survey_Key
		JOIN	Survey_Event	SE	ON	SE.Survey_Key 		= SU.Survey_Key
		JOIN	Sample			SA	ON	SA.Survey_Event_Key	=	SE.Survey_Event_Key
		WHERE	ST.Concept_Key 	= 	@Key
		UNION		
		SELECT	'Taxon_Occurrence' AS TableName, TOC.Taxon_Occurrence_Key AS ItemKey
		FROM	Survey_Tag			ST
		JOIN	Survey				SU	ON	SU.Survey_Key 		= ST.Survey_Key
		JOIN	Survey_Event		SE	ON	SE.Survey_Key 		= SU.Survey_Key
		JOIN	Sample				SA	ON	SA.Survey_Event_Key	=	SE.Survey_Event_Key
		JOIN	Taxon_Occurrence	TOC	ON	TOC.Sample_Key		= 	SA.Sample_Key
		WHERE	ST.Concept_Key 		= 	@Key
		UNION		
		SELECT	'Biotope_Occurrence' AS TableName, BOC.Biotope_Occurrence_Key AS ItemKey
		FROM	Survey_Tag			ST
		JOIN	Survey				SU	ON	SU.Survey_Key 		= ST.Survey_Key
		JOIN	Survey_Event		SE	ON	SE.Survey_Key 		= SU.Survey_Key
		JOIN	Sample				SA	ON	SA.Survey_Event_Key	=	SE.Survey_Event_Key
		JOIN	Biotope_Occurrence	BOC	ON	BOC.Sample_Key		= 	SA.Sample_Key
		WHERE	ST.Concept_Key 		= 	@Key
	END ELSE
	-- Retrieve all items below survey
	IF @Type = 'Survey' BEGIN
		SELECT	'Survey_Event' AS TableName, SE.Survey_Event_Key AS ItemKey
		FROM	Survey_Event	SE
		WHERE	SE.Survey_Key	= @Key
		UNION
		SELECT	'Sample' AS TableName, SA.Sample_Key AS ItemKey
		FROM	Survey_Event	SE
		JOIN	Sample			SA	ON	SA.Survey_Event_Key	=	SE.Survey_Event_Key
		WHERE	SE.Survey_Key	= @Key
		UNION		
		SELECT	'Taxon_Occurrence' AS TableName, TOC.Taxon_Occurrence_Key AS ItemKey
		FROM	Survey_Event		SE
		JOIN	Sample				SA	ON	SA.Survey_Event_Key	=	SE.Survey_Event_Key
		JOIN	Taxon_Occurrence	TOC	ON	TOC.Sample_Key		= 	SA.Sample_Key
		WHERE	SE.Survey_Key	= @Key
		UNION		
		SELECT	'Biotope_Occurrence' AS TableName, BOC.Biotope_Occurrence_Key AS ItemKey
		FROM	Survey_Event		SE
		JOIN	Sample				SA	ON	SA.Survey_Event_Key	=	SE.Survey_Event_Key
		JOIN	Biotope_Occurrence	BOC	ON	BOC.Sample_Key		= 	SA.Sample_Key
		WHERE	SE.Survey_Key	= @Key
	END ELSE 
	-- Retrieve all items below event
	IF @Type = 'Survey_Event' BEGIN
		SELECT	'Sample' AS TableName, SA.Sample_Key AS ItemKey
		FROM	Sample	SA
		WHERE	SA.Survey_Event_Key = @Key
		UNION
		SELECT	'Taxon_Occurrence' AS TableName, TOC.Taxon_Occurrence_Key AS ItemKey
		FROM	Sample				SA
		JOIN	Taxon_Occurrence	TOC	ON	TOC.Sample_Key	= 	SA.Sample_Key
		WHERE	SA.Survey_Event_Key = @Key
		UNION
		SELECT	'Biotope_Occurrence' AS TableName, BOC.Biotope_Occurrence_Key AS ItemKey
		FROM	Sample				SA
		JOIN	Biotope_Occurrence	BOC	ON	BOC.Sample_Key	= 	SA.Sample_Key
		WHERE	SA.Survey_Event_Key = @Key
	END ELSE 
	-- Retrieve all items below sample
	IF @Type = 'Sample' BEGIN
		SELECT	'Taxon_Occurrence' AS TableName, TOC.Taxon_Occurrence_Key AS ItemKey
		FROM	Taxon_Occurrence	TOC
		WHERE	TOC.Sample_Key = @Key
		UNION
		SELECT	'Biotope_Occurrence' AS TableName, BOC.Biotope_Occurrence_Key AS ItemKey
		FROM	Biotope_Occurrence	BOC
		WHERE	BOC.Sample_Key = @Key
	END	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Observations_Select_AllChildrenForLevel') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Observations_Select_AllChildrenForLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Observations_Select_AllChildrenForLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Observations_Select_AllChildrenForLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Observations_Select_AllChildrenForLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Observations_Select_AllChildrenForLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Observations_Select_AllChildrenForLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Observations_Select_AllChildrenForLevel TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_Surveys_Select]
GO

/*===========================================================================*\
  Description:		Returns all Surveys on the system using the standard caption
		for a survey in alphabetical order. (Saved in JNCC folder).

  Parameters:

  Created:	May 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Surveys_Select]
AS
	SET NOCOUNT ON

	SELECT		SU.Survey_Key,
				SU.Item_Name + ' - ' + 
				CASE N.Organisation
					WHEN 0 THEN CASE WHEN I.Forename IS NULL THEN I.Surname ELSE I.Forename + ' ' + I.Surname END
					WHEN 1 THEN O.Full_Name
					ELSE ''
				END AS Display_Name
	FROM		Survey			SU	
	JOIN		Name			N	ON	N.Name_Key		=	SU.Run_By
	LEFT JOIN	Individual		I	ON	I.Name_Key		=	N.Name_Key
									AND	N.Organisation	=	0
	LEFT JOIN	Organisation 	O 	ON	O.Name_Key		=	N.Name_Key
									AND	N.Organisation	=	1
	ORDER BY	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Surveys_Select') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Surveys_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Surveys_Select TO [Dev - JNCC SQL]
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
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Surveys_Select_ForSurveyTag]
	@Key 	CHAR(16),
	@Order	BIT = 0
AS
	SET NOCOUNT ON

	DECLARE @Results TABLE (
		Survey_Tag_Key	CHAR(16),
		Concept_Key		CHAR(16),
		Survey_Key		CHAR(16),
		Item_Name		VARCHAR(100),
		FullName		VARCHAR(60),
		Entered_By		CHAR(16),
		Custodian		CHAR(8)
	)

	INSERT INTO @Results
	SELECT		ST.Survey_Tag_Key,
				ST.Concept_Key,
				ST.Survey_Key, 
				SU.Item_Name,
				CASE N.Organisation
					WHEN 0 THEN CASE WHEN I.Forename IS NULL THEN I.Surname ELSE I.Forename + ' ' + I.Surname END
					WHEN 1 THEN O.Full_Name
					ELSE ''
				END,
				S.User_Name_Key, 
				ST.Custodian
	FROM		Survey_Tag		ST
	JOIN		Survey			SU	ON	SU.Survey_Key	=	ST.Survey_Key
	JOIN		Session			S	ON	S.Session_ID	= 	ST.Entered_Session_ID
	JOIN		Name			N	ON	N.Name_Key		=	SU.Run_By
	LEFT JOIN	Individual		I	ON	I.Name_Key		=	N.Name_Key
									AND	N.Organisation	=	0
	LEFT JOIN	Organisation 	O 	ON	O.Name_Key		=	N.Name_Key
									AND	N.Organisation	=	1
	WHERE		ST.Concept_Key		=	@Key

	/* Return list sorted as requested */
	SELECT	Survey_Tag_Key,
			Concept_Key,
			Survey_Key,
			Item_Name AS SurveyName,
			FullName,
			Item_Name + ' - ' + FullName AS ItemName,
			Entered_By,
			Custodian
	FROM	@Results
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTags_Select]
GO

/*===========================================================================*\
  Description:	Retrieve the survey tags for display in observation tree

  Parameters:

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTags_Select]
AS
	SELECT DISTINCT
				ST.Concept_Key, 
				CT.Sort_Code,
				CT.PlainText
	FROM		Survey_Tag ST
	JOIN		VW_ConceptTerm CT ON CT.Concept_Key = ST.Concept_Key
	ORDER BY 	CT.Sort_Code, CT.PlainText
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTags_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select_ForSurvey') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTags_Select_ForSurvey]
GO

/*===========================================================================*\
  Description:	Retrieve the tags for a survey

  Parameters:
	@Key		Survey Key

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTags_Select_ForSurvey]
	@Key CHAR(16)
AS
	SELECT 	ST.Survey_Tag_Key,
			CT.Concept_Key, 
			CT.Plaintext,
			S.User_Name_Key AS Entered_By, 
			ST.Custodian
	FROM 	Survey_Tag			ST
	JOIN 	VW_ConceptTerm 		CT 	ON 	CT.Concept_Key	=	ST.Concept_Key
	JOIN 	Session 			S 	ON 	S.Session_ID	=	ST.Entered_Session_ID
	WHERE 	ST.Survey_Key 		= 	@Key
	AND		CT.List_Preferred 	= 	1
	ORDER BY CT.Sort_Code, CT.Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTags_Select_ForSurvey') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTags_Select_ForSurvey'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTags_Select_ForSurvey TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Delete') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTag_Delete]
GO

/*===========================================================================*\
  Description:	Delete a tag for a survey

  Parameters:
	@Key		Survey Tag Key

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTag_Delete]
	@Key CHAR(16)
AS
	SET NOCOUNT OFF
	DELETE FROM Survey_Tag WHERE Survey_Tag_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Delete') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTag_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Delete_ForConcept') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTag_Delete_ForConcept]
GO

/*===========================================================================*\
  Description:	Remove a given tag from all its associated surveys.

  Parameters:
	@ConceptKey

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTag_Delete_ForConcept]
	@ConceptKey CHAR(16)
AS
	SET NOCOUNT OFF

	DELETE FROM Survey_Tag
	WHERE  Concept_Key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Delete_ForConcept') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTag_Delete_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForConcept TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Delete_ForSurvey') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTag_Delete_ForSurvey]
GO

/*===========================================================================*\
  Description:	Remove a survey from all its associated tags.

  Parameters:
	@SurveyKey

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTag_Delete_ForSurvey]
	@SurveyKey CHAR(16)
AS
	SET NOCOUNT OFF

	DELETE FROM Survey_Tag
	WHERE  Survey_Key = @SurveyKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Delete_ForSurvey') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTag_Delete_ForSurvey'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForSurvey TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForSurvey TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForSurvey TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Delete_ForSurvey TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SurveyTag_Insert]
GO

/*===========================================================================*\
  Description:	Insert a tag for a survey

  Parameters:
	@Key		Survey Tag Key OUTPUT
	@SurveyKey
	@ConceptKey
	@SessionID

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTag_Insert]
	@Key 		CHAR(16) OUTPUT,
	@SurveyKey 	CHAR(16),
	@ConceptKey CHAR(16),
	@SessionID 	CHAR(16)
AS
	SET NOCOUNT OFF
	
	EXEC spNextKey 'Survey_Tag', @Key OUTPUT

	INSERT INTO Survey_Tag (Survey_Tag_Key, Survey_Key, Concept_Key, Entered_Session_ID)
	VALUES (@Key, @SurveyKey, @ConceptKey, @SessionID)

	IF @@Error <> 0
		RAISERROR ('usp_SurveyTag_Insert failed', 16, 1)
	
	RETURN 0
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Insert') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTag_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Select') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTag_Select]
GO

/*===========================================================================*\
  Description:	Retrieve the details of a survey tag

  Parameters:
	@Key		Survey Tag Key

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTag_Select]
	@Key CHAR(16)
AS
	SELECT 	ST.Concept_Key, 
			CT.Plaintext,
			ST.Custodian
	FROM 	Survey_Tag			ST
	JOIN 	VW_ConceptTerm 		CT 	ON 	CT.Concept_Key	=	ST.Concept_Key
--	JOIN	Thesaurus_Fact		TF	ON	TF.
	JOIN 	Session 			S 	ON 	S.Session_ID	=	ST.Entered_Session_ID
	WHERE 	ST.Concept_Key			= 	@Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Select') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTag_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Update') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTag_Update]
GO

/*===========================================================================*\
  Description:	Update a tag for a survey

  Parameters:
	@Key		Survey Tag
	@ConceptKey
	@SessionID

  Created:	December 2005

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyTag_Update]
	@Key 		CHAR(16),
	@SurveyKey	CHAR(16),
	@ConceptKey CHAR(16),
	@SessionID 	CHAR(16)
AS
	SET NOCOUNT OFF

	UPDATE 	Survey_Tag
	SET 	Survey_Key			=	@SurveyKey,
			Concept_Key			=	@ConceptKey, 
			Changed_Session_ID	=	@SessionID
	WHERE 	Survey_Tag_Key		=	@Key

	IF @@Error <> 0
		RAISERROR ('usp_SurveyTag_Update failed', 16, 1)
	
	RETURN 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Update') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_SurveyTag_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_SurveyTag_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SurveyTag_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Survey_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Survey_Select]
GO

/*===========================================================================*\
  Description:	Returns a survey record

  Parameters:	@Key - survey key

  Created:	April 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Survey_Select]
	@Key CHAR(16)
AS
	SELECT 		SU.Item_Name, 
				From_Vague_Date_Start,
				From_Vague_Date_End,
				From_Vague_Date_Type,
				To_Vague_Date_Start,
				To_Vague_Date_End,
				To_Vague_Date_Type,
				SW_Spatial_Ref,
				NE_Spatial_Ref, 
				Spatial_Ref_System,
				SW_Lat,
				SW_Long,
				NE_Lat,
				NE_Long,
				SU.Item_Name + ' - ' + 
				CASE N.Organisation
					WHEN 0 THEN CASE WHEN I.Forename IS NULL THEN I.Surname ELSE I.Forename + ' ' + I.Surname END
					WHEN 1 THEN O.Full_Name
					ELSE ''
				END AS Display_Name
	FROM 		Survey			SU
	JOIN		Name			N	ON	N.Name_Key		=	SU.Run_By
	LEFT JOIN	Individual		I	ON	I.Name_Key		=	N.Name_Key
									AND	N.Organisation	=	0
	LEFT JOIN	Organisation 	O 	ON	O.Name_Key		=	N.Name_Key
									AND	N.Organisation	=	1
	WHERE 		Survey_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Survey_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Survey_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Survey_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Survey_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Survey_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Survey_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_Survey_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Survey_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_PreferredType_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_TaxonDetermination_PreferredType_Get
GO

/*===========================================================================*\
  Description:	Returns the verified value of the given determination type.

  Parameters:	@DetTypeKey

  Created:	January 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_TaxonDetermination_PreferredType_Get
	@Key char(16),
	@Output char(16) output
AS
	SELECT @Output = Determination_Type_Key
	FROM Taxon_Determination
	WHERE Taxon_Occurrence_Key = @Key AND Preferred = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_PreferredType_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_PreferredType_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_PreferredType_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Select_ForSurveyTag') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Select_ForSurveyTag]
GO

/*===========================================================================*\
  Description:	Returns the first fact for a given concept used as a survey tag.

  Parameters:	@Concept	Concept Key

  Created:	February 2008

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/08 15:24 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Select_ForSurveyTag]
	@ConceptKey char(16)
AS
	SET NOCOUNT ON

	SELECT 	TOP 1
			Thesaurus_Fact_Key,
			Item_Name,
			Data,
			Language_Key,
			Fact_Vague_Date_Start,
			Fact_Vague_Date_End,
			Fact_Vague_Date_Type,
			[Timestamp]
	FROM	Thesaurus_Fact
	WHERE 	Concept_Key 			= @ConceptKey
	AND		Fact_Type_Concept_Key 	= 'SYSTEM00000002NQ'

	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Select_ForSurveyTag') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ThesaurusFact_Select_ForSurveyTag'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Select_ForSurveyTag TO [Dev - JNCC SQL]
END
GO

