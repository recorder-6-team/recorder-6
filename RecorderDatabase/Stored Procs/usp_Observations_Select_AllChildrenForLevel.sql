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
    $Revision: 3 $
    $Date: 7/02/08 16:32 $
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