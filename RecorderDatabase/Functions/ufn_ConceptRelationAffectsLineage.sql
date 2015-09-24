/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_ConceptRelationAffectsLineage]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_ConceptRelationAffectsLineage
GO

/*===========================================================================*\
  Description:	Does the specified relation affect concept lineage?

  Parameters:   @from_concept_key		Source concept key
				@to_concept_key			Destination concept key
				@relation_type_key		Relation type key

  Created:		Jan 2004

  Last revision information:
	$Revision: 3 $
	$Date: 6/05/04 11:01 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_ConceptRelationAffectsLineage(
	@from_concept_key		CHAR(16),
	@to_concept_key			CHAR(16),
	@relation_type_key		CHAR(16))
RETURNS
	BIT
AS
BEGIN
	DECLARE		@from_group						CHAR(16),
				@from_preferred					BIT,
				@to_group						CHAR(16),
				@to_preferred					BIT,
				@hierarchy_relation_type_key	CHAR(16)

	SELECT		@from_group						=	c.Concept_Group_Key,
				@from_preferred					=	c.List_Preferred,
				@hierarchy_relation_type_key	=	g.Hierarchy_Relation_Type_Key
	FROM		Concept							AS	c
	INNER JOIN	Concept_Group					AS	g
	ON			g.Concept_Group_Key				=	c.Concept_Group_Key
	WHERE		c.Concept_Key					=	@from_concept_key

	SELECT		@to_group						=	c.Concept_Group_Key,
				@to_preferred					=	c.List_Preferred
	FROM		Concept							AS	c
	WHERE		c.Concept_Key					=	@to_concept_key

	RETURN		CASE
					WHEN @from_group IS NULL OR @to_group IS NULL THEN 0
					WHEN @from_preferred <> 1 THEN 0
					WHEN @to_preferred <> 1 THEN 0
					WHEN @from_group <> @to_group THEN 0
					WHEN @relation_type_key <> @hierarchy_relation_type_key THEN 0
					ELSE 1
				END
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_ConceptRelationAffectsLineage]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_ConceptRelationAffectsLineage'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_ConceptRelationAffectsLineage TO [Dev - JNCC SQL]
	END
GO

