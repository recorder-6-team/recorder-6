/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_NextChildLineage]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_NextChildLineage
GO

/*===========================================================================*\
  Description:	Next available lineage for a child of the given lineage in
  				the specified concept group.

  Parameters:   @parent_lineage			Lineage of parent
				@concept_group_key		Concept group key

  Created:		Jan 2004

  Last revision information:
	$Revision: 5 $
	$Date: 6/05/04 11:02 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_NextChildLineage(
	@parent_lineage		VARCHAR(900),
	@concept_group_key	CHAR(16))
RETURNS
	VARCHAR(900)
AS
BEGIN
	DECLARE     @lineage_pattern	VARCHAR(900),
				@sequence			VARCHAR(8)

	IF LEN(@parent_lineage) > 0
	BEGIN
		SELECT		@sequence				=	l.Last_Sequence_Number
		FROM		Concept_Lineage			AS	l
		INNER JOIN	Concept					AS	c
		ON			c.Concept_Key			=	l.Concept_Key
		WHERE		l.Lineage				=	@parent_lineage
		AND			c.Concept_Group_Key		=	@concept_group_key
	END
	ELSE
	BEGIN
		SELECT		@sequence				=	Last_Sequence_Number
		FROM		Concept_Group
		WHERE		Concept_Group_Key		=	@concept_group_key
	END

	SET			@sequence	=	CASE WHEN @sequence IS NULL
									THEN '0'
									ELSE dbo.ufn_Base36Sum(@sequence, 1)
								END

	RETURN		ISNULL(@parent_lineage, '')
				+ CASE WHEN LEN(@parent_lineage) > 0 THEN '\' ELSE '' END
				+ @sequence
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_NextChildLineage]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_NextChildLineage'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_NextChildLineage TO [Dev - JNCC SQL]
	END
GO
