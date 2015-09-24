/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_Base36Sum]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_Base36Sum
GO

/*===========================================================================*\
  Description:	Base 36 representation of the sum of two integers, one of
				which is given in its base 36 representation.

				'#ERROR!' if @operand_1 is not a valid representation of a
				non-negative integer in base 36, or if the sum would be
				negative.

  Parameters:   @operand_1				First operand (base 36 representation)
				@operand_2				Second operand

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 16/12/05 9:06 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_Base36Sum(
	@operand_1		VARCHAR(16),
	@operand_2		INT)
RETURNS
	VARCHAR(16)
AS
BEGIN
	DECLARE		@sum				VARCHAR(16),
				@char_index			INT,
				@operand_1_char		CHAR,
				@temp				INT

	SELECT		@sum		=	'',
				@char_index	=	LEN(@operand_1),
				@operand_1	=	UPPER(@operand_1),
				@temp		=	@operand_2

	WHILE		@char_index > 0
	BEGIN
		SET			@operand_1_char	=	SUBSTRING(@operand_1, @char_index, 1)

		IF @operand_1_char >= '0' AND @operand_1_char <= '9'
		BEGIN
			SET			@temp	=	@temp + ASCII(@operand_1_char) - ASCII('0')
		END
		ELSE IF @operand_1_char >= 'A' AND @operand_1_char <= 'Z'
		BEGIN
			SET			@temp	=	@temp + ASCII(@operand_1_char) - ASCII('A') + 10
		END
		ELSE
		BEGIN
			RETURN		'#ERROR!'
		END

		SET			@sum	=	CASE WHEN ((36 + (@temp % 36)) % 36) < 10
									THEN CHAR(ASCII('0') + ((36 + (@temp % 36)) % 36))
									ELSE CHAR(ASCII('A') + ((36 + (@temp % 36)) % 36) - 10)
								END
								+ @sum

		SET			@temp		=	@temp / 36
									- CASE WHEN (@temp % 36) < 0 THEN 1 ELSE 0 END
		SET			@char_index	=	@char_index - 1
	END

	IF @temp < 0 OR (@temp > 0 AND LEN(@sum) = 16)
	BEGIN
		RETURN		'#ERROR!'
	END

	WHILE @temp > 0
	BEGIN
		SET			@sum	=	CASE WHEN (@temp % 36) < 10
									THEN CHAR(ASCII('0') + (@temp % 36))
									ELSE CHAR(ASCII('A') + (@temp % 36) - 10)
								END
								+ @sum

		SET			@temp	=	@temp / 36
	END

	SET			@sum	=	REPLACE(LTRIM(REPLACE(@sum, '0', ' ')), ' ', '0')
	RETURN		CASE WHEN @sum = '' THEN '0' ELSE @sum END
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_Base36Sum]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_Base36Sum'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_Base36Sum TO [Dev - JNCC SQL]
	END
GO




/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_ConceptItemName_Get]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_ConceptItemName_Get
GO

/*===========================================================================*\
  Description:	Returns the actual name of a concept.  The common name is 
				appended if required

  Parameters:	@Key 		Concept_Key
				@ItemName	Output - 303 characters = 150 + 150 for common + 3 for 
								space and 2 brackets
				@IncludeCommonName (optional)
				@IncludeAuthor (optional)
				@Formatted (optional)

  Created:	09 Jan 2004

  Last revision information:
    $Revision: 2 $
    $Date: 16/12/05 9:06 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_ConceptItemName_Get]
(
@Key CHAR(16),
@IncludeCommonName BIT = 0,
@IncludeAuthor BIT = 0,
@Formatted BIT = 0
)
RETURNS varchar(303)

AS
BEGIN
DECLARE @ReturnValue VARCHAR(303)

SELECT @ReturnValue=
	CASE @Formatted WHEN 1 THEN Item_Name ELSE Plaintext END +
	CASE WHEN @IncludeAuthor=1 AND Author_Copy IS NOT NULL THEN ' ' + Author_Copy ELSE '' END
FROM VW_ConceptTerm 
WHERE Concept_Key=@Key

IF @IncludeCommonName=1
BEGIN
  DECLARE @CommonName VARCHAR(150)
	SELECT Top 1 @CommonName=T.Item_Name
	FROM Concept C1
	INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
	    AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
	INNER JOIN Term T on T.Term_Key=C2.Term_Key
	INNER JOIN Language L on L.Language_Key=T.Language_Key
	    AND L.Priority=1
	WHERE C1.Concept_Key=@Key
  IF @CommonName IS NOT NULL 
	  SET @ReturnValue = @CommonName + ' (' + @ReturnValue + ')'
END

RETURN @ReturnValue

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_ConceptItemName_Get]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_ConceptItemName_Get'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_ConceptItemName_Get TO [Dev - JNCC SQL]
	END
GO

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
	$Revision: 2 $
	$Date: 16/12/05 9:06 $
	$Author: Johnvanbreda $

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
    $Date: 16/12/05 9:06 $
    $Author: Johnvanbreda $

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

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_LineageSequenceNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_LineageSequenceNumber
GO

/*===========================================================================*\
  Description:	Sequence number from the last level of the given lineage.

  Parameters:   @lineage				Lineage

  Created:		Jan 2004

  Last revision information:
	$Revision: 2 $
	$Date: 16/12/05 9:06 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_LineageSequenceNumber(
	@lineage			VARCHAR(900))
RETURNS
	VARCHAR(8)
AS
BEGIN
	DECLARE     @len		INT,
				@result		VARCHAR(8)

	SET			@len		=	CHARINDEX('\', REVERSE(@lineage)) - 1

	IF @len < 0
	BEGIN
		SET			@len		=	LEN(@lineage)
	END

	RETURN		RIGHT(@lineage, @len)
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_LineageSequenceNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_LineageSequenceNumber'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [Dev - JNCC SQL]
	END
GO

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
	$Revision: 2 $
	$Date: 16/12/05 9:06 $
	$Author: Johnvanbreda $

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

