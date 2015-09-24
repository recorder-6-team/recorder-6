/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_Samples_Select_ForEvent') IS NOT NULL
	DROP PROCEDURE dbo.usp_Samples_Select_ForEvent
GO

/*===========================================================================*\
  Description:
	This procedure returns samples for a given event.

  Parameters:
	@EventKey  	Event containing the samples.
	@SortOrder	How to order the results.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 14/01/09 17:42 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Samples_Select_ForEvent
	@EventKey	CHAR(16),
	@SortOrder	TINYINT
AS
	SET NOCOUNT ON

	DECLARE @Samples TABLE (
		Sample_Key			CHAR(16),
		Sample_Reference	VARCHAR(15),
		Vague_Date_Start	INT,
		Vague_Date_End		INT,
		Vague_Date_Type		VARCHAR(2),
		Outstanding_Card	TINYINT,
		Short_Name			VARCHAR(20),
		Sample_Type_Key		CHAR(16),
		Location_Key		CHAR(16),
		Location_Name		VARCHAR(100)
	)

	INSERT INTO @Samples
	SELECT 		S.Sample_Key, 
				S.Sample_Reference, 
				S.Vague_Date_Start, 
				S.Vague_Date_End,
				S.Vague_Date_Type, 
				S.Outstanding_Card, 
				T.Short_Name, 
				T.Sample_Type_Key, 
				S.Location_Key,
				CASE 
					WHEN LN.Item_Name IS NULL THEN 
						CASE 
							WHEN S.Spatial_Ref IS NULL OR S.Spatial_Ref = '' THEN S.Location_Name 
							ELSE S.Spatial_Ref 
						END 
					ELSE LN.Item_Name 
				END AS Location_Name
	FROM 		Sample 			S 
	INNER JOIN 	Sample_Type 	T 	ON 	S.Sample_Type_Key 	= T.Sample_Type_Key
	LEFT JOIN 	Location_Name 	LN 	ON 	LN.Location_Key 	= S.Location_Key
	WHERE 		S.Survey_Event_Key = @EventKey 
	AND 		(LN.Preferred = 1 OR LN.Preferred IS NULL)

	IF @SortOrder = 1 
		SELECT * FROM @Samples ORDER BY Sample_Reference
	ELSE
	IF @SortOrder = 2
		SELECT * FROM @Samples ORDER BY Vague_Date_Start
	ELSE
		SELECT * FROM @Samples ORDER BY Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_Samples_Select_ForEvent TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Samples_Select_ForEvent TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Samples_Select_ForEvent TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Samples_Select_ForEvent TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Samples_Select_ForEvent TO R2k_RecordCardsOnly
GO

