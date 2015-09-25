/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_Sample_Cascade_Vague_Date_Start_FromEvent') IS NOT NULL
	DROP PROCEDURE dbo.usp_Sample_Cascade_Vague_Date_Start_FromEvent
GO

/*===========================================================================*\
  Description:
	Updates all the samples under the parameter Event

  Parameters:
	@EventKey
	@StartValue
	@PreviousStartValue
	@EndValue
	@PreviousEndValue
	@TypeValue
	@PreviousTypeValue
	@CurrentSample

  Created:	May 2008

  Last revision information:
    $Revision: 6 $
    $Date: 14/04/09 17:40 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Sample_Cascade_Vague_Date_Start_FromEvent (
	@EventKey 			CHAR(16),
	@StartValue 		INT,
	@PreviousStartValue INT,
	@EndValue 			INT,
	@PreviousEndValue 	INT,
	@TypeValue 			VARCHAR(2),
	@PreviousTypeValue 	VARCHAR(2),
	@CurrentSample 		CHAR(16)
) 
AS
	UPDATE 	Sample
	SET		Vague_Date_Start = @StartValue,
			Vague_Date_End   = @EndValue,
			Vague_Date_Type  = @TypeValue
	WHERE	Survey_Event_Key = @EventKey
	AND 	(@CurrentSample      IS NULL OR @CurrentSample      <> Sample_Key)
	AND		(@PreviousStartValue IS NULL OR @PreviousStartValue =  Vague_Date_Start)
	AND		(@PreviousEndValue   IS NULL OR @PreviousEndValue   =  Vague_Date_End)
	AND		(@PreviousTypeValue  IS NULL OR @PreviousTypeValue  =  Vague_Date_Type)

	EXEC usp_Taxon_Determination_Cascade_Vague_Date_Start_FromEvent
		@EventKey,
		@StartValue,
		@PreviousStartValue,
		@EndValue,
		@PreviousEndValue,
		@TypeValue,
		@PreviousTypeValue,
		@CurrentSample
    
	EXEC usp_Biotope_Determination_Cascade_Vague_Date_Start_FromEvent 
		@EventKey,
		@StartValue,
		@PreviousStartValue,
		@EndValue,
		@PreviousEndValue,
		@TypeValue,
		@PreviousTypeValue,
		@CurrentSample
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_Sample_Cascade_Vague_Date_Start_FromEvent'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Sample_Cascade_Vague_Date_Start_FromEvent TO [R2k_Administrator]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Sample_Cascade_Vague_Date_Start_FromEvent TO [R2k_FullEdit]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_Sample_Cascade_Vague_Date_Start_FromEvent TO [Dev - JNCC SQL]
GO
