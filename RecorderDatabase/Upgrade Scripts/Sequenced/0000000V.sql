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
    $Revision: 1 $
    $Date: 8/04/04 9:40 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Survey_Select]
	@Key char(16)
AS
  SELECT 
		Item_Name, 
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
		NE_Long
	FROM Survey
	WHERE Survey_Key=@Key

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