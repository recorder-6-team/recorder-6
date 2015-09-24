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
    $Date: 7/02/08 14:11 $
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