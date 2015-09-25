/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_SampleDetails_Get') IS NOT NULL
	DROP PROCEDURE dbo.usp_SampleDetails_Get
GO

/*===========================================================================*\
  Description:
	This procedure returns the whole row from SAMPLE

  Parameters:
	@SampleKey  Parameter holding the Sample Key  

  Created:	February 2008

  Last revision information:
    $Revision: 7 $
    $Date: 14/01/09 17:40 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_SampleDetails_Get 	
	@SampleKey CHAR(16)
AS
	SELECT		S.Sample_Key, 
				S.Location_Key,
				LN.Item_Name 	AS Location, 
				S.Location_Name, 
				S.Vague_Date_Start, 
				S.Vague_Date_End, 
				S.Vague_Date_Type, 
				S.Spatial_Ref, 
				S.Spatial_Ref_System,
				S.Spatial_Ref_Qualifier,
				S.Lat,
				S.Long,
				ST.Short_Name 	AS Sample_Type, 
				ST.Sample_Type_Key,
				S.Sample_Reference,
				S.Comment,
				S.Custodian
	FROM		Sample 			S
	LEFT JOIN 	Location_Name	LN	ON 	LN.Location_Key 	= S.Location_Key
									AND	LN.Preferred		= 1
	INNER JOIN 	Sample_Type 	ST	ON	ST.Sample_Type_Key 	= S.Sample_Type_Key	
	WHERE 		S.Sample_Key 		= 	@SampleKey
	ORDER BY 	Sample_Reference
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_SampleDetails_Get'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_SampleDetails_Get TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SampleDetails_Get TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SampleDetails_Get TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SampleDetails_Get TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SampleDetails_Get TO R2k_RecordCardsOnly
GO
