/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_SampleRecoders_Select_ForSample') IS NOT NULL
	DROP PROCEDURE dbo.usp_SampleRecoders_Select_ForSample
GO

/*===========================================================================*\
  Description:
	Returns the recorders for the given sample

  Parameters:
	@SampleKey  The Sample Key  

  Created:	December 2008

  Last revision information:
    $Revision: 2 $
    $Date: 15/01/09 8:50 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_SampleRecoders_Select_ForSample 	
	@SampleKey CHAR(16)
AS
	SELECT		SER.SE_Recorder_Key 					AS "Key",
				SER.Name_Key,
				dbo.ufn_GetFormattedName(SER.Name_Key) 	AS "Name"
	FROM		Sample_Recorder			SR
	JOIN 		Survey_Event_Recorder	SER	ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
	WHERE 		SR.Sample_Key 			= 	@SampleKey
	ORDER BY	"Name"
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_SampleRecoders_Select_ForSample'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_SampleRecoders_Select_ForSample TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SampleRecoders_Select_ForSample TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SampleRecoders_Select_ForSample TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SampleRecoders_Select_ForSample TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SampleRecoders_Select_ForSample TO R2k_RecordCardsOnly
GO
