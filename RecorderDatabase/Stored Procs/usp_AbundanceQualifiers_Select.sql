/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AbundanceQualifiers_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_AbundanceQualifiers_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of measurement qualifiers for the Abundance 
		measurement type.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/06/04 18:17 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_AbundanceQualifiers_Select]
AS
	SELECT 		Measurement_Qualifier_Key AS Item_Key, Short_Name AS Item_Name
	FROM 		Measurement_Qualifier
	WHERE 		Measurement_Type_Key = 'NBNSYS0000000004'
	ORDER BY	Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AbundanceQualifiers_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_AbundanceQualifiers_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_AbundanceQualifiers_Select TO [Dev - JNCC SQL]
END
GO