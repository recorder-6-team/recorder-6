/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_MeasurementColumns_Get') IS NOT NULL
	DROP PROCEDURE dbo.usp_MeasurementColumns_Get
GO

/*===========================================================================*\
  Description:
	Returns measurement column names for all possible combinations.

  Parameters:
	@TranslatedOf	The localized version of "of" used for the displayed string.

  Created:	December 2008

  Last revision information:
    $Revision: 2 $
    $Date: 15/01/09 8:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_MeasurementColumns_Get 	
	@TranslatedOf VARCHAR(10)
AS
	SELECT 	MQ.Measurement_Qualifier_Key, 
			MU.Measurement_Unit_Key, 
			MU.Data_Type,
		    MT.Short_Name + ' ' + @TranslatedOf + ' ' + MQ.Short_Name + ' (' + MU.Short_Name + ')' AS ColumnName,
			MT.Short_Name + ' of ' + MQ.Short_Name + ' (' + MU.Short_Name + ')' AS OriginalColumnName
	FROM 	Measurement_Context 		MC
	JOIN 	Measurement_Type_Context 	MTC	ON MTC.Measurement_Context_Key 	= MC.Measurement_Context_Key
	JOIN 	Measurement_Type 			MT	ON MT.Measurement_Type_Key 		= MTC.Measurement_Type_Key
	JOIN 	Measurement_Qualifier 		MQ	ON MQ.Measurement_Type_Key 		= MT.Measurement_Type_Key
	JOIN 	Measurement_Unit 			MU	ON MU.Measurement_Type_Key 		= MT.Measurement_Type_Key 
	WHERE 	MC.Data_Table 					= 'Taxon_Occurrence_Data'
	ORDER BY ColumnName
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_MeasurementColumns_Get'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_MeasurementColumns_Get TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_MeasurementColumns_Get TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_MeasurementColumns_Get TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_MeasurementColumns_Get TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_MeasurementColumns_Get TO R2k_RecordCardsOnly
GO
