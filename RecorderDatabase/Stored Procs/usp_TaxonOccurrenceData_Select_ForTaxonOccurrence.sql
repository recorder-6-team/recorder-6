/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF OBJECT_ID('dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence
GO

/*===========================================================================*\
  Description:	
	Returns all measuerement data for a taxon occurrence.

  Parameters:
	@OccurrenceKey

  Created:	December 2008

  Last revision information:
    $Revision: 2 $
    $Date: 14/01/09 17:40 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence
	@OccurrenceKey	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT	DT.Taxon_Occurrence_Data_Key 	AS Data_Key,
			DT.Measurement_Qualifier_Key, 
			MQ.Short_Name 					AS Qualifier_Short_Name,
			DT.Measurement_Unit_Key,  
			MU.Short_Name 					AS Unit_Short_Name,
			MT.Measurement_Type_Key, 
			MT.Short_Name 					AS Type_Short_Name, 
			DT.Data,
			DT.Accuracy,
			DT.Custodian
	FROM 	Measurement_Type 		MT
	JOIN 	Measurement_Qualifier 	MQ	ON 	MT.Measurement_Type_Key = MQ.Measurement_Type_Key
	JOIN 	Measurement_Unit 		MU	ON 	MT.Measurement_Type_Key = MU.Measurement_Type_Key
	JOIN 	Taxon_Occurrence_Data	DT	ON 	MU.Measurement_Unit_Key = DT.Measurement_Unit_Key
										AND MQ.Measurement_Qualifier_Key = DT.Measurement_Qualifier_Key
	WHERE	DT.Taxon_Occurrence_Key = @OccurrenceKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence TO R2k_RecordCardsOnly
GO