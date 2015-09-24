IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrenceData_Select_DataForMeasurement') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonOccurrenceData_Select_DataForMeasurement]
GO

/*===========================================================================*\
  Description:	Returns the data for the given taxon occurrence and measurement
	unit and type.

  Parameters:
	@Key		Taxon occurrence key
	@Unit		Unit short name
	@Type		Type short name

  Created:	May 2008

  Last revision information:
    $Revision: 1 $
    $Date: 26/05/08 11:38 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_TaxonOccurrenceData_Select_DataForMeasurement] 
	@Key	CHAR(16),
	@Unit	VARCHAR(40),
	@Type	VARCHAR(20)
AS
	SET NOCOUNT ON

	SELECT 	TOD.Taxon_Occurrence_Data_Key, 
			TOD.Data
	FROM	Taxon_Occurrence_Data 		TOD 
	JOIN 	Measurement_Unit 			MU 	ON TOD.Measurement_Unit_Key = MU.Measurement_Unit_Key
	JOIN 	Measurement_Type 			MT 	ON MU.Measurement_Type_Key 	= MT.Measurement_Type_Key
	WHERE 	MU.Short_Name 				= 	@Unit
	AND 	MT.Short_Name 				= 	@Type
	AND		TOD.Taxon_Occurrence_Key	= 	@Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrenceData_Select_DataForMeasurement') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_TaxonOccurrenceData_Select_DataForMeasurement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_DataForMeasurement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_DataForMeasurement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_DataForMeasurement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_DataForMeasurement TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_DataForMeasurement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_DataForMeasurement TO [Dev - JNCC SQL]
END
GO

