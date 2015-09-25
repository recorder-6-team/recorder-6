SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationName') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_ImportWizard_GetLocationName]
GO

/*===========================================================================*\
  Description:  Obtain the location name from an import record.

  Parameters:   @record_no          Identifies the import record.
                @lock_key			Location key if the record has a location
                @location_name      [on exit] Location name.

  Created:      June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/05/08 14:10 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetLocationName]
    @record_no      INT,
    @loc_key		CHAR(16) = null,
    @location_name  VARCHAR(100) OUTPUT
AS
	SELECT      @location_name      =   SYSTEM0100000000_data
	FROM        #master
	WHERE       Record_No           =   @record_no
	IF @@ROWCOUNT = 0 GOTO NoSuchRecord

	IF @loc_key IS NOT NULL
	BEGIN
		DECLARE @properlocname VARCHAR(100)
		EXEC usp_LocationName_Get @loc_key, @properlocname OUTPUT --is missing 'output' which will always return null value.

		--If location name already identified through main location record, no need to 
		--use the vague location name field
		IF @properlocname = @location_name 
			SET @location_name = NULL
	END
        	
	RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationName') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_GetLocationName'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationName TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationName TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationName TO [Dev - JNCC SQL]
END

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
    $Date: 26/05/08 14:10 $
    $Author: Johnvanbreda $

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


IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrence_Update_ZeroAbundanceForSample') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonOccurrence_Update_ZeroAbundanceForSample]
GO

/*===========================================================================*\
  Description:	Updates the Zero Abundance flag in taxon occurrence of sample.

  Parameters:
	@SampleKey	Sample key
	@Unit		Unit short name
	@Type		Type short name

  Created:	May 2008

  Last revision information:
    $Revision: 1 $
    $Date: 26/05/08 14:10 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_TaxonOccurrence_Update_ZeroAbundanceForSample] 
	@SampleKey	CHAR(16),
	@Unit		VARCHAR(40),
	@Type		VARCHAR(20)
AS
	SET NOCOUNT ON

	UPDATE 		Taxon_Occurrence
	SET 		Zero_Abundance = CASE WHEN (TOD1.Data IS NOT NULL AND TOD2.Data IS NULL) THEN 1 ELSE 0 END
	FROM		Taxon_Occurrence 		OCC
	LEFT JOIN 	Taxon_Occurrence_Data 	TOD1	ON 	TOD1.Taxon_Occurrence_Key 	= 	OCC.Taxon_Occurrence_Key 
												AND TOD1.Data					=	'0'
	LEFT JOIN 	Measurement_Unit 		MU1 	ON 	TOD1.Measurement_Unit_Key 	= 	MU1.Measurement_Unit_Key 
												AND MU1.Short_Name 				= 	@Unit
	LEFT JOIN 	Measurement_Type 		MT1		ON 	MU1.Measurement_Type_Key 	= 	MT1.Measurement_Type_Key 
												AND MT1.Short_Name 				= 	@Type
	LEFT JOIN 	Taxon_Occurrence_Data 	TOD2 	ON 	TOD2.Taxon_Occurrence_Key 	= 	OCC.Taxon_Occurrence_Key 
												AND TOD2.Data					<>	'0'
	LEFT JOIN 	Measurement_Unit 		MU2 	ON 	TOD2.Measurement_Unit_Key 	= 	MU2.Measurement_Unit_Key 
												AND MU2.Short_Name 				= 	@Unit
	LEFT JOIN 	Measurement_Type 		MT2 	ON 	MU2.Measurement_Type_Key 	= 	MT2.Measurement_Type_Key 
												AND MT2.Short_Name 				= 	@Type
	WHERE 		OCC.Sample_Key = @SampleKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrence_Update_ZeroAbundanceForSample') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_TaxonOccurrence_Update_ZeroAbundanceForSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ZeroAbundanceForSample TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ZeroAbundanceForSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ZeroAbundanceForSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ZeroAbundanceForSample TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ZeroAbundanceForSample TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ZeroAbundanceForSample TO [Dev - JNCC SQL]
END
GO

