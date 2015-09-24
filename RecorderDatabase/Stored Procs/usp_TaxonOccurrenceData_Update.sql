/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_TaxonOccurrenceData_Update') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonOccurrenceData_Update
GO

/*===========================================================================*\
  Description:
	Update a record in Taxon_Occurrence_Data.

  Parameters:
	@Key 			Data record key
	@QualifierKey 	Measurement Qualifier key
	@Accuracy		Accuracy
	@UnitKey		Measurement unit key
	@Data			Actual data of the measurement
	@ChangedBy		Who made the change

  Created:
	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 14/01/09 17:45 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonOccurrenceData_Update
	@Key 			CHAR(16),
	@Data			VARCHAR(20),
	@Accuracy		VARCHAR(10),
	@QualifierKey 	CHAR(16),
	@UnitKey		CHAR(16),
	@ChangedBy		CHAR(16)
AS
	SET NOCOUNT OFF
	
	UPDATE 	Taxon_Occurrence_Data
	SET		Data						= @Data, 
			Accuracy					= @Accuracy,
			Measurement_Qualifier_Key	= @QualifierKey, 
			Measurement_Unit_Key		= @UnitKey, 
			Changed_By					= @ChangedBy,
			Changed_Date 				= GetDate()
	WHERE	Taxon_Occurrence_Data_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_TaxonOccurrenceData_Update'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Update TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Update TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Update TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Update TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Update TO "Dev - JNCC SQL"
GO