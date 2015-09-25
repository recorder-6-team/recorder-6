/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_TaxonOccurrenceData_Insert') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonOccurrenceData_Insert
GO

/*===========================================================================*\
  Description:
	Insert a record in Taxon_Occurrence_Data.

  Parameters:
	@TaxonOccurrenceKey 
	@QualifierKey
	@Accuracy
	@UnitKey
	@Data
	@EnteredBy
	@Key

  Created:
	November 2004

  Last revision information:
    $Revision: 2 $
    $Date: 14/01/09 17:40 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonOccurrenceData_Insert
	@TaxonOccurrenceKey 	CHAR(16),
	@QualifierKey 			CHAR(16),
	@Accuracy				VARCHAR(10),
	@UnitKey				CHAR(16),
	@Data					VARCHAR(20),
	@EnteredBy				CHAR(16),
	@Key					CHAR(16) OUTPUT
AS
	SET NOCOUNT OFF
	
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Taxon_Occurrence_Data', @Key OUTPUT
	
	INSERT INTO Taxon_Occurrence_Data(
		Taxon_Occurrence_Data_Key, Taxon_Occurrence_Key, Data, Accuracy,
		Measurement_Qualifier_Key, Measurement_Unit_Key, Entered_By
	) VALUES (
		@Key, @TaxonOccurrenceKey, @Data, @Accuracy,
		@QualifierKey, @UnitKey, @EnteredBy
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_TaxonOccurrenceData_Insert'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO "Dev - JNCC SQL"
GO