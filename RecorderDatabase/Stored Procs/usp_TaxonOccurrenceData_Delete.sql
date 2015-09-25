/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_TaxonOccurrenceData_Delete') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonOccurrenceData_Delete
GO

/*===========================================================================*\
  Description:
	Delete a record from Taxon_Occurrence_Data.

  Parameters:
	@Key 	Data record key

  Created:
	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 14/01/09 17:45 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonOccurrenceData_Delete
	@Key CHAR(16)
AS
	SET NOCOUNT OFF
	
	DELETE FROM	Taxon_Occurrence_Data
	WHERE	Taxon_Occurrence_Data_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_TaxonOccurrenceData_Delete'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Delete TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Delete TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Delete TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Delete TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Delete TO "Dev - JNCC SQL"
GO