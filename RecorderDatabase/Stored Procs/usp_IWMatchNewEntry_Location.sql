/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Location') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_Location]
GO

/*===========================================================================*\
  Description:	Create a new location from an import value.

  Parameters:	
	@ImportValue	The name of the location.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/07/04 14:37 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_Location]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS

	DECLARE	@Key char(16),
		@LNKey char(16)

	/*===================================================*\
	  Now create new Location and Location Name records
	\*===================================================*/
	BEGIN TRANSACTION
		EXECUTE spNextKey 'Location', @Key OUTPUT

		INSERT INTO Location (
			Location_Key, Location_Type_Key, Entered_By,
			Spatial_Ref, Spatial_Ref_System, Lat, Long, Spatial_Ref_Qualifier
		)
		SELECT	@Key, 'NBNSYS0000000001', @EnteredBy,
			Spatial_Ref, Spatial_Ref_System, Lat, Long, Spatial_Ref_Qualifier			
		FROM	#Locations
		WHERE	Import_Value = @ImportValue

		IF @@Error <> 0 GOTO RollbackAndExit

		EXECUTE spNextKey 'Location_Name', @LNKey OUTPUT
		INSERT INTO Location_Name (
			Location_Name_Key, Item_Name, Location_Key, Preferred, Entered_By
		) VALUES (
			@LNKey, @ImportValue, @Key, 1, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#Locations
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Location') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_Location'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Location TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Location TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Location TO [Dev - JNCC SQL]
END
GO