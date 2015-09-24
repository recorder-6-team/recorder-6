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
    $Revision: 5 $
    $Date: 22/05/08 14:41 $
    $Author: Qingsun $

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