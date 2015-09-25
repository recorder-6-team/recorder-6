/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationLocationName') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_ImportWizard_GetLocationLocationName
GO

/*===========================================================================*\
  Description:  Obtain the location name from an import record.

  Parameters:   @record_no       Identifies the import record.
                @location_name   [on exit] Location name.

  Created:      June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 18/02/08 10:57 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetLocationLocationName]
    @record_no      INT,
    @location_name  VARCHAR(100) OUTPUT
AS
    SELECT      @location_name      =   SYSTEM010000000T_data
    FROM        #master
    WHERE       Record_No           =   @record_no    

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationLocationName') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_GetLocationLocationName'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationLocationName TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationLocationName TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationLocationName TO [Dev - JNCC SQL]
END
GO