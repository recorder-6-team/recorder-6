/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_GetLocationSpatialRef]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_GetLocationSpatialRef]
GO

/*===========================================================================*\
  Description:  Obtain the spatial reference details for a location.

  Parameters:   @location_key           Identifies the location.

  Created:      June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 1/07/04 9:09 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetLocationSpatialRef]
    @location_key       CHAR(16)
AS
    SELECT      SPATIAL_REF,
                SPATIAL_REF_SYSTEM,
                LAT,
                LONG,
                SPATIAL_REF_QUALIFIER
    FROM        Location
    WHERE       Location_Key        =   @location_key    

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetLocationSpatialRef') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_GetLocationSpatialRef'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationSpatialRef TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationSpatialRef TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetLocationSpatialRef TO [Dev - JNCC SQL]
END