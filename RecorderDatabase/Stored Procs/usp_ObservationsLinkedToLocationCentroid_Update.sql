/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ObservationsLinkedToLocationCentroid_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ObservationsLinkedToLocationCentroid_Update]
GO

/*===========================================================================*\
  Description:	Updates the centroid of survey event or sample records that are
     linked to a location and a specific spatial reference (its centroid).

  Parameters:	@Key	Location_Key
		@Old_Spatial_ref				- old reference to match against
		@Spatial_Ref 						- new reference details
		@Spatial_Ref_System 
		@Spatial_Ref_Qualifier
		@Lat
		@Long

  Created:	Feb 2004

  Last revision information:
    $Revision: 1 $
    $Date: 11/02/04 15:48 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ObservationsLinkedToLocationCentroid_Update]
	@Key CHAR(16),
	@Old_Spatial_Ref VARCHAR(20),
	@Spatial_Ref VARCHAR(20),
	@Spatial_Ref_Qualifier VARCHAR(20),
	@Spatial_Ref_System VARCHAR(4),
	@Lat FLOAT,
	@Long FLOAT
AS

DECLARE @Custodian CHAR(8)

SELECT @Custodian = Data FROM Setting WHERE [Name]='SiteID'

UPDATE [Sample] 
SET 
	Spatial_Ref=@Spatial_Ref,
	Spatial_Ref_Qualifier = @Spatial_Ref_Qualifier,
	Spatial_Ref_System = @Spatial_Ref_System,
	Lat = @Lat,
	Long = @Long
WHERE Location_Key=@Key
AND Spatial_Ref=@Old_Spatial_Ref
AND Custodian=@Custodian


UPDATE Survey_Event 
SET 
	Spatial_Ref=@Spatial_Ref,
	Spatial_Ref_Qualifier = @Spatial_Ref_Qualifier,
	Spatial_Ref_System = @Spatial_Ref_System,
	Lat = @Lat,
	Long = @Long
WHERE Location_Key=@Key
AND Spatial_Ref=@Old_Spatial_Ref
AND Custodian=@Custodian

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ObservationsLinkedToLocationCentroid_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ObservationsLinkedToLocationCentroid_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroid_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroid_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroid_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroid_Update TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroid_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroid_Update TO [Dev - JNCC SQL]
END

GO