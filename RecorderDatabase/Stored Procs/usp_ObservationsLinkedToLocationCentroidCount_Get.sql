/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ObservationsLinkedToLocationCentroidCount_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ObservationsLinkedToLocationCentroidCount_Get]
GO

/*===========================================================================*\
  Description:	Returns the number of survey event or sample records that are
     linked to a location and a specific spatial reference (its centroid).

  Parameters:	@Key	Location_Key
		@Spatial_Ref

  Created:	Feb 2004

  Last revision information:
    $Revision: 1 $
    $Date: 11/02/04 15:48 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ObservationsLinkedToLocationCentroidCount_Get]
	@Key CHAR(16),
	@Spatial_Ref VARCHAR(20),
	@Count INT OUTPUT
AS

DECLARE @Custodian CHAR(8)

SELECT @Custodian = Data FROM Setting WHERE [Name]='SiteID'

SELECT @Count = COUNT(*) 
FROM [Sample] S
WHERE S.Location_Key=@Key
AND S.Spatial_Ref=@Spatial_Ref
AND S.Custodian=@Custodian

SELECT @Count = @Count + COUNT(*) 
FROM Survey_Event SE
WHERE SE.Location_Key=@Key
AND SE.Spatial_Ref=@Spatial_Ref
AND SE.Custodian=@Custodian

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ObservationsLinkedToLocationCentroidCount_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ObservationsLinkedToLocationCentroidCount_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroidCount_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroidCount_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroidCount_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroidCount_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroidCount_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ObservationsLinkedToLocationCentroidCount_Get TO [Dev - JNCC SQL]
END

GO