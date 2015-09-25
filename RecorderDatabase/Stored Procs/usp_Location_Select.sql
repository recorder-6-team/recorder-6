/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Location_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Location_Select]
GO

/*===========================================================================*\
  Description:	Select a single location record

  Parameters:	@Key

  Created:	April 2004

  Last revision information:
    $Revision: 4 $
    $Date: 16/05/08 13:59 $
    $Author: Qingsun $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Location_Select]
	@Key char(16)
AS
SET NOCOUNT ON
	SELECT
		LN.Item_Name,
		L.Spatial_Ref,
		L.Spatial_Ref_System,
		L.Lat,
		L.Long,
		L.Spatial_Ref_Qualifier,
		L.Parent_Key
	FROM Location L
	INNER JOIN Location_Name LN ON LN.Location_Key=L.Location_Key
		AND LN.Preferred=1
	WHERE L.Location_Key=@Key
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Location_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Location_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Location_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Location_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Location_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Location_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Location_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Location_Select TO [Dev - JNCC SQL]
END
GO