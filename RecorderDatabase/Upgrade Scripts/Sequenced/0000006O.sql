/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GridSquare_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_GridSquare_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a grid square record

  Created:	July 2006

  Last revision information:
    $Revision: 1 $
    $Date: 26/03/09 11:22 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_GridSquare_Insert]
@Key CHAR(16) OUTPUT,
@Spatial_Ref VARCHAR(20),
@Location_Key CHAR(16),
@Spatial_Ref_System VARCHAR(4),
@Spatial_Ref_Qualifier VARCHAR(20),
@Size INT,
@Lat FLOAT,
@Long FLOAT,
@Entered_By CHAR(16)
AS

	SET NOCOUNT OFF
	
	--Existing records are not duplicated
	IF EXISTS(SELECT 1 FROM Grid_Square WHERE Spatial_Ref=@Spatial_Ref AND Location_Key=@Location_Key and Spatial_Ref_System=@Spatial_Ref_System)
		SET @Key = NULL	-- New key is null if no record inserted.
	ELSE BEGIN
		-- Only need a new key if we are inserting a new record.
		EXEC spNextKey 'Grid_Square', @Key OUTPUT
		
		INSERT INTO Grid_Square (
			Grid_Square_Key, Spatial_Ref, Location_Key, Spatial_Ref_System, Spatial_Ref_Qualifier, Size, Lat, Long, Entered_By)
		VALUES (
			@Key, @Spatial_Ref, @Location_Key, @Spatial_Ref_System, @Spatial_Ref_Qualifier, @Size, @Lat, @Long, @Entered_By)
	END

	IF @@Error <> 0
		RAISERROR ('usp_GridSquare_Insert failed', 16, 1)
	
	RETURN 0


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GridSquare_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_GridSquare_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_GridSquare_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_GridSquare_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_GridSquare_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_GridSquare_Insert TO [Dev - JNCC SQL]
END
GO
