/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementUnitValue_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_MeasurementUnitValue_Insert]
GO

/*===========================================================================*\
  Description:	Insert a measurement unit value

  Parameters:
	@Key		Measurement Unit Key
	@Data

  Created:	March 2008

  Last revision information:
    $Revision: 2 $
    $Date: 29/05/08 16:36 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeasurementUnitValue_Insert]
	@Key 	CHAR(16),
	@Data 	VARCHAR(20),
	@EnteredBy CHAR(16)
AS
	SET NOCOUNT OFF
	
	INSERT INTO Measurement_Unit_Value (Measurement_Unit_Key, Data, Entered_By)
	VALUES (@Key, @Data, @EnteredBy)

	IF @@Error <> 0
		RAISERROR ('usp_MeasurementUnitValue_Insert failed', 16, 1)
	
	RETURN 0
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementUnitValue_Insert') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_MeasurementUnitValue_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Insert TO [Dev - JNCC SQL]
END
GO
