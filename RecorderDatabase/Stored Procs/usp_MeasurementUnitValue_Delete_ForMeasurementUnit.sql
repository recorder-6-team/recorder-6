/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementUnitValue_Delete_ForMeasurementUnit') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_MeasurementUnitValue_Delete_ForMeasurementUnit]
GO

/*===========================================================================*\
  Description:	Remove values linked to a measurement unit.

  Parameters:
	@Key

  Created:	March 2008

  Last revision information:
    $Revision: 1 $
    $Date: 18/03/08 16:04 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeasurementUnitValue_Delete_ForMeasurementUnit]
	@Key CHAR(16)
AS
	SET NOCOUNT OFF

	DELETE FROM Measurement_Unit_Value
	WHERE  Measurement_Unit_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementUnitValue_Delete_ForMeasurementUnit') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_MeasurementUnitValue_Delete_ForMeasurementUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Delete_ForMeasurementUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Delete_ForMeasurementUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Delete_ForMeasurementUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Delete_ForMeasurementUnit TO [Dev - JNCC SQL]
END
GO
