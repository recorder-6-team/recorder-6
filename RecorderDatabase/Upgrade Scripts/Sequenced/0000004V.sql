SET QUOTED_IDENTIFIER ON

/*===========================================================================*\
  Increase size of DATA field to cope with Measurement_Unit_Value.
\*===========================================================================*/
ALTER TABLE Biotope_Occurrence_Data
	ALTER COLUMN Data VARCHAR(20) NOT NULL

ALTER TABLE Sample_Data
	ALTER COLUMN Data VARCHAR(20) NOT NULL

ALTER TABLE Taxon_Occurrence_Data
	ALTER COLUMN Data VARCHAR(20) NOT NULL
GO

/*===========================================================================*\
  Script for Measurement_Unit_Value table.
\*===========================================================================*/
IF NOT EXISTS (SELECT * FROM dbo.SysObjects WHERE Id = Object_Id(N'[dbo].[Measurement_Unit_Value]') AND ObjectProperty(Id, N'IsUserTable') = 1)
	CREATE TABLE dbo.Measurement_Unit_Value (
		Measurement_Unit_Key	CHAR(16)	NOT NULL,
		Data					VARCHAR(20)	NOT NULL
		CONSTRAINT	PK_Measurement_Unit_Value PRIMARY KEY CLUSTERED 
		(
			Measurement_Unit_Key,
			Data
		) ON [PRIMARY],
		CONSTRAINT	FK_Measurement_Unit_Value_Measurement_Unit FOREIGN KEY
		(
			Measurement_Unit_Key
		) REFERENCES dbo.Measurement_Unit (
			Measurement_Unit_Key
		)
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT SELECT ON [dbo].[Measurement_Unit_Value] TO [R2k_ReadOnly]
GO
GRANT SELECT ON [dbo].[Measurement_Unit_Value] TO [R2k_RecordCardsOnly]
GO
GRANT SELECT ON [dbo].[Measurement_Unit_Value] TO [R2k_AddOnly]
GO
GRANT SELECT, UPDATE, INSERT ON [dbo].[Measurement_Unit_Value] TO [R2k_FullEdit]
GO
GRANT SELECT, UPDATE, INSERT ON [dbo].[Measurement_Unit_Value] TO [R2k_Administrator]
GO


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
    $Date: 18/03/08 16:11 $
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
    $Revision: 1 $
    $Date: 18/03/08 16:11 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeasurementUnitValue_Insert]
	@Key 	CHAR(16),
	@Data 	VARCHAR(20)
AS
	SET NOCOUNT OFF
	
	INSERT INTO Measurement_Unit_Value (Measurement_Unit_Key, Data)
	VALUES (@Key, @Data)

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


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementUnitValue_Select_ForMeasurementUnit') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_MeasurementUnitValue_Select_ForMeasurementUnit]
GO

/*===========================================================================*\
  Description:		Returns all values for a measurement unit.

  Parameters:

  Created:	March 2008

  Last revision information:
    $Revision: 1 $
    $Date: 18/03/08 16:11 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeasurementUnitValue_Select_ForMeasurementUnit]
	@Key CHAR(16)
AS
	SET NOCOUNT ON

	SELECT		Data
	FROM		Measurement_Unit_Value
	WHERE		Measurement_Unit_Key	=	@Key
	ORDER BY	Data
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementUnitValue_Select_ForMeasurementUnit') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_MeasurementUnitValue_Select_ForMeasurementUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Select_ForMeasurementUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Select_ForMeasurementUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Select_ForMeasurementUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Select_ForMeasurementUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Select_ForMeasurementUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_MeasurementUnitValue_Select_ForMeasurementUnit TO [Dev - JNCC SQL]
END
GO
