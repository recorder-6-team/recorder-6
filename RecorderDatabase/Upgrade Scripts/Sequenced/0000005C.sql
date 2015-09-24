If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spRptMeasurementByContextName]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[spRptMeasurementByContextName]
GO

/*===========================================================================*\
  Description:

  Parameters:
	@ContextName

  Created:	
	November 2002

  Last revision information:
    $Revision: 1 $
    $Date: 26/05/08 16:02 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[spRptMeasurementByContextName]
	@ContextName varchar(20)
AS
	SET NOCOUNT ON

	SELECT 		MQ.Measurement_Qualifier_Key, 
				@ContextName + ' ' + MT.Short_Name + ' (' + MQ.Short_Name + ')' AS Description
	FROM 		Measurement_Type 			MT
	INNER JOIN 	Measurement_Qualifier 		MQ	ON MQ.Measurement_Type_Key 		= MT.Measurement_Type_Key
	INNER JOIN 	Measurement_Type_Context	MTC	ON MTC.Measurement_Type_Key		= MT.Measurement_Type_Key
	INNER JOIN	Measurement_Context 		MC	ON MC.Measurement_Context_Key 	= MTC.Measurement_Context_Key
	WHERE 		MC.Context_Name = @ContextName
	ORDER BY 	Description DESC
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.spRptMeasurementByContextName') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure spRptMeasurementByContextName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'Dev- JNCC SQL')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [Dev- JNCC SQL]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE Name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_RecordCardsOnly]
END
GO
