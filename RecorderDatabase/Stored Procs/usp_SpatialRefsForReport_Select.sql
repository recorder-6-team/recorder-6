/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpatialRefsForReport_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpatialRefsForReport_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of spatial references that apply to the 
		current report output.

  Parameters:	None

  Created:	July 2004

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/04 16:49 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpatialRefsForReport_Select]
AS

DECLARE @SQL AS NVARCHAR(1000)
DECLARE @Select AS NVARCHAR(1000)
DECLARE @Order AS NVARCHAR(100)

SET @Select = 'SELECT DISTINCT '

-- Find the occurrence key and return it if available
IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Type' AND ID=Object_Id('TempDB..#REPORT_OUTPUT')) AND
		EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Occurrence_Key' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
BEGIN
	SET @Select = @Select + 'Occurrence_Key, Type, '
	SET @Order = ' ORDER BY Type'
END

IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Lat' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
		AND EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Long' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
BEGIN
	-- Spatial reference is in the table.  If it is complete, return all the fields, otherwise fill in as a lat-long
	IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Spatial_Ref' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
			AND EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Spatial_Ref_System' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
		SET @Select = @Select + 'Spatial_Ref, Spatial_Ref_System, Lat, Long '
	ELSE
		SET @Select = @Select + 
				'	CASE 
						WHEN Lat>=0 THEN CAST(Lat AS VARCHAR(20)) + ''N'' 
						ELSE CAST((0-Lat) AS VARCHAR(20)) + ''S'' 
					END + '', '' +  
					CASE 
						WHEN Long>=0 THEN CAST(Long AS VARCHAR(20)) + ''E'' 
						ELSE CAST((0-Long) AS VARCHAR(20)) + ''W'' 
					END AS Spatial_Ref, 
					''LTLN'' AS Spatial_Ref_System, 
					Lat, 
					Long '
	IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Vague_Date_Start' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
		SET @Select = @Select + ', Vague_Date_Start'
	SET @SQL = @Select + ' FROM #REPORT_OUTPUT'
END
ELSE IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Sample_Key' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
BEGIN
	SET @Select = @Select + 
			'	S.Sample_Key,
				S.Spatial_Ref,
				S.Spatial_Ref_System,
				S.Lat,
				S.Long,
				S.Vague_Date_Start '
	SET @SQL = @Select + ' FROM #REPORT_OUTPUT R INNER JOIN [Sample] S ON S.Sample_Key=R.Sample_Key'
END
ELSE IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Location_Key' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
BEGIN
	SET @Select = @Select + 
			'	L.Location_Key,
				L.Spatial_Ref,
				L.Spatial_Ref_System,
				L.Lat,
				L.Long '
	IF EXISTS(SELECT * FROM TempDB..SysColumns WHERE Name='Vague_Date_Start' AND ID=Object_Id('TempDB..#REPORT_OUTPUT'))
		SET @Select = @Select + ', R.Vague_Date_Start'
	SET @SQL = @Select + ' FROM #REPORT_OUTPUT R
			INNER JOIN [Location] L ON L.Location_Key=R.Location_Key'
END

IF @Order IS NOT NULL
	SET @SQL = @SQL + @Order

IF @SQL IS NOT NULL 
	EXEC sp_executesql @stmt = @SQL

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpatialRefsForReport_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_SpatialRefsForReport_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpatialRefsForReport_Select TO [Dev - JNCC SQL]
END

GO