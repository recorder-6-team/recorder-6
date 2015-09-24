/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonOccurrenceDataTypes_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonOccurrenceDataTypes_Select]
GO

/*===========================================================================*\
  Description:	Retrieves the list of taxon occurrence data types available.

  Parameters:	None

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 6/07/04 12:44 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrenceDataTypes_Select]
AS

SELECT 
		MT.Measurement_Type_Key, 
		MQ.Measurement_Qualifier_Key, 
		MU.Measurement_Unit_Key,
		MT.Short_Name + ' of ' + MQ.Short_Name + ' (' + MU.Short_Name+ ')' AS Item_Name
FROM Measurement_Type MT
INNER JOIN Measurement_Unit MU ON MU.Measurement_Type_Key = MT.Measurement_Type_Key
INNER JOIN Measurement_Qualifier MQ ON MQ.Measurement_Type_Key = MT.Measurement_Type_Key
INNER JOIN Measurement_Type_Context MTC ON MTC.Measurement_Type_Key=Mt.Measurement_Type_Key
		AND MTC.Measurement_Context_Key='NBNSYS0000000003'
ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrenceDataTypes_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_TaxonOccurrenceDataTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceDataTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceDataTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceDataTypes_Select TO [Dev - JNCC SQL]
END

GO