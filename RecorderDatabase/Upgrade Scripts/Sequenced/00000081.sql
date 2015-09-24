/*===========================================================================*\
  Description:	Retrieves the list of taxon occurrence data types available.

  Parameters:	None

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*=========================================================================== */
ALTER PROCEDURE [dbo].[usp_TaxonOccurrenceDataTypes_Select]
AS

SELECT 
		MT.Measurement_Type_Key, 
		MQ.Measurement_Qualifier_Key, 
		MU.Measurement_Unit_Key,
		MT.SHORT_NAME AS MeasurementType,
		MQ.SHORT_NAME AS Qualifier, 
		MU.SHORT_NAME AS Unit
FROM Measurement_Type MT
INNER JOIN Measurement_Unit MU ON MU.Measurement_Type_Key = MT.Measurement_Type_Key
INNER JOIN Measurement_Qualifier MQ ON MQ.Measurement_Type_Key = MT.Measurement_Type_Key
INNER JOIN Measurement_Type_Context MTC ON MTC.Measurement_Type_Key=Mt.Measurement_Type_Key
		AND MTC.Measurement_Context_Key='NBNSYS0000000003'
ORDER BY MeasurementType, Qualifier, Unit

