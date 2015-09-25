SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_CalculateZeroAbundance]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_CalculateZeroAbundance]
GO


/*===========================================================================*\
  Description:  Set the Zero_Abundance flag on #Taxon_Occurrence where there
                exist numeric abundance data and they are all zero.

  Parameters:   None

  Created:      June 2004

  Last revision information:
    $Revision: 3 $
    $Date: 22/02/06 13:11 $
    $Author: Johnvanbreda $

 -- Einige Ergänzungen durch TS
    08.08.2011

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_CalculateZeroAbundance]
AS
    UPDATE      #Taxon_Occurrence
    SET         Zero_Abundance      =   1
    FROM        #Taxon_Occurrence   AS  o
    WHERE       (SELECT     SUM(
					CASE RTRIM(LTRIM(d.Data))
					WHEN '0' THEN 0
					ELSE 1
					END)
                FROM        #Taxon_Occurrence_Data      AS  d
                INNER JOIN  Measurement_Qualifier       AS  q
                ON          q.Measurement_Qualifier_Key =   d.Measurement_Qualifier_Key
                INNER JOIN  Measurement_Unit as u
                ON          q.Measurement_Type_Key = u.Measurement_Type_Key
                WHERE       d.Taxon_Occurrence_Key      =   o.Taxon_Occurrence_Key
                AND         q.Measurement_Type_Key      =   'NBNSYS0000000004' /* Abundance */
                AND         u.data_type = 'N'  -- only numeric (TS)
                )
                =           0

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_CalculateZeroAbundance') AND SysStat & 0xf = 4)
BEGIN 
    	PRINT 'Setting up security on procedure usp_ImportWizard_CalculateZeroAbundance'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateZeroAbundance TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateZeroAbundance TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateZeroAbundance TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateZeroAbundance TO [Dev - JNCC SQL]
END
