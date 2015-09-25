/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_CalculateSurveyEventSpatialRef]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_CalculateSurveyEventSpatialRef]
GO

/*===========================================================================*\
  Description:  Set the spatial reference fields of any records in
                #Survey_Event whose samples all have the same spatial
                reference to the values from the sample.

  Parameters:   None

  Created:      June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 1/07/04 9:10 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_CalculateSurveyEventSpatialRef]
AS
    UPDATE      #Survey_Event
    SET         Spatial_Ref             =   p.Spatial_Ref,
                Spatial_Ref_System      =   p.Spatial_Ref_System,
                Lat                     =   p.Lat,
                Long                    =   p.Long,
                Spatial_Ref_Qualifier   =   p.Spatial_Ref_Qualifier
    FROM        #Survey_Event           AS  e
    INNER JOIN  #Sample                 AS  p
    ON          p.Survey_Event_Key      =   e.Survey_Event_Key
    WHERE       NOT EXISTS (
                    SELECT      1
                    FROM        #Sample             AS  p2
                    WHERE       p2.Survey_Event_Key =   p.Survey_Event_Key
                    AND         p2.Spatial_Ref      <>  p.Spatial_Ref)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_CalculateSurveyEventSpatialRef') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_CalculateSurveyEventSpatialRef'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateSurveyEventSpatialRef TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateSurveyEventSpatialRef TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_CalculateSurveyEventSpatialRef TO [Dev - JNCC SQL]
END