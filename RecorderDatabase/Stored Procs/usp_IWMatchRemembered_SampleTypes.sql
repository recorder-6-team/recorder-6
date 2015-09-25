/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatchRemembered_SampleTypes')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchRemembered_SampleTypes]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values from previous
                imports.

  Parameters:   <none>

  Created:      July 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/07/04 14:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_SampleTypes]
AS
    UPDATE      i
    SET         i.Match_Count           =   1,
                i.Match_Key             =   m.Matched_Key,
                i.Match_Value           =   t.Short_Name,
                i.Remembered            =   1
    FROM        #SampleTypes            AS  i
    INNER JOIN  IW_Matched_Sample_Types AS  m
    ON          m.Matched_Value         =   i.Import_Value
    INNER JOIN  Sample_Type             AS  t
    ON          t.Sample_Type_Key       =   m.Matched_Key
    WHERE       i.Match_Key             IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_SampleTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatchRemembered_SampleTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatchRemembered_SampleTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatchRemembered_SampleTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatchRemembered_SampleTypes TO [Dev - JNCC SQL]
END
GO