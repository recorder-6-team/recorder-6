/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatch_SampleTypes')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatch_SampleTypes]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched
                only.

  Parameters:   <none>

  Created:  July 2004

  Last revision information:
    $Revision: 3 $
    $Date: 27/07/04 15:09 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_SampleTypes]
AS
    UPDATE      #SampleTypes
    SET         Match_Count     =  (SELECT  Count(*)
                                    FROM    Sample_Type     AS  t
                                    WHERE   t.Short_Name    =   Import_Value
                                    OR      t.Long_Name     =   Import_Value)
    WHERE       Match_Key       IS NULL

    UPDATE      i
    SET         i.Match_Value   =   t.Short_Name,
                i.Match_Key     =   t.Sample_Type_Key
    FROM        #SampleTypes    AS  i
    INNER JOIN  Sample_Type     AS  t
    ON          (t.Short_Name   =   i.Import_Value
    OR          t.Long_Name     =   i.Import_Value)
    WHERE       i.Match_Count   =   1
    AND         i.Match_Key     IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_SampleTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_SampleTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_SampleTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_SampleTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_SampleTypes TO [Dev - JNCC SQL]
END
GO