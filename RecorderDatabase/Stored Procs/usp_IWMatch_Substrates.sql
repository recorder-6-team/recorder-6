/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Substrates') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_Substrates]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched only.

  Parameters:   <none>

  Created:  June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 27/07/04 14:09 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Substrates]
AS
    UPDATE  #Substrates
    SET Match_Count =  (SELECT  Count(*)
                FROM    Substrate
                WHERE   Import_Value = Short_Name
                OR      Import_Value = Long_Name
                )
    WHERE   Match_Key IS NULL
    
    UPDATE  #Substrates
    SET Match_Value = Short_Name,
        Match_Key = Substrate_Key
    FROM    Substrate
    WHERE   Match_Count = 1
    AND Match_Key IS NULL
    AND (Import_Value = Short_Name
    OR  Import_Value = Long_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Substrates') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_Substrates'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_Substrates TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_Substrates TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_Substrates TO [Dev - JNCC SQL]
END
GO