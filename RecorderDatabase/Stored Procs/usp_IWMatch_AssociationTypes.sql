/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AssociationTypes') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_AssociationTypes]
GO

/*===========================================================================*\
  Description:  Populate import table with matched values for unique matched only.

  Parameters:   <none>

  Created:  June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 27/07/04 14:53 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_AssociationTypes]
AS
    UPDATE  #AssociationTypes
    SET Match_Count =  (SELECT  Count(*)
                FROM    Relationship_Type
                WHERE   Import_Value = Short_Name
                OR      Import_Value = Long_Name
                )
    WHERE   Match_Key IS NULL

    UPDATE  #AssociationTypes
    SET Match_Value = Short_Name,
        Match_Key = Relationship_Type_Key
    FROM    Relationship_Type
    WHERE   Match_Count = 1
    AND Match_Key IS NULL
    AND (Import_Value = Short_Name
    OR  Import_Value = Long_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_AssociationTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatch_AssociationTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatch_AssociationTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatch_AssociationTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatch_AssociationTypes TO [Dev - JNCC SQL]
END
GO