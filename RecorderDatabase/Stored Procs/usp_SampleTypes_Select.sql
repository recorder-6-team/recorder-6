/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_SampleTypes_Select')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleTypes_Select]
GO

/*===========================================================================*\
  Description:  Returns a list of Sample types.

  Parameters:   <none>

  Created:      July 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/07/04 14:07 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleTypes_Select]
AS
    SELECT      Sample_Type_Key AS  Item_Key,
                Short_Name      AS  Item_Name
    FROM        Sample_Type
    ORDER BY    Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_SampleTypes_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_SampleTypes_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_SampleTypes_Select TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_SampleTypes_Select TO [Dev - JNCC SQL]
END
GO