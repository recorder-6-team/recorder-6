/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_IWColumnType_Select]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWColumnType_Select]
GO

/*===========================================================================*\
  Description:  Returns details of a single import wizard column type

  Parameters:   @Key - IW_Column_Type_Key

  Created:  May 2004

  Last revision information:
    $Revision: 3 $
    $Date: 8/07/04 14:53 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWColumnType_Select]
    @Key CHAR(16)
AS

    SELECT
        Item_Name,
        Required,
        Commonly_Used,
        Parser_Class_Name,
        Term_List_Table,
        Field_Type,
        Maximum_Length
    FROM    IW_Column_Type
    WHERE   IW_Column_Type_Key=@Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWColumnType_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWColumnType_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWColumnType_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWColumnType_Select TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWColumnType_Select TO [Dev - JNCC SQL]
END
GO