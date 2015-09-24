/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Departments_Select_ForOrganisation]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Departments_Select_ForOrganisation]
GO

/*===========================================================================*\
  Description:	Returns all departments within an organisation

  Parameters:	@Key	Name key

  Created:	Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/01/06 11:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Departments_Select_ForOrganisation]
	@Key char(16)
AS

SET NOCOUNT ON

--Select data.  Note relationships are selected which ever way round they are
SELECT DISTINCT
  OD.Organisation_Department_Key, 
	OD.Acronym, 
	OD.Item_Name,
	OD.Custodian,
	OD.Entered_By
FROM Organisation_Department OD
WHERE OD.Name_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Departments_Select_ForOrganisation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Departments_Select_ForOrganisation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_Departments_Select_ForOrganisation TO [Dev - JNCC SQL]
END
GO