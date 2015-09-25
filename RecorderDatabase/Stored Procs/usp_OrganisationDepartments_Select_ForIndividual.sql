/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OrganisationDepartments_Select_ForIndividual]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OrganisationDepartments_Select_ForIndividual]
GO

/*===========================================================================*\
  Description:	Returns the list of departments that an individual could
      					possibly belong to

  Parameters:	@Key	Name key

  Created:	Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 16/01/04 11:28 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OrganisationDepartments_Select_ForIndividual]
	@Key char(16)
AS

SET NOCOUNT ON

--Select data.  Note relationships are selected which ever way round they are
SELECT DISTINCT
  OD.Organisation_Department_Key, 
	OD.Item_Name
FROM Organisation_Department OD
INNER JOIN Organisation O ON O.Name_Key=OD.Name_Key
INNER JOIN Name_Relation NR ON (NR.Name_Key_1=OD.Name_Key OR NR.Name_Key_2=OD.Name_Key)
WHERE NR.Name_Key_1=@Key
OR NR.Name_Key_2=@Key	

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OrganisationDepartments_Select_ForIndividual') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OrganisationDepartments_Select_ForIndividual'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OrganisationDepartments_Select_ForIndividual TO [Dev - JNCC SQL]
END


GO