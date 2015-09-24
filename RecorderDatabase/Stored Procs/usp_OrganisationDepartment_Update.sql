/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OrganisationDepartment_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OrganisationDepartment_Update]
GO

/*===========================================================================*\
  Description: 	Updates a record in Organisation_Department.

  Parameters:	@Key 
		@ItemName 
		@Description
		@Changed_By

  Created:     	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 11/02/04 15:48 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OrganisationDepartment_Update]
	@Key char(16),
  @Acronym varchar(20),
	@ItemName varchar(100),
  @ChangedBy char(16)
AS
		UPDATE 	Organisation_Department
		SET	Item_Name = @ItemName,
			Acronym = @Acronym,
			Changed_By = @ChangedBy,
			Changed_Date = GetDate()
		WHERE	Organisation_Department_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OrganisationDepartment_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OrganisationDepartment_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Update TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OrganisationDepartment_Update TO [Dev - JNCC SQL]
END
GO
