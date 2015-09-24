
SET QUOTED_IDENTIFIER ON 
SET ANSI_NULLS ON 
GO

IF OBJECT_ID('dbo.usp_RelationRoleType_Select') IS NOT NULL
	DROP PROCEDURE dbo.usp_RelationRoleType_Select
GO

/*===========================================================================
Description: 
	Gets all the relation role type records for display in a dropdown
Parameters: 
	
Created: 
	05/10/2009
Last revision information: 
	$Revision: 1 $ 
	$Date: 5/10/09 11:30 $ 
	$Author: Bonnerearle $ 
===========================================================================*/
CREATE PROCEDURE dbo.usp_RelationRoleType_Select
AS

SET NOCOUNT ON

SELECT	Short_Name
FROM	dbo.Relation_Role_Type
ORDER BY Short_Name

GO

GRANT EXECUTE ON dbo.usp_RelationRoleType_Select TO 
	R2k_AddOnly, 
	R2k_Administrator,
	R2k_FullEdit,
	R2k_ReadOnly,
	R2k_RecordCardsOnly

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_RelationRoleType_Select TO "Dev - JNCC SQL"
GO



