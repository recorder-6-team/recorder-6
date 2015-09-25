/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Names_Select_AllChildrenForLevel') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_Names_Select_AllChildrenForLevel
GO

/*===========================================================================*\
  Description:	Retrieves keys of all items below a survey/event/sample.

  Parameters:
	@Key	Parent key, either individual or organisation.
	@IsOrg	0 if parent key is individual, 1 if organisation.

  Created:	January 2008

  Last revision information:
    $Revision: 1 $
    $Date: 30/01/08 11:07 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Names_Select_AllChildrenForLevel
	@Key	CHAR(16),
	@IsOrg	BIT
AS
	IF @IsOrg = 0
		SELECT	DISTINCT 'Organisation' AS TableName, O.Name_Key AS ItemKey
		FROM Name_Relation 	NR 
		JOIN Individual 	I 	ON I.Name_Key IN (NR.Name_Key_1, NR.Name_Key_2) 
		JOIN Organisation 	O 	ON O.Name_Key IN (NR.Name_Key_1, NR.Name_Key_2)
		WHERE 	I.Name_Key = @Key
	ELSE
		SELECT	DISTINCT 'Individual' AS TableName, I.Name_Key AS ItemKey
		FROM Name_Relation 	NR 
		JOIN Individual 	I 	ON I.Name_Key IN (NR.Name_Key_1, NR.Name_Key_2) 
		JOIN Organisation 	O 	ON O.Name_Key IN (NR.Name_Key_1, NR.Name_Key_2)
		WHERE 	O.Name_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Names_Select_AllChildrenForLevel') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Names_Select_AllChildrenForLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Names_Select_AllChildrenForLevel TO [Dev - JNCC SQL]
END
GO