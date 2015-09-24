/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchRule_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchRule_Select]
GO

/*===========================================================================*\
  Description:	returns details of a match rule

  Parameters:	@Key - IW_Match_Rule_Key

  Created:	June 2004

  Last revision information:
    $Revision: 5 $
    $Date: 5/07/04 18:12 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRule_Select]
	@Key CHAR(16)
AS
	SELECT 	[Sequence], 
		Item_Name, 
		Control_Type,
		Imported_Data_Insert_Sql,
		Remembered_Matches_Procedure,
		Match_Procedure,
		Record_Matches_Procedure,
		New_Entry_Procedure,
		Requires_Checklist,
		Set_Match_Procedure,
		Table_Create_Sql,
		Key_To_Caption_Procedure,
		Search_Type,
		Checklists_Select_Procedure,
		Termlist_Select_Procedure
	FROM 	IW_Match_Rule
	WHERE 	IW_Match_Rule_Key = @Key
	ORDER BY [Sequence]
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRule_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWMatchRule_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRule_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRule_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRule_Select TO [Dev - JNCC SQL]
END
GO