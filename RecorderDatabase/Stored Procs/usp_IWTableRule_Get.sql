/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRule_Get]')
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_IWTableRule_Get]
GO

/*===========================================================================*\
  Description:	A table rule.

  Parameters:	@table_rule_key			Identifies the table rule.

  Created:		June 2004

  Last revision information:
	$Revision: 2 $
	$Date: 5/07/04 11:30 $
	$Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRule_Get]
	@table_rule_key		CHAR(16)
AS
	SELECT		Table_Name,
				Filter_Expression
	FROM		IW_Table_Rule
	WHERE		IW_Table_Rule_Key	=	@table_rule_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRule_Get') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_IWTableRule_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWTableRule_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWTableRule_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
			GRANT EXECUTE ON dbo.usp_IWTableRule_Get TO [Dev - JNCC SQL]
END

