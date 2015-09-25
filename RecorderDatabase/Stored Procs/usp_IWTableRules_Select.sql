/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWTableRules_Select]')
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_IWTableRules_Select]
GO

/*===========================================================================*\
  Description:	List of table rules, in the order they must be applied.

  Parameters:	None

  Created:		June 2004

  Last revision information:
	$Revision: 2 $
	$Date: 5/07/04 11:30 $
	$Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWTableRules_Select]
AS
	SELECT		IW_Table_Rule_Key,
				Table_Name,
				Filter_Expression
	FROM		IW_Table_Rule
	ORDER BY	Sequence
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWTableRules_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWTableRules_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWTableRules_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWTableRules_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWTableRules_Select TO [Dev - JNCC SQL]
END

