/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Names') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_Names]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 5 $
    $Date: 30/07/04 10:29 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Names]
AS
	-- Set Match_Count first.
	UPDATE	#Names
	SET	Match_Count =  (SELECT Count(*) FROM Individual
				WHERE	Import_Value = Forename + ' ' + Surname
				OR	Import_Value = Surname + ', ' + Forename
				OR	Import_Value = Surname + ' ' + Forename
				OR	Import_Value = Initials + ' ' + Surname
				OR	Import_Value = Initials + Surname
				OR	Import_Value = Surname + ', ' + Initials
				OR	Import_Value = Surname + ' ' + Initials
				OR	Import_Value = Surname
				)
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
	UPDATE	#Names
	SET	Match_Key = Name_Key,
		Match_Value = dbo.ufn_GetFormattedName(Name_Key)
	FROM	Individual
	WHERE	Match_Count = 1
	AND 	Match_Key IS NULL
	AND 	(Import_Value = Forename + ' ' + Surname
	OR	 Import_Value = Surname + ', ' + Forename
	OR	 Import_Value = Surname + ' ' + Forename
	OR	 Import_Value = Initials + ' ' + Surname
	OR	 Import_Value = Initials + Surname
	OR	 Import_Value = Surname + ', ' + Initials
	OR	 Import_Value = Surname + ' ' + Initials
	OR	 Import_Value = Surname)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Names') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_Names'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_Names TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_Names TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_Names TO [Dev - JNCC SQL]
END
GO