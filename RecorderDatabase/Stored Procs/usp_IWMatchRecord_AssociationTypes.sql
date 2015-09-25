/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_AssociationTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_AssociationTypes]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/07/04 14:37 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_AssociationTypes]
AS
	-- Update existing items, if they were "rematched"
	UPDATE	IW_Matched_Association_Types
	SET	Matched_Key = Match_Key
	FROM	#AssociationTypes
	WHERE	Matched_Value = Import_Value
	AND	Match_Key IS NOT NULL
	AND	Manual_Match = 1

	-- Add the new ones now.
	INSERT INTO 	IW_Matched_Association_Types
	SELECT		Import_Value, Match_Key
	FROM		#AssociationTypes 
	LEFT JOIN	IW_Matched_Association_Types M ON M.Matched_Value = Import_Value
	WHERE		Matched_Value IS NULL
	AND		Match_Key IS NOT NULL
	AND		Manual_Match = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_AssociationTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_AssociationTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_AssociationTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_AssociationTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_AssociationTypes TO [Dev - JNCC SQL]
END
GO