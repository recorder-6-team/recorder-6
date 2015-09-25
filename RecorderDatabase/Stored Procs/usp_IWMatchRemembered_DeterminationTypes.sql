SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_DeterminationTypes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_DeterminationTypes]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 19/01/09 11:30 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_DeterminationTypes]
AS
	-- Update temp table with relevant data.
	UPDATE 	#DeterminationTypes
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = Short_Name, 
		Remembered = 1
	FROM 	IW_Matched_Determination_Types 
	JOIN	Determination_Type ON Determination_Type_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_DeterminationTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_DeterminationTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_DeterminationTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_DeterminationTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_DeterminationTypes TO [Dev - JNCC SQL]
END
GO