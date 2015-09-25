/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Biotopes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_Biotopes]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 4 $
    $Date: 7/07/04 17:29 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_Biotopes]
	@ChecklistKey char(16)
AS
	-- Grab Classification name frist. Avoid joins on update query.
	DECLARE	@Classification varchar(100)

	SELECT	@Classification = Short_Name
	FROM	Biotope_Classification
	WHERE	Biotope_Classification_Key = @ChecklistKey

	-- Update temp table with relevant data.
	UPDATE 	#Biotopes
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedBiotopeName(Matched_Key),
		Remembered = 1,
		Classification = @Classification,
		Classification_Key = @ChecklistKey
	FROM 	IW_Matched_Biotopes
	JOIN	Biotope_List_Item ON Biotope_List_Item_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
	AND	Match_Classification_Key = @ChecklistKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Biotopes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_Biotopes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Biotopes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Biotopes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Biotopes TO [Dev - JNCC SQL]
END
GO