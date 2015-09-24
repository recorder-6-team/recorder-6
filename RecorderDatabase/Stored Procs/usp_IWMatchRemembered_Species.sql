/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_Species]
GO

/*===========================================================================*\
  Description:	
	Populate import table with matched values from previous imports.

  Parameters:	
	@ChecklistKey	The primary key of the current checklist.

  Created:	July 2004

  Last revision information:
    $Revision: 5 $
    $Date: 12/01/09 9:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_Species]
	@ChecklistKey CHAR(16)
AS
	UPDATE 	#Species
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedSpeciesName(Matched_Key),
		Remembered = 1,
		Checklist = TL.Item_Name,
		Checklist_Key = @ChecklistKey
	FROM 	IW_Matched_Species
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = Matched_Key
	JOIN	Taxon_List_Version TLV ON TLI.Taxon_List_Version_Key = TLV.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TLV.Taxon_List_Key = TL.Taxon_List_Key
	WHERE 	Import_Value = Matched_Value 
	AND		Match_Key IS NULL
	AND		(Match_Checklist_Key = @ChecklistKey 
			OR (Match_Checklist_Key IS NULL AND @ChecklistKey IS NULL))
	AND		Temp_User_ID IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_Species TO [Dev - JNCC SQL]
END
GO