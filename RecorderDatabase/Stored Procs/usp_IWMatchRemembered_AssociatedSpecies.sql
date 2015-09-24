/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_AssociatedSpecies') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_AssociatedSpecies]
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
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_AssociatedSpecies]
	@ChecklistKey char(16)
AS
	-- Grab checklist name frist. Avoid joins on update query.
	DECLARE	@Checklist varchar(100)

	SELECT	@Checklist = Item_Name
	FROM	Taxon_List
	WHERE	Taxon_List_Key = @ChecklistKey

	-- Update temp table with relevant data.
	UPDATE 	#AssociatedSpecies
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedSpeciesName(Matched_Key),
		Remembered = 1,
		Checklist = @Checklist,
		Checklist_Key = @ChecklistKey
	FROM 	IW_Matched_Associated_Species
	JOIN	Taxon_List_Item ON Taxon_List_Item_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
	AND	Match_Checklist_Key = @ChecklistKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_AssociatedSpecies') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_AssociatedSpecies'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AssociatedSpecies TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AssociatedSpecies TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AssociatedSpecies TO [Dev - JNCC SQL]
END
GO