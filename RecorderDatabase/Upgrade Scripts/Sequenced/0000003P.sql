/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	@SearchKey
		@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/10/06 16:45 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch]
	@SearchKey char(16) = NULL,
	@SearchText varchar(100)
AS
	IF @SearchKey IS NULL
		SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') + ISNULL(' - ' + TL.Item_Name, '') AS SearchTerm

		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN 	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name LIKE @SearchText + '%'
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
		OR 	ITN.Authority LIKE @SearchText + '%')
		AND	TLI.Taxon_List_Version_To IS NULL
		ORDER BY SearchTerm
	ELSE
		SELECT	ITN.Taxon_List_Item_Key AS Item_Key,
			CASE Actual_Name_Italic
				WHEN 1 THEN '<i>' + Actual_Name + '</i>'
				ELSE Actual_Name
			END + ISNULL(' ' + ITN.Authority, '') AS DisplayTerm, 
			Actual_Name + ISNULL(' ' + ITN.Authority, '') AS SearchTerm
	
		FROM	Index_Taxon_Name ITN
		JOIN 	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key AND TLV.Taxon_List_Key = @SearchKey
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

		WHERE 	(Actual_Name LIKE @SearchText + '%'
		OR	Actual_Name + ' ' + ITN.Authority LIKE @SearchText + '%'
		OR 	ITN.Authority LIKE @SearchText + '%')
		AND	TLI.Taxon_List_Version_To IS NULL
		ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Species_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch TO [Dev - JNCC SQL]
END
GO


IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id(N'[dbo].[usp_RemoveUnwantedOccurrences]') AND ObjectProperty(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_RemoveUnwantedOccurrences]
GO
    
/*
  DESCRIPTION
  This procedure removes invalid taxon and biotope occurrences
  that could have appeared after an import.  Returns the deleted 
  occurrence keys

  PARAMETERS
  None

  Last Revision Details:
    $Revision: 1 $
    $Date: 20/10/06 16:45 $
    $Author: Johnvanbreda $
    
*/

CREATE PROCEDURE [dbo].[usp_RemoveUnwantedOccurrences]
AS

SET NOCOUNT ON

   BEGIN TRAN
	-- Gather invalid taxon occurrences keys, they will be used several times
	SELECT Occ.Taxon_Occurrence_Key INTO #DeleteTaxa
	FROM Taxon_Occurrence Occ 
	LEFT JOIN Taxon_Determination TD ON TD.Taxon_Occurrence_Key=Occ.Taxon_Occurrence_Key
	WHERE TD.Taxon_Determination_Key IS NULL

	--Record the records we are about to remove
	SELECT Taxon_Occurrence_Key AS ItemKey, CAST('TAXON_OCCURRENCE' AS VARCHAR(30)) as TableName
 	INTO #Deletions
	FROM #DeleteTaxa

	INSERT INTO #Deletions
	SELECT TOS.Source_Link_Key AS ItemKey, 'TAXON_OCCURRENCE_SOURCES' as TableName
	FROM TAXON_OCCURRENCE_SOURCES TOS
	INNER JOIN #DeleteTaxa D ON D.Taxon_Occurrence_Key=TOS.Taxon_Occurrence_Key

	INSERT INTO #Deletions
	SELECT TOD.Taxon_Occurrence_Data_Key AS ItemKey, 'TAXON_OCCURRENCE_DATA' as TableName
	FROM TAXON_OCCURRENCE_DATA TOD
	INNER JOIN #DeleteTaxa D ON D.Taxon_Occurrence_Key=TOD.Taxon_Occurrence_Key

	-- Remove associated Taxon Occurrence Sources
	DELETE FROM Taxon_Occurrence_Sources 
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Remove associated Taxon Occurrence Data
	DELETE FROM Taxon_Occurrence_Data
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Remove associated specimen data
	DELETE FROM Specimen
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- And finally remove Taxon Occurrences
	DELETE FROM Taxon_Occurrence 
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Gather invalid biotope occurrences keys, they will be used several times
	SELECT 	Occ.Biotope_Occurrence_Key into #DeleteBiotopes
	FROM Biotope_Occurrence Occ WHERE Occ.Biotope_Occurrence_Key NOT IN (
		SELECT Biotope_Occurrence_Key FROM Biotope_Determination
	)

	--Record the records we are about to remove
	INSERT INTO #Deletions
	SELECT Biotope_Occurrence_Key AS ItemKey, 'BIOTOPE_OCCURRENCE' as TableName
	FROM #DeleteBiotopes

	INSERT INTO #Deletions
	SELECT BOS.Source_Link_Key AS ItemKey, 'BIOTOPE_OCCURRENCE_SOURCES' as TableName
	FROM BIOTOPE_OCCURRENCE_SOURCES BOS
	INNER JOIN #DeleteBiotopes D ON D.Biotope_Occurrence_Key=BOS.Biotope_Occurrence_Key

	INSERT INTO #Deletions
	SELECT BOD.Biotope_Occurrence_Data_Key AS ItemKey, 'BIOTOPE_OCCURRENCE_DATA' as TableName
	FROM BIOTOPE_OCCURRENCE_DATA BOD
	INNER JOIN #DeleteBiotopes D ON D.Biotope_Occurrence_Key=BOD.Biotope_Occurrence_Key

	-- Remove associated Biotope Occurrence Sources
	DELETE FROM Biotope_Occurrence_Sources
	WHERE Biotope_Occurrence_Key IN (
		SELECT Biotope_Occurrence_Key FROM #DeleteBiotopes
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Remove associated Biotope Occurrence Data
	DELETE FROM Biotope_Occurrence_Data
	WHERE Biotope_Occurrence_Key IN (
		SELECT Biotope_Occurrence_Key FROM #DeleteBiotopes
	)
	IF @@Error > 0 GOTO RollBackAndExit
	
	-- And finally remove Biotope Occurrences
	DELETE FROM Biotope_Occurrence 
	WHERE Biotope_Occurrence_Key IN (
		SELECT Biotope_Occurrence_Key FROM #DeleteBiotopes
	)
	IF @@Error > 0 GOTO RollBackAndExit

	SELECT * FROM #Deletions

  COMMIT TRAN

SET NOCOUNT OFF 

RollBackAndExit: 
    IF @@TranCount> 0 ROLLBACK TRAN 
    SET NOCOUNT OFF  
GO 

-- Grant access permissions
IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id('[dbo].[usp_RemoveUnwantedOccurrences]') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_RemoveUnwantedOccurrences] TO [R2k_Administrator]
END
GO 


