/**********************************************************************************/
/* CCN63 - script to get rid of duplicate concept and taxon ranks that occur when */
/* synching data between the taxon dictionary and thesaurus.                      */
/**********************************************************************************/

-- There is a PK on the mapping table's Taxon_Rank_Key, but because 1 rank can actually
-- map to many domains in the thesaurus this should not be there. It will later be replaced
-- by a PK on both Taxon_Rank_Key and Concept_Rank_Key
IF EXISTS(SELECT 1 FROM SysObjects WHERE Name='PK_Taxon_Dictionary_Concept_Rank_Mapping')
	ALTER TABLE Taxon_Dictionary_Concept_Rank_Mapping
	DROP CONSTRAINT PK_Taxon_Dictionary_Concept_Rank_Mapping


DECLARE	@DomainKey CHAR(16), 
		@ConceptRankKey CHAR(16), 
		@ItemName VARCHAR(100),
		@DuplicateCount INT

-- Find the distinct list of concept ranks, with a count of the duplications
DECLARE csr CURSOR FOR
	SELECT Domain_Key, Item_Name, COUNT(Concept_Rank_Key) AS DuplicateCount 
	FROM Concept_Rank
	GROUP BY Domain_Key, Item_Name
	HAVING COUNT(Concept_Rank_Key)>1
	ORDER BY COUNT(Concept_Rank_Key) DESC

OPEN csr

WHILE (1=1)
BEGIN
	FETCH NEXT FROM csr INTO @DomainKey, @ItemName, @DuplicateCount
	IF @@FETCH_STATUS<>0 BREAK
	
	-- Obtain the Concept Rank we want to keep for this name/domain combination
	SELECT TOP 1 @ConceptRankKey=Concept_Rank_Key
	FROM Concept_Rank
	WHERE Domain_Key=@DomainKey
	AND Item_Name=@ItemName

	-- Map all concepts back to the one we are keeping
	UPDATE C
	SET C.Concept_Rank_Key=@ConceptRankKey
	FROM Concept C
	INNER JOIN Concept_Rank CR ON CR.Concept_Rank_Key=C.Concept_Rank_Key
	WHERE CR.Item_Name=@ItemName
	AND CR.Domain_Key=@DomainKey

	-- And also update the mappings
	UPDATE CRM
	SET CRM.Concept_Rank_Key=@ConceptRankKey
	FROM Taxon_Dictionary_Concept_Rank_Mapping CRM
	INNER JOIN Concept_Rank CR ON CR.Concept_Rank_Key=CRM.Concept_Rank_Key
	WHERE CR.Item_Name=@ItemName
	AND CR.Domain_Key=@DomainKey

	-- Delete the now redundant concept ranks
	DELETE FROM Concept_Rank
	WHERE Concept_Rank_Key<>@ConceptRankKey
	AND Domain_Key=@DomainKey
	AND Item_Name=@ItemName

END

CLOSE csr
DEALLOCATE csr

--The second half of this script fixes up the taxon rank duplications.

CREATE TABLE #UniqueRanks (
	Taxon_Rank_Key CHAR(16),
	Long_Name VARCHAR(100),
	Sequence INT
)

DECLARE 
	@TaxonRankKey CHAR(16),
	@LongName VARCHAR(100),
	@Sequence INT

DECLARE csr CURSOR FOR
	SELECT Taxon_Rank_Key, Long_Name, Sequence
	FROM Taxon_Rank TR

OPEN csr

WHILE 1=1 
BEGIN
	FETCH NEXT FROM csr INTO @TaxonRankKey, @LongName, @Sequence
	IF @@FETCH_STATUS <> 0 BREAK

	IF NOT EXISTS(SELECT 1 FROM #UniqueRanks WHERE Long_Name=@LongName and Sequence=@Sequence)
		INSERT INTO #UniqueRanks VALUES (@TaxonRankKey, @LongName, @Sequence)

END
CLOSE csr

OPEN csr

DECLARE @RankKeyToUse CHAR(16)

SELECT * 
INTO #TempMapping
FROM Taxon_Dictionary_Concept_Rank_Mapping

CREATE TABLE #RanksToDelete (
	Taxon_Rank_Key CHAR(16)
)

WHILE 1=1 
BEGIN
	FETCH NEXT FROM csr INTO @TaxonRankKey, @LongName, @Sequence
	IF @@FETCH_STATUS <> 0 BREAK

	IF NOT EXISTS(SELECT 1 FROM #UniqueRanks WHERE Taxon_Rank_Key=@TaxonRankKey) 
	BEGIN
		-- This rank is a duplicate which we want to remove, so find the original which we want to map to
		SELECT @RankKeyToUse=Taxon_Rank_Key
		FROM #UniqueRanks
		WHERE Long_Name=@LongName
		AND Sequence=@Sequence

		-- Now, we need to update the temp copy of the dictionary mapping table. 
		UPDATE #TempMapping
		SET Taxon_Rank_Key=@RankKeyToUse
		WHERE Taxon_Rank_Key=@TaxonRankKey

		-- And update the tables that use this key, then store the unused rank for later deletion
		UPDATE Taxon_List_Item 
		SET Taxon_Rank_Key=@RankKeyToUse
		WHERE Taxon_Rank_Key=@TaxonRankKey

		INSERT INTO #RanksToDelete VALUES (@TaxonRankKey)

	END
END

CLOSE csr
DEALLOCATE csr

DELETE FROM Taxon_Dictionary_Concept_Rank_Mapping
INSERT INTO Taxon_Dictionary_Concept_Rank_Mapping (Taxon_Rank_Key, Concept_Rank_Key)
	SELECT DISTINCT Taxon_Rank_Key, Concept_Rank_Key
	FROM #TempMapping

-- Now the unused ranks can be cleared out
DELETE FROM TAXON_RANK
WHERE Taxon_Rank_Key IN (
	SELECT Taxon_Rank_Key FROM #RanksToDelete
)



DROP TABLE #UniqueRanks
DROP TABLE #TempMapping
DROP TABLE #RanksToDelete

ALTER TABLE Taxon_Dictionary_Concept_Rank_Mapping
	ADD CONSTRAINT [PK_Taxon_Dictionary_Concept_Rank_Mapping] PRIMARY KEY CLUSTERED 
(
	Taxon_Rank_Key ASC, 
	Concept_Rank_Key ASC
)

