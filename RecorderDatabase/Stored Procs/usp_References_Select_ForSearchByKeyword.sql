/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_References_Select_ForSearchByKeyword]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_References_Select_ForSearchByKeyword]
GO

/*===========================================================================*\
  Description:	Retrieve a list of references that match the supplied keywords

  Parameters:	@SearchText

  Created:	December 2005

  Last revision information:
    $Revision: 2 $
    $Date: 15/12/05 15:31 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_References_Select_ForSearchByKeyword]
	@SearchText VARCHAR(500)
AS
	SET @SearchText = RTRIM(LTRIM(@SearchText))
	
	--Create a temp table to hold the parsed search terms
	DECLARE @SearchTerms TABLE 
	(term VARCHAR(150) COLLATE SQL_Latin1_General_CP1_CI_AS)
	
	DECLARE @char INT
	SET @char=1
	DECLARE @currentWord VARCHAR(150)
	
	SET @currentWord=''
	
	--Parse out each individual search term
	WHILE  @char<=LEN(@SearchText)
	BEGIN
		PRINT SUBSTRING(@SearchText, @char, 1)
		IF SUBSTRING(@SearchText, @char, 1) = ' '
		BEGIN
			IF RTRIM(LTRIM(@CurrentWord))<>'' 
				INSERT INTO @SearchTerms VALUES (RTRIM(LTRIM(@CurrentWord)))
			SET @CurrentWord=''
		END
		ELSE
			SET @CurrentWord = @CurrentWord + SUBSTRING(@SearchText, @char, 1)
		SET @char = @char + 1
	END
	
	IF RTRIM(LTRIM(@CurrentWord))<>'' 
				INSERT INTO @SearchTerms VALUES (RTRIM(LTRIM(@CurrentWord)))
	
	-- Find out how many terms we need to match against
	DECLARE @SearchTermCount INT
	SELECT @SearchTermCount = COUNT(*) FROM @SearchTerms
	
	--Query, joining to search terms to ensure they are all matched
	SELECT R.Source_Key, Author, R.Year_Vague_Date_Start, 
	       R.Year_Vague_Date_End, R.Year_Vague_Date_Type, 
	       dbo.ufn_RtfToPlaintext(CAST(R.Title AS VARCHAR(200))) AS Title, Count(DISTINCT ST.Term)
	FROM Reference AS R 
	INNER JOIN VW_REFERENCE_AUTHORS AS A ON R.Source_Key = A.Source_Key
	INNER JOIN Reference_Keyword RK ON RK.Source_Key=R.Source_Key
	INNER JOIN VW_ConceptChildren CC ON CC.Child_Concept_Key=RK.Concept_Key
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=CC.Parent_Concept_Key
	INNER JOIN @SearchTerms ST ON CT.Plaintext LIKE ST.Term + '%' COLLATE SQL_Latin1_General_CP1_CI_AS
	GROUP BY R.Source_Key, Author, R.Year_Vague_Date_Start, 
	       R.Year_Vague_Date_End, R.Year_Vague_Date_Type, dbo.ufn_RtfToPlaintext(CAST(R.Title AS VARCHAR(200)))
	HAVING Count(DISTINCT ST.Term)=@SearchTermCount

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_References_Select_ForSearchByKeyword') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_References_Select_ForSearchByKeyword'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [Dev - JNCC SQL]
END
GO
