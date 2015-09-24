/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetPartialTaxonSearchText]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetPartialTaxonSearchText
GO


/*===========================================================================*\
  Description:	Returns the taxon search text formatted for use when matching partial taxon names.
		Up to three individual words are searched for individually, separated by spaces.
		Wildcards are used to create a search pattern for use with the LIKE operator.
		If any search term other than the first begins with a dot, match the end of that word.

  Parameters:	@SearchText

  Created:	February 2008

  Last revision information:
    $Revision: 1 $
    $Date: 25/02/08 14:21 $
    $Author: Johndurman $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetPartialTaxonSearchText]
	(@SearchText varchar(100))
RETURNS varchar(110)
AS
BEGIN
	DECLARE	@FormattedSearchText varchar(110)		-- Holds the result
			
	DECLARE @SpacePos INT, 
			@FirstWord	VARCHAR(100),
			@SecondWord VARCHAR(100),
			@RestOfText	VARCHAR(100)

	SET @FormattedSearchText = LTRIM(RTRIM(@SearchText))
	SET @SpacePos = CHARINDEX(' ', @FormattedSearchText)
	IF @SpacePos > 0
	BEGIN
		SET @FirstWord = LEFT(@FormattedSearchText, @SpacePos - 1) + '% '
		SET @RestOfText = LTRIM(SUBSTRING(@FormattedSearchText, @SpacePos + 1, 110))

		SET @SpacePos = CHARINDEX(' ', @RestOfText)
		IF @SpacePos > 0
		BEGIN
			SET @SecondWord = LEFT(@RestOfText, @SpacePos - 1)
			SET @RestOfText = LTRIM(SUBSTRING(@RestOfText, @SpacePos + 1, 110))
			IF LEFT(@SecondWord, 1) = '.'
			BEGIN
				SET @SecondWord = '%' + SUBSTRING(@SecondWord, 2, 110) + ' '
			END
			ELSE
			BEGIN
				SET @SecondWord = @SecondWord + '% '
			END
		END

		IF LEFT(@RestOfText, 1) = '.'
		BEGIN
			SET @RestOfText = '%' + SUBSTRING(@RestOfText, 2, 110)
		END
		ELSE
		BEGIN
			SET @RestOfText = @RestOfText + '%'
		END

		SET @FormattedSearchText = @FirstWord + ISNULL(@SecondWord, '') + @RestOfText
	END
	ELSE
	BEGIN
		SET @FormattedSearchText = @FormattedSearchText + '%'
	END

	RETURN @FormattedSearchText
END

GO