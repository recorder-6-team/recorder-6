/****** Object:  UserDefinedFunction [dbo].[ufn_FormattedSpeciesName]    Script Date: 11/25/2015 12:42:09 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
 * Description:	Returns the formatted species Name.
 *
 * Parameters:	@ActualName 		ITN.Actual_Name,
 *				@Authority 			ITN.Authority ,
 *				@PreferredAuthority	ITN.Authority where ITN.Taxon_List_Item_Key = TLI.Preferred_Name,
 *				@PreferredName		ITN.Preferred_Name,
 *				@ActualItalic 		ITN.Actual_Name_Italic,
 *				@PreferredItalic	ITN.Preferred_Name_Italic,
 *				@Attribute			ITN.Attribute,
 *				@RankName			ITN.Short_Name
 *              @CanExpand			BIT,
 *              @CanExpandChar      VARCHAR(50)
 * AUTHOR:	Qing Sun, Dorset Software
 * CREATED: 25/11/2008
 * Modified for Mantis 594  November 2015
\*===========================================================================*/
ALTER FUNCTION [dbo].[ufn_FormattedSpeciesName]
( @ActualName 			VARCHAR(100),
  @Authority 			VARCHAR(100),
  @PreferredAuthority	VARCHAR(100),
  @PreferredName		VARCHAR(100),
  @ActualItalic 		BIT,
  @PreferredItalic		BIT,
  @Attribute			VARCHAR(100),
  @RankName				VARCHAR(100),
  @CanExpand			BIT,
  @CantExpandSuffix       VARCHAR(50))
RETURNS varchar(200)
AS

BEGIN
	DECLARE	@FormattedName 	VARCHAR(200)
	
	SET 	@FormattedName = ''

    IF @CanExpand = 1  SET
       @CantExpandSuffix = NULL 	
	
	IF @ActualItalic = 1
		SET @FormattedName = '<i>' + @ActualName + '</i>'
	ELSE
		SET @FormattedName = @ActualName

	SET @FormattedName = @FormattedName + ISNULL(' ' + @Attribute, '') + ISNULL(' ' + @Authority, '') + ISNULL(' [' + @RankName + ']', '')

	IF @ActualName <> @PreferredName
	BEGIN
		
		IF @PreferredItalic = 1
			SET @FormattedName = @FormattedName + ' (' + '<i>' + @PreferredName + '</i>' + ISNULL(' ' + @PreferredAuthority, '') + ')'
		ELSE
			SET @FormattedName = @FormattedName + ' (' + @PreferredName + ISNULL(' ' + @PreferredAuthority, '') + ')'
	END	
   
    SET @FormattedName = @FormattedName + ISNull(@CantExpandSuffix,'')
	
	RETURN @FormattedName
END

