/****** Object:  UserDefinedFunction [dbo].[ufn_GetFormattedSpeciesName]    Script Date: 03/30/2016 10:04:24 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:	Returns the formatted taxon name for the given taxon list item.

  Parameters:	@Key

  Created:	

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $
    
\*===========================================================================*/
ALTER FUNCTION [dbo].[ufn_GetFormattedSpeciesName]
	(@ListItemKey char(16))
RETURNS varchar(200)
AS
BEGIN
	DECLARE	@FormattedName 			VARCHAR(200),
			@ActualName 			VARCHAR(100),
			@Authority 				VARCHAR(100),
			@PreferredName			VARCHAR(100),
			@PreferredAuthority		VARCHAR(100),
			@ActualItalic 			BIT,
			@PreferredItalic		BIT,
			@Attribute				VARCHAR(100),
			@RankName				VARCHAR(100)
	SET 	@FormattedName	= ''

	SELECT 	@ActualName 		= ITN1.Actual_Name, 
			@Authority 			= ITN1.Authority, 
			@PreferredName 		= ITN1.Preferred_Name,
			@PreferredAuthority	= ITN2.Authority, 
			@ActualItalic 		= ITN1.Actual_Name_Italic,
			@PreferredItalic	= ITN1.Preferred_Name_Italic,
			@Attribute			= TV.Attribute,
			@RankName			= TR.Short_Name
	FROM	Index_Taxon_Name	ITN1
	INNER JOIN	Taxon_LisT_Item		TLI	ON	TLI.Taxon_List_Item_Key = ITN1.Taxon_List_Item_Key
	INNER JOIN	Taxon_Version		TV	ON	TV.Taxon_Version_Key	= TLI.Taxon_Version_Key
	INNER JOIN	Taxon_Rank			TR	ON	TR.Taxon_Rank_Key		= TLI.Taxon_Rank_Key
	INNER JOIN    Index_Taxon_Name	ITN2 ON ITN2.Taxon_List_Item_Key = TLI.Preferred_Name
	WHERE	TLI.Taxon_List_Item_Key = @ListItemKey
	
	SET @FormattedName = dbo.ufn_FormattedSpeciesName(@ActualName,@Authority,@PreferredAuthority,@PreferredName,@ActualItalic,@PreferredItalic,@Attribute,@RankName,0,'')
	

	RETURN @FormattedName
END