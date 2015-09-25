If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[ufn_FormattedSpeciesName]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function ufn_FormattedSpeciesName'
        DROP FUNCTION [dbo].[ufn_FormattedSpeciesName]
    END
GO

    PRINT 'Creating function ufn_FormattedSpeciesName'
GO

/*===========================================================================*\
 * Description:	Returns the formatted specie Name.
 *
 * Parameters:	@ActualName 		ITN.Actual_Name,
 *				@Authority 			ITN.Authority ,
 *				@PreferredAuthority	ITN.Authority where ITN.Taxon_List_Item_Key = TLI.Preferred_Name,
 *				@PreferredName		ITN.Preferred_Name,
 *				@ActualItalic 		ITN.Actual_Name_Italic,
 *				@PreferredItalic	ITN.Preferred_Name_Italic,
 *				@Attribute			ITN.Attribute,
 *				@RankName			ITN.Short_Name
 *
 * AUTHOR:	Qing Sun, Dorset Software
 * CREATED: 25/11/2008
 *
\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_FormattedSpeciesName]
( @ActualName 			VARCHAR(100),
  @Authority 			VARCHAR(100),
  @PreferredAuthority	VARCHAR(100),
  @PreferredName		VARCHAR(100),
  @ActualItalic 		BIT,
  @PreferredItalic		BIT,
  @Attribute			VARCHAR(100),
  @RankName				VARCHAR(100))
RETURNS varchar(200)
AS

BEGIN
	DECLARE	@FormattedName 	VARCHAR(200)
		
	SET 	@FormattedName = ''
	
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

	RETURN @FormattedName
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_FormattedSpeciesName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function [ufn_FormattedSpeciesName]'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        GRANT EXECUTE ON dbo.[ufn_FormattedSpeciesName] TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.[ufn_FormattedSpeciesName] TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.[ufn_FormattedSpeciesName] TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.[ufn_FormattedSpeciesName] TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.[ufn_FormattedSpeciesName] TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        GRANT EXECUTE ON dbo.[ufn_FormattedSpeciesName] TO [Dev - JNCC SQL]
	END
GO
