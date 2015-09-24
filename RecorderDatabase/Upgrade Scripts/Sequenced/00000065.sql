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

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedSpeciesName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedSpeciesName
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
CREATE FUNCTION [dbo].[ufn_GetFormattedSpeciesName]
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
	
	SET @FormattedName = dbo.ufn_FormattedSpeciesName(@ActualName,@Authority,@PreferredAuthority,@PreferredName,@ActualItalic,@PreferredItalic,@Attribute,@RankName)
	

	RETURN @FormattedName
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedSpeciesName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
BEGIN
   	PRINT 'Setting up security on function ufn_GetFormattedSpeciesName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.ufn_GetFormattedSpeciesName TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWCheckRecoverableMatches_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWCheckRecoverableMatches_Species]
GO

/*===========================================================================*\
  Description:
	Checks whether there are any recoverable matches for this checklist.

  Parameters:
	@ChecklistKey	The checklist which the species are being recovered from.
	@UserID			The Name_Key of the current user.
	@FoundMatches	An output boolean which is set to true if matches are found,
					and false otherwise.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWCheckRecoverableMatches_Species]
	@ChecklistKey CHAR(16) = NULL,
	@UserID CHAR(16),
	@FoundMatches BIT OUT
AS
	If EXISTS (
		SELECT	Temp_User_ID
		FROM	IW_Matched_Species
		LEFT JOIN #Species
			ON	(Checklist_Key	=	Match_Checklist_Key
				OR (Checklist_Key IS NULL AND Match_Checklist_Key IS NULL))
			AND	Match_Key		=	Matched_Key
			AND Import_Value	=	Matched_Value
		WHERE	Temp_User_ID	=	@UserID
			AND	(Match_Checklist_Key = @ChecklistKey
					OR	(Match_Checklist_Key IS NULL AND @ChecklistKey IS NULL))
			AND	Match_Key IS NULL -- Don't want matches which are already displayed.
	)  
		SET @FoundMatches = 1
	ELSE
		SET @FoundMatches = 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWCheckRecoverableMatches_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWCheckRecoverableMatches_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWCheckRecoverableMatches_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWCheckRecoverableMatches_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWCheckRecoverableMatches_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWCheckUnconfirmedMatches_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWCheckUnconfirmedMatches_Species]
GO

/*===========================================================================*\
  Description:
	Checks whether there are any unconfirmed matches for this checklist.

  Parameters:
	@UserID			The Name_Key of the current user.
	@FoundMatches	An output boolean which is set to true if matches are found,
					and false otherwise.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWCheckUnconfirmedMatches_Species]
	@UserID CHAR(16),
	@FoundMatches BIT OUT
AS
	If EXISTS (
		SELECT	Temp_User_ID
		FROM	IW_Matched_Species
		LEFT JOIN #Species
			ON	(Checklist_Key	=	Match_Checklist_Key
				OR (Checklist_Key IS NULL AND Match_Checklist_Key IS NULL))
			AND	Match_Key		=	Matched_Key
			AND Import_Value	=	Matched_Value
		WHERE	Temp_User_ID	=	@UserID
			AND	Match_Key IS NOT NULL -- Only want temporary matches that are currently displayed
	)  
		SET @FoundMatches = 1
	ELSE
		SET @FoundMatches = 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWCheckUnconfirmedMatches_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWCheckUnconfirmedMatches_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWCheckUnconfirmedMatches_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWCheckUnconfirmedMatches_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWCheckUnconfirmedMatches_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchClear_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchClear_Species]
GO

/*===========================================================================*\
  Description:
	Deletes all temporary matches for the current user.

  Parameters:
	@UserID			The ID of the current user.
	@ChecklistKey	The key of the current checklist (null for all checklists).

  Created:	January	2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_IWMatchClear_Species]
	@UserID CHAR(16),
	@ChecklistKey CHAR(16) = NULL
AS
	DELETE FROM	IW_Matched_Species
	WHERE		Temp_User_ID	=	@UserID
			AND	(Match_Checklist_Key = @ChecklistKey
			OR	@ChecklistKey IS NULL)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchClear_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchClear_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchClear_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchClear_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchClear_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_Species]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:
	@UserID			The ID of the current User.

  Created:	July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_Species]
	@UserID			char(16)
AS
	-- Removes existing matches which have a replacement.
	DELETE		MS1
	FROM		IW_Matched_Species		MS1
	INNER JOIN	IW_Matched_Species		MS2
			ON	MS2.Temp_User_ID		=	@UserID
			AND	MS1.Match_Checklist_Key	=	MS2.Match_Checklist_Key
			AND	MS1.Matched_Value		=	MS2.Matched_Value
	WHERE		MS1.Temp_User_ID		IS	NULL
	
	-- Makes the temporary changes this user made permenant.
	UPDATE	IW_Matched_Species
	SET		Temp_User_ID	=	NULL
	WHERE	Temp_User_ID	=	@UserID
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecovered_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecovered_Species]
GO

/*===========================================================================*\
  Description:
	Restores any temporary species matches left over from a previous session.

  Parameters:
	@ChecklistKey	The checklist which the species are being recovered from.
	@UserID			The Name_Key of the current user.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecovered_Species]
	@ChecklistKey CHAR(16),
	@UserID CHAR(16)
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
	AND		Temp_User_ID = @UserID
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecovered_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecovered_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecovered_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecovered_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecovered_Species TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_Species]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	
		@ImportValue	The name of the species.
		@MatchKey		The key of the Taxon_List_Item that the species is
						being matched to.
		@UserID			The ID of the current user.

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_Species]
	@ImportValue varchar(100),
	@MatchKey CHAR(16),
	@UserID CHAR(16),
	@ChecklistKey CHAR(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Order varchar(100), 
			@Checklist varchar(100),
			@MatchValue varchar(100)

		-- Key value for rank 'Order' is 'NBNSYS..12'. 
		SELECT	@Order = ITN.Actual_Name
		FROM	Index_Taxon_Synonym ITS 
		JOIN	Index_Taxon_Group ITG ON ITG.Contained_List_Item_Key = ITS.Synonym_List_Item_Key
		JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key 
		WHERE	ITS.Taxon_List_Item_Key = @MatchKey
		AND	TLI.Taxon_Rank_Key = 'NBNSYS0000000012' 
	
		-- 'Not available' value handled and regionalised in app.
		IF @Order IS NULL SET @Order = 'Not available'
	
		-- Get the associated checklist.
		SELECT	@Checklist = TL.Item_Name
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	ITN.Taxon_List_Item_Key = @MatchKey

		SET	@MatchValue =	dbo.ufn_GetFormattedSpeciesName(@MatchKey)

		-- And update match table.
		UPDATE	#Species
		SET	Match_Value = @MatchValue,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			[Order] = @Order,
			Checklist = @Checklist,
			Checklist_Key = @ChecklistKey
		WHERE	Import_Value = @ImportValue

		-- Remove any previous temporary matches this user made of this value.
		DELETE FROM IW_Matched_Species
		WHERE	Matched_Value		=	@ImportValue
			AND	Temp_User_ID		=	@UserID
		
		-- Inserts the temporary match.
		INSERT INTO IW_Matched_Species (
			Matched_Value,
			Matched_Key,
			Match_Checklist_Key,
			Temp_User_ID
		) VALUES (
			@ImportValue,
			@MatchKey,
			@ChecklistKey,
			@UserID
		)
	END ELSE BEGIN
		UPDATE	#Species
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			[Order] = NULL,
			Checklist = NULL,
			Checklist_Key = NULL
		WHERE	Import_Value = @ImportValue
		
		-- Remove any previous temporary matches this user made of this value.
		DELETE FROM IW_Matched_Species
		WHERE	Matched_Value		=	@ImportValue
			AND	Temp_User_ID		=	@UserID
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_Species]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Species]
	@ChecklistKey char(16)
AS
	-- Setup new column with reworked Import_Value, hopefully that will speed things up a bit.
	UPDATE	#Species
	SET	Species_Name = 
		CASE	WHEN Right(Import_Value, 2) = 'sp' THEN Left(Import_Value, Len(Import_Value) - 2)
			WHEN Right(Import_Value, 3) = 'sp.' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 3) = 'spp' THEN Left(Import_Value, Len(Import_Value) - 3)
			WHEN Right(Import_Value, 4) = 'spp.' THEN Left(Import_Value, Len(Import_Value) - 4)
			ELSE Import_Value
		END

	IF @ChecklistKey IS NULL
	BEGIN
		-- Handle searches against the virtual preferred list

		-- Set Match_Count first. Broken down in two separate updates for speed.
		UPDATE	UpdatedSpecies
		SET	Match_Count =  (SELECT	Count(DISTINCT ITN2.Taxon_List_Item_Key)
					FROM	#Species S  
					INNER JOIN Index_Taxon_Name ITN1 ON ITN1.Actual_Name = S.Species_Name
					INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
					WHERE	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
					AND ITN2.Preferred_List = 1
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND TLI.Taxon_List_Version_To IS NULL
					AND TLV.Version >= (SELECT MAX(Version)
									FROM Taxon_List_Version 
          							WHERE Taxon_List_Key = TLV.Taxon_List_Key
									AND Version_Is_Amendment = 0))
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL

		-- Set Match_Count for species name + authority test. Note the strange way of doing
		-- this join - it seems to be faster this way!
		UPDATE	UpdatedSpecies
		SET	Match_Count = Match_Count + (SELECT	Count(DISTINCT ITN2.Taxon_List_Item_Key)
					FROM	#Species S  
					INNER JOIN Index_Taxon_Name ITN1 ON LEFT(S.Species_Name, LEN(ITN1.Actual_Name)) = ITN1.Actual_Name
					INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
					WHERE	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
					AND S.Species_Name = ITN1.Actual_Name + ' ' + ITN1.Authority
					AND ITN2.Preferred_List = 1
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND TLI.Taxon_List_Version_To IS NULL
					AND TLV.Version >= (SELECT MAX(Version)
									FROM Taxon_List_Version 
          							WHERE Taxon_List_Key = TLV.Taxon_List_Key
									AND Version_Is_Amendment = 0))
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL


		-- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
		UPDATE	#Species
		SET	Match_Key = ITN2.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN2.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = NULL
		FROM	Index_Taxon_Name ITN1
		JOIN	Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
		AND ITN2.Preferred_List = 1
		AND	Species_Name = ITN1.Actual_Name
		AND TLI.Taxon_List_Version_To IS NULL

		UPDATE	#Species
		SET	Match_Key = ITN2.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN2.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = NULL
		FROM	Index_Taxon_Name ITN1
		JOIN	Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key	
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
		AND ITN2.Preferred_List = 1
		AND	Species_Name = ITN1.Actual_Name + ' ' + ITN1.Authority
		AND TLI.Taxon_List_Version_To IS NULL

	END ELSE
	BEGIN
		-- Handle searches against a specified list

		-- Set Match_Count first. Broken down in two separate updates for speed.
		UPDATE	UpdatedSpecies
		SET	Match_Count =  (SELECT	Count(*)
					FROM	#Species S 
					INNER JOIN Index_Taxon_Name ITN ON ITN.Actual_Name = S.Species_Name 
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key					
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
					WHERE	TLV.Taxon_List_Key = @ChecklistKey
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND TLI.Taxon_List_Version_To IS NULL
					AND TLV.Version >= (SELECT MAX(Version)
									FROM Taxon_List_Version 
          							WHERE Taxon_List_Key = TLV.Taxon_List_Key
									AND Version_Is_Amendment = 0))
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL

		UPDATE	UpdatedSpecies
		SET	Match_Count = Match_Count + (SELECT	Count(*)
					FROM	#Species S 
					INNER JOIN Index_Taxon_Name ITN ON ITN.Actual_Name + ' ' + ITN.Authority = S.Species_Name 
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key					
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
					WHERE	TLV.Taxon_List_Key = @ChecklistKey
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND TLI.Taxon_List_Version_To IS NULL
					AND TLV.Version >= (SELECT MAX(Version)
									FROM Taxon_List_Version 
          							WHERE Taxon_List_Key = TLV.Taxon_List_Key
									AND Version_Is_Amendment = 0))
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL


		-- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
		UPDATE	#Species
		SET	Match_Key = ITN.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TL.Taxon_List_Key = @ChecklistKey
		AND	Species_Name = Actual_Name
		AND TLI.Taxon_List_Version_To IS NULL

		UPDATE	#Species
		SET	Match_Key = ITN.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key	
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TL.Taxon_List_Key = @ChecklistKey
		AND	Species_Name = Actual_Name + ' ' + ITN.Authority
		AND TLI.Taxon_List_Version_To IS NULL
	END

	-- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
	UPDATE	S
	SET	[Order] = ITN.Actual_Name
	FROM	#Species	S
	JOIN	Index_Taxon_Synonym ITS ON ITS.Synonym_List_Item_Key = S.Match_Key
	JOIN	Index_Taxon_Group ITG ON ITS.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
	JOIN	Taxon_List_Item TLI 
		ON	ITG.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
		AND TLI.Taxon_Rank_Key = 'NBNSYS0000000012'
	JOIN	Index_Taxon_Name ITN
		ON	ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
	WHERE Match_Count = 1
	AND	[Order] IS NULL

	-- Order field can still be null, so deal with that. 'Not available' value handled and regionalised in app.
	UPDATE	#Species
	SET	[Order] = 'Not available'
	WHERE	Match_Count = 1
	AND	[Order] IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	
		@SearchKey	Taxon List key
		@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch]
	@SearchKey 	CHAR(16) = NULL,
	@SearchText VARCHAR(100)
AS
	IF @SearchKey IS NULL
		SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
				dbo.ufn_FormattedSpeciesName(	ITN.Actual_Name,
												ITN.Authority,
												ITN2.Authority,
												ITN.Preferred_Name, 
												ITN.Actual_Name_Italic,
												ITN.Preferred_Name_Italic,
												TV.Attribute,
												TR.Short_Name ) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '')
				+ ISNULL(' - ' + TL.Item_Name, '')                       AS DisplayTerm,
				ITN.Actual_Name 
				+ ISNULL(' ' + ITN.Authority, '') 
				+ ISNULL(' - ' + TL.Item_Name, '')                       AS SearchTerm

		FROM	Index_Taxon_Name 	ITN
		INNER JOIN 	Taxon_List_Version 	TLV ON TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
		INNER JOIN 	Taxon_List 			TL 	ON TL.Taxon_List_Key 			= TLV.Taxon_List_Key
		INNER JOIN	Taxon_List_Item 	TLI ON TLI.Taxon_List_Item_Key 		= ITN.Taxon_List_Item_Key
		INNER JOIN	Taxon_Version		TV	ON	TV.Taxon_Version_Key		= TLI.Taxon_Version_Key
		INNER JOIN	Taxon_Rank			TR	ON	TR.Taxon_Rank_Key			= TLI.Taxon_Rank_Key
		INNER JOIN	Index_Taxon_Name	ITN2  ON ITN2.Taxon_List_Item_Key	= TLI.Preferred_Name
		
		WHERE 	(ITN.Actual_Name 						LIKE @SearchText + '%'
		OR		 ITN.Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText + '%'
		OR 		 ITN.Authority 							LIKE @SearchText + '%')
		AND		 TLI.Taxon_List_Version_To 				IS NULL
		AND 	 TLV.Version >= (
					SELECT 	MAX(Version) FROM Taxon_List_Version 
					WHERE 	Taxon_List_Key = (SELECT Taxon_List_Key FROM Taxon_List_Version WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
					AND		Version_Is_Amendment = 0)
		ORDER BY SearchTerm
	ELSE
		SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
				dbo.ufn_FormattedSpeciesName(	ITN.Actual_Name,
												ITN.Authority,
												ITN2.Authority,
												ITN.Preferred_Name, 
												ITN.Actual_Name_Italic,
												ITN.Preferred_Name_Italic,
												TV.Attribute,
												TR.Short_Name ) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '')            AS DisplayTerm,
				ITN.Actual_Name 
				+ ISNULL(' ' + ITN.Authority, '')                        AS SearchTerm

		FROM	Index_Taxon_Name 	ITN
		INNER JOIN 	Taxon_List_Version 	TLV ON	TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key 
										AND TLV.Taxon_List_Key 			= @SearchKey
		INNER JOIN	Taxon_List_Item 	TLI ON 	TLI.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key
		INNER JOIN	Taxon_Version		TV	ON	TV.Taxon_Version_Key		= TLI.Taxon_Version_Key
		INNER JOIN	Taxon_Rank			TR	ON	TR.Taxon_Rank_Key			= TLI.Taxon_Rank_Key
		INNER JOIN	Index_Taxon_Name	ITN2  ON ITN2.Taxon_List_Item_Key	= TLI.Preferred_Name
		
		WHERE 	(ITN.Actual_Name 						LIKE @SearchText + '%'
		OR		 ITN.Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText + '%'
		OR 		 ITN.Authority 							LIKE @SearchText + '%')
		AND		 TLI.Taxon_List_Version_To 				IS NULL
		AND 	 TLV.Version >= (
					SELECT 	MAX(Version) FROM Taxon_List_Version
					WHERE 	Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
					AND 	Version_Is_Amendment = 0)
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string.

  Parameters:	@SearchText

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearchByList]
	@SearchKey 	CHAR(16),
	@SearchText VARCHAR(100)
AS
	SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
			dbo.ufn_FormattedSpeciesName(	ITN.Actual_Name,
												ITN.Authority,
												ITN2.Authority,
												ITN.Preferred_Name, 
												ITN.Actual_Name_Italic,
												ITN.Preferred_Name_Italic,
												TV.Attribute,
												TR.Short_Name ) 
			+ ISNULL(' - [' + ITN.Abbreviation + ']', '')            AS DisplayTerm, 
			ITN.Actual_Name + ' ' 
			+ ISNULL(ITN.Authority, '')                              AS SearchTerm
	FROM	Index_Taxon_Name 	ITN
	INNER JOIN 	Taxon_List_Version 	TLV ON 	TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key 
									AND TLV.Taxon_List_Key 			= @SearchKey
	INNER JOIN	Taxon_List_Item 	TLI ON 	TLI.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key
    INNER JOIN	Taxon_Version		TV	ON	TV.Taxon_Version_Key		= TLI.Taxon_Version_Key
	INNER JOIN	Taxon_Rank			TR	ON	TR.Taxon_Rank_Key			= TLI.Taxon_Rank_Key
	INNER JOIN	Index_Taxon_Name	ITN2  ON ITN2.Taxon_List_Item_Key	= TLI.Preferred_Name
		
	
	WHERE 	(ITN.Actual_Name 						LIKE @SearchText + '%'
	OR		ITN.Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText + '%'
	OR 		ITN.Authority 						LIKE @SearchText + '%')
	AND		TLI.Taxon_List_Version_To 			IS NULL
	AND 	TLV.Version >= (SELECT MAX(Version)
		FROM Taxon_List_Version
		WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
		WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
		AND Version_Is_Amendment = 0)
	ORDER BY SearchTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Species_Select_ForSearchByList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList_Partial') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearchByList_Partial]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string,
					using partial taxon matching.

  Parameters:	@SearchText

  Created:	January 2008

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearchByList_Partial]
	@SearchKey 	CHAR(16),
	@SearchText VARCHAR(100)
AS
	SET @SearchText = dbo.[ufn_GetPartialTaxonSearchText](@SearchText)

	SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
			dbo.ufn_FormattedSpeciesName(	ITN.Actual_Name,
												ITN.Authority,
												ITN2.Authority,
												ITN.Preferred_Name, 
												ITN.Actual_Name_Italic,
												ITN.Preferred_Name_Italic,
												TV.Attribute,
												TR.Short_Name ) 
			+ ISNULL(' - [' + ITN.Abbreviation + ']', '')            AS DisplayTerm, 
			ITN.Actual_Name + ' ' 
			+ ISNULL(ITN.Authority, '')                              AS SearchTerm

	FROM	Index_Taxon_Name 	ITN
	INNER JOIN 	Taxon_List_Version 	TLV ON 	TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key 
									AND TLV.Taxon_List_Key 			= @SearchKey
	INNER JOIN	Taxon_List_Item 	TLI ON 	TLI.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key
	INNER JOIN	Taxon_Version		TV	ON	TV.Taxon_Version_Key		= TLI.Taxon_Version_Key
	INNER JOIN	Taxon_Rank			TR	ON	TR.Taxon_Rank_Key			= TLI.Taxon_Rank_Key
	INNER JOIN	Index_Taxon_Name	ITN2  ON ITN2.Taxon_List_Item_Key	= TLI.Preferred_Name
	
	WHERE 	(ITN.Actual_Name 						LIKE @SearchText
	OR		ITN.Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText
	OR 		ITN.Authority 						LIKE @SearchText)
	AND		TLI.Taxon_List_Version_To 			IS NULL
	AND 	TLV.Version >= (SELECT MAX(Version)
		FROM Taxon_List_Version
		WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
		WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
		AND Version_Is_Amendment = 0)
	ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearchByList_Partial') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Species_Select_ForSearchByList_Partial'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearchByList_Partial TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch_Partial') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Species_Select_ForSearch_Partial]
GO

/*===========================================================================*\
  Description:	Returns a list of taxa matching a search string,
					using partial taxon matching.

  Parameters:	@SearchKey
		@SearchText

  Created:	January 2008

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Species_Select_ForSearch_Partial]
	@SearchKey 	CHAR(16) = NULL,
	@SearchText VARCHAR(100)
AS
	SET @SearchText = dbo.[ufn_GetPartialTaxonSearchText](@SearchText)

	IF @SearchKey IS NULL
		SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
				dbo.ufn_FormattedSpeciesName(	ITN.Actual_Name,
												ITN.Authority,
												ITN2.Authority,
												ITN.Preferred_Name, 
												ITN.Actual_Name_Italic,
												ITN.Preferred_Name_Italic,
												TV.Attribute,
												TR.Short_Name ) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '') 
				+ ISNULL(' - ' + TL.Item_Name, '')                       AS DisplayTerm, 
				ITN.Actual_Name 
				+ ISNULL(' ' + ITN.Authority, '') 
				+ ISNULL(' - ' + TL.Item_Name, '')                       AS SearchTerm

		FROM	Index_Taxon_Name 	ITN
		INNER JOIN 	Taxon_List_Version 	TLV ON TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
		INNER JOIN 	Taxon_List 			TL 	ON TL.Taxon_List_Key 			= TLV.Taxon_List_Key
		INNER JOIN	Taxon_List_Item 	TLI ON TLI.Taxon_List_Item_Key 		= ITN.Taxon_List_Item_Key
		INNER JOIN	Taxon_Version		TV	ON	TV.Taxon_Version_Key		= TLI.Taxon_Version_Key
		INNER JOIN	Taxon_Rank			TR	ON	TR.Taxon_Rank_Key			= TLI.Taxon_Rank_Key
		INNER JOIN	Index_Taxon_Name	ITN2  ON ITN2.Taxon_List_Item_Key	= TLI.Preferred_Name
		
		WHERE 	(ITN.Actual_Name 						LIKE @SearchText
		OR		ITN.Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText
		OR 		ITN.Authority 						LIKE @SearchText)
		AND		TLI.Taxon_List_Version_To 			IS NULL
		AND 	TLV.Version >= (SELECT MAX(Version)
			FROM Taxon_List_Version
			WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
			WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
			AND Version_Is_Amendment = 0)
		ORDER BY SearchTerm
	ELSE
		SELECT	ITN.Taxon_List_Item_Key                                  AS Item_Key,
				dbo.ufn_FormattedSpeciesName(	ITN.Actual_Name,
												ITN.Authority,
												ITN2.Authority,
												ITN.Preferred_Name, 
												ITN.Actual_Name_Italic,
												ITN.Preferred_Name_Italic,
												TV.Attribute,
												TR.Short_Name ) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '')            AS DisplayTerm, 
				ITN.Actual_Name 
				+ ISNULL(' ' + ITN.Authority, '')                        AS SearchTerm
	
		FROM	Index_Taxon_Name 	ITN
		INNER JOIN 	Taxon_List_Version 	TLV ON 	TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
										AND TLV.Taxon_List_Key 			= @SearchKey
		INNER JOIN	Taxon_List_Item 	TLI ON 	TLI.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key
		INNER JOIN	Taxon_Version		TV	ON	TV.Taxon_Version_Key		= TLI.Taxon_Version_Key
		INNER JOIN	Taxon_Rank			TR	ON	TR.Taxon_Rank_Key			= TLI.Taxon_Rank_Key
		INNER JOIN	Index_Taxon_Name	ITN2  ON ITN2.Taxon_List_Item_Key	= TLI.Preferred_Name
		
		WHERE 	(ITN.Actual_Name 						LIKE @SearchText
		OR		ITN.Actual_Name + ' ' + ITN.Authority 	LIKE @SearchText
		OR 		ITN.Authority 						LIKE @SearchText)
		AND		TLI.Taxon_List_Version_To 			IS NULL
		AND 	TLV.Version >= (SELECT MAX(Version)
			FROM Taxon_List_Version
			WHERE Taxon_List_Key = (SELECT Taxon_List_Key From Taxon_List_Version
			WHERE Taxon_List_Version_Key = TLV.Taxon_List_Version_Key)
			AND Version_Is_Amendment = 0)
		ORDER BY SearchTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Species_Select_ForSearch_Partial') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Species_Select_ForSearch_Partial'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Species_Select_ForSearch_Partial TO [Dev - JNCC SQL]
END
GO

