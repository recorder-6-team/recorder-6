/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Substrate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchSet_Substrate]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/07/04 14:37 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_Substrate]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#Substrates
		SET	Match_Value = Short_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		FROM	Substrate
		WHERE	Substrate_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#Substrates
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Substrate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_Substrate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Substrate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Substrate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Substrate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Substrate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Substrate TO [Dev - JNCC SQL]
END
GO