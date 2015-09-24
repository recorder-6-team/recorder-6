/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchSet_Biotope]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchSet_Biotope]
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 5 $
    $Date: 23/07/04 14:37 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_Biotope]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Classification varchar(100),
			@ClassificationKey char(16)

		-- Get the associated checklist.
		SELECT	@ClassificationKey = BC.Biotope_Classification_Key, @Classification = BC.Short_Name
		FROM	Biotope_List_Item BLI
		JOIN	Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
		JOIN	Biotope_Classification BC ON BC.Biotope_Classification_Key = BCV.Biotope_Classification_Key
		WHERE	BLI.Biotope_List_Item_Key = @MatchKey

		-- And update match table.
		UPDATE	#Biotopes
		SET	Match_Value = dbo.ufn_GetFormattedBiotopeName(@MatchKey),
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			Classification = @Classification,
			Classification_Key = @ClassificationKey
		WHERE	Import_Value = @ImportValue
	END ELSE
		UPDATE	#Biotopes
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			Classification = NULL,
			Classification_Key = NULL
		WHERE	Import_Value = @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_Biotope') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchSet_Biotope'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Biotope TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchSet_Biotope TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchSet_Biotope TO [Dev - JNCC SQL]
END
GO