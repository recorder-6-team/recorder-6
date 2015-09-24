/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Biotopes') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatch_Biotopes]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 6 $
    $Date: 26/07/04 14:07 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Biotopes]
	@ChecklistKey char(16)
AS
	-- Set Match_Count first.
	UPDATE	#Biotopes
	SET	Match_Count =  (SELECT Count(*) 
				FROM Biotope B
				JOIN Biotope_List_Item BLI ON BLI.Biotope_Key = B.Biotope_Key
				JOIN Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
				WHERE	BLI.BT_CL_Version_To IS NULL
				AND 	BCV.Biotope_Classification_Key = @ChecklistKey
				AND	(Import_Value = B.Short_Term
				OR	 Import_Value = B.Full_Term
				OR	 Import_Value = B.Original_Code
				OR	 Import_Value = B.Original_Code + ', ' + B.Short_Term
				OR	 Import_Value = B.Original_Code + '-' + B.Short_Term
				OR	 Import_Value = B.Original_Code + '_' + B.Short_Term)
				)
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
	UPDATE	#Biotopes
	SET	Match_Key = Biotope_List_Item_Key,
		Match_Value = dbo.ufn_GetFormattedBiotopeName(Biotope_List_Item_Key),
		Classification = BC.Short_Name,
		Classification_Key = BC.Biotope_Classification_Key
	FROM	Biotope B
	JOIN	Biotope_List_Item BLI ON B.Biotope_Key = BLI.Biotope_Key
	JOIN	Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
	JOIN	Biotope_Classification BC ON BC.Biotope_Classification_Key = BCV.Biotope_Classification_Key
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	BLI.BT_CL_Version_To IS NULL
	AND	BC.Biotope_Classification_Key = @ChecklistKey
	AND 	(Import_Value = B.Short_Term
	OR	 Import_Value = B.Full_Term
	OR	 Import_Value = B.Original_Code
	OR	 Import_Value = B.Original_Code + ', ' + B.Short_Term
	OR	 Import_Value = B.Original_Code + '-' + B.Short_Term
	OR	 Import_Value = B.Original_Code + '_' + B.Short_Term)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_Biotopes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_Biotopes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_Biotopes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_Biotopes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_Biotopes TO [Dev - JNCC SQL]
END
GO