/****** Changes to allow Biotope List Item key to be used by Import Wizard  ******/

/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Biotopes]    Script Date: 12/19/2016 16:29:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatch_Biotopes]
	@ChecklistKey char(16)
AS
   UPDATE	#Biotopes
		SET	Match_Key = BLI.Biotope_List_Item_Key,
			Match_Value =  BLI.Biotope_List_Item_Key,
			Classification = BC.Short_Name,
		    Classification_Key = @ChecklistKey,
			Match_Count = 1
		FROM	
		   BIOTOPE_LIST_ITEM BLI 
		   INNER JOIN BIOTOPE_CLASSIFICATION_VERSION BCV 
		   ON BCV.BT_CL_VERSION_KEY = BLI.BT_CL_VERSION_KEY
		   INNER JOIN BIOTOPE_CLASSIFICATION BC 
		   ON BC.BIOTOPE_CLASSIFICATION_KEY=BCV.BIOTOPE_CLASSIFICATION_KEY
		
		WHERE	#Biotopes.Import_Value  = BLI.Biotope_List_Item_Key
  
	
	-- Set Match_Count first.
	UPDATE	#Biotopes
	SET	Match_Count =  (SELECT Count(*) 
				FROM Biotope B
				JOIN Biotope_List_Item BLI ON BLI.Biotope_Key = B.Biotope_Key
				JOIN Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
				WHERE	(BLI.BT_CL_Version_To IS NULL
				         OR Import_Value = BLI.BIOTOPE_LIST_ITEM_KEY)
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
	AND	(BLI.BT_CL_Version_To IS NULL OR Import_Value = BLI.BIOTOPE_LIST_ITEM_KEY) 
	AND	BC.Biotope_Classification_Key = @ChecklistKey
	AND 	(Import_Value = B.Short_Term
	OR	 Import_Value = B.Full_Term
	OR	 Import_Value = B.Original_Code
	OR	 Import_Value = B.Original_Code + ', ' + B.Short_Term
	OR	 Import_Value = B.Original_Code + '-' + B.Short_Term
	OR	 Import_Value = B.Original_Code + '_' + B.Short_Term)
