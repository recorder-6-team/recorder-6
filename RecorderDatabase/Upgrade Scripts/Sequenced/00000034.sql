SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IndexTaxonName_ApplyNameServer_SingleRecord]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer_SingleRecord]
GO

/*===========================================================================*\
  Description: Applies the NameServer information to the Index_Taxon_Name
		Recommended_Taxon_List_Item_Key table for just the supplied key.

  Parameters:	None

  Created:	November 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/01/06 14:56 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer_SingleRecord]
	@TLIKey CHAR(16)
AS

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = NS.RECOMMENDED_TAXON_LIST_ITEM_KEY
FROM NAMESERVER NS
INNER JOIN TAXON_LIST_ITEM TLI ON NS.INPUT_TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL 
AND ITN.Taxon_List_Item_Key=@TLIKey

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM TAXON_LIST TL 
INNER JOIN TAXON_LIST_VERSION TLV ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
INNER JOIN TAXON_LIST_ITEM TLI ON TLV.TAXON_LIST_VERSION_KEY = TLI.TAXON_LIST_VERSION_KEY
INNER JOIN TAXON_LIST_ITEM TLI1 ON TLI.TAXON_VERSION_KEY = TLI1.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON TLI1.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
WHERE TL.PREFERRED=1 AND ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL
AND ITN.Taxon_List_Item_Key=@TLIKey

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM TAXON_VERSION TV
INNER JOIN TAXON_LIST_ITEM TLI ON TV.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
INNER JOIN TAXON_GROUP TG ON TV.Output_group_key = TG.TAXON_GROUP_KEY
INNER JOIN TAXON_LIST_VERSION TLV ON TLV.TAXON_LIST_KEY=TG.USE_TAXON_LIST_KEY
		AND TLI.TAXON_LIST_VERSION_KEY=TLV.TAXON_LIST_VERSION_KEY
INNER JOIN TAXON_LIST_ITEM AS TLI1 
		ON TLI.TAXON_VERSION_KEY = TLI1.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON TLI1.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL
AND ITN.Taxon_List_Item_Key=@TLIKey

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM INDEX_TAXON_NAME ITN
INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY=ITN.TAXON_LIST_ITEM_KEY
INNER JOIN TAXON_LIST_ITEM TLI2 on TLI2.TAXON_LIST_ITEM_KEY=TLI.PREFERRED_NAME
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL
AND ITN.Taxon_List_Item_Key=@TLIKey

--Now set up the recommended sort orders, which depend on the recommended names

UPDATE ITN
SET ITN.Sort_Order=
	LEFT('000', 3 - LEN(CONVERT(VARCHAR(3), ISNULL(TG.Sort_Order,0))))
  + CONVERT(VARCHAR(3), ISNULL(TG.Sort_Order,0)) 
	+ LEFT('00000000', 8 - LEN(CONVERT(VARCHAR(8), ISNULL(TLI.Sort_Code,0))))
  + CONVERT(VARCHAR(8), ISNULL(TLI.Sort_Code,0)) 
FROM Index_Taxon_Name ITN 
INNER JOIN Taxon_List_Item TLI 
	ON TLI.Taxon_List_Item_Key=ITN.Recommended_Taxon_List_Item_Key
	AND ITN.Taxon_List_Item_Key=@TLIKey
INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key=TLI.Taxon_Version_Key
LEFT JOIN Taxon_Group TG ON TG.Taxon_Group_Key=TV.Output_Group_Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IndexTaxonName_ApplyNameServer_SingleRecord') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IndexTaxonName_ApplyNameServer_SingleRecord'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IndexTaxonName_ApplyNameServer_SingleRecord TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IndexTaxonName_ApplyNameServer_SingleRecord TO [Dev - JNCC SQL]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_IndexTaxonName_ApplyNameServer_SingleRecord TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IndexTaxonName_ApplyNameServer_SingleRecord TO [R2k_FullEdit]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ListSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns List Synonyms

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 20/01/06 14:56 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	/*=============================*\
	  Get the list synonyms.
	\*=============================*/
	SELECT 		CListSynonyms.Concept_Key AS Item_Key,
			IsNull(T.Item_Name + ' ' + TV.Author_And_Date, T.Item_Name) AS Item_Name,
			T.Language_Key,
			L.Item_Name AS Language,
			CListSynonyms.Custodian,
			S.User_Name_Key AS Entered_By
	FROM 		Concept AS CSource
	INNER JOIN	Concept AS CListSynonyms 	ON CListSynonyms.Meaning_Key = CSource.Meaning_Key 
							AND CListSynonyms.Concept_Group_Key = CSource.Concept_Group_Key
							AND CListSynonyms.Concept_Key <> @Key
	INNER JOIN	Term AS T 			ON T.Term_Key = CListSynonyms.Term_Key
	INNER JOIN 	Language AS L			ON L.Language_Key=T.Language_Key
	LEFT JOIN		Term_Version AS TV 		ON TV.Term_Version_Key = CListSynonyms.Term_Version_Key
	LEFT JOIN Session S ON S.Session_ID=CListSynonyms.Entered_Session_ID
	WHERE 		CSource.Concept_key = @Key

	ORDER BY 	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ListSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ListSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEventOwners_Select_ForSurveyEvent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEventOwners_Select_ForSurveyEvent]
GO

/*===========================================================================*\
  Description:	Returns all names for an event's ownership

  Parameters:	@Key	Survey_Event_key

  Created:	Feb 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/01/06 14:56 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventOwners_Select_ForSurveyEvent]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT DISTINCT
  SO.Survey_Event_Owner_Key,
	SO.Name_Key,
	OT.Short_Name,
	SO.Survey_Event_Owner_Type_Key,
	SO.Custodian,
	dbo.ufn_GetFormattedName(Name_Key) as DisplayName,
	SO.Entered_By
FROM Survey_Event_Owner SO
INNER JOIN Survey_Event_Owner_Type OT ON OT.Survey_Event_Owner_Type_Key = SO.Survey_Event_Owner_Type_Key
WHERE Survey_Event_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventOwners_Select_ForSurveyEvent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventOwners_Select_ForSurveyEvent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SurveyEventOwners_Select_ForSurveyEvent TO [Dev - JNCC SQL]
END
GO

