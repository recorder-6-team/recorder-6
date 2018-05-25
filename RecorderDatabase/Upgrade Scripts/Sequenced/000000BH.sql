/****** Changes the way ITN is populated*****/

/****** Object:  StoredProcedure [dbo].[usp_Index_Taxon_Name_Populate]    Script Date: 16/03/2018 08:53:29 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Contains all sql for populating index taxon name 
  
\*===========================================================================*/
CREATE procedure [dbo].[usp_Index_Taxon_Name_Populate]
 ( @stage as integer )

AS

if @stage = 1 or @stage = 0 begin
  TRUNCATE TABLE INDEX_TAXON_NAME 
end
if  @stage = 2 or @stage = 0 begin
  INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key, Actual_Name,Actual_Name_Italic,
  Common_Name_Italic,PREFERRED_NAME_ITALIC,SYSTEM_SUPPLIED_DATA, ABBREVIATION,ACTUAL_NAME_ATTRIBUTE,AUTHORITY)
  SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key,T.Item_Name,0,0,0,1,T.ABBREVIATION,TV.ATTRIBUTE,T.AUTHORITY
  FROM TAXON_LIST_ITEM TLI LEFT JOIN Taxon_version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key 
  LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key
end
if  @stage = 3 or @stage = 0 begin
  UPDATE Index_Taxon_Name Set Common_Name = T.Item_Name,
  COMMON_NAME_ATTRIBUTE = TV.ATTRIBUTE
  FROM Index_Taxon_Name ITN INNER JOIN Taxon_Common_Name  TCN ON TCN.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key 
  INNER JOIN Taxon_Version TV ON TV.TAXON_VERSION_KEY = TCN.TAXON_VERSION_KEY
  INNER JOIN Taxon T ON T.TAXON_KEY = TV.TAXON_KEY
end
if  @stage = 4 or @stage = 0 begin
  UPDATE Index_Taxon_Name Set Preferred_Name = T.Item_Name, 
  Preferred_Name_Attribute = TV.Attribute,
  Preferred_Name_Authority = T.Authority
  FROM Index_Taxon_Name ITN INNER JOIN Taxon_List_Item TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
  INNER JOIN TAXON_LIST_ITEM TLI2 ON TLI2.TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
  INNER JOIN Taxon_Version TV ON TV.TAXON_VERSION_KEY = TLI2.TAXON_VERSION_KEY
  INNER JOIN Taxon T On T.Taxon_key = TV.Taxon_Key
end
if  @stage = 5 or @stage = 0 begin
  UPDATE Index_Taxon_Name Set Actual_Name_Italic = 1
  FROM Index_Taxon_Name ITN INNER JOIN Taxon_List_Item TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
  INNER JOIN Taxon_Version TV ON TV.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
  INNER JOIN Taxon T On T.Taxon_key = TV.Taxon_Key
  INNER JOIN Taxon_Rank TR On TR.TAXON_RANK_KEY = TLI.TAXON_RANK_KEY
  WHERE T.LANGUAGE = 'La' and TR.List_Font_Italic = 1
end 
if  @stage = 6 or @stage = 0 begin
  UPDATE Index_Taxon_Name Set Common_Name_Italic = 1
  FROM Index_Taxon_Name ITN INNER JOIN Taxon_Common_Name  TCN ON TCN.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key 
  INNER JOIN Taxon_List_Item TLI ON TLI.TAXON_LIST_ITEM_KEY = TCN.TAXON_LIST_ITEM_KEY
  INNER JOIN Taxon_Version TV ON TV.TAXON_VERSION_KEY = TCN.TAXON_VERSION_KEY
  INNER JOIN Taxon T ON T.TAXON_KEY = TV.TAXON_KEY
  INNER JOIN Taxon_Rank TR On TR.TAXON_RANK_KEY = TLI.TAXON_RANK_KEY
  WHERE T.LANGUAGE = 'La' and TR.List_Font_Italic = 1 
end 
if  @stage = 7 or @stage = 0 begin
  UPDATE Index_Taxon_Name Set Preferred_Name_Italic = 1 
  FROM Index_Taxon_Name ITN INNER JOIN Taxon_List_Item TLI ON TLI.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
  INNER JOIN TAXON_LIST_ITEM TLI2 ON TLI2.TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
  INNER JOIN Taxon_Version TV ON TV.TAXON_VERSION_KEY = TLI2.TAXON_VERSION_KEY
  INNER JOIN Taxon_Rank TR On TR.TAXON_RANK_KEY = TLI2.TAXON_RANK_KEY
  INNER JOIN Taxon T On T.Taxon_key = TV.Taxon_Key
  WHERE T.LANGUAGE = 'La' and TR.List_Font_Italic = 1 
end
if  @stage = 8 or @stage = 0 begin
  INSERT INTO Index_Taxon_Name ( Taxon_List_Item_Key, Taxon_List_Version_Key, 
  Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, 
  Preferred_Name, Preferred_Name_Italic, System_Supplied_Data)
  SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, 
  TUN.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND TUN.Language = 'La' THEN 1 ELSE 0 END,
  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END, 
  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END,0 
  FROM Taxon_User_Name AS TUN 
  LEFT JOIN Taxon_List_Item AS TLI ON TLI.Taxon_List_Item_Key = TUN.Taxon_List_Item_Key 
  LEFT JOIN Taxon_version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key
  LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key 
  LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key 
  LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key 
  LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name 
  LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key 
  LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key 
  LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key 
  WHERE TLI.Taxon_List_Version_To IS NULL
end
if  @stage = 9 or @stage = 0 begin
  UPDATE Index_Taxon_Name
  SET Common_Name = TUN.Item_Name, 
  Common_Name_Italic = CASE WHEN TR.List_Font_Italic=1 AND TUN.Language='La' THEN 1 ELSE 0 END 
  FROM Index_Taxon_Name ITN 
  INNER JOIN Taxon_User_Name TUN ON TUN.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key 
  INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
  INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = TLI.Taxon_Rank_Key
  WHERE TUN.Preferred = 1
end
if  @stage =10 or @stage = 0 begin
  UPDATE ITN 
  SET ITN.Preferred_List=TL.Preferred, ITN.Allow_Data_Entry=TLT.Allow_Data_Entry 
  FROM Index_Taxon_Name ITN 
  INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
  INNER JOIN Taxon_List TL ON TL.Taxon_List_Key=TLV.Taxon_List_Key 
  INNER JOIN Taxon_List_Type TLT ON TL.Taxon_List_Type_Key=TLT.Taxon_List_Type_Key
end
if  @stage =11 or @stage = 0 begin
  UPDATE ITN
  SET ITN.Preferred_List=1
  FROM TAXON_LIST_ITEM TLI 
  INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY=TLI.TAXON_LIST_ITEM_KEY 
  INNER JOIN TAXON_VERSION TV ON TLI.TAXON_VERSION_KEY = TV.TAXON_VERSION_KEY
  INNER JOIN TAXON_GROUP TG ON TV.OUTPUT_GROUP_KEY = TG.TAXON_GROUP_KEY 
  INNER JOIN TAXON_LIST_VERSION TLV
  ON TLI.TAXON_LIST_VERSION_KEY = TLV.TAXON_LIST_VERSION_KEY 
 AND TG.USE_TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
end
if  @stage =12 or @stage = 0 begin
  UPDATE ITN 
  SET Has_Children=1 
  FROM Index_Taxon_Name ITN 
  INNER JOIN Taxon_List_Item TLIChild 
  ON TLIChild.Parent=ITN.Taxon_List_Item_Key
end
if  @stage =13 or @stage = 0 begin
  execute usp_IndexTaxonName_ApplyNameServer
end
if  @stage =14 or @stage = 0 begin
  execute usp_IndexTaxonName_ApplySorts
end
if  @stage =15 or @stage = 0 begin
  execute usp_Populate_Index_Taxon_Hierarchy
end
if  @stage =16 or @stage = 0 begin
  execute usp_Index_Taxon_Name_Apply_Preferred_Taxa
end 
GO

GRANT EXECUTE ON  [dbo].[usp_Index_Taxon_Name_Populate] To PUBLIC 