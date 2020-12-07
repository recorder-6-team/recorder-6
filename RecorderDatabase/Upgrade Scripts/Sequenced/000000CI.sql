/*****Changes following RC issue. With Redundant taxa. Display the Recommended name as an option ****/

/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Species_Select]    Script Date: 09/18/2020 18:05:51 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Returns details matches where  one or more 
      possible match is acceptable 

  Parameters: @Key TaxonName 

  Created:	November 2018 
   
    
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWNotes_Species_Select]
@Key varCHAR(75)
AS
  Declare @MatchedCount integer,@Remembered bit, @ManualMatch bit,
  @OutputName varchar(100)
  Select @MatchedCount = Match_Count from #Species where Import_Value = @Key   
  Select @Remembered = Remembered from #Species where Import_Value = @Key   
  Select @ManualMatch =  Manual_Match from #Species where Import_Value = @Key   
  Select @OutputName =  Species_Name from #Species where Import_Value = @Key   


  IF @MatchedCount = 0 Or  @Remembered = 1 Or @ManualMatch = 1 
  BEGIN
    SELECT	
    ITN.TAXON_LIST_ITEM_KEY AS AKey,	
    LTRIM(dbo.ufn_GetDeprecated(ITN.DEPRECATED,ITN.Allow_Data_Entry)+ ' ') +
    [dbo].[ufn_GetFullSpeciesName] (ITN.TAXON_LIST_ITEM_KEY) +
    ' (' + TG.TAXON_GROUP_NAME + ')'  
    AS FullDetails,ISNULL(#Species.Match_Key,'') As MatchKey, OUTPUT_TAXON_NAME, 1 AS POSSIBLE, 
    TG.Taxon_Group_Name   
    FROM INDEX_TAXON_NAME ITN INNER JOIN
    TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
    INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY   
    INNER JOIN #Species ON #Species.Import_Value = @Key
    WHERE (OUTPUT_TAXON_NAME = @OutputName AND PREFERRED_TAXA > 0)
    OR (#Species.MATCH_KEY = ITN.TAXON_LIST_ITEM_KEY) 
   UNION SELECT
    ITN.TAXON_LIST_ITEM_KEY AS AKey,	
    '---- ' + LTRIM(dbo.ufn_GetDeprecated(ITN.DEPRECATED,ITN.Allow_Data_Entry)+ ' ') +
    [dbo].[ufn_GetFullSpeciesName] (ITN.TAXON_LIST_ITEM_KEY) +
    ' (' + TG.TAXON_GROUP_NAME + ')'  
    AS FullDetails,ISNULL(#Species.Match_Key,'') As MatchKey, OUTPUT_TAXON_NAME, 0 AS POSSIBLE, 
    TG.Taxon_Group_Name   
    FROM INDEX_TAXON_NAME ITN INNER JOIN
    TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
    INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY   
    INNER JOIN #Species ON #Species.Import_Value = @Key
    WHERE (OUTPUT_TAXON_NAME  LIKE(LEFT(@OutputName ,CHARINDEX(' ',@OutputName + ' ')-1)+'%') OR
    OUTPUT_TAXON_NAME  LIKE('%' + RIGHT(@OutputName,LEN(@OutputName) - CHARINDEX(' ',@OutputName))))
    AND PREFERRED_TAXA > 0
    ORDER BY POSSIBLE DESC, TAXON_GROUP_NAME, OUTPUT_TAXON_NAME
  END
  ELSE
  BEGIN
    SELECT	
    ITN.TAXON_LIST_ITEM_KEY AS AKey,	
    LTRIM(dbo.ufn_GetDeprecated(ITN.DEPRECATED,ITN.Allow_Data_Entry)+ ' ') +
    [dbo].[ufn_GetFullSpeciesName] (ITN.TAXON_LIST_ITEM_KEY) +
    ' (' + TG.TAXON_GROUP_NAME + ')'  
    AS FullDetails,ISNULL(#Species.Match_Key,'') As MatchKey, OUTPUT_TAXON_NAME, 1 AS POSSIBLE, 
    TG.Taxon_Group_Name   
    FROM INDEX_TAXON_NAME ITN INNER JOIN
    TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
    INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY   
    INNER JOIN #Species ON #Species.Import_Value = @Key 
    WHERE OUTPUT_TAXON_NAME = @OutputName AND PREFERRED_TAXA > 0
    UNION SELECT
    ITN2.TAXON_LIST_ITEM_KEY AS AKey,	
    LTRIM(dbo.ufn_GetDeprecated(ITN2.DEPRECATED,ITN2.Allow_Data_Entry)+ ' ') +
    [dbo].[ufn_GetFullSpeciesName] (ITN2.TAXON_LIST_ITEM_KEY) +
    ' (' + TG.TAXON_GROUP_NAME + ')'  
    AS FullDetails,ISNULL(#Species.Match_Key,'') As MatchKey, ITN.OUTPUT_TAXON_NAME, 1 AS POSSIBLE, 
    TG.Taxon_Group_Name   
    FROM INDEX_TAXON_NAME ITN INNER JOIN
    TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
    INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY   
    INNER JOIN INDEX_TAXON_NAME ITN2 on ITN2.TAXON_LIST_ITEM_KEY = 
    ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY 
    INNER JOIN #Species ON #Species.Import_Value = @Key 
    WHERE ITN.OUTPUT_TAXON_NAME = @OutputName AND ITN.PREFERRED_TAXA > 0
    ORDER BY FullDetails 
 
  END

