/*****Stop the virtual list table being generated if the Master list is present 
Setting table MasterList name is set to YES or if the setting isn't present then
the Virtual table is not populated ****/
   

/****** Object:  StoredProcedure [dbo].[usp_Index_Taxon_Name_Apply_Preferred_Taxa]    Script Date: 03/10/2020 16:51:11 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	
			Procedure which populates the preferred taxa column 
			and virtual organism 
  Created:	September 2016 Mike Weideli
  Updated:	August 2019             

  
   ===========================================================================*/
ALTER PROCEDURE [dbo].[usp_Index_Taxon_Name_Apply_Preferred_Taxa] 

AS

UPDATE INDEX_TAXON_NAME SET PREFERRED_TAXA = 1 
FROM
INDEX_TAXON_NAME ITN
INNER JOIN TAXON_LIST_VERSION TLV
ON TLV.TAXON_LIST_VERSION_KEY = ITN.TAXON_LIST_VERSION_KEY
INNER JOIN TAXON_LIST TL ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
WHERE DEPRECATED = 0 AND Allow_Data_Entry = 1
AND PRIORITY = (SELECT MIN(Priority) FROM TAXON_LIST
INNER JOIN TAXON_LIST_VERSION ON
TAXON_LIST_VERSION.TAXON_LIST_KEY = TAXON_LIST.TAXON_LIST_KEY 
INNER JOIN INDEX_TAXON_NAME ON INDEX_TAXON_NAME.TAXON_LIST_VERSION_KEY
= TAXON_LIST_VERSION.TAXON_LIST_VERSION_KEY
WHERE DEPRECATED = 0 AND Allow_Data_Entry = 1 
AND INDEX_TAXON_NAME.RECOMMENDED_TAXON_VERSION_KEY = ITN.RECOMMENDED_TAXON_VERSION_KEY
AND  INDEX_TAXON_NAME.OUTPUT_TAXON_NAME = ITN.OUTPUT_TAXON_NAME )

UPDATE INDEX_TAXON_NAME SET PREFERRED_TAXA = -1
FROM INDEX_TAXON_NAME ITN
WHERE PREFERRED_TAXA = 0 AND 
EXISTS (SELECT * FROM INDEX_TAXON_NAME ITN2 WHERE
ITN2.OUTPUT_TAXON_NAME = ITN.OUTPUT_TAXON_NAME 
AND ITN2.RECOMMENDED_TAXON_VERSION_KEY =
ITN.RECOMMENDED_TAXON_VERSION_KEY AND ITN2.PREFERRED_TAXA = 1 )  




UPDATE INDEX_TAXON_NAME SET PREFERRED_TAXA = 2 
FROM
INDEX_TAXON_NAME ITN
INNER JOIN TAXON_LIST_VERSION TLV
ON TLV.TAXON_LIST_VERSION_KEY = ITN.TAXON_LIST_VERSION_KEY
INNER JOIN TAXON_LIST TL ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
WHERE Deprecated = 1 AND Allow_Data_Entry = 1  AND PREFERRED_TAXA = 0 
AND PRIORITY = (SELECT MIN(Priority) FROM TAXON_LIST
INNER JOIN TAXON_LIST_VERSION ON
TAXON_LIST_VERSION.TAXON_LIST_KEY = TAXON_LIST.TAXON_LIST_KEY 
INNER JOIN INDEX_TAXON_NAME ON INDEX_TAXON_NAME.TAXON_LIST_VERSION_KEY
= TAXON_LIST_VERSION.TAXON_LIST_VERSION_KEY
WHERE Deprecated = 1 AND Allow_Data_Entry = 1  AND PREFERRED_TAXA = 0 
AND INDEX_TAXON_NAME.RECOMMENDED_TAXON_VERSION_KEY = ITN.RECOMMENDED_TAXON_VERSION_KEY
AND  INDEX_TAXON_NAME.OUTPUT_TAXON_NAME = ITN.OUTPUT_TAXON_NAME )
 
 

UPDATE INDEX_TAXON_NAME SET PREFERRED_TAXA = -2
FROM INDEX_TAXON_NAME ITN
WHERE PREFERRED_TAXA = 0 AND 
EXISTS (SELECT * FROM INDEX_TAXON_NAME ITN2 WHERE
ITN2.OUTPUT_TAXON_NAME = ITN.OUTPUT_TAXON_NAME 
AND ITN2.RECOMMENDED_TAXON_VERSION_KEY =
ITN.RECOMMENDED_TAXON_VERSION_KEY AND ITN2.PREFERRED_TAXA = 2 )  

UPDATE INDEX_TAXON_NAME SET PREFERRED_TAXA = 3 
FROM
INDEX_TAXON_NAME ITN
INNER JOIN TAXON_LIST_VERSION TLV
ON TLV.TAXON_LIST_VERSION_KEY = ITN.TAXON_LIST_VERSION_KEY
INNER JOIN TAXON_LIST TL ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
WHERE  PREFERRED_TAXA = 0 
AND PRIORITY = (SELECT MIN(Priority) FROM TAXON_LIST
INNER JOIN TAXON_LIST_VERSION ON
TAXON_LIST_VERSION.TAXON_LIST_KEY = TAXON_LIST.TAXON_LIST_KEY 
INNER JOIN INDEX_TAXON_NAME ON INDEX_TAXON_NAME.TAXON_LIST_VERSION_KEY
= TAXON_LIST_VERSION.TAXON_LIST_VERSION_KEY
WHERE PREFERRED_TAXA = 0 
AND INDEX_TAXON_NAME.RECOMMENDED_TAXON_VERSION_KEY = ITN.RECOMMENDED_TAXON_VERSION_KEY
AND  INDEX_TAXON_NAME.OUTPUT_TAXON_NAME = ITN.OUTPUT_TAXON_NAME )
 
 

UPDATE INDEX_TAXON_NAME SET PREFERRED_TAXA = -3
FROM INDEX_TAXON_NAME ITN
WHERE PREFERRED_TAXA = 0 AND 
EXISTS (SELECT * FROM INDEX_TAXON_NAME ITN2 WHERE
ITN2.OUTPUT_TAXON_NAME = ITN.OUTPUT_TAXON_NAME 
AND ITN2.RECOMMENDED_TAXON_VERSION_KEY =
ITN.RECOMMENDED_TAXON_VERSION_KEY AND ITN2.PREFERRED_TAXA = 3 ) 


TRUNCATE TABLE VIRTUAL_ORGANISM 

IF NOT EXISTS (SELECT * FROM SETTING WHERE NAME = 'MasterList' AND DATA = 'NO')  
  EXECUTE [dbo].[usp_Virtual_Organism_Populate] 


UPDATE TAXON_LIST_ITEM
SET TAXON_LIST_ITEM.SORT_CODE = ORGANISM.SORT_CODE
FROM TAXON_LIST_ITEM INNER JOIN ORGANISM ON ORGANISM.TAXON_VERSION_KEY 
= TAXON_LIST_ITEM.TAXON_VERSION_KEY AND TAXON_LIST_ITEM.TAXON_LIST_VERSION_KEY = 
'NHMSYS0020020201'  

 
UPDATE TAXON_LIST_ITEM 
SET TAXON_LIST_ITEM.SORT_CODE = TLI2.SORT_CODE
FROM TAXON_LIST_ITEM 
INNER JOIN TAXON_LIST_ITEM TLI2 ON 
TAXON_LIST_ITEM.PREFERRED_NAME = TLI2.TAXON_LIST_ITEM_KEY 
WHERE TAXON_LIST_ITEM.PREFERRED_NAME <> TLI2.TAXON_LIST_ITEM_KEY  

