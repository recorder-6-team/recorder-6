/****** Object:  UserDefinedFunction [dbo].[LCReturnFullHierarchy]    Script Date: 03/27/2018 08:33:23 */

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/* Description:	Function to return hierarchy of a TLi key 
         	    as a ; separted string 
  Parameters:
		@TLiKey - Taxon_List_Item key 
                
              
  Created:	NOv 2012
  Author: MikeWeideli
*/

ALTER FUNCTION [dbo].[LCReturnFullHierarchy]
(@TLIkey char(16))
RETURNS  varchar(1000)

AS
BEGIN
Declare @sortKey varchar(30)
Declare @return varchar(1000)


Set @sortkey = (SELECT DISTINCT LINEAGE  FROM
ORGANISM ORG INNER JOIN TAXON_LIST_ITEM TLI 
ON TLI.TAXON_VERSION_KEY = ORG.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN 
ON ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY
WHERE ITN.TAXON_LIST_ITEM_KEY = @TLIkey AND
ITN.SYSTEM_SUPPLIED_DATA = 1)
 
SET @Return  = ''  
  
SELECT  @Return = @Return + T.ITEM_NAME + '; '
FROM TAXON T
INNER JOIN TAXON_VERSION TV ON
T.TAXON_KEY = TV.TAXON_KEY
INNER JOIN ORGANISM ORG  
ON ORG.TAXON_VERSION_KEY = TV.TAXON_VERSION_KEY 
INNER JOIN TAXON_RANK TR 
ON TR.TAXON_RANK_KEY = ORG.ORGANISM_RANK_KEY where @SortKey like ORG.Lineage  + '%'
AND ORG.Sort_Level < (SELECT Sort_Level FROM
ORGANISM ORG INNER JOIN TAXON_LIST_ITEM TLI 
ON TLI.TAXON_VERSION_KEY = ORG.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN 
ON ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY
WHERE ITN.TAXON_LIST_ITEM_KEY = @TLIkey)  
ORDER BY ORG.Sort_Level

if len(@Return) > 0 
BEGIN
  set  @RETURN  = left(@RETURN,len(@RETURN)-1)
END



RETURN   @Return



END
