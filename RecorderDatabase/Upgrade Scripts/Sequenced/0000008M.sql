/****** Object: Delete UserDefinedFunction [dbo].[LCReturnHierarchy]    Script Date: 08/22/2012 08:22:22 ******/

GO


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCReturnHierarchy]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCReturnHierarchy] 

Go
/****** Object:  UserDefinedFunction [dbo].[LCReturnHierarchyLevel]    Script Date: 12/01/2012 12:00:37 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCReturnHierarchyLevel]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCReturnHierarchyLevel] 

GO


SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Function to return lavel in the Taxonomic hierarachy given a TLI key  
  Parameters:
		@TLiKey - Taxon_List_Item key 
        @ReturnLevel Required (K,P,C,O,F)        
              
  Created:	NOv 2012
  Author: MikeWeideli

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCReturnHierarchyLevel]
(@TLIkey char(16), @ReturnLevel char(1))

RETURNS  varchar(100)

AS
BEGIN
--****************************************************************************************************
Declare @Return varchar(100)
SELECT @RETURN = T2.ITEM_NAME FROM
Index_Taxon_Name ITN 
INNER JOIN TAXON_LIST_ITEM TLI 
ON TLI.TAXON_LIST_ITEM_KEY = ITN.Recommended_Taxon_List_Item_Key AND ITN.System_Supplied_Data = 1
INNER JOIN Index_Taxon_Hierarchy ITH 
ON ITH.Recommended_Taxon_Version_Key = TLI.TAXON_VERSION_KEY 
INNER JOIN TAXON_VERSION TV2 ON TV2.Taxon_Version_key = ITH.Hierarchy_Taxon_Version_key  
INNER JOIN TAXON T2 ON T2.Taxon_Key = TV2.Taxon_key WHERE ITH.Hierarchy_Type = @ReturnLevel
AND ITN.TAXON_LIST_ITEM_KEY = @TLIKey  



RETURN   @Return



END

GO

GRANT EXECUTE ON [dbo].[LCReturnHierarchyLevel] TO PUBLIC
