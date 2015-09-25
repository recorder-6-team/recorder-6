/****** Object:  UserDefinedFunction [dbo].[LCReturnLocalName]    Script Date: 11/14/2011 18:14:42 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCReturnLocalName]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCReturnLocalName]
GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO
/****** Object:  UserDefinedFunction [dbo].[LCReturnLocalName]    Script Date: 11/20/2012 22:31:16 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Function to return the Recommended Common Name or the Taxon Group where the Common Name is the scientific name 
         	
  Parameters:
		@TLI Key  Taxon List Item Key
                
                

  Created:	October 2012
  Author: MikeWeideli 

\*=========================================================================== */


CREATE FUNCTION [dbo].[LCReturnLocalName]
(@TLI char(16) )
RETURNS  varchar(100)


AS
BEGIN
Declare @CommonName varchar(100)
Declare @TaxonGroup varchar(50)


SET @TaxonGroup  =  (Select Taxon_Group_Name from Taxon_Group TG
INNER JOIN Taxon_version TV ON TV.Output_Group_Key = TG.Taxon_Group_Key
INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_Version_Key = TV.Taxon_version_Key
WHERE TLI.Taxon_List_Item_Key = @TLI)

SET @CommonName =
(Select DISTINCT T.Item_Name
From Taxon_List_Item TLI INNER
JOIN INDEX_Taxon_Name ITN ON ITN.Taxon_List_Item_key = TLI.Taxon_LIst_Item_Key 
INNER JOIN Taxon_Common_Name TCN
ON TCN.Taxon_List_Item_Key = ITN.Recommended_Taxon_List_Item_Key
INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = TCN.Taxon_Version_Key
INNER JOIN TAXON T ON T.Taxon_Key = TV.Taxon_Key
WHERE T.Language <> 'la' AND TLI.Taxon_List_Item_Key = @TLI) 

If @CommonName is Null
BEGIN
	SET @CommonName =
	(Select DISTINCT TUN.Item_Name
	From Taxon_List_Item TLI 
	INNER JOIN INDEX_Taxon_Name ITN ON ITN.Taxon_List_Item_key = TLI.Taxon_LIst_Item_Key 
	INNER JOIN TAXON_USER_NAME TUN ON TUN.Taxon_List_Item_Key = ITN.Recommended_Taxon_List_Item_Key
	WHERE TUN.Language <> 'la' AND TLI.Taxon_List_Item_Key = @TLI) 
END

If  @CommonName is Null  SET @CommonName = @TaxonGroup

Return @CommonName


END

GO

GRANT EXECUTE ON [dbo].[LCReturnLocalName] TO PUBLIC 