/****** Fixes an issue with SQL SERVER 2017 ******/
/****** Object:  StoredProcedure [dbo].[usp_Index_Taxon_Designation_Rebuild]    Script Date: 18/04/2018 20:04:30 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:      
                    Procedure which rebuilds the indexed list of Taxon Designations for
                    the supplied list of Taxon_List_Keys. Includes special processing to include Chiroptera 
                         

  Created:    January 2009

  Updated April 2018. SQl Server 2017 was taking forever to process the Chiroptera.  


\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_Index_Taxon_Designation_Rebuild]
AS
       DECLARE       @KeyList      VARCHAR(250)
       
       SET          @KeyList      =      ''
       
       SELECT @KeyList      =      Data
       FROM   Setting
       WHERE  Name         =      'TaxDesList'

       DELETE FROM   Index_Taxon_Designation

       INSERT INTO   Index_Taxon_Designation    (
                           Taxon_List_Item_Key,
                           Taxon_Designation_Type_Key )
       SELECT       ITN.Taxon_List_Item_Key,
                           TDES.Taxon_Designation_Type_Key
       FROM         Index_Taxon_Name                                      ITN
       INNER JOIN    Index_Taxon_Name                                      ITN2
                    ON     ITN.Recommended_Taxon_List_Item_Key             =       ITN2.Recommended_Taxon_List_Item_Key
       INNER JOIN    Index_Taxon_Group                                     ITG
                    ON     ITG.Contained_List_Item_Key                    =       ITN2.Taxon_List_Item_Key
       INNER JOIN    Index_Taxon_Name                                      ITN3
                    ON     ITN3.Taxon_List_Item_Key                       =       ITG.Taxon_List_Item_Key
       INNER JOIN    Index_Taxon_Name                                      ITN4
                    ON     ITN4.Recommended_Taxon_List_Item_Key     =       ITN3.Recommended_Taxon_List_Item_Key
       INNER JOIN    Taxon_Designation                                     TDES
                    ON     TDES.Taxon_List_Item_Key                       =       ITN4.Taxon_List_Item_Key
       INNER JOIN    Taxon_List_Version                                    TLV
                    ON     TLV.Taxon_List_Version_Key                     =       ITN4.Taxon_List_Version_Key

       WHERE        (@Keylist     LIKE '%' + TLV.Taxon_List_Key + '%'
                                 OR     @Keylist      =      ''
                                 OR     TDES.System_Supplied_Data  =      0)
                    AND    TDES.Date_To IS NULL
       GROUP BY      ITN.TAXON_LIST_ITEM_KEY,
                           TDES.TAXON_DESIGNATION_TYPE_KEY
       INSERT INTO Index_Taxon_Designation
   


 SELECT  D.Taxon_List_Item_Key, D.Taxon_Designation_Type_Key FROM 
    (SELECT Distinct iTN3.Taxon_List_Item_Key, ITD.Taxon_Designation_Type_Key from  Index_taxon_Name ITN
    INNER JOin Index_taxon_Group ITG ON ITG.Contained_List_Item_Key = ITN.Recommended_Taxon_List_Item_Key
    INNER JOIn INdex_Taxon_name ITN2 On ITN2.Taxon_List_Item_Key = ITG.Taxon_List_Item_Key AND ITN2.Actual_Name =     'Chiroptera'
    INNER JOIN Index_Taxon_Group ITG2 ON ITG2.Contained_List_Item_Key = ITn.Taxon_List_Item_Key
    INNER JOiN Index_Taxon_Designation ITD ON ITD.Taxon_List_Item_key = ITN.Taxon_List_Item_Key 
    INNER JOIN Index_taxon_Group ITG3 ON ITG3.Contained_List_Item_Key = ITN.Taxon_List_Item_Key
    INNER JOIN Index_taxon_Name ITN3 ON ITN3.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITG3.TAXON_LIST_ITEM_KEY) AS D
       INNER JOIN    Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = D.Taxon_List_Item_Key
    INNER JOIN Taxon_Rank TR ON TR.TAXON_RANK_KEY = TLI.TAXON_RANK_KEY AND TR.Sequence > 99  
    AND NOT EXists (Select * FRom Index_taxon_Designation where Index_Taxon_Designation.Taxon_List_Item_Key 
    =D.Taxon_List_Item_Key and Index_Taxon_Designation.Taxon_Designation_Type_Key = D.Taxon_Designation_Type_Key)


