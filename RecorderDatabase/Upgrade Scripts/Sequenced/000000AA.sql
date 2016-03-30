ALTER TABLE [dbo].[INDEX_TAXON_NAME]
ALTER COLUMN [SORT_ORDER] varchar(36)NULL
GO
ALTER TABLE [dbo].[ORGANISM]
ALTER COLUMN [SORT_ORDER] varchar(36)NULL
GO
ALTER TABLE [dbo].[ORGANISM]
ALTER COLUMN [LINEAGE] varchar(40)NULL
GO
/****** Object:  StoredProcedure [dbo].[spPopulateOrganismLineage]    Script Date: 03/03/2016 18:45:36 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Populates Organism Lineage and Sort Order 
  Parameters:
	None

  Created:	October 2012
  March 2016 - Change field length of lineage to 40 
  Author: MikeWeideli

\*=========================================================================== */


ALTER procedure [dbo].[spPopulateOrganismLineage]
  
as
    set nocount on
    Declare @NuSortLevel as integer
    Declare @PrevLevel as integer
    Declare @NuLineage as varchar(2)
    Declare @PrevLineage as integer
   
    Declare @OrganismKey as char(16) 
    Declare @ParentLineage varchar(40)
    Declare @PrevParentKey char(16)
    Declare @CurrentParentKey char(16)
    Declare @CurrentLevel integer
    Declare @PreviousLevel integer  
  
    
    Update Organism SET Lineage = '0', Sort_Order = null,Sort_Level = 0 
  
   
   Update Organism SET Sort_Level = 1 where parent_key is null and Deleted_date is null
       
    Set @PrevLevel = 1
    Set @NuSortLevel = 1
       
    WHILE 1 =1 
    BEGIN
        Set @NuSortLevel = @NuSortLevel + 1
        UPDATE ORG SET Sort_Level = @NUSortLevel
        FROM ORGANISM ORG INNER JOIN ORGANISM ORG1 on ORG1.Organism_Key = ORG.Parent_Key
        WHERE ORG1.Sort_Level = @PrevLevel AND ORG1.Deleted_date IS NUll and  ORG.Deleted_date IS NULl 
        
        iF @@ROWCOUNT = 0  BREAK
             
        Set @PrevLevel = @NuSortLevel
     
    END
       

    DECLARE csrLineage CURSOR
    FOR
    SELECT Org.Organism_Key,Org.Sort_Level,ORg.Parent_Key from Organism ORG 
    INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key = ORG.Taxon_Version_Key
    INNER JOIN Taxon T ON T.Taxon_key =  TV.Taxon_Key
    INNER JOIN Taxon_Rank TR ON TR.Taxon_Rank_Key = ORG.Organism_Rank_Key
    Where Org.Deleted_Date is Null
    ORDER BY Sort_Level,Org.Parent_Key,Sequence,Org.Weight,Item_name
    
    
    OPEN csrLineage
    
        
    FETCH NEXT FROM csrLineage  INTO @OrganismKey,@Currentlevel,@CurrentParentKey
    
    
    SET @ParentLineage = (Select  Lineage FROM ORGANISM WHERE Organism_Key = @CurrentParentKey )
    IF @ParentLineage is null Set @ParentLineage = '0' 
   
    SET @NULineage =  dbo.LCOrgIncrementLineage('0')
    
    IF @@FETCH_STATUS = 0 UPDATE ORGANISM  SET Lineage = RTRIM(@ParentLineage) +  '\' + @NuLineage WHERE ORGANISM_KEY = @OrganismKey
    Set @PrevParentKey = @CurrentParentKey
    SET @PrevLevel = @CurrentLevel   
    
    
    WHILE @@FETCH_STATUS = 0
    BEGIN
	
	  FETCH NEXT FROM csrLineage  INTO @OrganismKey,@Currentlevel,@CurrentParentKey
	  
	 SET @ParentLineage = (Select  Lineage FROM ORGANISM WHERE Organism_Key = @CurrentParentKey )
     IF @ParentLineage is null Set @ParentLineage = '0' 
   
	 
	  IF @PrevParentKey <> @CurrentParentKey or @PrevLevel <> @CurrentLevel SET  @NuLineage = '0'
	   
	  SET @NuLineage =  dbo.LCOrgIncrementLineage(@NuLineage)
     
      UPDATE ORGANISM SET Lineage = RTRIM(@ParentLineage) + '\' + @NuLineage  WHERE ORGANISM_KEY = @OrganismKey
	  Set @PrevParentKey = @CurrentParentKey
      SET @PrevLevel = @CurrentLevel    
	   
	   
   END
   
   CLOSE csrLineage
   DEALLOCATE csrLineage
  
   UPDATE ORGANISM SET Lineage = Lineage + '\' 
    
   UPDATE ORGANISM SET Sort_Order =  [dbo].[LCOrgCreateSort] (Lineage)
   
   
GO
/****** Object:  UserDefinedFunction [dbo].[LCOrgCreateSort]    Script Date: 03/03/2016 18:50:39 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Function to turn the Organism Lineage into a Sort Order
  Parameters:
		@Lineage - eg (1/1/ZZ) 
              

  Created:	July 2012
  Updated: March 2016 to increase lenth of lineage (40 char) and sort order 36 char
  Author: MikeWeideli 

\*=========================================================================== */

ALTER function [dbo].[LCOrgCreateSort] (@Lineage varchar(40))
Returns Char(36)
as
BEGIN 
  Declare @Work varchar(36)
  Declare @S integer
  Declare @Temp varchar(36)
  Declare @SortLevel varchar(36)
  SET @Work = Rtrim(@Lineage) 
  SET @S = charindex('\',@work)
  SET @sortlevel = ''
  
  WHILE @S <> 0 
  BEGIN
     SET @Temp = Left(@work,@S-1)
     If len(@Temp) = 2 SET @SortLevel = @SortLevel + @Temp
     ELSE  SET @SortLevel = @SortLevel + '0' +  @Temp   
  
     SET @Work = Right(@Work,Len(@work) - @S)
     SET @S = charindex('\',@work)
   
  END   

    SET @SortLevel = @SortLevel + left('00000000000000000000000000000000000',36-len(@sortLevel))
  
  RETURN  @Sortlevel
  
END


