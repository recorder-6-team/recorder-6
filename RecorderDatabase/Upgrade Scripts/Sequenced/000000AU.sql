/****** Changes to implement virtual organism table ******/


/****** Object:  Table [dbo].[Index_Virtual_Lists]    Script Date: 10/21/2016 10:23:14 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[Index_Virtual_Lists](
	[Taxon_List_key] [char](16) NOT NULL,
	[Group_Name] varchar(100)  Null, 
	[Start_Lineage] [varchar] (40) NULL,
	[Start_Sort_Code] [int] NULL,
	[End_Sort_Code] [int] NULL)
        ON [PRIMARY]

GO

    ALTER TABLE [dbo].[Index_Virtual_Lists] ADD 
	CONSTRAINT [PK_TAXON_LIST_KEY] PRIMARY KEY CLUSTERED 
	( [TAXON_LIST_KEY]
	)  ON [PRIMARY] 



SET ANSI_PADDING OFF

GO


GRANT SELECT ON [dbo].[Index_Virtual_Lists] TO R2k_AddOnly 
GRANT INSERT ON [dbo].[Index_Virtual_Lists] TO R2k_AddOnly 
GRANT SELECT ON [dbo].[Index_Virtual_Lists] TO R2k_ReadOnly 
GRANT SELECT ON [dbo].[Index_Virtual_Lists] TO R2k_RecordCardsOnly
GRANT SELECT ON [dbo].[Index_Virtual_Lists] TO R2k_Administrator 
GRANT INSERT ON [dbo].[Index_Virtual_Lists] TO R2k_Administrator 
GRANT DELETE ON [dbo].[Index_Virtual_Lists] TO R2k_Administrator 
GRANT UPDATE ON [dbo].[Index_Virtual_Lists] TO R2k_Administrator 
GRANT SELECT ON [dbo].[Index_Virtual_Lists] TO R2k_FullEdit
GRANT INSERT ON [dbo].[Index_Virtual_Lists] TO R2k_FullEdit 
GRANT DELETE ON [dbo].[Index_Virtual_Lists] TO R2k_FullEdit
GRANT UPDATE ON [dbo].[Index_Virtual_Lists] TO R2k_FullEdit


/****** Add Additional Fields to Organism table ******/

ALTER TABLE ORGANISM
ADD Sort_Code int

GO

ALTER TABLE ORGANISM
ADD Has_Children bit


GO

/****** Object:  StoredProcedure [dbo].[usp_Populate_Index_Virtual_List]    Script Date: 10/21/2016 11:44:07 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Poulates table Index_Virtual_Hierarchy
 

  Created:	October 2016

  Last revision information:
     $Author: Mike Weideli $

\*=========================================================================== */
CREATE PROCEDURE [dbo].[usp_Populate_Index_Taxon_Virtual]
	
AS
 
TRUNCATE TABLE Index_Virtual_Lists

INSERT INTO Index_Virtual_Lists (Taxon_List_Key,Group_Name)
Select Taxon_List_Key,Update_Mechanism  from TAXON_LIST where
LEFT(Taxon_List.taxon_List_Key,8) = 'VIRTUAL_' AND Update_Mechanism <> 'None'

UPDATE Index_Virtual_Lists set start_Lineage = 
(SeLect lineage from Organism Inner join Taxon_Version TV
on TV.Taxon_Version_Key = ORGANISM.TAXON_VERSION_KEY
INNEr JOIN Taxon T ON T.Taxon_Key =
TV.TAXON_KEY WHERE T.ITEM_NAME = Index_Virtual_Lists.Group_Name 
AND ORGANISM.REDUNDANT_FLAG IS NULL)    

UPDATE Index_Virtual_Lists set Start_Sort_Code =
(SeLect min(Sort_Code) FROM Organism 
WHERE LINEAGE like(Index_Virtual_Lists.Start_Lineage) + '%')

UPDATE Index_Virtual_Lists set End_Sort_Code =
(Select max(Sort_Code) FROM Organism 
WHERE LINEAGE like(Index_Virtual_Lists.Start_Lineage) + '%')

GO

GRANT EXECUTE ON [dbo].[usp_Populate_Index_Taxon_Virtual] TO PUBLIC

GO

/****** Object:  StoredProcedure [dbo].[spPopulateOrganismLineage]    Script Date: 10/20/2016 09:50:22 ******/
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
  October 2016 - Change to add sort_code
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
    Declare @SortCode integer
    
    Update Organism SET Lineage = '0', Sort_Order = null,Sort_Level = 0,Has_Children = 0 
  
  
    UPDATE ORGANISM SET HAS_CHILDREN = 1
    WHERE EXISTS (SELECT * FROM ORGANISM O
    WHERE O.PARENT_KEY = ORGANISM.ORGANISM_KEY)  
 
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
   
   DECLARE csrSortCode CURSOR
    FOR
    SELECT Org.Organism_Key
    FROM Organism Org 
    ORDER BY Sort_Order 
    
    
    OPEN csrSortCode
    
        
    FETCH NEXT FROM csrSortCode INTO @OrganismKey
       Set @SortCode = 1
      
      IF @@FETCH_STATUS = 0 UPDATE ORGANISM  SET Sort_Code = str(@SortCode) WHERE ORGANISM_KEY = @OrganismKey
      
    
    WHILE @@FETCH_STATUS = 0
    BEGIN
	  Set @SortCode = @SortCode + 1
	  FETCH NEXT FROM csrSortCode  INTO @OrganismKey
	  UPDATE ORGANISM  SET Sort_Code = str(@SortCode) WHERE ORGANISM_KEY = @OrganismKey
	  
	  
	   
   END
   CLOSE csrSortCode
   DEALLOCATE csrSortCode


   DECLARE csrRedundant CURSOR
   FOR
   SELECT Distinct Org.Sort_Level
   FROM Organism Org 
   ORDER BY Sort_Level Desc 
    
    
   OPEN csrRedundant
    
        
   FETCH NEXT FROM csrRedundant INTO @CurrentLevel
           
    
   WHILE @@FETCH_STATUS = 0
   BEGIN
	  
	  FETCH NEXT FROM csrRedundant  INTO @CurrentLevel
	  UPDATE ORGANISM SET REDUNDANT_FLAG = 'Y'
          FROM ORGANISM O WHERE
          EXISTS(SELECT * FROM ORGANISM OP WHERE OP.PARENT_KEY = O.ORGANISM_KEY AND OP.REDUNDANT_FLAG ='Y')
          AND  NOT EXISTS(SELECT * FROM ORGANISM OP WHERE OP.PARENT_KEY = O.ORGANISM_KEY AND OP.REDUNDANT_FLAG IS NULL )
          AND O.Sort_Level = @CurrentLevel    
	  
	   
   END
   CLOSE csrRedundant
   DEALLOCATE csrRedundant


   
   EXECUTE usp_Populate_Index_Taxon_Virtual


GO

/****** Object:  View [dbo].[VIRTUAL_ORGANISM]    Script Date: 10/20/2016 10:50:54 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[VIRTUAL_ORGANISM]
AS
SELECT     TLI.TAXON_LIST_ITEM_KEY, 'VIRTUAL_ORGANISM' AS TAXON_LIST_VERSION_KEY, TLI.TAXON_VERSION_KEY, 
                      TLI.TAXON_LIST_ITEM_KEY AS PREFERRED_NAME, NULL AS TAXON_LIST_VERSION_TO, O.SORT_CODE, TLI2.TAXON_LIST_ITEM_KEY AS PARENT, 
                      TLI.TAXON_RANK_KEY, TLI.ENTERED_BY, TLI.ENTRY_DATE, TLI.SYSTEM_SUPPLIED_DATA, O.HAS_CHILDREN
FROM         dbo.ORGANISM AS O INNER JOIN
                      dbo.TAXON_LIST_ITEM AS TLI ON TLI.TAXON_VERSION_KEY = O.TAXON_VERSION_KEY INNER JOIN
                      dbo.INDEX_TAXON_NAME AS ITN ON ITN.TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY AND 
                      ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY AND ITN.CAN_EXPAND = 1 INNER JOIN
                      dbo.ORGANISM AS O2 ON O2.ORGANISM_KEY = O.PARENT_KEY INNER JOIN
                      dbo.TAXON_LIST_ITEM AS TLI2 ON TLI2.TAXON_VERSION_KEY = O2.TAXON_VERSION_KEY INNER JOIN
                      dbo.INDEX_TAXON_NAME AS ITN2 ON ITN2.TAXON_LIST_ITEM_KEY = TLI2.TAXON_LIST_ITEM_KEY AND 
                      ITN2.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN2.TAXON_LIST_ITEM_KEY
                    UNION
SELECT     TLI.TAXON_LIST_ITEM_KEY, 'VIRTUAL_ORGANISM' AS TAXON_LIST_VERSION_KEY, TLI.TAXON_VERSION_KEY, 
                      TLI.TAXON_LIST_ITEM_KEY AS PREFERRED_NAME, NULL AS TAXON_LIST_VERSION_TO, O.SORT_CODE, NULL AS PARENT, TLI.TAXON_RANK_KEY, 
                      TLI.ENTERED_BY, TLI.ENTRY_DATE, TLI.SYSTEM_SUPPLIED_DATA, O.HAS_CHILDREN
FROM         dbo.ORGANISM AS O INNER JOIN
                      dbo.TAXON_LIST_ITEM AS TLI ON TLI.TAXON_VERSION_KEY = O.TAXON_VERSION_KEY AND O.PARENT_KEY IS NULL INNER JOIN
                      dbo.INDEX_TAXON_NAME AS ITN ON ITN.TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY AND ITN.CAN_EXPAND = 1 AND 
                      ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
       

GO

GRANT SELECT On VIRTUAL_ORGANISM TO PUBLIC

GO


/****** Object:  View [dbo].[VIRTUAL_COLEOPTE]    Script Date: 10/20/2016 11:09:28 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[VIRTUAL_COLEOPTE]
AS
SELECT     TLI.TAXON_LIST_ITEM_KEY, 'VIRTUAL_COLEOPTE' AS TAXON_LIST_VERSION_KEY, TLI.TAXON_VERSION_KEY, 
                      TLI.TAXON_LIST_ITEM_KEY AS PREFERRED_NAME, NULL AS TAXON_LIST_VERSION_TO, O.SORT_CODE, TLI2.TAXON_LIST_ITEM_KEY AS PARENT, 
                      TLI.TAXON_RANK_KEY, TLI.ENTERED_BY, TLI.ENTRY_DATE, TLI.SYSTEM_SUPPLIED_DATA, O.HAS_CHILDREN
FROM         dbo.ORGANISM AS O INNER JOIN
                      dbo.TAXON_LIST_ITEM AS TLI ON TLI.TAXON_VERSION_KEY = O.TAXON_VERSION_KEY INNER JOIN
                      dbo.INDEX_TAXON_NAME AS ITN ON ITN.TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY AND 
                      ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY AND ITN.CAN_EXPAND = 1 INNER JOIN
                      dbo.ORGANISM AS O2 ON O2.ORGANISM_KEY = O.PARENT_KEY INNER JOIN
                      dbo.TAXON_LIST_ITEM AS TLI2 ON TLI2.TAXON_VERSION_KEY = O2.TAXON_VERSION_KEY INNER JOIN
                      dbo.INDEX_TAXON_NAME AS ITN2 ON ITN2.TAXON_LIST_ITEM_KEY = TLI2.TAXON_LIST_ITEM_KEY AND 
                      ITN2.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN2.TAXON_LIST_ITEM_KEY INNER JOIN
                      dbo.Index_Virtual_Lists AS IVL ON IVL.Taxon_List_key = 'VIRTUAL_COLEOPTE' AND O.SORT_CODE >= IVL.Start_Sort_Code AND O.SORT_CODE <= IVL.End_Sort_Code
GO

GRANT SELECT On VIRTUAL_COLEOPTE TO PUBLIC

GO


INSERT INTO TAXON_LIST(TAXON_LIST_KEY,ITEM_NAME,DESCRIPTION,AUTHORITY,TAXON_LIST_TYPE_KEy,
LOCAL_DISK,UPDATE_MECHANISM,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA,CUSTODIAN,PREFERRED,PRIORITY)
VALUES('VIRTUAL_ORGANISM','  ORGANISM View','Blank list for displaying vitual Organsim Table',
'JNCC', 'NBNSYS0000000005',1,'Biota','TESTDATA00000001',Getdate(),1,'TESTDATA',0,9999)

GO

INSERT INTO TAXON_LIST_VERSION(TAXON_LIST_VERSION_KEY,TAXON_LIST_KEY,VERSION,AUTHORITY,VAGUE_DATE_START,
VERSION_IS_AMENDMENT,QUALITY,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA,CUSTODIAN)
VALUES ('VIRTUAL_ORGANISM','VIRTUAL_ORGANISM',1,'JNCC',37393,0,'No items','TESTDATA00000001',GetDate(),1,'TESTDATA')

GO

GO


INSERT INTO TAXON_LIST(TAXON_LIST_KEY,ITEM_NAME,DESCRIPTION,AUTHORITY,TAXON_LIST_TYPE_KEy,
LOCAL_DISK,UPDATE_MECHANISM,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA,CUSTODIAN,PREFERRED,PRIORITY)
VALUES('VIRTUAL_COLEOPTE','  COLEOPTERA View','Blank list for displaying vitual Coleoptera Table',
'JNCC', 'NBNSYS0000000005',1,'Coleoptera','TESTDATA00000001',Getdate(),1,'TESTDATA',0,9999)

GO

INSERT INTO TAXON_LIST_VERSION(TAXON_LIST_VERSION_KEY,TAXON_LIST_KEY,VERSION,AUTHORITY,VAGUE_DATE_START,
VERSION_IS_AMENDMENT,QUALITY,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA,CUSTODIAN)
VALUES ('VIRTUAL_COLEOPTE','VIRTUAL_COLEOPTE',1,'JNCC',37393,0,'No items','TESTDATA00000001',GetDate(),1,'TESTDATA')

GO