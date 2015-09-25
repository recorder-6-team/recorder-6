
/****** Object:  StoredProcedure [dbo].[usp_Populate_Index_Taxon_Hierarchy]    Script Date: 11/30/2012 21:03:16 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

IF EXISTS(SELECT 1 FROM sysobjects WHERE NAME='usp_Populate_Index_Taxon_Hierarchy')
  DROP PROCEDURE [usp_Populate_Index_Taxon_Hierarchy];
GO

/*===========================================================================*\
  Description:	Poulates table Index_Taxon_Hierarchy
 

  Created:	November 2012

  Last revision information:
    $Revision: 	1 $
    $Date: 6/05/08 11:00 $
    $Author: Mike Weideli $

\*=========================================================================== */
CREATE PROCEDURE [dbo].[usp_Populate_Index_Taxon_Hierarchy]
	
AS
 Declare @x Integer 
 Declare @Sequence integer 
 Declare @LevelKey char(1)
 TRUNCATE TABLE INDEX_TAXON_HIERARCHY
  
 CREATE TABLE #TEMPO (Organism_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS , Pos_Hierarchy char(16) COLLATE SQL_Latin1_General_CP1_CI_AS ,Sort_Level Integer)
 Set @X = 0
 WHILE @x < 5
   BEGIN 
    SET @X = @X + 1 
    SET @Sequence  =
    CASE @X
	WHEN '1' THEN 181 
	WHEN '2' THEN 101
	WHEN '3' THEN 61
	WHEN '4' THEN 31
	ELSE 11
    END
    SET @LevelKey = 
    CASE @X
	WHEN '1' THEN 'F' 
	WHEN '2' THEN 'O'
	WHEN '3' THEN 'C'
	WHEN '4' THEN 'P' 
	ELSE 'K'
    END
    
    INSERT INTO #TEMPO 
    SELECT ORG.Organism_Key, ORg1.Organism_Key, ORg1.Sort_Level
    FROM ORGANISM ORG INNER JOIN ORGANISM ORG1 ON ORG.Lineage like  ORG1.Lineage  + '%'
    INNER JOIN TAXON_RANK TR ON TR.TAXON_RANK_KEY = ORG1.ORGANISM_RANK_KEY 
    WHERE TR.SEQUENCE < @Sequence 
   
    DELETE FROM #TEMPO 
    FROM  #TEMPO TORG
    INNER JOIN ORGANISM ORG ON ORG.ORGANISM_KEY = TORG.ORGANISM_KEY    
    INNER JOIN TAXON_RANK TR ON TR.TAXON_RANK_KEY = ORG.ORGANISM_RANK_KEY 
    WHERE TR.SEQUENCE < @Sequence - 1 
 
    INSERT INTO INDEX_TAXON_HIERARCHY   
    SELECT ORG.Taxon_Version_Key,ORG1.Taxon_Version_Key, @LevelKey
    FROM #TEMPO INNER JOIN ORGANISM ORG ON ORG.ORGANISM_KEY = #TEMPO.ORGANISM_KEY
    INNER JOIN ORGANISM ORG1  ON ORG1.ORGANISM_KEY = #TEMPO.POS_HIERARCHY
    WHERE 
    #TEMPO.Sort_Level = (SELECT MAX(SORT_LEVEL) FROM 
    #TEMPO T1 WHERE T1.ORGANISM_KEY = #TEMPO.ORGANISM_KEY) 
    TRUNCATE TABLE #TEMPO
  END 
 

  DROP TABLE #TEMPO

GO


GRANT EXECUTE ON usp_Populate_Index_Taxon_Hierarchy TO R2k_ReadOnly
GRANT EXECUTE ON usp_Populate_Index_Taxon_Hierarchy TO R2k_RecordCardsOnly
GRANT EXECUTE ON usp_Populate_Index_Taxon_Hierarchy TO R2k_AddOnly
GRANT EXECUTE ON usp_Populate_Index_Taxon_Hierarchy TO R2k_FullEdit
GRANT EXECUTE ON usp_Populate_Index_Taxon_Hierarchy TO R2k_Administrator