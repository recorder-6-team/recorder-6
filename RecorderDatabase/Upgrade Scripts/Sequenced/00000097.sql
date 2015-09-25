/****** Metadata: Fix lower case in grid refs  ******/

Update Sample set Spatial_ref = UPPER(Spatial_ref)
WHERE SPATIAL_REF_SYSTEM IN ('OSGB','OSNI')

Update Survey_event  set Spatial_ref = UPPER(Spatial_ref)
WHERE SPATIAL_REF_SYSTEM IN ('OSGB','OSNI')

Update Location set Spatial_ref = UPPER(Spatial_ref)
WHERE SPATIAL_REF_SYSTEM IN ('OSGB','OSNI')
 
GO
/****** Metadata: Add names to Setting for temporary data  ******/

IF NOT EXISTS(SELECT 1 FROM [dbo].[NAME] WHERE NAME_KEY='LCA0002400000001') 
INSERT INTO NAME (NAME_KEY, ORGANISATION,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA,CUSTODIAN)
VALUES ('LCA0002400000001', 0, 'NBNSYS0000000001', GETDATE(), 1,'NBNSYS00');

IF NOT EXISTS(SELECT 1 FROM [dbo].[INDIVIDUAL] WHERE NAME_KEY='LCA0002400000001') 
INSERT INTO INDIVIDUAL (NAME_KEY,SURNAME,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002400000001','Withheld','NBNSYS0000000001', GETDATE(), 1);

DELETE FROM SETTING WHERE [NAME] = 'TempName';

INSERT INTO SETTING VALUES ('TempName', 'LCA0002400000001');


GO


/****** Metadata: Allows matching of taxon version keys ******/


/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Species]    Script Date: 09/26/2013 17:48:47 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Set the match for the specified import record in the match table.

  Parameters: @ChecklistKey

  Created:  June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/13 10:37 $
    $Author: MikeWeideli $

\*===========================================================================*/


ALTER PROCEDURE [dbo].[usp_IWMatch_Species]
  @ChecklistKey char(16)
AS
    
    -- Setup new column with reworked Import_Value, hopefully that will speed things up a bit.
  UPDATE  #Species
  SET Species_Name = 
    CASE  WHEN Right(Import_Value, 2) = 'sp' THEN Left(Import_Value, Len(Import_Value) - 2)
      WHEN Right(Import_Value, 3) = 'sp.' THEN Left(Import_Value, Len(Import_Value) - 3)
      WHEN Right(Import_Value, 3) = 'spp' THEN Left(Import_Value, Len(Import_Value) - 3)
      WHEN Right(Import_Value, 4) = 'spp.' THEN Left(Import_Value, Len(Import_Value) - 4)
      ELSE Import_Value
    END
     --  remove the sub genus from import_value 
     
     UPDATE #Species Set Species_name = dbo.LCRemoveSubGenusText(Species_name)
     
    -- Match TV Key
       UPDATE #Species
    SET Match_Key = ITN2.Taxon_List_Item_Key,
      Match_Value =  TLI.Taxon_Version_Key,
      Checklist = TL.Item_Name,
      Checklist_Key = @ChecklistKey,
      Match_Count = 1
    FROM  
    Taxon_List_Item TLI INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLi.Taxon_List_Item_Key
    INNER JOIN Index_Taxon_Name ITN2 ON ITN.Recommended_Taxon_List_Item_Key = ITN2.Taxon_List_Item_Key
    JOIN  Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
    JOIN  Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
    WHERE Species_Name = TLI.Taxon_Version_Key
  
    IF @ChecklistKey IS NULL
  BEGIN
    -- Handle searches against the virtual preferred list
  
    
    UPDATE  UpdatedSpecies
    SET Match_Count =  (SELECT  Count(DISTINCT ITN2.Taxon_List_Item_Key)
          FROM  #Species S  
          INNER JOIN Index_Taxon_Name ITN1 ON dbo.LCRemoveSubGenusText(ITN1.Actual_Name) = S.Species_Name
          INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
          INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
          INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
          WHERE TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
          AND ITN2.Preferred_List = 1
          AND S.Import_Value = UpdatedSpecies.Import_Value
          AND TLI.Taxon_List_Version_To IS NULL
          AND TLV.Version >= (SELECT MAX(Version)
                  FROM Taxon_List_Version 
                        WHERE Taxon_List_Key = TLV.Taxon_List_Key
                  AND Version_Is_Amendment = 0))
    FROM  #Species UpdatedSpecies
    WHERE Match_Key IS NULL

    -- Set Match_Count for species name + authority test. Note the strange way of doing
    -- this join - it seems to be faster this way !
    UPDATE  UpdatedSpecies
    SET Match_Count = Match_Count + (SELECT Count(DISTINCT ITN2.Taxon_List_Item_Key)
          FROM  #Species S  
          INNER JOIN Index_Taxon_Name ITN1 ON LEFT(S.Species_Name, LEN(dbo.LCRemoveSubGenusText(ITN1.Actual_Name))) = dbo.LCRemoveSubGenusText(ITN1.Actual_Name)
          INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
          INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
          INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
          WHERE TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
          AND S.Species_Name = dbo.LCRemoveSubGenusText(ITN1.Actual_Name) + ' ' + ITN1.Authority
          AND ITN2.Preferred_List = 1
          AND S.Import_Value = UpdatedSpecies.Import_Value
          AND TLI.Taxon_List_Version_To IS NULL
          AND TLV.Version >= (SELECT MAX(Version)
                  FROM Taxon_List_Version 
                        WHERE Taxon_List_Key = TLV.Taxon_List_Key
                  AND Version_Is_Amendment = 0))
    FROM  #Species UpdatedSpecies
    WHERE Match_Key IS NULL


    -- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
    UPDATE  #Species
    SET Match_Key = ITN2.Taxon_List_Item_Key,
      Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN2.Taxon_List_Item_Key),
      Checklist = TL.Item_Name,
      Checklist_Key = NULL
    FROM  Index_Taxon_Name ITN1
    JOIN  Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
    JOIN  Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
    JOIN  Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
    JOIN  Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
    WHERE Match_Count = 1
    AND Match_Key IS NULL
    AND TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
    AND ITN2.Preferred_List = 1
    AND Species_Name = dbo.LCRemoveSubGenusText(ITN1.Actual_Name)
    AND TLI.Taxon_List_Version_To IS NULL

    UPDATE  #Species
    SET Match_Key = ITN2.Taxon_List_Item_Key,
      Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN2.Taxon_List_Item_Key),
      Checklist = TL.Item_Name,
      Checklist_Key = NULL
    FROM  Index_Taxon_Name ITN1
    JOIN  Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
    JOIN  Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
    JOIN  Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
    JOIN  Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key 
    WHERE Match_Count = 1
    AND Match_Key IS NULL
    AND TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
    AND ITN2.Preferred_List = 1
    AND Species_Name = dbo.LCRemoveSubGenusText(ITN1.Actual_Name) + ' ' + ITN1.Authority
    AND TLI.Taxon_List_Version_To IS NULL

  END ELSE
  BEGIN
    -- Handle searches against a specified list
        -- Set Match_Count first. Broken down in two separate updates for speed.
    UPDATE  UpdatedSpecies
    SET Match_Count =  (SELECT  Count(*)
          FROM  #Species S 
          INNER JOIN Index_Taxon_Name ITN ON dbo.LCRemoveSubGenusText(ITN.Actual_Name) = S.Species_Name
          INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key          
          INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
          WHERE TLV.Taxon_List_Key = @ChecklistKey
          AND S.Import_Value = UpdatedSpecies.Import_Value
          AND TLI.Taxon_List_Version_To IS NULL
          AND TLV.Version >= (SELECT MAX(Version)
                  FROM Taxon_List_Version 
                        WHERE Taxon_List_Key = TLV.Taxon_List_Key
                  AND Version_Is_Amendment = 0))
    FROM  #Species UpdatedSpecies
    WHERE Match_Key IS NULL

    UPDATE  UpdatedSpecies
    SET Match_Count = Match_Count + (SELECT Count(*)
          FROM  #Species S 
          INNER JOIN Index_Taxon_Name ITN ON dbo.LCRemoveSubGenusText(ITN.Actual_Name)  + ' ' + ITN.Authority = S.Species_Name 
          INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key          
          INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
          WHERE TLV.Taxon_List_Key = @ChecklistKey
          AND S.Import_Value = UpdatedSpecies.Import_Value
          AND TLI.Taxon_List_Version_To IS NULL
          AND TLV.Version >= (SELECT MAX(Version)
                  FROM Taxon_List_Version 
                        WHERE Taxon_List_Key = TLV.Taxon_List_Key
                  AND Version_Is_Amendment = 0))
    FROM  #Species UpdatedSpecies
    WHERE Match_Key IS NULL
    -- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
    UPDATE  #Species
    SET Match_Key = ITN.Taxon_List_Item_Key,
      Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
      Checklist = TL.Item_Name,
      Checklist_Key = TL.Taxon_List_Key
    FROM  Index_Taxon_Name ITN
    JOIN  Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
    JOIN  Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
    JOIN  Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
    WHERE Match_Count = 1
    AND Match_Key IS NULL
    AND TL.Taxon_List_Key = @ChecklistKey
    AND Species_Name = dbo.LCRemoveSubGenusText(Actual_Name)
    AND TLI.Taxon_List_Version_To IS NULL

    UPDATE  #Species
    SET Match_Key = ITN.Taxon_List_Item_Key,
      Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
      Checklist = TL.Item_Name,
      Checklist_Key = TL.Taxon_List_Key
    FROM  Index_Taxon_Name ITN
    JOIN  Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
    JOIN  Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
    JOIN  Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key  
    WHERE Match_Count = 1
    AND Match_Key IS NULL
    AND TL.Taxon_List_Key = @ChecklistKey
    AND Species_Name = dbo.LCRemoveSubGenusText(Actual_Name) + ' ' + ITN.Authority
    AND TLI.Taxon_List_Version_To IS NULL
  END

    

  -- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
  UPDATE  S
  SET [Order] = ITN.Actual_Name
  FROM  #Species  S
  JOIN  Index_Taxon_Synonym ITS ON ITS.Synonym_List_Item_Key = S.Match_Key
  JOIN  Index_Taxon_Group ITG ON ITS.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
  JOIN  Taxon_List_Item TLI 
    ON  ITG.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
    AND TLI.Taxon_Rank_Key = 'NBNSYS0000000012'
  JOIN  Index_Taxon_Name ITN
    ON  ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
  WHERE Match_Count = 1
  AND [Order] IS NULL

  -- Order field can still be null, so deal with that. 'Not available' value handled and regionalised in app.
  UPDATE  #Species
  SET [Order] = 'Not available'
  WHERE Match_Count = 1
  AND [Order] IS NULL

GO

/****** Object:  StoredProcedure [dbo].[usp_IWMatch_AssociatedSpecies]    Script Date: 09/26/2013 17:48:47 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Set the match for the specified import record in the match table.

  Parameters: @ChecklistKey

  Created:  June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/2/2013 10:37 $
    $Author: Mikeweideli $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatch_AssociatedSpecies]
  @ChecklistKey char(16)
AS
  -- Setup new column with reworked Import_Value, hopefully that will speed things up a bit.
  UPDATE  #AssociatedSpecies
  SET Species_Name = 
    CASE  WHEN Right(Import_Value, 2) = 'sp' THEN Left(Import_Value, Len(Import_Value) - 2)
      WHEN Right(Import_Value, 3) = 'sp.' THEN Left(Import_Value, Len(Import_Value) - 3)
      WHEN Right(Import_Value, 3) = 'spp' THEN Left(Import_Value, Len(Import_Value) - 3)
      WHEN Right(Import_Value, 4) = 'spp.' THEN Left(Import_Value, Len(Import_Value) - 4)
      ELSE Import_Value
    END

  --  remove the sub genus from import_value 
     
     UPDATE #AssociatedSpecies Set Species_name = dbo.LCRemoveSubGenusText(Species_name)


  IF @ChecklistKey IS NULL
  BEGIN
    -- Handle searches against the virtual preferred list

    -- Set Match_Count first. Broken down in two separate updates for speed.
    UPDATE  UpdatedSpecies
    SET Match_Count =  (SELECT  Count(DISTINCT ITN2.Taxon_List_Item_Key)
          FROM  #AssociatedSpecies S  
          INNER JOIN Index_Taxon_Name ITN1 ON ITN1.Actual_Name = S.Species_Name
          INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
          INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
          INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
          WHERE TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
          AND ITN2.Preferred_List = 1
          AND S.Import_Value = UpdatedSpecies.Import_Value
          AND TLI.Taxon_List_Version_To IS NULL
          AND TLV.Version >= (SELECT MAX(Version)
                  FROM Taxon_List_Version 
                        WHERE Taxon_List_Key = TLV.Taxon_List_Key
                  AND Version_Is_Amendment = 0))
    FROM  #AssociatedSpecies UpdatedSpecies
    WHERE Match_Key IS NULL

    -- Set Match_Count for species name + authority test. Note the strange way of doing
    -- this join - it seems to be faster this way!
    UPDATE  UpdatedSpecies
    SET Match_Count = Match_Count + (SELECT Count(DISTINCT ITN2.Taxon_List_Item_Key)
          FROM  #AssociatedSpecies S  
          INNER JOIN Index_Taxon_Name ITN1 ON LEFT(S.Species_Name, LEN(ITN1.Actual_Name)) = ITN1.Actual_Name
          INNER JOIN Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
          INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
          INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
          WHERE TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
          AND S.Species_Name = ITN1.Actual_Name + ' ' + ITN1.Authority
          AND ITN2.Preferred_List = 1
          AND S.Import_Value = UpdatedSpecies.Import_Value
          AND TLI.Taxon_List_Version_To IS NULL
          AND TLV.Version >= (SELECT MAX(Version)
                  FROM Taxon_List_Version 
                        WHERE Taxon_List_Key = TLV.Taxon_List_Key
                  AND Version_Is_Amendment = 0))
    FROM  #AssociatedSpecies UpdatedSpecies
    WHERE Match_Key IS NULL


    -- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
    UPDATE  #AssociatedSpecies
    SET Match_Key = ITN2.Taxon_List_Item_Key,
      Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN2.Taxon_List_Item_Key),
      Checklist = TL.Item_Name,
      Checklist_Key = NULL
    FROM  Index_Taxon_Name ITN1
    JOIN  Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
    JOIN  Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
    JOIN  Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
    JOIN  Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key
    WHERE Match_Count = 1
    AND Match_Key IS NULL
    AND TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
    AND ITN2.Preferred_List = 1
    AND Species_Name = ITN1.Actual_Name
    AND TLI.Taxon_List_Version_To IS NULL

    UPDATE  #AssociatedSpecies
    SET Match_Key = ITN2.Taxon_List_Item_Key,
      Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN2.Taxon_List_Item_Key),
      Checklist = TL.Item_Name,
      Checklist_Key = NULL
    FROM  Index_Taxon_Name ITN1
    JOIN  Index_Taxon_Name ITN2 ON ITN2.Recommended_Taxon_List_Item_Key=ITN1.Recommended_Taxon_List_Item_Key
    JOIN  Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
    JOIN  Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
    JOIN  Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN2.Taxon_List_Item_Key 
    WHERE Match_Count = 1
    AND Match_Key IS NULL
    AND TLI.Preferred_Name = TLI.Taxon_List_Item_Key 
    AND ITN2.Preferred_List = 1
    AND Species_Name = ITN1.Actual_Name + ' ' + ITN1.Authority
    AND TLI.Taxon_List_Version_To IS NULL

  END ELSE
  BEGIN
    -- Handle searches against a specified list

    -- Set Match_Count first. Broken down in two separate updates for speed.
    UPDATE  UpdatedSpecies
    SET Match_Count =  (SELECT  Count(*)
          FROM  #AssociatedSpecies S 
          INNER JOIN Index_Taxon_Name ITN ON ITN.Actual_Name = S.Species_Name 
          INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key          
          INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
          WHERE TLV.Taxon_List_Key = @ChecklistKey
          AND S.Import_Value = UpdatedSpecies.Import_Value
          AND TLI.Taxon_List_Version_To IS NULL
          AND TLV.Version >= (SELECT MAX(Version)
                  FROM Taxon_List_Version 
                        WHERE Taxon_List_Key = TLV.Taxon_List_Key
                  AND Version_Is_Amendment = 0))
    FROM  #AssociatedSpecies UpdatedSpecies
    WHERE Match_Key IS NULL

    UPDATE  UpdatedSpecies
    SET Match_Count = Match_Count + (SELECT Count(*)
          FROM  #AssociatedSpecies S 
          INNER JOIN Index_Taxon_Name ITN ON ITN.Actual_Name + ' ' + ITN.Authority = S.Species_Name 
          INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key          
          INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
          WHERE TLV.Taxon_List_Key = @ChecklistKey
          AND S.Import_Value = UpdatedSpecies.Import_Value
          AND TLI.Taxon_List_Version_To IS NULL
          AND TLV.Version >= (SELECT MAX(Version)
                  FROM Taxon_List_Version 
                        WHERE Taxon_List_Key = TLV.Taxon_List_Key
                  AND Version_Is_Amendment = 0))
    FROM  #AssociatedSpecies UpdatedSpecies
    WHERE Match_Key IS NULL


    -- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
    UPDATE  #AssociatedSpecies
    SET Match_Key = ITN.Taxon_List_Item_Key,
      Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
      Checklist = TL.Item_Name,
      Checklist_Key = TL.Taxon_List_Key
    FROM  Index_Taxon_Name ITN
    JOIN  Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
    JOIN  Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
    JOIN  Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
    WHERE Match_Count = 1
    AND Match_Key IS NULL
    AND TL.Taxon_List_Key = @ChecklistKey
    AND Species_Name = Actual_Name
    AND TLI.Taxon_List_Version_To IS NULL

    UPDATE  #AssociatedSpecies
    SET Match_Key = ITN.Taxon_List_Item_Key,
      Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
      Checklist = TL.Item_Name,
      Checklist_Key = TL.Taxon_List_Key
    FROM  Index_Taxon_Name ITN
    JOIN  Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
    JOIN  Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
    JOIN  Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key  
    WHERE Match_Count = 1
    AND Match_Key IS NULL
    AND TL.Taxon_List_Key = @ChecklistKey
    AND Species_Name = Actual_Name + ' ' + ITN.Authority
    AND TLI.Taxon_List_Version_To IS NULL
  END

  -- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
  UPDATE  S
  SET [Order] = ITN.Actual_Name
  FROM  #AssociatedSpecies  S
  JOIN  Index_Taxon_Synonym ITS ON ITS.Synonym_List_Item_Key = S.Match_Key
  JOIN  Index_Taxon_Group ITG ON ITS.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
  JOIN  Taxon_List_Item TLI 
    ON  ITG.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
    AND TLI.Taxon_Rank_Key = 'NBNSYS0000000012'
  JOIN  Index_Taxon_Name ITN
    ON  ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
  WHERE Match_Count = 1
  AND [Order] IS NULL

  -- Order field can still be null, so deal with that. 'Not available' value handled and regionalised in app.
  UPDATE  #AssociatedSpecies
  SET [Order] = 'Not available'
  WHERE Match_Count = 1
  AND [Order] IS NULL

GO


/****** Metadata: Procedure to removes entries from External_Gui_link where Taxon Occurrence no longer Exists  ******/

Update Report_field set Field_size = 300 Where Report_Field_Key = 'JNCCDEV100000006'

IF EXISTS(SELECT 1 FROM sysobjects WHERE NAME='usp_ImportWizard_Unwanted_Imported')
  DROP PROCEDURE [usp_ImportWizard_Unwanted_Imported];
GO

/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_Unwanted_Imported]    Script Date: 02/17/2013 17:34:46 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Removes entries in Imported_Data_Keys which no longer have Taxon_Version_Keys
         
  Parameters:   None

  Created:      Feb 2013

  Last revision information:
       $Author: Mikeweideli$

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ImportWizard_Unwanted_Imported]
AS
   DELETE FROM External_GUI_Link 
   WHERE NOT EXISTS (SELECT * FROM Taxon_Occurrence WHERE 
   Taxon_Occurrence_Key = External_GUI_Link.Taxon_Occurrence_Key)
GO

GRANT EXECUTE ON  [dbo].[usp_ImportWizard_Unwanted_Imported] TO PUBLIC  

GO

/****** Metadata: Creates new table 'External_GUI_Link' ******/

IF EXISTS(SELECT 1 FROM SYSOBJECTS WHERE NAME='External_GUI_Link')
  DROP TABLE External_GUI_Link


GO

/****** Object:  Table [dbo].[External_GUI_Link]    Script Date: 02/17/2013 17:41:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT * FROM dbo.SysObjects WHERE Id = Object_Id(N'[dbo].[External_GUI_Link]') AND ObjectProperty(Id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[External_GUI_Link](
  [External_GUI_Link_Key] [char](16) NOT NULL,
  [Taxon_Occurrence_Key] [char](16) NOT NULL,
  [External_GUI] [varchar](30) NOT NULL,
  [Date_Imported] [datetime] NOT NULL,
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[External_GUI_Link] ADD
 CONSTRAINT [PK_External_GUI_Link] PRIMARY KEY CLUSTERED 
(
  [External_GUI_Link_Key] ASC
) ON [PRIMARY]
GO

SET ANSI_PADDING OFF
GO

ALTER TABLE [dbo].[External_GUI_Link] ADD  CONSTRAINT [DF_External_GUI_Date_Imported]  DEFAULT (getdate()) FOR [Date_Imported]
GO


GRANT SELECT ON External_GUI_Link TO R2k_Administrator 
GRANT UPDATE ON External_GUI_Link TO R2k_Administrator 
GRANT DELETE ON External_GUI_Link TO R2k_Administrator 
GRANT INSERT ON External_GUI_Link TO R2k_Administrator 
GRANT SELECT ON External_GUI_Link TO R2k_FullEdit
GRANT SELECT ON External_GUI_Link TO R2k_ReadOnly
GRANT SELECT ON External_GUI_Link TO R2k_AddOnly
GRANT SELECT ON External_GUI_Link TO R2k_RecordCardsOnly


GO

/* Add Iw entries for populating the  External_GUI_Link */

/* Delete all the entries in the tables. in reverse order */

DELETE FROM IW_Post_Processing_Procedure WHERE IW_Post_Processing_Procedure_Key  = 'LCA00023000000R8'
GO
DELETE FROM IW_Table_Rule_Related_Field WHERE IW_Table_Rule_Key  = 'LCA00023000000R8'



GO
DELETE FROM IW_Table_Rule_Related_Table WHERE IW_Table_Rule_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Table_Rule_OUTPUT_FIELD WHERE IW_Table_Rule_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Output_field  WHERE IW_Output_Field_Key  = 'LCA00023000000R8'
GO
DELETE FROM IW_Output_field  WHERE IW_Output_Field_Key  = 'LCA00023000000R9'
GO
DELETE FROM IW_Table_Rule WHERE IW_Table_Rule_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Column_Type_Match_Rule WHERE IW_Column_Type_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Match_Rule WHERE IW_Match_Rule_Key = 'LCA00023000000R8'
GO
DELETE FROM IW_Column_Type WHERE IW_Column_Type_Key  = 'LCA00023000000R8'
GO

DELETE FROM DataBase_RelationShip WHERE Relationship_Key   = 'LCA00023000000R8'
GO

INSERT INTO IW_Column_Type (IW_Column_Type_Key,Class_Name,Item_Name,Required,
Commonly_Used, Sequence,Parser_Class_Name,Maximum_Length,Entered_By,
Entry_Date,System_Supplied_Data) 
VALUES ('LCA00023000000R8','TColumnType','External Key',0,0,9,'TTextParser',
30,'TESTDATA00000001',GetDate(),1)

GO

INSERT INTO IW_TABLE_RULE
(IW_Table_Rule_Key,Sequence,Table_Name,Filter_Expression,Entered_By,
Entry_Date,System_Supplied_Data)
VALUES ('LCA00023000000R8',17,'External_GUI_Link','#master.LCA00023000000R8_data <> ''''',
'TESTDATA00000001',GetDate(),1)

GO


INSERT INTO IW_OUTPUT_FIELD
(IW_outPut_Field_Key,Name,Data_Type,IW_Column_Type_Key,Source_Field_Name,Entered_by,
Entry_date,System_Supplied_data) 
VALUES ('LCA00023000000R8','External_GUI','VARCHAR(30)','LCA00023000000R8','data',
'TESTDATA00000001',GetDate(),1)

GO

INSERT INTO IW_OUTPUT_FIELD
(IW_outPut_Field_Key,Name,Data_Type,Generating_Class_Name,Generator_Field_Index, Entered_by,
Entry_date,System_Supplied_data) 
VALUES ('LCA00023000000R9','External_GUI_Link_Key','CHAR(16)','TKeyFieldGenerator',0,
'TESTDATA00000001',GetDate(),1)

GO


INSERT IW_Table_Rule_OUTPUT_FIELD (IW_table_Rule_Key,IW_Output_Field_Key,Entered_By,
Entry_Date,System_supplied_data)
VALUES ('LCA00023000000R8','LCA00023000000R8','TESTDATA00000001',GetDate(),1)


GO

INSERT IW_Table_Rule_OUTPUT_FIELD (IW_table_Rule_Key,IW_Output_Field_Key,Entered_By,
Entry_Date,System_supplied_data)
VALUES ('LCA00023000000R8','SYSTEM010000001A','TESTDATA00000001',GetDate(),1)

GO

INSERT IW_Table_Rule_OUTPUT_FIELD (IW_table_Rule_Key,IW_Output_Field_Key,Entered_By,
Entry_Date,System_supplied_data)
VALUES ('LCA00023000000R8','LCA00023000000R9','TESTDATA00000001',GetDate(),1)


GO

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key,IW_Column_Type_Key,Relationship,
Entered_By,Entry_date,System_supplied_data)
VALUES ('LCA00023000000R8','LCA00023000000R8',2,'TESTDATA00000001',GetDate(),1)

GO


INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key,Table_name,Relationship,
Entered_By,Entry_date,System_supplied_data)
VALUES ('LCA00023000000R8','Taxon_Occurrence',1,'TESTDATA00000001',GetDate(),1)

GO

INSERT INTO IW_Post_Processing_procedure (IW_Post_Processing_Procedure_Key,Sequence,Procedure_name,
Entered_By,Entry_date,System_supplied_data)
VALUES ('LCA00023000000R8',2 ,'usp_ImportWizard_Unwanted_Imported','TESTDATA00000001',GetDate(),1)

GO

INSERT INTO DATABASE_RELATIONSHIP (Relationship_Key, Relationship_Name,Master_table,Master_Field,Detail_Table,Detail_Field,
Follow_Up,Follow_Down,One_to_One)
VALUES ('LCA00023000000R8','TAXON_OCCURRENCEIMPORTEDDATAKEYS','TAXON_OCCURRENCE','TAXON_OCCURRENCE_KEY',
'EXTERNAL_GUI_LINK', 'TAXON_OCCURRENCE_KEY',0,1,0)

GO

INSERT INTO IW_Column_type_Pattern  (IW_Column_Type_Key,Pattern,Exclude_Match,
Entered_By,Entry_date,System_supplied_data)
VALUES ('LCA00023000000R8','ext%' ,0,'TESTDATA00000001',GetDate(),1) 

INSERT INTO IW_Column_type_Pattern  (IW_Column_Type_Key,Pattern,Exclude_Match,
Entered_By,Entry_date,System_supplied_data)
VALUES ('LCA00023000000R8','%gui%' ,0,'TESTDATA00000001',GetDate(),1) 
GO

/* UDF for consolidating the normal and temporary Recorder in the report wizard*/


/****** Object:  UserDefinedFunction [dbo].[ConsolidatedRecorder]    Script Date: 09/26/2013 19:04:39 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/****** Object:  UserDefinedFunction [dbo].[ConsolidateRecorder]    Script Date: 10/08/2012 18:04:43 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[ConsolidateRecorder]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[ConsolidateRecorder]

GO
/*===========================================================================*\
  Description:  Returns the Recorder name from the Recorders column in sample if the field is not null
  Otherwise returns the Recorder in the normal way  

  Parameters:
  @SampleKey - Sample key 
      
  Created:  September 2013
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[ConsolidateRecorder]
(@SampleKey char(16) )

RETURNS varchar(8000)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)


Select @ReturnString = Sample.Recorders
FROM Sample where Sample_Key = @SampleKey

If  @ReturnString IS NULL SET @ReturnString = [dbo].[FormatEventRecorders](@SampleKey)
    

RETURN @ReturnString

END

GO

GRANT EXECUTE ON [dbo].[ConsolidateRecorder] TO PUBLIC

GO

/****** Including consolidated recorder in the report wizard: ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000009') and REPORT_FIELD_KEY = 'LCA0002200000009'
Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000009') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000009'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000009', 'Sample', 'SAMPLE', 'Consolidated Recorders', 
'#REPORT_OUTPUT.[Consolidated Recorders] = dbo.ConsolidateRecorder(SAMPLE_KEY)', 
'NBNSYS0000000035', NULL, 'TESTDATA00000001', GetDate(), NULL, NULL, 1)
Insert Into REPORT_FIELD
Values ('LCA0002200000009', 'LCA0002200000009', 'Consolidated Recorders', 'varchar', 100, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[GetExternalRef]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[GetExternalRef]

GO
/****** Object:  UserDefinedFunction [dbo].[GetExternalRef]    Script Date: 09/27/2013 21:00:55 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Returns the External Reference for a given Taxon Occurrence Key

  Parameters:
  @ToccKey - Taxon occurrence key 
      
  Created:  September 2013
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[GetExternalRef]
(@ToccKey char(16) )

RETURNS varchar(40)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(40)


Select @ReturnString = External_GUI_Link.External_GUI
FROM External_GUI_Link where Taxon_Occurrence_key = @TOCCKey

If  @ReturnString IS NULL SET @ReturnString = 'Not recorded'
    

RETURN @ReturnString

END

GO



GRANT EXECUTE ON [dbo].[GetExternalRef] TO Public

GO

/****** Including external reference in the report wizard: ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000010') and REPORT_FIELD_KEY = 'LCA0002200000010'

Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000010') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000010'

GO
Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000010', 'Taxon\Observations', 'OBSERVATION', 'External Ref', 
'#REPORT_OUTPUT.[External Ref] =[dbo].[GetExternalRef](TAXON_OCCURRENCE_KEY)', 
'NBNSYS0000000016', 'NBNSYS0000000000', 'TESTDATA00000001', GetDate(), NULL, NULL, 1)

Go

Insert Into REPORT_FIELD
Values ('LCA0002200000010', 'LCA0002200000010', 'External Ref', 'varchar', 40, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO


/****** Modify script to allow the import of Name keys in the Recorders column of Iw ******/


/****** Object:  StoredProcedure [dbo].[usp_IWMatchRemembered_Names]    Script Date: 09/28/2013 21:25:08 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Populate import table with matched values from previous imports.

  Parameters: <none>

  Created:  July 2004

  Last revision information:
    $Revision: 8 $
    $Date: 28/09/13 15:52 $
    $Author: mikeweideli $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRemembered_Names]
AS
  -- Update temp table with relevant data.
  UPDATE  #Names
  SET   Match_Count = 1, 
    Match_Key = Matched_Key, 
    Match_Value = dbo.ufn_GetFormattedName(Matched_Key), 
    Remembered = 1
  FROM  IW_Matched_Names 
  JOIN  [Name] ON Name_Key = Matched_Key
  WHERE   Import_Value = Matched_Value 
  AND   Match_Key IS NULL AND NAME_KEY <> Import_Value     
  
  
  UPDATE  #Names
  SET   Match_Count = 1, 
    Match_Key = I.Name_key,
    Match_Value =  I.NAME_KEY,
    Remembered = 1
  FROM  Individual I 
  JOIN  #Names On I.Name_Key = #names.import_value 
    WHERE   Match_Key IS NULL      


/****** Scripts and table entries to allow the import of location keys in the Location column of the IW. ******/

/****** Object:  StoredProcedure [dbo].[usp_IWMatchRemembered_Locations]    Script Date: 09/28/2013 21:28:04 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Populate import table with matched values from previous imports.

  Parameters: <none>

  Created:  July 2004

  Last revision information:
    $Revision: 8 $
    $Date: 28 Sep 2013 15:52 $
    $Author: mikeweideli $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRemembered_Locations]
AS
  -- Update temp table with relevant data.
  UPDATE  #Locations
  SET   Match_Count = 1, 
    Match_Key = L.Location_Key, 
    Match_Value = L.Location_Key,
    Remembered = 1,
    Spatial_Ref = L.Spatial_Ref,
    Spatial_Ref_System = L.Spatial_Ref_System,
    Lat = L.Lat,
    Long = L.Long
  FROM  IW_Matched_Locations 
  JOIN  Location L ON L.Location_Key = Matched_Key
  JOIN  Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
  WHERE   Import_Value = Matched_Value 
  AND   Match_Key IS NULL AND Import_Value <> L.LOCATION_KEY
       
    UPDATE  #Locations
  SET   Match_Count = 1, 
    Match_Key = L.Location_key, 
    Match_Value = L.Location_key,
    Remembered = 1,
    Spatial_Ref = L.Spatial_Ref,
    Spatial_Ref_System = L.Spatial_Ref_System,
    Lat = L.Lat,
    Long = L.Long
    
  
  FROM  Location L 
  JOIN #Locations ON L.Location_Key = Import_value
  JOIN  Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
  WHERE   Match_Key IS NULL 
  
Go


/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_CalcSampleSELocationName]    Script Date: 09/28/2013 21:32:52 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Sets the Location Name in #sample and #survey event to null
  in situations where the Location Name is populated with the Location key  
  Parameters:   None

  Created:      Sept 2013

  Last revision information:
    $Revision: 1 $
    $Date: 28/09/13 15:52 $
    $Author: MikeWeideli $

\*===========================================================================*/
--hier


IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id(N'[dbo].[usp_ImportWizard_CalcSampleSELocationName]') AND ObjectProperty(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ImportWizard_CalcSampleSELocationName]
GO

CREATE PROCEDURE [dbo].[usp_ImportWizard_CalcSampleSELocationName]
AS
    UPDATE      #Survey_Event
    SET         Location_Name      =   null 
                WHERE Location_Key = Location_name

    UPDATE      #Sample
    SET         Location_Name          =   null 
                WHERE Location_Key = Location_name


GO

GRANT EXECUTE ON [dbo].[usp_ImportWizard_CalcSampleSELocationName] to Public
               
GO

Delete FROM IW_Post_Processing_Procedure WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000004'

GO

INSERT INTO IW_Post_Processing_procedure (IW_Post_Processing_Procedure_Key,Sequence,Procedure_name,
Entered_By,Entry_date,System_supplied_data)
VALUES ('SYSTEM0100000004',3 ,'usp_ImportWizard_CalcSampleSELocationName','TESTDATA00000001',GetDate(),1)

GO


/****** Scripts and table entries to fix lower case errors in UK grid refs ******/

/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_TidyGridRef]    Script Date: 09/28/2013 21:58:49 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Makes sure the spatial ref on OSGB and OSNI is upper case for sample and events
  Parameters:   None

  Created:      Sept 2013

  Last revision information:
    $Revision: 1 $
    $Date: 28/09/13 15:52 $
    $Author: MikeWeideli $

\*===========================================================================*/

IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id(N'[dbo].[usp_ImportWizard_TidyGridRef]') AND ObjectProperty(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ImportWizard_TidyGridRef]
GO

CREATE PROCEDURE [dbo].[usp_ImportWizard_TidyGridRef]
AS
    UPDATE      #Survey_Event
    SET         spatial_ref = UPPER(spatial_ref)
                where Spatial_Ref_System IN ('OSGB', 'OSNI')

    UPDATE      #Sample
    SET         spatial_ref = UPPER(spatial_ref)
                where Spatial_Ref_System IN ('OSGB', 'OSNI')
                    

GO

GRANT EXECUTE ON [dbo].[usp_ImportWizard_TidyGridRef] to Public

GO

Delete FROM IW_Post_Processing_Procedure WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000005'

GO

INSERT INTO IW_Post_Processing_procedure (IW_Post_Processing_Procedure_Key,Sequence,Procedure_name,
Entered_By,Entry_date,System_supplied_data)
VALUES ('SYSTEM0100000005',4 ,'usp_ImportWizard_TidyGridRef','TESTDATA00000001',GetDate(),1)
                   
GO


/****** Scripts and table entries to fix lower case errors in UK grid refs ******/

/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_TidyGridRef]    Script Date: 09/28/2013 21:58:49 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Makes sure the spatial ref on OSGB and OSNI is upper case for locations
  Parameters:   None

  Created:      Sept 2013

  Last revision information:
    $Revision: 1 $
    $Date: 28/09/13 15:52 $
    $Author: MikeWeideli $

\*===========================================================================*/

IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id(N'[dbo].[usp_ImportWizard_TidyLocationGridRef]') AND ObjectProperty(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ImportWizard_TidyLocationGridRef]
GO

CREATE PROCEDURE [dbo].[usp_ImportWizard_TidyLocationGridRef]
AS
    UPDATE      #Location
    SET         spatial_ref = UPPER(spatial_ref)
                where Spatial_Ref_System IN ('OSGB', 'OSNI')

  
GO

GRANT EXECUTE ON [dbo].[usp_ImportWizard_TidyLocationGridRef] to Public

GO

Delete FROM IW_Post_Processing_Procedure WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000006'

GO

INSERT INTO IW_Post_Processing_procedure (IW_Post_Processing_Procedure_Key,Sequence,Required_Table_Name,Procedure_Name,
Entered_By,Entry_date,System_supplied_data)
VALUES ('SYSTEM0100000006',4 ,'Location','usp_ImportWizard_TidyLocationGridRef','TESTDATA00000001',GetDate(),1)
                   

INSERT INTO IW_Post_Processing_procedure (IW_Post_Processing_Procedure_Key,Sequence,Required_Table_Name,Procedure_Name,
Entered_By,Entry_date,System_supplied_data)
VALUES ('SYSTEM0100000006',4 ,'Location','usp_ImportWizard_TidyLocationGridRef','TESTDATA00000001',GetDate(),1)