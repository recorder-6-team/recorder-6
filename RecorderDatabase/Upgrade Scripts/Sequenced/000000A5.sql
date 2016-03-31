/****** Mantis 447 ******/

/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Locations_No_Grid_Ref]    Script Date: 11/12/2015 18:41:30 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON

GO
/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.
  Based only on Location and because grif ref is not available

  Parameters:	<none>

  Created:	November 2005 

  Mike Weideli

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Locations_No_Grid_Ref]
AS
	
	-- Set Match_Count first.
	UPDATE	#Locations
	SET	Match_Count =  (SELECT	Count(DISTINCT L.Location_Key)
				FROM	#Master M
				JOIN	Location_Name LN ON LN.Item_Name = M.SYSTEM0100000000_Data
				JOIN	Location L ON L.Location_Key = LN.Location_Key
				JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
				WHERE	M.SYSTEM0100000000_Data = Import_Value
	)
	WHERE	Match_Key IS NULL
	
	-- Now get values and keys for unique matches only.
	UPDATE	#Locations
	SET	Match_Key = L.Location_Key,
		Match_Value = LN2.Item_Name,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
	FROM	#Master M
	JOIN	Location_Name LN ON LN.Item_Name = M.SYSTEM0100000000_Data
	JOIN	Location L ON L.Location_Key = LN.Location_Key
	JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	M.SYSTEM0100000000_Data = Import_Value

GO

GRANT EXECUTE ON [dbo].[usp_IWMatch_Locations_No_Grid_Ref] TO PUBLIC

GO

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Locations_Grid_Ref]    Script Date: 11/16/2015 14:06:05 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 13/07/04 15:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_Locations_Grid_Ref]
AS
	DECLARE @EarthRadiusKM float
	SET 	@EarthRadiusKM = 6378
		
	-- Set Match_Count first.
	UPDATE	#Locations
	SET	Match_Count =  (SELECT	Count(DISTINCT L.Location_Key)
				FROM	#Master M
				JOIN	Location_Name LN ON LN.Item_Name = M.SYSTEM0100000000_Data
				JOIN	Location L ON L.Location_Key = LN.Location_Key
				JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
				WHERE	M.SYSTEM0100000000_Data = Import_Value
				-- next is a safety check to avoid domain errors on ACOS
				AND COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
						COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
						SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat)) BETWEEN -1 AND 1
				AND	@EarthRadiusKM * ACOS(COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
						COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
						SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat))) < 2
				)
	WHERE	Match_Key IS NULL
	
	-- Now get values and keys for unique matches only.
	UPDATE	#Locations
	SET	Match_Key = L.Location_Key,
		Match_Value = LN2.Item_Name,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
	FROM	#Master M
	JOIN	Location_Name LN ON LN.Item_Name = M.SYSTEM0100000000_Data
	JOIN	Location L ON L.Location_Key = LN.Location_Key
	JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
	WHERE	Match_Count = 1
	AND	Match_Key IS NULL
	AND	M.SYSTEM0100000000_Data = Import_Value
	-- next is a safety check to avoid domain errors on ACOS
	AND COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
			COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
			SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat)) BETWEEN -1 AND 1
	AND	@EarthRadiusKM * ACOS(COS(RADIANS(M.SYSTEM0100000001_Lat)) * COS(RADIANS(L.Lat)) * 
			COS(RADIANS(M.SYSTEM0100000001_Long) - RADIANS(L.Long)) + 
			SIN(RADIANS(M.SYSTEM0100000001_Lat)) * SIN(RADIANS(L.Lat))) < 2

GO

Grant Execute ON [dbo].[usp_IWMatch_Locations_Grid_Ref] TO Public


GO
/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_TidyGridRef]    Script Date: 11/12/2015 18:43:58 ******/
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
ALTER PROCEDURE [dbo].[usp_ImportWizard_TidyGridRef]
AS
    UPDATE      #Survey_Event
    SET         spatial_ref = UPPER(spatial_ref)
                where Spatial_Ref_System IN ('OSGB', 'OSNI')

    UPDATE      #Sample
    SET         spatial_ref = UPPER(spatial_ref)
                where Spatial_Ref_System IN ('OSGB', 'OSNI')
                    

    UPDATE      #Sample
    SET         Lat = null, Long = null, 
                Spatial_Ref_System = 'Grid' 
                where isnull(Spatial_Ref_System,'') = ''
                
    
    UPDATE      #Survey_Event
    SET         Lat = null, Long = null, 
                Spatial_Ref_System = 'Grid' 
                where isnull(Spatial_Ref_System,'') = ''
                
GO

USE [NBNData]
GO
/****** Object:  StoredProcedure [dbo].[usp_IW_UpdateSampleGridRefsFromExistingLocations]    Script Date: 11/12/2015 18:46:34 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Transfers the spatail ref from a Location where the spatial ref does not have one
  Parameters:   None

  Created:      Nov 2015

  Last revision information:
  $Author: MikeWeideli $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IW_UpdateSampleGridRefsFromExistingLocations]
AS
    UPDATE     #Sample 
    SET         Spatial_ref = Location.Spatial_ref,
                Spatial_ref_system = Location.Spatial_ref_System,
                Spatial_ref_qualifier = Location.Spatial_ref_Qualifier,   
                Lat = Location.Lat,
                Long = Location.Long  
    FROM #Sample INNER JOIN Location ON
    Location.Location_Key = #Sample.Location_Key
    WHERE isnull(#Sample.Spatial_Ref,'') = '' 
 
                
GO

GRANT EXECUTE ON [dbo].[usp_IW_UpdateSampleGridRefsFromExistingLocations] TO PUBLIC

GO
/****** Object:  StoredProcedure [dbo].[usp_IW_UpdateSampleGridRefsFromNewLocations]    Script Date: 11/12/2015 18:47:29 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Transfers the spatial ref from a Location where the spatial ref does not have one
  Parameters:   None

  Created:      Nov 2015

  Last revision information:
  $Author: MikeWeideli $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IW_UpdateSampleGridRefsFromNewLocations]
AS
    UPDATE     #Sample 
    SET         Spatial_ref = #Location.Spatial_ref,
                Spatial_ref_system = #Location.Spatial_ref_System,
                Spatial_ref_qualifier = #Location.Spatial_ref_Qualifier,   
                Lat = #Location.Lat,
                Long = #Location.Long  
    FROM #Sample INNER JOIN #Location ON
    #Location.Location_Key = #Sample.Location_Key
    WHERE isnull(#Sample.Spatial_Ref,'') = '' 
          
GO

GRANT EXECUTE ON [dbo].[usp_IW_UpdateSampleGridRefsFromNewLocations] TO PUBLIC 

GO



UPDATE IW_Match_Rule SET Imported_Data_Insert_SQL =
  'usp_IW_Imported_Insert_Locations'  
   WHERE IW_Match_Rule_Key = 'SYSTEM0100000003'
  
GO
    
INSERT INTO IW_Post_Processing_Procedure
(IW_Post_Processing_Procedure_Key,Sequence,Required_Table_Name,Procedure_Name,
Entered_By, Entry_Date,System_Supplied_Data)
VALUES ('SYSTEM0100000007',3,'Location','usp_IW_UpdateSampleGridRefsFromNewLocations',
'TESTDATA00000001',GetDate(),1)

GO
 
INSERT INTO IW_Post_Processing_Procedure
(IW_Post_Processing_Procedure_Key,Sequence,Required_Table_Name,Procedure_Name,
Entered_By, Entry_Date,System_Supplied_Data)
VALUES ('SYSTEM0100000009',4,'Sample','usp_IW_UpdateSampleGridRefsFromExistingLocations',
'TESTDATA00000001',GetDate(),1)
  
GO

UPDATE IW_Post_Processing_Procedure Set Sequence = 8
WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000002'

GO

UPDATE IW_Post_Processing_Procedure Set Sequence = 5
WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000004'

GO

UPDATE IW_Post_Processing_Procedure Set Sequence = 6
WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000005'

GO

UPDATE IW_Post_Processing_Procedure Set Sequence = 7
WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000006'

GO

UPDATE IW_Post_Processing_Procedure Set Sequence = 3
WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000007'  

GO

UPDATE IW_Post_Processing_Procedure Set Sequence = 4
WHERE IW_Post_Processing_Procedure_Key = 'SYSTEM0100000009'       

GO

INSERT INTO IW_Column_Type_Relationship(IW_Column_Type_Key,Related_IW_Column_Type_Key,
Relationship_Type,Entered_By,Entry_Date,System_Supplied_Data)
Values ('SYSTEM0100000001','SYSTEM0100000000',5,'TESTDATA00000001',
GETDATE(),1)

Update IW_Column_Type_Relationship set Relationship_Type = 5
WHERE IW_Column_Type_Key = 'SYSTEM0100000001' 
and Related_IW_Column_Type_Key = 'SYSTEM0100000000' 

Update IW_Column_Type_Relationship set Relationship_Type = 5
WHERE IW_Column_Type_Key = 'SYSTEM0100000001' 
and Related_IW_Column_Type_Key = 'SYSTEM010000000T' 

Update IW_Column_Type_Relationship set Relationship_Type = 5
WHERE IW_Column_Type_Key = 'SYSTEM010000000T '
and Related_IW_Column_Type_Key = 'SYSTEM0100000001' 

Update IW_Column_Type_Relationship set Relationship_Type = 5
WHERE IW_Column_Type_Key = 'SYSTEM0100000000' 
and Related_IW_Column_Type_Key = 'SYSTEM0100000001' 


GO
/****** Object:  StoredProcedure [dbo].[usp_IW_Imported_Insert_Locations_Grid_Ref]    Script Date: 11/16/2015 14:12:42 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	Nov 2015

  $Author: MikeWeideli $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IW_Imported_Insert_Locations_Grid_Ref]
AS
	DECLARE @GridRefs varchar(100), @LocationName varchar(100)
	DECLARE @CRLF char(2)
	SET @CRLF=CHAR(13)+CHAR(10)
	DECLARE curLocations CURSOR FOR
		SELECT Import_Value FROM #Locations    
	OPEN curLocations
	FETCH NEXT FROM curLocations INTO @LocationName  
	WHILE @@Fetch_Status = 0 BEGIN
        
        SET @GridRefs = ''
       
        IF EXISTS(SELECT * FROM tempdb.INFORMATION_SCHEMA.COLUMNS 
          WHERE OBJECT_ID('tempdb..' + table_name)=OBJECT_ID('tempdb..#Master')
          and COLUMN_NAME like 'System0100000001_Spatial_ref%')
        
          SELECT @GridRefs = CASE 
		  WHEN CharIndex(SYSTEM0100000001_Spatial_Ref, @GridRefs) = 0 
	  	  THEN @GridRefs + @CRLF + SYSTEM0100000001_Spatial_Ref + '|' + SYSTEM0100000001_system
		   ELSE @GridRefs END
		   FROM #Master
	       WHERE SYSTEM0100000000_Data = @LocationName     -- Remove superfluous leading carriage return.
  	     
  	     ELSE
  	       SET @GridRefs = '|'
  	     	
		   
         IF SubString(@GridRefs, 1, 2) = @CRLF
	     SET @GridRefs = SubString(@GridRefs, 3, Len(@GridRefs))     -- Update field in match table.
		
         UPDATE #Locations
	     SET Import_Grid_Reference = @GridRefs
		 WHERE Import_Value = @LocationName
		
		 FETCH NEXT FROM curLocations INTO @LocationName
	END
	CLOSE curLocations
	DEALLOCATE curLocations
	
GO

GRANT EXECUTE ON [dbo].[usp_IW_Imported_Insert_Locations_Grid_Ref] TO PUBLIC

GO
	
GO
/****** Object:  StoredProcedure [dbo].[usp_IW_Imported_Insert_Locations_No_Grid_ref]    Script Date: 11/16/2015 14:14:03 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.
  Used where there is no spatial ref column 
  Parameters:	<none>

  Created:	Nov 2015

  $Author: MikeWeideli $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IW_Imported_Insert_Locations_No_Grid_ref]
AS
	DECLARE @GridRefs varchar(100), @LocationName varchar(100)
	DECLARE @CRLF char(2)
	SET @CRLF=CHAR(13)+CHAR(10)
	DECLARE curLocations CURSOR FOR
		SELECT Import_Value FROM #Locations    
	OPEN curLocations
	FETCH NEXT FROM curLocations INTO @LocationName  
	WHILE @@Fetch_Status = 0 BEGIN
        
        SET @GridRefs = '|'
         UPDATE #Locations
	     SET Import_Grid_Reference = @GridRefs
		 WHERE Import_Value = @LocationName
		
		 FETCH NEXT FROM curLocations INTO @LocationName
	END
	CLOSE curLocations
	DEALLOCATE curLocations
	
GO

GRANT EXECUTE ON [dbo].[usp_IW_Imported_Insert_Locations_No_Grid_ref] TO PUBLIC

GO

/****** Object:  StoredProcedure [dbo].[usp_IW_Imported_Insert_Locations]    Script Date: 11/16/2015 14:11:18 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	Nov 2015

  $Author: MikeWeideli $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IW_Imported_Insert_Locations]
AS
	IF EXISTS(SELECT * FROM tempdb.INFORMATION_SCHEMA.COLUMNS 
      WHERE OBJECT_ID('tempdb..' + table_name)=OBJECT_ID('tempdb..#Master')
      and COLUMN_NAME like 'System0100000001_Spatial_ref%')
 
    exec usp_IW_Imported_Insert_Locations_Grid_Ref

    Else
    
     exec usp_IW_Imported_Insert_Locations_No_Grid_Ref
     
GO     
     
GRANT EXECUTE ON [dbo].[usp_IW_Imported_Insert_Locations] TO PUBLIC
	
	


GO

/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Locations]    Script Date: 03/30/2016 20:15:17 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.
  if a Grid Ref column exists and is populated then uses  
  [dbo].[usp_IWMatch_Locations_Grid_Ref]
  otherwise uses [dbo].[usp_IWMatch_Locations_No_Grid_Ref]
  Parameters:	<none>[

  Updated Mrach 2016 for Mantis 447

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatch_Locations]
AS
	
	IF Exists(SELECT * FROM tempdb.INFORMATION_SCHEMA.COLUMNS 
    WHERE OBJECT_ID('tempdb..' + table_name)=OBJECT_ID('tempdb..#Master')
    and COLUMN_NAME like 'System0100000001_Spatial_ref%') 
    EXEC [dbo].[usp_IWMatch_Locations_Grid_Ref]
    ELSE 
    EXEC [dbo].[usp_IWMatch_Locations_No_Grid_Ref]
