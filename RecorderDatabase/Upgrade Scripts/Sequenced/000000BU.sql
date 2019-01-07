/********* Changes for adding Notes to IW Location Matching   *****************

1. Changes and populates field in IW_Match_Rules 

2, UDF - ConsolidateLocationNames
3. Add new stored procedures 
     usp_IWMatch_Location_Notes -  done
     usp_IWMatch_Location_Notes_Single - done 
     usp_IWNotes_Locations_Update - done
     usp_IWNotes_Locations_Select - done  
     usp_IWNotes_Locations_Detail
4. Changes Stored Procedures
      usp_IWMatch_Locations - done
      usp_IWMatchNewEntry_Location - done 
      usp_IWMatchSet_Location - done
      usp_IWMatchRemembered_Locations - done
     usp_IWMatch_Locations_Grid_Ref
      usp_IWMatch_Locations_No_Grid_Ref
*********************************************/

Update IW_MATCH_RULE 
SET Table_Create_SQL =
' CREATE TABLE #Locations(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Import_Grid_Reference VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Count INT,
 Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0,
 Spatial_Ref VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Spatial_Ref_System VARCHAR(4) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Lat FLOAT,
 Long FLOAT,
 Spatial_Ref_Qualifier VARCHAR(20) COLLATE SQL_Latin1_General_CP1_CI_AS, Notes VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS
)',
Update_Notes_Procedure = 
'usp_IWNotes_Locations_Update',
Display_Notes_Procedure =
'usp_IWNotes_Locations_Select',
Detailed_Notes_Procedure =
'usp_IWNotes_Locations_Detail'
WHERE IW_MATCH_RULE_KEY = 'SYSTEM0100000003'

GO
/****** Object:  UserDefinedFunction [dbo].[ConsolidateLocationNames]    Script Date: 12/11/2018 15:20:31 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Returns the Location name  
                and all synonyms  

  Parameters:
  @SampleKey - Sample key 
 		  
  Created:	December 2018
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[ConsolidateLocationNames]
(@LocationKey char(16) )

RETURNS varchar(8000)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)

SET @ReturnString = ''

Select @ReturnString = @ReturnString + LN.Item_Name + ';' 
FROM Location_name LN where Location_Key = @LocationKey
ORDER BY LN.PREFERRED DESC

RETURN @ReturnString

END

GO

GRANT EXECUTE ON [dbo].[ConsolidateLocationNames] TO PUBLIC

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Location_Notes]    Script Date: 12/10/2018 10:11:39 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the notes for IW imported data  

  Created:	Nov 2018 

    
  Populates the notes column 
\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_IWMatch_Location_Notes]

AS
    
 Update #Locations set Notes = 'Matched'
 WHERE match_key is not null and remembered = 0
  
	
 Update #Locations set Notes = 'Remembered matches' 
 WHERE  match_key is not null and remembered = 1
	
 Update #Locations set Notes =  '' 
 WHERE  Match_Key is null  
 
 Update #Locations set Notes = ltrim(str(Match_Count)) + ' possible matches ' 
 WHERE  Match_Key is null and Match_Count > 0

GO

GRANT EXECUTE ON [dbo].[usp_IWMatch_Location_Notes] TO PUBLIC


GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Location_Notes_Single]    Script Date: 12/10/2018 16:25:01 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the notes for IW imported data  

  Created:	Nov 2018 
  For a single IW Import Value/Grid Ref
    
  Populates the notes column 
\*===========================================================================*/


CREATE PROCEDURE [dbo].[usp_IWMatch_Location_Notes_Single]
@ImportValue varchar(100)
AS
    
 Update #Locations set Notes = 'Matched'
 WHERE match_key is not null and remembered = 0
 AND Import_Value = @ImportValue
     
	
 Update #Locations set Notes = 'Remembered matches' 
 WHERE  match_key is not null and remembered = 1
 AND Import_Value = @ImportValue
 
 Update #Locations set Notes =  '' 
 WHERE  Match_Key is null  
AND Import_Value = @ImportValue
 
 Update #Locations set Notes = ltrim(str(Match_Count)) + ' possible matches ' 
 WHERE  Match_Key is null and Match_Count > 0
 AND Import_Value = @ImportValue

GO

GRANT EXECUTE ON [dbo].[usp_IWMatch_Location_Notes_Single] TO PUBLIC


GO
/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Locations_Select]    Script Date: 12/22/2018 20:25:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[usp_IWNotes_Locations_Select]
@Key varchar(100)
AS
  Declare @MatchedCount integer,@Remembered bit, @ManualMatch bit
  Select @MatchedCount = Match_Count from #Locations where Import_Value = @Key   
  Select @Remembered = Remembered from #Locations where Import_Value = @Key   
  Select @ManualMatch =  Manual_Match from #Locations where Import_Value = @Key   

  IF  @MatchedCount = 0 Or @Remembered = 1 Or @ManualMatch = 1 
  BEGIN
   SELECT LOCATION.LOCATION_KEY AS AKEY, '---- ' + LOCATION_NAME.ITEM_NAME +
   ISNULL(' ' + LOCATION.SPATIAL_REF,'')+
    ' - ' + LT.SHORT_NAME AS FULLDETAILS,ISNULL(#Locations.Match_Key,'') As MatchKey,
   0 As possible 
   FROM LOCATION INNER JOIN LOCATION_NAME 
   ON LOCATION_NAME.LOCATION_KEY =  LOCATION.LOCATION_KEY AND LOCATION_NAME.PREFERRED = 1  
   INNER JOIN LOCATION_TYPE LT ON LT.LOCATION_TYPE_KEY =
   LOCATION.LOCATION_TYPE_KEY 
   LEFT JOIN #Locations ON #Locations.Import_Value = @Key  
   WHERE 
   LOCATION.LOCATION_KEY IN(SELECT LN.LOCATION_KEY FROM 
   LOCATION_NAME LN WHERE LN.ITEM_NAME = @KEY OR LEFT(LN.ITEM_NAME,LEN(@KEY)) = @Key) OR
   LOCATION.FILE_CODE = @KEY 
  
   UNION 
   SELECT LOCATION.LOCATION_KEY AS AKEY,  LOCATION_NAME.ITEM_NAME +
   ISNULL(' ' + LOCATION.SPATIAL_REF,'')+
    ' - ' + LT.SHORT_NAME AS FULLDETAILS,ISNULL(#Locations.Match_Key,'') As MatchKey,
   1 As possible 
   FROM LOCATION INNER JOIN LOCATION_NAME 
   ON LOCATION_NAME.LOCATION_KEY =  LOCATION.LOCATION_KEY AND LOCATION_NAME.PREFERRED = 1  
   INNER JOIN LOCATION_TYPE LT ON LT.LOCATION_TYPE_KEY =
   LOCATION.LOCATION_TYPE_KEY  
   LEFT JOIN #Locations ON #Locations.Import_Value = @Key
   WHERE 
   LOCATION.LOCATION_KEY IN(SELECT LN.LOCATION_KEY FROM 
   LOCATION_NAME LN WHERE LN.ITEM_NAME = @Key) 
   ORDER BY possible  DESC ,FULLDETAILS  
  END
  ELSE 
  BEGIN
   SELECT LOCATION.LOCATION_KEY AS AKEY,  LOCATION_NAME.ITEM_NAME +
   ISNULL(' ' + LOCATION.SPATIAL_REF,'')+
    ' - ' + LT.SHORT_NAME AS FULLDETAILS,ISNULL(#Locations.Match_Key,'') As MatchKey,
   1 As possible 
   FROM LOCATION INNER JOIN LOCATION_NAME 
   ON LOCATION_NAME.LOCATION_KEY =  LOCATION.LOCATION_KEY AND LOCATION_NAME.PREFERRED = 1  
   INNER JOIN LOCATION_TYPE LT ON LT.LOCATION_TYPE_KEY =
   LOCATION.LOCATION_TYPE_KEY
   LEFT JOIN #Locations ON #Locations.Import_Value = @Key
   WHERE 
   LOCATION.LOCATION_KEY IN(SELECT LN.LOCATION_KEY FROM 
   LOCATION_NAME LN WHERE LN.ITEM_NAME = @KEY)  
   ORDER BY FULLDETAILS
  END 
  
GO

GRANT EXECUTE ON [dbo].[usp_IWNotes_Locations_Select] TO PUBLIC

GO

/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Locations_Update]    Script Date: 12/11/2018 14:09:14 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Updates the #table following 
  selction from the notes column 
  Parameters @ImportValue 
			 @MatchCount
			 @MatchKey
             @ManualMatch
             @Remembered
            
             

  Created:	November 2018 
 

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWNotes_Locations_Update] 
@ImportValue VARCHAR(100),
@MatchCount INT,
@MatchKey CHAR(16),
@ManualMatch BIT,
@Remembered BIT

AS
  UPDATE	#Locations
		SET	Match_Value = LN.Item_Name,
			Match_Key = @MatchKey,
			Match_Count = @MatchCount,
			Manual_Match = @ManualMatch,
			Remembered = @Remembered,
			Spatial_Ref = L.Spatial_Ref,
			Spatial_Ref_System = L.Spatial_Ref_System,
			Lat = L.Lat,
			Long = L.Long,
			Notes = 'Matched'
		FROM	Location L
		JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
		WHERE	L.Location_Key = @MatchKey
		AND	Import_Value = @ImportValue

  DELETE FROM IW_Matched_Locations WHERE 
      Matched_Value = @ImportValue 
      AND Matched_Key <> @MatchKey


GO
  GRANT EXECUTE ON [dbo].[usp_IWNotes_Locations_Update] TO PUBLIC 
GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Locations]    Script Date: 12/10/2018 10:02:06 ******/
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

  Updated March 2016 for Mantis 447
          December 2018 for IW notes
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatch_Locations]
AS
	
	IF Exists(SELECT * FROM tempdb.INFORMATION_SCHEMA.COLUMNS 
    WHERE OBJECT_ID('tempdb..' + table_name)=OBJECT_ID('tempdb..#Master')
    and COLUMN_NAME like 'System0100000001_Spatial_ref%') 
    EXEC [dbo].[usp_IWMatch_Locations_Grid_Ref]
    ELSE 
    EXEC [dbo].[usp_IWMatch_Locations_No_Grid_Ref]

    EXEC [dbo].[usp_IWMatch_Location_Notes]

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchNewEntry_Location]    Script Date: 12/10/2018 10:41:33 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Create a new location from an import value.

  Parameters:	
	@ImportValue	The name of the location.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchNewEntry_Location]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS

	DECLARE	@Key char(16),
		@LNKey char(16)

	/*===================================================*\
	  Now create new Location and Location Name records
	\*===================================================*/
	BEGIN TRANSACTION
		EXECUTE spNextKey 'Location', @Key OUTPUT

		INSERT INTO Location (
			Location_Key, Location_Type_Key, Entered_By,
			Spatial_Ref, Spatial_Ref_System, Lat, Long, Spatial_Ref_Qualifier
		)
		SELECT	@Key, 'NBNSYS0000000001', @EnteredBy,
			Spatial_Ref, Spatial_Ref_System, Lat, Long, Spatial_Ref_Qualifier			
		FROM	#Locations
		WHERE	Import_Value = @ImportValue

		IF @@Error <> 0 GOTO RollbackAndExit

		EXECUTE spNextKey 'Location_Name', @LNKey OUTPUT
		INSERT INTO Location_Name (
			Location_Name_Key, Item_Name, Location_Key, Preferred, Entered_By
		) VALUES (
			@LNKey, @ImportValue, @Key, 1, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#Locations
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			Notes = 'Matched'
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	
GO

GRANT EXECUTE ON [dbo].[usp_IWNotes_Locations_Update] TO PUBLIC
 
GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchSet_Location]    Script Date: 12/10/2018 10:46:02 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ImportValue
		@MatchKey

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchSet_Location]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#Locations
		SET	Match_Value = LN.Item_Name,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			Spatial_Ref = L.Spatial_Ref,
			Spatial_Ref_System = L.Spatial_Ref_System,
			Lat = L.Lat,
			Long = L.Long,
			Notes = 'Matched'
		FROM	Location L
		JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
		WHERE	L.Location_Key = @MatchKey
		AND	Import_Value = @ImportValue
	ELSE
		UPDATE	#Locations
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			Spatial_Ref = NULL,
			Spatial_Ref_System = NULL,
			Lat = NULL,
			Long = NULL,
			Notes = ''
			
		WHERE	Import_Value = @ImportValue

GO
 /****** Object:  StoredProcedure [dbo].[usp_IWMatchRemembered_Locations]    Script Date: 12/16/2018 17:00:01 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [dbo].[usp_IWMatchRemembered_Locations]
AS
	-- Update temp table with relevant data.
	UPDATE 	#Locations
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = LN.Item_Name,
		Remembered = 1,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
	FROM 	IW_Matched_Locations 
	JOIN	Location L ON L.Location_Key = Matched_Key
	JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL AND Import_Value <> L.LOCATION_KEY
	AND  [dbo].[ufn_Location_Expired](L.Location_Key) = 0    
    
    UPDATE 	#Locations
	SET 	Match_Count = 1, 
		Match_Key = L.Location_key, 
		Match_Value = L.Location_key,
		Remembered = 1,
		Spatial_Ref = L.Spatial_Ref,
		Spatial_Ref_System = L.Spatial_Ref_System,
		Lat = L.Lat,
		Long = L.Long
	FROM 	Location L 
	JOIN #Locations ON L.Location_Key = Import_value
	JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE 	Match_Key IS NULL AND  [dbo].[ufn_Location_Expired](L.Location_Key) = 0  

    EXECUTE dbo.usp_IWMatch_Location_Notes
    
GO
/****** Object:  StoredProcedure [dbo].[usp_IW_Imported_Insert_Locations]    Script Date: 12/10/2018 10:56:02 ******/
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
ALTER PROCEDURE [dbo].[usp_IW_Imported_Insert_Locations]
AS
	IF EXISTS(SELECT * FROM tempdb.INFORMATION_SCHEMA.COLUMNS 
      WHERE OBJECT_ID('tempdb..' + table_name)=OBJECT_ID('tempdb..#Master')
      and COLUMN_NAME like 'System0100000001_Spatial_ref%')
 
    exec usp_IW_Imported_Insert_Locations_Grid_Ref

    Else
    
     exec dbo.usp_IW_Imported_Insert_Locations_No_Grid_Ref
     
EXEC dbo.usp_IWMatch_Location_Notes

GO
/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Locations_Detail]    Script Date: 12/21/2018 20:04:15 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Returns additional information on a name
                    selected for possible matching 
  Parameters: @Key Location_Key @Level The amount of detail required 
  Created:	November 2018 
 
  Note - Done with repeated code, because implementations using IF seem to 
  be much slower in both instances. 
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWNotes_Locations_Detail]
(@Key varchar(16),@Detail int, @ImportValue varchar(100))


AS
  DECLARE @EOL CHAR(2)
  SET @EOL = CHAR(13) + CHAR(10)  
  
  IF @Detail = 0 
  BEGIN 
    SELECT L.Location_Key as AKey,
    'Is current match : '  
    + Case when #Locations.MATCH_KEY = @Key  then 'Yes'
          else 'No'
      End 
    + @EOL  
    + 'Active Dates : '  
    + [dbo].[ufn_Location_Date_Active](@Key) 
    + @EOL 
    + 'Location Name : ' +  LN.ITEM_NAME
    + @EOL
    + 'All Names : '  
    + [dbo].[ConsolidateLocationNames](@Key)
    + @EOL 
    + 'Grid Ref : ' +  L.Spatial_Ref 
    + @EOL
    + 'Spatial Type : ' + L.Spatial_ref_Qualifier
    + @EOL
    + 'File Code : ' + isnull(L.File_Code,'')
    + @EOL
    + 'Location Key : ' + L.Location_Key
    + @EOL
    + 'Custodian : ' + L.Custodian
    + @EOL
    + 'Location Type : ' + LT.SHORT_NAME 
    + @EOL
    + 'Parent : ' + ISNULL(LN2.ITEM_NAME,'')
    AS DETAILS 
    FROM LOCATION L INNER JOIN LOCATION_NAME LN ON 
    LN.LOCATION_KEY = L.LOCATION_KEY AND
    LN.PREFERRED = 1
    INNER JOIN #LOCATIONS ON #LOCATIONS.IMPORT_VALUE = @ImportValue 
    INNER JOIN LOCATION_TYPE LT ON LT.LOCATION_TYPE_KEY
    = L.LOCATION_TYPE_KEY 
    LEFT JOIN  LOCATION_NAME LN2 ON LN2.LOCATION_KEY =
    L.PARENT_KEY AND LN2.PREFERRED = 1
    WHERE L.LOCATION_KEY = @key
    
  END  
  BEGIN
   SELECT L.Location_Key as AKey,
    'Is current match : '  
    + Case when #Locations.MATCH_KEY = @Key  then 'Yes'
          else 'No'
      End 
    + @EOL 
    + 'Location Name : ' +  LN.ITEM_NAME
    + @EOL
    + 'Active Dates : '  
    + [dbo].[ufn_Location_Date_Active](@Key) 
    + @EOL 
    + 'All Names : '  
    + [dbo].[ConsolidateLocationNames](@Key)
    + @EOL 
    + 'Grid Ref : ' +  L.Spatial_Ref 
    + @EOL
    + 'Spatial Type : ' + L.Spatial_ref_Qualifier
    + @EOL
    + 'File Code : ' + isnull(L.File_Code,'')
    + @EOL
    + 'Location Key : ' + L.Location_Key
    + @EOL
    + 'Custodian : ' + L.Custodian
    + @EOL
    + 'Location Type : ' + LT.SHORT_NAME 
    + @EOL 
    + 'Top Parent : ' + dbo.LCLocationTopParent(L.Location_Key,0)
    + @EOL
    + 'Parent : ' + ISNULL(LN2.ITEM_NAME,'')
    AS DETAILS 
    FROM LOCATION L INNER JOIN LOCATION_NAME LN ON 
    LN.LOCATION_KEY = L.LOCATION_KEY AND
    LN.PREFERRED = 1
    INNER JOIN #LOCATIONS ON #LOCATIONS.IMPORT_VALUE = @ImportValue 
    INNER JOIN LOCATION_TYPE LT ON LT.LOCATION_TYPE_KEY
    = L.LOCATION_TYPE_KEY 
    LEFT JOIN LOCATION_NAME LN2 ON LN2.LOCATION_KEY =
    L.PARENT_KEY AND LN2.PREFERRED = 1
    WHERE L.LOCATION_KEY = @key
  END 
    
    
GO

  GRANT EXECUTE ON [dbo].[usp_IWNotes_Locations_Detail] TO PUBLIC  
  

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Locations_No_Grid_Ref]    Script Date: 12/16/2018 17:06:24 ******/
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
ALTER PROCEDURE [dbo].[usp_IWMatch_Locations_No_Grid_Ref]
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
	-- even if only one dont want an automatic match if is is not an active location 
	AND [dbo].[ufn_Location_Expired](L.Location_Key)= 0 
	AND	Match_Key IS NULL
	AND	M.SYSTEM0100000000_Data = Import_Value


