/**********Implement all the SQl changes for IW Notes and Species *\
List of all changes made by this SQL
Alters table ITN
Alters [dbo].[usp_Index_Taxon_Name_Populate]
       [dbo].[usp_Index_Taxon_Name_Apply_Preferred_Taxa]
       [dbo].[usp_IndexTaxonName_ApplyNameServer_SingleRecord]
       [dbo].[usp_IndexTaxonName_ApplySorts]
       [dbo].[ufn_GetDeprecated]
 

 Implement all the SQl changes for IW Notes and Species 

 Changes to IW_Match_Rule Table 
 

 Add three fields and populate

-- Changes/new to udf   

[dbo].[FormatIndividualFull]  
[dbo].[ufn_GetFullSpeciesName]
[dbo].[ufn_GetFormattedName]
[dbo].[ufn_GetDatesRecording]
[dbo].[LCGroupsRecorded]
[dbo].[ufn_GetCountofRecords]
[dbo].[[dbo].[ufn_Location_Expired]]
[dbo].[ufn_Location_Expired]
[dbo].[ufn_CompareNames]

-- Changes/new for stored procedures

[dbo].[usp_IWMatch_Names_Notes]
[dbo].[usp_IWMatch_Names_Notes_Single]
[dbo].[usp_IWMatch_Species_Notes]
[dbo].[usp_IWMatch_Species_Notes_Single]
[dbo].[usp_IWMatch_Species_Order]
[dbo].[usp_IWMatch_Species_Order_Single]
[dbo].[usp_IWNotes_Species_Select]
[dbo].[usp_IW_Names_Multi_Update]
[dbo].[usp_IWNotes_Name_Detail]
[dbo].[usp_IWNotes_Names_Select]
[dbo].[usp_IWCheckRecoverableMatches_Species]
[dbo].[usp_IWMatchRecovered_Species]
[dbo].[usp_IWMatchClear_Species]
[dbo].[usp_IWMatchNewEntry_Name]
[dbo].[usp_IWMatchRecord_Names]
[dbo].[usp_IWMatchRecord_Species]
[dbo].[usp_IWMatchRemembered_Names]
[dbo].[usp_IWMatchRemembered_Species]
{dbo].[usp_IWMatchRule_Select]
[dbo].[usp_IWMatchSet_Name]
[dbo].[usp_IWMatch_Species]
[dbo].[usp_IWMatch_Names]
[dbo].[usp_IW_Species_Multi_Update]
[dbo].[usp_IWMatchSet_Species]
[dbo].[usp_IWNotes_Species_Detail]
[dbo].[usp_IWCheckUnconfirmedMatches_Species] 
[dbo].[usp_IWMatchSet_Species]

\*===========================================================================*/

ALTER TABLE IW_MATCH_RULE
ADD Update_Notes_Procedure varchar(50)
GO

ALTER TABLE IW_MATCH_RULE
ADD Display_Notes_Procedure varchar(50)

GO

ALTER TABLE IW_MATCH_RULE
ADD  Detailed_Notes_Procedure varchar(50)

GO

UPDATE IW_MATCH_RULE SET Update_Notes_Procedure = 'usp_IW_Species_Multi_Update'
WHERE IW_MATCH_RULE_KEY = 'SYSTEM0100000001'
GO

UPDATE IW_MATCH_RULE SET Display_Notes_Procedure = 'usp_IWNotes_Species_Select'
WHERE IW_MATCH_RULE_KEY = 'SYSTEM0100000001'
GO

UPDATE IW_MATCH_RULE SET Detailed_Notes_Procedure = 'usp_IWNotes_Species_Detail'
WHERE IW_MATCH_RULE_KEY = 'SYSTEM0100000001'

GO

UPDATE IW_MATCH_RULE SET Update_Notes_Procedure = 'usp_IW_Names_Multi_Update'
WHERE IW_MATCH_RULE_KEY = 'SYSTEM0100000000'

GO

UPDATE IW_MATCH_RULE SET Display_Notes_Procedure = 'usp_IWNotes_Names_Select'
WHERE IW_MATCH_RULE_KEY = 'SYSTEM0100000000'

GO

UPDATE IW_MATCH_RULE SET Detailed_Notes_Procedure = 'usp_IWNotes_Name_Detail'
WHERE IW_MATCH_RULE_KEY = 'SYSTEM0100000000'

GO

UPDATE IW_MATCH_RULE SET TABLE_CREATE_SQL =
  'CREATE TABLE #Names(
   Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
   Match_Count INT,
   Match_Value VARCHAR(60) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Manual_Match BIT DEFAULT 0,
   Remembered BIT DEFAULT 0,
   Notes VARCHAR(100)  COLLATE SQL_Latin1_General_CP1_CI_AS,
   Title Varchar(4) COLLATE SQL_Latin1_General_CP1_CI_AS,
   ForeName VARCHAR (20) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Initials VARCHAR (8) COLLATE SQL_Latin1_General_CP1_CI_AS,
   Surname VARCHAR (30) COLLATE SQL_Latin1_General_CP1_CI_AS 
   )'
   
   WHERE IW_MATCH_RULE_KEY = 'SYSTEM0100000000'
   
GO
UPDATE IW_MATCH_RULE SET TABLE_CREATE_SQL =
 'CREATE TABLE #Species(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0,
 [Order] VARCHAR(60) COLLATE SQL_Latin1_General_CP1_CI_AS,
 CheckList VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Checklist_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Species_Name VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Notes VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,status INTEGER)'
 WHERE IW_MATCH_RULE_KEY = 'SYSTEM0100000001'
   
GO   

UPDATE TAXON_LIST SET ITEM_NAME =  '  View - COLEOPTERA'
WHERE TAXON_LIST_KEY = 'VIRTUAL_COLEOPTE'   

GO

/****** Object:  View [dbo].[VIRTUAL_COLEOPTE]    Script Date: 12/05/2018 21:26:39 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

ALTER VIEW [dbo].[VIRTUAL_COLEOPTE]
AS
SELECT     ITN.TAXON_LIST_ITEM_KEY, 'VIRTUAL_COLEOPTE' AS TAXON_LIST_VERSION_KEY, ITN.TAXON_VERSION_KEY, 
                      ITN.TAXON_LIST_ITEM_KEY AS PREFERRED_NAME, NULL AS TAXON_LIST_VERSION_TO, O.Sort_Code, ITN2.TAXON_LIST_ITEM_KEY AS PARENT, 
                      O.ORGANISM_RANK_KEY AS TAXON_RANK_KEY, O.ENTERED_BY, O.ENTRY_DATE, O.SYSTEM_SUPPLIED_DATA, O.Has_Children
FROM         dbo.ORGANISM AS O INNER JOIN
                      dbo.INDEX_TAXON_NAME AS ITN ON O.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY AND 
                      ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY AND ITN.CAN_EXPAND = 1 INNER JOIN
                      dbo.ORGANISM AS O2 ON O2.ORGANISM_KEY = O.PARENT_KEY INNER JOIN
                      dbo.INDEX_TAXON_NAME AS ITN2 ON O2.TAXON_VERSION_KEY = ITN2.TAXON_VERSION_KEY AND 
                      ITN2.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN2.TAXON_LIST_ITEM_KEY INNER JOIN
                      dbo.Index_Virtual_Lists AS IVL ON IVL.Taxon_List_key = 'VIRTUAL_COLEOPTE' AND O.Sort_Code >= IVL.Start_Sort_Code AND O.Sort_Code <= IVL.End_Sort_Code



GO

/****** Object:  View [dbo].[VIRTUAL_ORGANISM]    Script Date: 12/05/2018 21:27:34 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

ALTER VIEW [dbo].[VIRTUAL_ORGANISM]
AS
SELECT   ITN.TAXON_LIST_ITEM_KEY, 'VIRTUAL_ORGANISM' AS TAXON_LIST_VERSION_KEY, ITN.TAXON_VERSION_KEY, 
                      ITN.TAXON_LIST_ITEM_KEY AS PREFERRED_NAME, NULL AS TAXON_LIST_VERSION_TO, O.Sort_Code, ITN2.TAXON_LIST_ITEM_KEY AS PARENT, 
                      O.ORGANISM_RANK_KEY AS TAXON_RANK_KEY, O.ENTERED_BY, O.ENTRY_DATE, O.SYSTEM_SUPPLIED_DATA, O.Has_Children
FROM     dbo.ORGANISM AS O INNER JOIN
                      dbo.INDEX_TAXON_NAME AS ITN ON O.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY AND 
                      ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY AND ITN.CAN_EXPAND = 1 INNER JOIN
                      dbo.ORGANISM AS O2 ON O2.ORGANISM_KEY = O.PARENT_KEY INNER JOIN
                      dbo.INDEX_TAXON_NAME AS ITN2 ON O2.TAXON_VERSION_KEY = ITN2.TAXON_VERSION_KEY AND 
                      ITN2.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN2.TAXON_LIST_ITEM_KEY 
UNION SELECT

ITN.TAXON_LIST_ITEM_KEY, 'VIRTUAL_ORGANISM' AS TAXON_LIST_VERSION_KEY, ITN.TAXON_VERSION_KEY, 
                      ITN.TAXON_LIST_ITEM_KEY AS PREFERRED_NAME, NULL AS TAXON_LIST_VERSION_TO, O.Sort_Code, NULL AS PARENT, 
                      O.ORGANISM_RANK_KEY AS TAXON_RANK_KEY, O.ENTERED_BY, O.ENTRY_DATE, O.SYSTEM_SUPPLIED_DATA, O.Has_Children
FROM     dbo.ORGANISM AS O INNER JOIN
                      dbo.INDEX_TAXON_NAME AS ITN ON O.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY AND 
                      ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY AND ITN.CAN_EXPAND = 1 
                      AND O.PARENT_KEY IS NULL
                      
GO
/****** Object:  UserDefinedFunction [dbo].[LCUserAddedNames]    Script Date: 12/07/2018 17:03:40 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[LCUserAddedNames]
(@TLIKey char(16))
RETURNS varchar(450)
--
--	DESCRIPTION
--	Function to return a semi-colon seperated string of all user 
--	added names for  a TLI key 
--	@TLIKey		Taxon List item key
--
--
--	AUTHOR:	Mike Weideli
--	CREATED: 30/11/2018
--

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(450)
DECLARE @ItemString varchar(70)
DECLARE @UserDefinedName varchar(450)
SET @UserDefinedName =''



SELECT @UserDefinedName= @UserDefinedName  
       + Item_Name + ';' FROM 
       TAXON_USER_NAME 
       WHERE TAXON_LIST_ITEM_KEY = @TLIKey  

if len(@UserDefinedName) > 0 
BEGIN
  set  @ReturnString  = left(@UserDefinedName,len(@UserDefinedName)-1)
END

--****************************************************************************************************
RETURN @ReturnString
END
GO

GRANT EXECUTE ON [dbo].[LCUserAddedNames] TO PUBLIC
GO

/****** Object:  UserDefinedFunction [dbo].[ufn_GetDeprecated]    Script Date: 12/07/2018 16:06:35 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns whether or not the TLI key is Deprecated or Redundant 
  If neither then returns blank. 
  Parameters:	@Redundant  and @AllowDatEntry (which equates to Redundant) 
  Note that taxa can be both redundant and deprecated so priority is given to Redudant   
  Created:	November 2018

  Mike Weideli:
    

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetDeprecated]
	(@Deprecated bit ,
     @Redundant bit)
RETURNS varchar(10)
AS
BEGIN
  DECLARE @Invalid varchar(10) 
  set @Invalid = '' 
  If @Redundant = 0   
    set @Invalid =  'Redundant'
  else if @Deprecated = 1  
    set @Invalid = 'Deprecated'
  
  Return  @Invalid   
      
  
END
GO
GRANT EXECUTE ON [dbo].[ufn_GetDeprecated] TO PUBLIC

GO
/****** Object:  UserDefinedFunction [dbo].[FormatIndividualFull]    Script Date: 12/06/2018 19:19:08 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



ALTER FUNCTION [dbo].[FormatIndividualFull](@Title varchar(10), @Initials varchar(8), @Forename varchar(30), @Surname varchar(30))
RETURNS varchar(100)
--
--	DESCRIPTION
--	Function to return a formatted string of an individuals title, forename, initials and surname.
--	Includes all elements unless null - Use where full information needs to be displayed. 
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@Title			Individual's Title
--	@Initials		Individual's Initials
--	@Forename		Individual's Forename
--	@Surname		Individual's Surname
--
--
--	AUTHOR:	Mike Weideli
--	CREATED: August 2016
--      REVISED Mike Weideli September 2016 - Alter order to facilitate searching

AS
BEGIN

--****************************************************************************************************

RETURN     @Surname +  ISNULL(' ' +@Title ,'') + ISNULL(' ' + @Forename ,'') +
 isnull(' ' +@initials,'')

--****************************************************************************************************

END



GO

/****** Object:  UserDefinedFunction [dbo].[ufn_GetFullSpeciesName]    Script Date: 12/05/2018 17:57:46 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:	Returns the full taxon name and if
  not the same the recomendedname for the given taxon list item.
  Parameters:	@Key

  Created:	

  Last revision information:
    November 2018    
\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFullSpeciesName]
	(@ListItemKey char(16))
RETURNS varchar(400)
AS
BEGIN
	DECLARE	@FullName 			VARCHAR(400),
			@ActualName 			VARCHAR(100),
			@Authority 				VARCHAR(100),
			@Attribute				VARCHAR(100),
            @RecommendedName 	    VARCHAR(100),
	        @RecommendedAuthority 	VARCHAR(100),
			@RecommendedAttribute	VARCHAR(100)
	
	SET 	@FullName	= ''
      
	
	SELECT 	@ActualName 		= ITN.Actual_Name, 
			@Authority 			= ITN.Authority, 
			@Attribute			= ITN.Actual_Name_Attribute
	        FROM	Index_Taxon_Name	ITN
	    	WHERE	ITN.Taxon_List_Item_Key = @ListItemKey
	
    
         
	SELECT 	@RecommendedName		= ITN2.Actual_Name, 
			@RecommendedAuthority	= ITN2.Authority, 
			@RecommendedAttribute	= ITN2.Actual_Name_Attribute
	FROM	Index_Taxon_Name	ITN
    INNER JOIN INDEX_TAXON_NAME ITN2 ON ITN2.TAXON_LIST_ITEM_KEY
    = ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY WHERE
    ITN.TAXON_LIST_ITEM_KEY <> ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY AND 
	ITN.Taxon_List_Item_Key = @ListItemKey

    SET @FullName =  @ActualName + ISNULL(' ' + @Attribute, '') + ISNULL(' ' 
    + @Authority, '') 
    + ISNULL('   ' + @RecommendedName,'')  + ISNULL(' ' + @RecommendedAttribute, '') + ISNULL(' ' 
    + @RecommendedAuthority, '') 

	RETURN @FullName
END

GO

GRANT EXECUTE ON [dbo].[ufn_GetFullSpeciesName] TO PUBLIC

GO
/****** Object:  UserDefinedFunction [dbo].[ufn_GetDatesRecording]    Script Date: 12/05/2018 17:58:25 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:	
		The Date Range the individual was recording/determining in   
  Parameters:	
		@NameKey

  Created:	Novmber 2018

  Last Mike Weideli
\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetDatesRecording]
(@NameKey CHAR(16))
RETURNS varchar(50)
AS
BEGIN
  DECLARE @ReturnString varchar(50)
  
  SELECT @ReturnString  = dbo.LCReturnVagueDateShort (MIN(S.Vague_Date_Start),Min(S.Vague_Date_Start),
  'D')  +  ' - ' +  
  dbo.LCReturnVagueDateShort (MAX(S.Vague_Date_End),MAX(S.Vague_Date_END),
  'D')    
  FROM Sample S
  INNER JOIN Sample_Recorder SR
	ON SR.Sample_Key = S.Sample_Key
	INNER JOIN    
	Survey_Event_Recorder SER
	ON SER.SE_RECORDER_KEY
	= SR.SE_RECORDER_KEY
	WHERE S.VAGUE_DATE_TYPE <> 'P'
	AND S.VAGUE_DATE_TYPE <> '-Y'
	AND S.VAGUE_DATE_TYPE <> 'Y-' 
    AND SER.NAME_KEY = @NameKey
	if ISNULL(@ReturnString,'') = '' 
	   Set @ReturnString = 'No Records' 
	
	RETURN	@ReturnString
END

GO

GRANT EXECUTE ON [dbo].[ufn_GetDatesRecording] TO PUBLIC


GO
/****** Object:  UserDefinedFunction [dbo].[ufn_GetCountofRecords]    Script Date: 12/05/2018 18:00:19 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Returns the number of records for a given individual  
	            or a Determiner 
  Parameters:	@NameKey, @Recorder(0 = Recorder, 1 = Deteminer)

  Created:	November 2018

  Last revision information:
    $Author: Mike Weideli 
    
\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetCountofRecords]
	(@NameKey char(16), @Recorder integer )
RETURNS varchar(10)
AS
BEGIN
	Declare @ReturnString int
	If @Recorder = 0 
	BEGIN
	  Select @ReturnString = COUNT(TOCC.Taxon_Occurrence_Key) FROM Taxon_Occurrence TOCC
	  INNER JOIN SAMPLE_RECORDER SR ON SR.Sample_Key = TOCC.Sample_Key
	  INNER JOIN  Survey_Event_Recorder SER
	  ON SER.SE_RECORDER_KEY
	  = SR.SE_RECORDER_KEY
	  WHERE SER.NAME_KEY = @NameKey
	END ELSE
	BEGIN
	 Select @ReturnString = COUNT(TOCC.Taxon_Occurrence_Key) FROM Taxon_Occurrence TOCC
	 INNER JOIN TAXON_DETERMINATION TDET 
	 ON TDET.TAXON_OCCURRENCE_KEY = TOCC.TAXON_OCCURRENCE_KEY
	 AND TDET.PREFERRED = 1 
	 WHERE TDET.DETERMINER  = @NameKey
	END 
	   
	
	RETURN LTrim(str(@ReturnString))
END

GO

GRANT EXECUTE ON [dbo].[ufn_GetDatesRecording] TO PUBLIC

GO

/****** Object:  UserDefinedFunction [dbo].[LCGroupsRecorded]    Script Date: 12/05/2018 18:01:47 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



CREATE FUNCTION [dbo].[LCGroupsRecorded]
(@NameKey char(16))
RETURNS varchar(6000)
--
--	DESCRIPTION
--	Function to return a semi-colon separated string  
--	taxon groups recorded by an individual 
--	@NameKey Name_Key
--
--
--	AUTHOR:	Mike Weideli
--	CREATED: 30/11/2018
--

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(6000)
DECLARE @ItemName varchar(50)

DECLARE csrGroupsRecorded CURSOR   
     FOR SELECT DISTINCT
     Taxon_Group_Name FROM
       TAXON_DETERMINATION TDET 
       INNER JOIN 
       TAXON_LIST_ITEM TLI
       ON TLI.TAXON_LIST_ITEM_KEY 
       = TDET.TAXON_LIST_ITEM_KEY  
       INNER JOIN TAXON_VERSION TV 
       ON TV.TAXON_VERSION_KEY = 
       TLI.TAXON_VERSION_KEY     
       INNER JOIN
       TAXON_GROUP TG ON
       TG.TAXON_GROUP_KEY =
       TV.OUTPUT_GROUP_KEY
       INNER JOIN TAXON_OCCURRENCE TOCC 
       ON TOCC.TAXON_OCCURRENCE_KEY  = TDET.TAXON_OCCURRENCE_KEY
       INNER JOIN SAMPLE_RECORDER SR
       ON SR.SAMPLE_KEY = TOCC.SAMPLE_KEY
       INNER JOIN SURVEY_EVENT_RECORDER SER
       ON SER.SE_RECORDER_KEY = SR.SE_RECORDER_KEY
       WHERE SER.NAME_KEY = @NAMEKEY   
       ORDER BY Taxon_Group_Name
OPEN csrGroupsRecorded

FETCH NEXT FROM csrGroupsRecorded INTO @ReturnString
WHILE @@FETCH_STATUS = 0
BEGIN
	FETCH NEXT FROM csrGroupsRecorded INTO @ItemName
	IF @@FETCH_STATUS = 0 SELECT @ReturnString = @ReturnString + ';' + @ItemName
END

CLOSE csrGroupsRecorded 
DEALLOCATE csrGroupsRecorded 
     
RETURN @ReturnString
END

GO

GRANT EXECUTE ON [dbo].[LCGroupsRecorded] TO PUBLIC


GO
/****** Object:  UserDefinedFunction [dbo].[ufn_GetFormattedName]    Script Date: 12/05/2018 18:04:38 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Returns the formatted name for the given name key.
		The rule for individual name format is:
			Title + (Forename | Initials) + Surname

		The rule for organisation name format is
			Acronym + ', ' + Full_Name

		Null fields are omitted.

  Parameters:	@NameKey

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 20/05/04 15:01 $
    $Author: Anthonysimpson $
    MIke Weideli December 2018
\*===========================================================================*/
ALTER FUNCTION [dbo].[ufn_GetFormattedName]
	(@NameKey char(16))
RETURNS varchar(100)
AS
BEGIN
	DECLARE	@FormattedName varchar(100)
	SET @FormattedName = ''

	IF EXISTS(SELECT * FROM [Name] WHERE Name_Key = @NameKey AND Organisation = 0)
	BEGIN
		SELECT	@FormattedName = 
    		(ISNULL (Title + ' ', '') + 
    		ISNULL (Forename + ' ','') +
    		ISNULL (Initials + ' ','') +
    		SURNAME)
    	FROM Individual
		WHERE	Name_Key = @NameKey
	END ELSE BEGIN
		SELECT	@FormattedName = 
			CASE WHEN Acronym IS NOT NULL THEN Acronym + ', ' + Full_Name
			ELSE Full_Name END
		FROM Organisation 
		WHERE Name_Key = @NameKey
	END
	RETURN @FormattedName
END


GO

GRANT EXECUTE ON [dbo].[ufn_GetFormattedName] TO PUBLIC
 
GO
/****** Object:  UserDefinedFunction [dbo].[ufn_Location_Expired]    Script Date: 12/16/2018 16:54:20 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns True if the location is not expired
                or False if it has. Expired will have 
                Location Designations where all entries have an expiry date.  
  Parameters:	@LocationKey

  Created:	December 2018

  Last revision information:
    $Author: Mike Weideli 
    
\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_Location_Expired]
	(@LocationKey char(16))
RETURNS bit
AS
BEGIN
  DECLARE @EXPIRED Bit
  SET @EXPIRED = 1
  IF EXISTS(SELECT * FROM LOCATION_DESIGNATION
     WHERE LOCATION_KEY = @LOCATIONKEY AND DATE_TO IS NULL)
     OR NOT EXISTS (SELECT * FROM 
     LOCATION_DESIGNATION WHERE 
     LOCATION_KEY = @LOCATIONKEY)  SET @EXPIRED = 0         
    
  RETURN @Expired    
END

GO

GRANT EXECUTE ON [dbo].[ufn_Location_Expired] TO PUBLIC

GO
/****** Object:  UserDefinedFunction [dbo].[ufn_Location_Date_Active]    Script Date: 12/16/2018 16:57:28 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns the date for a 
                a Location which is expired (ie all 
                the location designations have an end date) 
  Parameters:	@LocationKey

  Created:	December 2018

  Last revision information:
    $Author: Mike Weideli 
    
\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_Location_Date_Active]
	(@LocationKey char(16))
RETURNS varchar(45)
AS
BEGIN
  Declare @StartDate varchar(20)
  Declare @EndDate varchar(20)
  SELECT @StartDate = isnull(CONVERT(VARCHAR(10), 
         MIN(Date_From), 103),'Not Recorded') FROM
         Location_Designation WHERE Location_key = @LocationKey  
         AND Date_From IS NOT NULL
  SELECT @EndDate = isnull(CONVERT(VARCHAR(10), 
         MAX(Date_To), 103),'Not Recorded') FROM
         Location_Designation WHERE Location_key = @LocationKey  
         AND Date_To IS NOT NULL
           
    
  RETURN @StartDate + ' - ' +  @EndDate   
END


GO
GRANT EXECUTE ON [dbo].[ufn_Location_Date_Active] TO PUBLIC
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns a numeric value showing how well
  two names compare is using Title,ForeName,Initials and Surname.
  Takes into account that either of the two names sets may have null values,
  however, surnames must be the same to match - assumes that at least one
  of the surnames is not null. 
  Returns 0 if not a possible match 
		  8 surname only matching
		  9 surname + title
		  10 surname + initials
		  11 surname + title + initials
		  12 surname + forename
		  13 surname + title +  forename
		  14 surname + surname + Forename + initials
		  15 All match  	  
  Parameters Title,Forename,Initials,Surname
  Title2,Forename2,Initials2,Surname2
       
  Created:	December 2018

  Mike Weideli:
    

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_CompareNames]
 (@Title varchar(4),
 @Forename varchar(20),
 @Initials varchar(8),
 @Surname varchar(30),
 @Title2 varchar(4),	
 @ForeName2 varchar(20),
 @Initials2 varchar(8),
 @Surname2 varchar(30))	

RETURNS integer
AS
BEGIN
  DECLARE @Compare integer 
  Set @Compare = 0
 -- try to deal with situations around Forenames and Initials

 if @Forename is not null and  @Forename2 is null and @Initials2 is not null 
  Begin
    set @ForeName2 = left(@Initials2,1)
    set @ForeName = left(@ForeName,1)
    set @Initials2 = null
    Set @Initials = null
  End   
  if @Forename2 is not null and  @Forename is null and @Initials is not null 
  Begin
    set @ForeName = left(@Initials,1)
    set @ForeName2 = left(@ForeName2,1)
    set @Initials2 = null
    Set @Initials = null 
  End   
  
  -- If there is a Forename then Initial is often the middle initial

  
  if @Forename is not null and Left(@forename,1) <> Left(@Initials,1) 
    set @Initials = left(@Forename,1) + '.' + isnull(@Initials,'') 
    
  if @Forename2 is not null and Left(@forename2,1) <> Left(@Initials2,1) 
    set @Initials2 = left(@Forename2,1) + '.' + isnull(@Initials2,'') 
     
  -- Deal with full stop at end of initials   
  If right(isnull(@Title,''),1) = '.' set @Title = left(@Title,len(@Title) -1)
  If right(isnull(@Title2,''),1) = '.' set @Title2 = left(@Title2,len(@Title2)-1)
  If right(isnull(@Initials,''),1) = '.' set @Initials = left(@Initials,len(@Initials)-1)
  If right(isnull(@Initials2,''),1) = '.' set @Initials2 = left(@Initials2,len(@Initials2)-1)
   
  -- Deal with differnt lengths of initials 
  
  If len(@Initials) > len(@Initials2)  set @Initials = left(@initials,len(@Initials2))
  If len(@Initials2) > len(@Initials)  set @Initials2 = left(@initials2,len(@Initials))
  
  -- The main process 
  if (isnull(@Title,'') = isnull(@Title2,'') 
    or @Title + @Title2 IS NULL)
  AND (isnull(@Initials,'') = isnull(@Initials2,'') 
    or @Initials + @Initials2 IS NULL)
  AND (isnull(@Forename,'') = isnull(@ForeName2,'')  
    or @Forename+@Forename2 IS NULL) 
  AND (ISNULL(@Surname,'') = ISNULL(@Surname2,'')) 
  BEGIN
    Set @Compare = 8 
    IF @Title= @Title2  set @Compare = @Compare + 1
    IF @Initials = @Initials2 set  @Compare = @Compare + 2
    IF @Forename = @Forename2 set @Compare = @Compare + 4
  END
  Return  @Compare   
      
  
END

GO

GRANT EXECUTE ON [dbo].[ufn_CompareNames] TO PUBLIC

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Names_Notes]    Script Date: 12/05/2018 17:03:32 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the notes for IW imported data  

  Created:	Nov 2018 

    
  Populates the notes column 
\*===========================================================================*/


CREATE PROCEDURE [dbo].[usp_IWMatch_Names_Notes]

AS
    
 Update #Names set Notes = 'Matched'
 WHERE match_key is not null and remembered = 0
  
	
 Update #Names set Notes = 'Remembered match' 
 WHERE  match_key is not null and remembered = 1
	
 Update #Species set Notes =  '' 
 WHERE  Match_Key is null  
 
 Update #Names set Notes = ltrim(str(Match_Count)) + ' possible matches ' 
 WHERE  Match_Key is null and Match_Count > 0

GO

GRANT EXECUTE ON [dbo].[usp_IWMatch_Names_Notes] TO PUBLIC

 
GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Names_Notes_Single]    Script Date: 12/05/2018 17:05:05 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the notes for IW imported data for a specified
  Import_Value 

  Created:	Nov 2018 

      
  Populates the notes column 
\*===========================================================================*/


CREATE PROCEDURE [dbo].[usp_IWMatch_Names_Notes_Single]
@ImportValue varchar(100)
AS
    
 Update #Names set Notes = 'Matched'
 WHERE match_key is not null and remembered = 0
 AND IMPORT_VALUE = @ImportValue 
	
 Update #Names set Notes = 'Remembered match' 
 WHERE  match_key is not null and remembered = 1
 AND IMPORT_VALUE = @ImportValue 	

 Update #Names set Notes =  '' 
 WHERE  Match_Key is null  
 AND IMPORT_VALUE = @ImportValue 
 
 Update #Names set Notes = ltrim(str(Match_Count)) + ' possible matches ' 
 WHERE  Match_Key is null and Match_Count > 0
 AND IMPORT_VALUE = @ImportValue 

GO

GRANT EXECUTE ON [dbo].[usp_IWMatch_Names_Notes_Single] TO PUBLIC


GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Species_Notes]    Script Date: 12/05/2018 17:06:56 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Adds and updates notes for Species matches 

  Created:	Nov 2018 

    
  Populates the notes column 
\*===========================================================================*/


CREATE PROCEDURE [dbo].[usp_IWMatch_Species_Notes]

AS
    
 Update #Species set Notes = 'Matches only to redundant taxa' 
 WHERE Status = 3 
	
 Update #Species set Notes = 'Matches only to deprecated taxa' 
 WHERE Status = 2 
	
 Update #Species set Notes = ltrim(str(Match_Count)) + ' Possible matches ' 
 WHERE Status = 1  AND Match_Key is null
	
 Update #Species set Notes =  'Remembered Match' 
 WHERE Match_Key is not null and Remembered = 1
 
 Update #Species set Notes =  'Matched' 
 WHERE Match_Key is not null and Remembered = 0
	
 Update #Species set Notes =  '' 
 WHERE Status = 0  AND Match_Key is null	

GO

GRANT EXECUTE ON [dbo].[usp_IWMatch_Species_Notes] TO PUBLIC

GO


/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Species_Notes_Single]    Script Date: 12/05/2018 17:08:52 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Adds and updates notes for Species matches 

  Created:	Nov 2018 
  Parameter @ImportValue = ImportValue  
  
    
  Updates the notes column for a single entry 
\*===========================================================================*/


CREATE PROCEDURE [dbo].[usp_IWMatch_Species_Notes_Single]
(@ImportValue varchar(75))
AS
    
 Update #Species set Notes = 'Matches only to redundant taxa' 
 WHERE Status = 3 AND Import_Value = @ImportValue 
	
 Update #Species set Notes = 'Matches only to deprecated taxa' 
 WHERE Status = 2 
 AND Import_Value = @ImportValue 
	
 Update #Species set Notes = ltrim(str(Match_Count)) + ' Possible matches ' 
 WHERE Status = 1  AND Match_Key is null 
 AND Import_Value = @ImportValue 
	
 Update #Species set Notes =  'Remembered Match' 
 WHERE Match_Key is not null and Remembered = 1
  AND Import_Value = @ImportValue 
  
 Update #Species set Notes =  'Matched' 
 WHERE Match_Key is not null and Remembered = 0
 AND Import_Value = @ImportValue 
 	
 Update #Species set Notes =  '' 
 WHERE Status = 0  AND Match_Key is null
  AND Import_Value = @ImportValue 

GO

GRANT EXECUTE ON [dbo].[usp_IWMatch_Species_Notes_Single] TO PUBLIC

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Species_Order]    Script Date: 12/05/2018 17:30:47 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the taxonomic order for all #Species 

  Created:	Nov 2018 
  Populates the ORDER column 
\*===========================================================================*/


CREATE PROCEDURE [dbo].[usp_IWMatch_Species_Order]

AS
   If EXISTS (SELECT *  FROM Index_Taxon_Hierarchy) 
   BEGIN   
     --   Use Index_Taxon_Hierarchy to get the the Order if available
     UPDATE	S
	 SET	[Order] = T.ITEM_NAME
	 FROM	#Species	S
	   INNER JOIN INDEX_TAXON_NAME ITN ON S.Match_Key  = ITN.Taxon_List_Item_Key
	   INNER JOIN INDEX_TAXON_HIERARCHY ITH ON ITH.Recommended_Taxon_Version_Key
	   = ITN.RECOMMENDED_TAXON_VERSION_KEY AND ITH.Hierarchy_Type ='O'
	   INNER JOIN TAXON_VERSION TV 
	   ON TV.TAXON_VERSION_KEY = ITH.Hierarchy_Taxon_Version_Key
	   INNER JOIN TAXON T ON T.TAXON_KEY = TV.TAXON_KEY
	   WHERE s.Match_Key IS NOT NULL 
	  
    END ELSE
    BEGIN
      -- Update the Order field. Key value for rank 'Order' is 'NBNSYS..12'. 
      UPDATE S
      SET    [Order] = ITN.Actual_Name
      FROM   #Species      S
        JOIN Index_Taxon_Synonym ITS ON ITS.Synonym_List_Item_Key = S.Match_Key
        JOIN Index_Taxon_Group ITG ON ITS.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
        JOIN Taxon_List_Item TLI 
              ON     ITG.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
              AND TLI.Taxon_Rank_Key = 'NBNSYS0000000012'
        JOIN   Index_Taxon_Name ITN
              ON     ITN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
       WHERE s.Match_Key IS NOT NULL
         
     END 
-- Order field can still be null, so deal with that. 'Not available' value handled and regionalised in app.
UPDATE #Species 
SET    [Order] = 'Not available'
WHERE  Match_Key IS NOT NULL
AND    [Order] IS NULL

GO

GRANT EXECUTE ON [dbo].[usp_IWMatch_Species_Order] TO PUBLIC


GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Species_Order_Single]    Script Date: 12/05/2018 17:35:11 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the taxonomic order for all a specified Species 
  Parameter = TLI_Key

  Created:	Nov 2018 
   
\*===========================================================================*/


CREATE PROCEDURE [dbo].[usp_IWMatch_Species_Order_Single]
(@TLIKey char(16))
AS
    
-- Use Index_Taxon_Hierarchy to get the the Order 
	UPDATE	#Species
	SET	[Order] = T.ITEM_NAME
	FROM	
	  #Species INNER JOIN INDEX_TAXON_NAME ITN
	  ON ITN.TAXON_LIST_ITEM_KEY = #Species.Match_Key  
	  INNER JOIN INDEX_TAXON_HIERARCHY ITH ON ITH.Recommended_Taxon_Version_Key
	  = ITN.RECOMMENDED_TAXON_VERSION_KEY AND ITH.Hierarchy_Type ='O'
	  INNER JOIN TAXON_VERSION TV 
	  ON TV.TAXON_VERSION_KEY = ITH.Hierarchy_Taxon_Version_Key
	  INNER JOIN TAXON T ON T.TAXON_KEY = TV.TAXON_KEY
	  WHERE ITN.Taxon_List_Item_Key = @TLIKey 
	
	-- Order field can still be null, so deal with that. 'Not available' value handled and regionalised in app.
	
	UPDATE	#Species
	SET	[Order] = 'Not available'
	WHERE	Match_Count = 1 AND Match_Key is not null
	AND	[Order] IS NULL


GO

GRANT EXECUTE ON [dbo].[usp_IWMatch_Species_Order_Single] TO PUBLIC

GO
/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Species_Select]    Script Date: 12/21/2018 20:16:18 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Returns details matches where  one or more 
      possible match is acceptable 

  Parameters: @Key TaxonName 

  Created:	November 2018 
   
    
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWNotes_Species_Select]
@Key varCHAR(75)
AS
  Declare @MatchedCount integer,@Remembered bit, @ManualMatch bit
  Select @MatchedCount = Match_Count from #Species where Import_Value = @Key   
  Select @Remembered = Remembered from #Species where Import_Value = @Key   
  Select @ManualMatch =  Manual_Match from #Species where Import_Value = @Key   


  IF @MatchedCount = 0 Or  @Remembered = 1 Or @ManualMatch = 1 
  BEGIN
    SELECT	
    ITN.TAXON_LIST_ITEM_KEY AS AKey,	
    LTRIM(dbo.ufn_GetDeprecated(ITN.DEPRECATED,ITN.Allow_Data_Entry)+ ' ') +
    [dbo].[ufn_GetFullSpeciesName] (ITN.TAXON_LIST_ITEM_KEY) +
    ' (' + TG.TAXON_GROUP_NAME + ')'  
    AS FullDetails,ISNULL(#Species.Match_Key,'') As MatchKey, OUTPUT_TAXON_NAME, 1 AS POSSIBLE, 
    TG.Taxon_Group_Name   
    FROM INDEX_TAXON_NAME ITN INNER JOIN
    TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
    INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY   
    LEFT JOIN #Species ON #Species.Import_Value = @Key
    WHERE OUTPUT_TAXON_NAME = @Key AND PREFERRED_TAXA > 0
   UNION SELECT
    ITN.TAXON_LIST_ITEM_KEY AS AKey,	
    '---- ' + LTRIM(dbo.ufn_GetDeprecated(ITN.DEPRECATED,ITN.Allow_Data_Entry)+ ' ') +
    [dbo].[ufn_GetFullSpeciesName] (ITN.TAXON_LIST_ITEM_KEY) +
    ' (' + TG.TAXON_GROUP_NAME + ')'  
    AS FullDetails,ISNULL(#Species.Match_Key,'') As MatchKey, OUTPUT_TAXON_NAME, 0 AS POSSIBLE, 
    TG.Taxon_Group_Name   
    FROM INDEX_TAXON_NAME ITN INNER JOIN
    TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
    INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY   
    LEFT JOIN #Species ON #Species.Import_Value = @Key
    WHERE (OUTPUT_TAXON_NAME  LIKE(LEFT(@Key ,CHARINDEX(' ',@Key + ' ')-1)+'%') OR
    OUTPUT_TAXON_NAME  LIKE('%' + RIGHT(@Key,LEN(@KEY) - CHARINDEX(' ',@Key))))
    AND PREFERRED_TAXA > 0
    ORDER BY POSSIBLE DESC, TAXON_GROUP_NAME, OUTPUT_TAXON_NAME
  END
  ELSE
  BEGIN
    SELECT	
    ITN.TAXON_LIST_ITEM_KEY AS AKey,	
    LTRIM(dbo.ufn_GetDeprecated(ITN.DEPRECATED,ITN.Allow_Data_Entry)+ ' ') +
    [dbo].[ufn_GetFullSpeciesName] (ITN.TAXON_LIST_ITEM_KEY) +
    ' (' + TG.TAXON_GROUP_NAME + ')'  
    AS FullDetails,ISNULL(#Species.Match_Key,'') As MatchKey, OUTPUT_TAXON_NAME, 1 AS POSSIBLE, 
    TG.Taxon_Group_Name   
    FROM INDEX_TAXON_NAME ITN INNER JOIN
    TAXON_VERSION TV ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
    INNER JOIN TAXON_GROUP TG ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY   
    LEFT JOIN #Species ON #Species.Import_Value = @Key 
    WHERE OUTPUT_TAXON_NAME = @Key AND PREFERRED_TAXA > 0
    ORDER BY TAXON_GROUP_NAME, OUTPUT_TAXON_NAME
   END

GO

GRANT EXECUTE ON [dbo].[usp_IWNotes_Species_Select] TO PUBLIC

GO

/****** Object:  StoredProcedure [dbo].[usp_IW_Names_Multi_Update]    Script Date: 12/05/2018 17:19:51 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Updates the #Species table following 
  selction from the notes column 
  Parameters @ImportValue 
			 @MatchCount
			 @MatchKey
             @ManualMatch
             @Remembered
            

  Created:	November 2018 
 

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IW_Names_Multi_Update] 
@ImportValue VARCHAR(100),
@MatchCount INT,
@MatchKey CHAR(16),
@ManualMatch BIT,
@Remembered BIT

AS
  UPDATE #NAMES 
  SET Match_Count = @MatchCount,
  Match_Key = @MatchKey,
  Manual_Match = @ManualMatch,
  Remembered = @Remembered,
  Match_Value =  dbo.ufn_GetFormattedName(@MatchKey)
  FROM INDIVIDUAL  I 
  WHERE
  #NAMES.Import_Value = @ImportValue
  AND @MatchKey <>''
  
  DELETE FROM IW_Matched_Names WHERE Matched_Value =
  @ImportValue and Matched_Key <> @MatchKey  
  
  EXEC dbo.usp_IWMatch_Names_Notes_single @ImportValue
  
GO

GRANT EXECUTE ON [dbo].[usp_IW_Names_Multi_Update] TO PUBLIC
 
GO
/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Name_Detail]    Script Date: 12/21/2018 20:03:10 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Returns additional information on a name
                    selected for possible matching 
  Parameters: @Key Name_Key @Level The amount of detail required 
  Created:	November 2018 
 
  Note - Done with repeated code, because implementations using IF seem to 
  be much slower in both instances. 
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWNotes_Name_Detail]
(@Key varchar(16),@Detail int, @ImportValue varchar(100) )


AS
  DECLARE @EOL CHAR(2)
  SET @EOL = CHAR(13) + CHAR(10)  
  
  IF @Detail = 0 
  BEGIN 
    SELECT
    I.NAME_KEY AS AKey,
    'Is current match : ' + 
    + Case when #NAMES.MATCH_KEY = @Key then 'Yes'
          else 'No'
      End 
    + @EOL  
    +'Full Name : ' + [dbo].[FormatIndividualFull](I.Title,I.Initials,I.ForeName,I.Surname)	
    + @EOL  
    + 'Address : ' + ISNULL (dbo.ufn_GetAddress(I.Name_Key),'')
    + @EOL 
    + 'DOB/DOD : ' +  ISNULL (dbo.ufn_GetDOBandDOD(I.Name_Key),'')
    + @EOL  
    + 'Active dates : ' + ISNULL(dbo.LCReturnVagueDateShort(I.ACTIVE_VAGUE_DATE_START,I.ACTIVE_VAGUE_DATE_END,
    ACTIVE_VAGUE_DATE_TYPE),'')
    + @EOL
    + 'Custodian : ' + NAME.CUSTODIAN  
    + @EOL
    + 'Name Key : ' + NAME.NAME_KEY 
    + @EOL
    + 'Comment : ' + ISNULL(dbo.ufn_RtfToPlaintext(Name.Comment),'') 
    AS Details 
    FROM INDIVIDUAL I INNER JOIN NAME ON NAME.NAME_KEY = I.NAME_KEY
    INNER JOIN #NAMES ON #NAMES.IMPORT_VALUE  = @IMPORTVALUE 
    WHERE I.NAME_KEY = @key
    ORDER BY [dbo].[FormatIndividualFull](I.Title,I.Initials,I.ForeName,I.Surname)
  END ELSE 
  BEGIN
   SELECT	
     I.NAME_KEY AS AKey,
    'Is current match : ' + 
    + Case when #NAMES.MATCH_KEY = @Key then 'Yes'
          else 'No'
      End 
    +  @EOL
    + 'Full Name : ' + [dbo].[FormatIndividualFull](I.Title,I.Initials,I.ForeName,I.Surname)	
    + @EOL  
    + 'Address : ' + ISNULL (dbo.ufn_GetAddress(I.Name_Key),'')
    + @EOL 
    + 'DOB/DOD : ' +  ISNULL (dbo.ufn_GetDOBandDOD(I.Name_Key),'')
    + @EOL  
    + 'Active Dates : ' + ISNULL(dbo.LCReturnVagueDateShort(I.ACTIVE_VAGUE_DATE_START,I.ACTIVE_VAGUE_DATE_END,
    I.ACTIVE_VAGUE_DATE_TYPE),'')
    + @EOL
    + 'Custodian : ' + NAME.CUSTODIAN   
    + @EOL 
    + 'Name Key : ' + NAME.NAME_KEY
    + @EOL
    + 'Comment : ' + ISNULL(dbo.ufn_RtfToPlaintext(Name.Comment),'') 
    + @EOL 
    + 'Recording Dates : ' +  ISNULL(dbo.ufn_GetDatesRecording(I.Name_Key),'')  
    + @EOL 
    + 'No of Records : ' + dbo.ufn_GetCountofRecords (I.Name_Key,0)  
    + @EOL 
    + 'No of Determinations : ' + dbo.ufn_GetCountofRecords (I.Name_Key,1) 
    + @EOL 
    + 'Groups Recorded : ' + ISNULL(dbo.LCGroupsRecorded(I.Name_Key),'')
     AS Details 
    FROM INDIVIDUAL I INNER JOIN NAME ON NAME.NAME_KEY = I.NAME_KEY
    INNER JOIN #NAMES ON #NAMES.IMPORT_VALUE  = @IMPORTVALUE 
    WHERE I.NAME_KEY = @key
    ORDER BY [dbo].[FormatIndividualFull](I.Title,I.Initials,I.ForeName,I.Surname)
  END 

GO
  
  GRANT EXECUTE ON [dbo].[usp_IWNotes_Name_Detail] TO PUBLIC


GO
/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Names_Select]    Script Date: 12/21/2018 20:17:36 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Returns details matches where  one or more 
      possible matches is acceptable 

  Parameters: @Key = Import_Value

  Created:	November 2018 
 

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWNotes_Names_Select]
@Key varchar(75)
AS
  
  SELECT I.NAME_KEY AS AKey,[dbo].[FormatIndividualFull](I.Title,I.Initials,I.ForeName,I.Surname)
  AS FullDetails, ISNULL(#NAMES.Match_Key,'') As MatchKey, 1 AS POSSIBLE, I.Surname,I.Forename,I.Initials,I.Title
  FROM INDIVIDUAL I 
  LEFT JOIN  #NAMES ON #NAMES.Import_Value = @Key 
  WHERE  dbo.ufn_CompareNames(#Names.Title,#Names.Forename,#Names.Initials,
  #Names.Surname,I.TITLE,I.ForeName,I.INITIALS,I.Surname) > 7
  UNION SELECT
  I.NAME_KEY AS AKey, '---- ' + [dbo].[FormatIndividualFull](I.Title,I.Initials,I.ForeName,I.Surname)
  AS FullDetails, ISNULL(#NAMES.Match_Key,'') As MatchKey, 0 AS POSSIBLE,    I.Surname AS ASurname,I.Forename,I.Initials,I.Title 
  FROM INDIVIDUAL I 
  LEFT JOIN  #NAMES ON #NAMES.Import_Value = @Key 
  WHERE #NAMES.SURNAME = I.SURNAME  
  ORDER BY POSSIBLE DESC,I.Forename,I.Initials,I.Title
  

GO

GRANT EXECUTE ON [dbo].[usp_IWNotes_Names_Select] TO PUBLIC


GO
/****** Object:  StoredProcedure [dbo].[usp_IWCheckRecoverableMatches_Species]    Script Date: 12/05/2018 18:08:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:
	Checks whether there are any recoverable matches for this checklist.

  Parameters:
	@ChecklistKey	The checklist which the species are being recovered from.
	@UserID			The Name_Key of the current user.
	@FoundMatches	An output boolean which is set to true if matches are found,
					and false otherwise.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $
    Mike Weideli December 2018
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWCheckRecoverableMatches_Species]
	@ChecklistKey VARCHAR(16) = NULL,
	@UserID CHAR(16),
	@FoundMatches BIT OUT
AS
	If EXISTS (
		SELECT	Temp_User_ID
		FROM	IW_Matched_Species
		LEFT JOIN #Species
			ON	(Checklist_Key	=	Match_Checklist_Key
				OR (Checklist_Key IS NULL AND Match_Checklist_Key IS NULL))
			AND	Match_Key		=	Matched_Key
			AND Import_Value	=	Matched_Value
		WHERE	Temp_User_ID	=	@UserID
			AND	(Match_Checklist_Key = @ChecklistKey
					OR	ISNULL(Match_Checklist_Key,'') = ISNULL(@ChecklistKey,''))
			AND	Match_Key IS NULL -- Don't want matches which are already displayed.
	)  
		SET @FoundMatches = 1
	ELSE
		SET @FoundMatches = 0

GO

/****** Object:  StoredProcedure [dbo].[usp_IWMatchRecovered_Species]    Script Date: 12/05/2018 18:11:06 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:
	Restores any temporary species matches left over from a previous session.

  Parameters:
	@ChecklistKey	The checklist which the species are being recovered from.
	@UserID			The Name_Key of the current user.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRecovered_Species]
	@ChecklistKey CHAR(16),
	@UserID CHAR(16)
AS
	UPDATE 	#Species
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedSpeciesName(Matched_Key),
		Remembered = 1,
		Checklist = TL.Item_Name,
		Checklist_Key = @ChecklistKey
	FROM 	IW_Matched_Species
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = Matched_Key
	JOIN	Taxon_List_Version TLV ON TLI.Taxon_List_Version_Key = TLV.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TLV.Taxon_List_Key = TL.Taxon_List_Key
	WHERE 	Import_Value = Matched_Value
	AND		Match_Key IS NULL 
	AND		(Match_Checklist_Key = @ChecklistKey 
			OR (Match_Checklist_Key IS NULL AND @ChecklistKey IS NULL))
	AND		Temp_User_ID = @UserID


GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchClear_Species]    Script Date: 12/05/2018 18:12:01 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:
	Deletes all temporary matches for the current user.

  Parameters:
	@UserID			The ID of the current user.
	@ChecklistKey	The key of the current checklist (null for all checklists).

  Created:	January	2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/

ALTER PROCEDURE [dbo].[usp_IWMatchClear_Species]
	@UserID CHAR(16),
	@ChecklistKey CHAR(16) = NULL
AS
	DELETE FROM	IW_Matched_Species
	WHERE		Temp_User_ID	=	@UserID
			AND	(Match_Checklist_Key = @ChecklistKey
			OR	@ChecklistKey IS NULL)

/****** Object:  StoredProcedure [dbo].[usp_IWCheckRecoverableMatches_Species]    Script Date: 12/05/2018 18:13:02 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:
	Checks whether there are any recoverable matches for this checklist.

  Parameters:
	@ChecklistKey	The checklist which the species are being recovered from.
	@UserID			The Name_Key of the current user.
	@FoundMatches	An output boolean which is set to true if matches are found,
					and false otherwise.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $
    Mike Weideli December 2018
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWCheckRecoverableMatches_Species]
	@ChecklistKey VARCHAR(16) = NULL,
	@UserID CHAR(16),
	@FoundMatches BIT OUT
AS
	If EXISTS (
		SELECT	Temp_User_ID
		FROM	IW_Matched_Species
		LEFT JOIN #Species
			ON	(Checklist_Key	=	Match_Checklist_Key
				OR (Checklist_Key IS NULL AND Match_Checklist_Key IS NULL))
			AND	Match_Key		=	Matched_Key
			AND Import_Value	=	Matched_Value
		WHERE	Temp_User_ID	=	@UserID
			AND	(Match_Checklist_Key = @ChecklistKey
					OR	ISNULL(Match_Checklist_Key,'') = ISNULL(@ChecklistKey,''))
			AND	Match_Key IS NULL -- Don't want matches which are already displayed.
	)  
		SET @FoundMatches = 1
	ELSE
		SET @FoundMatches = 0

GO


/*===========================================================================*\
  Description:
	Deletes all temporary matches for the current user.

  Parameters:
	@UserID			The ID of the current user.
	@ChecklistKey	The key of the current checklist (null for all checklists).

  Created:	January	2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/

ALTER PROCEDURE [dbo].[usp_IWMatchClear_Species]
	@UserID CHAR(16),
	@ChecklistKey CHAR(16) = NULL
AS
	DELETE FROM	IW_Matched_Species
	WHERE		Temp_User_ID	=	@UserID
			AND	(Match_Checklist_Key = @ChecklistKey
			OR	@ChecklistKey IS NULL)
GO


GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchNewEntry_Name]    Script Date: 12/20/2018 17:40:42 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Create a new individual from an import value.

  Parameters:	
	@ImportValue	The raw name to parse and insert in database.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 22/02/06 10:58 $
    $Author: Johnvanbreda $
    Changed by M Weideli to bring in line with the
    parsing of names used in IW processing. Also not to save the records if they
    are not sufficiently complete. 
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchNewEntry_Name]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
		
   UPDATE #Names set Title  = dbo.ufn_IWParseImportValue(Import_Value,1)
   UPDATE #Names set Forename  = dbo.ufn_IWParseImportValue(Import_Value,2)
   UPDATE #Names set Initials  = dbo.ufn_IWParseImportValue(Import_Value,3)
   UPDATE #Names set Surname  = dbo.ufn_IWParseImportValue(Import_Value,4)

   EXECUTE spNextKey 'Name', @Key OUTPUT

   
   INSERT INTO [Name] (	
		Name_Key, Organisation, Entered_By,Entry_Date
				) 
		SELECT @Key,0,@EnteredBy,GETDATE()
		FROM #Names WHERE 
		LEN(Surname) > 2 AND (Initials IS NOT NULL 
	    OR ForeName is not null) AND IMPORT_VALUE = @ImportValue
	    AND NOT EXISTS (SELECT * FROM INDIVIDUAL I WHERE
	    dbo.ufn_CompareNames(#Names.Title,#Names.Forename,#Names.Initials,
	    #Names.Surname,I.Title,I.FORENAME,I.INITIALS,I.SURNAME) > 13) 
	    
	    INSERT INTO Individual (
			Name_Key, Title, Forename, Initials, Surname, Entered_By,Entry_Date
		) 
		SELECT @Key,Title,Forename,Initials,Surname,@enteredBy,GETDATE()
		FROM #Names WHERE IMPORT_VALUE = @ImportValue AND 
		EXISTS (SELECT * FROM NAME WHERE NAME.NAME_KEY = @Key)       	    	
   
		UPDATE	#Names
		SET	Match_Value = dbo.ufn_GetFormattedName(@Key),
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			Notes = 'Matched'
	    WHERE Import_Value = @ImportValue
        AND EXISTS(SELECT * FROM INDIVIDUAL WHERE 
        NAME_KEY = @Key)  	  

        UPDATE	#Names
		SET	Notes = 'Add failed. Exists or insufficient info.'
	    WHERE Import_Value = @ImportValue
        AND NOT EXISTS(SELECT * FROM INDIVIDUAL WHERE 
        NAME_KEY = @Key)  

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchRecord_Names]    Script Date: 12/05/2018 18:31:53 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:	<none>

  Created:	Julye 2004

  Last revision information:
    $Revision: 1 $
    $Date: 30/07/04 15:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRecord_Names]
AS
	-- Update existing items, if they were "rematched"
	DELETE FROM IW_Matched_Names
	FROM  #Names
	INNER JOIN IW_Matched_Names 
	ON IW_Matched_Names.Matched_Value
	=  #Names.Import_Value
	WHERE IW_Matched_Names.Matched_Key <> #NAMES.Match_Key
	
	-- Add the new ones now.
	-- But only if they are reasonable
	INSERT INTO 	IW_Matched_Names
	SELECT		DISTINCT Import_Value, Match_Key
	FROM		#Names
	INNER JOIN  INDIVIDUAL I ON I.NAME_KEY = #NAMES.Match_Key
	WHERE	Match_Key IS NOT NULL
	AND		Manual_Match = 1
	AND CHARINDEX(I.Surname,Import_Value) > 0
	AND (I.FORENAME  IS NOT NULL OR I.INITIALS IS NOT NULL) 
    AND NOT EXISTS (SELECT * FROM IW_Matched_Names 
    WHERE Matched_Value = #NAMES.Import_Value)  


GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchRecord_Species]    Script Date: 12/05/2018 18:32:48 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:
	@UserID			The ID of the current User.

  Created:	July 2004

  Last revision information:
    $Revision: 6 $
    $Date: 05/03/13 15:50 $
    $Author: Michaelcaptain $
    Micahel Weideli December 2018
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRecord_Species]
	@UserID			char(16)
AS
	-- Removes existing matches which have a replacement.
	DELETE		MS1
	FROM		IW_Matched_Species		MS1
	INNER JOIN	IW_Matched_Species		MS2
			ON	MS2.Temp_User_ID		=	@UserID
			AND	(MS1.Match_Checklist_Key	=	MS2.Match_Checklist_Key
                        OR (MS1.Match_Checklist_Key IS NULL AND MS2.Match_Checklist_Key IS NULL))
			AND	MS1.Matched_Value		=	MS2.Matched_Value
	WHERE		MS1.Temp_User_ID		IS	NULL
	
	-- Makes the temporary changes this user made permenant.
	UPDATE	IW_Matched_Species
	SET		Temp_User_ID	=	NULL
	WHERE	Temp_User_ID	=	@UserID


GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchRemembered_Names]    Script Date: 12/05/2018 18:33:53 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 8 $
    $Date: 28/09/13 15:52 $
    $Author: mikeweideli $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRemembered_Names]
AS
	-- Update temp table with relevant data.
	UPDATE 	#Names
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedName(Matched_Key), 
		Remembered = 1
	FROM 	IW_Matched_Names 
	JOIN	[Name] ON Name_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL AND NAME_KEY <> Import_Value     
	
	
	UPDATE 	#Names
	SET 	Match_Count = 1, 
		Match_Key = I.Name_key,
		Match_Value =  I.NAME_KEY,
		Remembered = 1
	FROM 	Individual I 
	JOIN  #Names On I.Name_Key = #names.import_value 
  	WHERE 	Match_Key IS NULL      

    EXEC [dbo].[usp_IWMatch_Names_Notes]

GO	

/****** Object:  StoredProcedure [dbo].[usp_IWMatchRemembered_Species]    Script Date: 12/05/2018 18:34:56 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:	
	Populate import table with matched values from previous imports.

  Parameters:	
	@ChecklistKey	The primary key of the current checklist.

  Created:	July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $
    Mike Weideli December 2018
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRemembered_Species]
	@ChecklistKey CHAR(16)
AS
	UPDATE 	#Species
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = dbo.ufn_GetFormattedSpeciesName(Matched_Key),
		Remembered = 1,
		Checklist = TL.Item_Name,
		Checklist_Key = @ChecklistKey
	FROM 	IW_Matched_Species
	JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = Matched_Key
	JOIN	Taxon_List_Version TLV ON TLI.Taxon_List_Version_Key = TLV.Taxon_List_Version_Key
	JOIN	Taxon_List TL ON TLV.Taxon_List_Key = TL.Taxon_List_Key
	WHERE 	Import_Value = Matched_Value 
	AND		Match_Key IS NULL
	AND		(Match_Checklist_Key = @ChecklistKey 
			OR (Match_Checklist_Key IS NULL AND @ChecklistKey IS NULL))
	AND		Temp_User_ID IS NULL
	
	
EXEC [dbo].[usp_IWMatch_Species_Notes]



GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchRule_Select]    Script Date: 12/05/2018 18:36:43 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	returns details of a match rule

  Parameters:	@Key - IW_Match_Rule_Key

  Created:	June 2004

  Last November 2018

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRule_Select]
	@Key CHAR(16)
AS
	SELECT 	[Sequence], 
		Item_Name, 
		Control_Type,
		Imported_Data_Insert_Sql,
		Remembered_Matches_Procedure,
		Match_Procedure,
		Record_Matches_Procedure,
		New_Entry_Procedure,
		Requires_Checklist,
		Set_Match_Procedure,
		Table_Create_Sql,
		Key_To_Caption_Procedure,
		Search_Type,
		Checklists_Select_Procedure,
		Termlist_Select_Procedure,
        Exclude_Unmatched_Procedure,
        Update_Notes_Procedure,
	    Display_Notes_Procedure,
	    Detailed_Notes_Procedure
	FROM 	IW_Match_Rule
	WHERE 	IW_Match_Rule_Key = @Key
	ORDER BY [Sequence]
GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchSet_Name]    Script Date: 12/21/2018 19:52:00 ******/
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
    December 2018 - Mike Weideli Update notes 
      
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchSet_Name]
	@ImportValue varchar(100),
	@MatchKey char(16)
AS
	IF @MatchKey IS NOT NULL
		UPDATE	#Names
		SET	Match_Value = dbo.ufn_GetFormattedName(@MatchKey),
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	ELSE
		UPDATE	#Names
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = 0,
			Manual_Match = 0,
			Remembered = 0
		WHERE	Import_Value = @ImportValue

EXEC dbo.usp_IWMatch_Names_Notes_single @ImportValue


GO

/****** Object:  StoredProcedure [dbo].[usp_IWMatchSet_Species]    Script Date: 12/05/2018 18:38:08 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	
		@ImportValue	The name of the species.
		@MatchKey		The key of the Taxon_List_Item that the species is
						being matched to.
		@UserID			The ID of the current user.

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $
    Mike Weideli December 2018
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchSet_Species]
	@ImportValue varchar(100),
	@MatchKey CHAR(16),
	@UserID CHAR(16),
	@ChecklistKey CHAR(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Order varchar(100), 
			@Checklist varchar(100),
			@MatchValue varchar(100)

		-- 'Not available' value handled and regionalised in app.
	    SET @Order = 'Not available'
	
		-- Get the associated checklist.
		SELECT	@Checklist = TL.Item_Name
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	ITN.Taxon_List_Item_Key = @MatchKey

		SET	@MatchValue =	dbo.ufn_GetFormattedSpeciesName(@MatchKey)

		-- And update match table.
		UPDATE	#Species
		SET	Match_Value = @MatchValue,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			[Order] = @Order,
			Checklist = @Checklist,
			Checklist_Key = @ChecklistKey
		WHERE	Import_Value = @ImportValue

		-- Remove any previous temporary matches this user made of this value.
		DELETE FROM IW_Matched_Species
		WHERE	Matched_Value		=	@ImportValue
			AND	Temp_User_ID		=	@UserID
		
		-- Inserts the temporary match.
		INSERT INTO IW_Matched_Species (
			Matched_Value,
			Matched_Key,
			Match_Checklist_Key,
			Temp_User_ID
		) VALUES (
			@ImportValue,
			@MatchKey,
			@ChecklistKey,
			@UserID
		)
	END ELSE BEGIN
		SET @MatchKey = null 
		UPDATE	#Species
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			[Order] = NULL,
			Checklist = NULL,
			Checklist_Key = NULL
		WHERE	Import_Value = @ImportValue
		
		-- Remove any previous temporary matches this user made of this value.
		DELETE FROM IW_Matched_Species
		WHERE	Matched_Value		=	@ImportValue
			AND	Temp_User_ID		=	@UserID
	END
EXEC dbo.usp_IWMatch_Species_Notes_Single @ImportValue
EXEC  dbo.usp_IWMatch_Species_Order_Single @MatchKey


GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Species]    Script Date: 12/05/2018 18:39:11 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	@ChecklistKey

  Created:	June 2004

  Last revision information:
    Nov 2018 
    MikeWeideli
    Preferred List changed to work on Preferred taxa. The count is based on all the preferred taxa
    (ie all possibilities), because we need to make sure that we do not match 
    erroneously, however we only actual do a match if the TLI is not deprecated 
    and allows data entry - preferred taxa = 1)  
    
    This UDF now uses ITN Output_Taxon_Name instead of the udf
    Also uses ITN.Deprecated instead of working this out each time based on the 
    version in TLV. There is no reason why Deprecated taxa can not be used for input/matching as they may have 
    been recorded using an earlier version of the list. However, if the TVK  has been 
    marked redundant in the Organism table this will cause Allow_Data_Entry is set to false.
    In this case the user needs to investigate as the Taxon should not be used. An example
    of a situation where this can occur is if a species thought to be in the UK is found not to be. 
    This means that the records need to be looked at to determine the 
    correct species, or the record could be ignored.  
      
   A new notes field is now being used to reflect the reason for rejection  

\*===========================================================================*/


ALTER PROCEDURE [dbo].[usp_IWMatch_Species]
	@ChecklistKey char(16)
AS
    
UPDATE	#Species
SET	Species_Name = 
    CASE	WHEN Right(Import_Value, 2) = 'sp' THEN Left(Import_Value, Len(Import_Value) - 2)
    WHEN Right(Import_Value, 3) = 'sp.' THEN Left(Import_Value, Len(Import_Value) - 3)
	WHEN Right(Import_Value, 3) = 'spp' THEN Left(Import_Value, Len(Import_Value) - 3)
	WHEN Right(Import_Value, 4) = 'spp.' THEN Left(Import_Value, Len(Import_Value) - 4)
	  ELSE Import_Value
	END
    --  remove the sub genus from import_value 
    UPDATE #Species Set Species_name = dbo.LCRemoveSubGenusText(Species_name),
    Match_Count = 0, Status = 0 
    
   
    -- Match TV Key
       UPDATE	#Species
		SET	Match_Key = ITN2.Taxon_List_Item_Key,
			Match_Value =  TLI.Taxon_Version_Key,
			Checklist = TL.Item_Name,
			Checklist_Key = @ChecklistKey,
			Match_Count = 1,
			Status = 1
		FROM	
		Taxon_List_Item TLI INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TLi.Taxon_List_Item_Key
		INNER JOIN Index_Taxon_Name ITN2 ON ITN.Recommended_Taxon_List_Item_Key = ITN2.Taxon_List_Item_Key
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN2.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	Species_Name = TLI.Taxon_Version_Key
  
 IF @ChecklistKey IS NULL
   BEGIN
   -- Handle searches against the preferrred taxa (Types above 0)     
   UPDATE	UpdatedSpecies
      SET	Match_Count =  (SELECT	Count(DISTINCT ITN.Taxon_List_Item_Key)
	  FROM	#Species S  
	  INNER JOIN Index_Taxon_Name ITN ON ITN.Output_Taxon_Name
	   = S.Species_Name
	  WHERE	ITN.Preferred_Taxa > 0 
	  AND	S.Import_Value = UpdatedSpecies.Import_Value)
	  FROM	#Species UpdatedSpecies
	  WHERE	Match_Key IS NULL

   -- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
   UPDATE	#Species
      SET	Match_Key = ITN.Taxon_List_Item_Key,
	  Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
	  Checklist = TL.Item_Name,
	  Checklist_Key = NULL, Status = 1
	  FROM	Index_Taxon_Name ITN
	  JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	  JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	  WHERE	Match_Count = 1
	  AND	Match_Key IS NULL
	   AND  ITN.Preferred_Taxa = 1 -- This is correct will auto match only 1
	  AND	Species_Name = ITN.Output_Taxon_Name
	 
     -- Now set the value for the preferred taxa where not set
      UPDATE #Species
      SET	Status = ITN.PREFERRED_TAXA
	  FROM	Index_Taxon_Name ITN
	  WHERE Species_Name = ITN.Output_Taxon_Name   
      AND Status = 0 AND PREFERRED_TAXA > 0
    END ELSE
    BEGIN
	  If LEFT(@checkListkey,7) = 'VIRTUAL'
	  -- Deal with Virtual Organism table 
	  BEGIN 
	    UPDATE	UpdatedSpecies
        SET	Match_Count =  (SELECT	Count(DISTINCT ITN.Taxon_List_Item_Key)
	    FROM	#Species S  
	    INNER JOIN Index_Taxon_Name ITN ON ITN.Output_Taxon_Name
	    = S.Species_Name
	   WHERE	ITN.Preferred_Taxa > 0 
	   AND	S.Import_Value = UpdatedSpecies.Import_Value)
	   FROM	#Species UpdatedSpecies
	   WHERE	Match_Key IS NULL
	   
	    -- Virtal Organism get values and keys for unique matches only. Broken down in two separate updates for speed.
		-- where the taxa is a non deprecated and allow data entry taxa.
		-- the only difference with this that this will always return the
		-- recommended name.       
	   UPDATE	#Species
       SET	Match_Key = ITN.Recommended_Taxon_List_Item_Key,
	   Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
	   Checklist = 'VIRTUAL_ORGANISM',
	   Checklist_Key = 'VIRTUAL_ORGANISM', Status = 1
	   FROM	Index_Taxon_Name ITN
	   JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	   JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
	   WHERE	Match_Count = 1
	   AND	Match_Key IS NULL
	   AND  ITN.Preferred_Taxa = 1 -- This is correct will auto match only 1
	   AND	Species_Name = ITN.Output_Taxon_Name
		
	   UPDATE #Species
       SET	Status = ITN.PREFERRED_TAXA
	   FROM	Index_Taxon_Name ITN
	   WHERE Species_Name = ITN.Output_Taxon_Name   
       AND Status = 0 AND PREFERRED_TAXA > 0		
		
	  END ELSE
	  BEGIN
	  -- Handle searches against a normal specified list No change here.
      -- Set Match_Count first. Broken down in two separate updates for speed.
		UPDATE	UpdatedSpecies
		SET	Match_Count =  (SELECT	Count(*)
					FROM	#Species S 
					INNER JOIN Index_Taxon_Name ITN ON ITN.OUTPUT_TAXON_NAME = S.Species_Name
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key					
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
					WHERE	TLV.Taxon_List_Key = @ChecklistKey
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND ITN.DEPRECATED = 0) 
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL

		UPDATE	UpdatedSpecies
		SET	Match_Count = Match_Count + (SELECT	Count(*)
					FROM	#Species S 
					INNER JOIN Index_Taxon_Name ITN ON ITN.Output_Taxon_Name  + ' ' + ITN.Authority = S.Species_Name 
					INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key					
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
					WHERE	TLV.Taxon_List_Key = @ChecklistKey
					AND	S.Import_Value = UpdatedSpecies.Import_Value
					AND ITN.DEPRECATED = 0)
		FROM	#Species UpdatedSpecies
		WHERE	Match_Key IS NULL
		
		 
		-- Now get values and keys for unique matches only. Broken down in two separate updates for speed.
		UPDATE	#Species
		SET	Match_Key = ITN.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key,
			Status = 1
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TL.Taxon_List_Key = @ChecklistKey
		AND	Species_Name = ITN.OUTPUT_TAXON_NAME
		AND ITN.DEPRECATED = 0

		UPDATE	#Species
		SET	Match_Key = ITN.Taxon_List_Item_Key,
			Match_Value = dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key),
			Checklist = TL.Item_Name,
			Checklist_Key = TL.Taxon_List_Key,
			Status = 1
		    
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	Match_Count = 1
		AND	Match_Key IS NULL
		AND	TL.Taxon_List_Key = @ChecklistKey
		AND	Species_Name = ITN.OUTPUT_TAXON_NAME + ' ' + ITN.Authority
		AND ITN.DEPRECATED IS NULL
	  END
    END
    -- Use Index_Taxon_Hierarchy to get the the Order 
    EXEC  [dbo].[usp_IWMatch_Species_Order]
	
	
    -- Update the notes
    EXEC [dbo].[usp_IWMatch_Species_Notes]

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Names]    Script Date: 12/19/2018 19:52:13 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
  Mike Weideli
  Only matches where there is a good match    
  The previous method could result in spurious matches   
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatch_Names]
AS
  UPDATE #Names set Title  = dbo.ufn_IWParseImportValue(Import_Value,1)
  UPDATE #Names set Forename  = dbo.ufn_IWParseImportValue(Import_Value,2)
  UPDATE #Names set Initials  = dbo.ufn_IWParseImportValue(Import_Value,3)
  UPDATE #Names set Surname  = dbo.ufn_IWParseImportValue(Import_Value,4)
   -- set the match count where enough similarities 
   UPDATE	#Names
   SET	Match_Count = 
		  (SELECT	 Count(*)
		  FROM INDIVIDUAL I 
		  WHERE  
		  dbo.ufn_CompareNames(#Names.Title,#Names.Forename,#Names.Initials,
		  #Names.Surname,I.TITLE,I.ForeName,I.INITIALS,I.Surname) > 7)
		  WHERE Match_Key IS NULL
	
    
    UPDATE #Names
    SET Match_Value = [dbo].[ufn_GetFormattedName](Name_Key),
    Match_Key = Name_Key
    FROM INDIVIDUAL I WHERE
    dbo.ufn_CompareNames(#Names.Title,#Names.Forename,#Names.Initials,
	#Names.Surname,I.TITLE,I.ForeName,I.INITIALS,I.Surname) > 10
	AND Match_Key IS NULL AND Match_Count = 1

 
   
    Exec [dbo].[usp_IWMatch_Names_Notes]

GO
/****** Object:  StoredProcedure [dbo].[usp_IW_Species_Multi_Update]    Script Date: 12/05/2018 20:33:50 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Updates the #Species table following 
  selction from the notes column 
  Parameters @ImportValue 
			 @MatchCount
			 @MatchKey
             @ManualMatch
             @Remembered
          

  Created:	November 2018 
 

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IW_Species_Multi_Update] 
@ImportValue VARCHAR(100),
@MatchCount INT,
@MatchKey CHAR(16),
@ManualMatch BIT,
@Remembered BIT

AS
  UPDATE #SPECIES 
  SET Match_Count = @MatchCount,
  Match_Key = @MatchKey,
  Manual_Match = @ManualMatch,
  Remembered = @Remembered,
  Match_Value = dbo.ufn_GetFormattedSpeciesName(@MatchKey),
  CheckList = TL.ITEM_NAME,
  CheckList_Key = '' 
  FROM #SPECIES S 
  INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY = @MatchKey 
  INNER JOIN TAXON_LIST_VERSION TLV 
  ON TLV.TAXON_LIST_VERSION_KEY = ITN.TAXON_LIST_VERSION_KEY
  INNER JOIN TAXON_LIST TL
  ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
  WHERE
  S.Import_Value = @ImportValue
  AND ITN.Allow_Data_Entry = 1
  AND @MatchKey <> ''
  
  DELETE FROM IW_Matched_Species
  WHERE Matched_Value = @ImportValue AND
  Matched_Key <> @MatchKey
    
  EXEC [dbo].[usp_IWMatch_Species_Notes]
  
  EXEC [dbo].[usp_IWMatch_Species_Order_Single] @MatchKey
GO

GRANT EXECUTE ON [dbo].[usp_IW_Species_Multi_Update] TO PUBLIC


GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchSet_Species]    Script Date: 12/05/2018 20:48:01 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	
		@ImportValue	The name of the species.
		@MatchKey		The key of the Taxon_List_Item that the species is
						being matched to.
		@UserID			The ID of the current user.

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $
    Mike Weideli December 2018
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchSet_Species]
	@ImportValue varchar(100),
	@MatchKey CHAR(16),
	@UserID CHAR(16),
	@ChecklistKey CHAR(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Order varchar(100), 
			@Checklist varchar(100),
			@MatchValue varchar(100)

		-- 'Not available' value handled and regionalised in app.
	    SET @Order = 'Not available'
	
		-- Get the associated checklist.
		SELECT	@Checklist = TL.Item_Name
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	ITN.Taxon_List_Item_Key = @MatchKey

		SET	@MatchValue =	dbo.ufn_GetFormattedSpeciesName(@MatchKey)

		-- And update match table.
		UPDATE	#Species
		SET	Match_Value = @MatchValue,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			[Order] = @Order,
			Checklist = @Checklist,
			Checklist_Key = @ChecklistKey
		WHERE	Import_Value = @ImportValue

		-- Remove any previous temporary matches this user made of this value.
		DELETE FROM IW_Matched_Species
		WHERE	Matched_Value		=	@ImportValue
			AND	Temp_User_ID		=	@UserID
		
		-- Inserts the temporary match.
		INSERT INTO IW_Matched_Species (
			Matched_Value,
			Matched_Key,
			Match_Checklist_Key,
			Temp_User_ID
		) VALUES (
			@ImportValue,
			@MatchKey,
			@ChecklistKey,
			@UserID
		)
	END ELSE BEGIN
		SET @MatchKey = null 
		UPDATE	#Species
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = NULL,
			Manual_Match = 0,
			Remembered = 0,
			[Order] = NULL,
			Checklist = NULL,
			Checklist_Key = NULL,
			Status = 0
		WHERE	Import_Value = @ImportValue
		
		-- Remove any previous temporary matches this user made of this value.
		DELETE FROM IW_Matched_Species
		WHERE	Matched_Value		=	@ImportValue
			AND	Temp_User_ID		=	@UserID
	END
EXEC dbo.usp_IWMatch_Species_Notes_Single @ImportValue
EXEC  dbo.usp_IWMatch_Species_Order_Single @MatchKey

GO
/****** Object:  StoredProcedure [dbo].[usp_IWNotes_Species_Detail]    Script Date: 12/21/2018 20:00:20 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:		Returns additional information on a species
                    selected for possible matching 
  Parameters: @Key TLI Key 
  Created:	November 2018 
 

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWNotes_Species_Detail]
(@Key varchar(16), @Detail integer, @ImportValue varchar(100) = Null)
AS
  DECLARE @EOL CHAR(2)
  SET @EOL = CHAR(13) + CHAR(10)  
  IF @Detail = 0 
  BEGIN 
     SELECT
     'Is current match : '  
     + Case when #SPECIES.MATCH_KEY = @Key  then 'Yes'
          else 'No'
      End 
     + @EOL	
     + 'Taxon Name: '     
     + dbo.ufn_GetFullSpeciesName (ITN.TAXON_LIST_ITEM_KEY)
     + @EOL	
	 + 'Taxon List : ' + TL.Item_Name 
	 + @EOL	
	 + 'Common Name : ' + ITN.COMMON_NAME
	 + @EOL	
	 + 'Preferred Name : ' + ITN.PREFERRED_NAME
	 + @EOL	
	 + 'Recommended Name : ' + dbo.ufn_GetFullSpeciesName(ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY)
	 + @EOL	
	 + 'Taxon Group : ' + TG.Taxon_Group_Name
	  + @EOL	
	 + 'Is User Added : ' 
	 + Case when TV.SYSTEM_SUPPLIED_DATA = 0 then 'Yes'
          else 'No'
	   End 
	 + @EOL	
	 + 'Deprecated : '   
	 + Case when Deprecated = 1 then 'Yes'
          else 'No'
	   End 
	  + @EOL	
	  + 'Allow Data Entry : '   
	  + Case when Allow_Data_Entry = 1 then 'Yes'
          else 'No'
	  +  @EOL
	  + 'Taxon User Names : '
	  + ISNULL(dbo.LCUserAddedNames (ITN.TAXON_LIST_ITEM_KEY),'') 
	  + @EOL
	  + 'TLI Key : '
	  +  ITN.TAXON_LIST_ITEM_KEY 
	  End 
	  AS Details
	  FROM INDEX_TAXON_NAME ITN
	  INNER JOIN TAXON_LIST_VERSION TLV
	  ON TLV.TAXON_LIST_VERSION_KEY = ITN.TAXON_LIST_VERSION_KEY
	  INNER JOIN TAXON_LIST TL
	  ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
	  INNER JOIN TAXON_VERSION TV
	  ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
	  INNER JOIN TAXON_GROUP TG 
	  ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY
	  INNER JOIN #SPECIES ON #SPECIES.IMPORT_VALUE = @IMPORTVALUE 
	  WHERE ITN.Taxon_List_Item_Key = @Key
	  AND ITN.SYSTEM_SUPPLIED_DATA = 1
  END ELSE
  BEGIN
     SELECT 
     'Is current match : ' 
     + Case when #SPECIES.MATCH_KEY = @Key  then 'Yes'
          else 'No'
       End 
     + @EOL	
     + 'Taxon Name: ' + dbo.ufn_GetFullSpeciesName (ITN.TAXON_LIST_ITEM_KEY)
     + @EOL	
	 + 'Taxon List : ' + TL.Item_Name 
	 + @EOL	
	 + 'Common Name : ' + ITN.COMMON_NAME
	 + @EOL	
	 + 'Preferred Name : ' + ITN.PREFERRED_NAME
	 + @EOL	
	 + 'Recommended Name : ' + dbo.ufn_GetFullSpeciesName(ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY)
	 + @EOL	
	 + 'Taxon Group : ' + TG.Taxon_Group_Name
	  + @EOL	
	 + 'Is User Added : ' 
	 + Case when TV.SYSTEM_SUPPLIED_DATA = 0 then 'Yes'
          else 'No'
	   End 
	 + @EOL	
	 + 'Deprecated : '   
	 + Case when Deprecated = 1 then 'Yes'
          else 'No'
	   End 
	  + @EOL	
	  + 'Allow Data Entry : '   
	  + Case when Allow_Data_Entry = 1 then 'Yes'
          else 'No'
	  End 
	  + @EOL 
	  + 'Taxon User Names : '
	  + ISNULL(dbo.LCUserAddedNames (ITN.TAXON_LIST_ITEM_KEY),'') 
	  + @EOL
	  + 'TLI Key : '
	  +  ITN.TAXON_LIST_ITEM_KEY
	  + @EOL
	  + 'Number of Records : ' 
	  + (SELECT STR(COUNT(Taxon_Determination_Key))
	     FROM TAXON_DETERMINATION TDET
	     WHERE TDET.TAXON_LIST_ITEM_KEY = @Key)
	  AS Details 
	  FROM INDEX_TAXON_NAME ITN
	  INNER JOIN TAXON_LIST_VERSION TLV
	  ON TLV.TAXON_LIST_VERSION_KEY = ITN.TAXON_LIST_VERSION_KEY
	  INNER JOIN TAXON_LIST TL
	  ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
	  INNER JOIN TAXON_VERSION TV
	  ON TV.TAXON_VERSION_KEY = ITN.TAXON_VERSION_KEY
	  INNER JOIN TAXON_GROUP TG 
	  ON TG.TAXON_GROUP_KEY = TV.OUTPUT_GROUP_KEY
	  INNER JOIN #SPECIES ON #SPECIES.IMPORT_VALUE = @IMPORTVALUE 
	  WHERE ITN.Taxon_List_Item_Key = @Key
      AND ITN.SYSTEM_SUPPLIED_DATA = 1
  END
  
GO

 GRANT EXECUTE ON [dbo].[usp_IWNotes_Species_Detail] TO PUBLIC


GO
/****** Object:  StoredProcedure [dbo].[usp_IWCheckUnconfirmedMatches_Species]    Script Date: 12/05/2018 21:33:25 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:
	Checks whether there are any unconfirmed matches for this checklist.

  Parameters:
	@UserID			The Name_Key of the current user.
	@FoundMatches	An output boolean which is set to true if matches are found,
					and false otherwise.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWCheckUnconfirmedMatches_Species]
	@UserID CHAR(16),
	@FoundMatches BIT OUT
AS
	If EXISTS (
		SELECT	Temp_User_ID
		FROM	IW_Matched_Species
		LEFT JOIN #Species
			ON	(Checklist_Key	=	Match_Checklist_Key
				OR (Checklist_Key IS NULL AND Match_Checklist_Key IS NULL))
			AND	Match_Key		=	Matched_Key
			AND Import_Value	=	Matched_Value
		WHERE	Temp_User_ID	=	@UserID
			AND	Match_Key IS NOT NULL -- Only want temporary matches that are currently displayed
	)  
		SET @FoundMatches = 1
	ELSE
		SET @FoundMatches = 0



GO
/****** Object:  UserDefinedFunction [dbo].[ufn_IWParseImportValue]    Script Date: 12/19/2018 20:00:24 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[ufn_IWParseImportValue](@ImportValue varchar(100),@Namepart integer)

RETURNS varchar(75)

--	DESCRIPTION
--	 Parses the name as held in the Import_Value (which is after parsing by R6 so shoudl be
--  Title/Initials/ForeName and Surname are output as a ; separated string  
--	@ImportValue @Namepart 1 = Title,2 =Forename,3 =Initials,4 = Surname  	
    
--	AUTHOR:	Mike Weideli December 2018 

AS
BEGIN

DECLARE @Key char(16),
		@Surname varchar(30),
		@Forename varchar(20),
		@Initials varchar(8),
		@Title varchar(4),
		@Working varchar(100),
		@Part varchar(50),
		@CurrentPos int,
		@NextPos int,
		@Result varchar(75)
-- Remove any ; in name just in case 
SET @IMPORTVALUE = REPLACE(@IMPORTVALUE,';',':')

	/*===================================================*\
	  Break down the ImportValue into name constituents.
	\*===================================================*/
	SET	@Surname = ''
	SET	@Working = @ImportValue
	SET 	@CurrentPos = 1
	SET	@NextPos = 1

	-- Rule 1. Check for 'xxx' or 'van xxx'
	IF CharIndex('.', @Working) = 0 BEGIN
		SET @NextPos = CharIndex(' ', @Working)
		IF @NextPos = 0
			SET @Surname = @Working
		ELSE BEGIN
			SET @Surname = SubString(@Working, @CurrentPos, @NextPos - 1)
			IF @Surname IN ('van', 'von') BEGIN
				SET @NextPos = CharIndex(' ', @Working, @NextPos + 1)
				IF @NextPos = 0
					SET @Surname = @Working
				ELSE
					SET @Surname = ''
			END ELSE
				SET @Surname = ''
		END
		IF @Surname <> ''
			SET @Working = ''
	END

	-- Rule 2. Check for 'xxx, ###' or 'van xxx, ###'
	IF (@Surname = '') BEGIN
		SET @NextPos = CharIndex(',', @Working)
		IF @NextPos <> 0 BEGIN
			SET @Surname = SubString(@Working, 1, @NextPos - 1)
			SET @Working = LTrim(SubString(@Working, @NextPos + 1, Len(@Working)))
		END
	END

	-- Rule 3. Check for '### xxx'/'###.xxx' or '### van xxx'/'###.van xxx'
	IF (@Surname = '') AND (Right(@Working, 1) <> '.') BEGIN
		SET @Working = Reverse(@Working)
		SET @NextPos = CharIndex(' ', @Working)
		IF (@NextPos = 0) OR (CharIndex('.', SubString(@Working, 1, @NextPos - 1)) <> 0)
			SET @NextPos = CharIndex('.', @Working)
		-- Surname is last word.
		SET @Surname = Reverse(SubString(@Working, 1, @NextPos - 1))
		SET @CurrentPos = @NextPos + 1
		-- Check for 'van/von' too
		SET @NextPos = CharIndex(' ', @Working, @CurrentPos)
		IF (@NextPos = 0) OR (CharIndex('.', SubString(@Working, @CurrentPos, @NextPos - 1)) <> 0)
			SET @NextPos = CharIndex('.', @Working, @CurrentPos)
		SET @Part = RTrim(SubString(@Working, @CurrentPos, Abs(@CurrentPos - @NextPos)))
		IF Reverse(@Part) IN ('van', 'von') BEGIN
			SET @Surname = LTrim(Reverse(SubString(@Working, 1, @NextPos - 1)))
			SET @CurrentPos = @NextPos + 1
		END

		SET @Working = Reverse(LTrim(SubString(@Working, @CurrentPos, Len(@Working))))
	END

	-- Rule 4. Check for 'xxx #.'/'xxx ###.' or 'van xxx #.'/'van xxx ###.'
	IF @Surname = '' BEGIN
		SET @NextPos = CharIndex(' ', @Working)
		If @NextPos > 0 
		BEGIN  
		  SET @Surname = SubString(@Working, 1, @NextPos - 1)
		  IF @Surname IN ('van', 'von') BEGIN
		 	SET @NextPos = CharIndex(' ', @Working, @NextPos + 1)
			SET @Surname = SubString(@Working, 1, @NextPos - 1)
		  END
        END 
		SET @Working = SubString(@Working, @NextPos + 1, Len(@Working))
	END

	-- Parse remainder into Forename, Initials and Title
	SET @Part     = '';
	SET @Forename = '';
	SET @Initials = '';
	SET @Title    = '';
	SET @CurrentPos = 1

	WHILE @CurrentPos <= Len(@Working) BEGIN
		IF SubString(@Working, @CurrentPos, 1) IN (' ', '.') BEGIN
			IF Len(@Part) = 1
				SET @Initials = @Initials + @Part + '.'
			ELSE
			IF @Part IN ('mr', 'mrs' ,'ms' ,'miss' ,'dr' ,'sir' ,'lord' ,'rev' ,'col')
				SET @Title = @Part
			ELSE
				SET @Forename = RTrim(LTrim(@Forename + ' ' + @Part))
			SET @Part = ''
		END ELSE
			SET @Part = @Part + SubString(@Working, @CurrentPos, 1)
	
		SET @CurrentPos = @CurrentPos + 1
	END

	IF @Part <> ''  
		IF Len(@Part) = 1
			SET @Initials = @Initials + @Part + '.'
		ELSE
		IF @Part IN ('mr', 'mrs' ,'ms' ,'miss' ,'dr' ,'sir' ,'lord' ,'rev' ,'col')
			SET @Title = @Part
		ELSE
			SET @Forename = RTrim(LTrim(@Forename + ' ' + @Part))
  
    If @Title ='' set @Title = Null
    If @ForeName = '' set @ForeName = Null
    If @Initials = '' set  @Initials = Null         
    If @Surname = '' set @Surname = Null            
             
    if @Namepart = 1 SET  @Result = @Title
    if @Namepart = 2 SET  @Result = @ForeName
    if @Namepart = 3 SET  @Result = @Initials
    if @Namepart = 4 SET  @Result = @surname 
  
    RETURN @Result 


END

GO
GRANT EXECUTE ON [dbo].[ufn_IWParseImportValue] TO PUBLIC

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchSet_Location]    Script Date: 12/21/2018 20:13:19 ******/
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
			Match_Count = 0,
			Manual_Match = 0,
			Remembered = 0,
			Spatial_Ref = NULL,
			Spatial_Ref_System = NULL,
			Lat = NULL,
			Long = NULL,
			Notes = ''
			
		WHERE	Import_Value = @ImportValue

GO
/****** Object:  StoredProcedure [dbo].[usp_IWMatchSet_Species]    Script Date: 12/21/2018 19:57:57 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:	Set the match for the specified import record in the match table.

  Parameters:	
		@ImportValue	The name of the species.
		@MatchKey		The key of the Taxon_List_Item that the species is
						being matched to.
		@UserID			The ID of the current user.

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/09 12:07 $
    $Author: Simonwood $
    Mike Weideli December 2018
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchSet_Species]
	@ImportValue varchar(100),
	@MatchKey CHAR(16),
	@UserID CHAR(16),
	@ChecklistKey CHAR(16)
AS
	IF @MatchKey IS NOT NULL BEGIN
		DECLARE	@Order varchar(100), 
			@Checklist varchar(100),
			@MatchValue varchar(100)

		-- 'Not available' value handled and regionalised in app.
	    SET @Order = 'Not available'
	
		-- Get the associated checklist.
		SELECT	@Checklist = TL.Item_Name
		FROM	Index_Taxon_Name ITN
		JOIN	Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
		JOIN	Taxon_List TL ON TL.Taxon_List_Key = TLV.Taxon_List_Key
		WHERE	ITN.Taxon_List_Item_Key = @MatchKey

		SET	@MatchValue =	dbo.ufn_GetFormattedSpeciesName(@MatchKey)

		-- And update match table.
		UPDATE	#Species
		SET	Match_Value = @MatchValue,
			Match_Key = @MatchKey,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0,
			[Order] = @Order,
			Checklist = @Checklist,
			Checklist_Key = @ChecklistKey
		WHERE	Import_Value = @ImportValue

		-- Remove any previous temporary matches this user made of this value.
		DELETE FROM IW_Matched_Species
		WHERE	Matched_Value		=	@ImportValue
			AND	Temp_User_ID		=	@UserID
		
		-- Inserts the temporary match.
		INSERT INTO IW_Matched_Species (
			Matched_Value,
			Matched_Key,
			Match_Checklist_Key,
			Temp_User_ID
		) VALUES (
			@ImportValue,
			@MatchKey,
			@ChecklistKey,
			@UserID
		)
	END ELSE BEGIN
		SET @MatchKey = null 
		UPDATE	#Species
		SET	Match_Value = NULL,
			Match_Key = NULL,
			Match_Count = 0,
			Manual_Match = 0,
			Remembered = 0,
			[Order] = NULL,
			Checklist = NULL,
			Checklist_Key = NULL
			
		WHERE	Import_Value = @ImportValue
		
		-- Remove any previous temporary matches this user made of this value.
		DELETE FROM IW_Matched_Species
		WHERE	Matched_Value		=	@ImportValue
			AND	Temp_User_ID		=	@UserID
	END
EXEC dbo.usp_IWMatch_Species_Notes_Single @ImportValue
EXEC  dbo.usp_IWMatch_Species_Order_Single @MatchKey

