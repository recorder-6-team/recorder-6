/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_add_comment]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_add_comment]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    22 May 2008

  Description	
	Add TAXON_OCCURRENCE.COMMENT to the Comment field in nbn_exchange_obs

  Parameters
	
  Revision information
  SGB  16 Jan 2009 - convert RTF to plain text

\*===========================================================================*/
create procedure [dbo].[nbn_exchange_add_comment]
as
-- First do the ones where we have RTF and/or line feeds
UPDATE ##nbn_exchange_obs 
SET Comment = dbo.ufn_RtfToPlaintext(TAXON_OCCURRENCE.[COMMENT])
FROM ##nbn_exchange_obs INNER JOIN
TAXON_OCCURRENCE ON ##nbn_exchange_obs.RecordKey = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY 
WHERE (TAXON_OCCURRENCE.[COMMENT] LIKE '{\rtf%')

-- Next check for any with LF/CR
UPDATE ##nbn_exchange_obs 
SET Comment = dbo.nbn_exchange_strip_LFCR(TAXON_OCCURRENCE.[COMMENT])
FROM ##nbn_exchange_obs INNER JOIN
TAXON_OCCURRENCE ON ##nbn_exchange_obs.RecordKey = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY 
WHERE ((TAXON_OCCURRENCE.[COMMENT] LIKE '%' + CHAR(13) + '%') OR (TAXON_OCCURRENCE.[COMMENT] LIKE '%' + CHAR(10) + '%')) 
AND (##nbn_exchange_obs.[Comment] is null)

-- then simply transfer the rest
UPDATE ##nbn_exchange_obs 
SET Comment = TAXON_OCCURRENCE.[COMMENT]
FROM ##nbn_exchange_obs INNER JOIN
TAXON_OCCURRENCE ON ##nbn_exchange_obs.RecordKey = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY 
WHERE (TAXON_OCCURRENCE.[COMMENT] Is Not Null) AND (##nbn_exchange_obs.[Comment] is null)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_add_comment') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_add_comment TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_add_comment TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_add_comment TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_comment TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_comment TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_add_determiner]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_add_determiner]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    22 May 2008

  Description	
	Add Determiner's name to the Determiner field in nbn_exchange_obs

  Parameters
	
  Revision information

\*===========================================================================*/
create procedure [dbo].[nbn_exchange_add_determiner]
as
UPDATE ##nbn_exchange_obs 
SET Determiner = dbo.ufn_GetFormattedName(TAXON_DETERMINATION.DETERMINER)
FROM ##nbn_exchange_obs INNER JOIN
TAXON_DETERMINATION ON ##nbn_exchange_obs.RecordKey = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY AND TAXON_DETERMINATION.PREFERRED = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_add_determiner') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_add_determiner TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_add_determiner TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_add_determiner TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_determiner TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_determiner TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_add_recorders]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_add_recorders]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    22 May 2008

  Description	
	Add (list of) recorder's name(s) to the Recorder field in nbn_exchange_obs

  Parameters
	
  Revision information

\*===========================================================================*/
create procedure [dbo].[nbn_exchange_add_recorders]
as
UPDATE ##nbn_exchange_obs 
SET Recorder = dbo.FormatEventRecorders(SampleKey)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_add_recorders') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_add_recorders TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_add_recorders TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_add_recorders TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_recorders TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_recorders TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_add_sample_type]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_add_sample_type]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    22 May 2008

  Description	
	Add Sample type to the SampleMethod field in nbn_exchange_obs

  Parameters
	
  Revision information

\*===========================================================================*/
create procedure [dbo].[nbn_exchange_add_sample_type]
as
UPDATE ##nbn_exchange_obs 
SET SampleMethod = SAMPLE_TYPE.SHORT_NAME
FROM ##nbn_exchange_obs INNER JOIN 
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY INNER JOIN
SAMPLE_TYPE ON SAMPLE.SAMPLE_TYPE_KEY = SAMPLE_TYPE.SAMPLE_TYPE_KEY
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_add_sample_type') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_add_sample_type TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_add_sample_type TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_add_sample_type TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_sample_type TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_sample_type TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_add_substrate]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_add_substrate]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    22 May 2008

  Description	
	Add substrate to the Substrate field in nbn_exchange_obs

  Parameters
	
  Revision information

\*===========================================================================*/
create procedure [dbo].[nbn_exchange_add_substrate]
as
UPDATE ##nbn_exchange_obs 
SET Substrate = SUBSTRATE.SHORT_NAME
FROM ##nbn_exchange_obs INNER JOIN 
TAXON_OCCURRENCE ON ##nbn_exchange_obs.RecordKey = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
INNER JOIN
SUBSTRATE ON TAXON_OCCURRENCE.SUBSTRATE_KEY = SUBSTRATE.SUBSTRATE_KEY
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_add_substrate') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_add_substrate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_add_substrate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_add_substrate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_substrate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_add_substrate TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_basic_update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_basic_update]
GO
                      
/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    20 May 2008

  Description	
     Update nbn_exchange_obs with basic information about the occurrences
     sample and survey. All of this information should always be present in
     some form, so we don't need to worry about joins.

     Add a location name to SiteName based on SAMPLE.LOCATION_NAME or LOCATION
     linked to SAMPLE.LOCATION_KEY or SURVEY_EVENT.LOCATION_KEY

     Add grid Precision and fix cases where the Spatial ref system isn't OSNI or OSGB
     in which case we store the Lat/Long. Remove the East and North fields if
     they are empty.

     Fix dates to ensure Nulls are handled correctly and replace DateType "P"
     with "OO"

  Parameters
     Use SampleLocationName first (1) or LOCATION name first (0)
	
  Revision information
  Updated SGB 16 Jan 2009
          SGB 08 Jul 2010

\*===========================================================================*/
CREATE procedure [dbo].[nbn_exchange_basic_update]
	@SampleLocationNameFirst int =1
AS
DECLARE @n as int

-- Populate columns with basic information
UPDATE ##nbn_exchange_obs
SET  SurveyKey = SURVEY_EVENT.SURVEY_KEY,
     SampleKey = SAMPLE.SAMPLE_KEY,
	 TaxonVersionKey = TAXON_LIST_ITEM.TAXON_VERSION_KEY,   
     DateType = SAMPLE.VAGUE_DATE_TYPE, 
     GridReference = SAMPLE.SPATIAL_REF, 
     Projection = SAMPLE.SPATIAL_REF_SYSTEM, 
     ZeroAbundance = CASE TAXON_OCCURRENCE.ZERO_ABUNDANCE WHEN 1 THEN 'T' ELSE 'F' END, 
     Sensitive = CASE TAXON_OCCURRENCE.CONFIDENTIAL WHEN 1 THEN 'T' ELSE 'F' END
FROM         TAXON_OCCURRENCE INNER JOIN
                      TAXON_DETERMINATION ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY INNER JOIN
                      SAMPLE ON TAXON_OCCURRENCE.SAMPLE_KEY = SAMPLE.SAMPLE_KEY INNER JOIN
                      SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY INNER JOIN
                      TAXON_LIST_ITEM ON TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY
WHERE     (TAXON_DETERMINATION.PREFERRED = 1) AND (##nbn_exchange_obs.RecordKey=TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY);

-- *********
-- Site Name
-- *********
IF @SampleLocationNameFirst = 1
BEGIN
	-- Get the SiteName from the LOCATION_NAME where we can
	UPDATE ##nbn_exchange_obs 
	SET SiteName = SAMPLE.LOCATION_NAME
	FROM ##nbn_exchange_obs INNER JOIN
	SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY;
END

-- If it is still null, try getting it from the SAMPLE.LOCATION_KEY
UPDATE ##nbn_exchange_obs 
SET SiteName = LOCATION_NAME.ITEM_NAME,
    SiteKey = SAMPLE.LOCATION_KEY 
FROM ##nbn_exchange_obs INNER JOIN
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY
INNER JOIN LOCATION_NAME ON SAMPLE.LOCATION_KEY = LOCATION_NAME.LOCATION_KEY AND LOCATION_NAME.PREFERRED = 1
WHERE SiteName Is Null OR LEN(SiteName)=0;

-- Finally, try getting it from the SURVEY_EVENT.LOCATION_KEY
UPDATE ##nbn_exchange_obs 
SET SiteName = LOCATION_NAME.ITEM_NAME,
    SiteKey = SURVEY_EVENT.LOCATION_KEY
FROM ##nbn_exchange_obs INNER JOIN
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY
INNER JOIN SURVEY_EVENT ON SAMPLE.SURVEY_EVENT_KEY = SURVEY_EVENT.SURVEY_EVENT_KEY
INNER JOIN LOCATION_NAME ON SURVEY_EVENT.LOCATION_KEY = LOCATION_NAME.LOCATION_KEY AND LOCATION_NAME.PREFERRED = 1
WHERE SiteName Is Null OR LEN(SiteName)=0;

-- if the option to use LocationName first was chosen
IF @SampleLocationNameFirst = 0
BEGIN
	-- Get the SiteName from the LOCATION_NAME
	UPDATE ##nbn_exchange_obs 
	SET SiteName = SAMPLE.LOCATION_NAME
	FROM ##nbn_exchange_obs INNER JOIN
	SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY
	WHERE SiteName Is Null OR LEN(SiteName)=0;
END


-- ***************
-- Grid references
-- ***************
-- Set the Preision field
UPDATE ##nbn_exchange_obs
SET [Precision] = CASE Len(GridReference)
	WHEN 4 THEN 10000
    WHEN 5 THEN  2000
    WHEN 6 THEN  1000
    WHEN 8 THEN   100
    WHEN 10 THEN   10
    WHEN 12 THEN    1
    ELSE 0
END
WHERE Projection='OSGB';

UPDATE ##nbn_exchange_obs
SET [Precision] = CASE Len(GridReference)
	WHEN 3 THEN 10000
    WHEN 4 THEN  2000
    WHEN 5 THEN  1000
    WHEN 7 THEN   100
    WHEN 9 THEN   10
    WHEN 11 THEN    1
    ELSE 0
END
WHERE Projection='OSNI';

-- In case where the Projection is not OSNI or OSGB, store the lat/long
UPDATE ##nbn_exchange_obs 
SET Projection = 'OSGB36', 
	GridReference = Null, 
	East = SAMPLE.[LONG], 
	North = SAMPLE.[LAT], 
	[Precision] = 100
FROM ##nbn_exchange_obs INNER JOIN SAMPLE 
ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY 
WHERE not (Projection='OSNI' Or Projection='OSGB');

-- Check whether there are rows, updated by the
-- previous command, where East, North are used
SELECT @n = COUNT(SampleKey)
FROM   ##nbn_exchange_obs
WHERE  (Projection = 'OSGB36')

-- If not, then remove these fields
IF @n = 0
BEGIN
	ALTER TABLE ##nbn_exchange_obs DROP COLUMN East, North	
END

-- =======
-- Dates
-- =======
--
-- Update StartDate and EndDate with dates in string format
-- in rows where the dates are not null
--
-- *********
-- StartDate
-- *********
UPDATE ##nbn_exchange_obs 
SET StartDate = dbo.nbn_exchange_date_to_string(SAMPLE.VAGUE_DATE_START)
FROM ##nbn_exchange_obs INNER JOIN
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY
WHERE SAMPLE.VAGUE_DATE_START is not null;

-- *******
-- EndDate
-- *******
UPDATE ##nbn_exchange_obs 
SET EndDate = dbo.nbn_exchange_date_to_string(SAMPLE.VAGUE_DATE_END)
FROM ##nbn_exchange_obs INNER JOIN
SAMPLE ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY
WHERE SAMPLE.VAGUE_DATE_END is not null;

-- Make sure nulls are set correctly according to DateType
UPDATE ##nbn_exchange_obs
SET	EndDate = Null
WHERE DateType='U' Or DateType='Y-';

UPDATE ##nbn_exchange_obs
SET	StartDate = Null
WHERE DateType='U' Or DateType='-Y';

UPDATE ##nbn_exchange_obs
SET	EndDate = Null,
    StartDate = Null,
	DateType = 'U'
WHERE DateType Is Null;

-- **************
-- "P" type dates
-- **************
UPDATE ##nbn_exchange_obs
SET    DateType = 'OO'
WHERE (DateType = 'P')
 
-- **************************************************
-- NBN Gateway does not support "S" or "M" type dates
-- Delete any rows with these date types
-- **************************************************
DELETE FROM ##nbn_exchange_obs
WHERE (DateType IN ('S', 'M'))
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_basic_update') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_basic_update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_basic_update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_basic_update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_basic_update TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_basic_update TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_create_tables]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_create_tables]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    16 May 2008

  Description	
     Create the tables needed to support export to NBN exchange format.

  Parameters
     None.
	
  Revision information

\*===========================================================================*/
CREATE PROCEDURE [dbo].[nbn_exchange_create_tables]
AS

-- Table in which to assemble the rows to export
CREATE TABLE [##nbn_exchange_obs]
(
	[RecordKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[SurveyKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SampleKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[StartDate] [varchar](12) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[EndDate] [varchar](12) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[DateType] [char](2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[TaxonVersionKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[ZeroAbundance] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Sensitive] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SiteKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SiteName] [varchar](255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Projection] [varchar](10) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[GridReference] [varchar](30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Precision] [int] NULL,
	[East] [float] NULL,
	[North] [float] NULL,
	[Recorder] [varchar](4096) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Determiner] [varchar](4096) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SampleMethod] [varchar](50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Comment] [varchar](1024) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Substrate] [varchar](20) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)
-- make RecordKey the primary key
ALTER TABLE [##nbn_exchange_obs] 
ADD CONSTRAINT pk_RecordKey PRIMARY KEY (RecordKey)

-- table containing the key(s) selected by the user in Rec6
CREATE TABLE [##nbn_exchange_export]
(
	[table_name] [varchar](30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[nbn_key] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) 

-- results of the validation check before the export
-- rows identified here should be excluded from the export
CREATE TABLE [##nbn_exchange_invalid]
(
	[table_name] [varchar](30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[nbn_key] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_create_tables') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_drop_tables]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_drop_tables]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    16 May 2008

  Description	
     Drop the tables used to support export to NBN exchange format.

  Parameters
     None.
	
  Revision information

\*===========================================================================*/
CREATE PROCEDURE [dbo].[nbn_exchange_drop_tables]
AS

DROP TABLE [##nbn_exchange_obs]

DROP TABLE [##nbn_exchange_export]

DROP TABLE [##nbn_exchange_invalid]
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_drop_tables') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_drop_tables TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_drop_tables TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_drop_tables TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_drop_tables TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_drop_tables TO [R2k_RecordCardsOnly]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_get_obs]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_get_obs]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    20 May 2008

  Description	
     Populate the nbn_exchange_obs with TAXON_OCCURRENCE_KEYs
     This will depend upon the type of data that the user selected to report
     so we need to do a series of queries for each table type in nbn_exchange_export.
     We then need to remove any invalid rows linked to the entries in nbn_exchange_invalid.
     Again, the way we do this depends upon the type of record that failed validation.

  Parameters
     ZeroAbundance - user want's to include ZERO_ABUNDANCE records 0/1.
     Sensitive - user wants ti include CONFIDENTIAL records 0/1.
	
  Revision information
  SGB 16/01/2009  Added DELETE query at the end to remove rows where the
                  TAXON_VERSION entry  is not system supplied

\*===========================================================================*/
CREATE PROCEDURE [dbo].[nbn_exchange_get_obs]
	@ZeroAbundance int =0,
	@Sensitive int =0
AS
DECLARE @aTable varchar(30)
DECLARE @MyCursor CURSOR

-- get the tables from nbn_exchange_export
SET @MyCursor = CURSOR FAST_FORWARD
FOR Select table_name From ##nbn_exchange_export GROUP BY table_name

-- we go through each table in turn adding TAXON_OCCURRENCE_KEYs to nbn_exchange_obs
OPEN @MyCursor
FETCH NEXT FROM @MyCursor
INTO @aTable

WHILE @@FETCH_STATUS = 0
BEGIN
	IF @aTable = 'SURVEY'
    BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   SURVEY_EVENT INNER JOIN
               SURVEY ON SURVEY_EVENT.SURVEY_KEY = SURVEY.SURVEY_KEY INNER JOIN
               SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY INNER JOIN
               TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN
               ##nbn_exchange_export ON SURVEY.SURVEY_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'SURVEY') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)	
    END
	ELSE
	IF @aTable = 'SURVEY_EVENT'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   SURVEY_EVENT INNER JOIN
               SAMPLE ON SURVEY_EVENT.SURVEY_EVENT_KEY = SAMPLE.SURVEY_EVENT_KEY INNER JOIN
               TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN
               ##nbn_exchange_export ON SURVEY_EVENT.SURVEY_EVENT_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'SURVEY_EVENT') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)	
	END
	ELSE
	IF @aTable = 'SAMPLE'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   SAMPLE INNER JOIN
               TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN
               ##nbn_exchange_export ON SAMPLE.SAMPLE_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'SAMPLE') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END
	ELSE
	IF @aTable = 'TAXON_OCCURRENCE'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   TAXON_OCCURRENCE INNER JOIN
               ##nbn_exchange_export ON 
               TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'TAXON_OCCURRENCE') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END
	ELSE
	IF @aTable = 'TAXON_LIST_ITEM'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   TAXON_OCCURRENCE INNER JOIN
               TAXON_DETERMINATION ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY INNER JOIN
               TAXON_LIST_ITEM AS TAXON_LIST_ITEM_1 ON 
               TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = TAXON_LIST_ITEM_1.TAXON_LIST_ITEM_KEY AND 
               TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = TAXON_LIST_ITEM_1.TAXON_LIST_ITEM_KEY INNER JOIN
               ##nbn_exchange_export INNER JOIN
               TAXON_LIST_ITEM ON ##nbn_exchange_export.nbn_key = TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY INNER JOIN
               NAMESERVER ON TAXON_LIST_ITEM.TAXON_VERSION_KEY = NAMESERVER.RECOMMENDED_TAXON_VERSION_KEY ON 
               TAXON_LIST_ITEM_1.TAXON_VERSION_KEY = NAMESERVER.INPUT_TAXON_VERSION_KEY
		WHERE  (##nbn_exchange_export.table_name = 'TAXON_LIST_ITEM') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END
	ELSE
	IF @aTable = 'NAME'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   SAMPLE INNER JOIN
               TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN
               SAMPLE_RECORDER ON SAMPLE.SAMPLE_KEY = SAMPLE_RECORDER.SAMPLE_KEY INNER JOIN
               SURVEY_EVENT_RECORDER ON SAMPLE_RECORDER.SE_RECORDER_KEY = SURVEY_EVENT_RECORDER.SE_RECORDER_KEY AND 
               SAMPLE_RECORDER.SE_RECORDER_KEY = SURVEY_EVENT_RECORDER.SE_RECORDER_KEY INNER JOIN
               ##nbn_exchange_export ON SURVEY_EVENT_RECORDER.NAME_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'NAME') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END
	ELSE
	IF @aTable = 'LOCATION'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   ##nbn_exchange_export INNER JOIN
               SAMPLE INNER JOIN
               TAXON_OCCURRENCE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY ON 
               ##nbn_exchange_export.nbn_key = SAMPLE.LOCATION_KEY
		WHERE  (##nbn_exchange_export.table_name = 'LOCATION') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END
	ELSE
	IF @aTable = 'REFERENCE'
	BEGIN
		INSERT INTO ##nbn_exchange_obs (RecordKey)
		SELECT TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY
		FROM   TAXON_OCCURRENCE_SOURCES INNER JOIN
               TAXON_OCCURRENCE ON 
               TAXON_OCCURRENCE_SOURCES.TAXON_OCCURRENCE_KEY = TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY INNER JOIN
               ##nbn_exchange_export ON 
               TAXON_OCCURRENCE_SOURCES.SOURCE_KEY = ##nbn_exchange_export.nbn_key
		WHERE  (##nbn_exchange_export.table_name = 'REFERENCE') AND (TAXON_OCCURRENCE.ZERO_ABUNDANCE = 0 or TAXON_OCCURRENCE.ZERO_ABUNDANCE = @ZeroAbundance) AND 
               (TAXON_OCCURRENCE.CONFIDENTIAL = 0 or TAXON_OCCURRENCE.CONFIDENTIAL = @Sensitive) AND (TAXON_OCCURRENCE.VERIFIED <> 1) AND (TAXON_OCCURRENCE.CHECKED = 1)
	END

FETCH NEXT FROM @MyCursor
INTO @aTable
END

CLOSE @MyCursor
DEALLOCATE @MyCursor

-- *******************
-- DELETE INVALID ROWS
-- *******************
-- Now we need to do more or less the same thing again to delete anything that
-- was flagged as invalid before the export, i.e. rows in nbn_exchange_invalid

-- get the tables from nbn_exchange_invalid
SET @MyCursor = CURSOR FAST_FORWARD
FOR Select table_name From ##nbn_exchange_invalid GROUP BY table_name

-- we go through each table in turn deleting corresponding TAXON_OCCURRENCE_KEYs 
-- from nbn_exchange_obs
OPEN @MyCursor
FETCH NEXT FROM @MyCursor
INTO @aTable

WHILE @@FETCH_STATUS = 0
BEGIN
	IF @aTable = 'TAXON_OCCURRENCE'
	BEGIN
		DELETE FROM ##nbn_exchange_obs
		FROM   ##nbn_exchange_obs INNER JOIN
               ##nbn_exchange_invalid ON ##nbn_exchange_obs.RecordKey = ##nbn_exchange_invalid.nbn_key
		WHERE  ##nbn_exchange_invalid.table_name = 'TAXON_OCCURRENCE'
	END

	IF @aTable = 'SAMPLE'
	BEGIN
		DELETE FROM ##nbn_exchange_obs
		FROM   ##nbn_exchange_obs INNER JOIN
               TAXON_OCCURRENCE ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ##nbn_exchange_obs.RecordKey INNER JOIN
               ##nbn_exchange_invalid ON  TAXON_OCCURRENCE.SAMPLE_KEY = ##nbn_exchange_invalid.nbn_key
		WHERE  ##nbn_exchange_invalid.table_name = 'SAMPLE'
	END

	IF @aTable = 'SURVEY_EVENT'
	BEGIN
		DELETE FROM ##nbn_exchange_obs
		FROM   ##nbn_exchange_obs INNER JOIN
               TAXON_OCCURRENCE ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ##nbn_exchange_obs.RecordKey INNER JOIN
               SAMPLE ON SAMPLE.SAMPLE_KEY = TAXON_OCCURRENCE.SAMPLE_KEY INNER JOIN
               ##nbn_exchange_invalid ON SAMPLE.SURVEY_EVENT_KEY  = ##nbn_exchange_invalid.nbn_key
		WHERE  ##nbn_exchange_invalid.table_name = 'SURVEY_EVENT'
	END

FETCH NEXT FROM @MyCursor
INTO @aTable
END

CLOSE @MyCursor
DEALLOCATE @MyCursor

-- finally, delete keys where the TAXON_VERSION is not system supplied
DELETE FROM ##nbn_exchange_obs
FROM   ##nbn_exchange_obs INNER JOIN
       TAXON_OCCURRENCE ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = ##nbn_exchange_obs.RecordKey INNER JOIN
       TAXON_DETERMINATION ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY INNER JOIN
       TAXON_LIST_ITEM ON TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY INNER JOIN
       TAXON_VERSION ON TAXON_LIST_ITEM.TAXON_VERSION_KEY = TAXON_VERSION.TAXON_VERSION_KEY
WHERE (TAXON_VERSION.SYSTEM_SUPPLIED_DATA = 0)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_get_obs') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_get_obs TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_get_obs TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_get_obs TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_get_obs TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_get_obs TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_date_to_string]')
	   AND 	  Type = 'FN')
    DROP FUNCTION [dbo].[nbn_exchange_date_to_string]
GO


/*===========================================================================*\
  Revision information
  SGB  14/12/2012  Correct a bug in the way leap years are handled
                   Fix from Graham French

\*===========================================================================*/
CREATE FUNCTION [dbo].[nbn_exchange_date_to_string](@IntDate int)
RETURNS varchar(11)
AS
BEGIN

--****************************************************************************************************
--constants
declare @D1 int
set @D1=365 							-- days in 1 year
declare @D4 int
set @D4 = @D1 * 4 + 1			-- days in 4 years, inc leap year
declare @D100 int
set @D100 = @D4 * 25 - 1 	-- days in 100 years (no leap year on 100th year)
declare @D400 int
set @D400 = @D100 * 4 + 1	-- days in 400 years - every 400 years you do  get a leap year
--variables
declare @T int
declare @Y int
declare @M int
declare @D int
declare @I int
declare @L int
declare @ds as varchar(11)
-- Leap year
set @L = 0
-- get number of days since 1/1/01 
set @T = @IntDate+693593
-- find number of whole 400 year blocks
set @Y = @T / @D400
set @T = @T - @Y * @D400
set @Y = @Y * 400 + 1
set @I = @T / @D100
set @D = @T - @I * @D100
if @I=4 begin
  set @I = @I - 1
  set @D = @D + @D100
-- set @L = 1  this doesn't work - Graham French  
end
set @Y = @Y + @I * 100
set @I = @D / @D4
set @D = @D - @I * @D4
set @Y = @Y + @I * 4
set @I = @D / @D1
set @D = @D - @I * @D1
if @I = 4 begin
  set @I = @I - 1
  set @D = @D + @D1
end
set @Y = @Y + @I
-- Is it a leap year?  - test due to Graham French 
if (@Y % 4 = 0 AND @Y % 100 != 0) OR (@Y % 400 = 0) 
	set @L = 1
set @M = 1
while 1=1 begin
	set @I = case @M
		when 1 then 31
		when 2 then 28 + @L
		when 3 then 31
		when 4 then 30
  		when 5 then 31
		when 6 then 30
		when 7 then 31
		when 8 then 31
		when 9 then 30
		when 10 then 31
		when 11 then 30
		when 12 then 31
	end
	if @D<@I break
	set @D = @D - @I
	set @M = @M + 1
end

set @ds = Right(Replicate('0',3)+Convert(varchar(11),@D+1),2) + 
          '/' + Right(Replicate('0',3)+Convert(varchar(11),@M),2) + 
          '/' + Right(Replicate('0',4)+Convert(varchar(11),@Y),4)
return @ds

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_date_to_string') AND Type = 'FN')
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_date_to_string TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_date_to_string TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_date_to_string TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_date_to_string TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_date_to_string TO [R2k_RecordCardsOnly]
END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_strip_LFCR]')
	   AND 	  Type = 'FN')
    DROP FUNCTION [dbo].[nbn_exchange_strip_LFCR]
GO


CREATE FUNCTION [dbo].[nbn_exchange_strip_LFCR](@text varchar(8000))
RETURNS varchar(8000)
AS
BEGIN
	DECLARE @stuff varchar(8000)
	SET @stuff = ''
	DECLARE @i int
	SET @i = 1

	IF (LEN(@text) > 0)
	BEGIN
        WHILE @i <= LEN(@text)
		BEGIN
			IF NOT ((SUBSTRING(@text, @i, 1) = CHAR(13)) OR (SUBSTRING(@text, @i, 1) = CHAR(10)))
				SET @stuff = @stuff + SUBSTRING(@text, @i, 1)
			SET @i = @i + 1
		END
		SET @stuff = RTRIM(LTRIM(@stuff))
	END
	RETURN @stuff

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_strip_LFCR') AND Type = 'FN')
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_strip_LFCR TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_strip_LFCR TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_strip_LFCR TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_strip_LFCR TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_strip_LFCR TO [R2k_RecordCardsOnly]
END
GO

