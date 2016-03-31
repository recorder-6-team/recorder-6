
/****** Object:  StoredProcedure [dbo].[nbn_exchange_basic_update]    Script Date: 10/25/2015 19:37:14 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
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
     they are empty. Conveerts Lat/Long to WSG84

     Fix dates to ensure Nulls are handled correctly and replace DateType "P"
     with "OO"

  Parameters
     Use SampleLocationName first (1) or LOCATION name first (0)
	
  Revision information
  Updated SGB 16 Jan 2009
          SGB 08 Jul 2010
          MDW Oct 2015 - Converts Lat/Long to WSG84

\*===========================================================================*/
ALTER procedure [dbo].[nbn_exchange_basic_update]
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
SET Projection = 'WGS84', 
	GridReference = Null, 
	East = [dbo].[LCOSGBLLtoWSG84LL](SAMPLE.[LAT], SAMPLE.[LONG],0), 
	North = [dbo].[LCOSGBLLtoWSG84LL](SAMPLE.[LAT], SAMPLE.[LONG],1),
	[Precision] = 100
FROM ##nbn_exchange_obs INNER JOIN SAMPLE 
ON ##nbn_exchange_obs.SampleKey = SAMPLE.SAMPLE_KEY 
WHERE not (Projection='OSNI' Or Projection='OSGB');

-- Drop Lat/Longs Outside NBN Area

Delete from ##nbn_exchange_obs where East < -14 OR
EAST > 13 OR North < 48 OR North > 62
 


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

-- Check whether there are rows, updated by the
-- previous command, where East, North are used
SELECT @n = COUNT(SampleKey)
FROM   ##nbn_exchange_obs
WHERE  (Projection = 'WGS84')

-- If not, then remove these fields
IF @n = 0
BEGIN
	ALTER TABLE ##nbn_exchange_obs DROP COLUMN East, North	
END

GO
