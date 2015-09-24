
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_get_obs]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_get_obs] 
GO

/****** Object:  StoredProcedure [dbo].[nbn_exchange_get_obs]    Script Date: 11/27/2014 12:54:16 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
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
  MDW  27/11/2014 Change deleted so that it only works on the preferred taxon
  Changed queries so that they don't attempt to insert the same TOCC key twice
  otherwise the whole query fails and the data is not inserted.    

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
	           AND NOT EXISTS(SELECT * FROM ##nbn_exchange_obs WHERE RecordKey = Taxon_Occurrence.Taxon_Occurrence_Key) 
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
	            AND NOT EXISTS(SELECT * FROM ##nbn_exchange_obs WHERE RecordKey = Taxon_Occurrence.Taxon_Occurrence_Key) 
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
	            AND NOT EXISTS(SELECT * FROM ##nbn_exchange_obs WHERE RecordKey = Taxon_Occurrence.Taxon_Occurrence_Key) 
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
	           AND NOT EXISTS(SELECT * FROM ##nbn_exchange_obs WHERE RecordKey = Taxon_Occurrence.Taxon_Occurrence_Key) 
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
	            AND NOT EXISTS(SELECT * FROM ##nbn_exchange_obs WHERE RecordKey = Taxon_Occurrence.Taxon_Occurrence_Key) 
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
	            AND NOT EXISTS(SELECT * FROM ##nbn_exchange_obs WHERE RecordKey = Taxon_Occurrence.Taxon_Occurrence_Key) 
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
                AND NOT EXISTS(SELECT * FROM ##nbn_exchange_obs WHERE RecordKey = Taxon_Occurrence.Taxon_Occurrence_Key) 
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
       TAXON_DETERMINATION ON TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY = TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY AND 
       TAXON_DETERMINATION.PREFERRED = 1 
       INNER JOIN TAXON_LIST_ITEM ON TAXON_DETERMINATION.TAXON_LIST_ITEM_KEY = TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY INNER JOIN
       TAXON_VERSION ON TAXON_LIST_ITEM.TAXON_VERSION_KEY = TAXON_VERSION.TAXON_VERSION_KEY
WHERE (TAXON_VERSION.SYSTEM_SUPPLIED_DATA = 0)

GO

GRANT EXECUTE ON [dbo].[nbn_exchange_get_obs] TO PUBLIC

