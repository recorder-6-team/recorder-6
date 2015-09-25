If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spCreateCustodianTriggers]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure spCreateCustodianTriggers'
        DROP PROCEDURE [dbo].[spCreateCustodianTriggers]
    END
GO

    PRINT 'Creating procedure spCreateCustodianTriggers'
GO

/*
    $History: spCreateCustodianTriggers.sql $
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 7/02/03    Time: 10:14
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Updated permissions at end of script
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 14/01/03   Time: 17:29
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Initial Build
 */

CREATE PROCEDURE dbo.spCreateCustodianTriggers
--
--	DESCRIPTION
--	This procedure creates the triggers required to maintain the custodian field for each table.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--
--
--	AUTHOR:	Ben Collier, Dorset Software.
--	CREATED: 07/01/2003
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
AS
SET NOCOUNT ON

EXEC spCreateCustodianTrigger 'ADDRESS', 'ADDRESS_KEY'
EXEC spCreateCustodianTrigger 'ADMIN_AREA', 'ADMIN_AREA_KEY'
EXEC spCreateCustodianTrigger 'ADMIN_BOUNDARY', 'ADMIN_BOUNDARY_KEY'
EXEC spCreateCustodianTrigger 'ADMIN_RELATION', 'ADMIN_RELATION_KEY'
EXEC spCreateCustodianTrigger 'ADMIN_TYPE', 'ADMIN_TYPE_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE', 'BIOTOPE_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_CLASSIFICATION', 'BIOTOPE_CLASSIFICATION_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_CLASSIFICATION_TYPE', 'BT_CL_TYPE_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_CLASSIFICATION_VERSION', 'BT_CL_VERSION_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_DESIGNATION', 'BIOTOPE_DESIGNATION_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_DESIGNATION_TYPE', 'BIOTOPE_DESIGNATION_TYPE_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_DETERMINATION', 'BIOTOPE_DETERMINATION_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_FACT', 'BIOTOPE_FACT_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_LIST_ITEM', 'BIOTOPE_LIST_ITEM_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_OCCURRENCE', 'BIOTOPE_OCCURRENCE_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_OCCURRENCE_DATA', 'BIOTOPE_OCCURRENCE_DATA_KEY'
EXEC spCreateCustodianTrigger 'BIOTOPE_RELATION', 'BIOTOPE_RELATION_KEY'
EXEC spCreateCustodianTrigger 'COMMUNICATION', 'COMMUNICATION_KEY'
EXEC spCreateCustodianTrigger 'CONTACT_NUMBER', 'CONTACT_NUMBER_KEY'
EXEC spCreateCustodianTrigger 'DAMAGE_OCCURRENCE', 'DAMAGE_OCCURRENCE_KEY'
EXEC spCreateCustodianTrigger 'DETERMINATION_TYPE', 'DETERMINATION_TYPE_KEY'
EXEC spCreateCustodianTrigger 'DETERMINER_ROLE', 'DETERMINER_ROLE_KEY'
EXEC spCreateCustodianTrigger 'GRID_SQUARE', 'GRID_SQUARE_KEY'
EXEC spCreateCustodianTrigger 'JOURNAL', 'JOURNAL_KEY'
EXEC spCreateCustodianTrigger 'LAND_PARCEL', 'LAND_PARCEL_KEY'
EXEC spCreateCustodianTrigger 'LOCATION', 'LOCATION_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_ADMIN_AREAS', 'LOCATION_ADMIN_AREAS_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_BOUNDARY', 'LOCATION_BOUNDARY_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_DATA', 'LOCATION_DATA_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_DESIGNATION', 'DESIGNATION_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_FEATURE', 'LOCATION_FEATURE_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_FEATURE_GRADING', 'FEATURE_GRADING_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_FEATURE_TYPE', 'LOCATION_FEATURE_TYPE_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_NAME', 'LOCATION_NAME_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_RELATION', 'LOCATION_RELATION_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_TYPE', 'LOCATION_TYPE_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_USE', 'LOCATION_USE_KEY'
EXEC spCreateCustodianTrigger 'MANAGEMENT_AIM', 'MANAGEMENT_AIM_KEY'
EXEC spCreateCustodianTrigger 'MEASUREMENT_QUALIFIER', 'MEASUREMENT_QUALIFIER_KEY'
EXEC spCreateCustodianTrigger 'MEASUREMENT_TYPE', 'MEASUREMENT_TYPE_KEY'
EXEC spCreateCustodianTrigger 'MEASUREMENT_UNIT', 'MEASUREMENT_UNIT_KEY'
EXEC spCreateCustodianTrigger 'NAME', 'NAME_KEY'
EXEC spCreateCustodianTrigger 'NAME_RELATION', 'NAME_RELATION_KEY'
EXEC spCreateCustodianTrigger 'ORGANISATION_TYPE', 'ORGANISATION_TYPE_KEY'
EXEC spCreateCustodianTrigger 'POTENTIAL_THREAT', 'POTENTIAL_THREAT_KEY'
EXEC spCreateCustodianTrigger 'LOCATION_TYPE', 'LOCATION_TYPE_KEY'
EXEC spCreateCustodianTrigger 'RECORD_TYPE', 'RECORD_TYPE_KEY'
EXEC spCreateCustodianTrigger 'RECORDER_ROLE', 'RECORDER_ROLE_KEY'
EXEC spCreateCustodianTrigger 'REFERENCE_AUTHOR', 'AUTHOR_KEY'
EXEC spCreateCustodianTrigger 'REFERENCE_EDITOR', 'EDITOR_KEY'
EXEC spCreateCustodianTrigger 'REFERENCE_NUMBER', 'NUMBER_KEY'
EXEC spCreateCustodianTrigger 'RELATIONSHIP_TYPE', 'RELATIONSHIP_TYPE_KEY'
EXEC spCreateCustodianTrigger 'SAMPLE', 'SAMPLE_KEY'
EXEC spCreateCustodianTrigger 'SAMPLE_DATA', 'SAMPLE_DATA_KEY'
EXEC spCreateCustodianTrigger 'SAMPLE_RELATION', 'SAMPLE_RELATION_KEY'
EXEC spCreateCustodianTrigger 'SAMPLE_TYPE', 'SAMPLE_TYPE_KEY'
EXEC spCreateCustodianTrigger 'SITE_STATUS', 'SITE_STATUS_KEY'
EXEC spCreateCustodianTrigger 'SOURCE', 'SOURCE_KEY'
EXEC spCreateCustodianTrigger 'SPECIMEN', 'SPECIMEN_KEY'
EXEC spCreateCustodianTrigger 'SPECIMEN_TYPE', 'SPECIMEN_TYPE_KEY'
EXEC spCreateCustodianTrigger 'SUBSTRATE', 'SUBSTRATE_KEY'
EXEC spCreateCustodianTrigger 'SURVEY', 'SURVEY_KEY'
EXEC spCreateCustodianTrigger 'SURVEY_EVENT', 'SURVEY_EVENT_KEY'
EXEC spCreateCustodianTrigger 'SURVEY_EVENT_RECORDER', 'SE_RECORDER_KEY'
EXEC spCreateCustodianTrigger 'SURVEY_MEDIA', 'SURVEY_MEDIA_KEY'
EXEC spCreateCustodianTrigger 'SURVEY_STATUS', 'SURVEY_STATUS_KEY'
EXEC spCreateCustodianTrigger 'SURVEY_TYPE', 'SURVEY_TYPE_KEY'
EXEC spCreateCustodianTrigger 'TAXON', 'TAXON_KEY'
EXEC spCreateCustodianTrigger 'TAXON_BIOTOPE_ASSOCIATION', 'ASSOCIATION_KEY'
EXEC spCreateCustodianTrigger 'TAXON_DESIGNATION', 'TAXON_DESIGNATION_KEY'
EXEC spCreateCustodianTrigger 'TAXON_DESIGNATION_TYPE', 'TAXON_DESIGNATION_TYPE_KEY'
EXEC spCreateCustodianTrigger 'TAXON_DETERMINATION', 'TAXON_DETERMINATION_KEY'
EXEC spCreateCustodianTrigger 'TAXON_FACT', 'TAXON_FACT_KEY'
EXEC spCreateCustodianTrigger 'TAXON_LIST', 'TAXON_LIST_KEY'
EXEC spCreateCustodianTrigger 'TAXON_LIST_ITEM', 'TAXON_LIST_ITEM_KEY'
EXEC spCreateCustodianTrigger 'TAXON_LIST_TYPE', 'TAXON_LIST_TYPE_KEY'
EXEC spCreateCustodianTrigger 'TAXON_LIST_VERSION', 'TAXON_LIST_VERSION_KEY'
EXEC spCreateCustodianTrigger 'TAXON_NAME_TYPE', 'TAXON_NAME_TYPE_KEY'
EXEC spCreateCustodianTrigger 'TAXON_OCCURRENCE', 'TAXON_OCCURRENCE_KEY'
EXEC spCreateCustodianTrigger 'TAXON_OCCURRENCE_DATA', 'TAXON_OCCURRENCE_DATA_KEY'
EXEC spCreateCustodianTrigger 'TAXON_OCCURRENCE_RELATION', 'TAXON_OCCURRENCE_RELATION_KEY'
EXEC spCreateCustodianTrigger 'TAXON_RANK', 'TAXON_RANK_KEY'
EXEC spCreateCustodianTrigger 'TAXON_TAXON_ASSOCIATION', 'ASSOCIATION_KEY'
EXEC spCreateCustodianTrigger 'TAXON_VERSION', 'TAXON_VERSION_KEY'
EXEC spCreateCustodianTrigger 'TAXON_VERSION_RELATION', 'TAXON_VERSION_RELATION_KEY'
EXEC spCreateCustodianTrigger 'TENURE', 'TENURE_KEY'
EXEC spCreateCustodianTrigger 'TENURE_TYPE', 'TENURE_TYPE_KEY'
EXEC spCreateCustodianTrigger 'THREAT_TYPE', 'THREAT_TYPE_KEY'

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
GO

    IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.spCreateCustodianTriggers') AND SysStat & 0xf = 4)
    BEGIN
        PRINT 'Setting up security on procedure spCreateCustodianTriggers'
        IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev- JNCC SQL')
        	GRANT EXECUTE ON dbo.spCreateCustodianTriggers TO [Dev- JNCC SQL]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
       		GRANT EXECUTE ON dbo.spCreateCustodianTriggers TO [R2k_Administrator]
    END
GO