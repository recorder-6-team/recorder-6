If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spUpdateCustodianFields]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure spUpdateCustodianFields'
        DROP PROCEDURE [dbo].[spUpdateCustodianFields]
    END
GO

    PRINT 'Creating procedure spUpdateCustodianFields'
GO

/*
    $History: spUpdateCustodianFields.sql $
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 7/02/03    Time: 10:14
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Updated permissions at end of script
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 20/01/03   Time: 15:36
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Initial Build
 */

CREATE PROCEDURE dbo.spUpdateCustodianFields
--
--	DESCRIPTION
--	This procedure updates the custodian fields for each of the specified tables.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--
--
--	AUTHOR:	Ben Collier, Dorset Software.
--	CREATED: 20/01/2003
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
AS
SET NOCOUNT ON

EXEC spUpdateCustodianField 'ADDRESS', 'ADDRESS_KEY'
EXEC spUpdateCustodianField 'ADMIN_AREA', 'ADMIN_AREA_KEY'
EXEC spUpdateCustodianField 'ADMIN_BOUNDARY', 'ADMIN_BOUNDARY_KEY'
EXEC spUpdateCustodianField 'ADMIN_RELATION', 'ADMIN_RELATION_KEY'
EXEC spUpdateCustodianField 'ADMIN_TYPE', 'ADMIN_TYPE_KEY'
EXEC spUpdateCustodianField 'BIOTOPE', 'BIOTOPE_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_CLASSIFICATION', 'BIOTOPE_CLASSIFICATION_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_CLASSIFICATION_TYPE', 'BT_CL_TYPE_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_CLASSIFICATION_VERSION', 'BT_CL_VERSION_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_DESIGNATION', 'BIOTOPE_DESIGNATION_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_DESIGNATION_TYPE', 'BIOTOPE_DESIGNATION_TYPE_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_DETERMINATION', 'BIOTOPE_DETERMINATION_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_FACT', 'BIOTOPE_FACT_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_LIST_ITEM', 'BIOTOPE_LIST_ITEM_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_OCCURRENCE', 'BIOTOPE_OCCURRENCE_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_OCCURRENCE_DATA', 'BIOTOPE_OCCURRENCE_DATA_KEY'
EXEC spUpdateCustodianField 'BIOTOPE_RELATION', 'BIOTOPE_RELATION_KEY'
EXEC spUpdateCustodianField 'COMMUNICATION', 'COMMUNICATION_KEY'
EXEC spUpdateCustodianField 'CONTACT_NUMBER', 'CONTACT_NUMBER_KEY'
EXEC spUpdateCustodianField 'DAMAGE_OCCURRENCE', 'DAMAGE_OCCURRENCE_KEY'
EXEC spUpdateCustodianField 'DETERMINATION_TYPE', 'DETERMINATION_TYPE_KEY'
EXEC spUpdateCustodianField 'DETERMINER_ROLE', 'DETERMINER_ROLE_KEY'
EXEC spUpdateCustodianField 'GRID_SQUARE', 'GRID_SQUARE_KEY'
EXEC spUpdateCustodianField 'JOURNAL', 'JOURNAL_KEY'
EXEC spUpdateCustodianField 'LAND_PARCEL', 'LAND_PARCEL_KEY'
EXEC spUpdateCustodianField 'LOCATION', 'LOCATION_KEY'
EXEC spUpdateCustodianField 'LOCATION_ADMIN_AREAS', 'LOCATION_ADMIN_AREAS_KEY'
EXEC spUpdateCustodianField 'LOCATION_BOUNDARY', 'LOCATION_BOUNDARY_KEY'
EXEC spUpdateCustodianField 'LOCATION_DATA', 'LOCATION_DATA_KEY'
EXEC spUpdateCustodianField 'LOCATION_DESIGNATION', 'DESIGNATION_KEY'
EXEC spUpdateCustodianField 'LOCATION_FEATURE', 'LOCATION_FEATURE_KEY'
EXEC spUpdateCustodianField 'LOCATION_FEATURE_GRADING', 'FEATURE_GRADING_KEY'
EXEC spUpdateCustodianField 'LOCATION_FEATURE_TYPE', 'LOCATION_FEATURE_TYPE_KEY'
EXEC spUpdateCustodianField 'LOCATION_NAME', 'LOCATION_NAME_KEY'
EXEC spUpdateCustodianField 'LOCATION_RELATION', 'LOCATION_RELATION_KEY'
EXEC spUpdateCustodianField 'LOCATION_TYPE', 'LOCATION_TYPE_KEY'
EXEC spUpdateCustodianField 'LOCATION_USE', 'LOCATION_USE_KEY'
EXEC spUpdateCustodianField 'MANAGEMENT_AIM', 'MANAGEMENT_AIM_KEY'
EXEC spUpdateCustodianField 'MEASUREMENT_QUALIFIER', 'MEASUREMENT_QUALIFIER_KEY'
EXEC spUpdateCustodianField 'MEASUREMENT_TYPE', 'MEASUREMENT_TYPE_KEY'
EXEC spUpdateCustodianField 'MEASUREMENT_UNIT', 'MEASUREMENT_UNIT_KEY'
EXEC spUpdateCustodianField 'NAME', 'NAME_KEY'
EXEC spUpdateCustodianField 'NAME_RELATION', 'NAME_RELATION_KEY'
EXEC spUpdateCustodianField 'ORGANISATION_TYPE', 'ORGANISATION_TYPE_KEY'
EXEC spUpdateCustodianField 'POTENTIAL_THREAT', 'POTENTIAL_THREAT_KEY'
EXEC spUpdateCustodianField 'LOCATION_TYPE', 'LOCATION_TYPE_KEY'
EXEC spUpdateCustodianField 'RECORD_TYPE', 'RECORD_TYPE_KEY'
EXEC spUpdateCustodianField 'RECORDER_ROLE', 'RECORDER_ROLE_KEY'
EXEC spUpdateCustodianField 'REFERENCE_AUTHOR', 'AUTHOR_KEY'
EXEC spUpdateCustodianField 'REFERENCE_EDITOR', 'EDITOR_KEY'
EXEC spUpdateCustodianField 'REFERENCE_NUMBER', 'NUMBER_KEY'
EXEC spUpdateCustodianField 'RELATIONSHIP_TYPE', 'RELATIONSHIP_TYPE_KEY'
EXEC spUpdateCustodianField 'SAMPLE', 'SAMPLE_KEY'
EXEC spUpdateCustodianField 'SAMPLE_DATA', 'SAMPLE_DATA_KEY'
EXEC spUpdateCustodianField 'SAMPLE_RELATION', 'SAMPLE_RELATION_KEY'
EXEC spUpdateCustodianField 'SAMPLE_TYPE', 'SAMPLE_TYPE_KEY'
EXEC spUpdateCustodianField 'SITE_STATUS', 'SITE_STATUS_KEY'
EXEC spUpdateCustodianField 'SOURCE', 'SOURCE_KEY'
EXEC spUpdateCustodianField 'SPECIMEN', 'SPECIMEN_KEY'
EXEC spUpdateCustodianField 'SPECIMEN_TYPE', 'SPECIMEN_TYPE_KEY'
EXEC spUpdateCustodianField 'SUBSTRATE', 'SUBSTRATE_KEY'
EXEC spUpdateCustodianField 'SURVEY', 'SURVEY_KEY'
EXEC spUpdateCustodianField 'SURVEY_EVENT', 'SURVEY_EVENT_KEY'
EXEC spUpdateCustodianField 'SURVEY_EVENT_RECORDER', 'SE_RECORDER_KEY'
EXEC spUpdateCustodianField 'SURVEY_MEDIA', 'SURVEY_MEDIA_KEY'
EXEC spUpdateCustodianField 'SURVEY_STATUS', 'SURVEY_STATUS_KEY'
EXEC spUpdateCustodianField 'SURVEY_TYPE', 'SURVEY_TYPE_KEY'
EXEC spUpdateCustodianField 'TAXON', 'TAXON_KEY'
EXEC spUpdateCustodianField 'TAXON_BIOTOPE_ASSOCIATION', 'ASSOCIATION_KEY'
EXEC spUpdateCustodianField 'TAXON_DESIGNATION', 'TAXON_DESIGNATION_KEY'
EXEC spUpdateCustodianField 'TAXON_DESIGNATION_TYPE', 'TAXON_DESIGNATION_TYPE_KEY'
EXEC spUpdateCustodianField 'TAXON_DETERMINATION', 'TAXON_DETERMINATION_KEY'
EXEC spUpdateCustodianField 'TAXON_FACT', 'TAXON_FACT_KEY'
EXEC spUpdateCustodianField 'TAXON_LIST', 'TAXON_LIST_KEY'
EXEC spUpdateCustodianField 'TAXON_LIST_ITEM', 'TAXON_LIST_ITEM_KEY'
EXEC spUpdateCustodianField 'TAXON_LIST_TYPE', 'TAXON_LIST_TYPE_KEY'
EXEC spUpdateCustodianField 'TAXON_LIST_VERSION', 'TAXON_LIST_VERSION_KEY'
EXEC spUpdateCustodianField 'TAXON_NAME_TYPE', 'TAXON_NAME_TYPE_KEY'
EXEC spUpdateCustodianField 'TAXON_OCCURRENCE', 'TAXON_OCCURRENCE_KEY'
EXEC spUpdateCustodianField 'TAXON_OCCURRENCE_DATA', 'TAXON_OCCURRENCE_DATA_KEY'
EXEC spUpdateCustodianField 'TAXON_OCCURRENCE_RELATION', 'TAXON_OCCURRENCE_RELATION_KEY'
EXEC spUpdateCustodianField 'TAXON_RANK', 'TAXON_RANK_KEY'
EXEC spUpdateCustodianField 'TAXON_TAXON_ASSOCIATION', 'ASSOCIATION_KEY'
EXEC spUpdateCustodianField 'TAXON_VERSION', 'TAXON_VERSION_KEY'
EXEC spUpdateCustodianField 'TAXON_VERSION_RELATION', 'TAXON_VERSION_RELATION_KEY'
EXEC spUpdateCustodianField 'TENURE', 'TENURE_KEY'
EXEC spUpdateCustodianField 'TENURE_TYPE', 'TENURE_TYPE_KEY'
EXEC spUpdateCustodianField 'THREAT_TYPE', 'THREAT_TYPE_KEY'

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
GO

    IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.spUpdateCustodianFields') AND SysStat & 0xf = 4)
    BEGIN
        PRINT 'Setting up security on procedure spUpdateCustodianFields'
        IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev- JNCC SQL')
	       	GRANT EXECUTE ON dbo.spUpdateCustodianFields TO [Dev- JNCC SQL]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
       		GRANT EXECUTE ON dbo.spUpdateCustodianFields TO [R2k_Administrator]
    END
GO