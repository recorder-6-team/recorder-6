/****** SQL Fixing Data To Long For Field for Cosnolidated Recorders in IW  ******/
UPDATE REPORT_FIELD SET FIELD_SIZE = 8000 WHERE REPORT_FIELD_KEY = 'LCA0002200000009'
GO
/****** Sets the value for Temporary_Survey to 0 in the SP which sets up the initial Survey on a new install 7:58 ******/
/****** Object:  StoredProcedure [dbo].[usp_Survey_Insert]    Script Date: 10/07/2019 13:17:58 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:
	Inserts a new survey record.

  Parameters:
		@Key,
		@ItemName,
		@Description,
		@RunByKey,
		@SurveyStatusKey,
		@SurveyMediaKey,
		@SurveyTypeKey,
		@LicenceKey,
		@FromVagueDateStart,
		@FromVagueDateEnd,
		@FromVagueDateType,
		@ToVagueDateStart,
		@ToVagueDateEnd,
		@ToVagueDateType,
		@OPFromVagueDateStart,
		@OPFromVagueDateEnd,
		@OPFromVagueDateType,
		@OPToVagueDateStart,
		@OPToVagueDateEnd,
		@OPToVagueDateType,
		@SWSpatialRef,
		@SWLat,
		@SWLong,
		@SWSpatialRefQualifier,
		@NESpatialRef,
		@NELat,
		@NELong,
		@NESpatialRefQualifier,
		@SpatialRefSystem,
		@GeographicCoverage,
		@Periodicity,
		@EnteredBy

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/

ALTER PROCEDURE [dbo].[usp_Survey_Insert]
	@Key					CHAR(16)	OUTPUT,
	@ItemName				VARCHAR(100),
	@Description			TEXT		= NULL,
	@RunByKey				CHAR(16),
	@SurveyStatusKey		CHAR(16)	= NULL,
	@SurveyMediaKey			CHAR(16)	= NULL,
	@SurveyTypeKey			CHAR(16),
	@LicenceKey		        CHAR(16),
	@FromVagueDateStart		INT			= NULL,
	@FromVagueDateEnd		INT			= NULL,
	@FromVagueDateType		VARCHAR(2),
	@ToVagueDateStart		INT			= NULL,
	@ToVagueDateEnd			INT			= NULL,
	@ToVagueDateType		VARCHAR(2)	= NULL,
	@OPFromVagueDateStart	INT			= NULL,
	@OPFromVagueDateEnd		INT			= NULL,
	@OPFromVagueDateType	VARCHAR(2)	= NULL,
	@OPToVagueDateStart		INT			= NULL,
	@OPToVagueDateEnd		INT			= NULL,
	@OPToVagueDateType		VARCHAR(2)	= NULL,
	@SWSpatialRef			VARCHAR(40) = NULL,
	@SWLat					FLOAT		= NULL,
	@SWLong					FLOAT		= NULL,
	@SWSpatialRefQualifier	VARCHAR(20) = NULL,
	@NESpatialRef			VARCHAR(40) = NULL,
	@NELat					FLOAT		= NULL,
	@NELong					FLOAT		= NULL,
	@NESpatialRefQualifier	VARCHAR(20) = NULL,
	@SpatialRefSystem		VARCHAR(4)	= NULL,
	@GeographicCoverage		TEXT		= NULL,
	@Periodicity			VARCHAR(16) = NULL,
	@EnteredBy				CHAR(16)
AS
	SET NOCOUNT ON

	EXECUTE spNextKey 'Survey', @Key OUTPUT

	INSERT INTO Survey (
		Survey_Key,
		Item_Name,
		Description,
		Run_By,
		Survey_Status_Key,
		Survey_Media_Key,
		Survey_Type_Key,
		Licence_key,
		From_Vague_Date_Start,
		From_Vague_Date_End,
		From_Vague_Date_Type,
		To_Vague_Date_Start,
		To_Vague_Date_End,
		To_Vague_Date_Type,
		OP_From_Vague_Date_Start,
		OP_From_Vague_Date_End,
		OP_From_Vague_Date_Type,
		OP_To_Vague_Date_Start,
		OP_To_Vague_Date_End,
		OP_To_Vague_Date_Type,
		SW_Spatial_Ref,
		SW_Lat,
		SW_Long,
		SW_Spatial_Ref_Qualifier,
		NE_Spatial_Ref,
		NE_Lat,
		NE_Long,
		NE_Spatial_Ref_Qualifier,
		Spatial_Ref_System,
		Geographic_Coverage,
		Periodicity,
		Entered_By,
		Temporary_Survey
	) VALUES (
		@Key,
		@ItemName,
		@Description,
		@RunByKey,
		@SurveyStatusKey,
		@SurveyMediaKey,
		@SurveyTypeKey,
		@LicenceKey,
		@FromVagueDateStart,
		@FromVagueDateEnd,
		@FromVagueDateType,
		@ToVagueDateStart,
		@ToVagueDateEnd,
		@ToVagueDateType,
		@OPFromVagueDateStart,
		@OPFromVagueDateEnd,
		@OPFromVagueDateType,
		@OPToVagueDateStart,
		@OPToVagueDateEnd,
		@OPToVagueDateType,
		@SWSpatialRef,
		@SWLat,
		@SWLong,
		@SWSpatialRefQualifier,
		@NESpatialRef,
		@NELat,
		@NELong,
		@NESpatialRefQualifier,
		@SpatialRefSystem,
		@GeographicCoverage,
		@Periodicity,
		@EnteredBy,0
	)

