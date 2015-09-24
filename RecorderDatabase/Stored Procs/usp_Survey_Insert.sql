IF Object_ID('dbo.usp_Survey_Insert') IS NOT NULL
	DROP PROCEDURE dbo.usp_Survey_Insert
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
    $Date: 16/07/09 11:47 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Survey_Insert
	@Key					CHAR(16)	OUTPUT,
	@ItemName				VARCHAR(100),
	@Description			TEXT		= NULL,
	@RunByKey				CHAR(16),
	@SurveyStatusKey		CHAR(16)	= NULL,
	@SurveyMediaKey			CHAR(16)	= NULL,
	@SurveyTypeKey			CHAR(16),
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
		Entered_By
	) VALUES (
		@Key,
		@ItemName,
		@Description,
		@RunByKey,
		@SurveyStatusKey,
		@SurveyMediaKey,
		@SurveyTypeKey,
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
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Survey_Insert TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Survey_Insert TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Survey_Insert TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Survey_Insert TO "Dev - JNCC SQL"
GO