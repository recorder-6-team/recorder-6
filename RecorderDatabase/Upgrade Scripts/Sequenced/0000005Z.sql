/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_BiotopeOccurrences_Select_ForSample') IS NOT NULL
	DROP PROCEDURE dbo.usp_BiotopeOccurrences_Select_ForSample
GO

/*===========================================================================*\
  Description:
	Returns the biotope occurrences (with preferred determinations) for the given sample

  Parameters:
	@SampleKey  The Sample Key  

  Created:	December 2008

  Last revision information:
    $Revision: 1 $
    $Date: 15/01/09 8:56 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_BiotopeOccurrences_Select_ForSample 	
	@SampleKey	CHAR(16)
AS
	SELECT	DISTINCT
			BO.Biotope_Occurrence_Key,
			CASE 
				WHEN B.Original_Code IS NULL THEN B.Short_Term 
				ELSE B.Original_Code + ', ' + B.Short_Term 
			END AS Item_Name,
			BD.Biotope_List_Item_Key,
			BO.Checked
	FROM 	Biotope_Occurrence 		BO 
	JOIN	Biotope_Determination	BD	ON	BD.Biotope_Occurrence_Key 	= BO.Biotope_Occurrence_Key
	JOIN	Biotope_List_Item 		BLI ON	BLI.Biotope_List_Item_Key 	= BD.Biotope_List_Item_Key
	JOIN	Biotope 				B 	ON	B.Biotope_Key 				= BLI.Biotope_Key
	WHERE 	BO.Sample_Key 	= @SampleKey
	AND 	BD.Preferred 	= 1
	ORDER BY Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_BiotopeOccurrences_Select_ForSample'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_BiotopeOccurrences_Select_ForSample TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_BiotopeOccurrences_Select_ForSample TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_BiotopeOccurrences_Select_ForSample TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_BiotopeOccurrences_Select_ForSample TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_BiotopeOccurrences_Select_ForSample TO R2k_RecordCardsOnly
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_MeasurementColumns_Get') IS NOT NULL
	DROP PROCEDURE dbo.usp_MeasurementColumns_Get
GO

/*===========================================================================*\
  Description:
	Returns measurement column names for all possible combinations.

  Parameters:
	@TranslatedOf	The localized version of "of" used for the displayed string.

  Created:	December 2008

  Last revision information:
    $Revision: 1 $
    $Date: 15/01/09 8:56 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_MeasurementColumns_Get 	
	@TranslatedOf VARCHAR(10)
AS
	SELECT 	MQ.Measurement_Qualifier_Key, 
			MU.Measurement_Unit_Key, 
			MU.Data_Type,
		    MT.Short_Name + ' ' + @TranslatedOf + ' ' + MQ.Short_Name + ' (' + MU.Short_Name + ')' AS ColumnName,
			MT.Short_Name + ' of ' + MQ.Short_Name + ' (' + MU.Short_Name + ')' AS OriginalColumnName
	FROM 	Measurement_Context 		MC
	JOIN 	Measurement_Type_Context 	MTC	ON MTC.Measurement_Context_Key 	= MC.Measurement_Context_Key
	JOIN 	Measurement_Type 			MT	ON MT.Measurement_Type_Key 		= MTC.Measurement_Type_Key
	JOIN 	Measurement_Qualifier 		MQ	ON MQ.Measurement_Type_Key 		= MT.Measurement_Type_Key
	JOIN 	Measurement_Unit 			MU	ON MU.Measurement_Type_Key 		= MT.Measurement_Type_Key 
	WHERE 	MC.Data_Table 					= 'Taxon_Occurrence_Data'
	ORDER BY ColumnName
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_MeasurementColumns_Get'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_MeasurementColumns_Get TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_MeasurementColumns_Get TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_MeasurementColumns_Get TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_MeasurementColumns_Get TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_MeasurementColumns_Get TO R2k_RecordCardsOnly
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_SampleDetails_Get') IS NOT NULL
	DROP PROCEDURE dbo.usp_SampleDetails_Get
GO

/*===========================================================================*\
  Description:
	This procedure returns the whole row from SAMPLE

  Parameters:
	@SampleKey  Parameter holding the Sample Key  

  Created:	February 2008

  Last revision information:
    $Revision: 1 $
    $Date: 15/01/09 8:56 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_SampleDetails_Get 	
	@SampleKey CHAR(16)
AS
	SELECT		S.Sample_Key, 
				S.Location_Key,
				LN.Item_Name 	AS Location, 
				S.Location_Name, 
				S.Vague_Date_Start, 
				S.Vague_Date_End, 
				S.Vague_Date_Type, 
				S.Spatial_Ref, 
				S.Spatial_Ref_System,
				S.Spatial_Ref_Qualifier,
				S.Lat,
				S.Long,
				ST.Short_Name 	AS Sample_Type, 
				ST.Sample_Type_Key,
				S.Sample_Reference,
				S.Comment,
				S.Custodian
	FROM		Sample 			S
	LEFT JOIN 	Location_Name	LN	ON 	LN.Location_Key 	= S.Location_Key
									AND	LN.Preferred		= 1
	INNER JOIN 	Sample_Type 	ST	ON	ST.Sample_Type_Key 	= S.Sample_Type_Key	
	WHERE 		S.Sample_Key 		= 	@SampleKey
	ORDER BY 	Sample_Reference
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_SampleDetails_Get'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_SampleDetails_Get TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SampleDetails_Get TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SampleDetails_Get TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SampleDetails_Get TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SampleDetails_Get TO R2k_RecordCardsOnly
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_SampleRecoders_Select_ForSample') IS NOT NULL
	DROP PROCEDURE dbo.usp_SampleRecoders_Select_ForSample
GO

/*===========================================================================*\
  Description:
	Returns the recorders for the given sample

  Parameters:
	@SampleKey  The Sample Key  

  Created:	December 2008

  Last revision information:
    $Revision: 1 $
    $Date: 15/01/09 8:56 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_SampleRecoders_Select_ForSample 	
	@SampleKey CHAR(16)
AS
	SELECT		SER.SE_Recorder_Key 					AS "Key",
				SER.Name_Key,
				dbo.ufn_GetFormattedName(SER.Name_Key) 	AS "Name"
	FROM		Sample_Recorder			SR
	JOIN 		Survey_Event_Recorder	SER	ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
	WHERE 		SR.Sample_Key 			= 	@SampleKey
	ORDER BY	"Name"
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_SampleRecoders_Select_ForSample'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_SampleRecoders_Select_ForSample TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SampleRecoders_Select_ForSample TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SampleRecoders_Select_ForSample TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SampleRecoders_Select_ForSample TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SampleRecoders_Select_ForSample TO R2k_RecordCardsOnly
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_Samples_Select_ForEvent') IS NOT NULL
	DROP PROCEDURE dbo.usp_Samples_Select_ForEvent
GO

/*===========================================================================*\
  Description:
	This procedure returns samples for a given event.

  Parameters:
	@EventKey  	Event containing the samples.
	@SortOrder	How to order the results.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 15/01/09 8:56 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Samples_Select_ForEvent
	@EventKey	CHAR(16),
	@SortOrder	TINYINT
AS
	SET NOCOUNT ON

	DECLARE @Samples TABLE (
		Sample_Key			CHAR(16),
		Sample_Reference	VARCHAR(15),
		Vague_Date_Start	INT,
		Vague_Date_End		INT,
		Vague_Date_Type		VARCHAR(2),
		Outstanding_Card	TINYINT,
		Short_Name			VARCHAR(20),
		Sample_Type_Key		CHAR(16),
		Location_Key		CHAR(16),
		Location_Name		VARCHAR(100)
	)

	INSERT INTO @Samples
	SELECT 		S.Sample_Key, 
				S.Sample_Reference, 
				S.Vague_Date_Start, 
				S.Vague_Date_End,
				S.Vague_Date_Type, 
				S.Outstanding_Card, 
				T.Short_Name, 
				T.Sample_Type_Key, 
				S.Location_Key,
				CASE 
					WHEN LN.Item_Name IS NULL THEN 
						CASE 
							WHEN S.Spatial_Ref IS NULL OR S.Spatial_Ref = '' THEN S.Location_Name 
							ELSE S.Spatial_Ref 
						END 
					ELSE LN.Item_Name 
				END AS Location_Name
	FROM 		Sample 			S 
	INNER JOIN 	Sample_Type 	T 	ON 	S.Sample_Type_Key 	= T.Sample_Type_Key
	LEFT JOIN 	Location_Name 	LN 	ON 	LN.Location_Key 	= S.Location_Key
	WHERE 		S.Survey_Event_Key = @EventKey 
	AND 		(LN.Preferred = 1 OR LN.Preferred IS NULL)

	IF @SortOrder = 1 
		SELECT * FROM @Samples ORDER BY Sample_Reference
	ELSE
	IF @SortOrder = 2
		SELECT * FROM @Samples ORDER BY Vague_Date_Start
	ELSE
		SELECT * FROM @Samples ORDER BY Short_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_Samples_Select_ForEvent TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Samples_Select_ForEvent TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Samples_Select_ForEvent TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Samples_Select_ForEvent TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Samples_Select_ForEvent TO R2k_RecordCardsOnly
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_TaxonOccurrenceData_Delete') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonOccurrenceData_Delete
GO

/*===========================================================================*\
  Description:
	Delete a record from Taxon_Occurrence_Data.

  Parameters:
	@Key 	Data record key

  Created:
	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 15/01/09 8:56 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonOccurrenceData_Delete
	@Key CHAR(16)
AS
	SET NOCOUNT OFF
	
	DELETE FROM	Taxon_Occurrence_Data
	WHERE	Taxon_Occurrence_Data_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_TaxonOccurrenceData_Delete'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Delete TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Delete TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Delete TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Delete TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Delete TO "Dev - JNCC SQL"
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_TaxonOccurrenceData_Insert') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonOccurrenceData_Insert
GO

/*===========================================================================*\
  Description:
	Insert a record in Taxon_Occurrence_Data.

  Parameters:
	@TaxonOccurrenceKey 
	@QualifierKey
	@Accuracy
	@UnitKey
	@Data
	@EnteredBy
	@Key

  Created:
	November 2004

  Last revision information:
    $Revision: 1 $
    $Date: 15/01/09 8:56 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonOccurrenceData_Insert
	@TaxonOccurrenceKey 	CHAR(16),
	@QualifierKey 			CHAR(16),
	@Accuracy				VARCHAR(10),
	@UnitKey				CHAR(16),
	@Data					VARCHAR(20),
	@EnteredBy				CHAR(16),
	@Key					CHAR(16) OUTPUT
AS
	SET NOCOUNT OFF
	
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Taxon_Occurrence_Data', @Key OUTPUT
	
	INSERT INTO Taxon_Occurrence_Data(
		Taxon_Occurrence_Data_Key, Taxon_Occurrence_Key, Data, Accuracy,
		Measurement_Qualifier_Key, Measurement_Unit_Key, Entered_By
	) VALUES (
		@Key, @TaxonOccurrenceKey, @Data, @Accuracy,
		@QualifierKey, @UnitKey, @EnteredBy
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_TaxonOccurrenceData_Insert'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO "Dev - JNCC SQL"
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF OBJECT_ID('dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence
GO

/*===========================================================================*\
  Description:	
	Returns all measuerement data for a taxon occurrence.

  Parameters:
	@OccurrenceKey

  Created:	December 2008

  Last revision information:
    $Revision: 1 $
    $Date: 15/01/09 8:56 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence
	@OccurrenceKey	CHAR(16)
AS
	SET NOCOUNT ON

	SELECT	DT.Taxon_Occurrence_Data_Key 	AS Data_Key,
			DT.Measurement_Qualifier_Key, 
			MQ.Short_Name 					AS Qualifier_Short_Name,
			DT.Measurement_Unit_Key,  
			MU.Short_Name 					AS Unit_Short_Name,
			MT.Measurement_Type_Key, 
			MT.Short_Name 					AS Type_Short_Name, 
			DT.Data,
			DT.Accuracy,
			DT.Custodian
	FROM 	Measurement_Type 		MT
	JOIN 	Measurement_Qualifier 	MQ	ON 	MT.Measurement_Type_Key = MQ.Measurement_Type_Key
	JOIN 	Measurement_Unit 		MU	ON 	MT.Measurement_Type_Key = MU.Measurement_Type_Key
	JOIN 	Taxon_Occurrence_Data	DT	ON 	MU.Measurement_Unit_Key = DT.Measurement_Unit_Key
										AND MQ.Measurement_Qualifier_Key = DT.Measurement_Qualifier_Key
	WHERE	DT.Taxon_Occurrence_Key = @OccurrenceKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Select_ForTaxonOccurrence TO R2k_RecordCardsOnly
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_TaxonOccurrenceData_Update') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonOccurrenceData_Update
GO

/*===========================================================================*\
  Description:
	Update a record in Taxon_Occurrence_Data.

  Parameters:
	@Key 			Data record key
	@QualifierKey 	Measurement Qualifier key
	@Accuracy		Accuracy
	@UnitKey		Measurement unit key
	@Data			Actual data of the measurement
	@ChangedBy		Who made the change

  Created:
	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 15/01/09 8:56 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonOccurrenceData_Update
	@Key 			CHAR(16),
	@Data			VARCHAR(20),
	@Accuracy		VARCHAR(10),
	@QualifierKey 	CHAR(16),
	@UnitKey		CHAR(16),
	@ChangedBy		CHAR(16)
AS
	SET NOCOUNT OFF
	
	UPDATE 	Taxon_Occurrence_Data
	SET		Data						= @Data, 
			Accuracy					= @Accuracy,
			Measurement_Qualifier_Key	= @QualifierKey, 
			Measurement_Unit_Key		= @UnitKey, 
			Changed_By					= @ChangedBy,
			Changed_Date 				= GetDate()
	WHERE	Taxon_Occurrence_Data_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_TaxonOccurrenceData_Update'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Update TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Update TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Update TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Update TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Update TO "Dev - JNCC SQL"
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF Object_ID('dbo.usp_TaxonOccurrences_Select_ForSample') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonOccurrences_Select_ForSample
GO

/*===========================================================================*\
  Description:
	Returns the taxon occurrences (with preferred determinations) for the given sample

  Parameters:
	@SampleKey  The Sample Key  

  Created:	December 2008

  Last revision information:
    $Revision: 1 $
    $Date: 15/01/09 8:56 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_TaxonOccurrences_Select_ForSample 	
	@SampleKey	CHAR(16)
AS
	SELECT 	TXO.Sample_Key, 
			TXO.Taxon_Occurrence_Key,
			TXO.Checked,
			TXO.Confidential,
			TXO.Provenance,
			TXO.Substrate_Key,
			TXO.Record_Type_Key,
			TXO.Comment,
			TXO.Custodian,
			ITN.Taxon_List_Item_Key,
			ITN.Preferred_Name,
			ITN.Preferred_Name_Italic,
			ITN.Common_Name,
			ITN.Common_Name_Italic,
			TD.Taxon_Determination_Key,
			TD.Determiner,
			TD.Vague_Date_Start,
			TD.Vague_Date_End,
			TD.Vague_Date_Type
	FROM 	Taxon_Occurrence 	TXO
	JOIN 	Taxon_Determination TD 	ON 	TD.Taxon_Occurrence_Key = TXO.Taxon_Occurrence_Key
	JOIN 	Index_Taxon_Name 	ITN ON 	ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	WHERE 	TXO.Sample_Key 				= @SampleKey
	AND   	TD.Preferred 				= 1
	AND 	ITN.System_Supplied_Data	= 1
	ORDER BY ITN.Preferred_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure dbo.usp_TaxonOccurrences_Select_ForSample'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSample TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSample TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSample TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSample TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSample TO R2k_RecordCardsOnly
GO
