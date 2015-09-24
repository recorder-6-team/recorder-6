SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
CCN375 (VI 21087) -- "Abundance Accuracy" column type in the Import Wizard.
\*============================================================================*/
DECLARE		@abundance_data_key			CHAR(16),
			@abundance_accuracy_key		CHAR(16),
			@output_field_key			CHAR(16)

SELECT		@abundance_data_key			=	'SYSTEM010000000S',
			@abundance_accuracy_key		=	'SYSTEM010000000X',
			@output_field_key			=	'SYSTEM010000001I'

IF NOT EXISTS (
	SELECT		1
	FROM		dbo.IW_Column_Type
	WHERE		IW_Column_Type_Key			=	@abundance_accuracy_key)
BEGIN
	INSERT INTO	dbo.IW_Column_Type (
				IW_Column_Type_Key,
				Class_Name,
				Item_Name,
				Required,
				Commonly_Used,
				Parser_Class_Name,
				Maximum_Length,
				Term_List_Table,
				Entered_By,
				System_Supplied_Data)
	VALUES		(@abundance_accuracy_key,
				'TColumnType',
				'Abundance Accuracy',
				0,
				0,
				'TTextParser',
				20,
				NULL,
				'NBNSYS0000000004',
				1)

	INSERT INTO	dbo.IW_Column_Type_Relationship (
				IW_Column_Type_Key,
				Related_IW_Column_Type_Key,
				Relationship_Type,
				Entered_By,
				System_Supplied_Data)
	VALUES		(@abundance_accuracy_key,
				@abundance_data_key,
				0,						-- "Abundance Accuracy" REQUIRES "Abundance Data"
				'NBNSYS0000000004',
				1)

	INSERT INTO	dbo.IW_Column_Type_Pattern (
				IW_Column_Type_Key,
				Pattern,
				Exclude_Match,
				Entered_By,
				System_Supplied_Data)
	VALUES		(@abundance_data_key,
				'abund%accuracy%',
				1,
				'NBNSYS0000000004',
				1)

	INSERT INTO	dbo.IW_Column_Type_Pattern (
				IW_Column_Type_Key,
				Pattern,
				Exclude_Match,
				Entered_By,
				System_Supplied_Data)
	VALUES		(@abundance_accuracy_key,
				'abund%accuracy%',
				0,
				'NBNSYS0000000004',
				1)

	UPDATE		dbo.IW_Output_Field
	SET			IW_Column_Type_Key		=	NULL,
				Source_Field_Name		=	NULL,
				Generating_Class_Name	=	'TAbundanceAccuracyFieldGenerator',
				Generator_Field_Index	=	0
	WHERE		IW_Output_Field_Key		=	@output_field_key
END
GO
