SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	VI 20472: also highlight spatial reference system field in the import
		wizard when the spatial reference fails to validate.
\*============================================================================*/
UPDATE		dbo.IW_Column_Type
SET			Parser_Class_Name			=	'TSpatialRefSystemParser'
WHERE		IW_Column_Type_Key			=	'SYSTEM010000000U'
GO

-- make spatial reference system a related field of spatial reference
INSERT INTO	dbo.IW_Column_Type_Relationship (
			IW_Column_Type_Key,
			Related_IW_Column_Type_Key,
			Relationship_Type,
			Entered_By,
			Entry_Date,
			System_Supplied_Data)
VALUES		('SYSTEM0100000001',
			'SYSTEM010000000U',
			4,						-- "required by" relationship type
			'SYSTEM0000000000',
			GETUTCDATE(),
			1)
GO