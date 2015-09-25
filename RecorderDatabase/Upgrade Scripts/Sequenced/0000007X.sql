SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
VI 22173 -- "Determination Type" and "Determiner Role" are no longer required
			fields when selected in the import wizard.
\*============================================================================*/
UPDATE		IW_Column_Type
SET			Parser_Class_Name			=	'TTextParser'
WHERE		IW_Column_Type_Key			IN	('SYSTEM010000000V',
											 'SYSTEM010000000W')
GO