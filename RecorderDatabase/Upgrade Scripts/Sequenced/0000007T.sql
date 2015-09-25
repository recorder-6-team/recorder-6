SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	VI 21380 - ensure that all spatial references are delocalized.
\*============================================================================*/
UPDATE		GRID_SQUARE
SET			SPATIAL_REF						=	CASE WHEN CHARINDEX(';', SPATIAL_REF) > 0
													THEN REPLACE(
															REPLACE(SPATIAL_REF, ',', '.'),
															';', ',')
													ELSE SPATIAL_REF
												END
WHERE		CHARINDEX(';', SPATIAL_REF)		>	0
GO

UPDATE		LOCATION
SET			SPATIAL_REF						=	CASE WHEN CHARINDEX(';', SPATIAL_REF) > 0
													THEN REPLACE(
															REPLACE(SPATIAL_REF, ',', '.'),
															';', ',')
													ELSE SPATIAL_REF
												END
WHERE		CHARINDEX(';', SPATIAL_REF)		>	0
GO

UPDATE		MAP_SHEET
SET			NE_SPATIAL_REF					=	CASE WHEN CHARINDEX(';', NE_SPATIAL_REF) > 0
													THEN REPLACE(
															REPLACE(NE_SPATIAL_REF, ',', '.'),
															';', ',')
													ELSE NE_SPATIAL_REF
												END,
			SW_SPATIAL_REF					=	CASE WHEN CHARINDEX(';', SW_SPATIAL_REF) > 0
													THEN REPLACE(
															REPLACE(SW_SPATIAL_REF, ',', '.'),
															';', ',')
													ELSE SW_SPATIAL_REF
												END
WHERE		CHARINDEX(';', NE_SPATIAL_REF)	>	0
OR			CHARINDEX(';', SW_SPATIAL_REF)	>	0
GO

UPDATE		SAMPLE
SET			SPATIAL_REF						=	CASE WHEN CHARINDEX(';', SPATIAL_REF) > 0
													THEN REPLACE(
															REPLACE(SPATIAL_REF, ',', '.'),
															';', ',')
													ELSE SPATIAL_REF
												END
WHERE		CHARINDEX(';', SPATIAL_REF)		>	0
GO

UPDATE		SURVEY
SET			NE_SPATIAL_REF					=	CASE WHEN CHARINDEX(';', NE_SPATIAL_REF) > 0
													THEN REPLACE(
															REPLACE(NE_SPATIAL_REF, ',', '.'),
															';', ',')
													ELSE NE_SPATIAL_REF
												END,
			SW_SPATIAL_REF					=	CASE WHEN CHARINDEX(';', SW_SPATIAL_REF) > 0
													THEN REPLACE(
															REPLACE(SW_SPATIAL_REF, ',', '.'),
															';', ',')
													ELSE SW_SPATIAL_REF
												END
WHERE		CHARINDEX(';', NE_SPATIAL_REF)	>	0
OR			CHARINDEX(';', SW_SPATIAL_REF)	>	0
GO

UPDATE		SURVEY_EVENT
SET			SPATIAL_REF						=	CASE WHEN CHARINDEX(';', SPATIAL_REF) > 0
													THEN REPLACE(
															REPLACE(SPATIAL_REF, ',', '.'),
															';', ',')
													ELSE SPATIAL_REF
												END
WHERE		CHARINDEX(';', SPATIAL_REF)		>	0
GO
