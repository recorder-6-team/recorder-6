
-- Not needed.
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyTag_Select') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SurveyTag_Select]
GO

SET QUOTED_IDENTIFIER ON

/*===========================================================================*\
  Script for Export_Filter_Tag table.
\*===========================================================================*/
IF NOT EXISTS (SELECT * FROM dbo.SysObjects WHERE Id = Object_Id(N'[dbo].[Export_Filter_Tag]') AND ObjectProperty(Id, N'IsUserTable') = 1)
	CREATE TABLE [dbo].[Export_Filter_Tag] (
		Export_Filter_Key	CHAR(16) 	NOT NULL,
		Concept_Key			CHAR(16)	NOT NULL 
		CONSTRAINT	PK_Export_Filter_Tag PRIMARY KEY CLUSTERED 
		(
			Export_Filter_Key, 
			Concept_Key
		) ON [PRIMARY],
		CONSTRAINT	FK_Export_Filter_Tag_EXPORT_FILTER FOREIGN KEY
		(
			Export_Filter_Key
		) REFERENCES dbo.EXPORT_FILTER (
			EXPORT_FILTER_KEY
		),
		CONSTRAINT	FK_Export_Filter_Tag_Concept FOREIGN KEY
		(
			Concept_Key
		) REFERENCES dbo.Concept (
			Concept_Key
		)
	) ON [PRIMARY]
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT  SELECT  ON [dbo].[Export_Filter_Tag]  TO [R2k_ReadOnly]
GO
GRANT  SELECT  ON [dbo].[Export_Filter_Tag]  TO [R2k_RecordCardsOnly]
GO
GRANT  SELECT  ON [dbo].[Export_Filter_Tag]  TO [R2k_AddOnly]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Export_Filter_Tag]  TO [R2k_FullEdit]
GO
GRANT  SELECT, UPDATE, INSERT, DELETE  ON [dbo].[Export_Filter_Tag]  TO [R2k_Administrator]
GO

