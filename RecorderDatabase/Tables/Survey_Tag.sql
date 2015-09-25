SET QUOTED_IDENTIFIER ON

/*===========================================================================*\
  Script for Survey_Tag table.
\*===========================================================================*/
IF NOT EXISTS (SELECT * FROM dbo.SysObjects WHERE Id = Object_Id(N'[dbo].[Survey_Tag]') AND ObjectProperty(Id, N'IsUserTable') = 1)
	CREATE TABLE dbo.Survey_Tag (
		Survey_Tag_Key 			CHAR(16) 	NOT NULL,
		Survey_Key 				CHAR(16) 	NOT NULL,
		Concept_Key 			CHAR(16) 	NOT NULL,
		Entered_Session_Id 		CHAR(16)	NOT NULL,
		Changed_Session_Id 		CHAR(16)	NULL,
		Custodian 				CHAR(8) 	NULL,
		System_Supplied_Data 	BIT 		NOT NULL	DEFAULT 0
		CONSTRAINT	PK_Survey_Tag PRIMARY KEY CLUSTERED 
		(
			Survey_Tag_Key
		) ON [PRIMARY],
		CONSTRAINT	FK_Survey_Tag_SURVEY FOREIGN KEY
		(
			Survey_Key
		) REFERENCES dbo.SURVEY (
			SURVEY_KEY
		),
		CONSTRAINT	FK_Survey_Tag_Concept FOREIGN KEY
		(
			Concept_Key
		) REFERENCES dbo.Concept (
			Concept_Key
		)
	)
GO

/*===========================================================================*\
  Deal with Custodian trigger.
\*===========================================================================*/
IF EXISTS(SELECT 1 FROM SysObjects WHERE [Name]='Survey_TagCustodianInsert')
  DROP TRIGGER Survey_TagCustodianInsert
GO

CREATE TRIGGER Survey_TagCustodianInsert ON dbo.Survey_Tag 
AFTER INSERT AS 
	UPDATE 	Survey_Tag 
	SET 	Survey_Tag.Custodian = SubString(Survey_Tag.Survey_Tag_Key, 1, 8) 
	FROM 	Survey_Tag 
	JOIN 	Inserted ON Survey_Tag.Survey_Tag_Key = Inserted.Survey_Tag_Key 
	WHERE 	Survey_Tag.Custodian IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT  SELECT  ON [dbo].[Survey_Tag]  TO [R2k_ReadOnly]
GO
GRANT  SELECT  ON [dbo].[Survey_Tag]  TO [R2k_RecordCardsOnly]
GO
GRANT  SELECT  ON [dbo].[Survey_Tag]  TO [R2k_AddOnly]
GO
GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Survey_Tag]  TO [R2k_FullEdit]
GO
GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Survey_Tag]  TO [R2k_Administrator]
GO

