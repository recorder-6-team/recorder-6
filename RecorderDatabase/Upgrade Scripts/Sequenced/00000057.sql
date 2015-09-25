
/*===========================================================================*\
  Add missing Custodian triggers.
\*===========================================================================*/
IF EXISTS(SELECT 1 FROM SysObjects WHERE [Name]='LOCATION_FEATURE_SOURCESCustodianInsert')
  DROP TRIGGER LOCATION_FEATURE_SOURCESCustodianInsert
GO

CREATE TRIGGER LOCATION_FEATURE_SOURCESCustodianInsert ON dbo.LOCATION_FEATURE_SOURCES 
AFTER INSERT AS 
	UPDATE 	LOCATION_FEATURE_SOURCES 
	SET 	LOCATION_FEATURE_SOURCES.Custodian = SubString(LOCATION_FEATURE_SOURCES.Source_Link_Key, 1, 8) 
	FROM 	LOCATION_FEATURE_SOURCES 
	JOIN 	Inserted ON LOCATION_FEATURE_SOURCES.Source_Link_Key = Inserted.Source_Link_Key 
	WHERE 	LOCATION_FEATURE_SOURCES.Custodian IS NULL
GO

IF EXISTS(SELECT 1 FROM SysObjects WHERE [Name]='Survey_Event_SourcesCustodianInsert')
  DROP TRIGGER Survey_Event_SourcesCustodianInsert
GO

CREATE TRIGGER Survey_Event_SourcesCustodianInsert ON dbo.Survey_Event_Sources 
AFTER INSERT AS 
	UPDATE 	Survey_Event_Sources 
	SET 	Survey_Event_Sources.Custodian = SubString(Survey_Event_Sources.Source_Link_Key, 1, 8) 
	FROM 	Survey_Event_Sources 
	JOIN 	Inserted ON Survey_Event_Sources.Source_Link_Key = Inserted.Source_Link_Key 
	WHERE 	Survey_Event_Sources.Custodian IS NULL
GO
