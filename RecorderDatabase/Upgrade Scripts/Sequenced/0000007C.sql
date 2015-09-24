/*===========================================================================*\
  Add missing defaults and triggers to Taxon_Designation_Set.
\*===========================================================================*/
ALTER TABLE dbo.Taxon_Designation_Set 
	ALTER COLUMN Custodian CHAR(16) NULL
GO

IF OBJECT_ID('DF_Taxon_Designation_Set_Entry_Date') IS NULL
	ALTER TABLE dbo.Taxon_Designation_Set 
		ADD CONSTRAINT DF_Taxon_Designation_Set_Entry_Date DEFAULT GETDATE() FOR Entry_Date

IF OBJECT_ID('DF_Taxon_Designation_Set_System_Supplied_Data') IS NULL
	ALTER TABLE dbo.Taxon_Designation_Set 
		ADD CONSTRAINT DF_Taxon_Designation_Set_System_Supplied_Data DEFAULT 0 FOR System_Supplied_Data

IF OBJECT_ID('Taxon_Designation_SetCustodianInsert') IS NOT NULL
	DROP TRIGGER Taxon_Designation_SetCustodianInsert

EXECUTE spCreateCustodianTrigger 'Taxon_Designation_Set', 'Taxon_Designation_Set_Key'


/*===========================================================================*\
  Add missing defaults and triggers to Taxon_Designation_Set_Item.
\*===========================================================================*/
ALTER TABLE dbo.Taxon_Designation_Set_Item 
	ALTER COLUMN Custodian CHAR(16) NULL
GO

IF OBJECT_ID('DF_Taxon_Designation_Set_Item_Entry_Date') IS NULL
	ALTER TABLE dbo.Taxon_Designation_Set_Item
		ADD CONSTRAINT DF_Taxon_Designation_Set_Item_Entry_Date DEFAULT GETDATE() FOR Entry_Date

IF OBJECT_ID('DF_Taxon_Designation_Set_Item_System_Supplied_Data') IS NULL
	ALTER TABLE dbo.Taxon_Designation_Set_Item
		ADD CONSTRAINT DF_Taxon_Designation_Set_Item_System_Supplied_Data DEFAULT 0 FOR System_Supplied_Data

IF OBJECT_ID('Taxon_Designation_Set_ItemCustodianInsert') IS NOT NULL
	DROP TRIGGER Taxon_Designation_Set_ItemCustodianInsert

EXECUTE spCreateCustodianTrigger 'Taxon_Designation_Set_Item', 'Taxon_Designation_Set_Item_Key'

