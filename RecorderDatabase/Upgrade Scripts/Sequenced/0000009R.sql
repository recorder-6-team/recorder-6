SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Mantis 495: Report Wizrad to Convert Lat/Long to OSGB Eastings/Northings 
\*============================================================================*/

Delete FROM Report_Field  WHERE Report_Field_Key = 'LCA0002300000453'
Go
Delete FROM Report_Attribute WHERE Report_Attribute_Key IN('LCA0002300000453')
Go


INSERT INTO Report_Attribute (
		Report_Attribute_Key,
		Item_Group,
		Source_Table,
		Item_Name,
		Attribute_SQL,
		Report_Join_Key,
		Entered_By,
		Entry_Date,		
		System_Supplied_Data)
	VALUES (
		'LCA0002300000453',
		'Sample',
		'SAMPLE',
		'Sample Eastings and Northings',
		'#REPORT_OUTPUT.[Sample Eastings and Northings] = [dbo].[LCOSGBLLToOSGBEN](SAMPLE.[Lat],SAMPLE.[Long],'','',3)',
		'NBNSYS0000000007',
		'NBNSYS0000000004',
		GetDate(),
		1)
GO


INSERT INTO Report_Field(
		Report_Field_Key,
		Report_Attribute_Key,
		Field_Item_Name,
		Field_Type,
		Field_Size,
		Entered_By,
		Entry_Date,		
		System_Supplied_Data)
	VALUES (
		'LCA0002300000453',
		'LCA0002300000453',
		'Sample Eastings and Northings',
		'varchar',
		 20,
		'NBNSYS0000000004',
		GetDate(),
		1)
GO