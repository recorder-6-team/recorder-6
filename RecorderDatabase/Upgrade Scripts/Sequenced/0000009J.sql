SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Mantis 450/451: Insert Report Wizard Fields and Filters
\*============================================================================*/

-- Firstly we delete the keys if they are there
 Delete FROM Report_Field  WHERE Report_Field_Key IN('LCA0002300000450','LCA0002300000451','LCA0002300000452' )
Go

Delete FROM Report_Attribute WHERE Report_Attribute_Key IN('LCA0002300000450','LCA0002300000451','LCA0002300000452')
Go

Delete FROM Usable_Field WHERE Usable_Field_Key IN('LCA0002300000450','LCA0002300000451')
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
		'LCA0002300000450',
		'Sample',
		'SAMPLE',
		'Private Location',
		'#REPORT_OUTPUT.[Private Location] = SAMPLE.Private_Location',
		'NBNSYS0000000007',
		'NBNSYS0000000004',
		GetDate(),
		1)
GO


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
		'LCA0002300000451',
		'Sample',
		'SAMPLE',
		'Private Code',
		'#REPORT_OUTPUT.[Private Code] = SAMPLE.Private_Code',
		'NBNSYS0000000007',
		'NBNSYS0000000004',
		GetDate(),
		1)
GO



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
		'LCA0002300000452',
		'Metadata',
		'SAMPLE',
		'Sample Location Key',
		'#REPORT_OUTPUT.[Sample Location Key] = SAMPLE.Location_Key',
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
		'LCA0002300000450',
		'LCA0002300000450',
		'Private Location',
		'varchar',
		 100,
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
		'LCA0002300000451',
		'LCA0002300000451',
		'Private Code',
		'varchar',
		 25,
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
		'LCA0002300000452',
		'LCA0002300000452',
		'Sample Location Key',
		'varchar',
		 16,
		'NBNSYS0000000004',
		GetDate(),
		1)

GO

INSERT INTO Usable_Field( 
                 Usable_Field_Key,
                 Table_name,
                 Field_Name,
		 Field_Description,
                 Field_Type,
                 Apply_To,
          	 Selectable,
                 Sortable,
                 Filterable,
                 Calculation_SQL)
           VALUES (
                'LCA0002300000450',
                'SAMPLE',
				'Private_Location',
                'Private Location',
                'TEXT',
                'A',
                1,
                1,
                1,
                'SAMPLE.PRIVATE_LOCATION')

                
INSERT INTO Usable_Field( 
                 Usable_Field_Key,
                 Table_name,
                 Field_Name,
		 Field_Description,
                 Field_Type,
                 Apply_To,
          	 Selectable,
                 Sortable,
                 Filterable,
                 Calculation_SQL)
           VALUES (
                'LCA0002300000451',
                'SAMPLE',
                'Private_Code',
				'Private Code',
                'TEXT',
                'A',
                1,
                1,
                1,
                'SAMPLE.PRIVATE_CODE')										
	




