
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Mantis 450/451: Insert IW Fields
\*============================================================================*/

-- Firstly we delete the keys if they are there

Delete FROM IW_Table_Rule_Related_Field WHERE IW_Table_Rule_Key IN('SYSTEM0100000002') AND IW_Column_Type_Key IN('LCA0002300000450','LCA0002300000451')
GO
Delete FROM IW_Table_Rule_Output_Field WHERE IW_Output_Field_Key IN('LCA0002300000450','LCA0002300000451')
GO
Delete FROM IW_Table_Rule WHERE IW_Table_Rule_Key IN('LCA0002300000450','LCA0002300000451')
Go
Delete FROM IW_Output_Field  WHERE IW_Output_Field_Key IN ('LCA0002300000450','LCA0002300000451')
Go
Delete FROM IW_Column_Type_Pattern  WHERE IW_Column_Type_Key IN('LCA0002300000450','LCA0002300000451')
GO
Delete FROM IW_Column_Type WHERE IW_Column_Type_Key IN('LCA0002300000450','LCA0002300000451')
GO

INSERT INTO IW_Column_Type (
		IW_Column_Type_Key,
		Class_Name,
		Item_Name,
		Required,
		Commonly_Used,
		Parser_Class_Name,
		Maximum_Length,
		Entered_By,
		Entry_Date,		
		System_Supplied_Data)
	VALUES (
		'LCA0002300000450',
		'TColumnType',
		'Private Location',
		0,
		0,
		'TTextParser',
		100,
		'NBNSYS0000000004',
		GetDate(),
		1)
GO

INSERT INTO IW_Column_Type (
		IW_Column_Type_Key,
		Class_Name,
		Item_Name,
		Required,
		Commonly_Used,
		Parser_Class_Name,
		Maximum_Length,
		Entered_By,
		Entry_Date,		
		System_Supplied_Data)
	VALUES (
		'LCA0002300000451',
		'TColumnType',
		'Private Code',
		0,
		0,
		'TTextParser',
		25,
		'NBNSYS0000000004',
		GetDate(),
		1)
GO


INSERT INTO IW_Column_Type_Pattern (
		IW_Column_Type_Key,
		Pattern,
		Exclude_Match,
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'LCA0002300000450',
		'Private Address',
		0,
		'NBNSYS0000000004',
		GetDate(),
		1)

INSERT INTO IW_Column_Type_Pattern (
		IW_Column_Type_Key,
		Pattern,
		Exclude_Match,
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'LCA0002300000450',
		'Address',
		0,
		'NBNSYS0000000004',
		GetDate(),
		1)

INSERT INTO IW_Column_Type_Pattern (
		IW_Column_Type_Key,
		Pattern,
		Exclude_Match,
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'LCA0002300000451',
		'Private Code',
		0,
		'NBNSYS0000000004',
		GetDate(),
		1)

GO


INSERT INTO IW_Column_Type_Pattern (
		IW_Column_Type_Key,
		Pattern,
		Exclude_Match,
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'LCA0002300000451',
		'Post Code',
		0,
		'NBNSYS0000000004',
		GetDate(),
		1)

GO

 

INSERT INTO IW_Output_Field (
		IW_Output_Field_Key,
		Name,
		Data_Type,
		IW_Column_Type_Key,
                Source_Field_Name,
                Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'LCA0002300000450',
		'Private_Location',
		'VARCHAR(100) ',
		'LCA0002300000450',
  		'data',
		
		'NBNSYS0000000004',
		GetDate(),
		1)
GO



INSERT INTO IW_Output_Field (
		IW_Output_Field_Key,
		Name,
		Data_Type,
		IW_Column_Type_Key,
                Source_Field_Name,
                Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES (
		'LCA0002300000451',
		'Private_Code',
		'VARCHAR(25) ',
         'LCA0002300000451',
  		'data',
		'NBNSYS0000000004',
		GetDate(),
		1)

GO 



INSERT INTO IW_Table_Rule_Output_Field (
		IW_Table_Rule_Key,
     	        IW_Output_Field_Key, 
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES ('SYSTEM0100000002',
		'LCA0002300000450',
		'NBNSYS0000000004',
		GetDate(),
		1)

GO


INSERT INTO IW_Table_Rule_Output_Field (
		IW_Table_Rule_Key,
     	        IW_Output_Field_Key, 
		Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES ('SYSTEM0100000002',
		'LCA0002300000451',
		'NBNSYS0000000004',
		GetDate(),
		1)



GO


INSERT INTO IW_Table_Rule_Related_Field (
		IW_Table_Rule_Key,
     	        IW_Column_Type_Key, 
                Relationship,		
                Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES ('SYSTEM0100000002',
		'LCA0002300000450',
                1,
		'NBNSYS0000000004',
		GetDate(),
		1)



GO
INSERT INTO IW_Table_Rule_Related_Field (
		IW_Table_Rule_Key,
     	        IW_Column_Type_Key, 
                Relationship,		
                Entered_By,
		Entry_Date,
		System_Supplied_Data)
	VALUES ('SYSTEM0100000002',
		'LCA0002300000451',
                1,
		'NBNSYS0000000004',
		GetDate(),
		1)


