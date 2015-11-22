
GO
/****** Object:  StoredProcedure [dbo].[usp_Admin_Areas_Select_AllViceCounties]    Script Date: 11/20/2015 22:06:19 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO

/*===========================================================================*\
  Description:
	Returns all Admin Areas of type Vice-County. The results are formatted to be
	easily added as key/value pairs without any additional processing in the 
	calling application.

  Parameters:
	<none>

  Created:
	November 2015

  
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Admin_Areas_Select_AllViceCounties]
AS
	SET NOCOUNT OFF
	
	SELECT	Item_Name + '=' + Admin_Area_Key AS Data
	FROM	ADMIN_AREA
	WHERE	Admin_Type_Key In ('NBNSYS0000000032','NBNSYS0000000036')
	AND		Item_Name IS NOT NULL

    UNION 
    
    SELECT	Short_Code + '=' + Admin_Area_Key AS Data
	FROM	ADMIN_AREA
	WHERE	Admin_Type_Key In ('NBNSYS0000000032','NBNSYS0000000036')
	AND		Short_Code IS NOT NULL
GO

GRANT EXECUTE ON [dbo].[usp_Admin_Areas_Select_AllViceCounties] TO PUBLIC

GO	


Delete From IW_Table_Rule_Related_Table WHERE
IW_Table_Rule_Related_Table.IW_Table_Rule_Key = 'SYSTEM0100000010'

Delete From IW_Table_Rule_Related_Field WHERE
IW_Table_Rule_Related_Field.IW_Column_Type_Key = 'SYSTEM0100000010'


Delete From IW_Table_Rule_Output_Field WHERE
IW_Table_Rule_Output_Field.IW_Table_Rule_Key  = 'SYSTEM0100000010'


Delete From IW_Output_Field WHERE
IW_Output_Field.IW_Output_Field_Key  IN  ('SYSTEM0100000100','SYSTEM0100000101')

Delete From IW_Table_Rule WHERE
IW_Table_Rule.IW_Table_Rule_Key = 'SYSTEM0100000010'


Delete From IW_Column_Type WHERE
IW_Column_Type.IW_Column_Type_Key = 'SYSTEM0100000010'

GO


Insert Into IW_Column_Type (IW_Column_Type_Key,Class_Name,Item_Name,
Required,Commonly_Used,Parser_Class_Name,
Term_List_Table,Entered_By,Entry_Date,System_Supplied_Data)
Values('SYSTEM0100000010','TColumnType','Sample Vice county',0,0,'TSampleVCParser',null,
'TESTDATA00000001',GETDATE(),1)

Insert Into IW_Table_Rule(IW_Table_Rule_Key,Sequence,Table_Name,
Filter_Expression,Entered_By,Entry_Date,System_Supplied_Data)
Values('SYSTEM0100000010',19,'Sample_Admin_Areas','#master.SYSTEM0100000010_key <> ''''',
'TESTDATA00000001',GETDATE(),1)

Insert Into IW_Output_Field(IW_Output_Field_Key,Name,Data_Type,IW_Column_Type_Key,Source_Field_Name,
Generating_Class_Name,Generator_Field_Index,Entered_By,Entry_Date,System_Supplied_Data)
Values ('SYSTEM0100000100','Admin_Area_Key','CHAR(16)','SYSTEM0100000010','key',
null,null,'TESTDATA00000001',GETDATE(),1)

Insert Into IW_Output_Field(IW_Output_Field_Key,Name,Data_Type,IW_Column_Type_Key,Source_Field_Name,
Generating_Class_Name,Generator_Field_Index,Entered_By,Entry_Date,System_Supplied_Data)
Values ('SYSTEM0100000101','Sample_Admin_Areas_Key','CHAR(16)',null,null,
'TKeyFieldGenerator',0,'TESTDATA00000001',GETDATE(),1)


Insert Into IW_Table_Rule_Output_Field(IW_Table_Rule_Key,IW_Output_Field_Key,Entered_By,Entry_Date,
System_Supplied_Data)
Values ('SYSTEM0100000010','SYSTEM010000000P','TESTDATA00000001',GETDATE(),1)


Insert Into IW_Table_Rule_Output_Field(IW_Table_Rule_Key,IW_Output_Field_Key,Entered_By,Entry_Date,
System_Supplied_Data)
Values ('SYSTEM0100000010','SYSTEM0100000024','TESTDATA00000001',GETDATE(),1)

Insert Into IW_Table_Rule_Output_Field(IW_Table_Rule_Key,IW_Output_Field_Key,Entered_By,Entry_Date,
System_Supplied_Data)
Values ('SYSTEM0100000010','SYSTEM0100000100','TESTDATA00000001',GETDATE(),1)

Insert Into IW_Table_Rule_Output_Field(IW_Table_Rule_Key,IW_Output_Field_Key,Entered_By,Entry_Date,
System_Supplied_Data)
Values ('SYSTEM0100000010','SYSTEM0100000101','TESTDATA00000001',GETDATE(),1)


Insert Into IW_Table_Rule_Related_Field(IW_Table_Rule_Key,IW_Column_Type_Key,
Relationship,Entered_By,Entry_Date,System_Supplied_Data)
Values('SYSTEM0100000002','SYSTEM0100000010',1,'TESTDATA00000001',GETDATE(),1)
 

Insert Into IW_Table_Rule_Related_Table(IW_Table_Rule_Key,Table_Name,
Relationship,Entered_By,Entry_Date,System_Supplied_Data)
Values('SYSTEM0100000010','Sample',1,'TESTDATA00000001',GETDATE(),1)
 




 

 