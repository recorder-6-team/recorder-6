/******Change to add Location Vice County to IW ******/

DELETE FROM IW_POST_PROCESSING_PROCEDURE WHERE 
IW_Post_Processing_Procedure_Key = 'SYSTEM01000000z1'

GO

DELETE FROM IW_Table_Rule_Related_Field WHERE  IW_COLUMN_TYPE_KEY= 'SYSTEM01000000Z1'

GO

DELETE FROM IW_Table_Rule_Output_Field WHERE IW_Table_Rule_key = 'SYSTEM01000000Z1'

GO

DELETE FROM IW_Output_Field WHERE IW_Output_Field_Key = 'SYSTEM01000000Z1'

GO

DELETE FROM IW_Output_Field WHERE IW_Output_Field_Key = 'SYSTEM01000000Z2'

GO

DELETE FROM IW_Column_Type_Relationship WHERE IW_Column_Type_key = 'SYSTEM01000000Z1'

GO

DELETE FROM IW_Column_Type_Relationship WHERE RElated_IW_Column_Type_key = 'SYSTEM01000000Z1'



GO

DELETE FROM IW_Column_Type_Pattern WHERE IW_Column_Type_key = 'SYSTEM01000000Z1'

GO

DELETE FROM IW_Table_Rule WHERE IW_Table_Rule_key = 'SYSTEM01000000Z1'

GO

DELETE FROM IW_Column_Type WHERE IW_Column_Type_key = 'SYSTEM01000000Z1'


GO

INSERT INTO IW_Column_Type (IW_Column_Type_key,Class_Name,Item_Name,Required,
Commonly_Used,Parser_Class_name,Entered_By,
Entry_date,System_Supplied_data)
Values ('SYSTEM01000000Z1','TcolumnType','Location Vice County',0,0,
'TSampleVCParser','TESTDATA0000001',Getdate(),1)


GO
INSERT INTO IW_COLUMN_TYPE_PATTERN (IW_COLUMN_TYPE_KEY,PATTERN,Exclude_Match,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES('SYSTEM01000000Z1','Location VC',0,'TESTDATA00000001',getdate(),1)

GO

INSERT INTO IW_Column_Type_Relationship (IW_Column_TYpe_Key,Related_IW_Column_TYpe_Key,
Relationship_Type,Entered_By,Entry_date,System_Supplied_data)
VALUES('SYSTEM01000000Z1','SYSTEM0100000000',0,'TESTDATA00000001',getdate(),1)

GO

INSERT INTO IW_Column_Type_Relationship (IW_Column_TYpe_Key,Related_IW_Column_TYpe_Key,
Relationship_Type,Entered_By,Entry_date,System_Supplied_data)
VALUES('SYSTEM01000000Z1','SYSTEM010000000R',1,'TESTDATA00000001',getdate(),1)

GO

INSERT INTO IW_Column_Type_Relationship (IW_Column_TYpe_Key,Related_IW_Column_TYpe_Key,
Relationship_Type,Entered_By,Entry_date,System_Supplied_data)
VALUES('SYSTEM010000000R','SYSTEM01000000Z1',1,'TESTDATA00000001',getdate(),1)


GO

INSERT INTO IW_TABLE_RULE(IW_TABLE_RULE_KEY,Sequence,Table_Name,Filter_Expression,
Entered_By,Entry_Date,System_supplied_Data)
VALUES ('SYSTEM01000000Z1',22,'Location_Admin_Areas','#master.SYSTEM01000000Z1_key <> ''''',
'TESTDATA00000001',getdate(),1)


GO

INSERT INTO IW_OUTPUT_FIELD (IW_OUTPUT_FIELD_KEY,NAME,DATA_TYPE,IW_COLUMN_TYPE_KEY,SOURCE_FIELD_NAME,
GENERATING_CLASS_NAME,GENERATOR_FIELD_INDEX,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('SYSTEM01000000Z1','Admin_Area_Key','CHAR(16)','SYSTEM01000000Z1',
'key',null,null,'TESTDATA00000001',getdate(),1)

GO

INSERT INTO IW_OUTPUT_FIELD (IW_OUTPUT_FIELD_KEY,NAME,DATA_TYPE,IW_COLUMN_TYPE_KEY,SOURCE_FIELD_NAME,
GENERATING_CLASS_NAME,GENERATOR_FIELD_INDEX,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('SYSTEM01000000Z2','Location_Admin_Areas_Key','CHAR(16)',null,
null,'TKeyFieldGenerator',0,'TESTDATA00000001',getdate(),1)


GO
 
INSERT INTO IW_TABLE_RULE_OUTPUT_FIELD (IW_TABLE_RULE_KEY,IW_OUTPUT_FIELD_KEY,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('SYSTEM01000000Z1','SYSTEM010000000A','TESTDATA00000001',getdate(),1)

GO
 
INSERT INTO IW_TABLE_RULE_OUTPUT_FIELD (IW_TABLE_RULE_KEY,IW_OUTPUT_FIELD_KEY,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('SYSTEM01000000Z1','SYSTEM010000000D','TESTDATA00000001',getdate(),1)

GO
 
INSERT INTO IW_TABLE_RULE_OUTPUT_FIELD (IW_TABLE_RULE_KEY,IW_OUTPUT_FIELD_KEY,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('SYSTEM01000000Z1','SYSTEM0100000024','TESTDATA00000001',getdate(),1)

GO
 
INSERT INTO IW_TABLE_RULE_OUTPUT_FIELD (IW_TABLE_RULE_KEY,IW_OUTPUT_FIELD_KEY,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('SYSTEM01000000Z1','SYSTEM01000000Z1','TESTDATA00000001',getdate(),1)

GO
 
INSERT INTO IW_TABLE_RULE_OUTPUT_FIELD (IW_TABLE_RULE_KEY,IW_OUTPUT_FIELD_KEY,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('SYSTEM01000000Z1','SYSTEM01000000Z2','TESTDATA00000001',getdate(),1)

GO

INSERT INTO IW_table_Rule_Related_Field(IW_TABLE_RULE_KEY,IW_Column_Type_key,
Relationship,Entered_By,Entry_Date,System_Supplied_Data)
VALUES ('SYSTEM01000000Z1','SYSTEM01000000Z1',2,'TESTDATA00000001',getdate(),1)

GO
/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_TidyLocationLocationAdminArea]    Script Date: 11/13/2019 21:40:13 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
 
  Created:      Nov 2019
 
    $Author: MikeWeideli $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_TidyLocationLocationAdminArea]
AS
    Declare @LocationKey as char(16) 
    Declare @LocationAdminArea char(16)
 
    DELETE FROM  #Location_Admin_Areas 
    WHERE LOCATION_Key IS NULL OR ADMIN_AREA_KEY = ''
    
    DELETE FROM #Location_Admin_Areas
    WHERE EXISTS(SELECT * FROM Location_ADMIN_AREAS 
    WHERE LOCATION_KEY = #Location_Admin_Areas.LOCATION_KEY 
    AND Location_ADMIN_AREAS.ADMIN_AREA_KEY =
     #Location_Admin_Areas.ADMIN_AREA_KEY) 
   
     
    DECLARE csrLocations CURSOR
    FOR
    SELECT DISTINCT Location_Key,Admin_Area_Key
    FROM #Location_Admin_Areas
   
    
    OPEN csrLocations 
    
        
    FETCH NEXT FROM csrLocations INTO @LocationKey,@LocationAdminArea
    
    DELETE FROM #Location_Admin_Areas 
    WHERE #Location_Admin_Areas.Location_Admin_Areas_Key 
    > (Select Min(Location_Admin_Areas_Key) FROM 
    #Location_Admin_Areas LA2 WHERE LA2.Location_Key
    = @LocationKey AND LA2.Admin_Area_Key =
    @LocationAdminArea) AND  #Location_Admin_Areas.Location_Key
    = @LocationKey  AND #Location_Admin_Areas.Admin_Area_Key =
    @LocationAdminArea
	
    
    WHILE @@FETCH_STATUS = 0
    BEGIN
	
	FETCH NEXT FROM csrLocations INTO @LocationKey,@LocationAdminArea
    
    DELETE FROM #Location_Admin_Areas 
    WHERE #Location_Admin_Areas.Location_Admin_Areas_Key
    > (Select Min(Location_Admin_Areas_Key) FROM 
    #Location_Admin_Areas LA2 WHERE LA2.Location_Key
    = @LocationKey AND LA2.Admin_Area_Key =
    @LocationAdminArea) AND  #Location_Admin_Areas.Location_Key
    = @LocationKey  AND #Location_Admin_Areas.Admin_Area_Key =
    @LocationAdminArea
	
        
	
    END
   
    CLOSE csrLocations
    DEALLOCATE csrLocations

GO

    GRANT EXECUTE ON [dbo].[usp_ImportWizard_TidyLocationLocationAdminArea] TO PUBLIC

GO

    INSERT IW_Post_Processing_Procedure (IW_Post_Processing_Procedure_Key,Sequence,
    Required_Table_Name,Procedure_name,Entered_By,Entry_Date,System_Supplied_data)
    VALUES     ('SYSTEM01000000Z1',12,'Location_Admin_Areas',
    'usp_ImportWizard_TidyLocationLocationAdminArea',
    'TESTDATA00000001', getdate(),1)   