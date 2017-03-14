/****** Allow reporting on sample Vague date start, sample vague date end and combinatiion of Location and Location Name******/

/*===========================================================================*\
  Drop udf before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetLocationAndLocationName]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[ufn_GetLocationAndLocationName]

GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO


/****** Object:  UserDefinedFunction [dbo].[ufn_GetLocationAndLocationName]    Script Date: 01/30/2017 12:13:15 ******/
/*===========================================================================*\
  Description:	
		Gets the Location and the Location Name for a Sample. 

  Parameters:	
		@sample_Key The key of the Sample of interest.

  Created:	January 2017

  Last revision information:
   
    $Author: Michael Weideli $

\*===========================================================================*/
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[ufn_GetLocationAndLocationName]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
   DROP FUNCTION [dbo].[ufn_GetLocationAndLocationName]

GO


CREATE FUNCTION [dbo].[ufn_GetLocationAndLocationName]
(
	@LocationName varchar(100),@Location varchar(100)
)
RETURNS	VARCHAR(202)

AS
BEGIN
  DECLARE  @ResultString VARCHAR(202),@Delimeter varChar(2)
  
  set @Delimeter = '. '
  If @Location = @LocationName Set @Location = ''
  if Isnull(@Location,'') = '' OR  ISNULL(@LocationName,'') = '' Set @Delimeter = ''
  Set @ResultString = ISNULL(@Location,'') + @Delimeter + ISNULL(@LocationName,'')    
  RETURN @ResultString;
END

GO

GRANT EXECUTE ON [dbo].[ufn_GetLocationAndLocationName] to public

GO

DELETE FROM REPORT_FIELD WHERE REPORT_FIELD_KEY = 'LCA0002300000600'
DELETE FROM REPORT_FIELD WHERE REPORT_FIELD_KEY = 'LCA0002300000601'
DELETE FROM REPORT_FIELD WHERE REPORT_FIELD_KEY = 'LCA0002300000602'

DELETE FROM  REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY = 'LCA0002300000600'
DELETE FROM  REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY = 'LCA0002300000601'
DELETE FROM  REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY = 'LCA0002300000602'


INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,ITEM_NAME,ITEM_GROUP,SOURCE_TABLE,ATTRIBUTE_SQL,REPORT_JOIN_KEY,ENTERED_BY,
ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002300000600','Sample Start Date','Sample','SAMPLE','#REPORT_OUTPUT.[Sample_Start_Date] = [dbo].[LCReturnDate](sample.Vague_Date_Start,sample.Vague_Date_Type,''F'')',
'NBNSYS0000000007','TESTDATA00000001',getdate(),1)

INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,ITEM_NAME,ITEM_GROUP,SOURCE_TABLE,ATTRIBUTE_SQL,REPORT_JOIN_KEY,ENTERED_BY,
ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002300000601','Sample End Date','Sample','SAMPLE','#REPORT_OUTPUT.[Sample_End_Date] = [dbo].[LCReturnDate](sample.Vague_Date_End,sample.Vague_Date_Type,''F'')',
'NBNSYS0000000007','TESTDATA00000001',getdate(),1)


INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,ITEM_NAME,ITEM_GROUP,SOURCE_TABLE,ATTRIBUTE_SQL,REPORT_JOIN_KEY,ENTERED_BY,
ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002300000602','Sample Location Info','Sample','SAMPLE','#REPORT_OUTPUT.[Sample_Location_Info] = [dbo].[ufn_GetLocationAndLocationName](sample.Location_Name,Location_Name.Item_Name)',
'NBNSYS0000000008','TESTDATA00000001',getdate(),1)

GO

INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,ENTERED_BY,REPORT_ATTRIBUTE_KEY,FIELD_ITEM_NAME,FIELD_TYPE,FIELD_SIZE,
ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002300000600','NBNSYS000000001','LCA0002300000600', 'Sample_Start_Date','varchar',10,getdate(),1)


INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,ENTERED_BY,REPORT_ATTRIBUTE_KEY,FIELD_ITEM_NAME,FIELD_TYPE,FIELD_SIZE,
ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002300000601','NBNSYS000000001','LCA0002300000601', 'Sample_End_Date','varchar',10,getdate(),1)



INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,ENTERED_BY,REPORT_ATTRIBUTE_KEY,FIELD_ITEM_NAME,FIELD_TYPE,FIELD_SIZE,
ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('LCA0002300000602','NBNSYS000000001','LCA0002300000602', 'Sample_Location_Info','varchar',202,getdate(),1)

