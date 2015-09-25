
/*==================================================================================================*\
	Script created by John van Breda 09 May 2010 to correct missing apostrophes from status attributes in the report wizard
 \*================================================================================================*/

UPDATE Report_Attribute 
SET Attribute_Sql=CAST(Attribute_Sql AS VARCHAR(8000))+''''
where Attribute_Sql like '%=''Yes'