/*
  Adds the option to display Survey Tags to the report wizard.

  Last Revision Details:
    $Revision: 2 $
    $Date: 16/01/09 11:20 $
    $Author: Pauldavies $
  
*/

DECLARE @Report_Join_Key		CHAR(16)

SET @Report_Join_Key = 'NBNSYS0000000041'

INSERT INTO Report_Join (
	Report_Join_Key,
	Join_SQL
) VALUES (
	@Report_Join_Key,
	'FROM #REPORT_OUTPUT'
)

DECLARE @Report_Attribute_Key	CHAR(16)

SET @Report_Attribute_Key = 'NBNSYS00000000BA'

INSERT INTO Report_Attribute (
	Report_Attribute_Key,
	Item_Group,
	Source_Table,
	Item_Name,
	Attribute_SQL,
	Report_Join_Key,
	System_Supplied_Data
) VALUES (
	@Report_Attribute_Key,
	'Survey',
	'SURVEY',
	'Survey Tags',
	'#REPORT_OUTPUT.[Survey Tags] = dbo.ufn_GetSurveyTagString(#REPORT_OUTPUT.Survey_Key)',
	@Report_Join_Key,
	1
)

DECLARE @Report_Field_Key	CHAR(16)

SET @Report_Field_Key = 'NBNSYS00000000C9'

INSERT INTO Report_Field (
	Report_Field_Key,
	Report_Attribute_Key,
	Field_Item_Name,
	Field_Type,
	Field_Size,
	System_Supplied_Data
) VALUES (
	@Report_Field_Key,
	@Report_Attribute_Key,
	'Survey Tags',
	'text',
	1000,
	1
)