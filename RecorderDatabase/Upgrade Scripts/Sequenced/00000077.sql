/*============================================================================*\
Description:	Additional entries in Database_Relationship to handle new table
				User_Survey_Restriction.

Created:		May 2009

Last revision information:
	$Revision: 2 $
	$Date: 14/05/09 8:28 $
	$Author: Simonwood $
\*============================================================================*/

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key = 'SYSTEM0000000032')
	INSERT INTO Database_Relationship (
		Relationship_Key, 
		Relationship_Name, 
		Master_Table, 
		Master_Field, 
		Detail_Table, 
		Detail_Field, 
		Follow_Up, 
		Follow_Down, 
		One_To_One
	) 
	VALUES (
		'SYSTEM0000000032', 
		'User_UserSurveyRestriction', 
		'User', 
		'Name_Key', 
		'User_Survey_Restriction', 
		'Name_Key', 
		0, 
		1, 
		0
	)
	
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key = 'SYSTEM0000000033')
	INSERT INTO Database_Relationship (
		Relationship_Key, 
		Relationship_Name, 
		Master_Table, 
		Master_Field, 
		Detail_Table, 
		Detail_Field, 
		Follow_Up, 
		Follow_Down, 
		One_To_One
	) 
	VALUES (
		'SYSTEM0000000033', 
		'Survey_UserSurveyRestriction', 
		'Survey', 
		'Survey_Key', 
		'User_Survey_Restriction', 
		'Survey_Key', 
		0, 
		1, 
		0
	)
	
