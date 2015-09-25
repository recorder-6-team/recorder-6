/*===========================================================================*\
  Fix up Concept Group for Survey Tags. Allow hierarchy.
\*===========================================================================*/
IF EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key = 'SYSTEM0100000001')
	UPDATE	Concept_Group
	SET	Hierarchy_Relation_Type_Key = 'SYSTEM0100000000'
	WHERE Concept_Group_Key = 'SYSTEM0100000001'
ELSE
	INSERT INTO Concept_Group (Concept_Group_Key, Local_Domain_Key, Item_Name, Authority, Hierarchy_Relation_Type_Key, Entered_Session_ID, System_Supplied_Data)
	VALUES ('SYSTEM0100000001', 'SYSTEM0100000000', 'Tags', 'System', 'SYSTEM0100000001', 'SYSTEM0100000000', 1)