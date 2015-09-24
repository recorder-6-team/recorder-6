IF NOT EXISTS(SELECT 1 FROM Language WHERE Language_Key='cy')
	INSERT INTO Language (Language_Key, Item_Name)
	VALUES ('cy', 'Welsh')

IF NOT EXISTS(SELECT 1 FROM Language WHERE Language_Key='en')
	INSERT INTO Language (Language_Key, Item_Name, Priority)
	VALUES ('en', 'English', 1)

IF NOT EXISTS(SELECT 1 FROM Language WHERE Language_Key='fr')
	INSERT INTO Language (Language_Key, Item_Name)
	VALUES ('fr', 'French')

IF NOT EXISTS(SELECT 1 FROM Language WHERE Language_Key='gd')
	INSERT INTO Language (Language_Key, Item_Name)
	VALUES ('gd', 'Gaelic (Scottish)')

IF NOT EXISTS(SELECT 1 FROM Language WHERE Language_Key='la')
	INSERT INTO Language (Language_Key, Item_Name)
	VALUES ('la', 'Latin')

IF NOT EXISTS(SELECT 1 FROM Language WHERE Language_Key='nl')
	INSERT INTO Language (Language_Key, Item_Name)
	VALUES ('nl', 'Dutch')

IF NOT EXISTS(SELECT 1 FROM Language WHERE Language_Key='de')
	INSERT INTO Language (Language_Key, Item_Name)
	VALUES ('de', 'German')

IF NOT EXISTS(SELECT 1 FROM Semantic_Relation WHERE Semantic_Relation_Key='SYSTEM0000000004')
	INSERT INTO Semantic_Relation (Semantic_Relation_Key, Item_Name, Unidirectional, Forward_Equivalence_Possible, Forward_Equivalence_Definite, 
		Reverse_Equivalence_Possible, Reverse_Equivalence_Definite, Proportional_Relationship, Adjacent, Entered_Session_ID, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0000000004', 'Hierarchy, Generic', 0, 1, 
	1, 1, 0, 0, 0, 'SYSTEM0100000000', 1,	'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Thesaurus_Relation_Type WHERE Thesaurus_Relation_Type_Key='SYSTEM0100000000')
	INSERT INTO Thesaurus_Relation_Type (Thesaurus_Relation_Type_Key, Semantic_Relation_Key, Item_Name, Forward_Term, Reverse_Term, Entered_Session_ID, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0100000000', 'SYSTEM0000000004', 'Broader -> Narrower Term', 'is a broader term for', 'is a narrower term for', 'SYSTEM0100000000', 1,	'SYSTEM01')

IF NOT EXISTS(SELECT 1 FROM Subject_Area WHERE Subject_Area_Key='SYSTEM0000000000')
	INSERT INTO Subject_Area (Subject_Area_Key, Item_Name, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0000000000', 'System Subjects', 'SYSTEM0100000000', 1,	'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Domain WHERE Domain_Key='SYSTEM0100000000')
	INSERT INTO Domain (Domain_Key, Item_Name, Subject_Area_Key, Has_Occurrences, Default_Hierarchy_Relation_Type_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES('SYSTEM0100000000', 'General Terminology', 'SYSTEM0000000000', 0, 'SYSTEM0100000000', 'SYSTEM0100000000', 1, 'SYSTEM01')

IF NOT EXISTS(SELECT 1 FROM Domain WHERE Domain_Key='SYSTEM0000000000')
	INSERT INTO Domain (Domain_Key, Item_Name, Subject_Area_Key, Has_Occurrences, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES('SYSTEM0000000000', 'System Term Lists', 'SYSTEM0000000000', 0, 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Local_Domain WHERE Local_Domain_Key='SYSTEM0100000000')
	INSERT INTO Local_Domain (Local_Domain_Key, Item_Name, Domain_Key, Language_Key, Concept_Group_Label, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0100000000', 'English Terminology', 'SYSTEM0100000000', 'en', 'List', 'SYSTEM0100000000', 1, 'SYSTEM01') 

IF NOT EXISTS(SELECT 1 FROM Local_Domain WHERE Local_Domain_Key='SYSTEM0000000000')
	INSERT INTO Local_Domain (Local_Domain_Key, Item_Name, Domain_Key, Language_Key, Concept_Group_Label, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0000000000', 'Global System Term Lists', 'SYSTEM0000000000', 'en', 'Term List', 'SYSTEM0000000000', 1, 'SYSTEM00') 

IF NOT EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key='SYSTEM0100000000')
	INSERT INTO Concept_Group (Concept_Group_Key, Local_Domain_Key, Item_Name, Authority, Hierarchy_Relation_Type_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0100000000', 'SYSTEM0100000000', 'Keywords', 'System', 'SYSTEM0100000000', 'SYSTEM0100000000', 1, 'SYSTEM01')

IF NOT EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key='SYSTEM000000000M')
	INSERT INTO Concept_Group (Concept_Group_Key, Local_Domain_Key, Item_Name, Authority, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM000000000M', 'SYSTEM0000000000', 'Thesaurus Name Types', 'System', 'SYSTEM0100000000', 1, 'SYSTEM01')

IF NOT EXISTS(SELECT 1 FROM Application WHERE Application_Key='SYSTEM0100000000') 
BEGIN
	INSERT INTO Application (Application_Key, Item_Name, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0100000000', 'Recorder Keywords', 'SYSTEM0100000000', 1, 'SYSTEM01')

	INSERT INTO Application_Concept_Group (Application_Key, Concept_Group_Key)
	VALUES ('SYSTEM0100000000', 'SYSTEM0100000000')
END

IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key='SYSTEM000000000K')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM000000000K', 'en', 'Common', 'Common', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key='SYSTEM000000000L')
	INSERT INTO Meaning (Meaning_Key)
	VALUES ('SYSTEM000000000L')

IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key='SYSTEM000000000L')
	INSERT INTO Concept (Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM000000000L', 'SYSTEM000000000K', 'SYSTEM000000000M', 1, 1, 1, 
		'SYSTEM000000000L', 'SYSTEM000000000L', 'SYSTEM0000000000', 1, 'SYSTEM00')

