/*===========================================================================*\
  Concept: Formal
\*===========================================================================*/
IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key = 'SYSTEM0000000000')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0000000000', 'en', 'Formal', 'Formal', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key = 'SYSTEM0000000000') INSERT INTO Meaning (Meaning_Key) VALUES ('SYSTEM0000000000')

IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = 'SYSTEM0000000000')
	INSERT INTO Concept (
		Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES (
		'SYSTEM0000000000', 'SYSTEM0000000000', 'SYSTEM000000000M', 1, 1, 1, 
		'SYSTEM0000000000', 'SYSTEM0000000000', 'SYSTEM0000000000', 1, 'SYSTEM00')

/*===========================================================================*\
  Concept: Unknown
\*===========================================================================*/
IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key = 'SYSTEM0000000003')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0000000003', 'en', 'Unknown', 'Unknown', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key = 'SYSTEM000000008Z') INSERT INTO Meaning (Meaning_Key) VALUES ('SYSTEM000000008Z')

IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = 'SYSTEM00000000AN')
	INSERT INTO Concept (
		Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES (
		'SYSTEM00000000AN', 'SYSTEM0000000003', 'SYSTEM000000000M', 1, 1, 1, 
		'SYSTEM0000000000', 'SYSTEM000000008Z', 'SYSTEM0000000000', 1, 'SYSTEM00')

/*===========================================================================*\
  Concept Group: Thesaurus Fact Types
  Terms in group: Currency Symbol, JPEG, GIF, Bitmap, WAV, AVI, HTML, Fact
\*===========================================================================*/
IF NOT EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key = 'SYSTEM000000000L')
	INSERT INTO Concept_Group (Concept_Group_Key, Local_Domain_Key, Item_Name, Authority, Entered_Session_ID, System_Supplied_Data)
	VALUES ('SYSTEM000000000L', 'SYSTEM0000000000', 'Thesaurus Fact Types', 'System', 'SYSTEM0000000000', 1)

-- Terms
IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key = 'SYSTEM000000008R')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM000000008R', 'en', 'Currency Symbol', 'Currency Symbol', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key = 'SYSTEM00000000VY')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM00000000VY', 'en', 'JPEG', 'JPEG', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key = 'SYSTEM00000000VZ')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM00000000VZ', 'en', 'GIF', 'GIF', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key = 'SYSTEM00000000W0')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM00000000W0', 'en', 'Bitmap', 'Bitmap', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key = 'SYSTEM00000001K9')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM00000001K9', 'en', 'WAV', 'WAV', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key = 'SYSTEM00000001KA')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM00000001KA', 'en', 'AVI', 'AVI', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key = 'SYSTEM00000001KV')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM00000001KV', 'en', 'HTML', 'HTML', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Term WHERE Term_Key = 'SYSTEM00000001L8')
	INSERT INTO Term (Term_Key, Language_Key, Item_Name, PlainText, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM00000001L8', 'en', 'Fact', 'Fact', 'SYSTEM0000000000', 1, 'SYSTEM00')

-- Meanings
IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key = 'SYSTEM000000009A') INSERT INTO Meaning (Meaning_Key) VALUES ('SYSTEM000000009A')
IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key = 'SYSTEM00000000C9') INSERT INTO Meaning (Meaning_Key) VALUES ('SYSTEM00000000C9')
IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key = 'SYSTEM00000000CA') INSERT INTO Meaning (Meaning_Key) VALUES ('SYSTEM00000000CA')
IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key = 'SYSTEM00000000CB') INSERT INTO Meaning (Meaning_Key) VALUES ('SYSTEM00000000CB')
IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key = 'SYSTEM000000023W') INSERT INTO Meaning (Meaning_Key) VALUES ('SYSTEM000000023W')
IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key = 'SYSTEM000000023X') INSERT INTO Meaning (Meaning_Key) VALUES ('SYSTEM000000023X')
IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key = 'SYSTEM000000026C') INSERT INTO Meaning (Meaning_Key) VALUES ('SYSTEM000000026C')
IF NOT EXISTS(SELECT 1 FROM Meaning WHERE Meaning_Key = 'SYSTEM000000026E') INSERT INTO Meaning (Meaning_Key) VALUES ('SYSTEM000000026E')

-- Concepts
IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = 'SYSTEM000000009A')
	INSERT INTO Concept (
		Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Sort_Code, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES (
		'SYSTEM000000009A', 'SYSTEM000000008R', 'SYSTEM000000000L', 1, 1, 1, 
		'SYSTEM000000000L', 'SYSTEM000000009A', 0, 'SYSTEM0000000000', 1, 'SYSTEM00')
IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = 'SYSTEM00000000HP')
	INSERT INTO Concept (
		Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES (
		'SYSTEM00000000HP', 'SYSTEM00000000VY', 'SYSTEM000000000L', 1, 1, 1, 
		'SYSTEM00000000AN', 'SYSTEM00000000C9', 'SYSTEM0000000000', 1, 'SYSTEM00')
IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = 'SYSTEM00000000HQ')
	INSERT INTO Concept (
		Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES (
		'SYSTEM00000000HQ', 'SYSTEM00000000VZ', 'SYSTEM000000000L', 1, 1, 1, 
		'SYSTEM00000000AN', 'SYSTEM00000000CA', 'SYSTEM0000000000', 1, 'SYSTEM00')
IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = 'SYSTEM00000000HR')
	INSERT INTO Concept (
		Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES (
		'SYSTEM00000000HR', 'SYSTEM00000000W0', 'SYSTEM000000000L', 1, 1, 1, 
		'SYSTEM00000000AN', 'SYSTEM00000000CB', 'SYSTEM0000000000', 1, 'SYSTEM00')
IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = 'SYSTEM00000002L8')
	INSERT INTO Concept (
		Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES (
		'SYSTEM00000002L8', 'SYSTEM00000001K9', 'SYSTEM000000000L', 1, 1, 1, 
		'SYSTEM00000000AN', 'SYSTEM000000023W', 'SYSTEM0000000000', 1, 'SYSTEM00')
IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = 'SYSTEM00000002L9')
	INSERT INTO Concept (
		Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES (
		'SYSTEM00000002L9', 'SYSTEM00000001KA', 'SYSTEM000000000L', 1, 1, 1, 
		'SYSTEM00000000AN', 'SYSTEM000000023X', 'SYSTEM0000000000', 1, 'SYSTEM00')
IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = 'SYSTEM00000002NO')
	INSERT INTO Concept (
		Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES (
		'SYSTEM00000002NO', 'SYSTEM00000001KV', 'SYSTEM000000000L', 1, 1, 1, 
		'SYSTEM00000000AN', 'SYSTEM000000026C', 'SYSTEM0000000000', 1, 'SYSTEM00')
IF NOT EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = 'SYSTEM00000002NQ')
	INSERT INTO Concept (
		Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
		Name_Type_Concept_Key, Meaning_Key, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES (
		'SYSTEM00000002NQ', 'SYSTEM00000001L8', 'SYSTEM000000000L', 1, 1, 1, 
		'SYSTEM00000000AN', 'SYSTEM000000026E', 'SYSTEM0000000000', 1, 'SYSTEM00')

-- Link Application to Concept Group
IF NOT EXISTS(SELECT 1 FROM Application WHERE Application_Key = 'SYSTEM0000000001')
BEGIN
	INSERT INTO Application (Application_Key, Item_Name, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0000000001', 'Thesaurus Fact Types', 'SYSTEM0000000000', 1, 'SYSTEM00')

	INSERT INTO Application_Concept_Group (Application_Key, Concept_Group_Key)
	VALUES ('SYSTEM0000000001', 'SYSTEM000000000L')
END


/*===========================================================================*\
  Concept Group: Survey Tags
\*===========================================================================*/
IF NOT EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key = 'SYSTEM0100000001')
	INSERT INTO Concept_Group (Concept_Group_Key, Local_Domain_Key, Item_Name, Authority, Entered_Session_ID, System_Supplied_Data)
	VALUES ('SYSTEM0100000001', 'SYSTEM0100000000', 'Tags', 'System', 'SYSTEM0100000000', 1)

-- Link Application to Concept Group
IF NOT EXISTS(SELECT 1 FROM Application WHERE Application_Key = 'SYSTEM0100000001')
BEGIN
	INSERT INTO Application (Application_Key, Item_Name, Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0100000001', 'Survey Tags', 'SYSTEM0100000000', 1, 'SYSTEM01')

	INSERT INTO Application_Concept_Group (Application_Key, Concept_Group_Key)
	VALUES ('SYSTEM0100000001', 'SYSTEM0100000001')
END
