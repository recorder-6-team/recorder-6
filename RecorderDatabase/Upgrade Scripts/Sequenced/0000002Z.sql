-- New records to add to Database_Relationship table to enable database export on the Thesaurus tables.
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000001Z')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000001Z', 'ConceptConcept', 'Concept', 'Concept_Key', 'Concept', 'Name_Type_Concept_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0000000020')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM0000000020', 'ConceptConcept_Lineage', 'Concept', 'Concept_Key', 'Concept_Lineage', 'Concept_Key', 0, 1)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0000000021')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM0000000021', 'ConceptConcept_DesignationConcept', 'Concept', 'Concept_Key', 'Concept_Designation', 'Concept_Key', 0, 1)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0000000022')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM0000000022', 'ConceptConcept_DesignationDesignation', 'Concept', 'Concept_Key', 'Concept_Designation', 'Designation_Type_Concept_Key', 0, 1)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002V')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM0000000025', 'LanguageLocal_Domain', 'Language', 'Language_Key', 'Local_Domain', 'Language_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002V')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002V', 'LanguageTerm', 'Language', 'Language_Key', 'Term', 'Language_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0000000028')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM0000000028', 'LanguageThesaurus_Fact', 'Language', 'Language_Key', 'Thesaurus_Fact', 'Language_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0000000029')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM0000000029', 'TermTerm_Version', 'Term', 'Term_Key', 'Term_Version', 'Term_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002A')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002A', 'TermConcept', 'Term', 'Term_Key', 'Concept', 'Term_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002B')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002B', 'MeaningTerm_Version', 'Meaning', 'Meaning_Key', 'Term_Version', 'Meaning_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002C')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002C', 'MeaningThesaurus_Fact', 'Meaning', 'Meaning_Key', 'Thesaurus_Fact', 'Meaning_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002D')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002D', 'MeaningConcept', 'Meaning', 'Meaning_Key', 'Concept', 'Meaning_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002E')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002E', 'MeaningMeaning_Relation', 'Meaning', 'Meaning_Key', 'Meaning_Relation', 'From_Meaning_Key', 0, 1)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002F')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002F', 'Local_DomainDomain_Hyperlink', 'Local_Domain', 'Local_Domain_Key', 'Domain_Hyperlink', 'Local_Domain_Key', 0, 1)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002G')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002G', 'Local_DomainConcept_Group', 'Local_Domain', 'Local_Domain_Key', 'Concept_Group', 'Local_Domain_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002H')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002H', 'DomainLocal_Domain', 'Domain', 'Domain_Key', 'Local_Domain', 'Domain_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002I')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002I', 'DomainConcept_Rank', 'Domain', 'Domain_Key', 'Concept_Rank', 'Domain_Key', 0, 1)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002J')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002J', 'Subject_AreaDomain', 'Subject_Area', 'Subject_Area_Key', 'Domain', 'Subject_Area_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002K')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002K', 'ConceptConcept_History', 'Concept', 'Concept_Key', 'Concept_History', 'Concept_Key', 1, 1)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002L')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002L', 'Concept_Group_VersionConcept_History', 'Concept_Group_Version', 'Concept_Group_Version_Key', 'Concept_History', 'Concept_Group_Version_From', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002M')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002M', 'Concept_GroupConcept_Group_Version', 'Concept_Group', 'Concept_Group_Key', 'Concept_Group_Version', 'Concept_Group_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002N')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002N', 'ConceptConcept_Relation', 'Concept', 'Concept_Key', 'Concept_Relation', 'From_Concept_Key', 0, 1)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002O')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002O', 'Thesaurus_Relation_TypeConcept_Relation', 'Thesaurus Relation_Type', 'Thesaurus_Relation_Type_Key', 'Concept_Relation', 'Thesaurus_Relation_Type_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002P')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002P', 'Thesaurus_Relation_TypeMeaning_Relation', 'Thesaurus Relation_Type', 'Thesaurus_Relation_Type_Key', 'Meaning_Relation', 'Thesaurus_Relation_Type_Key', 1, 1)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002R')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002R', 'Thesaurus_Relation_TypeThesaurus_Relation_Type_Usage', 'Thesaurus Relation_Type', 'Thesaurus_Relation_Type_Key', 'Thesaurus_Relation_Type_Usage', 'Thesaurus_Relation_Type_Key', 0, 1)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002S')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002S', 'Semantic_RelationThesaurus_Relation_Type', 'Semantic_Relation', 'Semantic_Relation_Key', 'Thesaurus_Relation_Type', 'Semantic_Relation_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002U')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002U', 'NAMEOrganisation_Department', 'NAME', 'NAME_KEY', 'Organisation_Department', 'Name_Key', 1, 0)

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002V')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table,  Detail_Field, Follow_Up, Follow_Down) 
	VALUES('SYSTEM000000002V', 'Concept_Group_VersionConcept_History_To', 'Concept_Group_Version', 'Concept_Group_Version_Key', 'Concept_History', 'Concept_Group_Version_To', 1, 0)

IF NOT EXISTS(SELECT * FROM Database_Relationship WHERE Relationship_Key = 'SYSTEM000000002W')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM000000002W', 'Concept_GroupConcept', 'Concept_Group', 'Concept_Group_Key', 'Concept', 'Concept_Group_Key', 1, 1)
	
IF NOT EXISTS(SELECT * FROM Database_Relationship WHERE Relationship_Key = 'SYSTEM000000002Y')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM000000002Y', 'SourceSource_Join','Source', 'Source_Key', 'Source_Join', 'Source_Key', 0, 1)
	
IF NOT EXISTS(SELECT * FROM Database_Relationship WHERE Relationship_Key='SYSTEM0000000030')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0000000030', 'ThesaurusRelationTypeDomain', 'Thesaurus_Relation_Type', 'Thesaurus_Relation_Type_Key', 'Domain', 'Domain_Key', 0, 1)
	
IF NOT EXISTS(SELECT * FROM Database_Relationship WHERE Relationship_Key='SYSTEM0100000000')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0100000000', 'ReferenceReferenceKeyword', 'Reference', 'Source_Key', 'Reference_Keyword', 'Source_Key', 1, 1)

IF NOT EXISTS(SELECT * FROM Database_Relationship WHERE Relationship_Key='SYSTEM0100000001')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0100000001', 'ConceptReferenceKeyword', 'Concept', 'Concept_Key', 'Reference_Keyword', 'Concept_Key', 0, 1)	


		