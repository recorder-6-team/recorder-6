--Export a reference includes the supplier
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0200000000')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0200000000', 'REFERENCEREFERENCE_SUPPLIER','Reference','Source_Key','Reference_Supplier','Source_Key',1,1)
	
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0200000001')
	-- Export a supplier includes the organisation
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0200000001', 'NAMEREFERENCE_SUPPLIER','Name','Name_Key','Reference_Supplier','Supplier_Name_Key',1,0)
	
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0200000002')
	--Export a reference includes the survey metadata
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0200000002', 'REFERENCEREFERENCE_SURVEY_METADATA','Reference','Source_Key','Reference_Survey_Metadata','Source_Key',0,1)
	
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0200000003')
	--Export a Reference include the location and vice versa
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0200000003', 'REFERENCEREFERENCE_LOCATION','Reference','Source_Key','Reference_Location','Source_Key',1,1)
	
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0200000004')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0200000004', 'LOCATIONREFERENCE_LOCATION','Location','Location_Key','Reference_Location','Location_Key',1,1)
	
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0200000005')
	--Export survey metadata includes the commissioned by organisation
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0200000005', 'NAMEREFERENCE_SURVEY_METADATA','Name','Name_Key','Reference_Survey_Metadata','Commissioned_By_Name_Key',1,0)
	
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0200000006')
	--supplier concepts
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0200000006', 'CONCEPTREFERENCE_SUPPLIER_MEDIA','Concept','Concept_Key','Reference_Supplier','Media_Concept_Key',1,0)
	
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0200000007')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0200000007', 'CONCEPTREFERENCE_SUPPLIER_FORMAT','Concept','Concept_Key','Reference_Supplier','Format_Concept_Key',1,0)
	
IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM0200000008')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM0200000008', 'CONCEPTREFERENCE_SUPPLIER_AVAIL','Concept','Concept_Key','Reference_Supplier','Availability_Concept_Key',1,0)
