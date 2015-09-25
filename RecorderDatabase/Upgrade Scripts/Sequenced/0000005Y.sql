/*
  Adds new rows to (and corrects one old row in) the Database_Relationship
  table, to make sure they are properly exported.

  Last Revision Details:
    $Revision: 3 $
    $Date: 21/01/09 10:53 $
    $Author: Pauldavies $
  
*/

-- Clears the records added in this script in case it is run more than
-- once.
DELETE FROM Database_Relationship
WHERE	Relationship_Key	IN (
	'SYSTEM0100000002',
	'SYSTEM0000000031',
	'NBNSYS000000004C',
	'NBNSYS000000004D',
	'NBNSYS000000004E',
	'NBNSYS000000004F',
	'NBNSYS000000004G',
	'NBNSYS000000004H',
	'NBNSYS000000004I',
	'NBNSYS000000004J'
)

-- Adds a relationship between sources and references.
INSERT INTO	Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
) VALUES (
	'SYSTEM0100000002',
	'SOURCESOURCE_KEY',
	'SOURCE',
	'SOURCE_KEY',
	'REFERENCE',
	'SOURCE_KEY',
	1,
	1,
	1
)
	
-- Adds a relationship between Organisations and Organisation_Departments.
INSERT INTO	Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
) VALUES (
	'SYSTEM0000000031',
	'ORGANISATIONOrganisationDepartment',
	'ORGANISATION',
	'NAME_KEY',
	'Organisation_Department',
	'Name_Key',
	0,
	1,
	0
)

-- Updates the link between Survey_Event_Owner_Type and Survey_Event_Owner to use the
-- correct name of the Survey_Event_Owner table.
UPDATE	Database_Relationship
SET		Detail_Table		=	'SURVEY_EVENT_OWNER',
		Detail_Field		=	'SURVEY_EVENT_OWNER_TYPE_KEY'
WHERE	Relationship_Key	=	'NBNSYS0000000049'

-- Adds a relationship between Survey_Tags and Concepts.
INSERT INTO	Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
) VALUES (
	'NBNSYS000000004C',
	'ConceptSurvey_Tag',
	'Concept',
	'Concept_Key',
	'Survey_Tag',
	'Concept_Key',
	1,
	0,
	0
)

-- Adds a relationship between Locations and Location_Types.
INSERT INTO	Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
) VALUES (
	'NBNSYS000000004D',
	'LOCATION_TYPELOCATION',
	'LOCATION_TYPE',
	'LOCATION_TYPE_KEY',
	'LOCATION',
	'LOCATION_TYPE_KEY',
	1,
	0,
	0
)

-- Adds a relationship between Location_Feature and Management_Aim.
INSERT INTO	Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
) VALUES (
	'NBNSYS000000004E',
	'LOCATION_FEATUREMANAGEMENT_AIM',
	'LOCATION_FEATURE',
	'LOCATION_FEATURE_KEY',
	'MANAGEMENT_AIM',
	'LOCATION_FEATURE_KEY',
	0,
	1,
	0
)

-- Adds a relationship between Measurement_Type and Measurement_Type_Context.
INSERT INTO	Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
) VALUES (
	'NBNSYS000000004F',
	'MEASUREMENT_TYPEMEASUREMENT_TYPE_CONTEXT',
	'MEASUREMENT_TYPE',
	'MEASUREMENT_TYPE_KEY',
	'MEASUREMENT_TYPE_CONTEXT',
	'MEASUREMENT_TYPE_KEY',
	0,
	1,
	0
)

-- Adds a relationship between Measurement_Context and Measurement_Type_Context.
INSERT INTO	Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
) VALUES (
	'NBNSYS000000004G',
	'MEASUREMENT_CONTEXTMEASUREMENT_TYPE_CONTEXT',
	'MEASUREMENT_CONTEXT',
	'MEASUREMENT_CONTEXT_KEY',
	'MEASUREMENT_TYPE_CONTEXT',
	'MEASUREMENT_CONTEXT_KEY',
	1,
	0,
	0
)

-- Adds a relationship between Location and Grid_Square.
INSERT INTO	Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
) VALUES (
	'NBNSYS000000004H',
	'LOCATIONGRID_SQUARE',
	'LOCATION',
	'LOCATION_KEY',
	'GRID_SQUARE',
	'LOCATION_KEY',
	0,
	1,
	0
)

-- Adds a relationship between Location and Location_Boundary.
INSERT INTO	Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
) VALUES (
	'NBNSYS000000004I',
	'LOCATIONLOCATION_BOUNDARY',
	'LOCATION',
	'LOCATION_KEY',
	'LOCATION_BOUNDARY',
	'LOCATION_KEY',
	0,
	1,
	0
)

-- Adds a relationship between Surveys and Survey_Tags.
INSERT INTO	Database_Relationship (
	Relationship_Key,
	Relationship_Name,
	Master_Table,
	Master_Field,
	Detail_Table,
	Detail_Field,
	Follow_Up,
	Follow_Down,
	One_To_One
) VALUES (
	'NBNSYS000000004J',
	'SURVEYSurvey_Tag',
	'SURVEY',
	'SURVEY_KEY',
	'Survey_Tag',
	'Survey_Key',
	0,
	1,
	0
)

-- Updates the link between Reference_Keyword and Concept_Key so that 
-- it is going in the right direction
UPDATE	Database_Relationship
SET		Follow_Up			=	1,
		Follow_Down			=	0
WHERE	Relationship_Key	=	'SYSTEM0100000001'