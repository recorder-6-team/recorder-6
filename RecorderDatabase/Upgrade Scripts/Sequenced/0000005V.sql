INSERT INTO dbo.Database_Relationship (
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
	'NBNSYS000000004B',
	'LOCATIONPARENTLOCATION',
	'Location',
	'Parent_Key',
	'Location',
	'Location_Key',
	0,
	1,
	0
)