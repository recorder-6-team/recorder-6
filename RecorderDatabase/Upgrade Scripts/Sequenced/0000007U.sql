
/*==================================================================================================*\
	Script created by John van Breda 09 May 2010 to create a taxon group filter for the report wizard
 \*================================================================================================*/

IF NOT EXISTS(SELECT 1 FROM Usable_Table WHERE Usable_Table_Key='JNCCDEV100000019')
    INSERT INTO Usable_Table VALUES (
        'JNCCDEV100000019',
        'Taxon_Group',
        'Taxon_Version',
        'Taxon_Group.Taxon_Group_Key = Taxon_Version.Output_Group_Key',
        NULL,
        'T',
        5) -- must be higher than the join_order for Taxon_Version

IF NOT EXISTS(SELECT 1 FROM Usable_Field WHERE Usable_Field_Key='JNCCDEV100000049')
    INSERT INTO Usable_Field VALUES (
        'JNCCDEV100000049',
        'Taxon_Group',
        'Taxon_Group_Name',
        'Taxon Group',
        'TEXT',
        'T',
        0,
        0,
        1,
        'Taxon_Group.Taxon_Group_Name'
    )