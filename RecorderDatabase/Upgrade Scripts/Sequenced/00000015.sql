-- Report Wizard taxon name optimisations

-- Fix the report join into the taxon names
UPDATE Report_Join
SET Join_SQL= 'FROM #REPORT_OUTPUT LEFT JOIN INDEX_TAXON_NAME ON INDEX_TAXON_NAME.TAXON_LIST_ITEM_KEY=#REPORT_OUTPUT.LIST_ITEM_KEY '+
	'AND INDEX_TAXON_NAME.SYSTEM_SUPPLIED_DATA=1'
WHERE Report_Join_Key = 'NBNSYS0000000027'

-- Fix the common name attribute
UPDATE Report_Attribute 
SET Attribute_SQL = '#REPORT_OUTPUT.[Taxon Common Name] = Index_Taxon_Name.Common_Name'
WHERE Report_Attribute_Key='NBNSYS0000000070'

-- Fix the latin name attribute
UPDATE Report_Attribute 
SET 
	Attribute_SQL = '#REPORT_OUTPUT.[Taxon Latin Name] = Index_Taxon_Name.Preferred_Name',
	Report_Join_Key='NBNSYS0000000027'
WHERE Report_Attribute_Key='NBNSYS0000000071'

-- Fix the authority attribute
UPDATE Report_Attribute 
SET 
	Attribute_SQL = '#REPORT_OUTPUT.[Taxon Latin Name Authority] = Index_Taxon_Name.Authority',
	Report_Join_Key='NBNSYS0000000027'
WHERE Report_Attribute_Key='NBNSYS0000000072'

-- Fix the actual name
UPDATE Report_Attribute 
SET 
	Attribute_SQL = '#REPORT_OUTPUT.[Taxon Latin Name] = Index_Taxon_Name.Actual_Name',
	Report_Join_Key='NBNSYS0000000027'
WHERE Report_Attribute_Key='NBNSYS0000000073'

-- Remove old report joins
DELETE FROM Report_Join WHERE Report_Join_Key IN ('NBNSYS0000000028', 'NBNSYS0000000029')

