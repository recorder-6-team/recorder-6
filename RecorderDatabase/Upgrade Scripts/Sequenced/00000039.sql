/* Fix Taxon Group report attribute for long group names */
UPDATE Report_Field
SET Field_Size=50
WHERE Report_Field_Key='NBNSYS0000000096'

	