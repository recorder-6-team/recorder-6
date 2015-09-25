/*===========================================================================*\
  Removed NHMSYS0020170854 and NBNSYS0000000101 from TaxDesList.
\*===========================================================================*/
UPDATE 	Setting
SET		Data = 'NHMSYS0020424779,NHMSYS0001749171,NHMSYS0001770825,NHMSYS0001770462,NBNSYS0000000112'
WHERE 	Name = 'TaxDesList'

GO


