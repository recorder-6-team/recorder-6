/*===========================================================================*\
  Script created by SBW 17 March 2009. 
  The reason was to bring location and location name closer together in the import 
  wizard (when matching columns). The catalyst for the change was a tendancy for 
  users to match to location when, very often, location name would be more 
  appropriate.
  The script reassigns a new sequence to the first eight columns in the initial 
  drop down list (when the column are assigned to known column types).
\*===========================================================================*/


UPDATE IW_COLUMN_TYPE 
SET SEQUENCE = 1 
WHERE IW_COLUMN_TYPE_KEY = 'SYSTEM010000000T'

UPDATE IW_COLUMN_TYPE 
SET SEQUENCE = 2 
WHERE IW_COLUMN_TYPE_KEY = 'SYSTEM0100000001'

UPDATE IW_COLUMN_TYPE 
SET SEQUENCE = 3 
WHERE IW_COLUMN_TYPE_KEY = 'SYSTEM0100000002'

UPDATE IW_COLUMN_TYPE 
SET SEQUENCE = 4 
WHERE IW_COLUMN_TYPE_KEY = 'SYSTEM0100000003'

UPDATE IW_COLUMN_TYPE 
SET SEQUENCE = 5 
WHERE IW_COLUMN_TYPE_KEY = 'SYSTEM0100000004'

UPDATE IW_COLUMN_TYPE 
SET SEQUENCE = 6 
WHERE IW_COLUMN_TYPE_KEY = 'SYSTEM0100000005'

UPDATE IW_COLUMN_TYPE 
SET SEQUENCE = 7 
WHERE IW_COLUMN_TYPE_KEY = 'SYSTEM010000000S'

UPDATE IW_COLUMN_TYPE 
SET SEQUENCE = 8 
WHERE IW_COLUMN_TYPE_KEY = 'SYSTEM0100000006'


