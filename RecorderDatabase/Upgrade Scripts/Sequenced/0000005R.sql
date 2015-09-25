--Queries built using SQL Express (2005)
--Created by SBW to patch a problem with the sample comment and survey event comment 
-- on the import wizard

DELETE FROM IW_Table_Rule_Related_Field WHERE IW_TABLE_RULE_KEY = 'SYSTEM0100000001' AND IW_COLUMN_TYPE_KEY = 'JNCCDEV500000005'

DELETE FROM IW_Table_Rule_Related_Field WHERE IW_TABLE_RULE_KEY = 'SYSTEM0100000002' AND IW_COLUMN_TYPE_KEY = 'JNCCDEV500000004'
