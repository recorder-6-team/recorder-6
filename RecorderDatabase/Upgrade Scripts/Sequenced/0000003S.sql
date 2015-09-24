--Disconnect the Substrate field from any associated occurrence records created in the Import Wizard
DELETE FROM IW_Table_Rule_Output_Field 
WHERE IW_Table_Rule_Key='SYSTEM0100000008' 
AND IW_Output_Field_Key='SYSTEM0100000015'