UPDATE IW_Match_Rule 
SET Checklists_Select_Procedure = 'usp_TaxonDataEntryLists_Select'
WHERE Checklists_Select_Procedure = 'usp_TaxonLists_Select'