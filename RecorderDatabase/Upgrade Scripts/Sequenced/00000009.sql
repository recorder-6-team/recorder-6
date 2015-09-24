UPDATE Usable_Field 
SET Calculation_SQL='SELECT Preferred_Name AS ItemKey FROM Taxon_List_Item TLI, Taxon_Version TV, Taxon T WHERE TLI.Taxon_Version_Key = TV.Taxon_Version_Key AND T.Taxon_Key = TV.Taxon_Key AND T.Language <> ''La'' AND Item_Name'
WHERE Usable_Field_Key='ABABABAB00000095'
