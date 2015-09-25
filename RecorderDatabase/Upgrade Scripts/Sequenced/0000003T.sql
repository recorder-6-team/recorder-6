--Updates 4 rows in the Usable_Field table that appear to have been truncated in some users databases 29-11-06

UPDATE Usable_Field 
SET Calculation_SQL='SELECT TXO.Taxon_Occurrence_Key AS ItemKey FROM Taxon T, Taxon_Version TV, Taxon_Occurrence TXO, Taxon_List_Item TLI, Taxon_Determination TD WHERE TLI.Taxon_List_Item_Key = TD.Taxon_List_Item_Key AND TXO.Taxon_Occurrence_Key = TD.Taxon_Occurrence_Key AND TV.Taxon_Version_Key = TLI.Taxon_Version_Key AND T.Taxon_Key = TV.Taxon_Key AND Preferred = True AND Item_Name'
WHERE Usable_Field_Key='ABABABAB00000086'

UPDATE Usable_Field 
SET Calculation_SQL='SELECT TXO.Taxon_Occurrence_Key AS ItemKey FROM Taxon_Occurrence TXO  JOIN Taxon_Determination TD ON TD.Taxon_Occurrence_Key = TXO.Taxon_Occurrence_Key  JOIN Individual I ON TD.Determiner = I.Name_Key   WHERE Surname'
WHERE Usable_Field_Key='ABABABAB00000087'

UPDATE Usable_Field 
SET Calculation_SQL= 'SELECT BO.Biotope_Occurrence_Key AS ItemKey FROM Biotope_Occurrence BO, Biotope B, Biotope_List_Item BLI, Biotope_Determination BD WHERE B.Biotope_Key = BLI.Biotope_Key AND BLI.Biotope_List_Item_Key = BD.Biotope_List_Item_Key AND BO.Biotope_Occurrence_Key = BD.Biotope_Occurrence_Key AND BD.Preferred = True AND Original_Code'
WHERE Usable_Field_Key='ABABABAB00000088'

UPDATE Usable_Field 
SET Calculation_SQL='SELECT BO.Biotope_Occurrence_Key AS ItemKey FROM Biotope_Occurrence BO, Biotope B, Biotope_List_Item BLI, Biotope_Determination BD WHERE B.Biotope_Key = BLI.Biotope_Key AND BLI.Biotope_List_Item_Key = BD.Biotope_List_Item_Key AND BO.Biotope_Occurrence_Key = BD.Biotope_Occurrence_Key AND BD.Preferred = True AND Short_Term'
WHERE Usable_Field_Key='ABABABAB00000089'

GO