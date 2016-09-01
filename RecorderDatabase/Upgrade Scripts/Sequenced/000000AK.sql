/****** Changes in Connection with Survey Tag Sorting ******/

Update Concept Set Sort_Code = 0 
FROM Concept INNER JOIN   Concept_Group
ON Concept_Group.Concept_Group_Key = Concept.Concept_Group_Key
Where Concept_Group.Item_Name = 'Tags' AND
Concept.Sort_Code is null


