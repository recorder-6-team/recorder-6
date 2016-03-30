/****** Object:  View [dbo].[LC_TOCC_REF_TITLE]    Script Date: 12/02/2015 20:46:51 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[LC_TOCC_REF_TITLE]
AS

Select TOCC.Taxon_Occurrence_Key, dbo.ufn_RtfToPlainText(R.Title) As TITLE  From 
Taxon_Occurrence TOCC INNER JOIN TAXON_OCCURRENCE_SOURCES TOS ON
TOS.Taxon_Occurrence_key = TOCC.Taxon_Occurrence_Key 
INNER JOIN Reference R ON R.SOURCE_KEY = TOS.Source_Key  
UNION 
Select TOCC.Taxon_Occurrence_Key, dbo.ufn_RtfToPlainText(R.Title) From 
Taxon_Occurrence TOCC INNER JOIN Sample S ON S.sample_key =
TOCC.SAMPLE_KEY INNER JOIN SAMPLE_SOURCES SS 
ON SS.SAMPLE_KEY = S.Sample_Key  
INNER JOIN Reference R ON R.SOURCE_KEY = SS.Source_Key      

GO

GRANT SELECT ON [dbo].[LC_TOCC_REF_TITLE] TO PUBLIC

GO

Insert INTO Usable_Table (Usable_table_Key,Table_Name,Link_Table,Link,Apply_To,Join_Order)
Values('SRA0000400000A4 ', 'LC_TOCC_REF_TITLE','Taxon_Occurrence','LC_TOCC_REF_TITLE.Taxon_Occurrence_Key = Taxon_Occurrence.Taxon_Occurrence_Key',
'T',11)

GO

Insert INTO Usable_Field (Usable_Field_Key,Table_Name,Field_Name,Field_Description,Field_Type,Apply_To,
Selectable,Sortable,Filterable,Calculation_Sql)
Values('SRA00004000000A4','LC_TOCC_REF_TITLE','Title','Reference Title','TEXT','T',0,0,1,'LC_TOCC_REF_TITLE.TITLE')

