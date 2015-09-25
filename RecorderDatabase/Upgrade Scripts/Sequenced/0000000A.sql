IF EXISTS (SELECT * FROM DBO.SYSINDEXES WHERE NAME = N'IX_Actual_Name' AND id = OBJECT_ID(N'[dbo].[Index_Taxon_Name]'))
DROP INDEX [dbo].[Index_Taxon_Name].[IX_Actual_Name]
GO

CREATE INDEX IX_Actual_Name ON Index_Taxon_Name(Actual_Name)