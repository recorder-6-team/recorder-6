/*****Changes to deal with situation where Taxon_Occurrence_Key included in the spreadsheet ****/

DELETE FROM Import_Wizard_Log WHERE Date_Time_End IS NULL
GO
      
ALTER TABLE IMPORT_WIZARD_LOG  
ALTER COLUMN Date_Time_End smalldatetime NOT NULL;
GO  

ALTER TABLE IMPORT_WIZARD_LOG  
  DROP CONSTRAINT PK_IMPORT_WIZARD_LOG;   
GO

ALTER TABLE IMPORT_WIZARD_LOG  
ADD CONSTRAINT PK_IMPORT_WIZARD_LOG PRIMARY KEY (Taxon_Occurrence_Key_1,
    Taxon_Occurrence_Key_2,Date_Time_End)
    






