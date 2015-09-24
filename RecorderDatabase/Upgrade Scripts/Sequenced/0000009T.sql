/****** Object:  Table [dbo].[Custodian_Relationship]    Script Date: 02/05/2015 11:39:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[Custodian_Relationship](
	[Custodian_Relationship_Key] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS CONSTRAINT PK_CUstodian_relationship_Key PRIMARY KEY NOT NULL,
	[Master_Table] [varchar](50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[Master_Field] [varchar](50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[Link_Field] [varchar](50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[Detail_Table] [varchar](50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[Detail_Field] [varchar](50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[Action] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[Process_Order] [int] NOT NULL,
	[Entered_By] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[Entry_Date] [smalldatetime] NOT NULL)

GO
SET ANSI_PADDING OFF

GRANT SELECT ON dbo.Custodian_Relationship TO R2k_AddOnly, R2k_Administrator,
		R2k_FullEdit, R2k_ReadOnly, R2k_RecordCardsOnly

 TRUNCATE TABLE Custodian_relationship
 GO 
 INSERT INTO Custodian_relationship Values('SYSTEM0000000001','Taxon_Determination','Taxon_Determination_Key','Taxon_Occurrence_key','Taxon_Determination','Taxon_Occurrence_key','A',51, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000002','Taxon_Occurrence','Taxon_Occurrence_Key','Taxon_Occurrence_Key','Taxon_Occurrence','Taxon_Occurrence_key','A',52, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000003','Sample','Sample_Key','Sample_Key','Taxon_Occurrence','Taxon_Occurrence_Key','P',53, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000004','Biotope_Determination','Biotope_Determination_Key','Biotope_Occurrence_Key','Biotope_Determination','Biotope_Occurrence_Key','A',54, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000005','Biotope_Occurrence','Biotope_Occurrence_Key','Biotope_Occurrence_Key','Biotope_Occurrence','Biotope_Occurrence_Key','A',55, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000006','Sample','Sample_Key','Sample_Key','Biotope_Occurrence','Biotope_Occurrence_Key','P',56, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000007','Sample','Sample_Key','Sample_Key_1','Sample_Relation','Sample_Relation_Key','P',57, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000008','Sample','Sample_Key','Sample_Key_2','Sample_Relation','Sample_Relation_Key','P',58, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000009','Survey_Event','Survey_Event_Key','Survey_Event_Key','Sample','Sample_Key','P',59, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000010','Survey_Event_Recorder','SE_Recorder_key','Survey_Event_Key','Survey_Event','Survey_Event_Key','D',60, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000011','Survey','Survey_Key','Survey_Key','Survey_Event','Survey_Event_Key','P',61, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000012','Taxon_Occurrence_Data','Taxon_Occurrence_Data_Key','Taxon_Occurrence_Key','Taxon_Occurrence','Taxon_Occurrence_Key','D',63, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000013','Biotope_Occurrence_Data','Biotope_Occurrence_Data_Key','Biotope_Occurrence_Key','Biotope_Occurrence','Biotope_Occurrence_Key','D',62, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000014','Determination_Type','Determination_Type_Key','Determination_Type_Key','Taxon_Determination','Taxon_Determination_Key','P',64, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000015','Determiner_Role','Determiner_Role_Key','Determiner_Role_Key','Taxon_Determination','Taxon_Determination_Key','P',65, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000016','Determination_Type','Determination_Type_Key','Determination_Type_Key','Biotope_Determination','Biotope_Determination_Key','P',66, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000017','Determiner_Role','Determiner_Role_Key','Determiner_Role_Key','Biotope_Determination','Biotope_Determination_Key','P',67, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000018','Record_Type','Record_Type_key','Record_Type_Key','Taxon_Occurrence','Taxon_Occurrence_Key','P',68, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000019','Substrate','Substrate_Key','Substrate_Key','Taxon_Occurrence','Taxon_Occurrence_Key','P',69, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000020','Sample_Data','Sample_Data_Key','Sample_Data_Key','Sample','Sample_Key','D',70, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000021','Sample_Type','Sample_Type_Key','Sample_Type_Key','Sample','Sample_Key','P',71, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000022','Recorder_Role','Recorder_Role_Key','Recorder_Role_Key','Survey_Event_Recorder','SE_Recorder_Key','P',72, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000023','Survey_Status','Survey_Status_Key','Survey_Status_Key','Survey','Survey_Key','P',73, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000024','Survey_Media','Survey_Media_Key','Survey_Media_Key','Survey','Survey_Key','P',74, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000025','Survey_Type','Survey_Type_Key','Survey_Type_Key','Survey','Survey_Key','P',75, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000026','Survey_Event_Owner','Survey_Event_Owner_Key','Survey_Event_Key','Survey_Event','Survey_Event_Key','D',76, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000027','Survey_Event_Owner_Type','Survey_Event_Owner_Type_Key','Survey_Event_Owner_Type_Key','Survey_Event_Owner','Survey_Event_Owner_Key','P',77, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000028','Specimen','Specimen_Key','Taxon_Occurrence_Key','Taxon_Occurrence','Taxon_Occurrence_Key','D',78, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000029','Specimen_Type','Specimen_Type_Key','Specimen_Type_Key','Specimen','Specimen_Type_Key','P',79, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000050','Location','Location_Key','Location_Key','Sample','Sample_Key','P',80, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000051','Location','Location_Key','Location_Key','Survey_Event','Survey_Event_Key','P',100, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000052','Location_Name','Location_Name_Key','Location_Key','Location','Location_Key','D',101, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000053','Location_Data','Location_Data_Key','Location_Key','Location','Location_Key','D',102, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000054','Location_Boundary','Location_Boundary_Key','Location_Key','Location','Location_Key','D',103, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000055','Location_Type','Location_Type_Key','Location_Type_Key','Location','Location_Key','P',104, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000056','Location_Designation','Designation_Key','Location_Key','Location','Location_Key','D',105, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000057','Location_Use','Location_Use_Key','Location_Key','Location','Location_Key','D',106, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000058','Site_Status','Site_Status_Key','Site_Status_Key','Location_Designation','Designation_Key','P',107, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000059','Location_Feature','Location_Feature_Key','Location_Key','Location','Location_Key','D',108, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000060','Management_Aim','Management_Aim_Key','Location_Features_Key','Location_Features','Location_Features_Key','D',109, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000061','Potential_Threat','Potential_Threat_Key','Location_Features_Key','Location_Features','Location_Features_Key','D',110, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000062','Threat_Type','Threat_Type_Key','Potential_Threat_Key','Potential_Threat','Potential_Threat_Key','D',111, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000063','Location_Feature_Grading','Feature_Grading_Key','Location_Feature_Grading_Key','Location_Feature','Location_Feature_Key','P',112, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000064','Location_Feature_Type','Location_Feature_Type_Key','Location_Feature_Type_Key','Location_Feature_Grading','Feature_Grading_Key','P',113, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000067','Damage_Occurrence','Damage_Occurrence_Key','Damage_Occurrence_Key','Location_Features','Location_Features_Key','D',114, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000068','Grid_Square','Grid_Square_Key','Location_Key','Location','Location_Key','D',115, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000069','Land_Parcel','Land_Parcel_Key','Location_Key','Location','Location_Key','D',116, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000070','Location_Admin_Areas','Location_Admin_Areas_Key','Location_Key','Location','Location_Key','D',117, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000071','Location','Location_Key','Location_Key_1','Location_Relation','Location_Relation_Key','P',118, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000072','Location','Location_Key','Location_Key_2','Location_Relation','Location_Relation_Key','P',119, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000073','Location_Relation','Location_Relation_Key','Location_Key_1','Location','Location_Key','D',120, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000074','Tenure','Tenure_Key','Location_Key','Location','Location_Key','D',121, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000075','Tenure_Type','Tenure_Type_Key','Tenure_Type_Key','Tenure_Type','Tenure_Type_Key','D',122, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000100','Name','Name_Key','Name_Key','Survey_Event_Recorder','SE_Recorder_Key','P',123, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000101','Name','Name_Key','Determiner','Taxon_Determination','Taxon_Determination_Key','P',124, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000102','Name','Name_Key','Determiner','Biotope_Determination','Biotope_Determination_Key','P',125, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000103','Name','Name_Key','Name_Key','Survey_Event_Owner','Survey_Event_Owner_Key','P',126, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000105','Name','Name_Key','Run_By','Survey','Survey_Key','P',127, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000106','Name','Name_Key','Name_Key','[User]','Name_Key','P',128, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000109','Name','Name_Key','Name_Key_1','Communication','Communication_Key','P',131, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000110','Name','Name_Key','Name_Key_2','Communication','Communication_Key','P',132, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000150','Address','Address_Key','Name_Key','Name','Name_Key','D',150, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000151','Communication','Communication_Key','Name_Key_1','Name','Name_Key','D',151, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000152','Contact_Number','Contact_Number_Key','Name_Key','Name','Name_Key','D',152, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000153','Name','Name_Key','Name_Key_1','Name_Relation','Name_Relation_Key','P',153, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000154','Name','Name_Key','Name_Key_2','Name_Relation','Name_Relation_Key','P',154, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000155','Name_Relation','Name_Relation_Key','Name_Key_1','Name','Name_Key','D',155, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000156','Organisation_Type','Organisation_Type_Key','Organisation_Type_Key','Organisation','Organisation_Type_Key','P',156, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000157','Organisation_Department','Organisation_Department_Key','Name_key','Name','Name_Key','D',157, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000158','Name','Name_Key','Owned_By','Tenure','Tenure_Key','P',158, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000200','Biotope_Occurrence_Sources','Source_Link_Key','Biotope_Occurrence_Key','Biotope_Occurrence','Biotope_Occurrence_Key','D',200, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000201','Location_Feature_Sources','Source_Link_Key','Location_Features_Key','Location_Features','Location_Features_Key','D',201, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000202','Location_Sources','Source_Link_Key','Location_Key','Location','Location_Key','D',202, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000203','Sample_Sources','Source_Link_Key','Sample_Key','Sample','Sample_Key','D',203, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000204','Survey_Event_Sources','Source_Link_Key','Survey_Event_Key','Survey_Event','Survey_Event_Key','D',204, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000205','Taxon_Occurrence_Sources','Source_Link_Key','Taxon_Occurrence_Key','Taxon_Occurrence','Taxon_Occurrence_Key','D',205, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000206','Name_Sources','Source_Link_Key','Name_Key','Name','Name_Key','D',206, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000207','Survey_Sources','Source_Link_Key','Survey_Key','Survey','Survey_Key','D',207, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000300','Measurement_Qualifier','Measurement_Qualifier_Key','Measurement_Qualifier_Key','Sample_Data','Sample_Data_Key','P',300, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000301','Measurement_Unit','Measurement_Unit_Key','Measurement_Unit_Key','Sample_Data','Sample_Data_Key','P',301, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000302','Measurement_Type','Measurement_Type_Key','Measurement_Type_Key','Measurement_Qualifier','Measurement_Qualifier_Key','P',302, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000303','Measurement_Unit','Measurement_Unit_Key','Measurement_Unit_Key','Taxon_Occurrence_Data','Taxon_Occurrence_Data_Key','P',303, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000304','Measurement_Qualifier','Measurement_Qualifier_Key','Measurement_Qualifier_Key','Taxon_Occurrence_Data','Taxon_Occurrence_Data_Key','P',304, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000305','Measurement_Unit','Measurement_Unit_Key','Measurement_Unit_Key','Biotope_Occurrence_Data','Biotope_Occurrence_Data_Key','P',305, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000306','Measurement_Qualifier','Measurement_Qualifier_Key','Measurement_Qualifier_Key','Biotope_Occurrence_Data','Biotope_Occurrence_Data_Key','P',306, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000307','Measurement_Unit','Measurement_Unit_Key','Measurement_Unit_Key','Location_Data','Location_Data_Key','P',307, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000308','Measurement_Qualifier','Measurement_Qualifier_Key','Measurement_Qualifier_Key','Location_Data','Location_Data_Key','P',308, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000320','Measurement_Type','Measurement_Type_Key','Measurement_Type_Key','Measurement_Unit','Measurement_Unit_Key','P',320, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000400','Journal','Journal_Key','Journal_key','Reference','Journal_Key','A',400, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000402','Source','Source_Key','Source_Key','Source_File','Source_Key','P',401, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000403','Source','Source_Key','Source_Key','Reference','Source_Key','P',403, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000404','Source','Source_Key','Source_Key','Reference_Author','Source_Key','P',404, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000405','Source','Source_Key','Source_Key','Reference_Editor','Source_Key','P',405, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000406','Source','Source_Key','Source_Key','Reference_Number','Source_Key','P',406, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000421','Reference_Author','Author_Key','Source_Key','Source','Source_Key','D',421, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000422','Reference_Editor','Editor_Key','Source_Key','Source','Source_key','D',422, 'TESTDATA00000001', GetDate() )
 INSERT INTO Custodian_relationship Values('SYSTEM0000000423','Reference_Number','Number_Key','Source_Key','Source','Source_key','D',423, 'TESTDATA00000001', GetDate() )
 