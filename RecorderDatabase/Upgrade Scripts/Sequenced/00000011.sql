/****************************************************/
/* Ensure all System supplied data refreshed        */
/****************************************************/

DELETE FROM IW_Column_Type_Relationship WHERE System_Supplied_Data=1
DELETE FROM IW_Column_Type_Pattern WHERE System_Supplied_Data=1
DELETE FROM IW_Table_Rule_Output_Field WHERE System_Supplied_Data=1
DELETE FROM IW_Output_Field WHERE System_Supplied_Data=1
DELETE FROM IW_Table_Rule_Output_Field WHERE System_Supplied_Data=1
DELETE FROM IW_Table_Rule_Related_Field WHERE System_Supplied_Data=1
DELETE FROM IW_Table_Rule WHERE System_Supplied_Data=1
DELETE FROM IW_Column_Type_Match_Rule WHERE System_Supplied_Data=1
DELETE FROM IW_Column_Type WHERE System_Supplied_Data=1
DELETE FROM IW_Match_Rule WHERE System_Supplied_Data=1
DELETE FROM IW_Post_Processing_Procedure WHERE System_Supplied_Data=1

/****************************************************/
/* IW_Column_Type data                              */
/****************************************************/

-- Basic common types

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000000', 'TColumnType', 'Location', 0, 1, 0, Null, 'TRequiredTextParser', 100, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'TColumnType', 'Grid Reference', 0, 1, 1, Null, 'TSpatialRefParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'TColumnType', 'Date', 0, 1, 2, Null, 'TDateParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'TColumnType', 'Species Name', 1, 1, 3, Null, 'TRequiredTextParser', 100, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 'TColumnType', 'Observer(s)', 0, 1, 4, Null, 'TObserverParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'TColumnType', 'Determiner Name', 0, 1, 5, Null, 'TRequiredTextParser', 100, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000S', 'TColumnType', 'Abundance Data', 0, 1, 6, Null, 'TAbundanceDataParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'TColumnType', 'Comment', 0, 1, 7, 'text', Null, NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

-- Advanced common types

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'TColumnType', 'Altitude', 0, 0, Null, Null, 'TAltitudeParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'TColumnType', 'Associated Species', 0, 0, Null, Null, 'TTextParser', 100, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'TColumnType', 'Association Type', 0, 0, Null, Null, 'TTextParser', 100, 'RELATIONSHIP_TYPE', 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'TColumnType', 'Biotope', 0, 0, Null, Null, 'TTextParser', 100, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'TColumnType', 'BRC Source', 0, 0, Null, Null, 'TBRCSourceParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'TColumnType', 'Confidential', 0, 0, Null, Null, 'TBooleanParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'TColumnType', 'Determination Reference', 0, 0, Null, Null, 'TTextParser', 500, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 'TColumnType', 'MapMate Key', 0, 0, Null, Null, 'TMapMateKeyParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'TColumnType', 'Provenance', 0, 0, Null, Null, 'TTextParser', 16, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'TColumnType', 'Publication Reference', 0, 0, Null, Null, 'TTextParser', 500, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000H', 'TColumnType', 'Record ID', 0, 0, Null, Null, 'TRecordIDParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000I', 'TColumnType', 'Record Type', 1, 0, Null, Null, 'TRequiredTextParser', 100, 'RECORD_TYPE', 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000J', 'TColumnType', 'Sampling Method', 1, 0, Null, Null, 'TRequiredTextParser', 100, 'SAMPLE_TYPE', 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000K', 'TColumnType', 'Site ID', 0, 0, Null, Null, 'TSiteIDParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000L', 'TColumnType', 'Specimen Comment', 0, 0, Null, Null, Null, NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000M', 'TColumnType', 'Specimen Number', 0, 0, Null, Null, 'TTextParser', 10, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000N', 'TColumnType', 'Specimen Type', 0, 0, Null, Null, 'TTextParser', 100, 'SPECIMEN_TYPE', 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000O', 'TColumnType', 'Substrate', 0, 0, Null, Null, 'TTextParser', 100, 'SUBSTRATE', 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000P', 'TColumnType', 'Surveyor''s Ref', 0, 0, Null, Null, 'TTextParser', 30, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000Q', 'TTaxonOccurrenceDataColumnType', 'Taxon Occurrence Data', 0, 0, Null, Null, 'TTaxonDataParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type (IW_Column_Type_Key, Class_Name, Item_Name, Required, Commonly_Used, Sequence, Field_Type, Parser_Class_Name, Maximum_Length, Term_List_Table, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000R', 'TColumnType', 'Vice-County Number', 0, 0, Null, Null, 'TViceCountyNumberParser', NULL, NULL, 'NBNSYS0000000004', GetDate(), 1)


/****************************************************/
/* IW_Column_Type_Relationship data                 */
/****************************************************/

-- Relationship type values
-- 0 = Column 1 requires Column 2 to be present
-- 1 = Columns conflict
-- 2 = Field level dependancy, column 2 required
-- 3 = Field level dependancy, column 2 not required

-- Location requires Grid Reference
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM0100000000', 'SYSTEM0100000001', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Associated species fields values require Association Type field values if column present
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM0100000008', 'SYSTEM0100000009', 3, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Associated type requires Associated species
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM0100000009', 'SYSTEM0100000008', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- BRC Source conflicts with Record Type
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000B', 'SYSTEM010000000I', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Record Type conflicts with BRC Source
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000I', 'SYSTEM010000000B', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- MapMate Key conflicts with Record ID
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000E', 'SYSTEM010000000H', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- MapMate Key conflicts with Site ID
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000E', 'SYSTEM010000000K', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Record ID conflicts with MapMate key
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000H', 'SYSTEM010000000E', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Site ID conflicts with MapMate Key
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000K', 'SYSTEM010000000E', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Record ID depends on Site ID
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000H', 'SYSTEM010000000K', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Site ID depends on Record ID
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000K', 'SYSTEM010000000H', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Specimen Comment field values depends on Specimen Number field values
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000L', 'SYSTEM010000000M', 2, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Specimen Type field values depends on Specimen Number field values
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000N', 'SYSTEM010000000M', 2, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Specimen Number field values depends on Specimen Type field values
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000M', 'SYSTEM010000000N', 3, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Vice-county Number depends on Grid Reference
INSERT INTO IW_Column_Type_Relationship
VALUES ('SYSTEM010000000R', 'SYSTEM0100000001', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)


/****************************************************/
/* IW_Column_Type_Pattern data                      */
/****************************************************/

-- Location
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000000', 'site%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000000', 'loc%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000000', 'place%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000000', 'site%id', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Grid reference
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000001', 'grid%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000001', 'spatial%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Date
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000002', '%date', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000002', 'date%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000002', 'det%date', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Species Name
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000003', 'sp%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000003', 'spec%comm%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000003', 'spec%num%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000003', 'spec%no%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000003', 'spec%ty%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000003', 'tax%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000003', 'name%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000003', 'common%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000003', 'spatial%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000003', 'specim%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Observers
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000004', 'obs%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000004', 'recorder%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000004', 'coll%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000004', 'surv%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000004', 'surv%ref%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Determiner Name
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000005', 'det%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000005', 'det%date%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000005', 'det%ref%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000005', 'det%source%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000005', 'det%work%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Abundance Data
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000S', 'sex%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000S', 'stage%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000S', 'mf%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000S', 'count%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000S', 'num%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000S', 'abund%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Comment
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000006', 'comm%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000006', 'common%', 1, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Altitude
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000007', 'alt%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000007', 'height%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000007', 'ht%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Associated Species
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000008', 'assoc%sp%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000008', 'assoc%tax%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Association Type
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000009', 'assoc%typ%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000009', 'relat%typ%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Biotope
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000A', 'biotope%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000A', 'habitat%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- BRC Source
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000B', 'brc%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Confidential
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000C', 'confid%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Determination Reference
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000D', 'det%ref%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000D', 'det%source%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000D', 'det%work%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- MapMate Key
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000E', 'mm%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000E', 'mapmate%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Provenance
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000F', 'prov%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Publication Reference
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000G', 'pub%ref%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000G', 'pub%source%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Record ID
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000H', 'rec%id', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Record Type
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000I', 'record%ty%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Sampling Method
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000J', 'samp%met%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000J', 'samp%ty%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Site ID
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000K', 'site%id', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Specimen Comment
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000L', 'spec%comm%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Specimen Number
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000M', 'spec%num%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000M', 'spec%no%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Specimen Type
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000N', 'spec%ty%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Substrate
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000O', 'subs%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Surveyor's Ref
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000P', 'surv%ref%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

-- Vice-county number
INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000R', 'vc%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM010000000R', 'vice%', 0, 'NBNSYS0000000004', GetDate(), Null, Null, 1)

/****************************************************/
/* IW_Match_Rule data     	                    */
/****************************************************/

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key, 
	[Sequence], 
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure, 
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date, 
	System_Supplied_Data)
VALUES (
	'SYSTEM0100000000',
	0,
	'Names',
	0,
	NULL, 
	'usp_IWMatchRemembered_Names',
	'usp_IWMatch_Names',
	'usp_IWMatchRecord_Names',
	'usp_IWMatchNewEntry_Name',
	0,
	'usp_IWMatchSet_Name',
	'CREATE TABLE #Names(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(60) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)',
	'usp_Name_Get',
	0,
	NULL,
	NULL,
	'NBNSYS0000000004',
	GetDate(),
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key, 
	[Sequence], 
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure, 
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES ('SYSTEM0100000001',
	1, 
	'Species', 
	0,
	NULL, 
	'usp_IWMatchRemembered_Species', 
	'usp_IWMatch_Species', 
	'usp_IWMatchRecord_Species', 
	NULL, 
	1, 
	'usp_IWMatchSet_Species', 
	'CREATE TABLE #Species(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0,
 [Order] VARCHAR(60) COLLATE SQL_Latin1_General_CP1_CI_AS,
 CheckList VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Checklist_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Species_Name VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS
)', 
	'usp_SpeciesName_Get', 
	5, 'usp_TaxonLists_Select ', 
	NULL, 
	'NBNSYS0000000004',
	GetDate(), 
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key, 
	[Sequence], 
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure, 
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES (
	'SYSTEM0100000002',
	3,
	'Biotopes',
	0,
	NULL,
	'usp_IWMatchRemembered_Biotopes',
	'usp_IWMatch_Biotopes',
	'usp_IWMatchRecord_Biotopes',
	NULL,
	1,
	'usp_IWMatchSet_Biotope',
	'CREATE TABLE #Biotopes(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0,
 Classification VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Classification_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
)', 
	'usp_BiotopeName_Get',
	4,
	'usp_BiotopeClassifications_Select',
	NULL,
	'NBNSYS0000000004',
	GetDate(), 
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key, 
	[Sequence], 
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure, 
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES ('SYSTEM0100000003',
	4,
	'Locations',
	0, 
	'
	DECLARE @GridRefs varchar(100), @LocationName varchar(100)
	DECLARE @CRLF char(2)
	SET @CRLF=CHAR(13)+CHAR(10)
	DECLARE curLocations CURSOR FOR
		SELECT Import_Value FROM #Locations    
	OPEN curLocations
	FETCH NEXT FROM curLocations INTO @LocationName  
	WHILE @@Fetch_Status = 0 BEGIN
		SET @GridRefs = ''''
		SELECT @GridRefs = CASE 
			WHEN CharIndex(SYSTEM0100000001_Spatial_Ref, @GridRefs) = 0 THEN @GridRefs + @CRLF + SYSTEM0100000001_Spatial_Ref 
			ELSE @GridRefs END
		FROM #Master
		WHERE SYSTEM0100000000_Data = @LocationName     /* Remove superfluous leading carriage return. */ 
	
		IF SubString(@GridRefs, 1, 2) = @CRLF
			SET @GridRefs = SubString(@GridRefs, 3, Len(@GridRefs))     -- Update field in match table.
	
		UPDATE #Locations
			SET Import_Grid_Reference = @GridRefs
		WHERE Import_Value = @LocationName
		
		FETCH NEXT FROM curLocations INTO @LocationName
	END
	CLOSE curLocations
	DEALLOCATE curLocations', 
	'usp_IWMatchRemembered_Locations',
	'usp_IWMatch_Locations',
	'usp_IWMatchRecord_Locations',
	'usp_IWMatchNewEntry_Location',
	0,
	'usp_IWMatchSet_Location',
	'CREATE TABLE #Locations(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Import_Grid_Reference VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Count INT,
 Match_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0,
 Spatial_Ref VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Spatial_Ref_System VARCHAR(4) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Lat FLOAT,
 Long FLOAT,
 Spatial_Ref_Qualifier VARCHAR(20) COLLATE SQL_Latin1_General_CP1_CI_AS
)',
	'usp_LocationName_Get',
	3,
	NULL,
	NULL,
	'NBNSYS0000000004',
	GetDate(),
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key, 
	[Sequence], 
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure, 
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES ('SYSTEM0100000004',
	5,
	'References',
	0,
	NULL,
	'usp_IWMatchRemembered_References',
	'usp_IWMatch_References',
	'usp_IWMatchRecord_References',
	NULL,
	0,
	'usp_IWMatchSet_Reference',
	'CREATE TABLE #References(
 Import_Value VARCHAR(500) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(500) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)',
	'usp_ReferenceName_Get',
	100,
	NULL,
	NULL,
	'NBNSYS0000000004',
	GetDate(),
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key, 
	[Sequence], 
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure, 
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES ('SYSTEM0100000005',
	6,
	'AbundanceQualifiers',
	1,
	NULL,
	'usp_IWMatchRemembered_AbundanceQualifiers',
	'usp_IWMatch_AbundanceQualifiers',
	'usp_IWMatchRecord_AbundanceQualifiers',
	'usp_IWMatchNewEntry_AbundanceQualifier',
	0,
	'usp_IWMatchSet_AbundanceQualifier',
	'CREATE TABLE #AbundanceQualifiers(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)',
	NULL,
	-1,
	NULL,
	'usp_AbundanceQualifiers_Select',
	'NBNSYS0000000004',
	GetDate(),
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key, 
	[Sequence], 
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure, 
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES ('SYSTEM0100000006',
	8,
	'Substrates',
	1,
	NULL, 
	'usp_IWMatchRemembered_Substrates',
	'usp_IWMatch_Substrates',
	'usp_IWMatchRecord_Substrates',
	'usp_IWMatchNewEntry_Substrate',
	0,
	'usp_IWMatchSet_Substrate', 
	'CREATE TABLE #Substrates(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)',
	NULL,
	-1,
	NULL,
	'usp_Substrates_Select',
	'NBNSYS0000000004',
	GetDate(),
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key, 
	[Sequence], 
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure, 
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES ('SYSTEM0100000007',
	9,
	'RecordTypes',
	1,
	NULL,
	'usp_IWMatchRemembered_RecordTypes',
	'usp_IWMatch_RecordTypes',
	'usp_IWMatchRecord_RecordTypes',
	'usp_IWMatchNewEntry_RecordType',
	0,
	'usp_IWMatchSet_RecordType', 
	'CREATE TABLE #RecordTypes(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)',
	NULL,
	-1,
	NULL,
	'usp_RecordTypes_Select ',
	'NBNSYS0000000004',
	GetDate(),
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key, 
	[Sequence], 
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure, 
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES ('SYSTEM0100000008',
	10,
	'AssociationTypes',
	1,
	NULL,
	'usp_IWMatchRemembered_AssociationTypes',
	'usp_IWMatch_AssociationTypes',
	'usp_IWMatchRecord_AssociationTypes',
	'usp_IWMatchNewEntry_AssociationType',
	0,
	'usp_IWMatchSet_AssociationType',
	'CREATE TABLE #AssociationTypes(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)', 
	NULL, 
	-1,
	NULL,
	'usp_RelationshipTypes_Select',
	'NBNSYS0000000004',
	GetDate(),
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key, 
	[Sequence], 
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure, 
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES ('SYSTEM0100000009',
	11,
	'SpecimenTypes',
	1,
	NULL,
	'usp_IWMatchRemembered_SpecimenTypes',
	'usp_IWMatch_SpecimenTypes',
	'usp_IWMatchRecord_SpecimenTypes',
	'usp_IWMatchNewEntry_SpecimenType',
	0,
	'usp_IWMatchSet_SpecimenType',
	'CREATE TABLE #SpecimenTypes(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0
)', 
	NULL,
	-1,
	NULL,
	'usp_SpecimenTypes_Select',
	'NBNSYS0000000004',
	GetDate(),
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key,
	[Sequence],
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure,
	Table_Create_SQL, 
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES ('SYSTEM010000000A',
	2,
	'AssociatedSpecies',
	0,
	NULL,
	'usp_IWMatchRemembered_AssociatedSpecies',
	'usp_IWMatch_AssociatedSpecies',
	'usp_IWMatchRecord_AssociatedSpecies',
	NULL,
	1,
	'usp_IWMatchSet_AssociatedSpecies',
	'CREATE TABLE #AssociatedSpecies(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0,
 [Order] VARCHAR(60) COLLATE SQL_Latin1_General_CP1_CI_AS,
 CheckList VARCHAR(200) COLLATE SQL_Latin1_General_CP1_CI_AS,
 CheckList_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Species_Name VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS
)',
	'usp_SpeciesName_Get',
	5,
	'usp_TaxonLists_Select',
	NULL,
	'NBNSYS0000000004',
	GetDate(),
	1)

INSERT INTO IW_Match_Rule (
	IW_Match_Rule_Key,
	[Sequence],
	Item_Name,
	Control_Type,
	Imported_Data_Insert_SQL,
	Remembered_Matches_Procedure,
	Match_Procedure,
	Record_Matches_Procedure,
	New_Entry_Procedure,
	Requires_Checklist,
	Set_Match_Procedure,
	Table_Create_SQL,
	Key_To_Caption_Procedure,
	Search_Type,
	Checklists_Select_Procedure,
	Termlist_Select_Procedure,
	Entered_By,
	Entry_Date,
	System_Supplied_Data)
VALUES  ('SYSTEM010000000B',
	7,
	'SampleTypes',
	1,
	NULL,
	'usp_IWMatchRemembered_SampleTypes',
	'usp_IWMatch_SampleTypes',
	'usp_IWMatchRecord_SampleTypes',
	'usp_IWMatchNewEntry_SampleType',
	0,
	'usp_IWMatchSet_SampleType',
	'CREATE TABLE #SampleTypes(
 Import_Value VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
 Match_Count INT,
 Match_Value VARCHAR(40) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Match_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
 Manual_Match BIT DEFAULT 0,
 Remembered BIT DEFAULT 0)',
	NULL,
	-1,
	NULL,
	'usp_SampleTypes_Select',
	'NBNSYS0000000004',
	GetDate(),
	1)

/****************************************************/
/* IW_Column_Type_Match_Rule data                   */
/****************************************************/

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000000', 'SYSTEM0100000003', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'SYSTEM0100000001', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 'SYSTEM0100000000', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'SYSTEM0100000000', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM010000000A', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM0100000008', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'SYSTEM0100000002', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'SYSTEM0100000004', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000I', 'SYSTEM0100000007', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000N', 'SYSTEM0100000009', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000O', 'SYSTEM0100000006', 0, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000S', 'SYSTEM0100000005', 1, 'NBNSYS0000000004', GetDate(), 1)

INSERT INTO IW_Column_Type_Match_Rule (IW_Column_Type_Key, IW_Match_Rule_Key, Field_Index, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000J', 'SYSTEM010000000B', 0, 'NBNSYS0000000004', GetDate(), 1)



DECLARE @user_id CHAR(16),
        @date    DATETIME

SELECT  @user_id    =   'NBNSYS0000000004',
        @date       =   '20040610'

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'Survey_Event_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'Spatial_Ref', 'VARCHAR(40)', NULL, NULL, 'TSurveyEventSpatialReferenceFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'Spatial_Ref_System', 'VARCHAR(4)', NULL, NULL, 'TSurveyEventSpatialReferenceFieldGenerator', 1, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 'Lat', 'FLOAT', NULL, NULL, 'TSurveyEventSpatialReferenceFieldGenerator', 2, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'Long', 'FLOAT', NULL, NULL, 'TSurveyEventSpatialReferenceFieldGenerator', 3, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'Spatial_Ref_Qualifier', 'VARCHAR(20)', NULL, NULL, 'TSurveyEventSpatialReferenceFieldGenerator', 4, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'Vague_Date_Start', 'INT', NULL, NULL, 'TDateFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'Vague_Date_End', 'INT', NULL, NULL, 'TDateFieldGenerator', 1, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'Vague_Date_Type', 'VARCHAR(2)', NULL, NULL, 'TDateFieldGenerator', 2, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'Location_Key', 'CHAR(16)', NULL, NULL, 'TLocationFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'Location_Name', 'VARCHAR(100)', NULL, NULL, 'TLocationFieldGenerator', 1, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'Survey_Key', 'CHAR(16)', NULL, NULL, 'TSurveyKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'Custodian', 'CHAR(8)', NULL, NULL, 'TCustodianFieldGenerator', 0, NULL, @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 'Sample_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'Spatial_Ref', 'VARCHAR(40)', NULL, NULL, 'TSpatialReferenceFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'Spatial_Ref_System', 'VARCHAR(4)', NULL, NULL, 'TSpatialReferenceFieldGenerator', 1, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000H', 'Lat', 'FLOAT', NULL, NULL, 'TSpatialReferenceFieldGenerator', 2, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000I', 'Long', 'FLOAT', NULL, NULL, 'TSpatialReferenceFieldGenerator', 3, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000J', 'Spatial_Ref_Qualifier', 'VARCHAR(20)', NULL, NULL, 'TSpatialReferenceFieldGenerator', 4, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000K', 'Sample_Type_Key', 'CHAR(16)', 'SYSTEM010000000J', 'data', NULL, NULL, NULL, @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000L', 'SE_Recorder_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000M', 'Survey_Event_Key', 'CHAR(16)', NULL, 'Survey_Event_Key', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000N', 'Name_Key', 'CHAR(16)', 'SYSTEM0100000004', 'name', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000O', 'Recorder_Role_Key', 'CHAR(16)', NULL, NULL, NULL, NULL, '''NBNSYS0000000002''', @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000P', 'Sample_Key', 'CHAR(16)', NULL, 'Sample_Key', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000Q', 'SE_Recorder_Key', 'CHAR(16)', NULL, 'SE_Recorder_Key', NULL, NULL, NULL, @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000R', 'Sample_Data_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000S', 'Data', 'VARCHAR(10)', 'SYSTEM0100000007', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000T', 'Accuracy', 'VARCHAR(10)', 'SYSTEM0100000007', 'accuracy', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000U', 'Measurement_Qualifier_Key', 'CHAR(16)', NULL, NULL, NULL, NULL, '''NBNSYS0000000003''', @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000V', 'Measurement_Unit_Key', 'CHAR(16)', NULL, NULL, NULL, NULL, '''NBNSYS0000000006''', @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000W', 'Taxon_Occurrence_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000X', 'Comment', 'TEXT', 'SYSTEM0100000006', 'data', NULL, NULL, NULL, @user_id, @date, 1)
/* Zero_Abundance is always set to 0 during table rule processing; the true
   value is set using special case code during post-processing.
 */
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000Y', 'Zero_Abundance', 'BIT', NULL, NULL, NULL, NULL, '0', @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000Z', 'Checked', 'BIT', NULL, NULL, NULL, NULL, '1', @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000010', 'Checked_By', 'CHAR(16)', NULL, NULL, 'TCurrentStatusFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000011', 'Checked_Date', 'DATETIME', NULL, NULL, 'TCurrentStatusFieldGenerator', 1, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000012', 'Confidential', 'BIT', 'SYSTEM010000000C', 'boolean', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000013', 'Surveyors_Ref', 'VARCHAR(30)', 'SYSTEM010000000P', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000014', 'Provenance', 'VARCHAR(16)', 'SYSTEM010000000F', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000015', 'Substrate_Key', 'CHAR(16)', 'SYSTEM010000000O', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000016', 'Record_Type_Key', 'CHAR(16)', NULL, NULL, 'TRecordTypeKeyFieldGenerator', 0, NULL, @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000017', 'Taxon_Determination_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000018', 'Taxon_List_Item_Key', 'CHAR(16)', 'SYSTEM0100000003', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000019', 'Taxon_List_Item_Key', 'CHAR(16)', 'SYSTEM0100000008', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001A', 'Taxon_Occurrence_Key', 'CHAR(16)', NULL, 'Taxon_Occurrence_Key', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001B', 'Preferred', 'BIT', NULL, NULL, NULL, NULL, '1', @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001C', 'Determiner', 'CHAR(16)', NULL, NULL, 'TDeterminerFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001D', 'Determination_Type_Key', 'CHAR(16)', NULL, NULL, NULL, NULL, '''NBNSYS0000000004''', @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001E', 'Determiner_Role_Key', 'CHAR(16)', NULL, NULL, NULL, NULL, '''NBNSYS0000000003''', @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001F', 'Source_Key', 'CHAR(16)', 'SYSTEM010000000D', 'data', NULL, NULL, NULL, @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001G', 'Taxon_Occurrence_Data_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001H', 'Data', 'VARCHAR(20)', 'SYSTEM010000000S', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001I', 'Accuracy', 'VARCHAR(10)', 'SYSTEM010000000S', 'accuracy', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001J', 'Measurement_Qualifier_Key', 'CHAR(16)', 'SYSTEM010000000S', 'qualifier', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001K', 'Measurement_Unit_Key', 'CHAR(16)', NULL, NULL, NULL, NULL, '''NBNSYS0000000009''', @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001L', 'Measurement_Unit_Key', 'CHAR(16)', 'SYSTEM010000000Q', 'measurement_unit_key', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001M', 'Data', 'VARCHAR(20)', 'SYSTEM010000000Q', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001N', 'Accuracy', 'VARCHAR(10)', 'SYSTEM010000000Q', 'accuracy', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001O', 'Measurement_Qualifier_Key', 'CHAR(16)', 'SYSTEM010000000Q', 'measurement_qualifier_key', NULL, NULL, NULL, @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001P', 'Biotope_Occurrence_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001Q', 'Biotope_Determination_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001R', 'Biotope_List_Item_Key', 'CHAR(16)', 'SYSTEM010000000A', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001S', 'Biotope_Occurrence_Key', 'CHAR(16)', NULL, 'Biotope_Occurrence_Key', NULL, NULL, NULL, @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001T', 'Source_Link_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001U', 'Source_Key', 'CHAR(16)', 'SYSTEM010000000G', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001V', 'Original', 'BIT', NULL, NULL, NULL, NULL, '1', @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001W', 'Taxon_Occurrence_Relation_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001X', 'Taxon_Occurrence_Key_1', 'CHAR(16)', NULL, 'Taxon_Occurrence_Key', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001Y', 'Taxon_Occurrence_Key_2', 'CHAR(16)', NULL, NULL, 'TAssociatedOccurenceKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000001Z', 'Relationship_Type_Key', 'CHAR(16)', 'SYSTEM0100000009', 'data', NULL, NULL, NULL, @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000020', 'Specimen_Key', 'CHAR(16)', NULL, NULL, 'TKeyFieldGenerator', 0, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000021', 'Number', 'VARCHAR(10)', 'SYSTEM010000000M', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000022', 'Comment', 'TEXT', 'SYSTEM010000000L', 'data', NULL, NULL, NULL, @user_id, @date, 1)
INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000023', 'Specimen_Type_Key', 'CHAR(16)', 'SYSTEM010000000N', 'data', NULL, NULL, NULL, @user_id, @date, 1)

INSERT INTO IW_Output_Field (IW_Output_Field_Key, Name, Data_Type, IW_Column_Type_Key, Source_Field_Name, Generating_Class_Name, Generator_Field_Index, Literal_Value, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000024', 'Entered_By', 'CHAR(16)', NULL, NULL, 'TCurrentStatusFieldGenerator', 0, NULL, @user_id, @date, 1)


/* -----------------------------------------------------------------------------
   Table rules
 */
INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 0, 'Survey_Event', NULL, @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES      ('SYSTEM0100000001', 'SYSTEM0100000001', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES      ('SYSTEM0100000001', 'SYSTEM0100000004', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES      ('SYSTEM0100000001', 'SYSTEM0100000002', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES      ('SYSTEM0100000001', 'SYSTEM010000000R', 1, @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM0100000001', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM0100000002', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM0100000003', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM0100000004', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM0100000005', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM0100000006', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM0100000007', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM0100000008', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM0100000009', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM010000000A', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM010000000B', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM010000000C', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 2, 'Sample', NULL, @user_id, @date, 1)

/* this is actually performed in 000000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'Survey_Event', 1, @user_id, @date, 1)
*/
INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES      ('SYSTEM0100000002', 'SYSTEM0100000007', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES      ('SYSTEM0100000002', 'SYSTEM010000000A', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES      ('SYSTEM0100000002', 'SYSTEM010000000J', 1, @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000E', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM0100000007', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM0100000008', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM0100000009', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000F', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000G', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000H', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000I', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000J', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000A', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000B', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000K', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000M', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 1, 'Survey_Event_Recorder', NULL, @user_id, @date, 1)

/* this is actually performed in 000000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'Survey_Event', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'CT_SYSTEM0100000004', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'SYSTEM010000000L', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'SYSTEM010000000M', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'SYSTEM010000000N', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'SYSTEM010000000O', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000003', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 3, 'Sample_Recorder', NULL, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 'Sample', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 'Survey_Event_Recorder', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 'SYSTEM010000000P', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 'SYSTEM010000000Q', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000004', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 4, 'Sample_Data',
        '#master.SYSTEM0100000007_data <> ''''',
        @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'SYSTEM0100000007', 2, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'Sample', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'SYSTEM010000000R', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'SYSTEM010000000S', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'SYSTEM010000000T', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'SYSTEM010000000U', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'SYSTEM010000000V', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'SYSTEM010000000P', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000005', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 5, 'Taxon_Occurrence', NULL, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'Sample', 2, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM010000000W', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM010000000X', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM010000000Y', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM010000000Z', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM0100000010', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM0100000011', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM0100000012', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM0100000013', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM0100000014', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM010000000P', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM0100000015', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM0100000016', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000006', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 6, 'Taxon_Determination', NULL, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'Taxon_Occurrence', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM0100000017', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM0100000018', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM010000001A', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM0100000007', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM0100000008', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM0100000009', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM010000001B', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM010000001C', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM010000001D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM010000001E', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM010000001F', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000007', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 10, 'Taxon_Occurrence',
        '#master.SYSTEM0100000008_data <> '''''
        + ' AND NOT EXISTS ('
        + ' SELECT 1 FROM #species AS s'
        + ' INNER JOIN #taxon_determination AS d ON d.Taxon_List_Item_Key = s.Match_Key'
        + ' INNER JOIN #taxon_occurrence AS o ON o.Taxon_Occurrence_Key = d.Taxon_Occurrence_Key'
        + ' INNER JOIN #RN_sample AS r ON r.Sample_Key = o.Sample_Key'
        + ' WHERE s.Import_Value = #master.SYSTEM0100000008_data'
        + ' AND r.Record_No = #master.Record_No)',-- i.e. same sample
        @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM0100000008', 2, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'Sample', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM010000000W', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM010000000X', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM010000000Y', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM010000000Z', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM0100000010', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM0100000011', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM0100000012', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM0100000013', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM0100000014', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM010000000P', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM0100000015', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM0100000016', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000008', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 11, 'Taxon_Determination',
        '#master.SYSTEM0100000008_data <> '''''
        + ' AND NOT EXISTS (SELECT 1'
        + ' FROM #taxon_determination AS d'
        + ' WHERE d.Taxon_Occurrence_Key = #taxon_occurrence.Taxon_Occurrence_Key)',
        @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM0100000008', 2, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'Taxon_Occurrence', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM0100000017', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM0100000019', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM010000001A', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM0100000007', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM0100000008', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM0100000009', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM010000001B', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM010000001C', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM010000001D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM010000001E', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM010000001F', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000009', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 8, 'Taxon_Occurrence_Data', NULL, @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES      ('SYSTEM010000000A', 'SYSTEM010000000Q', 2, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'Taxon_Occurrence', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'CT_SYSTEM010000000Q', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'SYSTEM010000001G', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'SYSTEM010000001M', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'SYSTEM010000001N', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'SYSTEM010000001O', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'SYSTEM010000001L', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'SYSTEM010000001A', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000A', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 9, 'Taxon_Occurrence_Data', NULL, @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'SYSTEM010000000S', 2, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'Taxon_Occurrence', 1, @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'CT_SYSTEM010000000S', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'SYSTEM010000001G', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'SYSTEM010000001H', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'SYSTEM010000001I', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'SYSTEM010000001J', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'SYSTEM010000001K', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'SYSTEM010000001A', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000B', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 12, 'Biotope_Occurrence',
        '#master.SYSTEM010000000A_data <> ''''',
        @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'SYSTEM010000000A', 2, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'Sample', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'SYSTEM010000001P', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'SYSTEM010000000Z', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'SYSTEM0100000010', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'SYSTEM0100000011', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'SYSTEM0100000013', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'SYSTEM010000000P', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000C', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 13, 'Biotope_Determination', NULL, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'Biotope_Occurrence', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM010000000A', 2, @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM010000001Q', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM010000001R', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM010000001S', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM0100000007', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM0100000008', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM0100000009', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM010000001B', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM010000001C', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM010000001D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM010000001E', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000D', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 14, 'Taxon_Occurrence_Sources',
        '#master.SYSTEM010000000G_data <> ''''',
        @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 'SYSTEM010000000G', 2, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 'Taxon_Occurrence', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 'SYSTEM010000001T', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 'SYSTEM010000001A', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 'SYSTEM010000001U', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 'SYSTEM010000001V', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000E', 'SYSTEM010000000D', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 15, 'Taxon_Occurrence_Relation', '#master.SYSTEM0100000008_data <> ''''', @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'SYSTEM0100000008', 2, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'Taxon_Occurrence', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'SYSTEM010000001W', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'SYSTEM010000001X', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'SYSTEM010000001Y', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'SYSTEM010000001Z', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000F', 'SYSTEM0100000024', @user_id, @date, 1)


INSERT INTO IW_Table_Rule (IW_Table_Rule_Key, Sequence, Table_Name, Filter_Expression, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 7, 'Specimen',
        '#master.SYSTEM010000000M_data <> ''''',
        @user_id, @date, 1)

INSERT INTO IW_Table_Rule_Related_Field (IW_Table_Rule_Key, IW_Column_Type_Key, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'SYSTEM010000000M', 2, @user_id, @date, 1)

/* this is actually performed in 00000017.sql
INSERT INTO IW_Table_Rule_Related_Table (IW_Table_Rule_Key, Table_Name, Relationship, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'Taxon_Occurrence', 1, @user_id, @date, 1)
*/

INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'SYSTEM0100000020', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'SYSTEM0100000021', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'SYSTEM0100000022', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'SYSTEM0100000023', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'SYSTEM010000000D', @user_id, @date, 1)
INSERT INTO IW_Table_Rule_Output_Field (IW_Table_Rule_Key, IW_Output_Field_Key, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM010000000G', 'SYSTEM0100000024', @user_id, @date, 1)

/* -----------------------------------------------------------------------------
   Post-processing
 */
INSERT INTO IW_Post_Processing_Procedure (IW_Post_Processing_Procedure_Key, Sequence, Required_Table_Name, Procedure_Name, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000001', 0, 'Taxon_Occurrence_Data', 'usp_ImportWizard_CalculateZeroAbundance', @user_id, @date, 1)
INSERT INTO IW_Post_Processing_Procedure (IW_Post_Processing_Procedure_Key, Sequence, Required_Table_Name, Procedure_Name, Entered_By, Entry_Date, System_Supplied_Data)
VALUES ('SYSTEM0100000002', 1, NULL, 'usp_ImportWizard_CalculateSurveyEventSpatialRef', @user_id, @date, 1)
