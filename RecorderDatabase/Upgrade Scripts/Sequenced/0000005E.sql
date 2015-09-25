--Queries built using SQL Express (2005)
--Add a number of columns to Import Wizard



--Adds Survey Event Weather to columns available in Import Wizard

-- remove any current entries
DELETE FROM IW_Table_Rule_Output_Field WHERE IW_Table_Rule_Key = 'SYSTEM0100000001' AND IW_Output_Field_Key ='JNCCDEV500000001';
DELETE from IW_Output_Field WHERE IW_Output_Field_Key = 'JNCCDEV500000001';
DELETE from IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'JNCCDEV500000001';
DELETE from IW_Column_Type WHERE IW_Column_Type_Key = 'JNCCDEV500000001';


INSERT INTO IW_Column_Type
VALUES ('JNCCDEV500000001',
'TColumnType',
'Survey Event Weather',
0,
0,
NULL,
'text',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'Mar 06 2008  1:0',
NULL,
NULL,
-1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('JNCCDEV500000001',
'%weath%',
0,
'NBNSYS0000000004',
'Mar 06 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Output_Field
VALUES ('JNCCDEV500000001',
'Survey_Event_Weather',
'TEXT',
'JNCCDEV500000001',
'data',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'2008-03-06 01:00:00',
NULL,
NULL,
1)

INSERT INTO IW_Table_Rule_Output_Field
VALUES ('SYSTEM0100000001',
'JNCCDEV500000001',
'NBNSYS0000000004',
'06/03/2008 01:00:00',
1)


-- Adds Sample Duration to columns available in Import Wizard

-- remove any current entries
DELETE FROM IW_Table_Rule_Output_Field WHERE IW_Table_Rule_Key = 'SYSTEM0100000002' AND IW_Output_Field_Key ='JNCCDEV500000002';
DELETE from IW_Output_Field WHERE IW_Output_Field_Key = 'JNCCDEV500000002';
DELETE from IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'JNCCDEV500000002';
DELETE from IW_Column_Type WHERE IW_Column_Type_Key = 'JNCCDEV500000002';

INSERT INTO IW_Column_Type
VALUES ('JNCCDEV500000002',
'TColumnType',
'Sample Duration',
0,
0,
NULL,
'text',
NULL,
'20',
NULL,
'NBNSYS0000000004',
'Mar 06 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('JNCCDEV500000002',
'%dura%',
0,
'NBNSYS0000000004',
'Mar 06 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Output_Field
VALUES ('JNCCDEV500000002',
'Duration',
'TEXT',
'JNCCDEV500000002',
'data',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'2008-03-06 01:00:00',
NULL,
NULL,
1)

INSERT INTO IW_Table_Rule_Output_Field
VALUES ('SYSTEM0100000002',
'JNCCDEV500000002',
'NBNSYS0000000004',
'06/03/2008 01:00:00',
1)



-- Adds Sample Comment to columns available in Import Wizard
-- remove any current entries
DELETE FROM IW_Table_Rule_Output_Field WHERE IW_Table_Rule_Key = 'SYSTEM0100000002' AND IW_Output_Field_Key ='JNCCDEV500000004';
DELETE from IW_Output_Field WHERE IW_Output_Field_Key = 'JNCCDEV500000004';
DELETE from IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'JNCCDEV500000004';
DELETE from IW_Column_Type WHERE IW_Column_Type_Key = 'JNCCDEV500000004';


INSERT INTO IW_Column_Type
VALUES ('JNCCDEV500000004',
'TColumnType',
'Sample Comment',
0,
0,
NULL,
'text',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'Mar 07 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('JNCCDEV500000004',
'Sam%Com%',
0,
'NBNSYS0000000004',
'Mar 07 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Output_Field
VALUES ('JNCCDEV500000004',
'Comment',
'TEXT',
'JNCCDEV500000004',
'data',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'2008-03-07 01:00:00',
NULL,
NULL,
1)

INSERT INTO IW_Table_Rule_Output_Field
VALUES ('SYSTEM0100000002',
'JNCCDEV500000004',
'NBNSYS0000000004',
'07/03/2008 01:00:00',
1)

-- Adds Survey Event Comment to columns available in Import Wizard
-- remove any current entries
DELETE FROM IW_Table_Rule_Output_Field WHERE IW_Table_Rule_Key = 'SYSTEM0100000001' AND IW_Output_Field_Key ='JNCCDEV500000005';
DELETE from IW_Output_Field WHERE IW_Output_Field_Key = 'JNCCDEV500000005';
DELETE from IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'JNCCDEV500000005';
DELETE from IW_Column_Type WHERE IW_Column_Type_Key = 'JNCCDEV500000005';


INSERT INTO IW_Column_Type
VALUES ('JNCCDEV500000005',
'TColumnType',
'Survey Event Comment',
0,
0,
NULL,
'text',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'Mar 07 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('JNCCDEV500000005',
'%Event%Com%',
0,
'NBNSYS0000000004',
'Mar 07 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Output_Field
VALUES ('JNCCDEV500000005',
'Comment',
'TEXT',
'JNCCDEV500000005',
'data',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'2008-03-07 01:00:00',
NULL,
NULL,
1)

INSERT INTO IW_Table_Rule_Output_Field
VALUES ('SYSTEM0100000001',
'JNCCDEV500000005',
'NBNSYS0000000004',
'07/03/2008 01:00:00',
1)

-- Updates existing column called 'Comment' to 'Taxon Occurrence Comment'
-- Column matching behaviour not changed
UPDATE IW_Column_Type
SET Item_Name = 'Taxon Occurrence Comment'
WHERE IW_Column_Type_Key = 'SYSTEM0100000006'

-- Adds Taxon Determination Comment to columns available in Import Wizard
-- remove any current entries
DELETE FROM IW_Table_Rule_Output_Field WHERE IW_Table_Rule_Key = 'SYSTEM0100000007' AND IW_Output_Field_Key ='JNCCDEV500000006';
DELETE from IW_Output_Field WHERE IW_Output_Field_Key = 'JNCCDEV500000006';
DELETE from IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'JNCCDEV500000006' AND pattern = 'Det%Com%';
DELETE from IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'SYSTEM0100000005' AND pattern = 'Det%Com%';
DELETE from IW_Column_Type WHERE IW_Column_Type_Key = 'JNCCDEV500000006';

INSERT INTO IW_Column_Type
VALUES ('JNCCDEV500000006',
'TColumnType',
'Taxon Determination Comment',
0,
0,
NULL,
'text',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'Mar 07 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('JNCCDEV500000006',
'Det%Com%',
0,
'NBNSYS0000000004',
'Mar 07 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('SYSTEM0100000005',
'Det%Com%',
1,
'NBNSYS0000000004',
'Mar 07 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Output_Field
VALUES ('JNCCDEV500000006',
'Comment',
'TEXT',
'JNCCDEV500000006',
'data',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'2008-03-07 01:00:00',
NULL,
NULL,
1)

INSERT INTO IW_Table_Rule_Output_Field
VALUES ('SYSTEM0100000007',
'JNCCDEV500000006',
'NBNSYS0000000004',
'07/03/2008 01:00:00',
1)


-- Adds Sample Start Time to columns available in Import Wizard
-- Has limitations are appropriate parser isn't available
-- This means will not flag up errors in import file - they just won't import at final stage (datatype mismatch error occurs).

-- remove any current entries
DELETE FROM IW_Table_Rule_Output_Field WHERE IW_Table_Rule_Key = 'SYSTEM0100000002' AND IW_Output_Field_Key ='JNCCDEV500000003';
DELETE from IW_Output_Field WHERE IW_Output_Field_Key = 'JNCCDEV500000003';
DELETE from IW_Column_Type_Pattern WHERE IW_Column_Type_Key = 'JNCCDEV500000003';
DELETE from IW_Column_Type WHERE IW_Column_Type_Key = 'JNCCDEV500000003';

INSERT INTO IW_Column_Type
VALUES ('JNCCDEV500000003',
'TColumnType',
'Sample Start Time',
0,
0,
NULL,
'text',
NULL,
'8',
NULL,
'NBNSYS0000000004',
'Mar 06 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Column_Type_Pattern
VALUES ('JNCCDEV500000003',
'%time%',
0,
'NBNSYS0000000004',
'Mar 06 2008  1:0',
NULL,
NULL,
1)

INSERT INTO IW_Output_Field
VALUES ('JNCCDEV500000003',
'Time',
'VARCHAR (8)',
'JNCCDEV500000003',
'data',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'2008-03-06 01:00:00',
NULL,
NULL,
1)

INSERT INTO IW_Table_Rule_Output_Field
VALUES ('SYSTEM0100000002',
'JNCCDEV500000003',
'NBNSYS0000000004',
'06/03/2008 01:00:00',
1)
