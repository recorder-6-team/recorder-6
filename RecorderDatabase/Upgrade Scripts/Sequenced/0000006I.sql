-- Scripts to fix the "EOleException -Problem: The text, ntext, and image data types cannot be
-- compared or sorted, except when using IS NULL or LIKE operator" with sample duration 
-- and sample start time column
-- TS, 10.03.2009, Recorder-D

--Sample Duration
update IW_COLUMN_TYPE set FIELD_TYPE = 'varchar (100)' where IW_COLUMN_TYPE_KEY = 'JNCCDEV500000002'
--Sample Start Time
update IW_COLUMN_TYPE set FIELD_TYPE = 'varchar (100)' where IW_COLUMN_TYPE_KEY = 'JNCCDEV500000003'
