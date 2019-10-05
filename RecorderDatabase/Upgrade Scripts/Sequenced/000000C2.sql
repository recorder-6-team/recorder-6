/****** SQL Fixing Data To Long For Field for Cosnolidated Recorders in IW  ******/
UPDATE REPORT_FIELD SET FIELD_SIZE = 8000 WHERE REPORT_FIELD_KEY = 'LCA0002200000009'
