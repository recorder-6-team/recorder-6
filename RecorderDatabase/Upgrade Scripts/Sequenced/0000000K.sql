
--  Update Attribute SQL for [Sample Month]

UPDATE Report_Attribute 
SET Attribute_SQL = '#REPORT_OUTPUT.[Sample Month] = CASE WHEN SAMPLE.VAGUE_DATE_TYPE IN (''U'', ''Y'', ''S'') THEN NULL ELSE dbo.FormatDatePart(SAMPLE.VAGUE_DATE_START, SAMPLE.VAGUE_DATE_END, SAMPLE.VAGUE_DATE_TYPE, 1) END'
WHERE Report_Attribute_Key = 'NBNSYS0000000047'
GO

--  Update Attribute SQL for [Sample Year]

UPDATE Report_Attribute
SET Attribute_SQL = '#REPORT_OUTPUT.[Sample Year] = CASE WHEN SAMPLE.VAGUE_DATE_TYPE IN (''U'', ''M'', ''S'') THEN NULL ELSE dbo.FormatDatePart(SAMPLE.VAGUE_DATE_START, SAMPLE.VAGUE_DATE_END, SAMPLE.VAGUE_DATE_TYPE, 0) END'
WHERE Report_Attribute_Key = 'NBNSYS0000000048'
GO