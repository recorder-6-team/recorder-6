SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Mantis 472: Correction for -Y in sample year  
\*============================================================================*/

 
UPDATE REPORT_ATTRIBUTE SET ATTRIBUTE_SQL = '#REPORT_OUTPUT.[Sample Year] = CASE WHEN SAMPLE.VAGUE_DATE_TYPE IN (''U'', ''M'', ''S'',''-Y'') THEN NULL ELSE dbo.FormatDatePart(SAMPLE.VAGUE_DATE_START, SAMPLE.VAGUE_DATE_END, SAMPLE.VAGUE_DATE_TYPE, 0) END ' WHERE
REPORT_ATTRIBUTE_KEY = 'NBNSYS0000000048'
