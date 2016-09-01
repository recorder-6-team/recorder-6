/****** Changes in Connection with Licence field on Survey ******/


CREATE TABLE [dbo].[LICENCES](
	[LICENCE_KEY] [char](16) NOT NULL,
	[LONG_NAME] [varchar](100) NOT NULL,
	[SHORT_NAME] [varchar](10) NOT NULL,
	[DESCRIPTION] [text] NOT NULL,
	[URL_READABLE] [varchar](255) NULL,
	[URL_LEGAL] [varchar](255) NULL,
	[VERSION] [char](10) NULL,
	[ENTERED_BY] [char](16) NOT NULL,
	[ENTRY_DATE] [smalldatetime] NOT NULL,
	[CHANGED_BY] [char](16) NULL,
	[CHANGED_DATE] [smalldatetime] NULL,
	[SYSTEM_SUPPLIED_DATA] [bit] NOT NULL,
	[CUSTODIAN] [char](8) NULL )
        ON [PRIMARY]
GO
  
ALTER TABLE [dbo].[LICENCES] ADD 
	CONSTRAINT [PK_LICENCES] PRIMARY KEY CLUSTERED 
	( [LICENCE_KEY]
	)  ON [PRIMARY] 



GO

ALTER TABLE [dbo].[SURVEY]  WITH NOCHECK ADD  CONSTRAINT [FK_SURVEY_LICENCES] FOREIGN KEY([LICENCE_KEY])
REFERENCES [dbo].[LICENCES] ([LICENCE_KEY])

GO

ALTER TABLE [dbo].[SURVEY] CHECK CONSTRAINT [FK_SURVEY_LICENCES]
GO


GRANT SELECT ON [dbo].[LICENCES] TO R2k_AddOnly 
GRANT INSERT ON [dbo].[LICENCES] TO R2k_AddOnly 
GRANT SELECT ON [dbo].[LICENCES] TO R2k_ReadOnly 
GRANT SELECT ON [dbo].[LICENCES] TO R2k_RecordCardsOnly
GRANT SELECT ON [dbo].[LICENCES] TO R2k_Administrator 
GRANT INSERT ON [dbo].[LICENCES] TO R2k_Administrator 
GRANT DELETE ON [dbo].[LICENCES] TO R2k_Administrator 
GRANT UPDATE ON [dbo].[LICENCES] TO R2k_Administrator 
GRANT SELECT ON [dbo].[LICENCES] TO R2k_FullEdit
GRANT INSERT ON [dbo].[LICENCES] TO R2k_FullEdit 
GRANT DELETE ON [dbo].[LICENCES] TO R2k_FullEdit
GRANT UPDATE ON [dbo].[LICENCES] TO R2k_FullEdit
GO


CREATE TRIGGER [dbo].[LICENCECustodianInsert] ON [dbo].[LICENCES] AFTER INSERT AS UPDATE LICENCES SET LICENCES.CUSTODIAN = SUBSTRING(LICENCES.LICENCE_KEY, 1, 8) FROM LICENCES INNER JOIN INSERTED ON LICENCES.LICENCE_KEY = INSERTED.LICENCE_KEY WHERE LICENCES.CUSTODIAN IS NULL

GO

INSERT INTO licences(
    LICENCE_KEY,
    Long_Name,
    Short_Name,
    description,
    url_readable,
    url_legal,
    version,
    ENTRY_DATE, ENTERED_BY, SYSTEM_SUPPLIED_DATA,CUSTODIAN
)
VALUES (
    'NBNSYS0000000003',
    'Creative Commons Attribution',
    'CC-BY',
    'This licence lets others distribute, remix, tweak, and build upon your work, even commercially, as long as they credit you for the original creation. This is the most accommodating of Creative Commons licences offered. Recommended for maximum dissemination and use of licenced materials.',
    'https://creativecommons.org/licenses/by/4.0', 
    'https://creativecommons.org/licenses/by/4.0/legalcode',
    '4.0',
    getdate(),'NBNSYS0000000001',1,'NBNSYS00');

INSERT INTO licences(
    LICENCE_KEY,
    Long_Name,
    Short_Name,
    description,
    url_readable,
    url_legal,
    version,
    ENTRY_DATE, ENTERED_BY, SYSTEM_SUPPLIED_DATA,CUSTODIAN
)
VALUES (
    'NBNSYS0000000004',
    'Creative Commons Attribution-NonCommercial',
    'CC-BY-NC',
    'This licence lets others remix, tweak, and build upon your work non-commercially, and although their new works must also acknowledge you and be non-commercial, they don’t have to licence their derivative works on the same terms',
    'https://creativecommons.org/licenses/by-nc/4.0', 
    'https://creativecommons.org/licenses/by-nc/4.0/legalcode', 
    '4.0',
     getdate(),'NBNSYS0000000001',1,'NBNSYS00');

INSERT INTO licences(
    LICENCE_KEY,
    Long_Name,
    Short_Name,
    description,
    url_readable,
    url_legal,
    version,
    ENTRY_DATE, ENTERED_BY, SYSTEM_SUPPLIED_DATA,CUSTODIAN
)
VALUES (
    'NBNSYS0000000005',
    'Creative Commons "No Rights Reserved"',
    'CC0',
    'Copyright and other laws throughout the world automatically extend copyright protection to works of authorship and databases, whether the author or creator wants those rights or not. CC0 gives those who want to give up those rights a way to do so, to the fullest extent allowed by law. Once the creator or a subsequent owner of a work applies CC0 to a work, the work is no longer his or hers in any meaningful sense under copyright law. Anyone can then use the work in any way and for any purpose, including commercial purposes, subject to other laws and the rights others may have in the work or how the work is used. Think of CC0 as the "no rights reserved" option.',
    'https://creativecommons.org/publicdomain/zero/1.0',
    'https://creativecommons.org/publicdomain/zero/1.0/legalcode', 
    '1.0',
    getdate(),'NBNSYS0000000001',1,'NBNSYS00');


INSERT INTO licences(
   LICENCE_KEY,
    Long_Name,
    Short_Name,
    description,
    url_readable,
    url_legal,
    version,
    ENTRY_DATE, ENTERED_BY, SYSTEM_SUPPLIED_DATA,CUSTODIAN
)
VALUES (
    'NBNSYS0000000006',
    'Open Government licence (UK)',
    'OGL',
    'Open Government Licence for public sector information in the UK.',
    'http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3',
    'http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3',
    '3',
    getdate(),'NBNSYS0000000001',1,'NBNSYS00');

INSERT INTO licences(
   LICENCE_KEY,
    Long_Name,
    Short_Name,
    description,
    version,
    ENTRY_DATE, ENTERED_BY, SYSTEM_SUPPLIED_DATA,CUSTODIAN
)
VALUES (
    'NBNSYS0000000001',
    'No licence applied',
    'None',
    'Default where no licence specified.',
    '1',
    getdate(),'NBNSYS0000000001',1,'NBNSYS00');
    
    
INSERT INTO licences(
   LICENCE_KEY,
    Long_Name,
    Short_Name,
    description,
    version,
    ENTRY_DATE, ENTERED_BY, SYSTEM_SUPPLIED_DATA,CUSTODIAN
)
VALUES (
    'NBNSYS0000000002',
    'Recorder 6 Export',
    'Recorder 6 ',
    'Data may be imported into Recorder 6 or a similar system where the record keys and custodian data is retained. The data may modified only in accordance with the Recorder 6 rules. The data may then passed on to other Recorder 6 users subject to the same conditions. Where data is passed on in any other format then it may be used by that user only and not passed on again in any format.',
    '1',
    getdate(),'NBNSYS0000000001',1,'NBNSYS00');

GO

DELETE FROM REPORT_FIELD WHERE REPORT_FIELD_KEY IN('NBNSYS0000000201','NBNSYS0000000202')

DELETE FROM REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY IN('NBNSYS0000000201','NBNSYS0000000202')

DELETE FROM REPORT_JOIN WHERE REPORT_JOIN_KEY  = 'NBNSYS0000000201'

INSERT INTO  REPORT_JOIN (REPORT_JOIN_KEY,JOIN_SQL,ENTERED_BY,ENTRY_DATE,
SYSTEM_SUPPLIED_DATA)
VALUES ('NBNSYS0000000201',
'FROM #REPORT_OUTPUT INNER JOIN SURVEY INNER JOIN LICENCES ON SURVEY.LICENCE_KEY = LICENCES.LICENCE_KEY
ON #REPORT_OUTPUT.SURVEY_KEY = SURVEY.SURVEY_KEY',
'TESTDATA00000001', Getdate(),1)

INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,ITEM_GROUP,SOURCE_TABLE,ITEM_NAME,
ATTRIBUTE_SQL, REPORT_JOIN_KEY,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('NBNSYS0000000201','Survey','SURVEY', 'Survey Licence (Long name)',
'#REPORT_OUTPUT.[Survey Licence (Long name)] = LICENCES.LONG_NAME',
'NBNSYS0000000201','TESTDATA00000001', Getdate(),1)

INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,ITEM_GROUP,SOURCE_TABLE,ITEM_NAME,
ATTRIBUTE_SQL, REPORT_JOIN_KEY,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('NBNSYS0000000202','Survey','SURVEY', 'Survey Licence (Short name)',
'#REPORT_OUTPUT.[Survey Licence (Short name)] = LICENCES.SHORT_NAME',
'NBNSYS0000000201','TESTDATA00000001', Getdate(),1)

INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,REPORT_ATTRIBUTE_KEY,FIELD_ITEM_NAME,
FIELD_TYPE,FIELD_SIZE,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('NBNSYS0000000201','NBNSYS0000000201','Survey Licence (Long name)',
'varchar',100,'TESTDATA00000001', Getdate(),1)


INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,REPORT_ATTRIBUTE_KEY,FIELD_ITEM_NAME,
FIELD_TYPE,FIELD_SIZE,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES ('NBNSYS0000000202','NBNSYS0000000202','Survey Licence (Short name)',
'varchar',10,'TESTDATA00000001', Getdate(),1)
