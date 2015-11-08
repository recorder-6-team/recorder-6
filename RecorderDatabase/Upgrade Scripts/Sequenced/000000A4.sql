Delete from SETTING where NAME = 'HelpURL'
GO
INSERT INTO SETTING VALUES('HelpURL','http://www.recorder6.info/WebHelpR6Main/')
GO
Delete from SETTING where NAME = 'GatewayURL'
GO
INSERT INTO SETTING VALUES('GatewayURL','https://data.nbn.org.uk/Search?q=')
GO
Delete from SETTING where NAME = 'PrefNames'
Go
INSERT INTO SETTING VALUES('PrefNames',(Select Data from Setting Where Name = 'SiteId'))
GO
Delete from SETTING where NAME = 'PrefLocs'
Go
INSERT INTO SETTING VALUES('PrefLocs',(Select Data from Setting Where Name = 'SiteId'))