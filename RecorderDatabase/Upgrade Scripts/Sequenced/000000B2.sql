/****** adds a new field to Setting to hold the data default ******/

ALTER TABLE Setting 
ADD  DATA_DEFAULT varchar(250) null

GO


UPDATE SETTING SET Data_Default = '+' WHERE [Name] = 'UrlSep'
UPDATE SETTING SET Data_Default = 'https://data.nbn.org.uk/Search?q=+' WHERE [Name] = 'GateWayUrl'
UPDATE SETTING SET Data_Default = '4' WHERE [Name] = 'LevelConf'
UPDATE SETTING SET Data_Default = 'NHMSYS0020424779,NHMSYS0001749171,NHMSYS0001770825,NHMSYS0001770462' 
WHERE [Name] = 'TaxDesList'
UPDATE SETTING SET Data_Default = '1'WHERE [Name] = 'ConfFull'
UPDATE SETTING SET Data_Default = '.' WHERE [Name] = 'DBListSep'
UPDATE SETTING SET Data_Default = 'NBNSYS0000000007'WHERE [Name] = 'TempMedia'
UPDATE SETTING SET Data_Default = 'LCA0002400000001'WHERE [Name] = 'TempName'
UPDATE SETTING SET Data_Default = 'Organism' WHERE [Name] = 'SortMethod'
UPDATE SETTING SET Data_Default = 'http://www.recorder6.info/WebHelpR6V625/' 
WHERE [Name] = 'HelpURL'
UPDATE SETTING SET Data_Default = (Select [DATA] From Setting Where [NAME] = 'SiteId')
WHERE [Name] = 'PrefNames'
UPDATE SETTING SET Data_Default = (Select [DATA] From Setting Where [NAME] = 'SiteId')
WHERE [Name] = 'PrefLocs'
UPDATE SETTING SET Data_Default = '0' WHERE [Name] = 'Competency'
