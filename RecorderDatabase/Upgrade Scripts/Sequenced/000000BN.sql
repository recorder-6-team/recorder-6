/****** Fixes link to species lookup on NBN ******/
Update SETTING SET [DATA_DEFAULT] = 'https://species.nbnatlas.org/search/?q=' WHERE
[NAME] = 'GatewayURL'

/****** Fixes Field Length of Obs Sources  ******/
Update REPORT_FIELD SET FIELD_SIZE  = 8000 WHERE REPORT_FIELD_KEY = 'JNCCDEV100000006'

/****** Takes out time from Taxon_Private_Data item_date   ******/
Update Taxon_Private_Data Set Item_Date = DATEADD(DAY, 1, Item_Date)