Delete From USABLE_FIELD 
Where Exists (Select * From USABLE_FIELD Where USABLE_FIELD_KEY = 'LCA0002300000001') and USABLE_FIELD_KEY = 'LCA0002300000001'

GO

Insert Into USABLE_FIELD
Values ('LCA0002300000001', 'INDIVIDUAL', 'NAME_KEY', 'Preferred (enter criterion as ''Y'')', 'TEXT', 'F', 0,0, 0, 'SELECT Name_Key AS ItemKey FROM 
Name where (SELECT DATA FROM SETTING WHERE NAME = ''SiteIdsOrg'') Like ''%'' + Name.custodian + ''%'' OR Name.Custodian = 
(SELECT DATA FROM SETTING WHERE NAME = ''SITEID'') OR  ''##''')

GO

Delete From USABLE_FIELD 
Where Exists (Select * From USABLE_FIELD Where USABLE_FIELD_KEY = 'LCA0002300000002') and USABLE_FIELD_KEY = 'LCA0002300000002'

GO

Insert Into USABLE_FIELD
Values ('LCA0002300000002', 'LOCATION', 'LOCATION_KEY', 'Preferred (enter criterion as ''Y'')', 'TEXT', 'F', 0,0, 0, 'SELECT Location_Key As  ItemKey FROM
Location WHERE (SELECT DATA FROM SETTING WHERE NAME = ''SiteIdsLoc'') Like ''%'' + Location.custodian + ''%'' OR Location.Custodian = 
(SELECT DATA FROM SETTING WHERE NAME = ''SITEID'') OR  ''##''')

GO

Delete From USABLE_FIELD 
Where Exists (Select * From USABLE_FIELD Where USABLE_FIELD_KEY = 'LCA0002300000003') and USABLE_FIELD_KEY = 'LCA0002300000003'

GO

Insert Into USABLE_FIELD
Values ('LCA0002300000003', 'SURVEY', 'SURVEY_KEY', 'Preferred (enter criterion as ''Y'')', 'TEXT', 'F', 0,0, 0, 'SELECT Survey_Key As  ItemKey FROM
Survey WHERE (SELECT DATA FROM SETTING WHERE NAME = ''SiteIdsLoc'') Like ''%'' + Survey.custodian + ''%'' OR Survey.Custodian = 
(SELECT DATA FROM SETTING WHERE NAME = ''SITEID'') OR  ''##''')

GO

GRANT INSERT ON Setting TO R2k_Administrator 

GO