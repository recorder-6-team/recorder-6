/*===========================================================================*\
  Functions and table entries to allow attributes to be returned in the report wizard  

\*=========================================================================== */

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCReturnAttribute]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCReturnAttribute]

GO

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCRecNameWithAttribute]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCRecNameWithAttribute]
GO

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCPrefNameWithAttribute]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCPrefNameWithAttribute]
GO

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCFullRecName]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCFullRecName]
GO

GO
/****** Object:  UserDefinedFunction [dbo].[LCFullRecName]    Script Date: 01/10/2013 14:57:16 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns full recommended taxon (Name, attribute, authority) 

  Parameters:
  @TLIKey - Taxon List Item Key (Normally from Taxon_Determination, but doesn't have to be) 
 
 		  
  Created:	January 2013
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCFullRecName]
(@TLIKey char(16) )

RETURNS varchar(242)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(242)
DECLARE @RecName varchar(100)
DECLARE @Authority varchar(75)
DECLARE @Attribute varchar(65)
DECLARE @TVKey char(16)

   Select @TVKey = TLI.Taxon_Version_Key
   FROM Taxon_List_Item TLI
   INNER JOIN INDEX_TAXON_NAME ITN ON ITN.Recommended_Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
   WHERE ITN.Taxon_List_Item_Key = @TLIKey   
     
   Select @Recname = T.ITem_name, @Attribute = 
   case  ISNULL(TV.Attribute,'')
      WHEN '' THEN  ''
      ELSE
      ' ' +   ISNULL(TV.Attribute,'') 
   END    
   , @Authority =
    case  ISNULL(T.Authority,'')
      WHEN '' THEN  ''
      ELSE
      ' ' +   ISNULL(T.Authority,'') 
   END  
           
   FROM Taxon T INNER JOIN Taxon_Version TV ON TV.Taxon_Key
   = T.Taxon_Key WHERE TV.Taxon_Version_Key = @TVKey 
 
  
   set @ReturnString = @RecName + @Attribute + @Authority 
   
    

RETURN @ReturnString
END
GO

GRANT EXECUTE ON [dbo].[LCFullRecName] TO PUBLIC

GO

/****** Object:  UserDefinedFunction [dbo].[LCReturnAttribute]    Script Date: 01/09/2013 16:51:57 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns attribute for actual, preferred or recommended taxon

  Parameters:
  @TLIKey - Taxon List Item Key (Normally from Taxon_Determination, but doesn't have to be) 
 
  @ReturnType set to 0 = actual, 1 = preferred or 2 = recommended 			  
  Created:	January 2013
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCReturnAttribute]
(@TLIKey char(16), @ReturnType as integer)

RETURNS varchar(65)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(166)

If  @ReturnType = 0 
   SELECT @ReturnString = TV.Attribute FROM Taxon_Version TV INNER JOIN Taxon_List_Item TLI
   ON TLI.Taxon_Version_Key = TV.Taxon_Version_Key  WHERE TLI.Taxon_List_Item_Key = @TLIKey
ELSE IF  @ReturnType = 1
    SELECT @ReturnString = TV.Attribute FROM Taxon_Version TV INNER JOIN Taxon_List_Item TLI
    ON TLI.Taxon_Version_Key = TV.Taxon_Version_Key INNER JOIN Taxon_List_Item TLI2 ON TLI2.Preferred_Name
    = TLI.Taxon_List_Item_Key WHERE TLI2.Taxon_List_Item_Key = @TLIKey        
ELSE 
    SELECT @ReturnString = TV.Attribute FROM Taxon_Version TV INNER JOIN Taxon_List_Item TLI
    ON TLI.Taxon_Version_Key = TV.Taxon_Version_Key INNER JOIN Index_Taxon_Name ITN ON ITN.Recommended_Taxon_List_Item_Key
    = TLI.Taxon_List_Item_Key WHERE ITN.Taxon_List_Item_Key = @TLIKey    
 
         
     

RETURN @ReturnString
END

GO

GRANT EXECUTE ON [dbo].[LCReturnAttribute] TO PUBLIC



GO

/****** Object:  UserDefinedFunction [dbo].[LCRecNameWithAttribute]    Script Date: 01/09/2013 16:49:52 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns recommended taxon name with attribute 

  Parameters:
  @TLIKey - Taxon List Item Key (Normally from Taxon_Determination, but doesn't have to be) 
 
  @SubGenus set to 1 if true or 0 if false			  
  Created:	January 2013
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCRecNameWithAttribute]
(@TLIKey char(16),  @SubGenus as bit )

RETURNS varchar(166)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(166)
DECLARE @RecName varchar(100)
DECLARE @SciName varchar(100)
DECLARE @Attribute varchar(65)
DECLARE @TVKey char(16)

   Select @TVKey = TLI.Taxon_Version_Key
   FROM Taxon_List_Item TLI
   INNER JOIN INDEX_TAXON_NAME ITN ON ITN.Recommended_Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
   WHERE ITN.Taxon_List_Item_Key = @TLIKey   
     
   Select @Recname = T.ITem_name, @Attribute = 
   case  ISNULL(TV.Attribute,'')
      WHEN '' THEN  ''
      ELSE
      ' ' +   ISNULL(TV.Attribute,'') 
   END    
      
        
   FROM Taxon T INNER JOIN Taxon_Version TV ON TV.Taxon_Key
   = T.Taxon_Key WHERE TV.Taxon_Version_Key = @TVKey 
 
   if (charindex('(', @RecName) > 1) AND (charindex(')',@RecName) > charindex('(',@RecName) + 1) AND (charindex(')',@RecName) <> len(@REcName) ) AND @SubGenus = 0 
        set  @ReturnString =  LEFT(@Recname,
           CHARINDEX(' (', @Recname)) + RIGHT(  @Recname,(LEN(@Recname) -(CHARINDEX(') ', @Recname) + 1 )  )) 
           + @Attribute
           
         else
           set @ReturnString = @RecName + @Attribute 
   
      
 

         
     

RETURN @ReturnString
END


GO

GRANT EXECUTE ON [dbo].[LCRecNameWithAttribute] TO PUBLIC


GO

/****** Object:  UserDefinedFunction [dbo].[LCPrefNameWithAttribute]    Script Date: 01/09/2013 16:48:06 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Returns preferred taxon name with attribute 

  Parameters:
  @TLIKey - Taxon List Item Key (Normally from Taxon_Determination, but doesn't have to be) 
 
  @SubGenus set to 1 if true or 0 if false			  
  Created:	January 2013
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCPrefNameWithAttribute]
(@TLIKey char(16),  @SubGenus as bit )

RETURNS varchar(166)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(166)
DECLARE @RecName varchar(100)
DECLARE @SciName varchar(100)
DECLARE @Attribute varchar(65)
DECLARE @TVKey char(16)

   Select @TVKey = TLI2.Taxon_Version_Key
   FROM Taxon_List_Item TLI
   INNER JOIN TAXON_LIST_ITEM TLI2 ON TLI2.Taxon_List_Item_Key = TLI.Preferred_Name
   WHERE TLI.Taxon_List_Item_Key = @TLIKey   
     
   Select @Recname = T.ITem_name, @Attribute = 
   case  ISNULL(TV.Attribute,'')
      WHEN '' THEN  ''
      ELSE
      ' ' +   ISNULL(TV.Attribute,'') 
   END    
     
        
   FROM Taxon T INNER JOIN Taxon_Version TV ON TV.Taxon_Key
   = T.Taxon_Key WHERE TV.Taxon_Version_Key = @TVKey 
 
   if (charindex('(', @RecName) > 1) AND (charindex(')',@RecName) > charindex('(',@RecName) + 1) AND (charindex(')',@RecName) <> len(@REcName) ) AND @SubGenus = 0 
        set  @ReturnString =  LEFT(@Recname,
           CHARINDEX(' (', @Recname)) + RIGHT(  @Recname,(LEN(@Recname) -(CHARINDEX(') ', @Recname) + 1 )  )) 
           + @Attribute
           
         else
           set @ReturnString = @RecName + @Attribute 
   
      
 

         
     

RETURN @ReturnString
END


GO

GRANT EXECUTE ON [dbo].[LCPrefNameWithAttribute]  TO PUBLIC



GO



/****** Including recommended taxon plus attribute in Report Wizard (without sub genus) ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000001') and REPORT_FIELD_KEY = 'LCA0002200000001'

GO

Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000001') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000001'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000001', 'Taxon', 'Taxon_Determination', 'Recommended Taxon Name/Attribute no sub gen.', 
'#REPORT_OUTPUT.[Recommended Taxon Name/Attribute no sub gen.] = dbo.LCRecNameWithAttribute(TAXON_LIST_ITEM_KEY,0)', 
'NBNSYS0000000027', NULL, 'TESTDATA00000001', GetDate(), NULL, NULL, 1)

Go

Insert Into REPORT_FIELD
Values ('LCA0002200000001', 'LCA0002200000001', 'Recommended Taxon Name/Attribute no sub gen.', 'varchar', 166, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

Go
/****** Including recommended taxon plus attribute in Report Wizard (with sub genus) ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000002') and REPORT_FIELD_KEY = 'LCA0002200000002'
Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000002') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000002'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000002', 'Taxon', 'Taxon_Determination', 'Recommended Taxon Name/Attribute', 
'#REPORT_OUTPUT.[Recommended Taxon Name/Attribute] = dbo.LCRecNameWithAttribute(TAXON_LIST_ITEM_KEY,1)', 
'NBNSYS0000000027', NULL, 'TESTDATA00000001', GetDate(), NULL, NULL, 1)

Go
Insert Into REPORT_FIELD
Values ('LCA0002200000002', 'LCA0002200000002', 'Recommended Taxon Name/Attribute', 'varchar', 166, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)
Go

/****** Including preferred taxon plus attribute in Report Wizard (without sub genus) ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000003') and REPORT_FIELD_KEY = 'LCA0002200000003'

GO

Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000003') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000003'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000003', 'Taxon', 'Taxon_Determination', 'Taxon Latin Name/Attribute no sub gen.', 
'#REPORT_OUTPUT.[Taxon Latin Name/Attribute no sub gen.] = dbo.LCPrefNameWithAttribute(TAXON_LIST_ITEM_KEY,0)', 
'NBNSYS0000000027', NULL, 'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO
Insert Into REPORT_FIELD
Values ('LCA0002200000003', 'LCA0002200000003', 'Taxon Latin Name/Attribute no sub gen.', 'varchar', 166, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO
/****** Including preferred taxon plus attribute in Report Wizard (with sub genus) ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000004') and REPORT_FIELD_KEY = 'LCA0002200000004'

GO

Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000004') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000004'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000004', 'Taxon', 'Taxon_Determination', 'Taxon Latin Name/Attribute', 
'#REPORT_OUTPUT.[Taxon Latin Name/Attribute] = dbo.LCPrefNameWithAttribute(TAXON_LIST_ITEM_KEY,1)', 
'NBNSYS0000000027', NULL, 'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO
Insert Into REPORT_FIELD
Values ('LCA0002200000004', 'LCA0002200000004', 'Taxon Latin Name/Attribute', 'varchar', 166, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO
/****** Including attributes in Report Wizard Taxon Attribute (Actual name as entered) ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000005') and REPORT_FIELD_KEY = 'LCA0002200000005'
Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000005') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000005'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000005', 'Taxon', 'Taxon_Determination', 'Taxon Attribute', 
'#REPORT_OUTPUT.[Taxon Attribute] = dbo.LCReturnAttribute(TAXON_LIST_ITEM_KEY,0)', 
'NBNSYS0000000027', NULL, 'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO
Insert Into REPORT_FIELD
Values ('LCA0002200000005', 'LCA0002200000005', 'Taxon Attribute', 'varchar', 166, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO
/****** Including attributes in Report Wizard Preferred Name Attribute (Preferred name as entered) ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000006') and REPORT_FIELD_KEY = 'LCA0002200000006'

GO

Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000006') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000006'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000006', 'Taxon', 'Taxon_Determination', 'Taxon Latin Attribute', 
'#REPORT_OUTPUT.[Taxon Latin Attribute] = dbo.LCReturnAttribute(TAXON_LIST_ITEM_KEY,0)', 
'NBNSYS0000000027', NULL, 'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO

Insert Into REPORT_FIELD
Values ('LCA0002200000006', 'LCA0002200000006', 'Taxon Latin Attribute', 'varchar', 166, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO
/****** Including attributes in Report Wizard Recommended Name Attribute  ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000007') and REPORT_FIELD_KEY = 'LCA0002200000007'

GO
Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000007') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000007'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000007', 'Taxon', 'Taxon_Determination', 'Recommended Taxon Name Attribute', 
'#REPORT_OUTPUT.[Recommended Taxon Name Attribute] = dbo.LCReturnAttribute(TAXON_LIST_ITEM_KEY,2)', 
'NBNSYS0000000027', NULL, 'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO
Insert Into REPORT_FIELD
Values ('LCA0002200000007', 'LCA0002200000007', 'Recommended Taxon Name Attribute', 'varchar', 166, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO
/****** Including attributes in Report Wizard Full Recommended Name  ******/

Delete From REPORT_FIELD 
Where Exists (Select * From REPORT_FIELD Where REPORT_FIELD_KEY = 'LCA0002200000008') and REPORT_FIELD_KEY = 'LCA0002200000008'
Delete From REPORT_ATTRIBUTE
Where Exists (Select * From REPORT_ATTRIBUTE Where REPORT_ATTRIBUTE_KEY = 'LCA0002200000008') and REPORT_ATTRIBUTE_KEY = 'LCA0002200000008'

GO

Insert Into REPORT_ATTRIBUTE
Values ('LCA0002200000008', 'Taxon', 'Taxon_Determination', 'Full Recommended Taxon Name', 
'#REPORT_OUTPUT.[Full Recommended Taxon Name] = dbo.LCFullREcName(TAXON_LIST_ITEM_KEY)', 
'NBNSYS0000000027', NULL, 'TESTDATA00000001', GetDate(), NULL, NULL, 1)

GO

Insert Into REPORT_FIELD
Values ('LCA0002200000008', 'LCA0002200000008', 'Full Recommended Taxon Name', 'varchar', 242, 
'TESTDATA00000001', GetDate(), NULL, NULL, 1)
