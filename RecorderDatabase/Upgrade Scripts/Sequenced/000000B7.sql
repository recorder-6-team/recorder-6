/****** Add Document to Report wizard - consolidates multiple documents from Occurrence and Sample     ******/

/*===========================================================================*\
  Drop udf before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCSampleandToccSources]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCSampleandToccSources]  

GO
/****** Object:  UserDefinedFunction [dbo].[LCSampleandToccSources]    Script Date: 01/27/2018 16:34:55 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

-- 

/****** Object:  UserDefinedFunction [dbo].[LCSampleandToccSources]    Script Date: 01/27/2018 19:02:42 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[LCSampleandToccSources]
(@TOCCKey char(16), @incSample bit)
RETURNS varchar(8000)
--
--	DESCRIPTION
--	Function to return a semi-colon seperated string of all sources (where including those  
--	attached to the Sample related to the TOCC key)
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@TOCCKey			Taxon Occurrence.
--      @incSample                      If 1 then includes Sample 
--
--	AUTHOR:	Mike Weideli
--	CREATED: 27/01/2018
--

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @ItemString varchar(200)
DECLARE @Documents  varchar(8000)
SET @Documents  =''


SELECT @Documents = @Documents + [dbo].[ufn_GetFormattedReferenceName]
(TOS.Source_Key) +  '; ' 

FROM 
 
TAXON_OCCURRENCE_SOURCES TOS 

WHERE TOS.Taxon_Occurrence_key = @TOCCkey



if @incSample = 1 
BEGIN
  SELECT @Documents = @Documents + [dbo].[ufn_GetFormattedReferenceName]
   (SOS.Source_Key) +  '; ' 
  FROM 
   SAMPLE_SOURCES SOS INNER JOIN Taxon_Occurrence TOCC
   ON TOCC.Sample_Key = SOS.Sample_Key
   WHERE TOCC.Taxon_Occurrence_key = @TOCCkey
  
END

if len(@Documents) > 0 
BEGIN
  set  @RETURNSTRING  = left(@Documents,len(@Documents)-1)
END




--****************************************************************************************************
RETURN @ReturnString
END

GO

GRANT EXECUTE ON [dbo].[LCSampleandToccSources] TO PUBLIC


GO

DELETE FROM REPORT_FIELD WHERE REPORT_FIELD_KEY = 'R6TEAM0100000001'

GO
DELETE FROM REPORT_ATTRIBUTE WHERE REPORT_ATTRIBUTE_KEY = 'R6TEAM0100000001'

GO

INSERT INTO REPORT_ATTRIBUTE (REPORT_ATTRIBUTE_KEY,ITEM_NAME,ITEM_GROUP,SOURCE_TABLE,
ATTRIBUTE_SQL,REPORT_JOIN_KEY,ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES('R6TEAM0100000001','Documents (Obs + Samples)','Documents','OBSERVATION',
'#REPORT_OUTPUT.[Documents (Obs + Samples)]=[dbo].[LCSampleandToccSources]( TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY,1)',
'NBNSYS0000000016','NBNSYS0000000001',getdate(),1)

GO

INSERT INTO REPORT_FIELD (REPORT_FIELD_KEY,ENTERED_BY,REPORT_ATTRIBUTE_KEY,
FIELD_ITEM_NAME, FIELD_TYPE,FIELD_SIZE,ENTRY_DATE,SYSTEM_SUPPLIED_DATA)
VALUES('R6TEAM0100000001','NBNSYS0000000001','R6TEAM0100000001','Documents (Obs + Samples)',
'varchar',8000,getDate(),1)

GO



UPdate REPORT_ATTRIBUTE SET ATTRIBUTE_SQL = '#REPORT_OUTPUT. [Obs Source]=[dbo].[LCSampleandToccSources]( TAXON_OCCURRENCE.TAXON_OCCURRENCE_KEY,0)', REPORT_JOIN_KEY = 'NBNSYS0000000016'
WHERE REPORT_ATTRIBUTE_KEY = 'JNCCDEV100000006'









