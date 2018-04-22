/****** Improve Review Update where review is a vague date******/
/***** If the Review and Latest TDET keys are the same then this is always OK.  ******/  
/****** Object:  UserDefinedFunction [dbo].[ufn_ReturnReviewStatus]    Script Date: 02/01/2018 15:14:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


ALTER FUNCTION [dbo].[ufn_ReturnReviewStatus] 
(@TOCCKey char(16))
RETURNS char(1)

AS
BEGIN

DECLARE @ReviewVDS Int
DECLARE @ReviewVDE Int
DECLARE @ReviewPreferred  bit
DECLARE @ReviewVerified int
DECLARE @ReviewTLIKey char(16) 
DECLARE @ReviewTDetKey char(16) 
DECLARE @ReturnString varchar(30) 
DECLARE @LatestVDS Int
DECLARE @LatestVDE Int
DECLARE @LatestPreferred bit
DECLARE @LatestVerified int
DECLARE @LatestTLIKey char(16)
DECLARE @LatestTDetKey char(16) 
DECLARE @PreferredVDS Int
DECLARE @PreferredVDE Int
DECLARE @PreferredPreferred bit
DECLARE @PreferredVerified int
DECLARE @PreferredTLIKey char(16)
DECLARE @PreferredTDetKey char(16)

DECLARE @Competency Int 

SET @Competency = (SELECT CAST([DATA] AS integer) FROM SETTING WHERE [NAME] = 'Competency')

SET @ReturnString = '0'

If @Competency <> 0

BEGIN  
  select @ReviewVDS = VAGUE_DATE_START, 
       @ReviewVDE = VAGUE_DATE_END,
       @ReviewTLIKey = TAXON_LIST_ITEM_KEY,
       @ReviewPreferred = PREFERRED,
       @ReviewVerified = DT.VERIFIED, 
       @ReviewTDETKey = TAXON_DETERMINATION.TAXON_DETERMINATION_KEY
       From TAXON_DETERMINATION
       INNER JOIN DETERMINATION_TYPE DT ON DT.DETERMINATION_TYPE_KEY = TAXON_DETERMINATION.DETERMINATION_TYPE_KEY     
       AND TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY = @TOCCKey  
       WHERE cast(TAXON_DETERMINATION.VAGUE_DATE_START as varchar(10)) + cast(TAXON_DETERMINATION.VAGUE_DATE_END as varchar(10))+ TAXON_DETERMINATION.TAXON_DETERMINATION_KEY
        =
       (select max(cast(tdet.VAGUE_DATE_START as varchar(10)) + cast(tdet.VAGUE_DATE_END as varchar(10))+ tdet.TAXON_DETERMINATION_KEY)
       From TAXON_DETERMINATION tdet inneR join 
       DETERMINER_ROLE DR on DR.DETERMINER_ROLE_KEY = tdet.DETERMINER_ROLE_KEY
       WHERE tdet.TAXON_OCCURRENCE_KEY = @TOCCKey AND DR.VALIDATION_COMPETENCY >= @Competency ) 

 
  select @LatestVDS = VAGUE_DATE_START, 
       @LatestVDE = VAGUE_DATE_END,
       @LatestTLIKey = TAXON_LIST_ITEM_KEY,
       @LatestPreferred = PREFERRED,
       @LatestVerified = DT.VERIFIED, 
       @LatestTDETKey = TAXON_DETERMINATION.TAXON_DETERMINATION_KEY
       From TAXON_DETERMINATION
       INNER JOIN DETERMINATION_TYPE DT ON DT.DETERMINATION_TYPE_KEY = TAXON_DETERMINATION.DETERMINATION_TYPE_KEY     
       AND TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY = @TOCCKey  
       WHERE cast(TAXON_DETERMINATION.VAGUE_DATE_START as varchar(10)) + cast(TAXON_DETERMINATION.VAGUE_DATE_END as varchar(10))+ TAXON_DETERMINATION.TAXON_DETERMINATION_KEY
        =
       (select max(cast(tdet.VAGUE_DATE_START as varchar(10)) + cast(tdet.VAGUE_DATE_END as varchar(10))+ tdet.TAXON_DETERMINATION_KEY)
       From TAXON_DETERMINATION tdet inneR join 
       DETERMINER_ROLE DR on DR.DETERMINER_ROLE_KEY = tdet.DETERMINER_ROLE_KEY
       WHERE tdet.TAXON_OCCURRENCE_KEY = @TOCCKey ) 

  select @PreferredVDS = VAGUE_DATE_START, 
       @PreferredVDE = VAGUE_DATE_END,
       @PreferredTLIKey = TAXON_LIST_ITEM_KEY,
       @PreferredPreferred = PREFERRED,
       @PreferredVerified = DT.VERIFIED, 
       @PreferredTDETKey = TAXON_DETERMINATION.TAXON_DETERMINATION_KEY
       From TAXON_DETERMINATION
       INNER JOIN DETERMINATION_TYPE DT ON DT.DETERMINATION_TYPE_KEY = TAXON_DETERMINATION.DETERMINATION_TYPE_KEY     
       AND TAXON_DETERMINATION.TAXON_OCCURRENCE_KEY = @TOCCKey  
       WHERE cast(TAXON_DETERMINATION.VAGUE_DATE_START as varchar(10)) + cast(TAXON_DETERMINATION.VAGUE_DATE_END as varchar(10))+ TAXON_DETERMINATION.TAXON_DETERMINATION_KEY
        =
       (select max(cast(tdet.VAGUE_DATE_START as varchar(10)) + cast(tdet.VAGUE_DATE_END as varchar(10))+ tdet.TAXON_DETERMINATION_KEY)
       From TAXON_DETERMINATION tdet inneR join 
       DETERMINER_ROLE DR on DR.DETERMINER_ROLE_KEY = tdet.DETERMINER_ROLE_KEY
       WHERE tdet.TAXON_OCCURRENCE_KEY = @TOCCKey AND TDET.PREFERRED = 1 ) 

   SET @ReturnString = '3' 
   
   IF  @LatestVDE > ISNUll(@ReviewVDS,0) AND @LatestTDETKey > ISNUll(@ReviewTDETKey,0)  
       Set @ReturnString = '1'
   ELSE IF
    (@PreferredTLIKey <> ISNULL(@ReviewTLIKey,'')) OR
    (@PreferredVerified <> @ReviewVerified AND  @ReviewVerified = 1) OR
    (@PreferredVerified <> @ReviewVerified AND  @PreferredVerified = 1) 
     SET @ReturnString = '2'
 END
 
 Return @ReturnString

END
