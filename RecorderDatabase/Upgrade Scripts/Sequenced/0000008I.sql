
/****** Object:  UserDefinedFunction [dbo].[LC_FormatTagGeography]    Script Date: 04/04/2012 19:50:15 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LC_FormatTagGeography]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LC_FormatTagGeography]

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Returns the list of Geography for a selected tag 

  Parameters:	@Key	Concept_Key

  Created:	April 2012
  Author: MikeWeideli 

\*=========================================================================== */   

CREATE FUNCTION [dbo].[LC_FormatTagGeography](@Key char(16))
RETURNS varchar(8000)

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @ItemString varchar(8000)

DECLARE csrTagRunBY CURSOR
FOR
  SELECT dbo.ufn_RtfToPlaintext(SV.Geographic_Coverage)
	FROM SURVEY SV 
	INNER JOIN SURVEY_TAG ST ON ST.SURVEY_KEY = SV.SURVEY_KEY
	WHERE ST.CONCEPT_KEY =@Key AND dbo.ufn_RtfToPlaintext(SV.Geographic_Coverage) <>  ''

OPEN csrTagRunBY

FETCH NEXT FROM  csrTagRunBY INTO  @ItemString
IF @@FETCH_STATUS = 0 
  SELECT @ReturnString = @ItemString
  
WHILE @@FETCH_STATUS = 0
BEGIN
	FETCH NEXT FROM  csrTagRunBY INTO @ItemString
	
	IF @@FETCH_STATUS = 0 
	  if charindex(@ItemString,@ReturnString)= 0 AND @ItemString <> ''  
		SELECT @ReturnString = @ReturnString + '; ' + @ItemString
END

CLOSE csrTagRunBY
DEALLOCATE csrTagRunBY

RETURN @ReturnString
--****************************************************************************************************

END

GO

GRANT EXECUTE ON [dbo].[LC_FormatTagGeography] TO PUBLIC

GO
