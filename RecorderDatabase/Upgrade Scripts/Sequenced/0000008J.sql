/****** Object:  UserDefinedFunction [dbo].[LC_FormatTagRunBy]    Script Date: 04/04/2012 19:50:44 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LC_FormatTagRunBy]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LC_FormatTagRunBy]

GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Function to return a list or run by for selected tag
         	
  Parameters:
		@Key Concept Key


  Created:	April 2012
  Author: MikeWeideli 

\*=========================================================================== */
 
CREATE FUNCTION [dbo].[LC_FormatTagRunBy](@Key char(16))
RETURNS varchar(8000)

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @NameKey char(16)
DECLARE @ItemString varchar(100)

DECLARE csrTagRunBY CURSOR
FOR
  SELECT SV.Run_By 
	FROM SURVEY SV 
	INNER JOIN SURVEY_TAG ST ON ST.SURVEY_KEY = SV.SURVEY_KEY
	WHERE ST.CONCEPT_KEY =@Key

OPEN csrTagRunBY

FETCH NEXT FROM  csrTagRunBY INTO  @nameKey
IF @@FETCH_STATUS = 0 
  SELECT @ReturnString =  dbo.ufn_GetFormattedName(@NameKey)
  
WHILE @@FETCH_STATUS = 0
BEGIN
	FETCH NEXT FROM  csrTagRunBY INTO @NameKey
	SELECT @ItemString = dbo.ufn_GetFormattedName(@NameKey) 
	IF @@FETCH_STATUS = 0 
	  if charindex(	@ItemString,@ReturnString)= 0 
		SELECT @ReturnString = @ReturnString + '; ' + @ItemString
END

CLOSE csrTagRunBY
DEALLOCATE csrTagRunBY

RETURN @ReturnString
--****************************************************************************************************

END

GO

GRANT EXECUTE ON [dbo].[LC_FormatTagRunBy] TO PUBLIC

