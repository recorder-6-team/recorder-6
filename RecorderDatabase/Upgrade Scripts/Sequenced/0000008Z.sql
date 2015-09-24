/****** Object:  UserDefinedFunction [dbo].[LCRemoveSubGenusText]    Script Date: 07/21/2012 21:43:58 ******/


SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCRemoveSubGenusText]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCRemoveSubGenusText]

GO

/*===========================================================================*\
  Description:	Remove unwanted sub genus from taxon names

  Parameters:
	The Taxon Name  @RecName

  Created:	July 2012
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCRemoveSubGenusText]
(@RecName varchar(100))
RETURNS varchar(100)
AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(100)

BEGIN 
   if (charindex('(', @RecName) > 1) AND (charindex(')',@RecName) > charindex('(',@RecName) + 1)  AND (charindex(')',@RecName) <> len(@REcName) )
   set @RETURNSTRING =  LEFT(@Recname, CHARINDEX(' (', @Recname)) + RIGHT(  @Recname,(LEN(@Recname) -(CHARINDEX(') ', @Recname) + 1 )  ))
   Else
   set @RETURNSTRING = @RecName
END 

RETURN @ReturnString
END

GO
GRANT EXECUTE ON [dbo].[LCRemoveSubGenusText] TO PUBLIC

