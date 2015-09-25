/****** Object:  UserDefinedFunction [dbo].[LCLLWSG84toGBGridRef]    Script Date: 07/20/2012 21:37:12 ******/

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCLLWSG84toGBGridRef]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCLLWSG84toGBGridRef]

GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Function to return an UK Grid Reference from a WSG84 Lat/Long
         	if not valid returns null 
  Parameters:
		@Latitude - latitude in decimal format 
                @Longitude in decimal format  
                @Precision - length of grid ref to be returned 

  Created:	July 2012
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCLLWSG84toGBGridRef] 
(@Latitude float, @Longitude float,@precision int)
RETURNS varchar(25)

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnValue varchar(25)
DECLARE @OSGBLat float
DECLARE @OSGBLong float
DECLARE @Eastings float
DECLARE @Northings float

Set @OSGBLat = dbo.LCWSG84ToOSGTBLL(@Latitude,@Longitude,1)
Set @OSGBLong = dbo.LCWSG84ToOSGTBLL(@Latitude,@Longitude,0)
Set @Eastings = dbo.LCOSGBLLToGRUK(@OSGBLat,@OSGBLong,1)
Set @Northings = dbo.LCOSGBLLToGRUK(@OSGBLat,@OSGBLong,0)

set @ReturnValue =  dbo.LCENtoGR(@Eastings,@Northings,@precision)

   
RETURN @ReturnValue
 
END


GO


GRANT EXECUTE ON [dbo].[LCLLWSG84toGBGridRef] TO PUBLIC