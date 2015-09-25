/****** Object:  UserDefinedFunction [dbo].[LCOSGBLLToGRUK]    Script Date: 07/14/2012 21:17:52 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCOSGBLLToGRUK]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCOSGBLLToGRUK]

GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Return either Eastings or Northings from an OSGB36 Latitude or the Longitude
  If not valid returns NULL
  
  Based on a scripts by  (c) Chris Veness 2005-2012            
  www.movable-type.co.uk/scripts/coordtransform.js                                           
  www.movable-type.co.uk/scripts/latlon-convert-coords.html   
  and adapted using additional information from other sources     
  
  Parameters:
	Latitude (Decimal) @Latitude  
        Longitude (Decimal) @Longitude  
  Author: MikeWeideli 

\*=========================================================================== */


CREATE FUNCTION [dbo].[LCOSGBLLToGRUK] 
(@Latitude float, @Longitude float,@iSEast bit)
RETURNS float
AS
BEGIN

DECLARE @ReturnValue float
Declare @lat float
Declare @lon float
Declare @a float
Declare @b float  
Declare @F0 float
Declare @lat0 float
Declare @lon0 float
Declare @N float 
Declare @E2 float
Declare @E0 float
Declare @N0 float 
Declare @Coslat float
Declare @SinLat float
Declare @N2 float
Declare @N3 float
Declare @rho float
Declare @Nu float
Declare @Eta2 float
Declare @Ma float 
Declare @Mb float
Declare @Mc float
Declare @Md float
Declare @M float
Declare @Cos3lat float
Declare @Cos5lat float
Declare @tan2lat float
Declare @tan4Lat float
Declare @I float
Declare @II float
Declare @III float
Declare @IIIA float
Declare @IV float
Declare @V float
Declare @VI float
Declare @DLon float
Declare @DLat float
Declare @DLon2 float
Declare @DLon3 float
Declare @DLon4 float
Declare @DLon5 float
Declare @DLon6 float
Declare @Northings float
Declare @Eastings float
Declare @FiveFour float
Declare @Fifteeneight float 
Declare @TwentyOneEight float 
Declare @ThirtyFiveTwentyFour float 
Declare @Lattrueorigin float
Declare @Longtrueorigin  float
set @lat = RADIANS(@Latitude) 
set @lon = RADIANS(@Longitude) 
set @Lattrueorigin = 49
set @Longtrueorigin = -2
-- set   @a = 6375020.481 
-- set   @b = 6353722.49  
set @a = 6377563.396
set @b = 6356256.909
set @F0 = 0.9996012717                         
set @lat0 = RADIANS(@Lattrueorigin)
set @lon0 = RADIANS(@Longtrueorigin)
Set @N0 = -100000
Set @E0 = 400000                
set @e2 = 1 - (@b*@b)/(@a*@a)              
set @n = (@a-@b)/(@a+@b)
Set @n2 = @n*@n
Set @n3 = @n*@n*@n
set @cosLat = cos(@lat)
set @sinLat = sin(@lat)
set @nu = @a*@F0/sqrt(1-@e2*@sinLat*@sinLat)
set @rho = @a*@F0*(1-@e2)/POWER(1-@e2*@sinLat*@sinLat, 1.5)  
set @eta2 = @nu/@rho-1
set @FiveFour = 1.25
set  @Fifteeneight = 1.875 
set  @TwentyOneEight = 2.625
set  @ThirtyFiveTwentyFour = 1.45833333333333333
set @Ma = (1 + @n + (@FiveFour*@n2) + (@FiveFour*@n3)) * (@lat-@lat0)
set @Mb = ((3*@n) + (3*@n2) + (@TwentyOneEight*@n3)) * sin(@lat-@lat0) * cos(@lat+@lat0)
set @Mc = ((@FifteenEight*@n2) + (@FifteenEight*@n3)) * sin(2*(@lat-@lat0)) * cos(2*(@lat+@lat0))
set @Md = @ThirtyFiveTwentyFour*@n3 * sin(3*(@lat-@lat0)) * cos(3*(@lat+@lat0))
set @M = @b * @F0 * (@Ma - @Mb + @Mc - @Md) 

set @cos3lat = @cosLat*@cosLat*@cosLat
set @cos5lat = @cos3lat*@cosLat*@cosLat
set @tan2lat = tan(@lat)* tan(@lat)
set @tan4lat = @tan2lat*@tan2lat

set @I = @M + @N0
set @II = (@nu/2.0000000)*@sinLat*@cosLat
set @III = (@nu/24.000000)*@sinLat*@cos3lat*(5-@tan2lat+9*@eta2)
set @IIIA = (@nu/720.000000)*@sinLat*@cos5lat*(61-58*@tan2lat+@tan4lat)
set @IV = @nu*@cosLat
set @V = (@nu/6)*@cos3lat*(@nu/@rho-@tan2lat)
set @VI = (@nu/120.00000000) * @cos5lat * (5 - 18*@tan2lat + @tan4lat + 14*@eta2 - 58*@tan2lat*@eta2)

set @dLon = @lon-@lon0
set @dLon2 = @dLon*@dLon
set @dLon3 = @dLon2*@dLon
set @dLon4 = @dLon3*@dLon
set @dLon5 = @dLon4*@dLon
set @dLon6 = @dLon5*@dLon
set @Northings = @I + @II*@dLon2 + @III*@dLon4 + @IIIA*@dLon6 
set @Eastings  = @E0 + @IV*@dLon + @V*@dLon3 + @VI*@dLon5

If @isEast = 1 

   set @ReturnValue  = @Eastings
else   
   set @ReturnValue = @Northings
RETURN @ReturnValue
 
END



GO

GRANT EXECUTE ON [dbo].[LCOSGBLLToGRUK] TO PUBLIC

