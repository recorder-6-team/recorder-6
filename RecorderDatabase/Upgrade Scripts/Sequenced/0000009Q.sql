SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO
/*============================================================================*\
	Mantis 495: Transact SQL to Convert Lat/Long to OSGB Eastings/Northings 
\*============================================================================*/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCOSGBLLToOSGBEN]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCOSGBLLToOSGBEN]


/****** Object:  UserDefinedFunction [dbo].[LCOSGBLLToOSGBEN]    Script Date: 17/11/2014 14:45:59 ******/

GO

/*===========================================================================*\
  Description:	Function to return an OSGB Eastings or Northings from a OSGB Lat/Long
         	if not valid returns ''
  Parameters:
		@Latitude - latitude in decimal format 
                @Longitude in decimal format  
		@Separator text
		@Output 1 = Eastings 2 = Northings 3 = Eastings/Separator/Northings 
  Created:	Nov 2014

        

  Author: MikeWeideli

\*=========================================================================== */
CREATE FUNCTION [dbo].[LCOSGBLLToOSGBEN] 
(@Latitude float, @Longitude float, @Separator char(1),@Output integer)
RETURNS varchar(20)

AS
BEGIN

Declare @LatW float
Declare @LonW float
Declare @a float
Declare @b float 
Declare @f float
Declare @lat0 float 
Declare @lon0 float
Declare @n0 float
Declare @e float
Declare @e2 float
Declare @n float
Declare @n2 float
Declare @n3 float
Declare @CosLat float
Declare @SinLat float
Declare @Nu float
Declare @eSq float
Declare @rho float
Declare @eta2 float
Declare @FiveFour float
Declare @FifteenEight float
Declare @TwentyoneEight float
Declare @ThirtyFiveTwentyFour float
Declare @M float
Declare @ma float
Declare @mb float
Declare @mc float
Declare @md float
Declare @lattrueorigin float
Declare @longtrueorigin float

Declare @cos3Lat float
Declare @cos5Lat float
Declare @Tan2Lat float
Declare @Tan4Lat float
Declare @I float
Declare @II float
Declare @III float
Declare @IIIa float
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
Declare @Two float
Declare @TwentyFour float
Declare @SevenTwoZero float
Declare @OneTwoZero float
Declare @EastingsInt Integer
Declare @NorthingsInt Integer

Declare @Result varchar(20)

If @Latitude is not null and @Longitude is not null
Begin
    If  @Latitude  = 0 set @Latitude = .0000001
	set  @Lattrueorigin = 49
	set  @Longtrueorigin = -2
	set  @a =  6377563.396
	set  @b =  6356256.908 
	set  @f = 0.9996012717
	set  @lat0 = radians(@Lattrueorigin)
	set  @lon0 = radians(@Longtrueorigin)
	set  @latw = radians(@Latitude)
	set  @lonw = radians(@Longitude)


	set  @n0 = -100000
	set  @e = 400000
	set  @e2 = 1 -(@b*@b)/ (@a*@a)
	set  @n = (@a-@b) /(@a+@b)
	set  @n2 = @n*@n
	set  @n3 = @n*@n*@n
	set  @CosLat = cos(@latW)
	set  @SinLat = sin(@latW)
	Set  @Nu = @A*@f / sqrt(1-@e2 * @sinLat * @sinlat)
	set  @rho = @A*@f*(1-@e2) /((1-@e2 * @sinlat) * Power(@sinlat , 1.5))
	set  @eta2 = @Nu/@rho -1 
	set  @FiveFour = 1.25
	set  @FifteenEight = 1.875
	set  @TwentyOneEight = 2.625
	Set  @ThirtyFiveTwentyFour = 1.45833333333333
	set  @Two = 2
	set  @TwentyFour = 24
	set  @SevenTwoZero = 720
	set  @OneTwoZero = 120

	set @Ma = (1 + @N + (@FiveFour * @n2) + (@FiveFour * @N3)) * (@latw - @lat0)
	set @Mb = ((3 * @N) + (3 * @N2) + (@TwentyOneEight * @N3)) * Sin(@latw - @lat0) * Cos(@latw + @lat0)
	set @Mc = ((@Fifteeneight * @N2) + (@Fifteeneight * @N3)) * Sin(2 * (@latw - @lat0)) * Cos(2 * (@latw + @lat0))
	set @Md = @ThirtyFiveTwentyFour * @N3 * Sin(3 * (@latw - @lat0)) * Cos(3 * (@latw + @lat0))
	set @M = @b * @F * (@Ma - @Mb + @Mc - @Md)
 


	set @Cos3lat = @Coslat * @Coslat * @Coslat
	set @Cos5lat = @Cos3lat * @Coslat * @Coslat
	set @tan2lat =  tan(@latw) * tan(@latw)
	set @tan4Lat = @tan2lat * @tan2lat

	set @I = @M + @N0
	set @II = (@Nu / @two) * @SinLat * @Coslat
	set @III = (@Nu / @twentyFour) * @SinLat * @Cos3lat * (5 - @tan2lat + 9 * @Eta2)
	set @IIIA = (@Nu / @SevenTwoZero) * @SinLat * @Cos5lat * (61 - 58 * @tan2lat + @tan4Lat)
	set @IV = @Nu * @Coslat
	set @V = (@Nu / 6) * @Cos3lat * (@Nu / @rho - @tan2lat)
	set @VI = (@Nu / @OneTwoZero) * @Cos5lat * (5 - 18 * @tan2lat + @tan4Lat + 14 * @Eta2 - 58 * @tan2lat * @Eta2)

	set @DLon = @lonw - @lon0
	set @DLon2 = @DLon * @DLon
	set @DLon3 = @DLon2 * @DLon
	set @DLon4 = @DLon3 * @DLon
	set @DLon5 = @DLon4 * @DLon
	set @DLon6 = @DLon5 * @DLon
 
	set @Northings = @I + @II * @DLon2 + @III * @DLon4 + @IIIA * @DLon6 

	set @Eastings =  @E + @IV * @DLon + @V * @DLon3 + @VI * @DLon5  

	set @EastingsInt = ceiling(@Eastings)
	set @NorthingsInt = ceiling(@Northings)


	if @Output = 2 
	  set @result = cast(@NorthingsInt as  varchar(10)) 
	else if @output = 1
	 set @result = cast(@EastingsInt as varchar(10))
	else 
	  set @result = cast(@EastingsInt as  varchar(10))  + @Separator +   cast(@NorthingsInt as varchar(10))
end
else
     set @Result = ''  
  
     Return @Result
End

GO

GRANT EXECUTE ON [dbo].[LCOSGBLLToOSGBEN]  TO PUBLIC