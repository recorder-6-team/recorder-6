If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[LCOSGTBENtoWSG84LL]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
   DROP FUNCTION [dbo].[LCOSGTBENtoWSG84LL] 
GO   
   
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[LCOSGBLLtoWSG84LL]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
   DROP FUNCTION [dbo].[LCOSGBLLtoWSG84LL]

Go
/* Functions for converting OSGB36 Lat Long to WGS  */
/****** Object:  UserDefinedFunction [dbo].[LCOSGTBENtoWSG84LL]    Script Date: 10/25/2015 19:29:37 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Function to return a WSG84 Lat or Long given a UK E/N 
         	if not valid returns null 
  Parameters:
		@Eastings - UK Eastings in m. 
                @Northings - UK Northing in  
                @isLat if 1 returns Lat if 0 then Long 
  Returns Lat or Long 

  Created:	October 2015

           

  Author: MikeWeideli

\*=========================================================================== */
CREATE FUNCTION [dbo].[LCOSGTBENtoWSG84LL] 
  (@Eastings float, @Northings  float, @iSLat bit )

Returns float     
AS
BEGIN
--************************************************************
declare @a float
declare @b float 
declare @F0 float 
declare @lat0 float 
declare @lon0 float
declare @N0 float
declare @E0 float
declare @e2 float
declare @lat float
declare @M float
declare @M1 float
declare @M2 float
declare @M3 float
declare @M4 float
declare @nu float
declare @rho float
declare @eta2 float 
declare @seclat float
declare @VII float
declare @VIII float
declare @IX float
declare @X float
declare @XI float
declare @XII float
declare @XIIA float
declare @dE float
declare @lat_1 float
declare @lon_1 float
declare @H float
declare @x_1 float
declare @y_1 float
declare @z_1 float
declare @s float
declare @tx float 
declare @ty float
declare @tz float 
declare @rxs float
declare @rys float
declare @rzs float 
declare @rx float 
declare @ry float 
declare @rz float
declare @x_2 float 
declare @y_2 float
declare @z_2 float 
declare @a_2 float 
declare @b_2 float
declare @e2_2 float 
declare @p float
declare @temp1 float
declare @temp2 float
declare @nu_2 float
declare @LatOld float
declare @Lon float
declare @Long float
declare @Returnvalue float 
declare @n float
declare @check float 
declare @PI float

set @PI = 3.14159265358979
set @a = 6377563.396
set @b = 6356256.909
set @F0 = 0.9996012717
set @lat0 = 49*@PI/180 
set @lon0 = -2*@PI/180
set @N0 = -100000
Set @E0 = 400000
Set @e2 = 1 - (@b*@b)/(@a*@a)
Set @n = (@a-@b)/(@a+@b);
set @lat = @lat0;
set @M = 0;



while (@Northings-@N0-@M >= 0.00001)
Begin 

  set @lat = (@Northings-@N0-@M)/(@a*@F0) + @lat
  set @M1 = (1 + @n + (5./4)* power(@n,2) + (5/4)*power(@n,3)) * (@lat-@lat0);
  set @M2 = (3*@n + 3*power(@n,2) + (21/8)*power(@n,3)) * sin(@lat-@lat0) * cos(@lat+@lat0);
  set @M3 = ((15./8)*power(@n,2) + (15/8)*power(@n,3)) * sin(2*(@lat-@lat0)) * cos(2*(@lat+@lat0));
  set @M4 = (35./24)*power(@n,3) * sin(3*(@lat-@lat0)) * cos(3*(@lat+@lat0));
  set @M = @b * @F0 * (@M1 - @M2 + @M3 - @M4) ;

End



Set @nu = @a*@F0/sqrt(1-@e2*power(sin(@lat),2));



set  @rho = @a*@F0*(1-@e2)*power((1-@e2*power(sin(@lat),2)),(-1.5));
set  @eta2 = @nu/@rho-1
set  @secLat = 1./cos(@lat);
set  @VII = tan(@lat)/(2*@rho*@nu);
set  @VIII = tan(@lat)/(24*@rho*power(@nu,3))*(5+3*power(tan(@lat),2)+@eta2-9*power(tan(@lat),2)*@eta2);
set  @IX = tan(@lat)/(720*@rho*power(@nu,5))*(61+90*power(tan(@lat),2)+45*power(tan(@lat),4));
set  @X = @secLat/@nu;
set  @XI = @secLat/(6*power(@nu,3))*(@nu/@rho+2*power(tan(@lat),2));
set  @XII = @secLat/(120*power(@nu,5))*(5+28*power(tan(@lat),2)+24*power(tan(@lat),4));
set  @XIIA = @secLat/(5040*power(@nu,7))*(61+662*power(tan(@lat),2)+1320*power(tan(@lat),4)+720*power(tan(@lat),6));
set  @dE = @Eastings-@E0;

set @lat_1 = @lat - @VII*power(@dE,2) + @VIII*power(@dE,4) - @IX*power(@dE,6);
set @lon_1 = @lon0 + @X*@dE - @XI*power(@dE,3) + @XII*power(@dE,5) - @XIIA*power(@dE,7);



set  @H = 0 
set @x_1 = (@nu/@F0 + @H)*cos(@lat_1)*cos(@lon_1);
set @y_1 = (@nu/@F0+ @H)*cos(@lat_1)*sin(@lon_1);
set @z_1 = ((1-@e2)*@nu/@F0 +@H)*sin(@lat_1);




set @s = -20.4894*power(10,-6)
set @tx = 446.448
set @ty = -125.157
set @tz = 542.060
set @rxs = 0.1502
set @rys = 0.2470
set @rzs = 0.8421
set @rx = @rxs*@PI/(180*3600)
set @ry= @rys*@PI/(180*3600)
set @rz = @rzs*@PI/(180*3600)
set @x_2 = @tx + (1+@s)*@x_1 + (-@rz)*@y_1 + (@ry)*@z_1;
set @y_2 = @ty + (@rz)*@x_1 + (1+@s)*@y_1 + (-@rx)*@z_1;
set @z_2 = @tz + (-@ry)*@x_1 + (@rx)*@y_1 + (1+@s)*@z_1;

set @a_2 = 6378137.000
set @b_2 = 6356752.3141
set @e2_2 = 1- (@b_2*@b_2)/(@a_2*@a_2)
set @p = sqrt(power(@x_2,2) + power(@y_2,2));


set @lat = atn2(@z_2,(@p*(1-@e2_2)))
set @latold = 2*@PI;

while (abs(@lat - @latold)>power(10,-16))

Begin
  set @temp1 = @lat
  set @temp2 = @latold
  set @latold = @temp1
  set @lat = @temp2
  set @lat = @latold
  set @latold = @lat
  set @nu_2 = @a_2/sqrt(1-@e2_2*power(sin(@latold),2));
  set @lat = atn2(@z_2+@e2_2*@nu_2*sin(@latold), @p);
End


set @lon = atn2(@y_2,@x_2);
set @H = @p/cos(@lat) - @nu_2;


set @lat = @lat*180/@PI;
set @lon = @lon*180/@PI;


If @isLat = 1 
   set @ReturnValue  =  @lat
 else 
   set @ReturnValue =  @lon

RETURN @ReturnValue
 
END

GO

GRANT EXECUTE ON [dbo].[LCOSGTBENtoWSG84LL] TO PUBLIC

USE [NBNData]
GO

/****** Object:  UserDefinedFunction [dbo].[LCOSGBLLtoWSG84LL]    Script Date: 10/25/2015 19:33:59 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Function to return a WSG84 Lat or Long given a 
                OSGB36 Lat/Long 
  Parameters:
		@Latitude (Decimal)
        @Longitude (Decimal)
        @isLat if 1 returns Lat if 0 then Long 
  Returns Lat or Long WSG84 as decimal 
  Note that thsi first converts the OSGB Lat/Long to Eastings/Northings
  
  Created:	October 2015
          

  Author: MikeWeideli

\*=========================================================================== */
CREATE FUNCTION [dbo].[LCOSGBLLtoWSG84LL] 
  (@Latitude float, @Longitude  float, @iSLat bit )

Returns float     
AS
BEGIN
--************************************************************
declare @Eastings varchar(20)
declare @Northings varchar(20)
declare @EastingsNumeric integer
declare @NorthingsNumeric integer
declare @ReturnValue float 

set @Eastings = dbo.LCOSGBLLToOSGBEN(@Latitude,@Longitude,',',1)
Set @Northings = dbo.LCOSGBLLToOSGBEN(@Latitude,@Longitude,',',2)

if ISNUMERIC(@Eastings) = 1  set @EastingsNumeric = cast(@Eastings as Int)

if ISNUMERIC(@Northings) = 1  set @NorthingsNumeric = cast(@Northings as Int)

if @islat = 1  
 set  @ReturnValue =  dbo.LCOSGTBENtoWSG84LL(@EastingsNumeric,@NorthingsNumeric,1)
else
 set  @ReturnValue =  dbo.LCOSGTBENtoWSG84LL(@EastingsNumeric,@NorthingsNumeric,0)


RETURN @ReturnValue
 
END

GO

GRANT EXECUTE ON [dbo].[LCOSGBLLtoWSG84LL] TO PUBLIC