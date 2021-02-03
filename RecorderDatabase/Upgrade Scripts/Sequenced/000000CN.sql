/****** Object:  UserDefinedFunction [dbo].[LCOSGBLLtoWSG84LL]    Script Date: 02/02/2021 19:21:51 ******/
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
  Note that this first converts the OSGB Lat/Long to Eastings/Northings
  Lat Longs outside the OSGB area are not converted 
  
  Created:	October 2015
          

  Author: MikeWeideli

\*=========================================================================== */
ALTER FUNCTION [dbo].[LCOSGBLLtoWSG84LL] 
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
if @islat = 1  
 set  @ReturnValue = @Latitude
else
 set  @ReturnValue =  @Longitude
 
if @Latitude > 49 
BEGIN
 set @Eastings = dbo.LCOSGBLLToOSGBEN(@Latitude,@Longitude,',',1)
 Set @Northings = dbo.LCOSGBLLToOSGBEN(@Latitude,@Longitude,',',2)

 if ISNUMERIC(@Eastings) = 1  set @EastingsNumeric = cast(@Eastings as Int)

 if ISNUMERIC(@Northings) = 1  set @NorthingsNumeric = cast(@Northings as Int)

 if @islat = 1  
   set  @ReturnValue =  dbo.LCOSGTBENtoWSG84LL(@EastingsNumeric,@NorthingsNumeric,1)
 else
   set  @ReturnValue =  dbo.LCOSGTBENtoWSG84LL(@EastingsNumeric,@NorthingsNumeric,0)
END 

RETURN @ReturnValue
 
END







