/****** Object:  UserDefinedFunction [dbo].[LCWSG84ToOSGTBLL]    Script Date: 07/14/2012 21:19:54 ******/

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCWSG84ToOSGTBLL]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCWSG84ToOSGTBLL]

GO



SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Function to return an OSGB Lat/Long given a WSG84 LL 
         	if not valid returns null 
  Parameters:
		@Latitude - latitude in decimal format 
                @Longitude in decimal format  
                @isLat -if 1 returns lat or 0 for long 

  Created:	July 2012

  Permissions
               Based with permission on a scripts by  (c) Chris Veness 2005-2012            
               www.movable-type.co.uk/scripts/coordtransform.js                                         
               www.movable-type.co.uk/scripts/latlon-convert-coords.html   
               and adapted using additional information from other sources             

  Author: MikeWeideli

\*=========================================================================== */
CREATE FUNCTION [dbo].[LCWSG84ToOSGTBLL] 
(@Latitude float, @Longitude float,@iSLat bit)
RETURNS float

AS
BEGIN
Declare @returnvalue float
Declare @LatW float
Declare @LonW float
Declare @a float
Declare @b float 
Declare @t float
Declare @sinPhi  float
Declare @cosPhi float
Declare @sinLambda float
Declare @cosLambda float
Declare @H float
Declare @eSq float
Declare @Nu float
Declare @x1 float
Declare @y1 float
Declare @z1 float
Declare @tx float
Declare @rx float
Declare @ry float
Declare @rz float
Declare @s1 float
Declare @x2 float
Declare @y2 float
Declare @z2 float
Declare @precision float
Declare @P float
Declare @phi float
Declare @Ty float
Declare @tz float
Declare @phiP float
declare @lambda float



set  @latW = RADIANS(@Latitude) 
set  @lonW = RADIANS(@Longitude) 


set  @a =  6378137
set  @b =  6356752.3142 
  
set @sinPhi = sin(@latW)
set @cosPhi = cos(@latW)
set @sinLambda = sin(@lonW)
set @cosLambda = cos(@lonW)
set @H = 24.7  --  for the moment
set @eSq = (@a*@a - @b*@b) / (@a*@a)
set @nu = @a / sqrt(1 - @eSq*@sinPhi*@sinPhi)


set @x1 = (@nu+@H) * @cosPhi * @cosLambda
set @y1 = (@nu+@H) * @cosPhi * @sinLambda
set @z1 = ((1-@eSq)*@nu + @H) * @sinPhi

set @tx =  -446.448
set @ty =   125.157
set @tz = -542.060
set @rx =  RADIANS ( -0.1502/3600)
set @ry =  RADIANS ( -0.2470/3600)
set @rz =  RADIANS (  -0.8421/3600)
set @s1 =  20.4894/1e6 + 1          -- normalise ppm to (s+1)

set @x2 = @tx + @x1*@s1 - @y1*@rz + @z1*@ry
set @y2 = @ty + @x1*@rz + @y1*@s1 - @z1*@rx
set @z2 = @tz - @x1*@ry + @y1*@rx + @z1*@s1

set  @a =  6377563.396
set  @b =  6356256.910

set  @precision = 4 / @a  --  results accurate to around 4 metres
set  @eSq = (@a*@a - @b*@b) / (@a*@a)
set  @p = sqrt(@x2*@x2 + @y2*@y2)
set  @phi = atn2(@z2, @p*(1-@eSq))
set  @phiP = 2*PI()
while (abs(@phi-@phiP) > @precision) 
BEGIN
    set @nu = @a / sqrt(1 - @eSq*sin(@phi)*sin(@phi))
    set @phiP = @phi
    set @phi = atn2(@z2 + @eSq*@nu*sin(@phi), @p)
END
set @lambda = atn2(@y2, @x2)
set @H = @p/cos(@phi) - @nu

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

If @isLat = 1 
   set @ReturnValue  =  DEGREES(@phi)
 else 
   set @ReturnValue =  DEGREES (@lambda)
 
RETURN @ReturnValue
 

    


END



GO


GRANT EXECUTE ON [dbo].[LCWSG84ToOSGTBLL] TO PUBLIC



