IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnWithinCircle]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION [dbo].[LCReturnWithinCircle]

GO
/****** Object:  UserDefinedFunction [dbo].[LCReturnWithinCircle]    Script Date: 04/03/2016 15:42:43 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[LCReturnWithinCircle]
(@GridRef varchar (20),@sRType as varchar(20), @CSpatialRef as varchar(20), @SRadius as varchar(20), @iCentre as integer )
RETURNS  integer


--	PARAMETERS

--	NAME			DESCRIPTION
--	@GridREf		Spatial ref from Sample
--	@sRType			Spatial ref type from Sample
--	@CSpatialRef		The spatial ref (must be GB or Irish Grid Ref.
--	@sRadius		Radius of circle in metres
--	@iCentre 		If 0 then SW corner.  If 1 then centre of square. If 2 then any corner or centre, 
--				If 3 then all 4 corners	
--	Returns a 1 if the grid ref is within the circle defined by the centre of the circle
--  	and radius in metres. Needs the LCReturnNorthingsV2 and   LCReturnEastingV2 functions to work.
--  	Will automatically work out if supplied ref is Ireland. If so only Irish references wil be included.  

--	AUTHOR:	Mike Weideli, Littlefield Consultancy
--	CREATED:  05/12/2007
--	UPDATED 27/01/2008 to include OSNI option
--	Updated 29/01/2008 to cover additional corners
AS
BEGIN
--****************************************************************************************************
--constants

declare @ReturnNumeric Integer
declare @SWEastings  integer
declare @SWNorthings  Integer
declare @CentreEastings  integer
declare @CentreNorthings  Integer
declare @NWEastings  integer
declare @NWNorthings  Integer
declare @SEEastings  integer
declare @SENorthings  Integer
declare @NEEastings  integer
declare @NENorthings  Integer
 
declare @bAng  float
declare @fNorth  float
declare @fEast  float
declare @CEastings integer
declare @CNorthings integer
declare @SWDistance  float
Declare @CentreDistance Float
Declare @NEDistance Float
Declare @NWDistance Float
Declare @SEDistance Float
Declare @CentreOffset Float
declare @Radius  integer
declare @InputSRType char(4)
set @ReturnNumeric = 0
set @InputSRType = 'OSGB'

if @Gridref is null set @SRType = 'INVALID'
 
if isnumeric(substring(@CSpaTialRef,2,1)) = 1
  set @InputSRType = 'OSNI'
 
 

if @SRType = @InputSRType  
begin
  set @cEastings =   dbo.LCReturnEastingsV2(UPPER(@CSpatialRef),@InputSRType,0)               
  set @cNorthings  =   dbo.LCReturnNorthingsV2(UPPER(@CSpatialRef),@InputSRType,0)               
  set @radius = cast(@SRadius as integer)      
       		
  set @SWNorthings = dbo.LCReturnNorthingsV2(@GridRef,@SRtype,0) 
  set @SWEastings = dbo.LCReturnEastingsV2(@GridRef,@SRtype,0) 
  set @fNorth = Abs(@SWNorthings-@CNorthings) 
  set @fEast = Abs(@SWEastings-@CEastings)
  set @SwDistance = SQRT((@fnorth * @fnorth) + (@feast * @feast))                  
  if @iCentre = 0 and @Radius >= @SWDistance set @ReturnNumeric = 1	
  
  If @iCentre > 0 
  begin
    set @CentreNorthings = dbo.LCReturnNorthingsV2(@GridRef,@SRtype,1) 
    set @CentreEastings = dbo.LCReturnEastingsV2(@GridRef,@SRtype,1)  
    set @fNorth = Abs(@CentreNorthings-@CNorthings) 
    set @fEast = Abs(@CentreEastings-@CEastings)  
    set @CentreDistance= SQRT((@fnorth * @fnorth) + (@feast * @feast))  
    set @CentreOffset = @CentreNorthings-@SWNorthings
    if @iCentre = 1 and @Radius >= @CentreDistance set @ReturnNumeric = 1	
  end
  if @iCentre > 1
    begin
      set @NWNorthings = @CentreNorthings + @CentreOffset
      set @NENorthings = @NWNorthings
      set @SENorthings = @SWNorthings
      set @NEEastings = @CentreEastings + @CentreOffset
      set @SEEastings = @NEEastings
      set @NWeastings = @SWEastings
      set @fNorth = Abs(@NWNorthings-@CNorthings) 
      set @fEast = Abs(@NWEastings-@CEastings)  
      set @NWDistance= SQRT((@fnorth * @fnorth) + (@feast * @feast))  
      set @fNorth = Abs(@NENorthings-@CNorthings) 
      set @fEast = Abs(@NEEastings-@CEastings)  
      set @NEDistance= SQRT((@fnorth * @fnorth) + (@feast * @feast))  
      set @fNorth = Abs(@SENorthings-@CNorthings) 
      set @fEast = Abs(@SEEastings-@CEastings)  
      set @SEDistance= SQRT((@fnorth * @fnorth) + (@feast * @feast))  
      If @iCentre = 2
        begin      
          if @Radius >= @SWDistance OR  @Radius >= @NWDistance
          OR @Radius >= @NEDistance OR @Radius >= @SEDistance set @ReturnNumeric = 1
        end 
        else
          if @Radius >= @SWDistance AND  @Radius >= @NWDistance
          AND @Radius >= @NEDistance AND @Radius >= @SEDistance set @ReturnNumeric = 1
   end    
end 

 
RETURN   @ReturnNumeric

END
GO

GRANT EXECUTE ON [dbo].[LCReturnWithinCircle] to Public

GO

IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnDistanceCircle]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION [dbo].[LCReturnDistanceCircle]

GO
/****** Object:  UserDefinedFunction [dbo].[LCReturnDistanceCircle]      Script Date: 04/03/2016 15:42:43 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE FUNCTION [dbo].[LCReturnDistanceCircle]
(@GridRef varchar (20),@sRType as varchar(20), @CSpatialRef as varchar(20), @SRadius as varchar(20), @iCentre as integer )
RETURNS  integer


--	PARAMETERS

--	NAME			DESCRIPTION
--	@GridREf		Spatial ref from Sample
--	@sRType			Spatial ref type from Sample
--	@CSpatialRef		The spatial ref (must be GB or Irish Grid Ref.
--	@sRadius		Radius of circle in metres
--	@iCentre 		If 0 then SW corner.  If 1 then centre of square. If 2 then any corner or centre, 
--				If 3 then all 4 corners	
--	Returns the distance from the point specified to the sw corner of the grid square if 
--  if the grid ref is within the circle defined by the centre of the circle
--  and radius in metres. 
--  Returns  -1 if not within  
--  Needs the LCReturnNorthingsV2 and   LCReturnEastingV2 functions to work.
--  Will automatically work out if supplied ref is Ireland. If so only Irish references wil be included.  

--	AUTHOR:	Mike Weideli, Littlefield Consultancy
--	CREATED:  02/04/2016
AS
BEGIN
--****************************************************************************************************
--constants

declare @ReturnNumeric Integer
declare @SWEastings  integer
declare @SWNorthings  Integer
declare @CentreEastings  integer
declare @CentreNorthings  Integer
declare @NWEastings  integer
declare @NWNorthings  Integer
declare @SEEastings  integer
declare @SENorthings  Integer
declare @NEEastings  integer
declare @NENorthings  Integer
 
declare @bAng  float
declare @fNorth  float
declare @fEast  float
declare @CEastings integer
declare @CNorthings integer
declare @SWDistance  float
Declare @CentreDistance Float
Declare @NEDistance Float
Declare @NWDistance Float
Declare @SEDistance Float
Declare @CentreOffset Float
declare @Radius  integer
declare @InputSRType char(4)
set @ReturnNumeric = 0
set @InputSRType = 'OSGB'

if @Gridref is null set @SRType = 'INVALID'
 
if isnumeric(substring(@CSpaTialRef,2,1)) = 1
  set @InputSRType = 'OSNI'
 
 

if @SRType = @InputSRType  
begin
  set @cEastings =   dbo.LCReturnEastingsV2(UPPER(@CSpatialRef),@InputSRType,0)               
  set @cNorthings  =   dbo.LCReturnNorthingsV2(UPPER(@CSpatialRef),@InputSRType,0)               
  set @radius = cast(@SRadius as integer)      
       		
  set @SWNorthings = dbo.LCReturnNorthingsV2(@GridRef,@SRtype,0) 
  set @SWEastings = dbo.LCReturnEastingsV2(@GridRef,@SRtype,0) 
  set @fNorth = Abs(@SWNorthings-@CNorthings) 
  set @fEast = Abs(@SWEastings-@CEastings)
  set @SwDistance = SQRT((@fnorth * @fnorth) + (@feast * @feast))                  
  if @iCentre = 0 and @Radius >= @SWDistance set @ReturnNumeric = 1	
  
  If @iCentre > 0 
  begin
    set @CentreNorthings = dbo.LCReturnNorthingsV2(@GridRef,@SRtype,1) 
    set @CentreEastings = dbo.LCReturnEastingsV2(@GridRef,@SRtype,1)  
    set @fNorth = Abs(@CentreNorthings-@CNorthings) 
    set @fEast = Abs(@CentreEastings-@CEastings)  
    set @CentreDistance= SQRT((@fnorth * @fnorth) + (@feast * @feast))  
    set @CentreOffset = @CentreNorthings-@SWNorthings
    if @iCentre = 1 and @Radius >= @CentreDistance set @ReturnNumeric = 1	
  end
  if @iCentre > 1
    begin
      set @NWNorthings = @CentreNorthings + @CentreOffset
      set @NENorthings = @NWNorthings
      set @SENorthings = @SWNorthings
      set @NEEastings = @CentreEastings + @CentreOffset
      set @SEEastings = @NEEastings
      set @NWeastings = @SWEastings
      set @fNorth = Abs(@NWNorthings-@CNorthings) 
      set @fEast = Abs(@NWEastings-@CEastings)  
      set @NWDistance= SQRT((@fnorth * @fnorth) + (@feast * @feast))  
      set @fNorth = Abs(@NENorthings-@CNorthings) 
      set @fEast = Abs(@NEEastings-@CEastings)  
      set @NEDistance= SQRT((@fnorth * @fnorth) + (@feast * @feast))  
      set @fNorth = Abs(@SENorthings-@CNorthings) 
      set @fEast = Abs(@SEEastings-@CEastings)  
      set @SEDistance= SQRT((@fnorth * @fnorth) + (@feast * @feast))  
      If @iCentre = 2
        begin      
          if @Radius >= @SWDistance OR  @Radius >= @NWDistance
          OR @Radius >= @NEDistance OR @Radius >= @SEDistance set @ReturnNumeric = 1
        end 
        else
          if @Radius >= @SWDistance AND  @Radius >= @NWDistance
          AND @Radius >= @NEDistance AND @Radius >= @SEDistance set @ReturnNumeric = 1
   end    
end 

If @ReturnNumeric = 0 
  set @ReturnNumeric = -1
else
  set @ReturnNumeric = @SWDistance
      
RETURN   @ReturnNumeric

END

GO

GRANT EXECUTE ON [dbo].[LCReturnDistanceCircle] to Public