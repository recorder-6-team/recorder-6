

/*===========================================================================*\
  Drop udf before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnVagueDateShort]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCReturnVagueDateShort]
GO


/****** Object:  UserDefinedFunction [dbo].[LCReturnVagueDateShort]    Script Date: 04/07/2008 20:09:09 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


CREATE FUNCTION [dbo].[LCReturnVagueDateShort] 
(@VagueDateStart int, @VagueDateEnd int, @VagueDateType varchar(2))
RETURNS nvarchar(40)
/* A Shorter Version which returns the Recorder Vague Date in the the Recorder 6 format
    Mike Weideli 
    15 October 2005	

*/
AS
BEGIN
declare @MM1  nvarchar(9)
declare @MM2  nvarchar(9)
declare @RP1  nvarchar(40)
declare  @Z  int
declare @G  int
declare @H  int
declare @A  int
declare @B  int
declare @Y  int
declare @C  int
declare @M int
declare @D  int
declare @RD  int
declare @Y4  int
declare @RD1  int
declare @RM1  int
declare @RY1  int
declare @RDS1  nvarchar(2)
declare @RDS2  nvarchar(2)
declare @RD2  int
declare @RM2  int
declare @RY2  int
declare @RMS1  nvarchar(2)
declare @RMS2  nvarchar(2)
declare @X int
declare @CC1 int
declare @CC2 int
declare @VD nvarchar(2)
set @X = 0
while @X < 2 begin
    set @VD = @VagueDateType
    if ascii(right(@VD,1)) = 0
    set  @VD = left(@VagueDateType,1)

    set @RD = @VagueDateStart + 693594
	if  @X > 0 begin
              set @RD = @VagueDateEnd+693594
	
            end 

	set @Z =  @RD + 306
	set @H = 100*@Z-25
	set @A = FLOOR(@H/3652425)
	set @B = @A-FLOOR(@A/4)
	set @Y = FLOOR((100*@B+@H)/36525)
	set @C =@B+@Z-365*@Y-FLOOR(@Y/4)
	set @M = (5*@C+456)/153
	set @G = (153*@M-457)/5
	set @D = @C-@G
	if @M >12 
   		begin
                       set  @Y = @Y+1
  		set @M = @M-12
  	end 

  	if @X = 0
              begin 
              	set @RD1  =@D
		set @RM1 = @M
		set @RY1 = @Y
		set @CC1 = (@Y/100) +1

                if  @RD1 < 10 begin 
                           set @RDS1 = '0' + str(@RD1,1,0)
		end 
		else
		   begin 
		     set @RDS1 = str(@RD1,2,0)
	 	   end                     
		 if  @RM1 < 10 begin 
                           set @RMS1 = '0' + str(@RM1,1,0)
		end 
		else
		   begin 
		     set @RMS1 = str(@RM1,2,0)
	 	   end                     
		


              end 
              else
              begin              
 		set @RD2  = @D
		set @RM2 = @M
		set @RY2 = @Y
                set @CC2 = @Y/100
                if  @RD2 < 10 begin 
                           set @RDS2 = '0' + str(@RD2,1,0)
		end 
		else
		   begin 
		     set @RDS2 = str(@RD2,2,0)
	 	   end                     
		 if  @RM2 < 10 begin 
                           set @RMS2 = '0' + str(@RM2,1,0)
		end 
		else
		   begin 
		     set @RMS2 = str(@RM2,2,0)
	 	   end            
             end 
   	 set @X = @X + 1	         
END
   
        set @MM1  =  case @RM1
          when  1 then 'January'
 	      when   2 then 'February'
	      when 3 then 'March'
	      when 4 then 'April'
	      when 5 then 'May'
	      when 6 then 'June'
	      when 7 then 'July'
	      when 8 then 'August'
                  when 9 then 'September'
                  when 10 then 'October'
                  when 11 then 'November'
	      when 12 then 'December'
        end 
        set @MM2  =  case  @RM2
                  when  1 then 'January'
 	      when   2 then 'February'
	      when 3 then 'March'
	      when 4 then 'April'
	      when 5 then 'May'
	      when 6 then 'June'
	      when 7 then 'July'
	      when 8 then 'August'
                  when 9 then 'September'
                  when 10 then 'October'
                  when 11 then 'November'
	      when 12 then 'December'
         end 
         set  @RP1 = str(@RD1,2,0) + '/' + str(@RM1,2,0) + '/' + str(@RY1,4,0)

         

	set  @RP1 = case @VD
                     when 'D' then       @RDS1 + '/' + @RMS1 + '/' + str(@RY1,4,0)
                     when 'DD' then 	@RDS1 + '/' + @RMS1 + '/' + str(@RY1,4,0) +  '- ' + @RDS2 + '/' + @RMS2 + '/' + str(@RY2,4,0) 
                     when 'O' then       @MM1 + ' ' + str(@RY1,4,0)
	         when 'OO' then 	@MM1 + ' ' + str(@RY1,4,0) + ' - ' + @MM2 + ' ' + str(@RY2,4,0)
	         when 'Y' then        str(@RY1,4,0)
	         when 'YY' then       str(@RY1,4,0) +  ' - ' +  str(@RY2,4,0)
	         when '-Y' then         ' - '  +  	str(@RY2,4,0)
	         when 'Y-' then      	str(@RY1,4,0) + '  -  '
	         when 'C' then str(@CC1,2,0) + 'c'
                 when 'CC' then str(@CC1,2,0) + 'c - ' + str(@CC2,2,0) +'c'
                 when '-C' then ' - ' + str(@CC2,2,0) + 'c'
                 when 'C-' then str(@CC2,2,0) + 'c  -  '
                 when 'U' then 	'Unknown'	
                 
		  		  	

			

           end 
        	
           if @VD =  'P'  or @VD =  'S'  begin 
                         if @RM2= 2 begin  set @RP1 = 'Winter'
		 end  
		  if @RM2 = 8 begin  set @RP1 = 'Summer'
		 end
                          if @RM2 = 11  begin  set @RP1 = 'Autumn' 
		 end    
		 if @RM2 = 5  begin  set @RP1 =  'Spring'
		 end
                         if @VD = 'P' begin
 			set @RP1 = @RP1 + ' ' + str(@RY2,4,0)
		 end 
          end 
         




return @RP1

RETURN ''


END

GO
grant execute on [dbo].[LCReturnVagueDateShort]  to public
GO


IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCToRataDie]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCToRataDie]
GO


/****** Object:  UserDefinedFunction [dbo].[LCToRataDie]    Script Date: 04/06/2008 16:35:25 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



Create FUNCTION [dbo].[LCToRataDie]
(@DateString varchar(10))
RETURNS int

/* Takes a date in the format dd/mm/yyyy  and returns the number used in Recorder 6
    Mike Weideli  15October 2005
*/
AS
BEGIN

declare @RD int
declare @Y int
declare @M int
declare @D int
declare @A int
set @Y = cast(substring( @Datestring,7,4) as int)
set @M = cast(substring(@Datestring,4,2) as int)
set @D = cast(substring(@datestring,1,2) as int)



if @M < 3 
   Begin 
	set @M = @M + 12 
            set @Y = @Y - 1 
end 

set @A = 153*@M-457
set @RD = @D + @A/5 + 365*@Y + @Y/4 - @Y/100 + @Y/400 - 306
set @RD = @RD  - 693594 
 


return @RD



RETURN ''


END

GO
grant execute on [dbo].[LCToRataDie] to public
GO



IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnWithinCircle]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCReturnWithinCircle] 
GO





GO
/****** Object:  UserDefinedFunction [dbo].[LCReturnWithinCircle]    Script Date: 04/06/2008 16:35:02 ******/
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
declare @InEastings  integer
declare @InNorthings  Integer
declare @bAng  float
declare @fNorth  float
declare @fEast  float
declare @CEastings integer
declare @CNorthings integer
declare @Distance  float
declare @Radius  integer
declare @InputSRType char(4)
declare @iIn  integer
declare @iLoop  integer
set @ReturnNumeric = 0
set @InputSRType = 'OSGB'
set @iLoop = 0
if @Gridref is null 
   set @SRType = 'INVALID'
 
if isnumeric(substring(@CSpaTialRef,2,1)) = 1
    set @InputSRType = 'OSNI'
  
 

if @SRType = @InputSRType  

begin
	set @cEastings =   dbo.LCReturnEastingsV2(UPPER(@CSpatialRef),@InputSRType,0)               
    set @cNorthings  =   dbo.LCReturnNorthingsV2(UPPER(@CSpatialRef),@InputSRType,0)               
    set @radius = cast(@SRadius as integer)      

	 if @iCentre < 2 
	 begin
	
        		
       		set @InNorthings = dbo.LCReturnNorthingsV2(@GridRef,@SRtype,@iCentre) 
   			set @InEastings = dbo.LCReturnEastingsV2(@GridRef,@SRtype,@iCentre) 
     	

           	set @fNorth = Abs(@iNNorthings-@CNorthings) 
           	set @fEast = Abs(@iNEastings-@CEastings)
           	set @Distance = SQRT((@fnorth * @fnorth) + (@feast * @feast))                  

		                       
			if @Radius >= @Distance    
					set @ReturnNumeric = 1	
			 
	  end -- end 
      else
         begin
             if @iCentre = 2 
			begin
                while @iLoop < 4
			    begin
                       	set @InNorthings = dbo.LCReturnNorthingsV2(@GridRef,@SRtype,@iloop) 
        		    	set @InEastings = dbo.LCReturnEastingsV2(@GridRef,@SRtype,@iloop) 
     	            	set @fNorth = Abs(@iNNorthings-@CNorthings) 
                    	set @fEast = Abs(@iNEastings-@CEastings)
                    	set @Distance = SQRT((@fnorth * @fnorth) + (@feast * @feast))                  
                       	if @Radius >= @Distance    
		                begin
  							set @ReturnNumeric = 1	
							break
 						end 
						set @iLoop = @iLoop + 1
		     
			    end --			                         
       		end --
			else
			begin
                            SET @iLoop = 2 
			    set @ReturnNumeric = 1	
			    while @iLoop < 5
			    
			    begin
                              set @InNorthings = dbo.LCReturnNorthingsV2(@GridRef,@SRtype,@iloop) 
        		      set @InEastings = dbo.LCReturnEastingsV2(@GridRef,@SRtype,@iloop) 
     	                      set @fNorth = Abs(@iNNorthings-@CNorthings) 
                	      set @fEast = Abs(@iNEastings-@CEastings)
                	      set @Distance = SQRT((@fnorth * @fnorth) + (@feast * @feast))                  
                               if @Radius >= @Distance       
		                begin
  				    set @ReturnNumeric = 0	
				    break
 				End --
			    set @iLoop = @iLoop + 1
			    end --
			end			                         
         

              end
			      




end -- end    
      


      



 

RETURN   @ReturnNumeric



END

GO
grant execute on [dbo].[LCReturnWithinCircle] to public
GO




IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnTetrad]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCReturnTetrad] 
GO



/****** Object:  UserDefinedFunction [dbo].[LCReturnTetrad]    Script Date: 04/06/2008 16:33:54 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*
  
 * 
 * 
 * *****************  Version 2  *****************
 * User: MikeWeidlei
 * Returns a  Tetrad 
 
* only works for  OSGB and OSNI

* must be no spaces in spatial ref
*   
  */

CREATE FUNCTION [dbo].[LCReturnTetrad]
(@SpatialRef varchar(20), @SrType varchar(20) )
RETURNS  varchar(5)

	

--	PARAMETERS
--	NAME			DESCRIPTION
 
--
--	AUTHOR:	Mike Weidlei, Littlefield Consultancy
--	CREATED:  04/12/2007
--

AS
BEGIN
--****************************************************************************************************
--constants

declare @Tetrad  varchar(5)
declare @TetradAscii  int
declare @OOffset int
declare @COffset int
declare  @LenS int
declare  @Me  char(1)
declare @Mn  char(1) 

if @SrType = 'OSNI' 
  set @SpatialRef = 'I'+ @SpatialRef

set @Tetrad=''

if (@SRType  = 'OSGB'or @SRTYPE = 'OSNI' ) and len(Cast(@SpatialRef as varchar)) >  4
    

begin
      if len(@SpatialRef) = 5 
     begin
             set  @tetrad  = @SpatialRef
      end 
      else
        -- calculate tetrad
       set @Lens = len(@SpatialRef)
       
       set @Me = substring(@spatialref,4,1)
       set @Mn = substring(@spatialRef, (@lens-2)/2+4,1)
       if isnumeric(@Me) =1 and isnumeric(@Mn)  = 1
       begin
            if  (@Me > 3 and @Mn >7)  or (@Me >5)   
            begin
               set  @OOffset  = 1 
            end
            else
                set @OOffset = 0
            
            set @COffset = (((Floor(@Me/2) ) *  5 )+ @OOffset) 
            
            set  @TetradAscii =  (Floor(@Mn/2) + 65) +  @COffset          
            set @Tetrad = left(@spatialref,3) + substring(@spatialref, (@lens-2)/2 +3,1) +  char(@TetradAscii)
                 
                  
         end             

         if  @sRType = 'OSNI'        
           set @Tetrad = right(@tetrad,4)
         
          
 
      


      

end


 

RETURN   @Tetrad



END

GO
grant execute on [dbo].[LCReturnTetrad] to public
GO



IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnNorthingsV2]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCReturnNorthingsV2]
GO




set ANSI_NULLS ON
set QUOTED_IDENTIFIER ON
GO

/*  
 * 
 * 
 * *****************  Version 2  *****************
 * User: Mike Weideli
 * Returns  Northings as metres
* only works for  OSGB and OSNI . Works for letter starting H,J,N,O,S,T(UK)   and all Irish Letters 
* returns  0 if not OSGB or NI
* must be no spaces in spatial ref. Will return 0 or innvalid result 
* Northing are calculated from the false origin of the appropriate grid 
* so comparison in queries must use the spatial reference type 
 */

CREATE FUNCTION dbo.LCReturnNorthingsV2


(@SpatialRef varchar(20), @SrType varchar(20), @C bit )
RETURNS  int

	

--	AUTHOR:	Mike Weidlei, Littlefield Consultancy
--	CREATED:  10/01/2008
--	if @C = 1 is set to 1 then northings is centre of the square 
--

AS
BEGIN
--****************************************************************************************************
--constants

declare @Northings   int
declare @LetA  char(1)
declare @LetB char(1)
declare @LenS int
declare @LenT int
declare @ReturnNorthings int
declare @NumericPart varchar(6)
declare @Offset int
declare @Coffset int
Declare @offsett int
Declare @Tetrad  int
Declare @TetradLet  int
set @Tetrad = 0
set @ReturnNorthings = 0
set @coffset = 0
set @lent= 0

if @SRTYpe = 'OSNI'
begin
  set @SpatialRef = 'I' +  @SpatialRef
end


if @SRType = 'OSGB' or @SRType = 'OSNI'

begin

-- next line used for testing only      
--   set @SpatialRef = left(@spatialref,2)	 + "00"
        set @LetA     = left(@SpatialRef,1)
    	set @Northings = 0
    	  	  
        if  @LetA = 'N' or @LetA = 'O' 
    	begin
          		set @Northings  = 500000
    	end  
        if  @LetA = 'H' or @LetA = 'J' 
    	begin
          		set @Northings  = 1000000
    	end  

        set @LetB  = substring(@SpatialRef,2,1)
        
        


            if  @LetB  <  'I' 
           begin
               set  @Offset = 65   
         end 
         else
           set @Offset = 66   
            

    	
     	
        set @Northings  =  ((( Floor(((ascii(@letb)-@offset)/5)))-4)  * -100000)+ @Northings
       	set @Lent = len(@Spatialref)
     	set @LenS = (len(@SpatialRef)-2)/2
 
     	set @Numericpart =   substring(@SpatialRef,   @LenS + 3,@LenS)          
     	if  isnumeric (@Numericpart) = 1
	begin   
                       
                if @Lent = 5 
                   begin              
              
                      set @TetradLet  = ascii(substring(@SpatialRef,@Lent,1)) -65 
                      set @offsett = floor((@TetradLet-1)/13) 
                      set @TetradLet = @TetradLet - @Offsett
                      set @Tetrad = (@TetradLet%5)*2000 

     
                   end   
                   if @C = 1
                   begin
                     set @coffset = 	
		       case @Lent
                       when 12 then 0
                       when 10 then 5 
                       when 8 then 50  	
                       when 6 then 500
                       when 5 then 1000
                       when 4 then 5000
                       when 2 then 50000
                       else 0
                     end 

                   end  


                  
                   set @Returnnorthings = @northings  +  (@NumericPart * (POWER(10,5-@lens)) )   + @Tetrad + @coffset

     	end 

      

            
    
      


      

end


RETURN   @ReturnNorthings



END

GO
grant execute on dbo.LCReturnNorthingsV2 to public
GO


GO

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnMonth]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCReturnMonth]
GO



/****** Object:  UserDefinedFunction [dbo].[LCReturnMonth]    Script Date: 04/06/2008 16:33:25 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE FUNCTION [dbo].[LCReturnMonth]
(@Monthval varchar(3))
RETURNS varchar(3)

/* Returns the month for a date part of the month
    Author Michael Weideli - Littlefield Consultanct 14 October 2005
   

*/


AS
BEGIN
DECLARE @M int
set @M = cast(ltrim(@Monthval) as int)
  	     set @Monthval = case @M                    	         
		when 1   then 'Jan'
		when 2   then 'Feb'
                when 3   then 'Mar'
		when 4   then 'Apr'
                when 5   then 'May'
		when 6   then 'Jun'
                when 7   then 'Jul'
		when 8   then 'Aug'
                when 9   then 'Sep'
		when 10  then 'Oct'
                when 11  then 'Nov'
		when 12  then 'Dec'              





            end 
              
                            
  
	 



return @MonthVal


 



END


GO
grant execute on [dbo].[LCReturnMonth] to public
GO


IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnHectad]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCReturnHectad] 
GO


/****** Object:  UserDefinedFunction [dbo].[LCReturnHectad]    Script Date: 04/06/2008 16:32:39 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*
  
 * 
 * 
 * *****************  Version 2  *****************
 * User: Mike Weidlei
 * Returns a  Hectad 
 * 12 January 2008
* only works for  OSGB and OSNI

* must be no spaces in spatial ref
*   
  */

CREATE FUNCTION [dbo].[LCReturnHectad]
(@SpatialRef varchar(20), @SrType varchar(20) )
RETURNS  varchar(4)

	

--	PARAMETERS
--	NAME			DESCRIPTION
 
--
--	AUTHOR:	Mike Weidlei, Littlefield Consultancy
--	CREATED:  12 jan 2008
--

AS
BEGIN
--****************************************************************************************************
--constants

declare @Hectad varchar(4)

declare  @LenS int

if @SRType  = 'OSNI'
   set @SpatialRef = 'I' + @SpatialRef

set @Hectad=''

if (@SRType  = 'OSGB' or @SRType  = 'OSNI')  and len(Cast(@SpatialRef as varchar)) >  3
    

begin
      if len(@SpatialRef) = 4 
     begin
             set  @Hectad  = @SpatialRef
      end 
      else
        -- calculate hectad
       set @Lens = len(@SpatialRef)
       
     
       set @Hectad = left(@spatialref,3) + substring(@spatialref, (@lens-2)/2 +3,1)                 
                  
       if @SRType  = 'OSNI'
         set @Hectad = right(@Hectad,3)
                 
         
         
          
 
      


      

end


 

RETURN   @Hectad



END



GO
grant execute on [dbo].[LCReturnHectad] to public
GO


IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnEastingsV2]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCReturnEastingsV2]
GO



set ANSI_NULLS ON
set QUOTED_IDENTIFIER ON
GO
/*
  
 * 
 * 
 * *****************  Version 2  *****************
 * User: Mike Weideli
 * Returns a  Easting as metres based on origin of appropriate grid
 
* only works for  OSGB and OSNI
* returns  0 if not OSGB or OSNI
* must be no spaces in spatial ref
*   
* if c = true then centre is reruned otherwise SW corner 


  */

CREATE FUNCTION [dbo].[LCReturnEastingsV2]
(@SpatialRef varchar(20), @SrType varchar(20),@C bit )
RETURNS  int

	

--	PARAMETERS
--	NAME			DESCRIPTION
 
--
--	AUTHOR:	Mike Weidlei, Littlefield Consultancy
--	10/01//2008
--

AS
BEGIN
--****************************************************************************************************
--constants

declare @Eastings   int
declare @LetA  char(1)
declare @LetB char(1)
declare  @LenS int
declare @lenT  int
declare @ReturnEastings int
declare @NumericPart varchar(6)
Declare @Offset int
Declare @offsett int
declare @Offsetc int
Declare @Tetrad  int
Declare @TetradLet  int
set @ReturnEastings = 0
set @Tetrad = 0
set @offsetc = 0
set @Lent = 0
if @SRType  = 'OSNI'
begin
  set @spatialRef = 'I' + @SpatialREF  
end 


if @SRType  = 'OSGB' or @SRType  = 'OSNI'

  begin
  -- set @spatialRef = left(@SpatialRef,2) + '00'   
    set @LetA     = left(@SpatialRef,1)
    set @Eastings = 500000
    if  @LetA = 'S' or @LetA = 'N' or @LetA = 'H' or @LETA = 'I'
    begin
          set @Eastings  = 0
    end  
       
    set @LetB  = substring(@SpatialRef,2,1)
    if @Letb < 'I' 
    begin
       set  @offset = 0
    end 
    else  
    set @offset  = -1 
        
    set @Eastings = (((ascii(@LetB)+@offset)%5) * 100000) + @Eastings
     
 
     set @Lent = len(@SpatialRef)
     set @LenS = (len(@SpatialRef)-2)/2
     set @Numericpart =   substring(@SpatialRef,    3,@LenS)          
     if  isnumeric (@Numericpart) = 1
     begin   
           if @Lent = 5 
              begin              
              
                  set @TetradLet  = ascii(substring(@SpatialRef,@Lent,1)) -65 
                  set @offsett = floor((@TetradLet-1)/13) 
                  set @TetradLet = @TetradLet - @Offsett
                  set @Tetrad = (floor(@TetradLet/5)) * 2000  

     
              end                    
          if @C = 1
                   begin
                     set @offsetc = 	
     		     case @Lent
                       when 12 then 0
                       when 10 then 5 
                       when 8 then 50  	
                       when 6 then 500
                       when 5 then 1000
                       when 4 then 5000
                       when 2 then 50000
                       else 0
                     end 

                   end  

           set @ReturnEastings = @Eastings  +  (@NumericPart * (POWER(10,5-@lens)) ) + @Tetrad + @offsetc
      end 

      

            
    
      


      

end


 

RETURN   @ReturnEastings



END

GO
grant execute on dbo.LCReturnEastingsV2 to public
GO


IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnVagueDateShort]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCReturnVagueDateShort]
GO






/****** Object:  UserDefinedFunction [dbo].[LCReturnVagueDateShort]    Script Date: 04/06/2008 16:34:29 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


CREATE FUNCTION [dbo].[LCReturnVagueDateShort] 
(@VagueDateStart int, @VagueDateEnd int, @VagueDateType varchar(2))
RETURNS nvarchar(40)
/* A Shorter Version which returns the Recorder Vague Date in the the Recorder 6 format
    Mike Weideli 
    15 October 2005	

*/
AS
BEGIN
declare @MM1  nvarchar(9)
declare @MM2  nvarchar(9)
declare @RP1  nvarchar(40)
declare  @Z  int
declare @G  int
declare @H  int
declare @A  int
declare @B  int
declare @Y  int
declare @C  int
declare @M int
declare @D  int
declare @RD  int
declare @Y4  int
declare @RD1  int
declare @RM1  int
declare @RY1  int
declare @RDS1  nvarchar(2)
declare @RDS2  nvarchar(2)
declare @RD2  int
declare @RM2  int
declare @RY2  int
declare @RMS1  nvarchar(2)
declare @RMS2  nvarchar(2)
declare @X int
declare @CC1 int
declare @CC2 int
declare @VD nvarchar(2)
set @X = 0
while @X < 2 begin
    set @VD = @VagueDateType
    if ascii(right(@VD,1)) = 0
    set  @VD = left(@VagueDateType,1)

    set @RD = @VagueDateStart + 693594
	if  @X > 0 begin
              set @RD = @VagueDateEnd+693594
	
            end 

	set @Z =  @RD + 306
	set @H = 100*@Z-25
	set @A = FLOOR(@H/3652425)
	set @B = @A-FLOOR(@A/4)
	set @Y = FLOOR((100*@B+@H)/36525)
	set @C =@B+@Z-365*@Y-FLOOR(@Y/4)
	set @M = (5*@C+456)/153
	set @G = (153*@M-457)/5
	set @D = @C-@G
	if @M >12 
   		begin
                       set  @Y = @Y+1
  		set @M = @M-12
  	end 

  	if @X = 0
              begin 
              	set @RD1  =@D
		set @RM1 = @M
		set @RY1 = @Y
		set @CC1 = (@Y/100) +1

                if  @RD1 < 10 begin 
                           set @RDS1 = '0' + str(@RD1,1,0)
		end 
		else
		   begin 
		     set @RDS1 = str(@RD1,2,0)
	 	   end                     
		 if  @RM1 < 10 begin 
                           set @RMS1 = '0' + str(@RM1,1,0)
		end 
		else
		   begin 
		     set @RMS1 = str(@RM1,2,0)
	 	   end                     
		


              end 
              else
              begin              
 		set @RD2  = @D
		set @RM2 = @M
		set @RY2 = @Y
                set @CC2 = @Y/100
                if  @RD2 < 10 begin 
                           set @RDS2 = '0' + str(@RD2,1,0)
		end 
		else
		   begin 
		     set @RDS2 = str(@RD2,2,0)
	 	   end                     
		 if  @RM2 < 10 begin 
                           set @RMS2 = '0' + str(@RM2,1,0)
		end 
		else
		   begin 
		     set @RMS2 = str(@RM2,2,0)
	 	   end            
             end 
   	 set @X = @X + 1	         
END
   
        set @MM1  =  case @RM1
          when  1 then 'January'
 	      when   2 then 'February'
	      when 3 then 'March'
	      when 4 then 'April'
	      when 5 then 'May'
	      when 6 then 'June'
	      when 7 then 'July'
	      when 8 then 'August'
                  when 9 then 'September'
                  when 10 then 'October'
                  when 11 then 'November'
	      when 12 then 'December'
        end 
        set @MM2  =  case  @RM2
                  when  1 then 'January'
 	      when   2 then 'February'
	      when 3 then 'March'
	      when 4 then 'April'
	      when 5 then 'May'
	      when 6 then 'June'
	      when 7 then 'July'
	      when 8 then 'August'
                  when 9 then 'September'
                  when 10 then 'October'
                  when 11 then 'November'
	      when 12 then 'December'
         end 
         set  @RP1 = str(@RD1,2,0) + '/' + str(@RM1,2,0) + '/' + str(@RY1,4,0)

         

	set  @RP1 = case @VD
                     when 'D' then       @RDS1 + '/' + @RMS1 + '/' + str(@RY1,4,0)
                     when 'DD' then 	@RDS1 + '/' + @RMS1 + '/' + str(@RY1,4,0) +  '- ' + @RDS2 + '/' + @RMS2 + '/' + str(@RY2,4,0) 
                     when 'O' then       @MM1 + ' ' + str(@RY1,4,0)
	         when 'OO' then 	@MM1 + ' ' + str(@RY1,4,0) + ' - ' + @MM2 + ' ' + str(@RY2,4,0)
	         when 'Y' then        str(@RY1,4,0)
	         when 'YY' then       str(@RY1,4,0) +  ' - ' +  str(@RY2,4,0)
	         when '-Y' then         ' - '  +  	str(@RY2,4,0)
	         when 'Y-' then      	str(@RY1,4,0) + '  -  '
	         when 'C' then str(@CC1,2,0) + 'c'
                 when 'CC' then str(@CC1,2,0) + 'c - ' + str(@CC2,2,0) +'c'
                 when '-C' then ' - ' + str(@CC2,2,0) + 'c'
                 when 'C-' then str(@CC2,2,0) + 'c  -  '
                 when 'U' then 	'Unknown'	
                 
		  		  	

			

           end 
        	
           if @VD =  'P'  or @VD =  'S'  begin 
                         if @RM2= 2 begin  set @RP1 = 'Winter'
		 end  
		  if @RM2 = 8 begin  set @RP1 = 'Summer'
		 end
                          if @RM2 = 11  begin  set @RP1 = 'Autumn' 
		 end    
		 if @RM2 = 5  begin  set @RP1 =  'Spring'
		 end
                         if @VD = 'P' begin
 			set @RP1 = @RP1 + ' ' + str(@RY2,4,0)
		 end 
          end 
         




return @RP1

RETURN ''


END

GO
grant execute on [dbo].[LCReturnVagueDateShort] to public
GO



IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCReturnDate]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCReturnDate]
GO


/****** Object:  UserDefinedFunction [dbo].[LCReturnDate]    Script Date: 04/06/2008 16:33:04 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO




CREATE FUNCTION [dbo].[LCReturnDate]
(@VagueDate int, @VagueDateType varchar(2), @DatePart varchar(1))
RETURNS nvarchar(10)

/* Returns a date or date part for a given vague date field
    Author Michael Weideli - Littlefield Consultanct 14 October 2005
    F RETURNS FULL dd/mm/yyyy
    D RETURNS DAY
    Y RETURNS YEAR
    M RETURNS MONTH
    N RETURNS MONTH/YEAR
    B RETURNS DECADE

    vague date type of U does not return anything


*/
AS
BEGIN
declare @MM1  nvarchar(9)
declare @MM2  nvarchar(9)
declare @RP1  nvarchar(10)
declare  @Z  int
declare @G  int
declare @H  int
declare @A  int
declare @B  int
declare @Y  int
declare @C  int
declare @M int
declare @D  int
declare @RD  int
declare @Y4  int
declare @RD1  int
declare @RM1  int
declare @RY1  int
declare @RDS1  nvarchar(2)
declare @RDS2  nvarchar(2)
declare @RD2  int
declare @RM2  int
declare @RY2  int
declare @RMS1  nvarchar(2)
declare @RMS2  nvarchar(2)
if @vaguedatetype is not null
begin     
        set @RD = @VagueDate + 693594
	Set @Z =  @RD + 306
	set @H = 100*@Z-25
	set @A = FLOOR(@H/3652425)
	set @B = @A-FLOOR(@A/4)
	set @Y = FLOOR((100*@B+@H)/36525)
	set @C =@B+@Z-365*@Y-FLOOR(@Y/4)
	set @M = (5*@C+456)/153
	set @G = (153*@M-457)/5
	set @D = @C-@G
	if @M >12 
   		begin
                       set  @Y = @Y+1
  		set @M = @M-12
  	end 

           	set @RD1  =@D
            set @RM1 = @M
	set @RY1 = @Y
	if  @RD1 < 10 begin 
                           set @RDS1 = '0' + str(@RD1,1,0)
	end 
	else
	   begin 
	     set @RDS1 = str(@RD1,2,0)
	   end                     
	 if  @RM1 < 10 begin 
                 set @RMS1 = '0' + str(@RM1,1,0)
	    end 
  	 else
	   begin 
	     set @RMS1 = str(@RM1,2,0)
	  end                     
		
              if  @VagueDatetype <> 'U'
              begin
  	     set @RP1 = case @Datepart                    	         
		when 'D' then str(@RD1,2,0)
		when 'M' then str(@RM1,2,0)
		when 'Y' then str(@RY1,4,0)
	    when 'F' then @RDS1 + '/'  + @RMS1 + '/' + str(@RY1,4,0)
	 	when 'N' then @RMS1 + '/' + str(@RY1,4,0)
	    when 'B' then left(str(@RY1,4,0),3)
        when 'C' then left(str(@RY1,4,0),2)
           end 
              
	  end	 

return @RP1


end 
RETURN ''


END

GO
grant execute on [dbo].[LCReturnDate] to public
GO



IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCRectifyGR]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCRectifyGR]
GO


/****** Object:  UserDefinedFunction [dbo].[LCRectifyGR]    Script Date: 04/06/2008 16:32:02 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*
  
 * 
 * 
 * *****************  Version 2  *****************
 * User: Mike Weidlei
 * Returns a  grid ref of the required precision
* Needs additional UDF to do calculations
* 12 January 2008
* only works for  OSGB and OSNI

* must be no spaces in spatial ref
*   
  */

create FUNCTION [dbo].[LCRectifyGR]
(@SpatialRef varchar(20), @SrType varchar(20), @Precision int )
RETURNS  varchar(20)

	

--	PARAMETERS
--	NAME			DESCRIPTION
--	Takes a grid ref and changes its precision to that specified.  10(0), 5 , 2  and 1 km
--	Grid refs which are of less than specified precision return blank
--
--	AUTHOR:	Mike Weidlei, Littlefield Consultancy
--	CREATED:  12 jan 2008
--

AS
BEGIN
--****************************************************************************************************
--constants

declare @NuSpatialRef varchar(20)

set @NuSpatialRef = ''

if @SpatialRef is  null 
   set @srtype = 'INVL'

if @srtype = 'OSGB' or @srtype = 'OSNI'  
 begin    
 if @precision = 0 or @precision = 1
           set @NuSpatialRef = dbo.FormatGridRef(@SpatialRef,@SrType, @precision)
 

 if @precision = 5
           set @NuSpatialRef = dbo.LCReturn5km( @SpatialRef,@SrType)
 if @precision = 2
           set @NuSpatialRef = dbo.LCReturnTetrad( @SpatialRef,@SrType)
 end

RETURN   @NuSpatialRef


END

GO
grant execute on [dbo].[LCRectifyGR]  to public
GO


IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCFormatAbundanceData]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCFormatAbundanceData] 
GO



/****** Object:  UserDefinedFunction [dbo].[LCFormatAbundanceData]    Script Date: 04/06/2008 16:30:23 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

-- Tested 4/12/2005

CREATE FUNCTION [dbo].[LCFormatAbundanceData]
(@TOCCKey char(16))
RETURNS varchar(8000)
--
--	DESCRIPTION
--	Function to return a semi-colon sperated string of all abundance data
--	
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@TOCCKey			Taxon Occurrence.
--
--
--	AUTHOR:	Mike Weideli
--	CREATED: 08/12/2005
--

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @ItemString varchar(70)
DECLARE @Abundance  varchar(8000)
SET @Abundance  =''



SELECT @Abundance = @Abundance  + dbo.LCFormatMeasurement(MEASUREMENT_UNIT.SHORT_NAME,MEASUREMENT_QUALIFIER.SHORT_NAME,TAXON_OCCURRENCE_DATA.DATA) +  '; ' 

FROM 
 
(TAXON_OCCURRENCE_DATA INNER JOIN MEASUREMENT_UNIT ON TAXON_OCCURRENCE_DATA.MEASUREMENT_UNIT_KEY
 = MEASUREMENT_UNIT.MEASUREMENT_UNIT_KEY) INNER JOIN MEASUREMENT_QUALIFIER ON 
TAXON_OCCURRENCE_DATA.MEASUREMENT_QUALIFIER_KEY = MEASUREMENT_QUALIFIER.MEASUREMENT_QUALIFIER_KEY
 INNER JOIN MEASUREMENT_TYPE ON MEASUREMENT_UNIT.MEASUREMENT_TYPE_KEY = MEASUREMENT_TYPE.MEASUREMENT_TYPE_KEY










 WHERE TAXON_OCCURRENCE_KEY = @TOCCKey  and  MEASUREMENT_TYPE.SHORT_NAME = 'Abundance'
 
 ORDER BY TAXON_OCCURRENCE_DATA.DATA

if len(@Abundance) > 0 
BEGIN
  set  @RETURNSTRING  = left(@Abundance,len(@Abundance)-1)
END

--****************************************************************************************************
RETURN @ReturnString
END

GO
grant execute on [dbo].[LCFormatAbundanceData] to public
GO


IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCFormatTaxonDesKind6]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCFormatTaxonDesKind6]
GO





/****** Object:  UserDefinedFunction [dbo].[LCFormatTaxonDesKind6]    Script Date: 06/12/2008 10:02:03 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


    /*
 
    */

CREATE FUNCTION [dbo].[LCFormatTaxonDesKind6]
(@TLIKey char(16), @Level INT,@Kind varchar(40),@DictKey varchar(16),@IKind varchar(500))
RETURNS varchar(8000)

--
--	DESCRIPTION
--	Function to return a semi-colon saparated string of Designations.
--	
--	V6.5 
--  
--	PARAMETERS
--	NAME				DESCRIPTION
--	@TLIKey			Taxon List Item Key  to perform manipulation on.

--	@Level			The level to be returned. 0 = Y or N (is/is not designated)
--				1 = Kind  
--				2 = Type short name
--				3 = Type long name
--              
--	@Kind			Kind as name. Just returns the kind specified by name, Set to 0 if not applicable
--      @DictKey		Key of Dictionary to use
--      @IKinds                 Kinds to be included (comma separated of kinds by name ) Set to 0 for all kinds
--	AUTHOR:	Mike Weideli, Littlefield Consultancy 
--	CREATED: 10 June 2008
--

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
set @IKind = ',' + @IKind + ','



set @Returnstring =''

if @Level = 0

	begin 

        select @Returnstring =  td.taxon_designation_type_key 
	
	
	FROM index_taxon_name its 
	INNER JOIN index_taxon_name its2
    ON its.Recommended_taxon_List_item_key = its2.recommended_taxon_List_item_key
    INNER JOIN index_taxon_group itg
	ON its2.taxon_List_item_key = itg.contained_list_item_key
	INNER JOIN index_taxon_name as itn
	ON itn.taxon_list_item_key = itg.taxon_list_item_key
        INNER JOIN index_taxon_name itn2
        on itn2.recommended_taxon_list_item_key = itn.recommended_taxon_list_item_key
	INNER JOIN taxon_designation TD
        ON TD.taxon_list_item_key = itn2.taxon_list_item_key
        INNER JOIN taxon_designation_type tdt
	ON TD.taxon_designation_type_key = TDT.taxon_designation_type_key
	INNER JOIN Taxon_List_Item TLI
    ON TLI.Taxon_List_Item_key = TD.Taxon_List_item_Key
    INNER JOIN Taxon_list_Version TLV
    ON TLV.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key


    WHERE
	ITS.taxon_List_item_key  =  @TLIKEY
	AND
    (TDT.Kind = @Kind or @Kind = '0') 
    AND
    (TLV.Taxon_List_Key =  @DictKey or @DictKey = '0')   
    AND
    (@Ikind =',0,' or CHARINDEX ( (',' + TDT.kind + ',') ,@IKind ) > '0' )

    group by td.Taxon_designation_type_key

          
        if @ReturnString = '' 
               
           set @ReturnString ='N;'
                	 
         Else
            set @ReturnString ='Y;'
	
    end 
         
Else if @Level = 1
       	
      begin 
	SELECT  @Returnstring =  @ReturnString + CAST(Tdt.Kind as varchar(20)) +  '; ' 
	FROM index_taxon_synonym its 
	INNER JOIN index_taxon_group itg
	ON its.synonym_list_item_key = itg.contained_list_item_key
	INNER JOIN index_taxon_name as itn
	ON itn.taxon_list_item_key = itg.taxon_list_item_key
        INNER JOIN index_taxon_name itn2
        on itn2.recommended_taxon_list_item_key = itn.recommended_taxon_list_item_key
	INNER JOIN taxon_designation TD
        ON TD.taxon_list_item_key = itn2.taxon_list_item_key
        INNER JOIN taxon_designation_type tdt
	ON td.taxon_designation_type_key = tdt.taxon_designation_type_key
		INNER JOIN Taxon_List_Item TLI
    ON TLI.Taxon_List_Item_key = TD.Taxon_List_item_Key
    INNER JOIN Taxon_list_Version TLV
    ON TLV.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key
    WHERE
	ITS.taxon_List_item_key  =  @TLIKEY
	AND
    (TDT.Kind = @Kind or @Kind = '0') 
    AND
    (TLV.Taxon_List_Key =  @DictKey or @DictKey = '0')  
    AND
    (@Ikind =',0,' or CHARINDEX ( (',' + TDT.kind + ',') ,@IKind ) > '0' )

 
	group by TDT.kind
    end 
else if @Level = 2
       	
    begin 
	SELECT  @Returnstring =  @ReturnString + CAST(tdt.short_name as varchar(40)) +  '; ' 
	
	FROM index_taxon_synonym its 
	INNER JOIN index_taxon_group itg
	ON its.synonym_list_item_key = itg.contained_list_item_key
	INNER JOIN index_taxon_name as itn
	ON itn.taxon_list_item_key = itg.taxon_list_item_key
        INNER JOIN index_taxon_name itn2
        on itn2.recommended_taxon_list_item_key = itn.recommended_taxon_list_item_key
	INNER JOIN taxon_designation TD
        ON TD.taxon_list_item_key = itn2.taxon_list_item_key
        INNER JOIN taxon_designation_type tdt
	ON td.taxon_designation_type_key = tdt.taxon_designation_type_key
		INNER JOIN Taxon_List_Item TLI
    ON TLI.Taxon_List_Item_key = TD.Taxon_List_item_Key
    INNER JOIN Taxon_list_Version TLV
    ON TLV.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key


    WHERE
	ITS.taxon_List_item_key  =  @TLIKEY
	AND
    (TDT.Kind = @Kind or @Kind = '0') 
    AND
    (TLV.Taxon_List_Key =  @DictKey or @DictKey = '0')   
    AND
      (@Ikind =',0,' or CHARINDEX ( (',' + TDT.kind + ',') ,@IKind ) > '0' )
	group by tdt.short_name
   end 

else if @Level = 3	
        
	Begin
	SELECT  @Returnstring =  @ReturnString + CAST(tdt.long_name as varchar(100)) +  '; ' 
       	FROM index_taxon_synonym its 
	INNER JOIN index_taxon_group itg
	ON its.synonym_list_item_key = itg.contained_list_item_key
	INNER JOIN index_taxon_name as itn
	ON itn.taxon_list_item_key = itg.taxon_list_item_key
        INNER JOIN index_taxon_name itn2
        on itn2.recommended_taxon_list_item_key = itn.recommended_taxon_list_item_key
	INNER JOIN taxon_designation TD
        ON TD.taxon_list_item_key = itn2.taxon_list_item_key
        INNER JOIN taxon_designation_type tdt
	ON td.taxon_designation_type_key = tdt.taxon_designation_type_key
		INNER JOIN Taxon_List_Item TLI
    ON TLI.Taxon_List_Item_key = TD.Taxon_List_item_Key
    INNER JOIN Taxon_list_Version TLV
    ON TLV.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key


    WHERE
	ITS.taxon_List_item_key  =  @TLIKEY
	AND
    (TDT.Kind = @Kind or @Kind = '0') 
    AND
    (TLV.Taxon_List_Key =  @DictKey or @DictKey = '0')   
    AND
      (@Ikind =',0,' or CHARINDEX ( (',' + TDT.kind + ',') ,@IKind ) > '0' )


        group by tdt.long_name
	end
 


if len(@ReturnString) >0 
  set @returnstring = left(@Returnstring,len(@returnstring)-1)



RETURN @ReturnString

END


GO
grant execute on [dbo].[LCFormatTaxonDesKind6]  to public
GO

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCFormatMeasurement]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCFormatMeasurement]
GO



/****** Object:  UserDefinedFunction [dbo].[LCFormatMeasurement]    Script Date: 04/06/2008 16:30:57 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


/*
   * 
 * *****************  Version 1  *****************
 * User: Mike Weideli
 * Created by Littlefield Consultancy 


    */

CREATE FUNCTION [dbo].[LCFormatMeasurement]
(@MUnit varchar(40), @MQual varchar(40), @Mdata varchar(20))
RETURNS varchar(110)
--
--	DESCRIPTION
--	Function to return a the Measurement formatted as a string
--	
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@MUnit			Short name for Unit
--	@MQualifier		Short name for Qulaifier
--	@MData 		Actual Data
--	CREATED: 25/12/2005
--

AS
BEGIN


--****************************************************************************************************
DECLARE @NuQual varchar(40)
DECLARE @NuUnit varchar(40)
DECLARE @RETURNDATA varchar (110)
set @NUUnit = @MUnit
set @NuQual = @MQual
set @RETURNDATA = ''
if @NuUnit = 'None'
begin 
  set @NuUnit = ''
end
if @NuQual = 'None' or  CAST(@Mdata as varchar) = @NuQual 
begin 
  set  @NuQual = ''
end  
else
  set  @NuQual = ' of '  + @NuQual 
   

IF @MData is not NULL
begin

   set @RETURNDATA =   @MData + ' ' + @NuUnit + @NuQual
end 

RETURN @RETURNDATA

END
Go
grant execute on [dbo].[LCFormatMeasurement] to Public
Go

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCFormatLocDesignation]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCFormatLocDesignation] 
GO



/****** Object:  UserDefinedFunction [dbo].[LCFormatLocDesignation]    Script Date: 04/07/2008 20:05:58 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE FUNCTION [dbo].[LCFormatLocDesignation]
(@LOCCKey char(16))
RETURNS varchar(8000)
--
--	DESCRIPTION
--	Function to return a semi-colon sperated string of all Location Designation
--	
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@LOCCKey			Location Key.
--
--
--	AUTHOR:	Mike Weideli
--	CREATED: 04/04/2008
--

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @Designation  varchar(8000)
SET @Designation   =''
SELECT @Designation  = @Designation   + SS.Short_Name  +  '; ' 
FROM 
 
Site_Status SS
INNER JOIN
Location_Designation LD
ON LD.Site_Status_Key = SS.Site_Status_Key 
Where LD.Location_Key 
=  @LOCCKey
Order by SS.Short_Name


if len(@Designation) > 0 
BEGIN
  set  @RETURNSTRING  = left(@Designation,len(@Designation)-1)
END

--****************************************************************************************************
RETURN @ReturnString
END
GO
grant execute on [dbo].[LCFormatLocDesignation] to Public
GO


IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[LCFormatVC]')
	   and	  Type = 'FN')
    DROP FUNCTION [dbo].[LCFormatVC] 
GO


/****** Object:  UserDefinedFunction [dbo].[LCFormatVC]    Script Date: 04/07/2008 20:05:58 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE FUNCTION [dbo].[LCFormatVC]
(@LOCCKey char(16))
RETURNS varchar(1000)
--
--	DESCRIPTION
--	Function to return a semi-colon sperated string of all Vice Counties
--	
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@LOCCKey			Location Key.
--
--
--	AUTHOR:	Mike Weideli
--	CREATED: 04/04/2008
--

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(1000)
DECLARE @VC varchar(1000)
SET @VC   =''
SELECT @VC  = @VC   + AA.Item_Name  +  '; ' 
FROM 
Admin_Area AA
INNER JOIN
Location_Admin_Areas LAA
ON LAA.Admin_Area_key = AA.Admin_Area_key
Where LAA.Location_Key 
=  @LOCCKey AND AA.Admin_Type_Key IN ('NBNSYS0000000036','NBNSYS0000000032')
Order by AA.Item_name


if len(@VC) > 0 
BEGIN
  set  @RETURNSTRING  = left(@VC,len(@VC)-1)
END

--****************************************************************************************************
RETURN @ReturnString
END

GO
grant execute on [dbo].[LCFormatVC] to Public
GO 

GRANT DELETE ON [dbo].[SURVEY_EVENT_OWNER] TO [R2k_FullEdit]

GO