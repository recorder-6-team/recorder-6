/****** Fix issues with UDF [dbo].[LCReturnHectad]  ******/

/****** Object:  UserDefinedFunction [dbo].[LCReturnHectad]    Script Date: 01/26/2018 19:33:25 ******/
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
 * 12 January 2008 modified 26 Jan 2018 to fix bug 
 * only works for  OSGB and OSNI

 * must be no spaces in spatial ref
 *   
*/

ALTER FUNCTION [dbo].[LCReturnHectad]
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
    begin 
      -- calculate hectad
      set @Lens = len(@SpatialRef)
      set @Hectad = left(@spatialref,3) + substring(@spatialref, (@lens-2)/2 +3,1)                 
    end
      
  if @SRType  = 'OSNI' set @Hectad = right(@Hectad,3)
     

end


 

RETURN   @Hectad



END


