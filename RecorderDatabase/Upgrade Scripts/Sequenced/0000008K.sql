
IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCReturn10mSq]') AND type in (N'FN', N'TF'))
DROP FUNCTION  [dbo].[LCReturn10mSq]

GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Function to return a 10M square
         	Only works for OSGB and OSNI
  Parameters:
		@Spatial_ref - Spatial Reference 
                @SrType   -  Spatial Ref Type  
                @Lower -  if 1 and grid ref precision is less than 10m returns the actual grid ref. If 0 then returns null 

  Created:	July 2012
  Author: MikeWeideli 

\*=========================================================================== */

CREATE FUNCTION [dbo].[LCReturn10mSq]
(@SpatialRef varchar(20), @SrType varchar(20),@Lower bit = 1  )
RETURNS  varchar(10)


AS
BEGIN


declare @TenM varchar(14)
declare  @LenS int

set @TenM=''
If @Lower = 1 
   SET @TenM = @SpatialRef 
  
if @SRType  = 'OSNI'
   set @SpatialRef = 'I' + @SpatialRef

if (@SRType  = 'OSGB' or @SRType  = 'OSNI')  and len(Cast(@SpatialRef as varchar)) >  9
    

begin
      -- calculate hectare
       set @Lens = len(@SpatialRef)
            
       set @TenM = left(@spatialref,6) + substring(@spatialref, (@lens-2)/2 +3 ,4)                 
                  
       if @SRType  = 'OSNI'
         set @TenM = right(@TenM,9)
   

end
 

RETURN   @TenM

END

GO

GRANT EXECUTE ON [dbo].[LCReturn10mSq] TO PUBLIC 