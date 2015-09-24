/****** Object:  UserDefinedFunction [dbo].[LCReturn100mSq]    Script Date: 11/14/2011 18:14:42 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LCReturn100mSq]') AND type in (N'FN', N'TF'))
DROP FUNCTION [dbo].[LCReturn100mSq]

GO

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO
/*===========================================================================*\
  Description:	Function to return a 100M square
         	Only works for OSGB and OSNI
  Parameters:
		@Spatial_ref - Spatial Reference 
                @SrType   -  Spatial Ref Type  
                @Lower -  if 1 and grid ref precision is less than 100m returns the actual grid ref. If 0 then returns null 

  Created:	November 2011 
  Author: MikeWeideli 

\*=========================================================================== */


CREATE FUNCTION [dbo].[LCReturn100mSq]
(@SpatialRef varchar(20), @SrType varchar(20),@Lower bit = 1  )
RETURNS  varchar(8)

AS
BEGIN


declare @Hectare varchar(12)
declare  @LenS int

set @Hectare=''
If @Lower = 1 
   SET @Hectare = @SpatialRef 
  
if @SRType  = 'OSNI'
   set @SpatialRef = 'I' + @SpatialRef

if (@SRType  = 'OSGB' or @SRType  = 'OSNI')  and len(Cast(@SpatialRef as varchar)) >  7
    

begin
      -- calculate hectare
       set @Lens = len(@SpatialRef)
            
       set @Hectare = left(@spatialref,5) + substring(@spatialref, (@lens-2)/2 +3,3)                 
                  
       if @SRType  = 'OSNI'
         set @Hectare = right(@Hectare,7)
                 
  

end


 

RETURN   @Hectare



END

GO

GRANT EXECUTE ON [dbo].[LCReturn100mSq] TO PUBLIC 