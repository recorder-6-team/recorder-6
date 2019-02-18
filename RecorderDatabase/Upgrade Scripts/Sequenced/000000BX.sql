/****** Object:  UserDefinedFunction [dbo].[ufn_Location_Expired]    Script Date: 02/18/2019 13:10:04 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns True if the location is not expired
                or False if it has. Expired will have 
                Location Designations where all entries have an expiry date.  
  Parameters:	@LocationKey

  Created:	December 2018
  Altered follwoing 2019 testing as too slow. This version 4X quicker.  
  Last revision information:
    $Author: Mike Weideli 
    
\*===========================================================================*/
ALTER FUNCTION [dbo].[ufn_Location_Expired]
	(@LocationKey char(16))
RETURNS bit
AS
BEGIN
  DECLARE @EXPIRED Bit
  SET @EXPIRED = 1
  SELECT @EXPIRED = 0 FROM Location L LEFT JOIN 
  LOCATION_DESIGNATION LD ON LD.LOCATION_KEY
  = L.LOCATION_KEY WHERE LD.DATE_TO IS NULL AND
  L.LOCATION_KEY = @LocationKey 
          
    
  RETURN @Expired    
END
