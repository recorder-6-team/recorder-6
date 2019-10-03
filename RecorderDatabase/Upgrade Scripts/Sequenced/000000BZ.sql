/****** SQL FOR CHANGES IN WORKING OF Inactive Locations ******/


ALTER TABLE Location
ADD  Active bit

GO

ALTER TABLE [dbo].[LOCATION] ADD  CONSTRAINT [DF_LOCATION_Active]  DEFAULT (1) FOR [Active]


GO

/****** Object:  StoredProcedure [dbo].[usp_LocationInactiveIndicator_Update]    Script Date: 08/23/2019 07:03:30 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Brings Location Active Indicator in line with the designations.
  Parameters:	@LocationKey

  Created:	August 2019
  
\*===========================================================================*/
Create PROCEDURE [dbo].[usp_LocationInactiveIndicator_Update]
	@LocationKey char(16)
AS
    Update LOCATION SET Active = dbo.ufn_Location_Expired(@LocationKey) 
    WHERE Location_Key = @LocationKey 

GO


GRANT EXECUTE ON [dbo].[usp_LocationInactiveIndicator_Update] TO PUBLIC

GO

Update LOCATION SET Active = 1 WHERE 
  dbo.ufn_Location_Expired(Location_Key) = 0


GO

/****** Object:  UserDefinedFunction [dbo].[ufn_Location_Active]    Script Date: 08/23/2019 07:33:25 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns True if the location is active
                or False if it has. Based on the
                Location Active indicator.  
  Parameters:	@LocationKey

  Created:	August 2019
      $Author: Mike Weideli 
    
\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_Location_Active]
	(@LocationKey char(16))
RETURNS bit
AS
BEGIN
  DECLARE @ISACTIVE Bit
  SET @ISACTIVE = (SELECT ACTIVE FROM LOCATION WHERE 
  Location_Key =  @LocationKey)   
          
    
  RETURN @ISACTIVE   
END

GO

GRANT EXECUTE ON [dbo].[ufn_Location_Active] TO PUBLIC  


GO
/****** Object:  StoredProcedure [dbo].[usp_LocationInactiveIndicator_Update]    Script Date: 08/30/2019 20:48:48 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Brings Location Active Indicator in line with the designations.
  Parameters:	@LocationKey

  Created:	August 2019
  
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_LocationInactiveIndicator_Update]
	@LocationKey char(16)
AS
    
    UPDATE LOCATION SET ACTIVE = 1 
    WHERE Location_Key = @LocationKey 
  
    Update LOCATION SET Active = 0 
    WHERE Location_Key = @LocationKey AND
    dbo.ufn_Location_Expired(@LocationKey) = 1 