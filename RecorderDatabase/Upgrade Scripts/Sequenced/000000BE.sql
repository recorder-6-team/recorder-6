/****** Adds/Updates UDFs  for external source reporting ******/

/****** Object:  UserDefinedFunction [dbo].[GetExternalRef]    Script Date: 10/03/2018 11:58:14 ******/

SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================
  Description:	Returns the External Key for a given Taxon Occurrence Key

  Parameters:
  @ToccKey - Taxon occurrence key 
 		  
  Created:	September 2013
  Updated       March 2018
  Author: MikeWeideli 

\*=========================================================================== */

ALTER FUNCTION [dbo].[GetExternalRef]
(@ToccKey char(16) )

RETURNS varchar(505)
AS
BEGIN

--****************************************************************************************************
DECLARE @ExtString varchar(505)

set @ExtString = ''

SELECT @ExtString =  @ExtString  +  Taxon_Private_Data.Detail + ';'
FROM Taxon_Private_Data where Taxon_Occurrence_key = @TOCCKey
AND Taxon_Private_Data.Taxon_Private_Type_Key = 'R6TEAM1800000001'



if len(@ExtString ) > 0 
  set  @ExtString   = left(@ExtString ,len(@ExtString )-1)
ELSE
  set @ExtString  = 'Not Recorded'
  

RETURN  @ExtString

END

GO

GRANT EXECUTE ON [dbo].[GetExternalRef] TO PUBLIC

GO


/****** Object:  UserDefinedFunction [dbo].[GetExternalSourceRef]    Script Date: 10/03/2018 11:53:51 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON

GO

/*===========================================================================
  Description:	Returns the Source + External Key for a given Taxon Occurrence Key

  Parameters:
  @ToccKey - Taxon occurrence key 
 		  
  Created:	September 2013
  Author: MikeWeideli 

=========================================================================== */

CREATE FUNCTION [dbo].[GetExternalSourceRef] 
(@ToccKey char(16) )

RETURNS varchar(700)
AS
BEGIN

--****************************************************************************************************
DECLARE @ExtString varchar(655)

set @ExtString = ''

SELECT @ExtString =  @ExtString  +  Taxon_Private_Data.Item_Name + '/' + Taxon_Private_Data.Detail + ';'
FROM Taxon_Private_Data where Taxon_Occurrence_key = @TOCCKey
AND Taxon_Private_Data.Taxon_Private_Type_Key = 'R6TEAM1800000001'



if len(@ExtString ) > 0 
  set  @ExtString   = left(@ExtString ,len(@ExtString )-1)
ELSE
  set @ExtString  = 'Not Recorded'
  

RETURN  @ExtString

END


GO
GRANT EXECUTE ON [dbo].[GetExternalSourceRef] TO PUBLIC 

GO


/****** Object:  UserDefinedFunction [dbo].[GetExternalSource]    Script Date: 10/03/2018 12:04:26 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON

GO

/*===========================================================================
  Description:	Returns the External Sources for a given Taxon Occurrence Key

  Parameters:
  @ToccKey - Taxon occurrence key 
 		  
  Created:	March 2018
  Author: MikeWeideli 

=========================================================================== */

CREATE FUNCTION [dbo].[GetExternalSource]
(@ToccKey char(16) )

RETURNS varchar(40)
AS
BEGIN
DECLARE @ExtString varchar(655)
set @ExtString = ''

SELECT @ExtString =  @ExtString  +  Taxon_Private_Data.Item_Name + ';'
FROM Taxon_Private_Data where Taxon_Occurrence_key = @TOCCKey
AND Taxon_Private_Data.Taxon_Private_Type_Key = 'R6TEAM1800000001'



if len(@ExtString ) > 0 
  set  @ExtString   = left(@ExtString ,len(@ExtString )-1)
ELSE
  set @ExtString  = 'Not Recorded'
  

RETURN  @ExtString

END

GO

GRANT EXECUTE ON [dbo].[GetExternalSource] TO PUBLIC 

