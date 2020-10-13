/****** Object:  StoredProcedure [dbo].[usp_IWMatch_Species_Notes]    Script Date: 10/13/2020 17:34:37 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Adds and updates notes for Species matches 

  Created:	Nov 2018 

  Updated October 2020 to make the notes more meaningful 

    
  Populates the notes column 
\*===========================================================================*/


ALTER PROCEDURE [dbo].[usp_IWMatch_Species_Notes]

AS
    
 Update #Species set Notes = 'Taxa not suitable for input' 
 WHERE Status = 3 
	
 Update #Species set Notes = 'Taxa not on any current list' 
 WHERE Status = 2 
	
 Update #Species set Notes = ltrim(str(Match_Count)) + ' Possible matches ' 
 WHERE Status = 1  AND Match_Key is null
	
 Update #Species set Notes =  'Remembered Match' 
 WHERE Match_Key is not null and Remembered = 1
 
 Update #Species set Notes =  'Matched' 
 WHERE Match_Key is not null and Remembered = 0
	
 Update #Species set Notes =  '' 
 WHERE Status = 0  AND Match_Key is null	







