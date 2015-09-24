/*===========================================================================*\
  Description:
	New trigger for setting the custodian of the Survey_Event_Owner_Type when it
	is created.

  Created:
	March 2009

  Last revision information:
    $Revision: 1 $
    $Date: 12/03/09 13:40 $
    $Author: Pauldavies $

\*===========================================================================*/

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TRIGGER	[dbo].[Survey_Event_Owner_TypeCustodianInsert] 
			ON	[dbo].[Survey_Event_Owner_Type] 
AFTER INSERT AS 
	UPDATE		SURVEY_EVENT_OWNER_TYPE 
	SET			SURVEY_EVENT_OWNER_TYPE.CUSTODIAN =
					SUBSTRING(SURVEY_EVENT_OWNER_TYPE.SURVEY_EVENT_OWNER_TYPE_KEY, 1, 8) 
	FROM		SURVEY_EVENT_OWNER_TYPE 
	INNER JOIN	INSERTED 
			ON	SURVEY_EVENT_OWNER_TYPE.SURVEY_EVENT_OWNER_TYPE_KEY = INSERTED.SURVEY_EVENT_OWNER_TYPE_KEY 
	WHERE		SURVEY_EVENT_OWNER_TYPE.CUSTODIAN IS NULL
