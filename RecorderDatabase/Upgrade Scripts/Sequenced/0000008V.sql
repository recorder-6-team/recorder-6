/****** Object:  View [dbo].[LC_DATE_TOCC_CHANGED]    Script Date: 11/14/2012 21:30:24 ******/

IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LC_DATE_TOCC_CHANGED]') AND type in (N'V'))
DROP VIEW [dbo].[LC_DATE_TOCC_CHANGED]

GO

SET ANSI_NULLS ON

GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	VIEW which returns the maximum data changed for a Taxon_Occurrence key taking into 
   account Survey Event and Sample Changes. Format retuned is yyymmdd
              
  Created:	November 2012
  Author: MikeWeideli 

\*=========================================================================== */
CREATE VIEW [dbo].[LC_DATE_TOCC_CHANGED]
AS
SELECT ToccKEY AS Taxon_Occurrence_key, MAX(DATESTRING) as DateString 
FROM
(Select TOCC.taxon_occurrence_key as TOccKey,
Case S.changed_Date 
WHEN  NULL THEN  CAST(CONVERT(char(10), S.ENTRY_DATE, 112) AS INT) 
ELSE
 CAST(CONVERT(char(10), S.CHANGED_DATE, 112) AS INT)     
END 
AS DATESTRING
from sample S
inner join taxon_occurrence Tocc On Tocc.sample_key = S.sample_key

Union Select
TOCC.taxon_occurrence_key as TOccKey,
Case SE.changed_Date 
WHEN  NULL THEN  CAST(CONVERT(char(10), SE.ENTRY_DATE, 112) AS INT) 
ELSE
 CAST(CONVERT(char(10), SE.CHANGED_DATE, 112) AS INT)     
END 
AS DATESTRING
from sample S
inner join taxon_occurrence Tocc On Tocc.sample_key = S.sample_key
Inner Join Survey_event SE ON SE.Survey_Event_key = S.Survey_Event_key

Union Select
TOCC.taxon_occurrence_key as TOccKey,
Case TOCC.changed_Date 
WHEN  NULL THEN  CAST(CONVERT(char(10), TOCC.ENTRY_DATE, 112) AS INT) 
ELSE
 CAST(CONVERT(char(10), TOCC.CHANGED_DATE, 112) AS INT)     
END 
AS DATESTRING
From taxon_occurrence Tocc) AS D

GROUP BY D.TOCCKey


GO

GRANT SELECT ON [dbo].[LC_DATE_TOCC_CHANGED] TO PUBLIC
