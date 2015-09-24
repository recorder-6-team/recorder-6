
/****** Object:  View [dbo].[LC_Date_Filter_Changed]    Script Date: 08/27/2012 21:30:24 ******/


IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LC_DATE_FILTER_CHANGED]') AND type in (N'V'))
DROP VIEW [dbo].[LC_DATE_FILTER_CHANGED]

GO

SET ANSI_NULLS ON

GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	VIEW which changes the changed date into yyymmdd format 
              
  Created:	August  2012
  Author: MikeWeideli 

\*=========================================================================== */
CREATE VIEW [dbo].[LC_DATE_FILTER_CHANGED]
AS
SELECT     TAXON_OCCURRENCE_KEY, 
CASE CHANGED_DATE
  WHEN  NULL THEN  CAST(CONVERT(char(10), ENTRY_DATE, 112) AS INT) 
  ELSE
  CAST(CONVERT(char(10), CHANGED_DATE, 112) AS INT)     
END 
AS DATESTRING

FROM dbo.TAXON_OCCURRENCE


GO

GRANT SELECT ON [dbo].[LC_DATE_FILTER_CHANGED] TO PUBLIC

