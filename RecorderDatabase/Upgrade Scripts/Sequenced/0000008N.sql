/****** Object:  View [dbo].[LC_DATE_FILTER]    Script Date: 08/27/2012 21:30:24 ******/



IF  EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[LC_DATE_FILTER]') AND type in (N'V'))
DROP VIEW [dbo].[LC_DATE_FILTER]


GO

SET ANSI_NULLS ON

GO

SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	VIEW which changes the entry date into yyymmdd format 
              
  Created:	October  2012
  Author: MikeWeideli

\*=========================================================================== */
CREATE VIEW [dbo].[LC_DATE_FILTER]
AS
SELECT     TAXON_OCCURRENCE_KEY, CAST(CONVERT(char(10), ENTRY_DATE, 112) AS INT) AS DATESTRING
FROM         dbo.TAXON_OCCURRENCE

GO

GRANT SELECT ON [dbo].[LC_DATE_FILTER] TO PUBLIC

