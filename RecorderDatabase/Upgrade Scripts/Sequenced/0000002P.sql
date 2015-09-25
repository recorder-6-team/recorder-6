if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[VW_ConceptChildren]') and OBJECTPROPERTY(id, N'IsView') = 1)
drop view [dbo].[VW_ConceptChildren]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[VW_ConceptChildrenOnly]') and OBJECTPROPERTY(id, N'IsView') = 1)
drop view [dbo].[VW_ConceptChildrenOnly]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[VW_ConceptTerm]') and OBJECTPROPERTY(id, N'IsView') = 1)
drop view [dbo].[VW_ConceptTerm]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[VW_ConceptTermCommon]') and OBJECTPROPERTY(id, N'IsView') = 1)
drop view [dbo].[VW_ConceptTermCommon]
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[VW_ConceptTermPreferred]') and OBJECTPROPERTY(id, N'IsView') = 1)
drop view [dbo].[VW_ConceptTermPreferred]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO


/*===========================================================================*\
  Description:	View that lists concepts with the actual term details.

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 14/12/05 8:41 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptTerm]
WITH SCHEMABINDING
AS
	SELECT 	C.Concept_Key, 
		T.Item_Name, 
		T.Plaintext,
		C.Author_Copy,
		C.Concept_Group_Key, 
		C.Concept_Rank_Key,
		C.Sort_Code, 
		C.Meaning_Key,
		List_Preferred, 
		Is_Current
	FROM	dbo.Concept C
	INNER JOIN dbo.Term T on T.Term_Key=C.Term_Key

GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO


/*===========================================================================*\
  Description:	View that lists concepts with the common term details

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 14/12/05 8:41 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptTermCommon]
WITH SCHEMABINDING
AS
	SELECT 		C1.Concept_Key, T.Item_Name, T.Plaintext, C2.Author_Copy, C1.Concept_Group_Key
	FROM 		dbo.Concept C1
	INNER JOIN 	dbo.Meaning M ON M.Meaning_Key = C1.Concept_Key
	INNER JOIN 	dbo.Concept C2 ON C2.Meaning_Key = M.Meaning_Key
	INNER JOIN 	dbo.Term T ON T.Term_Key = C2.Term_Key
	INNER JOIN 	dbo.Language L ON L.Language_Key = T.Language_Key
	WHERE 		C2.Name_Type_Concept_Key = 'SYSTEM000000000L'
	AND 		L.Priority = 1

GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO


/*===========================================================================*\
  Description:	View that lists concepts with the preferred term details

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 14/12/05 8:41 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptTermPreferred]
AS
SELECT C.Concept_Key, T.Item_Name, T.Plaintext, C.Author_Copy, C.Concept_Group_Key, C.Concept_Rank_Key
FROM dbo.Concept C
INNER JOIN dbo.Concept CP ON CP.Meaning_Key=C.Meaning_Key
    AND CP.List_Preferred=1
		AND CP.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN dbo.Term T on T.Term_Key=C.Term_Key

GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO


/*===========================================================================*\
  Description:	View that lists concepts together with all child concepts.

  Created:	August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 14/12/05 8:41 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptChildren]
AS
    SELECT      c.Concept_Key           AS  Parent_Concept_Key,
                s.Concept_Key           AS  Child_Concept_Key
    FROM        Concept                 AS  c
    INNER JOIN  Concept                 AS  s
    ON          s.Concept_Group_Key     =   c.Concept_Group_Key
    AND         s.Meaning_Key           =   c.Meaning_Key
    UNION
    SELECT      p.Concept_Key           AS  Parent_Concept_Key,
                cs.Concept_Key          AS  Child_Concept_Key
    FROM        Concept                 AS  p
    INNER JOIN  Concept                 AS  ps
    ON          ps.Concept_Group_Key    =   p.Concept_Group_Key
    AND         ps.Meaning_Key          =   p.Meaning_Key
    INNER JOIN  Concept_Lineage         AS  pl
    ON          pl.Concept_Key          =   ps.Concept_Key
    INNER JOIN  Concept_Lineage         AS  cl
    ON          cl.Lineage              LIKE pl.Lineage + '\%'
    INNER JOIN  Concept                 AS  c
    ON          c.Concept_Key           =   cl.Concept_Key
    AND         c.Concept_Group_Key     =   p.Concept_Group_Key
    INNER JOIN  Concept                 AS  cs
    ON          cs.Concept_Group_Key    =   c.Concept_Group_Key
    AND         cs.Meaning_Key          =   c.Meaning_Key

GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO


/*===========================================================================*\
  Description:	View that lists concepts together with all child concepts.

  Created:	August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 14/12/05 8:41 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptChildrenOnly]
AS
	SELECT	DISTINCT
		P.Concept_Key AS Parent_Concept_Key, CSyn.Concept_Key AS Child_Concept_Key

	FROM	Concept P
	JOIN	Concept PSyn		ON PSyn.Concept_Group_Key = P.Concept_Group_Key
					AND PSyn.Meaning_Key = P.Meaning_Key
	JOIN  	Concept_Lineage PL	ON PL.Concept_Key = PSyn.Concept_Key
	JOIN  	Concept_Lineage CL 	ON CL.Lineage LIKE PL.Lineage + '\%'
	JOIN  	Concept C		ON C.Concept_Key = CL.Concept_Key
					AND C.Concept_Group_Key = P.Concept_Group_Key
	JOIN  	Concept CSyn		ON CSyn.Concept_Group_Key = C.Concept_Group_Key
					AND CSyn.Meaning_Key = C.Meaning_Key

GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

