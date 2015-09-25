/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_ConceptTermPreferred]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_ConceptTermPreferred]
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
    $Date: 21/03/08 9:52 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptTermPreferred]
AS
	SELECT 	C.Concept_Key, 
			T.Item_Name, 
			T.Plaintext, 
			C.Author_Copy, 
			C.Concept_Group_Key, 
			C.Concept_Rank_Key, 
			C.Sort_Code,
			CP.Concept_Key AS Preferred_Concept_Key
	FROM 	Concept C
	JOIN 	Concept CP 	ON 	CP.Meaning_Key			=	C.Meaning_Key
						AND CP.List_Preferred		=	1
						AND CP.Concept_Group_Key	=	C.Concept_Group_Key
	JOIN 	Term 	T 	ON	T.Term_Key				=	CP.Term_Key
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT SELECT ON [dbo].[VW_ConceptTermPreferred] TO [Public]
GO