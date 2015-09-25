If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_RecursionCheck_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get]
GO

/*===========================================================================*\
  Description:  Checks that the user isn't trying to create a circular
        Concept_Relation.

  Parameters:   @PotentialChildKey - key of dragged node.
                @PotentialParentKey - key of target node.
                @RecursionExists - if cycle exists (i.e. a problem) return 1
                        else return 0 (i.e. OK)

  Created:  March 2004

  Last revision information:
    $Revision: 6 $
    $Date: 26/11/07 12:23 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get] 
	@PotentialChildKey 	CHAR(16),
	@PotentialParentKey CHAR(16),
	@RecursionExists 	BIT OUTPUT
AS
    SET NOCOUNT ON

    IF @PotentialChildKey = @PotentialParentKey
        SET	@RecursionExists    =   1
    ELSE
    BEGIN
        SELECT 		@RecursionExists = MAX(CASE WHEN LP.Lineage LIKE LC.Lineage + '\%' THEN 1 ELSE 0 END)
        FROM 		Concept_Lineage LC
		JOIN		Concept			CC 	ON	CC.Concept_Key			=	LC.Concept_Key
        CROSS JOIN 	Concept_Lineage LP
		JOIN		Concept			CP	ON	CP.Concept_Key			= 	LP.Concept_Key
										AND	CP.Concept_Group_Key	= 	CC.Concept_Group_Key
        WHERE 		LC.Concept_Key	=	@PotentialChildKey
        AND 		LP.Concept_Key 	= 	@PotentialParentKey

        IF @RecursionExists IS NULL
            SET	@RecursionExists    =   0
    END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_RecursionCheck_Get') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_RecursionCheck_Get'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [Dev - JNCC SQL]
END

GO