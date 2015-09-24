SET QUOTED_IDENTIFIER ON
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_RecursionCheck_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get]
GO

CREATE PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get] 
@PotentialChildKey CHAR(16),
@PotentialParentKey CHAR(16),
@RecursionExists BIT OUTPUT
AS

/*===========================================================================*\
  Description:  Checks that the user isn't trying to create a circular
        Concept_Relation.

  Parameters:   @PotentialChildKey - key of dragged node.
                @PotentialParentKey - key of target node.
                @RecursionExists - if cycle exists (i.e. a problem) return 1
                        else return 0 (i.e. OK)

  Created:  March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 25/01/06 9:25 $
    $Author: Johnvanbreda $

\*===========================================================================*/

    SET NOCOUNT ON

    IF @PotentialChildKey = @PotentialParentKey
        SET         @RecursionExists    =   1
    ELSE
    BEGIN
        SELECT @RecursionExists = MAX(CASE WHEN LP.Lineage LIKE LC.Lineage + '\%' THEN 1 ELSE 0 END)
        FROM Concept_Lineage LC
        CROSS JOIN Concept_Lineage LP
        WHERE LC.Concept_Key=@PotentialChildKey
        AND LP.Concept_Key = @PotentialParentKey

        IF @RecursionExists IS NULL
            SET         @RecursionExists    =   0
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReferenceKeywords_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReferenceKeywords_Get]
GO

/*===========================================================================*\
  Description:	Retrieve the keywords for a reference

  Parameters:	@Key		Reference Source_Key

  Created:	December 2005

  Last revision information:
    $Revision: 1 $
    $Date: 25/01/06 9:25 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReferenceKeywords_Get]
	@Key CHAR(16)
AS

	SELECT RK.Reference_Keyword_Key, CT.Concept_Key, CT.Plaintext,
		S.User_Name_Key AS Entered_By, RK.Custodian
	FROM Reference_Keyword RK
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=RK.Concept_Key
	INNER JOIN Session S ON S.Session_ID=RK.Entered_Session_ID
	WHERE RK.Source_Key = @Key
	ORDER BY CT.Sort_Code, CT.Plaintext


GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReferenceKeywords_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReferenceKeywords_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeywords_Get TO [Dev - JNCC SQL]
END
GO

