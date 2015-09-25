If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[spRptMeasurementByContextName]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure spRptMeasurementByContextName'
        DROP PROCEDURE [dbo].[spRptMeasurementByContextName]
    END
GO

    PRINT 'Creating procedure spRptMeasurementByContextName'
GO

    /*
    $History: 0000002T.sql $
 * 
 * *****************  Version 4  *****************
 * User: Johnvanbreda Date: 23/12/05   Time: 11:35
 * Updated in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Update languages stored proc
 * 
 * *****************  Version 3  *****************
 * User: Johnvanbreda Date: 23/12/05   Time: 10:48
 * Updated in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * Fix to concept delete proc
 * 
 * *****************  Version 2  *****************
 * User: Johnvanbreda Date: 16/12/05   Time: 9:23
 * Updated in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * CCN133
 * Amended usp_conceptdelete
 * 
 * *****************  Version 1  *****************
 * User: Johnvanbreda Date: 15/12/05   Time: 16:20
 * Created in $/JNCC/Development/Build/SQL Scripts/Upgrade Scripts/Sequenced
 * 
 * *****************  Version 3  *****************
 * User: Johnvanbreda Date: 3/02/05    Time: 10:33
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * IR8445
 * Wizard measurements sort order
 * 
 * *****************  Version 2  *****************
 * User: Bencollier   Date: 7/02/03    Time: 10:14
 * Updated in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Updated permissions at end of script
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 2/12/02    Time: 12:16
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * Initial Build
 */

CREATE PROCEDURE [dbo].[spRptMeasurementByContextName]

--
--	DESCRIPTION
--	This procedure populates the SURVEY_EVENT_KEY based upon SAMPLE_KEY values
--
--	PARAMETERS
--	NAME			DESCRIPTION
--
--
--	AUTHOR:	Ben Collier, Dorset Software.
--	CREATED: 28/11/2002  
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
@ContextName varchar(20)
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

AS
SET NOCOUNT ON

    DECLARE	@Procedure_Name    SYSNAME	--    Holds the name of the currently executing procedure
    DECLARE	@ErrorFlag    INT         		--    Flags if there has been an error during the procedure
    DECLARE	@ErrorNo    INT         		--    Holds the @@ERROR value
    DECLARE	@ErrorMsg    VARCHAR(255)     	--    User defined error message

    --    Set the procedure name in the variable
    SELECT @Procedure_Name = OBJECT_NAME (@@PROCID)

    --    Set the @ErrorFlag variable to -1 as the default return value. 
    SELECT @ErrorFlag = -1

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
DECLARE @DisplayContextName varchar(20)

SET @DisplayContextName =
	CASE UPPER(@ContextName)
		WHEN 'TAXON OCCURRENCE' THEN 'Taxon'
		WHEN 'BIOTOPE OCCURRENCE' THEN 'Biotope'
		ELSE UPPER(SUBSTRING(@ContextName, 1, 1)) + SUBSTRING(@ContextName, 2, LEN(@ContextName))
	END

SELECT MEASUREMENT_QUALIFIER.MEASUREMENT_QUALIFIER_KEY, 
	@DisplayContextName + ' ' + MEASUREMENT_TYPE.SHORT_NAME + ' (' + MEASUREMENT_QUALIFIER.SHORT_NAME+')' AS Description
FROM 
(MEASUREMENT_TYPE 
INNER JOIN 
	MEASUREMENT_QUALIFIER 
ON MEASUREMENT_TYPE.MEASUREMENT_TYPE_KEY = MEASUREMENT_QUALIFIER.MEASUREMENT_TYPE_KEY) 
INNER JOIN 
	(MEASUREMENT_CONTEXT 
	INNER JOIN 
		MEASUREMENT_TYPE_CONTEXT 
	ON MEASUREMENT_CONTEXT.MEASUREMENT_CONTEXT_KEY = MEASUREMENT_TYPE_CONTEXT.MEASUREMENT_CONTEXT_KEY) 
ON MEASUREMENT_TYPE.MEASUREMENT_TYPE_KEY = MEASUREMENT_TYPE_CONTEXT.MEASUREMENT_TYPE_KEY
WHERE (((MEASUREMENT_CONTEXT.CONTEXT_NAME) = @ContextName))
ORDER BY [Description] DESC;
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

    SELECT @ErrorNo = @@ERROR

    IF @ErrorNo <> 0 
    BEGIN
        SELECT @ErrorMsg = 'Error number ' + CONVERT ( VARCHAR(6), @ErrorNo ) + ' in procedure ' + @Procedure_Name
        RAISERROR (@ErrorMsg, 16, -1) --WITH LOG
        GOTO PROC_RETURN
    END

    SELECT @ErrorFlag = 0

PROC_RETURN:

    RETURN @ErrorFlag
GO

    IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.spRptMeasurementByContextName') AND SysStat & 0xf = 4)
    BEGIN
        PRINT 'Setting up security on procedure spRptMeasurementByContextName'
        	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev- JNCC SQL')
        		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [Dev- JNCC SQL]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_AddOnly]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_Administrator]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_FullEdit]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_ReadOnly]

			IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        		GRANT EXECUTE ON dbo.spRptMeasurementByContextName TO [R2k_RecordCardsOnly]
    END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupKey_Get_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupKey_Get_ForConcept]
GO

/*===========================================================================*\
  Description:	Outputs the concept group key for a concept.

  Parameters:	@Key	Concept key

  Created:	January 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupKey_Get_ForConcept]
	@Key char(16),
	@ConceptGroupKey char(16) OUTPUT
AS
	SELECT 		@ConceptGroupKey = Concept_Group_Key
	FROM		Concept
	WHERE		Concept_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupKey_Get_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupKey_Get_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConcept TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupLanguage_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupLanguage_Get]
GO

/*===========================================================================*\
  Description:	Update a simple non preferred synonym

  Parameters:	@Key		Concept Group key.
		@LanguageKey	OUTPUT

  Created:	December 2005

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupLanguage_Get]
	@Key CHAR(16),
	@LanguageKey VARCHAR(4) OUTPUT
AS
 	SELECT @LanguageKey = LD.Language_Key
	FROM Local_Domain LD 
	INNER JOIN Concept_Group CG ON CG.Local_Domain_Key=LD.Local_Domain_Key
		AND CG.Concept_Group_Key=@Key

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupLanguage_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupLanguage_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupLanguage_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupLanguage_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupLanguage_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupLanguage_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Select_ForApplication]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Select_ForApplication]
GO

/*===========================================================================*\
  Description:	Retrieve the concept group responsible for a particular
		task (e.g. the one containing keywords)

  Parameters:	@Key		Application_Key

  Created:	December 2005

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Select_ForApplication]
	@Key CHAR(16)
AS

	SELECT CG.Concept_Group_Key, CG.Hierarchy_Relation_Type_Key
	FROM Concept_Group CG
	INNER JOIN Application_Concept_Group ACG ON ACG.Concept_Group_Key=CG.Concept_Group_Key
		AND ACG.Application_Key=@Key

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Select_ForApplication') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Select_ForApplication'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForApplication TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Delete_ForPaste]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Delete_ForPaste]
GO

/*===========================================================================*\
  Description:	A From key and a To key are passed in, and the corresponding
		Concept_Relation record is deleted.

  Parameters:	@FromKey
				@ToKey
				@RecordsAffected

  Created:	January 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Delete_ForPaste]
	@FromKey char(16),
	@ToKey char(16),
	@RecordsAffected INT OUTPUT
AS
	SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Make required changes to Concept_Lineage
		\*-------------------------------------------------------------*/
		DECLARE		@relation_type_key		CHAR(16)
		
		DECLARE		types						CURSOR LOCAL FAST_FORWARD FOR
		SELECT		Thesaurus_Relation_Type_Key
		FROM		Concept_Relation
		WHERE		From_Concept_Key			=	@FromKey
		AND			To_Concept_Key				=	@ToKey

		OPEN		types

		WHILE 1 = 1
		BEGIN
			FETCH		types
			INTO		@relation_type_key

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_RelationDeleted	@FromKey,
															@ToKey,
															@relation_type_key
			IF @@ERROR <> 0 GOTO CloseRollbackAndExit
		END

		CLOSE		types

		/*-------------------------------------------------------------*\
		  Delete record in Concept_Relation.
		\*-------------------------------------------------------------*/
		DECLARE		@error		INT

		DELETE		Concept_Relation
		WHERE		From_Concept_Key	=	@FromKey
		AND			To_Concept_Key		=	@ToKey

		SELECT		@error				=	@@ERROR,
					@RecordsAffected	=	@@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION
	RETURN 0

CloseRollBackAndExit:
	CLOSE		types

RollBackAndExit:
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Delete_ForPaste failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Delete_ForPaste') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Delete_ForPaste'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete_ForPaste TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete_ForPaste TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete_ForPaste TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete_ForPaste TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Concept_Relation table

  Parameters:	@FromConceptKey
		@ToConceptKey
		@ThesaurusRelationTypeKey
		@Multiplicity
		@Inherited
		@Comment
		@SessionID
		@SystemSuppliedData

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Insert]
	@Key char(16) OUTPUT,
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16) = NULL,
	@Multiplicity float = NULL,
	@Inherited bit = NULL,
	@Comment text = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @ParentRelationTypeKey CHAR(16)

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT
	IF @@ERROR <> 0 GOTO RollBackAndExit

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  If the app. doesn't pass in a @ThesaurusRelationTypeKey then
		  the Hierarchy_Relation_Type_Key for the Concept Group of the
		  parent is used.
		\*-------------------------------------------------------------*/		
		SELECT 		@ParentRelationTypeKey = CG.Hierarchy_Relation_Type_Key
		FROM		Concept_Group AS CG
		INNER JOIN	Concept AS C ON C.Concept_Group_Key = CG.Concept_Group_Key
		WHERE		C.Concept_Key = @FromConceptKey

		IF @ThesaurusRelationTypeKey IS NULL 
			SET @ThesaurusRelationTypeKey = @ParentRelationTypeKey

		/*-------------------------------------------------------------*\
			Validate to ensure we are not trying to create a cycle in the 
			concept group hierarchy
		\*-------------------------------------------------------------*/				
		IF @ThesaurusRelationTypeKey = @ParentRelationTypeKey
		BEGIN
			IF EXISTS(
					SELECT 1
					FROM Concept_Lineage CLParent
					INNER JOIN Concept_Lineage CLChild ON CLParent.Lineage LIKE CLChild.Lineage + '\%'
							OR CLParent.Lineage = CLChild.Lineage		
					WHERE CLParent.Concept_Key=@FromConceptKey
					AND CLChild.Concept_Key=@ToConceptKey
					)
				RAISERROR ('Cyclical relationship', 16, 1)
			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Insert in Concept_Relation.
		\*-------------------------------------------------------------*/
		INSERT INTO Concept_Relation (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Inherited,
			Multiplicity,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@FromConceptKey,
			@ToConceptKey,
			@ThesaurusRelationTypeKey,
			IsNull(@Inherited, 0),
			@Multiplicity,
			@Comment,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Make corresponding changes to lineage table if this is a 
			parent relationship
		\*-------------------------------------------------------------*/
		IF @ThesaurusRelationTypeKey=@ParentRelationTypeKey
		BEGIN
			EXECUTE		usp_ConceptLineage_NewRelation	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit:
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [Dev - JNCC SQL]
END

GO
			

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptSimple_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptSimple_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new concept and term (if necessary).

  Parameters:	@Key OUTPUT,
		@TermKey 
		@ConceptGroupKey 
		@TermVersionKey 
		@ListPreferred 
		@IsCurrent
		@Preferred 
		@ConceptRankKey 
		@NameTypeConceptKey 
		@MeaningKey 
		@AuthorCopy 
		@SortCode 
		@ListCode
		@SessionID 
		@SystemSuppliedData 

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConceptSimple_Insert]
	@Key char(16) OUTPUT,
	@TermKey char(16),
	@ConceptGroupKey char(16),
	@TermVersionKey char(16) = NULL,
	@ListPreferred bit = NULL,
	@IsCurrent bit = NULL,
	@Preferred bit = NULL,
	@ConceptRankKey char(16) = NULL,
	@NameTypeConceptKey char(16) = NULL,
	@MeaningKey char(16) = NULL,
	@AuthorCopy varchar(100) = NULL,
	@SortCode int = NULL,
	@ListCode varchar(50) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF
	SET XACT_ABORT ON

	BEGIN TRANSACTION
		DECLARE @NewMeaningKey char(16)
		
		SET @ListPreferred = IsNull(@ListPreferred, 1)
	
		/*-------------------------------------------------------------*\
		  If we don't have a meaning key, create one.
		\*-------------------------------------------------------------*/
		IF @MeaningKey IS NULL
		BEGIN
			EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT--, @SiteID
			IF @@ERROR <> 0 GOTO RollbackAndExit
			
			INSERT INTO Meaning (
				Meaning_Key
			) VALUES (
				@NewMeaningKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
			SET @NewMeaningKey = @MeaningKey
	
		/*-------------------------------------------------------------*\
		  Create new Concept.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept', @Key OUTPUT
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		INSERT INTO Concept (
			Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, 
			Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, 
			Meaning_Key, Author_Copy, Sort_Code, List_Code, 
			Entered_Session_ID, System_Supplied_Data
		) VALUES (
			@Key, @TermKey, @ConceptGroupKey, @ListPreferred, 
			IsNull(@IsCurrent, 1), IsNull(@Preferred, 0), @ConceptRankKey,
			@NameTypeConceptKey, @NewMeaningKey, @AuthorCopy, @SortCode,
			@ListCode, @SessionID, 0
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	
		/*-------------------------------------------------------------*\
		  Create Concept_Lineage.
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_NewConcept	@Key
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		/*----------------------------------------------------------------------------*\
		  If @Preferred = 1, then make sure the updated concept is the 
		  only Preferred synonym with the same language key and name type concept key.
		\*----------------------------------------------------------------------------*/
		IF @Preferred = 1 
			UPDATE		CSynonyms
			SET		Preferred = 0
			FROM 		Concept AS CSource
			INNER JOIN	Term 	AS TSource 	ON TSource.Term_Key = CSource.Term_Key
			INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
								AND CSynonyms.Name_Type_Concept_Key = CSource.Name_Type_Concept_Key
								AND CSynonyms.Concept_Key <> CSource.Concept_Key
			INNER JOIN	Term 	AS TSynonyms 	ON TSynonyms.Term_Key = CSynonyms.Term_Key
								AND TSynonyms.Language_Key = TSource.Language_Key
			WHERE		CSource.Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSimple_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ConceptSimple_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the concept table.

  Parameters:	@Key
		@TermKey 
		@ConceptGroupKey 
		@Preferred
		@ConceptRankKey 
		@NameTypeConceptKey
		@SortCode 
		@ListCode 
		@SessionID 
		@RecordsAffected 
		@Timestamp 

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSimple_Update]
	@Key char(16),
	@TermKey char(16),
	@ConceptGroupKey char(16),
	@ListPreferred bit = NULL,
	@Preferred bit,
	@ConceptRankKey char(16),
	@NameTypeConceptKey char(16) = NULL,
	@SortCode int,
	@ListCode varchar(50),
	@SessionID char(16),
	@RecordsAffected int OUTPUT,
	@Timestamp timestamp
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		/*-------------------*\
		  Update the Concept.
		\*-------------------*/
		DECLARE @old_concept_group_key CHAR(16),
			@old_list_preferred BIT,
			@OldConceptRankKey char(16),
			@error INT
		
		UPDATE	Concept
		SET 	@old_concept_group_key = Concept_Group_Key,
			@old_list_preferred = List_Preferred,
			@OldConceptRankKey = Concept_Rank_Key,
			List_Preferred = IsNull(@ListPreferred, List_Preferred),
			Concept_Group_Key = @ConceptGroupKey,
			Term_Key = @TermKey,
			Concept_Rank_Key = @ConceptRankKey,
			Preferred = @Preferred,
			Name_Type_Concept_Key = @NameTypeConceptKey,
			Sort_Code = @SortCode,
			List_Code = @ListCode,
			Changed_Session_ID = @SessionID			
		WHERE	Concept_Key = @Key
		AND	TSEqual (@Timestamp, Timestamp)

		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		/*----------------------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage
		\*----------------------------------------------------------------------------*/
		EXECUTE	usp_ConceptLineage_ConceptUpdated	@Key,
								@old_concept_group_key,
								@old_list_preferred
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		/*----------------------------------------------------------------------------*\
		  If @Preferred = 1, then make sure the updated concept is the 
		  only Preferred synonym with the same language key and name type concept key.
		\*----------------------------------------------------------------------------*/
		IF @Preferred = 1 
			UPDATE		CSynonyms
			SET		Preferred = 0
			FROM 		Concept AS CSource
			INNER JOIN	Term 	AS TSource 	ON TSource.Term_Key = CSource.Term_Key
			INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
								AND CSynonyms.Name_Type_Concept_Key = CSource.Name_Type_Concept_Key
								AND CSynonyms.Concept_Key <> CSource.Concept_Key
			INNER JOIN	Term 	AS TSynonyms 	ON TSynonyms.Term_Key = CSynonyms.Term_Key
								AND TSynonyms.Language_Key = TSource.Language_Key
			WHERE		CSource.Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSimple_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptSynonym_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptSynonym_Insert]
GO

/*===========================================================================*\
  Description:	Create a new synonym of an existing concept

  Parameters:	@PreferredConceptKey CHAR(16),
	@Term VARCHAR(150),
	@LanguageKey VARCHAR(4),
	@NameTypeConceptKey CHAR(16),
	@SessionID CHAR(16),
	@SiteID char(8) = NULL,
	@SystemSuppliedData BIT=0,
	@ConceptKey CHAR(16) OUTPUT - created concept key

  Created:	December 2005

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSynonym_Insert]
	@PreferredConceptKey CHAR(16),
	@Term VARCHAR(150),
	@LanguageKey VARCHAR(4),
	@NameTypeConceptKey CHAR(16),
	@SessionID CHAR(16),
	@SiteID char(8) = NULL,
	@SystemSuppliedData BIT=0,
	@ConceptKey CHAR(16) OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DECLARE 
			@TermKey CHAR(16),
			@MeaningKey CHAR(16),
			@ConceptGroupKey CHAR(16),
			@SynonymConceptKey CHAR(16)

		-- Find matching term record, if any
		SELECT @TermKey=Term_Key
		FROM Term
		WHERE Plaintext=@Term AND Language_Key=@LanguageKey
		
		-- No matching term record, so create one
		IF @TermKey IS NULL
		BEGIN
			EXEC usp_Term_Insert @TermKey OUTPUT, @LanguageKey, @Term, @Term, @SessionID, @SystemSuppliedData
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		
		-- Identify the preferred term's meaning and concept group so we can link to it
		SELECT @MeaningKey = Meaning_Key, @ConceptGroupKey = Concept_Group_Key
		FROM Concept WHERE Concept_Key=@PreferredConceptKey
		
		EXEC spNextKey 'CONCEPT', @ConceptKey OUTPUT, @SiteID

		INSERT INTO Concept (
			Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, Is_Current, Preferred, 
			Name_Type_Concept_Key, Meaning_Key, Entered_Session_ID)
		VALUES (
			@ConceptKey, @TermKey, @ConceptGroupKey, 0, 1, 0, 
			@NameTypeConceptKey, @MeaningKey, @SessionID)
		IF @@Error <> 0 GOTO RollbackAndExit		

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSynonym_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSynonym_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSynonym_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSynonym_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptSynonym_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptSynonym_Update]
GO

/*===========================================================================*\
  Description:	Update a simple non preferred synonym

  Parameters:	@Key		Concept key.
		@Term		Term string
		@LanguageKey

  Created:	December 2005

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSynonym_Update]
	@ConceptKey CHAR(16),
	@Term VARCHAR(150),
	@LanguageKey VARCHAR(4),
	@SessionID CHAR(16),
	@SystemSuppliedData BIT=0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE 
		@TermKey CHAR(16),
		@OldTermKey CHAR(16)
		 

	BEGIN TRANSACTION

		-- Check if the term record is still OK
		SELECT @TermKey = T.Term_Key
		FROM Concept C
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		WHERE T.Plaintext=@Term
		AND T.Language_Key=@LanguageKey
		AND C.Concept_Key=@ConceptKey

		IF @TermKey IS NULL 
		BEGIN
			-- Term does not match, so need to find a new one
			
			SELECT @OldTermKey = Term_Key
			FROM Concept
			WHERE Concept_Key=@ConceptKey
			IF @@Error <> 0 GOTO RollbackAndExit

			-- Is there an existing match?
			SELECT @TermKey=Term_Key
			FROM Term
			WHERE Plaintext=@Term AND Language_Key=@LanguageKey
			
			-- No matching term record, so create one
			IF @TermKey IS NULL
			BEGIN
				EXEC usp_Term_Insert @TermKey OUTPUT, @LanguageKey, @Term, @Term, @SessionID, @SystemSuppliedData
				IF @@Error <> 0 GOTO RollbackAndExit
			END
			
			-- Now update the synonym concept
			UPDATE Concept 
			SET Term_Key=@TermKey, Changed_Session_ID=@SessionID
			WHERE Concept_Key=@ConceptKey
			IF @@Error <> 0 GOTO RollbackAndExit	

			-- Remove the old term if no longer required
			DELETE T
			FROM Term T
			LEFT JOIN Concept C ON  C.Term_Key=T.Term_Key
				AND C.Concept_Key<>@ConceptKey
			LEFT JOIN Term_Version TV ON TV.Term_Key=T.Term_Key
			WHERE C.Concept_Key IS NULL
			AND T.System_Supplied_Data = 0
			AND T.Term_Key=@OldTermKey
			AND TV.Term_Version_Key IS NULL
			IF @@Error <> 0 GOTO RollbackAndExit			

		END	

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSynonym_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSynonym_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSynonym_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSynonym_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSynonym_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ChooseListPreferred_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_ChooseListPreferred_Update]
GO

/*===========================================================================*\
  Description:	Takes a concept key from a concept that is about to stop
		being a synonym, selects all the other synonyms and makes the
		first one list preferred.

  Parameters:	@ConceptGroupKey - key of the concept group
		@ConceptKey - concept key

  Created:	March 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ChooseListPreferred_Update]
	@Key varchar(16)
AS
	DECLARE	@ConceptKeyMakePreferred char(16)

	SELECT TOP 1	@ConceptKeyMakePreferred = C1.Concept_Key
	FROM		Concept AS C1
	INNER JOIN 	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C2.Concept_Key = @Key
	AND		C2.Concept_Key <> C1.Concept_Key
	ORDER BY	C1.Sort_Code

	IF @ConceptKeyMakePreferred IS NOT NULL 
	BEGIN
		UPDATE	Concept
		SET	List_Preferred = 1
		WHERE	Concept_Key = @ConceptKeyMakePreferred
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ChooseListPreferred_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ChooseListPreferred_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ChooseListPreferred_Update TO [Dev - JNCC SQL]
END

GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept tables. Also deletes records
		from other tables where necessary.

  Parameters:	@Key		Concept key.
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @MeaningKey char(16),
			@TermKey char(16),
			@TermVersionKey char(16),
			@ConceptsSharingMeaningKeyCount int,
			@ConceptsSharingTermKeyCount int,
			@ConceptsSharingTermVersionKeyCount int,
			@OriginalTimestamp timestamp

	-- Store the Meaning, Term and Term Version keys because the concept record
	-- needs to be deleted before these other records can be, due to referential
	-- integrity.
	SELECT	@MeaningKey = Meaning_Key,
			@TermKey = Term_Key,
			@TermVersionKey = Term_Version_Key,
			@OriginalTimestamp = [Timestamp]
	FROM 	Concept
	WHERE	Concept_Key = @Key

	-- Count the number of concepts that use this meaning key.
	SELECT 		@ConceptsSharingMeaningKeyCount = Count(C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Key = C1.Term_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term version key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermVersionKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Version_Key = C1.Term_Version_Key
	WHERE		C1.Concept_Key = @Key


	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Item table exists before
			  attempting any of this deletion. In the future, the 
			  Thesaurus module could be installed without the Taxon
			  tables, so would go wrong if we tried to delete from
			  non-existant tables.			
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Item]')
					AND 	  Type = 'U')
			BEGIN
				-- Get the Taxon List Item Key for the current Concept
				DECLARE @TaxonListItemKey char(16)
	
				SELECT 	@TaxonListItemKey = Taxon_List_Item_Key
				FROM	Taxon_Dictionary_Concept_Mapping
				WHERE	Concept_Key = @Key

				/*--------------------------------------------------------*\
				  Delete the records related to the Taxon_List_Item table
				\*--------------------------------------------------------*/
				DELETE 	Taxon_Dictionary_Concept_Mapping
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				AND	Concept_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Common_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Synonym
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Synonym_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Export_Filter_Taxon
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Group
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Contained_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Designation
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_User_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Nameserver
				WHERE	Recommended_Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- If one Concept shares the Term, attempt to delete the equivalent Taxon.
				IF @ConceptsSharingTermKeyCount = 1
				BEGIN
					DECLARE @TaxonKey char(16)

					-- Get the key of the equivalent Taxon
					SELECT 	@TaxonKey = Taxon_Key
					FROM	Taxon_Dictionary_Term_Mapping
					WHERE	Term_Key = @TermKey

							-- Only delete if there are no Taxon_Version records using the Taxon
					IF NOT EXISTS(SELECT 	*
									FROM 	Taxon_Version
									WHERE	Taxon_Key = @TaxonKey)
					BEGIN
						DELETE SF
						FROM Source_File SF
						INNER JOIN Taxon_Sources TS ON TS.Source_Key=SF.Source_Key
						WHERE TS.Taxon_Key=@TaxonKey
		
						DELETE Taxon_Sources
						WHERE Taxon_Key=@TaxonKey
					
						DELETE	Taxon
						WHERE	Taxon_Key = @TaxonKey
					END
				END

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLI record
				  is referred to in the Taxon_Determination table, or a row in 
				  the TLI table has its Parent set to the record we are attempting
				  to delete. This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18). Before deleting the TLI records, we
				  need to remove the Taxon_Dictionary_Meaning_Mapping records.
				\*-----------------------------------------------------------------*/ 
				DELETE	Taxon_Dictionary_Meaning_Mapping
				WHERE	Preferred_Name = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE 	Taxon_List_Item
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END			
	
		/*====================================*\
		  Delete the records.
		\*====================================*/
		-- Delete the Concept_History record.
		DELETE	Concept_History
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the relation records which refer to the concept.
		\*-------------------------------------------------------*/
		DELETE	Concept_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Meaning_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Term_Version_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the Enquiry_Concept records because otherwise
		  the deletion will fail because it says other records
		  link to the Concept. Enquiries cannot be viewed in the
		  Thesaurus Editor it would appear at a casual glance
		  that nothing is actually linked to the concept. 
		  So best to just delete the Enquiry_Concept join records.
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Enquiry_Concept]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE	Enquiry_Concept
			WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_Lineage records.
		IF EXISTS (SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteConcept	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------*\
			Delete the concept's designation records (and related)
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Designation_Mapping]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE DM
			FROM Taxon_Dictionary_Concept_Designation_Mapping DM
			INNER JOIN Concept_Designation CD ON CD.Concept_Designation_Key=DM.Concept_Designation_Key
			WHERE CD.Concept_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit		

		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Source_Join]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
		BEGIN
			--Delete the source files
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept_Designation'
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
	
			IF @@Error <> 0 GOTO RollbackAndExit
		
			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
			WHERE SJ.Table_Name='Concept_Designation'

			IF @@Error <> 0 GOTO RollbackAndExit

			--Delete the source files for the main concept
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit

			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			WHERE SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE 
		FROM Concept_Designation
		WHERE Concept_Key=@Key

		/*-------------------------------------------------------*\
			 Delete the Concept record. Have to check timestamp passed into the proc
			 against the timestamp the Concept had before any of its related records
			 were deleted. This is because deleting the records above may cause
			 triggers to be fired. Deleting the record in Concept_History will fire
			 a trigger that updates the current Concept, causing its timestamp to 
			 change.
		\*-------------------------------------------------------*/

		DELETE	Concept
		WHERE	Concept_Key = @Key
		AND		(TSEqual(@Timestamp, @OriginalTimestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Meaning record if only one Concept uses that Meaning key.
		IF @ConceptsSharingMeaningKeyCount = 1 
			DELETE 	Meaning
			WHERE	Meaning_Key = @MeaningKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term Version record if only one Concept uses that Term Version key.
		IF @ConceptsSharingTermVersionKeyCount = 1
			DELETE	Term_Version
			WHERE	Term_Version_Key = @TermVersionKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term record if only one Concept uses that Term key.
		IF @ConceptsSharingTermKeyCount = 1
			IF NOT EXISTS(SELECT * FROM Term_Version WHERE Term_Key = @TermKey)	
				DELETE	Term
				WHERE	Term_Key = @TermKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Paste') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Concept_Paste]
GO

/*===========================================================================*\
  Description:	Pastes a concept from one position to another

  Parameters:	@DestConceptKey CHAR(16) - output param = key of newly pasted concept

  Created:	Aug 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Paste]
	@ConceptKey CHAR(16),
	@DestConceptGroupKey CHAR(16),
	@DestParentConceptKey CHAR(16),
	@IsCut BIT,
	@SessionID CHAR(16),
	@SystemSuppliedData BIT=0,
	@DestConceptKey CHAR(16) OUTPUT
AS

BEGIN TRANSACTION

	/*-------------------------------------------------------------*\
		Prepare things for the operation
	\*-------------------------------------------------------------*/

	--Enforce a value in @SystemSuppliedData as the default value 
	--doesn't seem to work every time
	IF @SystemSuppliedData IS NULL
		SET @SystemSuppliedData=0

	DECLARE @SrcConceptGroupKey CHAR(16)
	DECLARE @Lineage VARCHAR(900)
	DECLARE @OldRelationTypeKey CHAR(16)
	DECLARE @NewRelationTypeKey CHAR(16)
	DECLARE @Key CHAR(16)


	SELECT 	@SrcConceptGroupKey = Concept_Group_Key
	FROM 	Concept
	WHERE 	Concept_Key = @ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	--Find the source concept group's hierarchy relationship
	SELECT 	@OldRelationTypeKey = CG.Hierarchy_Relation_Type_Key
	FROM 	Concept C
	JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	WHERE 	C.Concept_Key = @ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	--Find the dest concept group's hierarchy relationship
	IF @DestParentConceptKey IS NULL 
		SET @NewRelationTypeKey=@OldRelationTypeKey
	ELSE
	BEGIN
		SELECT	@NewRelationTypeKey = CG.Hierarchy_Relation_Type_Key
		FROM 	Concept C
		JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
		WHERE 	C.Concept_Key = @DestParentConceptKey
	END

/*-------------------------------------------------------------*\
	Perform the cut or copy operation
\*-------------------------------------------------------------*/
IF @IsCut=1 
BEGIN
	SET @DestConceptKey=@ConceptKey

	--Prepare to delete subtree of lineage
	SELECT @Lineage=Lineage
	FROM Concept_Lineage
	WHERE Concept_Key=@ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	IF @DestParentConceptKey IS NULL
	BEGIN
		--Delete source's parent relationship(s)
		DECLARE @KeyToDel CHAR(16)
		DECLARE @Timestamp TIMESTAMP

		DECLARE csr CURSOR STATIC LOCAL FOR
			SELECT Concept_Relation_Key, Timestamp
			FROM Concept_Relation 
			WHERE To_Concept_Key=@ConceptKey
			AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey

		OPEN csr
		
		WHILE 1=1
		BEGIN
			FETCH NEXT FROM csr INTO @KeyToDel, @Timestamp

			IF @@FETCH_STATUS<>0 
				BREAK
			
			EXEC usp_ConceptRelation_Delete @KeyToDel, @Timestamp
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	END
	ELSE
	BEGIN
		--Update source's parent relationship to point to new parent key
		IF EXISTS(SELECT 1 FROM Concept_Relation 
					WHERE To_Concept_Key=@ConceptKey
					AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey)
		BEGIN
			DECLARE @OldKey CHAR(16)
			SELECT @OldKey=From_Concept_Key, @Key=Concept_Relation_Key
			FROM Concept_Relation
			WHERE To_Concept_Key=@ConceptKey
				AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit

			UPDATE Concept_Relation 
			SET From_Concept_Key=@DestParentConceptKey,
				Changed_Session_ID=@SessionID,
				Thesaurus_Relation_Type_Key=@NewRelationTypeKey
			WHERE To_Concept_Key=@ConceptKey
				AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE		usp_ConceptLineage_UpdateRelation	
					@Key,
					@OldKey,
					@ConceptKey,
					@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
		BEGIN
			EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT	
		
			EXEC usp_ConceptRelation_Insert
				@Key,
				@DestParentConceptKey,
				@ConceptKey,
				@OldRelationTypeKey,
				NULL,
				NULL,
				NULL,
				@SessionID, 
				@SystemSuppliedData
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	END

	IF @SrcConceptGroupKey<>@DestConceptGroupKey 
	BEGIN
		--Update concept group for source concepts to new group
		UPDATE CChild
		SET Concept_Group_Key = @DestConceptGroupKey
		FROM VW_ConceptChildren CC 
		INNER JOIN Concept CChild ON CChild.Concept_Key=CC.Child_Concept_Key
			AND CChild.Concept_Group_Key=@SrcConceptGroupKey
		WHERE CC.Parent_Concept_Key=@ConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	-- Actually delete the old lineage information	
	EXEC usp_ConceptLineage_DeleteSubtree @SrcConceptGroupKey, @Lineage
	IF @@Error <> 0 GOTO RollbackAndExit
END
ELSE
BEGIN
	--Whole branch being copied into a the concept group, so find all concepts and clone them
	DECLARE @ChildConceptKey CHAR(16)

	--Create a local table to hold key mappings
	DECLARE @ConceptMapping TABLE (
		Src_Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Dest_Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	--Clone the source concepts, updating concept group key
	DECLARE csr CURSOR STATIC LOCAL FOR
		SELECT 	CChild.Concept_Key
		FROM 	VW_ConceptChildrenOnly CC 
		JOIN 	Concept CChild	ON CChild.Concept_Key = CC.Child_Concept_Key
					AND CChild.Concept_Group_Key = @SrcConceptGroupKey
		WHERE 	CC.Parent_Concept_Key = @ConceptKey
		-- Add the copied concept, instead of duplicating code to clone it separatly
		UNION
		SELECT	@ConceptKey

	OPEN csr
	FETCH NEXT FROM csr INTO @ChildConceptKey
	WHILE @@FETCH_STATUS=0
	BEGIN
		EXECUTE spNextKey 'Concept', @Key OUTPUT
		IF @@Error <> 0 GOTO RollBackAndExit
		
		-- When cloning the actual selected concept, remember the new concept key
		IF @ChildConceptKey = @ConceptKey 
			SET @DestConceptKey = @Key

		-- Rememer mappings so we can update relationships later
		INSERT INTO @ConceptMapping VALUES (@ChildConceptKey, @Key)
		IF @@Error <> 0 GOTO RollBackAndExit

		-- Clone the concept
		INSERT INTO Concept (
			Concept_Key, Term_Key, Concept_Group_Key, Term_Version_Key, List_Preferred, 
			Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, Meaning_Key,
			Author_Copy, Sort_Code, List_Code, Entered_Session_ID, System_Supplied_Data, Custodian
		)
		SELECT 	@Key, Term_Key, @DestConceptGroupKey, Term_Version_Key, List_Preferred, 
			Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, Meaning_Key,
			Author_Copy, Sort_Code, List_Code, @SessionID, @SystemSuppliedData, LEFT(@Key, 8)
		FROM 	Concept 
		WHERE 	Concept_Key = @ChildConceptKey
		IF @@Error <> 0 GOTO RollBackAndExit

		FETCH NEXT FROM csr INTO @ChildConceptKey
	END

	CLOSE csr
	DEALLOCATE csr

	/*-------------------------------------------------------------*\
		Clone the hierarchical relationships within the copied branch
			of concepts
	\*-------------------------------------------------------------*/
	DECLARE @SrcKey CHAR(16), @DestKey CHAR(16)

	--Declare a temp table with same structure as concept relation that 
	--we can populate with dummy primary keys, then update later
	SELECT TOP 0 * INTO #TempRel FROM Concept_Relation
	IF @@Error <> 0 GOTO RollbackAndExit

	DECLARE cmap CURSOR STATIC LOCAL FOR
		--Note we are cloning parent relationships within the branch, so 
		--exclude the top node
		SELECT * FROM @ConceptMapping WHERE Dest_Concept_Key<>@DestConceptKey
	
	OPEN cmap
	
	FETCH NEXT FROM cmap INTO @SrcKey, @DestKey
	WHILE @@FETCH_STATUS=0
	BEGIN
		INSERT INTO #TempRel (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
			)
		SELECT 
			CR.Concept_Relation_Key, -- Will be replaced later
			ISNULL(CM.Dest_Concept_Key, CR.From_Concept_Key),
			@DestKey,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			@SessionID,
			@SystemSuppliedData,
			Left(@DestKey, 8)
		FROM Concept_Relation CR
		LEFT JOIN @ConceptMapping CM ON CM.Src_Concept_Key=CR.From_Concept_Key
		WHERE CR.To_Concept_Key=@SrcKey
		AND CR.Thesaurus_Relation_Type_Key=@OldRelationTypeKey
		IF @@Error <> 0 GOTO RollbackAndExit

		FETCH NEXT FROM cmap INTO @SrcKey, @DestKey
	END
	
	CLOSE cmap
	DEALLOCATE cmap 

	--Now we have a table of concept relationships to insert, but we must update the keys first
	DECLARE crel CURSOR LOCAL FOR
		SELECT Concept_Relation_Key FROM #TempRel
	
	OPEN crel
	
	FETCH NEXT FROM crel INTO @SrcKey
	
	WHILE @@FETCH_STATUS=0
	BEGIN
		EXECUTE spNextKey 'Concept_Relation', @DestKey OUTPUT
		IF @@Error <> 0 GOTO RollbackAndExit
		
		UPDATE #TempRel
		SET Concept_Relation_Key=@DestKey
		WHERE CURRENT OF crel
		IF @@Error <> 0 GOTO RollbackAndExit

		FETCH NEXT FROM crel INTO @SrcKey		
	END

	CLOSE crel
	DEALLOCATE crel

	--Copy the relationships into the concept relation table
	INSERT INTO Concept_Relation (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		)
		SELECT 
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian 
		FROM #TempRel
	IF @@Error <> 0 GOTO RollbackAndExit

	DROP TABLE #TempRel
END

	/*-------------------------------------------------------------*\
	 Join the copied branch of concepts to the destination concept.
	 This also fixes up the lineage.
	\*-------------------------------------------------------------*/
	IF (@DestParentConceptKey IS NOT NULL) AND ((@SrcConceptGroupKey<>@DestConceptGroupKey) OR (@IsCut=0))
	BEGIN
		EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT	

		EXEC usp_ConceptRelation_Insert
			@Key,
			@DestParentConceptKey,
			@DestConceptKey,
			@OldRelationTypeKey,
			NULL,
			NULL,
			NULL,
			@SessionID, 
			@SystemSuppliedData
		IF @@Error <> 0 GOTO RollbackAndExit
	END
	ELSE BEGIN
		IF @DestParentConceptKey IS NULL
		EXEC usp_ConceptLineage_CreateSubtree @DestConceptKey, ''
	END

COMMIT TRANSACTION

RETURN

RollBackAndExit: 
	IF @@TranCount > 0 ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Paste') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_Concept_Paste'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select]
GO

/*===========================================================================*\
  Description:	Returns fields from the Concept table.

  Parameters:	@ConceptKey

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select]
	@ConceptKey char(16)
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT	
			C.Term_Key AS Item_Key,
			TL.Item_Name AS Item_Name,
			Lower(TL.Language_Key) AS Language_Key,
			L.Language_Key + ' - ' + L.Item_Name AS Language_Name,
			C.Concept_Group_Key,
			CG.Hierarchy_Relation_Type_Key,
			C.Term_Version_Key,	
			C.List_Preferred,
			C.Is_Current,
			C.Preferred,
			C.Concept_Rank_Key,
			CR.Item_Name AS Concept_Rank_Name,
			C.Name_Type_Concept_Key,
			CT.Item_Name AS Name_Type_Concept_Name,
			C.Meaning_Key,
			C.Author_Copy,
			C.Sort_Code,
			C.List_Code,
			CASE WHEN CRel.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren,
			C.System_Supplied_Data,
			C.Entered_Session_ID,
			C.[Timestamp]
	FROM 		Concept AS C
	INNER JOIN	Term AS TL ON TL.Term_Key = C.Term_Key
	INNER JOIN	Language AS L ON L.Language_Key = TL.Language_Key
	LEFT JOIN	Concept_Rank AS CR ON CR.Concept_Rank_Key = C.Concept_Rank_Key
	INNER JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = C.Name_Type_Concept_Key
	INNER JOIN	Concept_Group AS CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	LEFT JOIN 	Concept_Relation CRel ON CRel.From_Concept_Key = C.Concept_Key
       				     	     AND CRel.Thesaurus_Relation_Type_Key = CG.Hierarchy_Relation_Type_Key
	WHERE		C.Concept_Key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForParent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForParent]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are with the supplied parent	

  Parameters:	@ParentConceptKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForParent]
	@ParentConceptKey varchar(100),
  	@HierarchyRelationTypeKey char(16)
AS

SELECT distinct C.Concept_Key, 
		T.Item_Name, 
		C.Sort_Code, 
  		CASE WHEN CR2.Concept_Relation_Key IS NULL THEN 0 
							   ELSE 1 
		END AS HasChildren,
  		C.Concept_Rank_Key
FROM 		Concept_Relation CR1
INNER JOIN 	Concept C ON C.Concept_Key = CR1.To_Concept_Key
INNER JOIN 	Term T ON T.Term_Key = C.Term_Key
LEFT JOIN 	(Concept_Relation CR2 
			INNER JOIN Concept AS C2 ON C2.Concept_Key = CR2.To_Concept_Key
						AND C2.List_Preferred = 1
						AND C2.Is_Current = 1)
		ON CR2.From_Concept_Key = C.Concept_Key
       		AND CR2.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
WHERE 		CR1.From_Concept_Key = @ParentConceptKey
AND 		CR1.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
AND 		C.List_Preferred = 1
AND 		C.Is_Current = 1
ORDER BY 	C.Sort_Code, 
		T.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForParent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForParent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForTopLevel]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForTopLevel]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are the top level for the 
    					 supplied concept group.

  Parameters:	@ConceptGroupKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForTopLevel]
	@ConceptGroupKey char(16),
  	@HierarchyRelationTypeKey char(16)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	SELECT DISTINCT CT.Concept_Key, 
			CT.Item_Name, 
	  		CASE WHEN CR2.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren,
	  		CT.Concept_Rank_Key, 
			CT.Sort_Code,
			CT.PlainText  -- Required by the ORDER BY

	FROM 		VW_ConceptTerm CT
	LEFT JOIN 	Concept_Relation CR1 ON CR1.To_Concept_Key=CT.Concept_Key
	      				    AND CR1.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
			-- This is used to get the children. Join into Concept table to get only
			-- List_Preferred and Is_Current because these are the only one that will be 
			-- visible when expanded.
	LEFT JOIN 	(Concept_Relation CR2 
				INNER JOIN Concept AS C2 ON C2.Concept_Key = CR2.To_Concept_Key
							AND C2.List_Preferred = 1
							AND C2.Is_Current = 1) 
			ON CR2.From_Concept_Key=CT.Concept_Key
	       		AND CR2.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey 
	WHERE 		CT.List_Preferred = 1
	AND 		CT.Is_Current = 1
	AND 		CT.Concept_Group_Key = @ConceptGroupKey
	AND 		CR1.From_Concept_Key IS NULL   -- i.e. No parents, therefore top level concept. 

	ORDER BY 	CT.Sort_Code, CT.PlainText  -- Use PlainText too, so list is alpha sorted when no Sort Codes.

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IndexTaxonName_ApplyNameServer]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer]
GO

/*===========================================================================*\
  Description: Applies the NameServer information to the Index_Taxon_Name
		Recommended_Taxon_List_Item_Key table.  Updates all records where this value
		is null.

  Parameters:	None

  Created:	November 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer]
AS
/* Remove any disconnected index_taxon_name records */
DELETE ITN 
FROM Index_Taxon_Name ITN
LEFT JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
WHERE TLI.Taxon_List_Item_Key IS NULL

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = NS.RECOMMENDED_TAXON_LIST_ITEM_KEY
FROM NAMESERVER NS
INNER JOIN TAXON_LIST_ITEM TLI ON NS.INPUT_TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL 

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM TAXON_LIST TL 
INNER JOIN TAXON_LIST_VERSION TLV ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
INNER JOIN TAXON_LIST_ITEM TLI ON TLV.TAXON_LIST_VERSION_KEY = TLI.TAXON_LIST_VERSION_KEY
INNER JOIN TAXON_LIST_ITEM TLI1 ON TLI.TAXON_VERSION_KEY = TLI1.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON TLI1.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
WHERE TL.PREFERRED=1 AND ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM TAXON_VERSION TV
INNER JOIN TAXON_LIST_ITEM TLI ON TV.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
INNER JOIN TAXON_GROUP TG ON TV.Output_group_key = TG.TAXON_GROUP_KEY
INNER JOIN TAXON_LIST_VERSION TLV ON TLV.TAXON_LIST_KEY=TG.USE_TAXON_LIST_KEY
		AND TLI.TAXON_LIST_VERSION_KEY=TLV.TAXON_LIST_VERSION_KEY
INNER JOIN TAXON_LIST_ITEM AS TLI1 
		ON TLI.TAXON_VERSION_KEY = TLI1.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON TLI1.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM INDEX_TAXON_NAME ITN
INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY=ITN.TAXON_LIST_ITEM_KEY
INNER JOIN TAXON_LIST_ITEM TLI2 on TLI2.TAXON_LIST_ITEM_KEY=TLI.PREFERRED_NAME
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL

--Now set up the recommended sort orders, which depend on the recommended names

UPDATE ITN
SET ITN.Sort_Order=
	LEFT('000', 3 - LEN(CONVERT(VARCHAR(3), ISNULL(TG.Sort_Order,0))))
  + CONVERT(VARCHAR(3), ISNULL(TG.Sort_Order,0)) 
	+ LEFT('00000000', 8 - LEN(CONVERT(VARCHAR(8), ISNULL(TLI.Sort_Code,0))))
  + CONVERT(VARCHAR(8), ISNULL(TLI.Sort_Code,0)) 
FROM Index_Taxon_Name ITN 
INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Recommended_Taxon_List_Item_Key
INNER JOIN Taxon_Version TV ON TV.Taxon_Version_Key=TLI.Taxon_Version_Key
LEFT JOIN Taxon_Group TG ON TG.Taxon_Group_Key=TV.Output_Group_Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IndexTaxonName_ApplyNameServer') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IndexTaxonName_ApplyNameServer'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IndexTaxonName_ApplyNameServer TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IndexTaxonName_ApplyNameServer TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AbundanceQualifier') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_AbundanceQualifier]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_AbundanceQualifier]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Measurement_Qualifier', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Measurement_Qualifier(
			Measurement_Qualifier_Key, Short_Name, Long_Name, Measurement_Type_Key, Entered_By
		) VALUES (
			@Key, CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, 'NBNSYS0000000004', @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#AbundanceQualifiers
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AbundanceQualifier') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_AbundanceQualifier'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AssociationType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_AssociationType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_AssociationType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Relationship_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Relationship_Type(
			Relationship_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#AssociationTypes
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AssociationType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_AssociationType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AssociationType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AssociationType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AssociationType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_RecordType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_RecordType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_RecordType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Record_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Record_Type(
			Record_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, 
			CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, 
			@EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#RecordTypes
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_RecordType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_RecordType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatchNewEntry_SampleType')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_SampleType]
GO

/*===========================================================================*\
  Description:  Create a new item in a term list from an import value.

  Parameters:   @ImportValue            Name of new item
                @EnteredBy              Identifies current user

  Created:      July 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_SampleType]
    @ImportValue    VARCHAR(100),
    @EnteredBy      CHAR(16)
AS
    DECLARE     @sample_type_key    CHAR(16)

    EXECUTE     spNextKey   'Sample_Type',
                            @sample_type_key    OUTPUT
    IF @@ERROR <> 0 RETURN

    BEGIN TRANSACTION

    INSERT INTO Sample_Type (
                Sample_Type_Key,
                Short_Name,
                Long_Name,
                Entered_By)
    VALUES      (@sample_type_key,
                CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END,
                @ImportValue,
                @EnteredBy)

    IF @@ERROR <> 0 GOTO RollbackAndExit

    /* update import table with new data */
    UPDATE      #SampleTypes
    SET         Match_Value     =   Import_Value,
                Match_Key       =   @sample_type_key,
                Match_Count     =   1,
                Manual_Match    =   1,
                Remembered      =   0
    WHERE       Import_Value    =   @ImportValue

    IF @@ERROR <> 0 GOTO RollbackAndExit
    
    COMMIT TRANSACTION
    RETURN 0

RollBackAndExit:
    ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_SampleType') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatchNewEntry_SampleType'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SampleType TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SampleType TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SampleType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_SpecimenType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_SpecimenType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_SpecimenType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Specimen_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Specimen_Type(
			Specimen_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, 
			CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, 
			@EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#SpecimenTypes
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_SpecimenType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_SpecimenType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SpecimenType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SpecimenType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SpecimenType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Substrate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_Substrate]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_Substrate]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Substrate', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Substrate(
			Substrate_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#Substrates
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Substrate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_Substrate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Substrate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Substrate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Substrate TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Languages_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Languages_Select]
GO

/*===========================================================================*\
  Description:	Returns all the Languages.

  Parameters:

  Created:	Setember 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Languages_Select]

AS

SET NOCOUNT ON

	SELECT		Lower(L.Language_Key) AS Language_Key,
			L.Item_Name,
			L.Priority

	FROM		Language AS L

	ORDER BY	ISNULL(Priority, 32767), L.Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Languages_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Languages_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Languages_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Languages_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Languages_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Languages_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Languages_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Languages_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ListSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns List Synonyms

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	/*=============================*\
	  Get the list synonyms.
	\*=============================*/
	SELECT 		CListSynonyms.Concept_Key AS Item_Key,
			IsNull(T.Item_Name + ' ' + TV.Author_And_Date, T.Item_Name) AS Item_Name,
			T.Language_Key,
			L.Item_Name AS Language
	FROM 		Concept AS CSource
	INNER JOIN	Concept AS CListSynonyms 	ON CListSynonyms.Meaning_Key = CSource.Meaning_Key 
							AND CListSynonyms.Concept_Group_Key = CSource.Concept_Group_Key
							AND CListSynonyms.Concept_Key <> @Key
	INNER JOIN	Term AS T 			ON T.Term_Key = CListSynonyms.Term_Key
	INNER JOIN 	Language AS L			ON L.Language_Key=T.Language_Key
	LEFT JOIN	Term_Version AS TV 		ON TV.Term_Version_Key = CListSynonyms.Term_Version_Key
	WHERE 		CSource.Concept_key = @Key

	ORDER BY 	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ListSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ListSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
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
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReferenceKeywords_Get]
	@Key CHAR(16)
AS

	SELECT RK.Reference_Keyword_Key, CT.Concept_Key, CT.Plaintext
	FROM Reference_Keyword RK
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=RK.Concept_Key
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReferenceKeyword_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReferenceKeyword_Delete]
GO

/*===========================================================================*\
  Description:	Delete a keyword for a reference

  Parameters:	@Key		Reference Keyword Key

  Created:	December 2005

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReferenceKeyword_Delete]
	@Key CHAR(16)
AS

	SET NOCOUNT OFF

	DELETE FROM Reference_Keyword WHERE Reference_Keyword_Key=@Key

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReferenceKeyword_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReferenceKeyword_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReferenceKeyword_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReferenceKeyword_Insert]
GO

/*===========================================================================*\
  Description:	Insert a keyword for a reference

  Parameters:	@Key		Reference Keyword Key OUTPUT
			@SourceKey
			@ConceptKey
			@SessionID

  Created:	December 2005

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReferenceKeyword_Insert]
	@Key CHAR(16) OUTPUT,
	@SourceKey CHAR(16),
	@ConceptKey CHAR(16),
	@SessionID CHAR(16)
AS

	SET NOCOUNT OFF
	
	EXEC spNextKey 'Reference_Keyword', @Key OUTPUT

	INSERT INTO Reference_Keyword (Reference_Keyword_Key, Source_Key, Concept_Key, Entered_Session_ID)
	VALUES (@Key, @SourceKey, @ConceptKey, @SessionID)

	IF @@Error <> 0
		RAISERROR ('usp_ReferenceKeyword_Insert failed', 16, 1)
	
	RETURN 0

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReferenceKeyword_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReferenceKeyword_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReferenceKeyword_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReferenceKeyword_Update]
GO

/*===========================================================================*\
  Description:	Update a keyword for a reference

  Parameters:	@Key		Reference Keyword Key
			@ConceptKey
			@SessionID

  Created:	December 2005

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReferenceKeyword_Update]
	@Key CHAR(16),
	@ConceptKey CHAR(16),
	@SessionID CHAR(16)
AS

	SET NOCOUNT OFF

	UPDATE Reference_Keyword 
	SET Concept_Key=@ConceptKey, Changed_Session_ID=@SessionID
	WHERE Reference_Keyword_Key=@Key
	IF @@Error <> 0
		RAISERROR ('usp_ReferenceKeyword_Update failed', 16, 1)
	
	RETURN 0

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReferenceKeyword_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReferenceKeyword_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReferenceKeyword_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_References_Select_ForSearchByKeyword]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_References_Select_ForSearchByKeyword]
GO

/*===========================================================================*\
  Description:	Retrieve a list of references that match the supplied keywords

  Parameters:	@SearchText

  Created:	December 2005

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_References_Select_ForSearchByKeyword]
	@SearchText VARCHAR(500)
AS
	SET @SearchText = RTRIM(LTRIM(@SearchText))
	
	--Create a temp table to hold the parsed search terms
	DECLARE @SearchTerms TABLE 
	(term VARCHAR(150) COLLATE SQL_Latin1_General_CP1_CI_AS)
	
	DECLARE @char INT
	SET @char=1
	DECLARE @currentWord VARCHAR(150)
	
	SET @currentWord=''
	
	--Parse out each individual search term
	WHILE  @char<=LEN(@SearchText)
	BEGIN
		PRINT SUBSTRING(@SearchText, @char, 1)
		IF SUBSTRING(@SearchText, @char, 1) = ' '
		BEGIN
			IF RTRIM(LTRIM(@CurrentWord))<>'' 
				INSERT INTO @SearchTerms VALUES (RTRIM(LTRIM(@CurrentWord)))
			SET @CurrentWord=''
		END
		ELSE
			SET @CurrentWord = @CurrentWord + SUBSTRING(@SearchText, @char, 1)
		SET @char = @char + 1
	END
	
	IF RTRIM(LTRIM(@CurrentWord))<>'' 
				INSERT INTO @SearchTerms VALUES (RTRIM(LTRIM(@CurrentWord)))
	
	-- Find out how many terms we need to match against
	DECLARE @SearchTermCount INT
	SELECT @SearchTermCount = COUNT(*) FROM @SearchTerms
	
	--Query, joining to search terms to ensure they are all matched
	SELECT R.Source_Key, Author, R.Year_Vague_Date_Start, 
	       R.Year_Vague_Date_End, R.Year_Vague_Date_Type, 
	       dbo.ufn_RtfToPlaintext(CAST(R.Title AS VARCHAR(200))) AS Title, Count(DISTINCT ST.Term)
	FROM Reference AS R 
	INNER JOIN VW_REFERENCE_AUTHORS AS A ON R.Source_Key = A.Source_Key
	INNER JOIN Reference_Keyword RK ON RK.Source_Key=R.Source_Key
	INNER JOIN VW_ConceptChildren CC ON CC.Child_Concept_Key=RK.Concept_Key
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=CC.Parent_Concept_Key
	INNER JOIN @SearchTerms ST ON CT.Plaintext LIKE ST.Term + '%' COLLATE SQL_Latin1_General_CP1_CI_AS
	GROUP BY R.Source_Key, Author, R.Year_Vague_Date_Start, 
	       R.Year_Vague_Date_End, R.Year_Vague_Date_Type, dbo.ufn_RtfToPlaintext(CAST(R.Title AS VARCHAR(200)))
	HAVING Count(DISTINCT ST.Term)=@SearchTermCount

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_References_Select_ForSearchByKeyword') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_References_Select_ForSearchByKeyword'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForSearchByKeyword TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Session_Close]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Session_Close]
GO

/*===========================================================================*\
  Description: Records the time that a session was closed

  Parameters:	@SessionID

  Created:	Nov 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Session_Close]
	@SessionID char(16)
AS

UPDATE Session SET Date_Time_End=GetDate() WHERE Session_ID=@SessionID

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Session_Close') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Session_Close'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_FullEdit]
	-- ReadOnly users have permission to run this stored proc because they need to shut
	-- the Session for their SessionID.
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Session_Close TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Session_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Session_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new Session record and generates a SessionID value.

  Parameters:	@Key - output SessionID
		@UserID

  Created:	Nov 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Session_Insert]
	@Key char(16) OUTPUT,
	@UserID char(16)
AS

EXECUTE spNextKey 'Session', @Key OUTPUT

INSERT INTO Session (Session_ID, User_Name_Key)
VALUES (@Key, @UserID)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Session_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Session_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Session_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Session_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Session_Insert TO [R2k_FullEdit]
	-- ReadOnly users have permission to run this stored proc because they will need a SessionID
	-- and this is the proc that actually generates the SessionID.
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Session_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Session_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Session_Insert TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDataEntryLists_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonDataEntryLists_Select]
GO

/*===========================================================================*\
  Description:	Returns list of taxon lists.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDataEntryLists_Select]
AS
	SELECT	TL.Taxon_List_Key AS Item_Key, TL.Item_Name
	FROM	Taxon_List TL
	INNER JOIN Taxon_List_Type TLT 
		ON TLT.Taxon_List_Type_Key=TL.Taxon_List_Type_Key
		AND TLT.Allow_Data_Entry=1
	ORDER BY Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDataEntryLists_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDataEntryLists_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Terms_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Terms_Select_ForSearch]
GO

/*===========================================================================*\
  Description: 	Search proc for Term table.

  Parameters:	@SearchText
		@SearchKey	Language_key

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Terms_Select_ForSearch] 
	@SearchText varchar(100),
	@SearchKey varchar(4) = NULL
AS

SET NOCOUNT ON
	-- NB: This proc does want to search on Item_Name (as opposed to 
	-- Plaintext). This is because as we are dealing with terms, we could
	-- have two terms with the same Plaintext, but different Item_Name
	-- i.e. one is italic, one isn't.

	IF @SearchKey IS NOT NULL 
		SELECT 
				T.Term_Key AS Item_Key,
				Item_Name AS DisplayTerm,
				Item_Name AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		Item_Name LIKE @SearchText + '%'
		AND 		Language_Key = @SearchKey
		ORDER BY 	Plaintext
	ELSE
		SELECT 
				T.Term_Key AS Item_Key,
				Item_Name AS DisplayTerm,
				Item_Name AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		Item_Name LIKE @SearchText + '%'
		ORDER BY 	Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Terms_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Terms_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Term_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a Term record

  Parameters:	@Key
		@LanguageKey 
		@ItemName 
		@Plaintext 
		@SessionID
		@SystemSuppliedData 

  Created:	January 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_Insert]
	@Key char(16) OUTPUT,
	@LanguageKey varchar(4),
	@ItemName nvarchar(150),
	@Plaintext nvarchar(150) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Term', @Key OUTPUT

	BEGIN TRANSACTION
	
		INSERT INTO Term (
			Term_Key,
			Language_Key,
			Item_Name,
			Plaintext,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@LanguageKey,
			@ItemName,
			@Plaintext,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Term_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Term table

  Parameters:	@Key 
		@LanguageKey 
		@ItemName 
		@Plaintext 
		@SessionID 

  Created:	January 2004

  Last revision information:
    $Revision: 4 $
    $Date: 23/12/05 11:35 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_Update]
	@Key char(16),
	@LanguageKey varchar(4),
	@ItemName nvarchar(150),
	@Plaintext nvarchar(150),
	@RecordsAffected int OUTPUT,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Term
		SET 	Language_Key = @LanguageKey,
			Item_Name = @ItemName,
			Plaintext = @Plaintext,
			Changed_Session_ID = @SessionID
		WHERE	Term_Key = @Key

		SET @RecordsAffected = @@Rowcount

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_Update TO [Dev - JNCC SQL]
END
GO

