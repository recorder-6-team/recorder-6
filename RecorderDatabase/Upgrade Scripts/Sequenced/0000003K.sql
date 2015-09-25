SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Boundary_GetLinkedLocation') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Boundary_GetLinkedLocation]
GO

/*===========================================================================*\
  Description:	Returns the location linked to a map boundary.  Returns an
			empty dataset if not linked

  Parameters:	Map_Sheet_Key, Static_Object_ID

  Created:	July 2006

  Last revision information:
    $Revision: 1 $
    $Date: 11/08/06 9:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Boundary_GetLinkedLocation]
@Map_Sheet_Key CHAR(16),
@Static_Object_ID INT
AS
	SELECT LN.Location_Key, LN.Item_Name 
	FROM Location_Boundary LB
	INNER JOIN Location_Name LN ON 
		LN.Location_Key=LB.Location_Key AND LN.Preferred=1
	WHERE LB.Map_Sheet_Key = @Map_Sheet_Key AND LB.[Object_Id] = @Static_Object_ID
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Boundary_GetLinkedLocation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Boundary_GetLinkedLocation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Boundary_GetLinkedLocation TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_SelectForAnyApplication]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_SelectForAnyApplication]
GO

/*===========================================================================*\
  Description:	Retrieve the concept groups linked to the Application table

  Created:	December 2005

  Last revision information:
    $Revision: 1 $
    $Date: 11/08/06 9:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_SelectForAnyApplication]
AS

	SELECT CG.Concept_Group_Key, CG.Item_Name, CG.Hierarchy_Relation_Type_Key
	FROM Concept_Group CG
	INNER JOIN Application_Concept_Group ACG ON ACG.Concept_Group_Key=CG.Concept_Group_Key
	ORDER BY CG.Item_Name

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_SelectForAnyApplication') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_SelectForAnyApplication'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_SelectForAnyApplication TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record from the ConceptRelation table.

  Parameters:	@Key	Concept_Relation_Key
		@Timestamp

  Created:	December 2003

  Last revision information: 
    $Revision: 1 $
    $Date: 11/08/06 9:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Remove Concept_Lineage records.
		\*-------------------------------------------------------------*/
		DECLARE		@from_concept_key	CHAR(16),
					@to_concept_key		CHAR(16),
					@relation_type_key	CHAR(16)

		SELECT		@from_concept_key		=	From_Concept_Key,
					@to_concept_key			=	To_Concept_Key,
					@relation_type_key		=	Thesaurus_Relation_Type_Key
		FROM		Concept_Relation
		WHERE		Concept_Relation_Key	=	@Key

		IF @@ROWCOUNT > 0
		BEGIN
			EXECUTE		usp_ConceptLineage_RelationDeleted	@from_concept_key,
															@to_concept_key,
															@relation_type_key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Update in Concept_Relation.
		\*-------------------------------------------------------------*/
		DELETE 	Concept_Relation
		WHERE	Concept_Relation_Key = @Key	
		AND	TSEqual(@Timestamp, Timestamp)
		
		IF @@Error <> 0 GOTO RollbackAndExit

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
	RAISERROR ('usp_ConceptRelation_Delete failed', 16, 1)
GO	

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [Dev - JNCC SQL]
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
    $Revision: 1 $
    $Date: 11/08/06 9:39 $
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
	IF @OldKey<>@DestParentConceptKey
	BEGIN
		EXEC usp_ConceptLineage_DeleteSubtree @SrcConceptGroupKey, @Lineage
		IF @@Error <> 0 GOTO RollbackAndExit
	END
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GridSquare_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_GridSquare_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a grid square record

  Created:	July 2006

  Last revision information:
    $Revision: 1 $
    $Date: 11/08/06 9:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_GridSquare_Insert]
@Key CHAR(16) OUTPUT,
@Spatial_Ref VARCHAR(20),
@Location_Key CHAR(16),
@Spatial_Ref_System VARCHAR(4),
@Spatial_Ref_Qualifier VARCHAR(20),
@Size INT,
@Lat FLOAT,
@Long FLOAT,
@Entered_By CHAR(16)
AS

	SET NOCOUNT OFF
	
	EXEC spNextKey 'Grid_Square', @Key OUTPUT
	
	--Existing records are not duplicated
	IF EXISTS(SELECT 1 FROM Grid_Square WHERE Spatial_Ref=@Spatial_Ref AND Location_Key=@Location_Key and Spatial_Ref_System=@Spatial_Ref_System)
		SELECT @Key=Grid_Square_Key	
		FROM Grid_Square 
		WHERE Spatial_Ref=@Spatial_Ref AND Location_Key=@Location_Key and Spatial_Ref_System=@Spatial_Ref_System
	ELSE
		INSERT INTO Grid_Square (
			Grid_Square_Key, Spatial_Ref, Location_Key, Spatial_Ref_System, Spatial_Ref_Qualifier, Size, Lat, Long, Entered_By)
		VALUES (
			@Key, @Spatial_Ref, @Location_Key, @Spatial_Ref_System, @Spatial_Ref_Qualifier, @Size, @Lat, @Long, @Entered_By)

	IF @@Error <> 0
		RAISERROR ('usp_GridSquare_Insert failed', 16, 1)
	
	RETURN 0


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GridSquare_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_GridSquare_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_GridSquare_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_GridSquare_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_GridSquare_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_GridSquare_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Name_Select_ForNameSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Name_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of names matching a search string..

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 11/08/06 9:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Name_Select_ForNameSearch]
	@SearchText varchar(100)
AS
	SET NO_BROWSETABLE OFF

	SELECT	Name_Key AS Item_Key, 'Individual' AS Type,
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Individual
	WHERE 	Forename LIKE @SearchText + '%'
	OR 	(Initials LIKE @SearchText + '%' AND Forename IS NULL)
	OR 	Surname LIKE @SearchText + '%'
	OR 	Title LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'
UNION
	SELECT	Name_Key AS Item_Key, 'Organisation' AS Type,
		dbo.ufn_GetFormattedName(Name_Key) AS DisplayTerm, 
		dbo.ufn_GetFormattedName(Name_Key) AS SearchTerm
	FROM	Organisation
	WHERE	Acronym LIKE @SearchText + '%'
	OR	Full_Name LIKE @SearchText + '%'
	OR	dbo.ufn_GetFormattedName(Name_Key) LIKE @SearchText + '%'

	-- Set the order here for all
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Name_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Name_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Name_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id(N'[dbo].[usp_RemoveUnwantedOccurrences]') AND ObjectProperty(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_RemoveUnwantedOccurrences]
GO
    
/*
  DESCRIPTION
  This procedure removes invalid taxon and biotope occurrences
  that could have appeared after an import.  Returns the deleted 
  occurrence keys

  PARAMETERS
  None

  Last Revision Details:
    $Revision: 1 $
    $Date: 11/08/06 9:39 $
    $Author: Johnvanbreda $
    
*/

CREATE PROCEDURE [dbo].[usp_RemoveUnwantedOccurrences]
AS

SET NOCOUNT ON

   BEGIN TRAN
	-- Gather invalid taxon occurrences keys, they will be used several times
	SELECT Occ.Taxon_Occurrence_Key INTO #DeleteTaxa
	FROM Taxon_Occurrence Occ 
	LEFT JOIN Taxon_Determination TD ON TD.Taxon_Occurrence_Key=Occ.Taxon_Occurrence_Key
	WHERE TD.Taxon_Determination_Key IS NULL

	--Record the records we are about to remove
	SELECT Taxon_Occurrence_Key AS ItemKey, CAST('TAXON_OCCURRENCE' AS VARCHAR(30)) as TableName
 	INTO #Deletions
	FROM #DeleteTaxa

	INSERT INTO #Deletions
	SELECT TOS.Source_Link_Key AS ItemKey, 'TAXON_OCCURRENCE_SOURCES' as TableName
	FROM TAXON_OCCURRENCE_SOURCES TOS
	INNER JOIN #DeleteTaxa D ON D.Taxon_Occurrence_Key=TOS.Taxon_Occurrence_Key

	INSERT INTO #Deletions
	SELECT TOD.Taxon_Occurrence_Data_Key AS ItemKey, 'TAXON_OCCURRENCE_DATA' as TableName
	FROM TAXON_OCCURRENCE_DATA TOD
	INNER JOIN #DeleteTaxa D ON D.Taxon_Occurrence_Key=TOD.Taxon_Occurrence_Key

	-- Remove associated Taxon Occurrence Sources
	DELETE FROM Taxon_Occurrence_Sources 
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Remove associated Taxon Occurrence Data
	DELETE FROM Taxon_Occurrence_Data
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- And finally remove Taxon Occurrences
	DELETE FROM Taxon_Occurrence 
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Gather invalid biotope occurrences keys, they will be used several times
	SELECT 	Occ.Biotope_Occurrence_Key into #DeleteBiotopes
	FROM Biotope_Occurrence Occ WHERE Occ.Biotope_Occurrence_Key NOT IN (
		SELECT Biotope_Occurrence_Key FROM Biotope_Determination
	)

	--Record the records we are about to remove
	INSERT INTO #Deletions
	SELECT Biotope_Occurrence_Key AS ItemKey, 'BIOTOPE_OCCURRENCE' as TableName
	FROM #DeleteBiotopes

	INSERT INTO #Deletions
	SELECT BOS.Source_Link_Key AS ItemKey, 'BIOTOPE_OCCURRENCE_SOURCES' as TableName
	FROM BIOTOPE_OCCURRENCE_SOURCES BOS
	INNER JOIN #DeleteBiotopes D ON D.Biotope_Occurrence_Key=BOS.Biotope_Occurrence_Key

	INSERT INTO #Deletions
	SELECT BOD.Biotope_Occurrence_Data_Key AS ItemKey, 'BIOTOPE_OCCURRENCE_DATA' as TableName
	FROM BIOTOPE_OCCURRENCE_DATA BOD
	INNER JOIN #DeleteBiotopes D ON D.Biotope_Occurrence_Key=BOD.Biotope_Occurrence_Key

	-- Remove associated Biotope Occurrence Sources
	DELETE FROM Biotope_Occurrence_Sources
	WHERE Biotope_Occurrence_Key IN (
		SELECT Biotope_Occurrence_Key FROM #DeleteBiotopes
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Remove associated Biotope Occurrence Data
	DELETE FROM Biotope_Occurrence_Data
	WHERE Biotope_Occurrence_Key IN (
		SELECT Biotope_Occurrence_Key FROM #DeleteBiotopes
	)
	IF @@Error > 0 GOTO RollBackAndExit
	
	-- And finally remove Biotope Occurrences
	DELETE FROM Biotope_Occurrence 
	WHERE Biotope_Occurrence_Key IN (
		SELECT Biotope_Occurrence_Key FROM #DeleteBiotopes
	)
	IF @@Error > 0 GOTO RollBackAndExit

	SELECT * FROM #Deletions

  COMMIT TRAN

SET NOCOUNT OFF 

RollBackAndExit: 
    IF @@TranCount> 0 ROLLBACK TRAN 
    SET NOCOUNT OFF  
GO 

-- Grant access permissions
IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id('[dbo].[usp_RemoveUnwantedOccurrences]') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_RemoveUnwantedOccurrences] TO [R2k_Administrator]
END
GO 



