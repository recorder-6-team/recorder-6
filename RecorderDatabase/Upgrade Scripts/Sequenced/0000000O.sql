/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ComputerMap_DeleteInvalid]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ComputerMap_DeleteInvalid]
GO

/*===========================================================================*\
  Description:	Delete invalid records from Copmuter_Map, in case something
		wasn't cleaned up properly.

  Parameters:	<none>

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/02/04 10:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ComputerMap_DeleteInvalid]
AS
	DELETE Computer_Map
	WHERE Base_Map_Key NOT IN (SELECT Base_Map_Key FROM Base_Map)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ComputerMap_DeleteInvalid') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ComputerMap_DeleteInvalid'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        	GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ComputerMap_DeleteInvalid TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEvent_Update_ForDragDrop]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEvent_Update_ForDragDrop]
GO

/*===========================================================================*\
  Description:	Update an event's parent key after a Drag/Drop or Cut/Paste
		operation.

  Parameters:	@Key		Event to update.
		@ParentKey	New parent Survey.
		@UserID		ID of user making the change.

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/02/04 10:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEvent_Update_ForDragDrop]
	@Key char(16),
	@ParentKey char(16),
	@UserID char(16)
AS
	UPDATE	Survey_Event
	SET	Survey_Key = @ParentKey,
		Changed_By = @UserID,
		Changed_Date = GetDate()
	WHERE	Survey_Event_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEvent_Update_ForDragDrop') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEvent_Update_ForDragDrop'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEvent_Update_ForDragDrop TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Update_ForDragDrop TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Update_ForDragDrop TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEvent_Update_ForDragDrop TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Update_ForDragDrop]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Update_ForDragDrop]
GO

/*===========================================================================*\
  Description:	Update a sample's parent key after a Drag/Drop or Cut/Paste
		operation.

  Parameters:	@Key		Sample to update.
		@ParentKey	New parent Event.
		@UserID		ID of user making the change.

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/02/04 10:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Update_ForDragDrop]
	@Key char(16),
	@ParentKey char(16),
	@UserID char(16)
AS
	UPDATE	[Sample]
	SET	Survey_Event_Key = @ParentKey,
		Changed_By = @UserID,
		Changed_Date = GetDate()
	WHERE	Sample_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Update_ForDragDrop') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Update_ForDragDrop'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Sample_Update_ForDragDrop TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Update_ForDragDrop TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Update_ForDragDrop TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Update_ForDragDrop TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_BiotopeOccurrence_Update_ForDragDrop]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_BiotopeOccurrence_Update_ForDragDrop]
GO

/*===========================================================================*\
  Description:	Update a biotope occurrence's parent key after a Drag/Drop or Cut/Paste
		operation.

  Parameters:	@Key		Biotope occurrence to update.
		@ParentKey	New parent Sample.
		@UserID		ID of user making the change.

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/02/04 10:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_BiotopeOccurrence_Update_ForDragDrop]
	@Key char(16),
	@ParentKey char(16),
	@UserID char(16)
AS
	UPDATE	Biotope_Occurrence
	SET	Sample_Key = @ParentKey,
		Changed_By = @UserID,
		Changed_Date = GetDate()
	WHERE	Biotope_Occurrence_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BiotopeOccurrence_Update_ForDragDrop') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_BiotopeOccurrence_Update_ForDragDrop'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_BiotopeOccurrence_Update_ForDragDrop TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_BiotopeOccurrence_Update_ForDragDrop TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_BiotopeOccurrence_Update_ForDragDrop TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_BiotopeOccurrence_Update_ForDragDrop TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonOccurrence_Update_ForDragDrop]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonOccurrence_Update_ForDragDrop]
GO

/*===========================================================================*\
  Description:	Update a taxon occurrence's parent key after a Drag/Drop or Cut/Paste
		operation.

  Parameters:	@Key		Taxon occurrence to update.
		@ParentKey	New parent Sample.
		@UserID		ID of user making the change.

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/02/04 10:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrence_Update_ForDragDrop]
	@Key char(16),
	@ParentKey char(16),
	@UserID char(16)
AS
	UPDATE	Taxon_Occurrence
	SET	Sample_Key = @ParentKey,
		Changed_By = @UserID,
		Changed_Date = GetDate()
	WHERE	Taxon_Occurrence_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrence_Update_ForDragDrop') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonOccurrence_Update_ForDragDrop'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ForDragDrop TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ForDragDrop TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ForDragDrop TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ForDragDrop TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Location_Update_ForDragDrop]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Location_Update_ForDragDrop]
GO

/*===========================================================================*\
  Description:	Update a location's parent key after a Drag/Drop or Cut/Paste
		operation.

  Parameters:	@Key		Location to update.
		@ParentKey	New parent location.
		@UserID		ID of user making the change.

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/02/04 10:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Location_Update_ForDragDrop]
	@Key char(16),
	@ParentKey char(16),
	@UserID char(16)
AS
	UPDATE	Location
	SET	Parent_Key = @ParentKey,
		Changed_By = @UserID,
		Changed_Date = GetDate()
	WHERE	Location_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Location_Update_ForDragDrop') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Location_Update_ForDragDrop'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Location_Update_ForDragDrop TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Location_Update_ForDragDrop TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Location_Update_ForDragDrop TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Location_Update_ForDragDrop TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocationFeature_Update_ForDragDrop]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocationFeature_Update_ForDragDrop]
GO

/*===========================================================================*\
  Description:	Update a features's parent key after a Drag/Drop or Cut/Paste
		operation.

  Parameters:	@Key		Feature to update.
		@ParentKey	New parent location.
		@UserID		ID of user making the change.

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/02/04 10:06 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationFeature_Update_ForDragDrop]
	@Key char(16),
	@ParentKey char(16),
	@UserID char(16)
AS
	UPDATE	Location_Feature
	SET	Location_Key = @ParentKey,
		Changed_By = @UserID,
		Changed_Date = GetDate()
	WHERE	Location_Feature_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeature_Update_ForDragDrop') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocationFeature_Update_ForDragDrop'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocationFeature_Update_ForDragDrop TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFeature_Update_ForDragDrop TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFeature_Update_ForDragDrop TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationFeature_Update_ForDragDrop TO [Dev - JNCC SQL]
END
GO
