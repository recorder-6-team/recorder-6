/*===========================================================================*\
  Description:  Removes entries with duplicate Matched_Value in the 
                IW_Matched_Species with null Matched_Key

  Parameters:   <none>

  Created:      March 2013

  Last revision information:
    $Revision: 2 $
    $Date: 05/03/13 15:45 $
    $Author: Michaelcaptain $

\*===========================================================================*/

IF COL_LENGTH('IW_Match_Rule','Exclude_Unmatched_Procedure') IS NULL
BEGIN
	ALTER TABLE IW_Match_Rule ADD Exclude_Unmatched_Procedure VARCHAR(50);
END
GO

UPDATE IW_Match_Rule
SET Exclude_Unmatched_Procedure = 'usp_IWExcludeUnmatched_Species'
WHERE Item_Name = 'Species'

GO

/*===========================================================================*\
  Description:  Removes entries with duplicate Matched_Value in the 
                IW_Matched_Species with null Matched_Key

  Parameters:   <none>

  Created:      March 2013

  Last revision information:
    $Revision: 2 $
    $Date: 21/03/13 16:26 $
    $Author: Michaelcaptain $

\*===========================================================================*/

IF COL_LENGTH('IW_Matched_Species','IW_Matched_Species_Id') IS NULL
BEGIN
	ALTER TABLE IW_Matched_Species 
	ADD IW_Matched_Species_Id INT IDENTITY(1,1);
END
GO

IF NOT EXISTS (SELECT * 
               FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS WHERE  CONSTRAINT_TYPE = 'PRIMARY KEY'
               AND TABLE_NAME = 'IW_Matched_Species' 
               AND TABLE_SCHEMA ='dbo')
BEGIN
	ALTER TABLE IW_Matched_Species 
	ADD CONSTRAINT PK_IW_Matched_Species PRIMARY KEY CLUSTERED (IW_Matched_Species_Id)

	WHILE EXISTS (SELECT COUNT(*) FROM IW_Matched_Species 
				GROUP BY Matched_Value, Match_Checklist_Key
				HAVING COUNT(*) > 1 
				AND Match_Checklist_Key IS NULL)
	BEGIN
		DELETE FROM IW_Matched_Species
		WHERE IW_Matched_Species_Id IN (SELECT MIN(IW_Matched_Species_Id)
									FROM IW_Matched_Species
									GROUP BY Matched_Value, Match_Checklist_Key
									HAVING COUNT(IW_Matched_Species_Id) > 1                          
									AND Match_Checklist_Key IS NULL)
	END
END
GO

/*===========================================================================*\
  Description:  Adds rows to the Setting table for temporary data imports
                and rows to tables for the import wizard to enable 
                temporary observers and determination dates.

  Parameters:   <none>

  Created:      March 2013

  Last revision information:
    $Revision: 1 $
    $Date:  $21/03/13 15:08$
    $Author: Michaelcaptain $

\*===========================================================================*/

-- General settings
SET DATEFORMAT ymd

SELECT "Name"
               FROM Setting
               WHERE (("Name" = 'TempMedia') OR ("Name" = 'TempName'))
IF @@ROWCOUNT <> 2
BEGIN
	INSERT INTO Setting
	("NAME", DATA)
	SELECT 'TempMedia', '' UNION ALL
	SELECT 'TempName', ''
END


DELETE FROM IW_Table_Rule_Output_Field
WHERE (Entry_Date = '2013-03-19')

DELETE FROM IW_Output_Field
WHERE ((IW_Output_Field_Key = 'SYSTEM0100000025') OR
       (IW_Output_Field_Key = 'SYSTEM0100000026') OR
       (IW_Output_Field_Key = 'SYSTEM0100000027') OR
       (IW_Output_Field_Key = 'SYSTEM0100000028'))

DELETE FROM IW_Column_Type_Pattern
WHERE IW_Column_Type_Key = 'SYSTEM010000000Z'

DELETE FROM IW_Column_Type_Relationship
WHERE IW_Column_Type_Key = 'SYSTEM010000000Z' AND Related_IW_Column_Type_Key = 'SYSTEM0100000002'

DELETE FROM IW_Column_Type
WHERE IW_Column_Type_Key = 'SYSTEM010000000Y' OR IW_Column_Type_Key = 'SYSTEM010000000Z'

INSERT INTO IW_Column_Type
(IW_Column_Type_Key,
Class_Name,
Item_Name,
"Required",
Commonly_Used,
Sequence,
Field_Type,
Parser_Class_Name,
Maximum_Length,
Term_List_Table,
Entered_By,
Entry_Date,
Changed_By,
Changed_Date,
System_Supplied_Data)
SELECT
'SYSTEM010000000Y',
'TColumnType',
'TempObserver(s)',
1,
1,
NULL,
NULL,
'TTextParser',
250,
NULL,
'NBNSYS0000000004',
'2013-03-08',
NULL,
NULL,
1 UNION ALL
SELECT
'SYSTEM010000000Z',
'TColumnType',
'Determination Date',
0,
0,
NULL,
NULL,
'TDeterminationDateParser',
NULL,
NULL,
'NBNSYS0000000004',
'2013-03-08',
NULL,
NULL,
1

INSERT INTO IW_Column_Type_Relationship
(IW_Column_Type_Key,
Related_IW_Column_Type_Key,
Relationship_Type,
Entered_By,
Entry_Date,
Changed_By,
Changed_Date,
System_Supplied_Data)
VALUES
('SYSTEM010000000Z',
'SYSTEM0100000002',
2,
'NBNSYS0000000004',
'2013-03-08',
NULL,
NULL,
1)

INSERT INTO IW_Column_Type_Pattern
(IW_Column_Type_Key,
Pattern,
Exclude_Match,
Entered_By,
Entry_Date,
Changed_By,
Changed_Date,
System_Supplied_Data)
VALUES
('SYSTEM010000000Z',
'det%date',
0,
'NBNSYS0000000004',
'2013-03-08',
NULL,
NULL,
1)

INSERT INTO IW_Output_Field
(IW_Output_Field_Key,
Name,
Data_Type,
IW_Column_Type_Key,
Source_Field_Name,
Generating_Class_Name,
Generator_Field_Index,
Literal_Value,
Entered_By,
Entry_Date,
Changed_By,
Changed_Date,
System_Supplied_Data)
SELECT
'SYSTEM0100000025',
'Recorders',
'VARCHAR(255)',
'SYSTEM010000000Y',
'data',
NULL,
NULL,
NULL,
'NBNSYS0000000004',
'2013-03-18',
NULL,
NULL,
1 UNION ALL
SELECT
'SYSTEM0100000026',
'Vague_Date_Start',
'INT',
NULL,
NULL,
'TDeterminationDateFieldGenerator',
0,
NULL,
'NBNSYS0000000004',
'2013-03-20',
NULL,
NULL,
1 UNION ALL
SELECT
'SYSTEM0100000027',
'Vague_Date_End',
'INT',
NULL,
NULL,
'TDeterminationDateFieldGenerator',
1,
NULL,
'NBNSYS0000000004',
'2013-03-20',
NULL,
NULL,
1 UNION ALL
SELECT
'SYSTEM0100000028',
'Vague_Date_Type',
'VARCHAR(2)',
NULL,
NULL,
'TDeterminationDateFieldGenerator',
2,
NULL,
'NBNSYS0000000004',
'2013-03-20',
NULL,
NULL,
1

UPDATE IW_Output_Field
SET 
IW_Column_Type_Key = NULL,
Source_Field_Name = NULL,
Generating_Class_Name = 'TObserverFieldGenerator'
WHERE IW_Output_Field_Key = 'SYSTEM010000000N'

DELETE FROM IW_Table_Rule_Output_Field
WHERE ((IW_Table_Rule_Key = 'SYSTEM0100000007') AND
(IW_Output_Field_Key = 'SYSTEM0100000007' OR
IW_Output_Field_Key = 'SYSTEM0100000008' OR
IW_Output_Field_Key = 'SYSTEM0100000009'))

INSERT INTO IW_Table_Rule_Output_Field
(IW_Table_Rule_Key,
IW_Output_Field_Key,
Entered_By,
Entry_Date,
System_Supplied_Data)
SELECT
'SYSTEM0100000002',
'SYSTEM0100000025',
'NBNSYS0000000004',
'2013-03-19',
1 UNION ALL
SELECT
'SYSTEM0100000007',
'SYSTEM0100000026',
'NBNSYS0000000004',
'2013-03-19',
1 UNION ALL
SELECT
'SYSTEM0100000007',
'SYSTEM0100000027',
'NBNSYS0000000004',
'2013-03-19',
1 UNION ALL
SELECT
'SYSTEM0100000007',
'SYSTEM0100000028',
'NBNSYS0000000004',
'2013-03-19',
1

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Species') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRecord_Species]
GO

/*===========================================================================*\
  Description:	Populate remembered matches table for future imports.

  Parameters:
	@UserID			The ID of the current User.

  Created:	July 2004

  Last revision information:
    $Revision: 6 $
    $Date: 05/03/13 15:50 $
    $Author: Michaelcaptain $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_Species]
	@UserID			char(16)
AS
	-- Removes existing matches which have a replacement.
	DELETE		MS1
	FROM		IW_Matched_Species		MS1
	INNER JOIN	IW_Matched_Species		MS2
			ON	MS2.Temp_User_ID		=	@UserID
			AND	(MS1.Match_Checklist_Key	=	MS2.Match_Checklist_Key
                        OR (MS1.Match_Checklist_Key IS NULL AND MS2.Match_Checklist_Key IS NULL))
			AND	MS1.Matched_Value		=	MS2.Matched_Value
	WHERE		MS1.Temp_User_ID		IS	NULL
	
	-- Makes the temporary changes this user made permenant.
	UPDATE	IW_Matched_Species
	SET		Temp_User_ID	=	NULL
	WHERE	Temp_User_ID	=	@UserID
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_Species') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRecord_Species'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRecord_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_IWExcludeUnmatched_Species]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWExcludeUnmatched_Species]
GO

/*===========================================================================*\
  Description:  Deletes all unmatched rows in the #Master and #Species
                temporary tables for a given IW_Match_Rule_Key.

  Parameters:   @RowsDeleted - Output, no. of rows deleted from #Master

  Created:  March 2013

  Last revision information:
    $Revision: 1 $
    $Date: 07/03/13 11:12 $
    $Author: Michaelcaptain $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWExcludeUnmatched_Species]
    @RowsDeleted INT OUT
AS
    DELETE m FROM #Master m
    JOIN #Species s ON s.Import_Value = m.[SYSTEM0100000003_data]
    WHERE s.Match_Key IS NULL

    SET @RowsDeleted = @@ROWCOUNT

    DELETE FROM #Species
    WHERE Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('usp_IWExcludeUnmatched_Species') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWExcludeUnmatched_Species'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWExcludeUnmatched_Species TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWExcludeUnmatched_Species TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWExcludeUnmatched_Species TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchRule_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchRule_Select]
GO

/*===========================================================================*\
  Description:	returns details of a match rule

  Parameters:	@Key - IW_Match_Rule_Key

  Created:	June 2004

  Last revision information:
    $Revision: 6 $
    $Date: 11/03/13 14:36 $
    $Author: Michaelcaptain $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRule_Select]
	@Key CHAR(16)
AS
	SELECT 	[Sequence], 
		Item_Name, 
		Control_Type,
		Imported_Data_Insert_Sql,
		Remembered_Matches_Procedure,
		Match_Procedure,
		Record_Matches_Procedure,
		New_Entry_Procedure,
		Requires_Checklist,
		Set_Match_Procedure,
		Table_Create_Sql,
		Key_To_Caption_Procedure,
		Search_Type,
		Checklists_Select_Procedure,
		Termlist_Select_Procedure,
        Exclude_Unmatched_Procedure
	FROM 	IW_Match_Rule
	WHERE 	IW_Match_Rule_Key = @Key
	ORDER BY [Sequence]
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRule_Select') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWMatchRule_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRule_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRule_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRule_Select TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id(N'[dbo].[usp_RemoveUnwantedOccurrences]') AND ObjectProperty(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_RemoveUnwantedOccurrences]
GO
    
/*
  DESCRIPTION
  This procedure removes invalid taxon and biotope occurrences
  that could have appeared after an import.  Returns the deleted 
  occurrence keys. Also updates the Verified column in the
  Taxon_Occurrence table to match the imported data.

  PARAMETERS
  None

  Last Revision Details:
    $Revision: 9 $
    $Date: 05/03/13 15:50 $
    $Author: Michaelcaptain $
    
*/

CREATE PROCEDURE [dbo].[usp_RemoveUnwantedOccurrences]
AS

SET NOCOUNT ON

   BEGIN TRAN
        -- Set Verified column in Taxon_Occurrence
        IF OBJECT_ID('tempdb..#Taxon_Occurrence') IS NOT NULL
	UPDATE taxo
	SET taxo.VERIFIED=det.Verified
	FROM Taxon_Occurrence taxo
	INNER JOIN #Taxon_Occurrence tt ON tt.TAXON_OCCURRENCE_KEY = taxo.TAXON_OCCURRENCE_KEY
	JOIN #Taxon_Determination taxd ON taxd.TAXON_OCCURRENCE_KEY = taxo.TAXON_OCCURRENCE_KEY
	JOIN DETERMINATION_TYPE det ON det.DETERMINATION_TYPE_KEY = taxd.DETERMINATION_TYPE_KEY

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

	-- Remove associated specimen data
	DELETE FROM Specimen
	WHERE Taxon_Occurrence_Key IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	IF @@Error > 0 GOTO RollBackAndExit

	-- Remove associated specimen field data
	IF OBJECT_ID(N'dbo.Specimen_Field_Data') IS NOT NULL
	BEGIN	
		DELETE FROM Specimen_Field_Data
		WHERE Taxon_Occurrence_Key IN (
			SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
		)
		IF @@Error > 0 GOTO RollBackAndExit
	END

	-- Remove taxon occurrence relations
	DELETE FROM Taxon_Occurrence_Relation
	WHERE Taxon_Occurrence_Key_1 IN (
		SELECT Taxon_Occurrence_Key FROM #DeleteTaxa
	)
	OR Taxon_Occurrence_Key_2 IN (
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
	IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_RemoveUnwantedOccurrences] TO [R2k_FullEdit]
END
GO 

IF Object_ID('dbo.usp_Survey_GetMediaKey') IS NOT NULL
	DROP PROCEDURE dbo.usp_Survey_GetMediaKey
GO

/*===========================================================================*\
  Description:
	Returns the media key for a survey.

  Parameters:
	@Key	The name of the survey to retrieve.
	@Value	The value of the media key, as an OUTPUT.

  Created:	March 2013

  Last revision information:
    $Revision: 1 $
    $Date: 11/03/13 11:00 $
    $Author: Michaelcaptain $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Survey_GetMediaKey
	@Key	VARCHAR(20),
	@Value	VARCHAR(250) OUTPUT

AS
	SET NOCOUNT ON

	SELECT	@Value = Survey_Media_Key
	FROM	Survey
	WHERE	Survey_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_Survey_GetMediaKey'
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Survey_GetMediaKey TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Survey_GetMediaKey TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Survey_GetMediaKey TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Survey_GetMediaKey TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Survey_GetMediaKey TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Survey_GetMediaKey TO "Dev - JNCC SQL"
GO

