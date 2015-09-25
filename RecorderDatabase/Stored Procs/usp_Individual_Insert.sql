IF Object_ID('dbo.usp_Individual_Insert') IS NOT NULL
	DROP PROCEDURE dbo.usp_Individual_Insert
GO

/*===========================================================================*\
  Description:
	Inserts a new individual record.

  Parameters:
	@Key			New key, output.
	@Title
	@Forename
	@Initials
	@Honorifics
	@Surname
	@Comment
	@DOBStart
	@DOBEnd
	@DOBType
	@DODStart
	@DODEnd
	@DODType
	@Floreat
	@EnteredBy
	@DepartmentKey

  Created:	July 2009

  Last revision information:
    $Revision: 1 $
    $Date: 16/07/09 11:47 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Individual_Insert
	@Key			CHAR(16)	OUTPUT,
	@Title			VARCHAR(4)	= NULL,
	@Forename		VARCHAR(20)	= NULL,
	@Initials		VARCHAR(8)	= NULL,
	@Honorifics		VARCHAR(20)	= NULL,
	@Surname		VARCHAR(30),
	@Comment		TEXT		= NULL,
	@DOBStart		INT			= NULL,
	@DOBEnd			INT			= NULL,
	@DOBType		VARCHAR(2)	= NULL,
	@DODStart		INT			= NULL,
	@DODEnd			INT			= NULL,
	@DODType		VARCHAR(2)	= NULL,
	@Floreat		VARCHAR(12)	= NULL,
	@EnteredBy		CHAR(16),
	@DepartmentKey	CHAR(16)	= NULL
AS
	SET NOCOUNT ON

	EXECUTE spNextKey 'Name', @Key OUTPUT

	-- Handles the First Run issue. The user is going to be the new one being created.
	IF ISNULL(@EnteredBy, '') = ''
		SET @EnteredBy = @Key

	INSERT INTO "Name" (
		Name_Key,
		Organisation,
		Entered_By
	) VALUES (
		@Key,
		0,
		@EnteredBy
	)

	INSERT INTO Individual (
		Name_Key,
		Title,
		Forename,
		Initials,
		Honorifics,
		Surname,
		Comment,
		Born_Vague_Date_Start,
		Born_Vague_Date_End,
		Born_Vague_Date_Type,
		Died_Vague_Date_Start,
		Died_Vague_Date_End,
		Died_Vague_Date_Type,
		Person_Floreat,
		Entered_By,
		Organisation_Department_Key
	) VALUES (
		@Key,
		@Title,
		@Forename,
		@Initials,
		@Honorifics,
		@Surname,
		@Comment,
		@DOBStart,
		@DOBEnd,
		@DOBType,
		@DODStart,
		@DODEnd,
		@DODType,
		@Floreat,
		@EnteredBy,
		@DepartmentKey
	)
		
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure '
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Individual_Insert TO R2k_AddOnly
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Individual_Insert TO R2k_Administrator
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Individual_Insert TO R2k_FullEdit
IF EXISTS (SELECT * FROM SysUsers WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Individual_Insert TO "Dev - JNCC SQL"
GO