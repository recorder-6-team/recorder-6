/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_GetDate]')
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_ImportWizard_GetDate]
GO

/*===========================================================================*\
  Description:	Obtain the date components from an imported record.

  Parameters:	@record_no				Identifies the import row.

  Created:		June 2004

  Last revision information:
	$Revision: 1 $
	$Date: 8/06/04 17:37 $
	$Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetDate]
	@record_no			INT
AS
	SELECT		SYSTEM0100000002_Vague_Date_Start	AS	Start,
				SYSTEM0100000002_Vague_Date_End		AS	"End",
				SYSTEM0100000002_Vague_Date_Type	AS	Type
	FROM		#master
	WHERE		Record_No							=	@record_no

	IF @@ROWCOUNT = 0 GOTO NoSuchRecord
	RETURN

NoSuchRecord:
	RAISERROR ('Unknown import record', 16, 1)
	RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetDate') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_ImportWizard_GetDate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ImportWizard_GetDate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ImportWizard_GetDate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
			GRANT EXECUTE ON dbo.usp_ImportWizard_GetDate TO [Dev - JNCC SQL]
END