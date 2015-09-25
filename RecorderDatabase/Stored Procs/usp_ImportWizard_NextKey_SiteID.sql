/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_NextKey_SiteID]')
	   AND 	  Type = 'P')
	DROP PROCEDURE [dbo].[usp_ImportWizard_NextKey_SiteID]
GO

/*===========================================================================*\
  Description:	Generate a key value for an imported record using the site
				identifier specified in the import data.

  Parameters:	@table_name				Name of table into which record is
										imported.
				@record_no				Identifies the import row.
				@key					[on exit] New key value.

  Created:		June 2004

  Last revision information:
	$Revision: 2 $
	$Date: 8/06/04 17:39 $
	$Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_NextKey_SiteID]
	@table_name				VARCHAR(50),
	@record_no				INT,
	@key					CHAR(16)	OUTPUT
AS
	IF OBJECT_ID('tempdb..#master') IS NULL
	BEGIN
		RAISERROR('Missing import data (#master table does not exist)', 16, 1)
		RETURN
	END

	DECLARE		@site_id	CHAR(8)

	SELECT		@site_id	=	SYSTEM010000000K_Site_ID
	FROM		#master
	WHERE		Record_No	=	@record_no

	IF @@ROWCOUNT = 0 GOTO NoSuchRecord

	EXECUTE		spNextKey	@table_name,
							@key		OUTPUT,
							@site_id
	RETURN

NoSuchRecord:
	RAISERROR ('Unknown import record', 16, 1)
	RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_NextKey_SiteID') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_ImportWizard_NextKey_SiteID'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_SiteID TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_SiteID TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
			GRANT EXECUTE ON dbo.usp_ImportWizard_NextKey_SiteID TO [Dev - JNCC SQL]
END
