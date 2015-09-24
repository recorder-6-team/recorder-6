SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_ReserveKeys') IS NOT NULL
	DROP PROCEDURE dbo.usp_ReserveKeys
GO

/*============================================================================*\
Description:	Reserves a number of key values in the specified table.

Parameters:		@table_name				The table for which to reserve the keys.
				@number_of_keys			The number of keys to reserve.
				@last_key				[output] The key value immediately
										before the first reserved key.

Created:		March 2009

Last revision information:
	$Revision: 2 $
	$Date: 16/03/09 14:51 $
	$Author: Andrewkemp $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_ReserveKeys
	@table_name							VARCHAR(30),
	@number_of_keys						INT,
	@last_key							CHAR(8) OUTPUT
AS
	DECLARE		@next_key				VARCHAR(16)	

	UPDATE		LAST_KEY
	SET			@last_key				=	LAST_KEY_TEXT,
				LAST_KEY_TEXT			=	RIGHT(
													'0000000'
													+ dbo.ufn_Base36Sum(
															LAST_KEY_TEXT,
															@number_of_keys),
													8)
	WHERE		TABLE_NAME				=	@table_name
	AND			dbo.ufn_Base36Sum(
						LAST_KEY_TEXT,
						@number_of_keys)	<>	'#ERROR!'

	IF @@ROWCOUNT = 0
	BEGIN
		SET			@last_key				=	'00000000'

		INSERT INTO	LAST_KEY (
					TABLE_NAME,
					LAST_KEY_TEXT)
		VALUES		(@table_name,
					dbo.ufn_Base36Sum(@last_key, @number_of_keys))
	END
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'Dev- JNCC SQL')
	GRANT EXECUTE ON dbo.usp_ReserveKeys TO "Dev- JNCC SQL"

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_AddOnly')
	GRANT EXECUTE ON dbo.usp_ReserveKeys TO R2k_AddOnly

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_ReserveKeys TO R2k_Administrator

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_ReserveKeys TO R2k_FullEdit

IF EXISTS (SELECT 1 FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_ReserveKeys TO R2k_RecordCardsOnly
GO