SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
VI 21691 -- make the Import Wizard reject record types longer than 40
characters (the maximum length of the SHORT_NAME field) instead of allowing
names up to 100 characters (the maximum length of the LONG_NAME field).
\*============================================================================*/
UPDATE		dbo.IW_Column_Type
SET			Maximum_Length				=	40
WHERE		IW_Column_Type_Key			=	'SYSTEM010000000I'
GO

/*============================================================================*\
VI 21701, problems 3 and 4 -- similarly for determination types and determiner
roles.
\*============================================================================*/
UPDATE		dbo.IW_Column_Type
SET			Maximum_Length				=	21
WHERE		IW_Column_Type_Key			=	'SYSTEM010000000W' -- determination type


UPDATE		dbo.IW_Column_Type
SET			Maximum_Length				=	20
WHERE		IW_Column_Type_Key			=	'SYSTEM010000000V' -- determiner role
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_DeterminationType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_DeterminationType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	January 2009

  Last revision information:
    $Revision: 2 $
    $Date: 4/06/10 16:11 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_DeterminationType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Determination_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Determination_Type(
			Determination_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, 
			CASE WHEN LEN(@ImportValue)>21 THEN LEFT(@ImportValue, 18) + '...' ELSE @ImportValue END, 
			@ImportValue, 
			@EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#DeterminationTypes
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_DeterminationType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_DeterminationType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_DeterminationType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_DeterminationType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_DeterminationType TO [Dev - JNCC SQL]
END
GO