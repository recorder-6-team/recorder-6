If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Name_IsIndividual_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Name_IsIndividual_Get]
GO 

CREATE PROCEDURE [dbo].[usp_Name_IsIndividual_Get] 
@NameKey CHAR(16),
@IsIndividual BIT OUTPUT

AS

--  DESCRIPTION
--  Returns whether or not a Name_Key is an Individual Name_Key
--
--	PARAMETERS
--	NAME					DESCRIPTION
--	@NameKey				Name_Key
--	@IsIndividual		Output
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			16/02/2004
--

IF EXISTS(SELECT * FROM Individual WHERE Name_Key = @NameKey)
	SET @IsIndividual = 1
ELSE
	SET @IsIndividual = 0

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Name_IsIndividual_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Name_IsIndividual_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Name_IsIndividual_Get TO [Dev - JNCC SQL]
END

GO