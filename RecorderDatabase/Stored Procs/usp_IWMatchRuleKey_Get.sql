/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchRuleKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchRuleKey_Get]
GO

/*===========================================================================*\
  Description:	returns the match rule key for a supplied column type key
		 and field index

  Parameters:	@Key - IW_ColumnTypeKey_Key
		@Index - index of field produced by parser

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 11/06/04 14:48 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRuleKey_Get]
	@Key CHAR(16),
	@Index INT,
	@MatchRuleKey CHAR(16) OUTPUT
AS

SELECT @MatchRuleKey = IW_Match_Rule_Key
FROM IW_Column_Type_Match_Rule
WHERE IW_Column_Type_Key=@Key
AND Field_Index = @Index

IF @MatchRuleKey IS NULL
	SET @MatchRuleKey=''
	

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRuleKey_Get') AND SysStat & 0xf = 4)
BEGIN
 	PRINT 'Setting up security on procedure usp_IWMatchRuleKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRuleKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRuleKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRuleKey_Get TO [Dev - JNCC SQL]
END

GO