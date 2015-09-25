IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_BiotopeName_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_BiotopeName_Get]
GO

/*===========================================================================*\
  Description:	Returns the Short_Term field of a Biotope record.

  Parameters:	@Key		Key of record to be returned
		@Caption	Output caption

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 17/06/04 11:59 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_BiotopeName_Get] 
	@Key CHAR(16),
	@Caption VARCHAR(100) OUTPUT
AS
SET NOCOUNT ON

	SELECT 		@Caption = IsNull(Original_Code + ', ', '') + Short_Term
	FROM		Biotope AS B
	INNER JOIN	Biotope_List_item AS BLI ON BLI.Biotope_key = B.biotope_key 
	WHERE		BLI.Biotope_List_item_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BiotopeName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_BiotopeName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_BiotopeName_Get TO [Dev - JNCC SQL]
END
GO