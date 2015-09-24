/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedBiotopeName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetFormattedBiotopeName
GO

/*===========================================================================*\
  Description:	Returns the formatted biotope name for the given biotope list item.

  Parameters:	@ListItemKey

  Created:	

  Last revision information:
    $Revision: 1 $
    $Date: 17/06/04 11:12 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetFormattedBiotopeName]
	(@ListItemKey char(16))
RETURNS varchar(200)
AS
BEGIN
	DECLARE	@FormattedName varchar(200)

	SELECT		@FormattedName = IsNull(Original_Code + ', ', '') + Short_Term
	FROM 		Biotope B 
	INNER JOIN 	Biotope_List_Item BLI ON B.Biotope_Key = BLI.Biotope_Key 
	WHERE 		BLI.Biotope_List_Item_Key = @ListItemKey

	RETURN @FormattedName
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetFormattedBiotopeName]')
	   AND    Type IN ('FN', 'IF', 'TF'))
BEGIN
   	PRINT 'Setting up security on function ufn_GetFormattedBiotopeName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.ufn_GetFormattedBiotopeName TO [Dev - JNCC SQL]
END
GO

