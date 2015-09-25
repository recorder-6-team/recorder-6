/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFullySpecified_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_LocationFullySpecified_Select]
GO

/*===========================================================================*\
  Description:	Return a Location name with preferred name and spatial ref.

  Parameters:	@ItemKey
		@Output

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/06/04 12:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationFullySpecified_Select]
	@ItemKey char(16),
	@Output varchar(300) OUTPUT
AS
	SELECT	@Output = LN1.Item_Name + ' (' + LN2.Item_Name + ' - ' + L.Spatial_Ref + ')'
	FROM	Location L
	JOIN	Location_Name LN1 ON LN1.Location_Key = L.Location_Key
	JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
	WHERE	L.Location_Key = @ItemKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocationFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [Dev - JNCC SQL]
END
GO