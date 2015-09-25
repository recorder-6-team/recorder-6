/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_BiotopeFullySpecified_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_BiotopeFullySpecified_Select]
GO

/*===========================================================================*\
  Description:	Return a Biotope name with checklist name.

  Parameters:	@ItemKey
		@Output

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 18/06/04 18:17 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_BiotopeFullySpecified_Select]
	@ItemKey char(16),
	@Output varchar(300) OUTPUT
AS
	SELECT	@Output = dbo.ufn_GetFormattedBiotopeName(Biotope_List_Item_Key) + ' - ' + BC.Short_Name
	FROM	Biotope_List_Item BLI
	JOIN	Biotope_Classification_Version BCV ON BCV.BT_CL_Version_Key = BLI.BT_CL_Version_Key
	JOIN	Biotope_Classification BC ON BC.Biotope_Classification_Key = BCV.Biotope_Classification_Key
	WHERE	BLI.Biotope_List_Item_Key = @ItemKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BiotopeFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_BiotopeFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_BiotopeFullySpecified_Select TO [Dev - JNCC SQL]
END
GO