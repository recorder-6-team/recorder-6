/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpeciesFullySpecified_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpeciesFullySpecified_Select]
GO

/*===========================================================================*\
  Description:	Return a species name with checklist name.

  Parameters:	
		@ItemKey
		@Output

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 10/04/08 11:34 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpeciesFullySpecified_Select]
	@ItemKey 	CHAR(16),
	@Output 	VARCHAR(300) OUTPUT
AS
	SELECT	@Output = dbo.ufn_GetFormattedSpeciesName(Taxon_List_Item_Key) 
				+ ISNULL(' - [' + ITN.Abbreviation + ']', '') 
				+ ' - ' + TL.Item_Name
	FROM	Index_Taxon_Name 	ITN
	JOIN	Taxon_List_Version 	TLV ON TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
	JOIN	Taxon_List 			TL 	ON TL.Taxon_List_Key 			= TLV.Taxon_List_Key
	WHERE	ITN.Taxon_List_Item_Key = @ItemKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpeciesFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_SpeciesFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_SpeciesFullySpecified_Select TO [Dev - JNCC SQL]
END
GO