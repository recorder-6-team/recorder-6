/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_Index_Taxon_Designation_Rebuild') 
	   AND    ObjectProperty(Id, N'IsProcedure') = 1)
    DROP PROCEDURE dbo.usp_Index_Taxon_Designation_Rebuild
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	
			Procedure which rebuilds the indexed list of Taxon Designations for
			the supplied list of Taxon_List_Keys. 

  Created:	January 2009

  Last revision information:
    $Revision: 6 $
    $Date: 28/04/09 10:43 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Index_Taxon_Designation_Rebuild
AS
	DECLARE	@KeyList	VARCHAR(250)
	
	SET		@KeyList	=	''
	
	SELECT	@KeyList	=	Data
	FROM	Setting
	WHERE	Name		=	'TaxDesList'

	DELETE FROM	Index_Taxon_Designation

	INSERT INTO	Index_Taxon_Designation	(
				Taxon_List_Item_Key,
				Taxon_Designation_Type_Key	)
	SELECT		ITN.Taxon_List_Item_Key,
				TDES.Taxon_Designation_Type_Key
	FROM		Index_Taxon_Name						ITN
	INNER JOIN	Index_Taxon_Name						ITN2
			ON	ITN.Recommended_Taxon_List_Item_Key		=	ITN2.Recommended_Taxon_List_Item_Key
	INNER JOIN	Index_Taxon_Group						ITG
			ON	ITG.Contained_List_Item_Key				=	ITN2.Taxon_List_Item_Key
	INNER JOIN	Index_Taxon_Name						ITN3
			ON	ITN3.Taxon_List_Item_Key				=	ITG.Taxon_List_Item_Key
	INNER JOIN	Index_Taxon_Name						ITN4
			ON	ITN4.Recommended_Taxon_List_Item_Key	=	ITN3.Recommended_Taxon_List_Item_Key
	INNER JOIN	Taxon_Designation						TDES
			ON	TDES.Taxon_List_Item_Key				=	ITN4.Taxon_List_Item_Key
	INNER JOIN	Taxon_List_Version						TLV
			ON	TLV.Taxon_List_Version_Key				=	ITN4.Taxon_List_Version_Key

	WHERE		(@Keylist	LIKE '%' + TLV.Taxon_List_Key + '%'
					OR	@Keylist	=	''
					OR	TDES.System_Supplied_Data	=	0)
			AND	TDES.Date_To IS NULL
	GROUP BY	ITN.TAXON_LIST_ITEM_KEY,
				TDES.TAXON_DESIGNATION_TYPE_KEY
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Index_Taxon_Designation_Rebuild') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Index_Taxon_Designation_Rebuild'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Index_Taxon_Designation_Rebuild TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Index_Taxon_Designation_Rebuild TO [Dev - JNCC SQL]
END
GO