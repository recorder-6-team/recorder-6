/*===========================================================================*\
  Description:
	Modifies the Location_Boundary table for storing boundaries in external files.

  Created:	February 2009

  Last revision information:
    $Revision: 2 $
    $Date: 3/03/09 17:09 $
    $Author: Pauldavies $

\*===========================================================================*/

ALTER TABLE dbo.Location_Boundary ADD
	External_Filename			VARCHAR(255) NULL,
	External_Filename_KeyField	VARCHAR(50) NULL

GO

ALTER TABLE dbo.Location_Boundary DROP
	CONSTRAINT	DF__Temporary__OBJEC__04E4BC85 
			--Thank you whoever made this table for following naming conventions!
			--I just hope this same arbitrary value is used in all databases...

ALTER TABLE dbo.Location_Boundary ALTER
	COLUMN	Object_ID			VARCHAR(30) NULL

ALTER TABLE dbo.Location_Boundary ADD
	CONSTRAINT	DF_LocationBoundary_ObjectID DEFAULT ('-1') FOR Object_ID
			--That's better!

GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_Polygon_Layers_Get') 
	   AND    ObjectProperty(Id, N'IsProcedure') = 1)
    DROP PROCEDURE dbo.usp_Polygon_Layers_Get
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	
			Gets a table of the available polygon layers

  Created:	February 2009

  Last revision information:
    $Revision: 2 $
    $Date: 3/03/09 17:09 $
    $Author: Pauldavies $

\*===========================================================================*/


CREATE PROCEDURE dbo.usp_Polygon_Layers_Get
AS

	SELECT		BM.Display_Name,
				MS.Sheet_Name,  
				MS.Dataset_Sheet_Filename,
				MS.Map_Sheet_Key,
				BM.Base_Map_Key
	FROM		Base_Map        BM
	INNER JOIN	Map_Sheet       MS
			ON	BM.Base_Map_Key = MS.Base_Map_Key
	WHERE		MS.Sheet_Type   = 3 -- Polygon layer
	ORDER BY	BM.Display_Name, MS.Sheet_Name

GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Polygon_Layers_Get') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Polygon_Layers_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Polygon_Layers_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Polygon_Layers_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Polygon_Layers_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_Polygon_Layers_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_Polygon_Layers_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Polygon_Layers_Get TO [Dev - JNCC SQL]
END
GO