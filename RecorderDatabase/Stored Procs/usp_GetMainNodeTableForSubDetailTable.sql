/*==	=========================================================================*\
Drop stored proc before re-creating.
\*===========================================================================*/
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_GetMainNodeTableForSubDetailTable]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_GetMainNodeTableForSubDetailTable]
GO
    
/*===========================================================================*\ 
Description: 
    When displaying a filter table, if the table is not one of the main hierarchy 
	nodes, find out the best match hierarchy node for it. For example, a 
	taxon determination actually links to the taxon occurrence node type.

Parameters: 
    @Key Collection unit key 
    @Name OUTPUT 

Created:    January 2008
Author:     David Kelly

Last revision information: 
    $Revision: 4 $
    $Date: 23/01/08 12:33 $
    $Author: Johnvanbreda $
\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_GetMainNodeTableForSubDetailTable]
  @DetailTable  VARCHAR(100)
AS

	SELECT TOP 1 Master_Table, Master_Field, Detail_Table, Detail_Field
	FROM Database_Relationship 
	WHERE 
	Master_Table IN ('name', 'individual', 'organisation','survey','survey_event','sample','taxon_occurrence',
	'biotope_occurrence','location','location_feature','source','reference','taxon_list_item','biotope_list_item','admin_area')
	AND Detail_Table = @DetailTable
	ORDER BY 
		-- make joins to tables that have a partial match on the name higher priority
		CASE WHEN LEFT(Detail_Table, LEN(Master_Table))=Master_Table THEN 0 ELSE 1 END, 		
		-- make joins to the NAME table lower priority
		CASE Master_Table WHEN 'name' THEN 1 ELSE 0 END,
		-- if the Export tool follows down this relationship, then also prioritise
		Follow_Down DESC


GO

/*===========================================================================*\
Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.[usp_GetMainNodeTableForSubDetailTable]') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_GetMainNodeTableForSubDetailTable]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_GetMainNodeTableForSubDetailTable TO [Dev - JNCC SQL]
END
GO