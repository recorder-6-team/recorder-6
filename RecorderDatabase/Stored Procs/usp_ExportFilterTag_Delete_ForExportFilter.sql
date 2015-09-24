/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ExportFilterTag_Delete_ForExportFilter') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_ExportFilterTag_Delete_ForExportFilter]
GO

/*===========================================================================*\
  Description:	Remove a given tag from all its associated surveys.

  Parameters:
	@ConceptKey

  Created:	February 2008

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/08 14:52 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ExportFilterTag_Delete_ForExportFilter]
	@Key CHAR(16)
AS
	SET NOCOUNT OFF

	DELETE FROM Export_Filter_Tag
	WHERE  Export_Filter_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ExportFilterTag_Delete_ForExportFilter') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ExportFilterTag_Delete_ForExportFilter'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Delete_ForExportFilter TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Delete_ForExportFilter TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Delete_ForExportFilter TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ExportFilterTag_Delete_ForExportFilter TO [Dev - JNCC SQL]
END
GO
