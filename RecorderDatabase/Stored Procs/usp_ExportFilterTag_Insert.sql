/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ExportFilterTag_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ExportFilterTag_Insert]
GO

/*===========================================================================*\
  Description:	Create a link between a concept and export filter.

  Parameters:
	@ExportFilterKey
	@ConceptKey

  Created:	January 2008

  Last revision information:
    $Revision: 1 $
    $Date: 12/02/08 14:52 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ExportFilterTag_Insert]
	@ExportFilterKey 	CHAR(16),
	@ConceptKey 		CHAR(16)
AS
	SET NOCOUNT OFF

	INSERT INTO Export_Filter_Tag (Export_Filter_Key, Concept_Key)
	VALUES (@ExportFilterKey, @ConceptKey)

	IF @@Error <> 0
		RAISERROR ('usp_ExportFilterTag_Insert failed', 16, 1)
	
	RETURN 0
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ExportFilterTag_Insert') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ExportFilterTag_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ExportFilterTag_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ExportFilterTag_Insert TO [Dev - JNCC SQL]
END
GO
