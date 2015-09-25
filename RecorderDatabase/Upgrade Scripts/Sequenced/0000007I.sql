/*===========================================================================*\
  Grant additional permissions on INDEX_TAXON_DESIGNATION to allow a
  administrator or full_edit to delete items in the table. This is needed
  if a user has created their own designation type but later wants to
  delete that designation type.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.INDEX_TAXON_DESIGNATION'))
BEGIN
   	PRINT 'Adjust security on INDEX_TAXON_DESIGNATION to allow batch updates to delete rows from table'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT DELETE ON dbo.INDEX_TAXON_DESIGNATION TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT DELETE ON dbo.INDEX_TAXON_DESIGNATION TO [R2k_FullEdit]
END
GO
