/*===========================================================================*\
  Description:	
			Adds a new index table, Index_Taxon_Designation, which can hold
			a list of keys for associated Taxon_List_Items and Taxon_Designations
			for one or more Taxon_Lists.

  Created:	January 2009

  Last revision information:
    $Revision: 2 $
    $Date: 10/02/09 10:19 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (	SELECT *
				FROM   SysObjects 
				WHERE  Id = Object_Id(N'dbo.Index_Taxon_Designation') 
				AND    ObjectProperty(Id, N'IsTable') = 1)
BEGIN
	CREATE TABLE Index_Taxon_Designation (
		Taxon_List_Item_Key CHAR(16)
			CONSTRAINT	FK_Index_Taxon_Designation_Taxon_List_Item 
			FOREIGN KEY	(Taxon_List_Item_Key)
			REFERENCES	Taxon_List_Item (Taxon_List_Item_Key),
		Taxon_Designation_Type_Key CHAR(16)
			CONSTRAINT	FK_Index_Taxon_Designation_Taxon_Designation_Type_Key
			FOREIGN KEY	(Taxon_Designation_Type_Key)
			REFERENCES	Taxon_Designation_Type (Taxon_Designation_Type_Key),
		CONSTRAINT PK_Index_Taxon_Designation PRIMARY KEY NONCLUSTERED 
		(
			Taxon_List_Item_Key ASC,
			Taxon_Designation_Type_Key ASC
		)
	)
	
	-- Creates indices for the table
	CREATE INDEX IX_Index_Taxon_Designation_Taxon_List_Item_Key
	ON Index_Taxon_Designation (Taxon_List_Item_Key)

	CREATE INDEX IX_Index_Taxon_Designation_Taxon_Designation_Key
	ON Index_Taxon_Designation (Taxon_Designation_Type_Key)

	-- Fills in the table
	EXECUTE		dbo.usp_Index_Taxon_Designation_Rebuild
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (	SELECT *
			FROM   SysObjects 
			WHERE  Id = Object_Id(N'dbo.Index_Taxon_Designation') 
			AND    ObjectProperty(Id, N'IsTable') = 1)
BEGIN
	PRINT 'Setting up security on procedure dbo.Index_Taxon_Designation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT SELECT ON dbo.Index_Taxon_Designation TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT SELECT ON dbo.Index_Taxon_Designation TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT SELECT ON dbo.Index_Taxon_Designation TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT SELECT ON dbo.Index_Taxon_Designation TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT SELECT ON dbo.Index_Taxon_Designation TO R2k_RecordCardsOnly
END
GO

