/*===========================================================================*\
  Description:	
			Adds a new link table, Taxon_Designation_Set_Item, which links the
			Taxon_Designation_Set to the Taxon_Designation_Types it holds.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 9/02/09 14:16 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (	SELECT *
				FROM   SysObjects 
				WHERE  Id = Object_Id(N'dbo.Taxon_Designation_Set_Item') 
				AND    ObjectProperty(Id, N'IsTable') = 1)
	CREATE TABLE Taxon_Designation_Set_Item (
		Taxon_Designation_Set_Item_Key CHAR(16)
		CONSTRAINT PK_Taxon_Designation_Set_Item PRIMARY KEY CLUSTERED,
		Taxon_Designation_Set_Key CHAR(16)
			CONSTRAINT	FK_Taxon_Designation_Set_Item_Taxon_Designation_Set
			FOREIGN KEY	(Taxon_Designation_Set_Key)
			REFERENCES	Taxon_Designation_Set	(Taxon_Designation_Set_Key),
		Taxon_Designation_Type_Key CHAR(16)
			CONSTRAINT	FK_Taxon_Designation_Set_Item_Taxon_Designation_Type
			FOREIGN KEY	(Taxon_Designation_Type_Key)
			REFERENCES	Taxon_Designation_Type	(Taxon_Designation_Type_Key),
		Entered_By CHAR(16) NOT NULL,
		Entry_Date SMALLDATETIME NOT NULL,
		Changed_By CHAR(16) NULL,
		Changed_Date SMALLDATETIME NULL,
		System_Supplied_Data BIT NOT NULL,
		Custodian CHAR(8) NOT NULL
	)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (	SELECT *
			FROM   SysObjects 
			WHERE  Id = Object_Id(N'dbo.Taxon_Designation_Set_Item') 
			AND    ObjectProperty(Id, N'IsTable') = 1)
BEGIN
	PRINT 'Setting up security on procedure dbo.Taxon_Designation_Set_Item'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set_Item TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT SELECT ON dbo.Taxon_Designation_Set_Item TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT SELECT ON dbo.Taxon_Designation_Set_Item TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set_Item TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set_Item TO R2k_RecordCardsOnly
END
GO