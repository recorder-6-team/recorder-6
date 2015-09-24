/*===========================================================================*\
  Description:	
			Adds a new table, Taxon_Designation_Set, which holds a list
			of sets of designations.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 9/02/09 14:16 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (	SELECT *
				FROM   SysObjects 
				WHERE  Id = Object_Id(N'dbo.Taxon_Designation_Set') 
				AND    ObjectProperty(Id, N'IsTable') = 1)
	CREATE TABLE Taxon_Designation_Set (
		Taxon_Designation_Set_Key CHAR(16) 
		CONSTRAINT PK_Taxon_Designation_Set PRIMARY KEY CLUSTERED,
		Title CHAR(100) NOT NULL,
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
			WHERE  Id = Object_Id(N'dbo.Taxon_Designation_Set') 
			AND    ObjectProperty(Id, N'IsTable') = 1)
BEGIN
	PRINT 'Setting up security on procedure dbo.Taxon_Designation_Set'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT SELECT ON dbo.Taxon_Designation_Set TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT SELECT ON dbo.Taxon_Designation_Set TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT SELECT ON dbo.Taxon_Designation_Set TO R2k_RecordCardsOnly
END
GO