/*===========================================================================*\
  Description:
	Table for linking between the Sample and Admin_Areas tables.

  Created:	March 2009

  Last revision information:
    $Revision: 1 $
    $Date: 3/03/09 11:04 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (SELECT 1 FROM SysObjects WHERE Id = OBJECT_ID('dbo.Sample_Admin_Areas') AND ObjectProperty(Id, N'IsUserTable') = 1)
	CREATE TABLE dbo.Sample_Admin_Areas (
		Sample_Admin_Areas_Key	CHAR(16)		NOT NULL
			CONSTRAINT	PK_Sample_Admin_Areas 
			PRIMARY KEY NONCLUSTERED ,
		Admin_Area_Key			CHAR(16)		NOT NULL
			CONSTRAINT	FK_Sample_Admin_Areas_Admin_Area 
			FOREIGN KEY	(Admin_Area_Key) 
			REFERENCES	Admin_Area	(Admin_Area_Key),
		Sample_Key				CHAR(16)		NOT NULL
			CONSTRAINT	FK_Sample_Admin_Areas_Sample
			FOREIGN KEY	(Sample_Key)
			REFERENCES	Sample		(Sample_Key),
		Entered_By				CHAR(16)		NOT NULL,
		Entry_Date				SMALLDATETIME	NOT NULL
			CONSTRAINT	DF_SampleAdminAreas_EntryDate
			DEFAULT		(getdate()),
		System_Supplied_Data	BIT				NOT NULL
			CONSTRAINT	DF_SampleAdminAreas_SystemSuppliedData
			DEFAULT		(0),
		Custodian				CHAR(8)			NULL
	)

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (	SELECT 1
			FROM   SysObjects 
			WHERE  Id = Object_Id(N'dbo.Sample_Admin_Areas') 
			AND    ObjectProperty(Id, N'IsTable') = 1)
BEGIN
	PRINT 'Setting up security on procedure dbo.Sample_Admin_Areas'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT SELECT, INSERT ON dbo.Sample_Admin_Areas TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT SELECT, UPDATE , INSERT , DELETE ON dbo.Sample_Admin_Areas TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT SELECT, UPDATE , INSERT , DELETE ON dbo.Sample_Admin_Areas TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT SELECT ON dbo.Sample_Admin_Areas TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT SELECT ON dbo.Sample_Admin_Areas TO R2k_RecordCardsOnly
END
GO