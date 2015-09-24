-- Tidy permissions to ensure Batch updates can't change data model

IF NOT EXISTS (SELECT *
		FROM	SysObjects 
		WHERE	Id = Object_Id(N'[dbo].[Collection_Unit]')
		AND		Type = 'U')
BEGIN
	sp_droprolemember 'db_Owner', 'NBNUser'
END
GO

sp_droprolemember 'db_ddladmin', 'NBNUser'
GO

sp_droprolemember 'db_ddladmin', 'R2k_FullEdit'
GO

