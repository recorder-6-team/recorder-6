
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[nbn_exchange_create_tables]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[nbn_exchange_create_tables]
GO

/*===========================================================================*\
  Author  Stuart Ball, JNCC
  Date    16 May 2008

  Description	
     Create the tables needed to support export to NBN exchange format.

  Parameters
     None.
	
  Revision information

\*===========================================================================*/
CREATE PROCEDURE [dbo].[nbn_exchange_create_tables]
AS

-- Table in which to assemble the rows to export
CREATE TABLE [##nbn_exchange_obs]
(
	[RecordKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[SurveyKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SampleKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[StartDate] [varchar](12) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[EndDate] [varchar](12) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[DateType] [char](2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[TaxonVersionKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[ZeroAbundance] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Sensitive] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SiteKey] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SiteName] [varchar](255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Projection] [varchar](10) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[GridReference] [varchar](40) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Precision] [int] NULL,
	[East] [float] NULL,
	[North] [float] NULL,
	[Recorder] [varchar](4096) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Determiner] [varchar](4096) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[SampleMethod] [varchar](50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Comment] [varchar](1024) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Substrate] [varchar](20) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)
-- make RecordKey the primary key
ALTER TABLE [##nbn_exchange_obs] 
ADD CONSTRAINT pk_RecordKey PRIMARY KEY (RecordKey)

-- table containing the key(s) selected by the user in Rec6
CREATE TABLE [##nbn_exchange_export]
(
	[table_name] [varchar](30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[nbn_key] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) 

-- results of the validation check before the export
-- rows identified here should be excluded from the export
CREATE TABLE [##nbn_exchange_invalid]
(
	[table_name] [varchar](30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[nbn_key] [varchar](16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
) 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.nbn_exchange_create_tables') AND SysStat & 0xf = 4)
BEGIN
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.nbn_exchange_create_tables TO [R2k_RecordCardsOnly]
END
GO