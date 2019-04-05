

/****** Object:  UserDefinedFunction [dbo].[ufn_Location_Expired]    Script Date: 02/18/2019 13:10:04 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns True if the location is not expired
                or False if it has. Expired will have 
                Location Designations where all entries have an expiry date.  
  Parameters:	@LocationKey

  Created:	December 2018
  Altered follwoing 2019 testing as too slow. This version 4X quicker.  
  Last revision information:
    $Author: Mike Weideli 
    
\*===========================================================================*/
ALTER FUNCTION [dbo].[ufn_Location_Expired]
	(@LocationKey char(16))
RETURNS bit
AS
BEGIN
  DECLARE @EXPIRED Bit
  SET @EXPIRED = 1
  SELECT @EXPIRED = 0 FROM Location L LEFT JOIN 
  LOCATION_DESIGNATION LD ON LD.LOCATION_KEY
  = L.LOCATION_KEY WHERE LD.DATE_TO IS NULL AND
  L.LOCATION_KEY = @LocationKey 
          
    
  RETURN @Expired    
END

GO


/****** Object:  Table [dbo].[Import_Wizard_Log]    Script Date: 02/23/2019 17:06:18 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[Import_Wizard_Log](
	[Taxon_Occurrence_Key_1] [char](16) NOT NULL,
	[Taxon_Occurrence_Key_2] [char](16) NOT NULL,
	[Date_Time_End] [smalldatetime] NULL,
	[Occurrence_Count] [int] NULL,
	[Determination_Count] [int] NULL,
	[Entered_By] [char](16) NULL,
 CONSTRAINT [PK_Import_Wizard_Log] PRIMARY KEY CLUSTERED 
(
	[Taxon_Occurrence_Key_1] ASC,
	[Taxon_Occurrence_Key_2] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF

GO
GRANT SELECT ON [dbo].[Import_Wizard_Log] TO PUBLIC
GO
GRANT INSERT ON [dbo].[Import_Wizard_Log] TO PUBLIC
GO
GRANT UPDATE ON [dbo].[Import_Wizard_Log] TO PUBLIC
GO
GRANT DELETE ON [dbo].[Import_Wizard_Log] TO PUBLIC
GO
/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_Log_Update]    Script Date: 02/23/2019 17:09:12 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description: Logs an Import           
  Parameters:   None

  Created:      Feb 2019

  Last revision information:
       Mikeweideli$

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ImportWizard_Log_Update]
AS
   INSERT INTO Import_Wizard_Log    
   SELECT Min(Taxon_Occurrence_key),
   Max(Taxon_Occurrence_key),
   GetDate(),
   Count(Taxon_Occurrence_Key),
   (SELECT COUNT(*) FROM #TAXON_DETERMINATION),
   Max(Entered_By) 
   FROM #Taxon_Occurrence
   
GO
  
GRANT EXECUTE ON [dbo].[usp_ImportWizard_Log_Update] TO PUBLIC

GO

DELETE FROM IW_Post_Processing_Procedure WHERE
IW_Post_Processing_Procedure_Key = 'LCA00023000000RB'

GO

INSERT INTO IW_Post_Processing_Procedure (IW_POST_PROCESSING_PROCEDURE_KEY,Sequence,REQUIRED_TABLE_NAME,
Procedure_Name,Entered_By,Entry_Date,System_Supplied_Data) VALUES
('LCA00023000000RB',11,'Taxon_Occurrence',
'usp_ImportWizard_Log_Update','TESTDATA',GetDate(),1)
   
   




