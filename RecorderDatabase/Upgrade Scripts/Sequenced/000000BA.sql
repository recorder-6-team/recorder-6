/***** -  Fix to a problem with importing a second determiner where the determiner name is left blank. ******/
/***** -  Also requires a change to R6 Core Code ******/

/****** Object:  StoredProcedure [dbo].[usp_ImportWizard_Unwanted_Review]    Script Date: 02/27/2018 21:40:03 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:  Removes entries in Taxon_Determination where there is no reviewer       
  Parameters:   None

  Created:      Feb 2018

  Last revision information:
       $Author: Mikeweideli$

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ImportWizard_Unwanted_Review]
AS
  DELETE FROM #Taxon_Determination where DETERMINER IS NULL 
 
  Update #Taxon_Determination  set Preferred = 1 where  
  PREFERRED = 0 and NOT EXISTS(Select * From #TAXON_DETERMINATION TDET
  WHERE TDET.TAXON_OCCURRENCE_KEY = #Taxon_Determination.TAXON_OCCURRENCE_KEY 
  AND TDET.PREFERRED = 1)
  
  GO
  
  GRANT EXECUTE ON [dbo].[usp_ImportWizard_Unwanted_Review] TO PUBLIC

GO
DELETE FROM IW_POST_PROCESSING_PROCEDURE WHERE IW_POST_PROCESSING_PROCEDURE_KEY = 'LCA00023000000R9'

GO

INSERT INTO IW_POST_PROCESSING_PROCEDURE (IW_POST_PROCESSING_PROCEDURE_KEY,SEQUENCE,REQUIRED_TABLE_NAME,PROCEDURE_NAME,
ENTERED_BY,ENTRY_DATE,SYSTEM_SUPPLIED_DATA) 
VALUES ('LCA00023000000R9',9,'TAXON_DETERMINATION','usp_ImportWizard_Unwanted_Review','TESTDATA00000001',getdate(),1)
