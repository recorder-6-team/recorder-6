/****** SQL FOR CHANGES IN Checking that all index tables are populated ******/
/****** Object:  StoredProcedure [dbo].[usp_Check_Index]    Script Date: 12/08/2018 14:52:46 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns a value if all table are ppoulated or
  if IndexChk in SEtting table is missing or set to anything other than
  'YES'  

   Created:	November 2018


\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Check_Index]
AS

SET NOCOUNT ON
  SELECT 'Record' As Record
  WHERE EXISTS (
  SELECT * FROM INDEX_TAXON_NAME) AND 
  EXISTS (
  SELECT * FROM INDEX_TAXON_GROUP) AND 
  EXISTS (
  SELECT * FROM INDEX_TAXON_SYNONYM) AND 
  EXISTS (
  SELECT * FROM INDEX_TAXON_DESIGNATION)
  OR EXISTS (SELECT * FROM SETTING WHERE 
  [NAME]  = 'IndexChk' AND [DATA] <> 'YES')
SET NOCOUNT OFF

GO

GRANT EXECUTE ON [dbo].[usp_Check_Index] TO PUBLIC