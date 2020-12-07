/****** Object:  StoredProcedure [dbo].[usp_Check_Index]    Script Date: 10/06/2020 17:58:17 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Returns a value if all table are populated or
  if IndexChk in Setting table is missing or set to anything other than
  'YES'  
  Will ignore index taxon synonym if  sort Method is set to Organism

   Created:	November 2018
   Modified     October 2020 

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_Check_Index]
AS

SET NOCOUNT ON
  SELECT 'Record' As Record
  WHERE EXISTS (
  SELECT * FROM INDEX_TAXON_NAME) AND 
  EXISTS (
  SELECT * FROM INDEX_TAXON_GROUP) AND 
  (EXISTS (
  SELECT * FROM INDEX_TAXON_SYNONYM) 
  OR EXISTS (SELECT * FROM SETTING WHERE 
  [NAME] = 'SORTMETHOD' AND [DATA] = 'ORGANISM'))   
  AND 
  EXISTS (
  SELECT * FROM INDEX_TAXON_DESIGNATION)
  OR EXISTS (SELECT * FROM SETTING WHERE 
  [NAME]  = 'IndexChk' AND [DATA] <> 'YES')
SET NOCOUNT OFF








