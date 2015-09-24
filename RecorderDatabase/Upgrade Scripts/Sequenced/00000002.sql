use nbndata
if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[VW_REFERENCE_AUTHORS]') and OBJECTPROPERTY(id, N'IsView') = 1)
drop view [dbo].[VW_REFERENCE_AUTHORS]
GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO



CREATE  VIEW [dbo].[VW_REFERENCE_AUTHORS]
AS 
	-- Use DISTINCT, otherwise, we get duplicate records when a reference has
	-- several authors.
	SELECT DISTINCT 
		RefA.Source_Key AS [Source_Key],
		CASE (SELECT Count(*) FROM Reference_Author WHERE Source_Key = RefA.Source_Key)
			-- Case with only one author
			WHEN 1	THEN CASE WHEN INITIALS IS NULL THEN Item_Name ELSE + Item_Name + ', ' + Initials END
			-- Case with two authors, format as '[Author 1] & [Author 2]'
			-- Use SORT_ORDER to get authors in correct order
			WHEN 2	THEN
				(SELECT Top 1 CASE WHEN INITIALS IS NULL THEN Item_Name ELSE + Item_Name + ', ' + Initials  END 
	 			 FROM Reference_Author 
				 WHERE Source_Key = RefA.Source_Key AND Sort_Order = 1) 
				+ ' & ' + 
				(SELECT Top 1 CASE WHEN INITIALS IS NULL THEN Item_Name ELSE + Item_Name + ', ' + Initials END
         FROM Reference_Author 
				 WHERE Source_Key = RefA.Source_Key AND Sort_Order = 2) 
			-- Case with three or more authors, format as '[Author 1] et al.'
			-- Use SORT_ORDER to get authors in correct order
			ELSE	(SELECT Top 1 CASE WHEN INITIALS IS NULL THEN Item_Name ELSE + Item_Name + ', ' + Initials END
         FROM Reference_Author 
				 WHERE Source_Key = RefA.Source_Key AND Sort_Order = 1) 
				+ ' et al.'
		END AS [Author]
	FROM	Reference_Author AS RefA




GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

GRANT  SELECT  ON [dbo].[VW_REFERENCE_AUTHORS]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[VW_REFERENCE_AUTHORS]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[VW_REFERENCE_AUTHORS]  TO [R2k_AddOnly]
GO

GRANT  SELECT  ON [dbo].[VW_REFERENCE_AUTHORS]  TO [R2k_FullEdit]
GO

GRANT  SELECT  ON [dbo].[VW_REFERENCE_AUTHORS]  TO [R2k_Administrator]
GO

