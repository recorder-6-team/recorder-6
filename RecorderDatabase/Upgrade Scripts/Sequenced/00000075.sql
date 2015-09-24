/*
  Adds an index to Index_Taxon_Name for use when performing a search by name.

  Last Revision Details:
    $Revision: 2 $
    $Date: 6/05/09 15:24 $
    $Author: Simonwood $
  
*/

IF NOT EXISTS (SELECT * FROM dbo.SysIndexes WHERE NAME = N'IX_INDEX_TAXON_NAME_Filter' AND id = OBJECT_ID(N'dbo.[Index_Taxon_Name]'))
	CREATE NONCLUSTERED INDEX [IX_INDEX_TAXON_NAME_Filter] ON [dbo].[INDEX_TAXON_NAME] 
	(

		  [ACTUAL_NAME] ASC,

		  [ABBREVIATION] ASC,

		  [AUTHORITY] ASC,

		  [PREFERRED_LIST] ASC

	) ON [PRIMARY]
