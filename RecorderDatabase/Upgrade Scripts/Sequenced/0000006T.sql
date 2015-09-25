/*===========================================================================*\
  Grant additional permission for batch updates.
\*===========================================================================*/
GRANT DELETE ON dbo.User_Survey_Restriction TO R2k_FullEdit
GRANT DELETE ON dbo.User_Survey_Restriction TO R2k_Administrator

GRANT INSERT, UPDATE, DELETE ON dbo.Taxon_Designation_Set TO R2k_FullEdit
GRANT INSERT, UPDATE, DELETE ON dbo.Taxon_Designation_Set TO R2k_Administrator

GRANT INSERT, UPDATE, DELETE ON dbo.Taxon_Designation_Set_Item TO R2k_FullEdit
GRANT INSERT, UPDATE, DELETE ON dbo.Taxon_Designation_Set_Item TO R2k_Administrator
