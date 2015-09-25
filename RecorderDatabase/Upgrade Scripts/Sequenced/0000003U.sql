/* Script created by JVB 21/12/2006 following problems users were experiencing 
upgrading the species dictionary. The problem was that when importing a zip file
some users did not have sufficient rights to import data into the NAMESERVER and
TAXON_GROUP tables.*/

GRANT  SELECT  ON [dbo].[NAMESERVER]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[NAMESERVER]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT ,  INSERT  ON [dbo].[NAMESERVER]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[NAMESERVER]  TO
[R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[NAMESERVER]  TO
[R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[TAXON_GROUP]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[TAXON_GROUP]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT ,  INSERT  ON [dbo].[TAXON_GROUP]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[TAXON_GROUP]  TO
[R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[TAXON_GROUP]  TO
[R2k_Administrator]
GO
