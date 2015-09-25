ALTER TABLE Reference_Keyword DROP CONSTRAINT FK_Reference_Keyword_Reference

ALTER TABLE [dbo].[Reference_Keyword] ADD 
	CONSTRAINT [FK_Reference_Keyword_Reference] FOREIGN KEY 
	(
		[Source_Key]
	) REFERENCES [dbo].[REFERENCE] (
		[SOURCE_KEY]
	) ON DELETE CASCADE 
GO