/****** Object:  StoredProcedure [dbo].[usp_ThesaurusFact_Delete]    Script Date: 03/08/2016 14:48:40 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	Delete a record from the Thesaurus_Fact table.

  Parameters:	@Key		Thesaurus Fact key
		@Timestamp

  Created:	December 2003
       
  Revised:      March 2016

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_Fact table exists before
			  attempting any of this deletion. 		
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_Fact]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE	@TaxonFactKey char(16)

				SELECT 	@TaxonFactKey = Taxon_Fact_Key
				FROM 	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Fact
				WHERE	Taxon_Fact_Key = @TaxonFactKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END	
		ELSE BEGIN
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping]')
					AND 	  Type = 'U')
			BEGIN 
			  DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
			  WHERE	Thesaurus_Fact_Key = @Key
  			  IF @@Error <> 0 GOTO RollbackAndExit
	        END 
		END

		DELETE	Thesaurus_Fact
		WHERE	Thesaurus_Fact_Key = @Key
		AND		(@Timestamp = Timestamp OR @Timestamp IS NULL)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION