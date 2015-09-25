SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IndexTaxonName_ApplyNameServer]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer]
GO

/*===========================================================================*\
  Description: Applies the NameServer information to the Index_Taxon_Name
		Recommended_Taxon_List_Item_Key table.  Updates all records where this value
		is null.

  Parameters:	None

  Created:	November 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/12/04 13:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IndexTaxonName_ApplyNameServer]
AS
/* Remove any disconnected index_taxon_name records */
DELETE ITN 
FROM Index_Taxon_Name ITN
LEFT JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
WHERE TLI.Taxon_List_Item_Key IS NULL

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = NS.RECOMMENDED_TAXON_LIST_ITEM_KEY
FROM NAMESERVER NS
INNER JOIN TAXON_LIST_ITEM TLI ON NS.INPUT_TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON ITN.TAXON_LIST_ITEM_KEY = TLI.TAXON_LIST_ITEM_KEY
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL 

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM TAXON_LIST TL 
INNER JOIN TAXON_LIST_VERSION TLV ON TL.TAXON_LIST_KEY = TLV.TAXON_LIST_KEY
INNER JOIN TAXON_LIST_ITEM TLI ON TLV.TAXON_LIST_VERSION_KEY = TLI.TAXON_LIST_VERSION_KEY
INNER JOIN TAXON_LIST_ITEM TLI1 ON TLI.TAXON_VERSION_KEY = TLI1.TAXON_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON TLI1.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
WHERE TL.PREFERRED=1 AND ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM TAXON_VERSION TV
INNER JOIN TAXON_LIST_ITEM TLI ON TV.TAXON_VERSION_KEY = TLI.TAXON_VERSION_KEY
INNER JOIN TAXON_GROUP TG ON TV.Output_group_key = TG.TAXON_GROUP_KEY
INNER JOIN TAXON_LIST_VERSION TLV ON TLV.TAXON_LIST_KEY=TG.USE_TAXON_LIST_KEY
INNER JOIN TAXON_LIST_ITEM AS TLI1 
		ON TLI.TAXON_VERSION_KEY = TLI1.TAXON_VERSION_KEY
		AND TLI1.TAXON_LIST_VERSION_KEY=TLV.TAXON_LIST_VERSION_KEY
INNER JOIN INDEX_TAXON_NAME ITN ON TLI1.TAXON_LIST_ITEM_KEY = ITN.TAXON_LIST_ITEM_KEY
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL

UPDATE ITN
SET ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY = TLI.PREFERRED_NAME
FROM INDEX_TAXON_NAME ITN
INNER JOIN TAXON_LIST_ITEM TLI ON TLI.TAXON_LIST_ITEM_KEY=ITN.TAXON_LIST_ITEM_KEY
WHERE ITN.RECOMMENDED_TAXON_LIST_ITEM_KEY IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IndexTaxonName_ApplyNameServer') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IndexTaxonName_ApplyNameServer'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IndexTaxonName_ApplyNameServer TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IndexTaxonName_ApplyNameServer TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AbundanceQualifier') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_AbundanceQualifier]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/12/04 13:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_AbundanceQualifier]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Measurement_Qualifier', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Measurement_Qualifier(
			Measurement_Qualifier_Key, Short_Name, Long_Name, Measurement_Type_Key, Entered_By
		) VALUES (
			@Key, CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, 'NBNSYS0000000004', @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#AbundanceQualifiers
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AbundanceQualifier') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_AbundanceQualifier'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AbundanceQualifier TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AssociationType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_AssociationType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/12/04 13:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_AssociationType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Relationship_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Relationship_Type(
			Relationship_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#AssociationTypes
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_AssociationType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_AssociationType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AssociationType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AssociationType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_AssociationType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_RecordType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_RecordType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/12/04 13:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_RecordType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Record_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Record_Type(
			Record_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, 
			CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, 
			@EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#RecordTypes
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_RecordType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_RecordType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_RecordType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatchNewEntry_SampleType')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_SampleType]
GO

/*===========================================================================*\
  Description:  Create a new item in a term list from an import value.

  Parameters:   @ImportValue            Name of new item
                @EnteredBy              Identifies current user

  Created:      July 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/12/04 13:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_SampleType]
    @ImportValue    VARCHAR(100),
    @EnteredBy      CHAR(16)
AS
    DECLARE     @sample_type_key    CHAR(16)

    EXECUTE     spNextKey   'Sample_Type',
                            @sample_type_key    OUTPUT
    IF @@ERROR <> 0 RETURN

    BEGIN TRANSACTION

    INSERT INTO Sample_Type (
                Sample_Type_Key,
                Short_Name,
                Long_Name,
                Entered_By)
    VALUES      (@sample_type_key,
                CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END,
                @ImportValue,
                @EnteredBy)

    IF @@ERROR <> 0 GOTO RollbackAndExit

    /* update import table with new data */
    UPDATE      #SampleTypes
    SET         Match_Value     =   Import_Value,
                Match_Key       =   @sample_type_key,
                Match_Count     =   1,
                Manual_Match    =   1,
                Remembered      =   0
    WHERE       Import_Value    =   @ImportValue

    IF @@ERROR <> 0 GOTO RollbackAndExit
    
    COMMIT TRANSACTION
    RETURN 0

RollBackAndExit:
    ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_SampleType') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatchNewEntry_SampleType'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SampleType TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SampleType TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SampleType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_SpecimenType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_SpecimenType]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/12/04 13:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_SpecimenType]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Specimen_Type', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Specimen_Type(
			Specimen_Type_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, 
			CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, 
			@EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#SpecimenTypes
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_SpecimenType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_SpecimenType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SpecimenType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SpecimenType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_SpecimenType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Substrate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_Substrate]
GO

/*===========================================================================*\
  Description:	Create a new item in a term list from an import value.

  Parameters:	
	@ImportValue	The name of the new item.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/12/04 13:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_Substrate]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
	EXECUTE spNextKey 'Substrate', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Substrate(
			Substrate_Key, Short_Name, Long_Name, Entered_By
		) VALUES (
			@Key, CASE WHEN LEN(@ImportValue)>20 THEN LEFT(@ImportValue, 17) + '...' ELSE @ImportValue END, 
			@ImportValue, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#Substrates
		SET	Match_Value = Import_Value,
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Substrate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_Substrate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Substrate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Substrate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Substrate TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDataEntryLists_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonDataEntryLists_Select]
GO

/*===========================================================================*\
  Description:	Returns list of taxon lists.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 20/12/04 13:54 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDataEntryLists_Select]
AS
	SELECT	TL.Taxon_List_Key AS Item_Key, TL.Item_Name
	FROM	Taxon_List TL
	INNER JOIN Taxon_List_Type TLT 
		ON TLT.Taxon_List_Type_Key=TL.Taxon_List_Type_Key
		AND TLT.Allow_Data_Entry=1
	ORDER BY Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDataEntryLists_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDataEntryLists_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDataEntryLists_Select TO [Dev - JNCC SQL]
END
GO

