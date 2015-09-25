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
    $Revision: 4 $
    $Date: 16/12/04 15:46 $
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