/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_GetAssociatedOccurrence]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_GetAssociatedOccurrence]
GO

/*===========================================================================*\
  Description:  Identify the taxon occurrence record generated for the
                associated species of an import record.

  Parameters:   @record_no              Identifies the import record.
                @taxon_occurrency_key   [on exit] Taxon occurrence identifier.

  Created:      June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 30/06/04 16:07 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_GetAssociatedOccurrence]
    @record_no              INT,
    @taxon_occurrence_key   CHAR(16)    OUTPUT
AS
    DECLARE     @sample_key             CHAR(16),
                @taxon_list_item_key    CHAR(16)

    DECLARE     @dummy                  INT,
                @message                VARCHAR(1000)

    SELECT      @dummy              =   1
    FROM        #master
    WHERE       Record_NO         =   @record_no
    IF @@ROWCOUNT = 0
    BEGIN
        SELECT @message = 'Missing master record: ' + LTRIM(STR(@record_no))
        RAISERROR (@message, 16, 1)
        RETURN
    END

    SELECT      @dummy              =   1
    FROM        #RN_Sample
    WHERE       Record_No               =   @record_no
    IF @@ROWCOUNT = 0
    BEGIN
        SELECT @message = 'Missing sample record: ' + LTRIM(STR(@record_no))
        RAISERROR (@message, 16, 1)
        RETURN
    END

    SELECT      @dummy                  =   1
    FROM        #master                 AS  i
    INNER JOIN  #AssociatedSpecies                AS  m
    ON          m.Import_Value          =   i.SYSTEM0100000008_data
    WHERE       i.Record_No             =   @record_no
    IF @@ROWCOUNT = 0
    BEGIN
        SELECT @message = 'Missing species record: ' + LTRIM(STR(@record_no))
                + ':' + SYSTEM0100000008_data + ':'
        FROM    #master
        WHERE   Record_No = @record_no
        RAISERROR (@message, 16, 1)
        RETURN
    END

    SELECT      @sample_key             =   s.Sample_Key,
                @taxon_list_item_key    =   m.Match_Key
    FROM        #master                 AS  i
    INNER JOIN  #RN_Sample              AS  s
    ON          s.Record_No             =   i.Record_No
    INNER JOIN  #AssociatedSpecies      AS  m
    ON          m.Import_Value          =   i.SYSTEM0100000008_data
    WHERE       i.Record_No             =   @record_no

    IF @@ROWCOUNT = 0 GOTO NoSuchRecord

    SELECT      @taxon_occurrence_key   =   o.Taxon_Occurrence_Key
    FROM        #Taxon_Occurrence       AS  o
    INNER JOIN  #Taxon_Determination    AS  d
    ON          d.Taxon_Occurrence_Key  =   o.Taxon_Occurrence_Key
    WHERE       o.Sample_Key            =   @sample_key
    AND         d.Taxon_List_Item_Key   =   @taxon_list_item_key

    IF @@ROWCOUNT = 0 GOTO MissingAssociatedSpecies
    RETURN

NoSuchRecord:
    RAISERROR ('Unknown import record', 16, 1)
    RETURN

MissingAssociatedSpecies:
    RAISERROR ('Missing associated species', 16, 1)
    RETURN
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_GetAssociatedOccurrence') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_GetAssociatedOccurrence'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetAssociatedOccurrence TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetAssociatedOccurrence TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_GetAssociatedOccurrence TO [Dev - JNCC SQL]
END