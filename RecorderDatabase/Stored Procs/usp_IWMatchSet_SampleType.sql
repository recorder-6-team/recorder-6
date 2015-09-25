/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatchSet_SampleType')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchSet_SampleType]
GO

/*===========================================================================*\
  Description:  Set the match for the specified import value in the match
                table.

  Parameters:   @ImportValue            Imported value
                @MatchKey               Identifies matching record

  Created:      July 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/07/04 14:37 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchSet_SampleType]
    @ImportValue    VARCHAR(100),
    @MatchKey       CHAR(16)
AS
    IF @MatchKey IS NOT NULL
        UPDATE      i
        SET         i.Match_Value       =   t.Short_Name,
                    i.Match_Key         =   @MatchKey,
                    i.Match_Count       =   1,
                    i.Manual_Match      =   1,
                    i.Remembered        =   0
        FROM        #SampleTypes        AS  i,
                    Sample_Type         AS  t
        WHERE       i.Import_Value      =   @ImportValue
        AND         t.Sample_Type_Key   =   @MatchKey
    ELSE
        UPDATE      #SampleTypes
        SET         Match_Value         =   NULL,
                    Match_Key           =   NULL,
                    Match_Count         =   NULL,
                    Manual_Match        =   0,
                    Remembered          =   0
        WHERE       Import_Value        =   @ImportValue
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchSet_SampleType') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatchSet_SampleType'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatchSet_SampleType TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatchSet_SampleType TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatchSet_SampleType TO [Dev - JNCC SQL]
END
GO