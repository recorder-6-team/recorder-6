/*===========================================================================*\
  Drop stored proc before re-creating.
*===========================================================================*/
IF EXISTS ( SELECT 1
            FROM   SysObjects
            WHERE  Id = Object_Id(N'dbo.usp_IWMatchRecord_SampleTypes')
            AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchRecord_SampleTypes]
GO

/*===========================================================================*\
  Description:  Record matches for future imports.

  Parameters:   <none>

  Created:  July 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/07/04 14:37 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRecord_SampleTypes]
AS
	/* Update existing items, if they were "rematched" */
	UPDATE     m
	SET        m.Matched_Key           =   i.Match_Key
	FROM       #SampleTypes            AS  i
	INNER JOIN IW_Matched_Sample_Types AS  m
	ON         m.Matched_Value         =   i.Import_Value
	WHERE      i.Match_Key             IS NOT NULL
	AND        i.Manual_Match          = 1

	/* Add the new ones now. */
	INSERT INTO IW_Matched_Sample_Types (
	            Matched_Value,
	            Matched_Key)
	SELECT      i.Import_Value,
	            i.Match_Key
	FROM        #SampleTypes            AS  i
	LEFT JOIN   IW_Matched_Sample_Types AS  m
	ON          m.Matched_Value         =   i.Import_Value
	WHERE       m.Matched_Value         IS NULL
	AND         i.Match_Key             IS NOT NULL
	AND         i.Manual_Match          = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRecord_SampleTypes') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_IWMatchRecord_SampleTypes'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_IWMatchRecord_SampleTypes TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_IWMatchRecord_SampleTypes TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_IWMatchRecord_SampleTypes TO [Dev - JNCC SQL]
END
GO