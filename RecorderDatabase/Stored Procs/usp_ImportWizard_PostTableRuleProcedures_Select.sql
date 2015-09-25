/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_ImportWizard_PostTableRuleProcedures_Select]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_ImportWizard_PostTableRuleProcedures_Select]
GO

/*===========================================================================*\
  Description:  List stored procedures that must be run after the table rules
                have been applied.

  Parameters:   None

  Created:      June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 5/07/04 14:09 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ImportWizard_PostTableRuleProcedures_Select]
AS
    SELECT      Procedure_Name
    FROM        IW_Post_Processing_Procedure
    WHERE       Required_Table_Name                     IS NULL
    OR          (SELECT     object_id(
                                'tempdb..#'
                                + Required_Table_Name)) IS NOT NULL
    ORDER BY    Sequence
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ImportWizard_PostTableRuleProcedures_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ImportWizard_PostTableRuleProcedures_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ImportWizard_PostTableRuleProcedures_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ImportWizard_PostTableRuleProcedures_Select TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ImportWizard_PostTableRuleProcedures_Select TO [Dev - JNCC SQL]
END