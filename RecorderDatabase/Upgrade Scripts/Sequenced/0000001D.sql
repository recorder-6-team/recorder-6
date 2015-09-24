IF EXISTS ( SELECT 1
            FROM   sysobjects
            WHERE  id = object_id('dbo.usp_ImportWizard_GetViceCountyLocation')
            AND    Type = 'P')
    DROP PROCEDURE dbo.usp_ImportWizard_GetViceCountyLocation
GO