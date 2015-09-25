IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchRuleKey_Get]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_IWMatchRuleKey_Get]
GO
