IF NOT EXISTS(SELECT 1 FROM Sysobjects WHERE Name='Specimen_Unit' AND Type='U')
	-- Drop usp_SpecimenTypes_Select if Collections Module not installed
	IF EXISTS (SELECT 1
			FROM   SysObjects 
			WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenTypes_Select]')
			AND 	  Type = 'P')
		DROP PROCEDURE [dbo].[usp_SpecimenTypes_Select]

GO

--Correct match rule for specimen types to point at standard term list
UPDATE IW_Match_Rule
SET Termlist_Select_Procedure = 'usp_SpecimenTypeTermList_Select'
WHERE IW_Match_Rule_Key='SYSTEM0100000009'