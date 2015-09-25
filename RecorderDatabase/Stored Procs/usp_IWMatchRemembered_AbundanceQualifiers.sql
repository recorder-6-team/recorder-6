/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_AbundanceQualifiers') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_IWMatchRemembered_AbundanceQualifiers]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values from previous imports.

  Parameters:	<none>

  Created:	July 2004

  Last revision information:
    $Revision: 2 $
    $Date: 6/07/04 11:10 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchRemembered_AbundanceQualifiers]
AS
	-- Update temp table with relevant data.
	UPDATE 	#AbundanceQualifiers
	SET 	Match_Count = 1, 
		Match_Key = Matched_Key, 
		Match_Value = Short_Name, 
		Remembered = 1
	FROM 	IW_Matched_Abundance_Qualifiers 
	JOIN	Measurement_Qualifier ON Measurement_Qualifier_Key = Matched_Key
	WHERE 	Import_Value = Matched_Value 
	AND 	Match_Key IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchRemembered_AbundanceQualifiers') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchRemembered_AbundanceQualifiers'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AbundanceQualifiers TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AbundanceQualifiers TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchRemembered_AbundanceQualifiers TO [Dev - JNCC SQL]
END
GO