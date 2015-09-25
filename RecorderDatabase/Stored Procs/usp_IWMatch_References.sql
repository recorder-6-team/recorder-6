/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_References') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_IWMatch_References]
GO

/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	June 2004

  Last revision information:
    $Revision: 3 $
    $Date: 6/07/04 11:10 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatch_References]
AS
	UPDATE	#References
	SET	Match_Count =  (SELECT Count(*)
				FROM	Reference R
				JOIN	vw_Reference_Authors RA ON RA.Source_Key = R.Source_Key
				WHERE 	Import_Value LIKE '%' + Cast(dbo.FormatDatePart(Year_Vague_date_Start, Year_Vague_Date_End, Year_Vague_Date_Type, 0) AS varchar(50)) + '%'
				AND 	Import_Value LIKE '%' + Replace(Cast(Title AS varchar(1000)), ' ', '%') + '%'
				AND	Import_Value LIKE '%' + Author + '%'
				)
	WHERE	Match_Key IS NULL

	-- Now get values and keys for unique matches only.
	UPDATE	#References
	SET	Match_Key = R.Source_Key,
		Match_Value = dbo.ufn_GetFormattedReferenceName(R.Source_Key)
	FROM	Reference R
	JOIN	vw_Reference_Authors RA ON RA.Source_Key = R.Source_Key
	WHERE 	Match_Count = 1
	AND	Match_Key IS NULL
	AND	Import_Value  LIKE '%' + Cast(dbo.FormatDatePart(Year_Vague_date_Start, Year_Vague_Date_End, Year_Vague_Date_Type, 0) AS varchar(50)) + '%'
	AND 	Import_Value  LIKE '%' + Replace(Cast(Title AS varchar(1000)), ' ', '%') + '%'
	AND	Import_Value  LIKE '%' + Author + '%'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatch_References') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatch_References'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatch_References TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatch_References TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatch_References TO [Dev - JNCC SQL]
END
GO