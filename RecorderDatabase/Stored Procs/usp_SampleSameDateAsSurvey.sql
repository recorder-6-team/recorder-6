IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SampleSameDateAsSurvey]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    BEGIN
        PRINT 'Dropping procedure usp_SampleSameDateAsSurvey'
        DROP PROCEDURE [dbo].[usp_SampleSameDateAsSurvey]
    END
GO

    PRINT 'Creating procedure usp_SampleSameDateAsSurvey'
GO

/*
    $History: usp_SampleSameDateAsSurvey.sql $
 * 
 * *****************  Version 1  *****************
 * User: Johndurman   Date: 18/02/08   Time: 10:26
 * Created in $/JNCC/Development/Build/SQL Scripts/Stored Procs
 * VI 15117 - CCN174 - NPWS - location and date cascading in observations

*/

CREATE PROCEDURE dbo.usp_SampleSameDateAsSurvey
--
--	DESCRIPTION
--	This procedure checks whether the specified sample has the same vague date as its survey event
--
--	PARAMETERS
--	NAME			DESCRIPTION
--
--
--	AUTHOR:	John Durman, Dorset Software.
--	CREATED: 15/02/2008
--
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
@SampleKey CHAR(16),
@SameDates BIT OUTPUT
--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
AS
SET NOCOUNT ON

	IF EXISTS (
		SELECT		S.Sample_Key 
		FROM		Sample				S
		INNER JOIN	Survey_Event		E
			ON		S.Survey_Event_Key	=	E.Survey_Event_Key
			AND		S.Vague_Date_Start	=	E.Vague_Date_Start
			AND		S.Vague_Date_End	=	E.Vague_Date_End
			AND		S.Vague_Date_Type	=	E.Vague_Date_Type
		WHERE		S.Sample_Key		=	@SampleKey
	)
		SET		@SameDates	=	1
	ELSE
		SET		@SameDates	=	0


--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
GO

    IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleSameDateAsSurvey') AND SysStat & 0xf = 4)
    BEGIN
		PRINT 'Setting up security on procedure [dbo].[usp_SampleSameDateAsSurvey]'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON [dbo].[usp_SampleSameDateAsSurvey] TO [R2k_AddOnly]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_SampleSameDateAsSurvey] TO [R2k_Administrator]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_SampleSameDateAsSurvey] TO [R2k_FullEdit]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON [dbo].[usp_SampleSameDateAsSurvey] TO [R2k_ReadOnly]

		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_SampleSameDateAsSurvey] TO [R2k_RecordCardsOnly]
    END
GO
