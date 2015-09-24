/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_GetAdditionalObservationKeys') 
	   AND    ObjectProperty(Id, N'IsProcedure') = 1)
    DROP PROCEDURE dbo.usp_GetAdditionalObservationKeys
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	
			Gets a table containing observation key values for inclusion in a 
			custody reassignment.

  Created:	February 2009

  Last revision information:
    $Revision: 1 $
    $Date: 13/02/09 11:46 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_GetAdditionalObservationKeys
	@IncludeObservations	BIT
AS

	SELECT 		S.Survey_Key, 
				R1.Source_Key				AS Survey_Reference, 
				S.Run_By,
				SE.Survey_Event_Key, 
				R2.Source_Key				AS Event_Reference,
				SER.Name_Key				AS SE_Recorder_Key, 
				SE.Location_Key				AS Event_Location_Key,
				SA.Sample_Key, 
				R3.Source_Key				AS Sample_Reference, 
				SA.Location_Key				AS Sample_Location_Key,
				TXO.Taxon_Occurrence_Key, 
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				BIO.Biotope_Occurrence_Key, 
				R5.Source_Key				AS Biotope_Occurrence_Reference
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	INNER JOIN	#Key_List					KL
			ON	(	KL.KeyField	=	S.Survey_Key
				AND	KL.ItemTable	=	'Survey'				)
			OR	(	KL.KeyField	=	SE.Survey_Event_Key
				AND	KL.ItemTable	=	'Survey_Event'			)
			OR	(	KL.KeyField	=	SA.Sample_Key
				AND	KL.ItemTable	=	'Sample'				)
			OR	(	KL.KeyField	=	TXO.Taxon_Occurrence_Key
				AND	KL.ItemTable	=	'Taxon_Occurrence'		)
			OR	(	KL.KeyField	=	BIO.Biotope_Occurrence_Key
				AND	KL.ItemTable	=	'Biotope_Occurrence'	)
			OR	(	@IncludeObservations		=	1
				AND	(	(	KL.KeyField		=	SE.Location_Key
							OR	KL.KeyField	=	SA.Location_Key	)
						AND	KL.ItemTable		=	'Location'				)
					OR	(	(	KL.KeyField	=	S.Run_By
							OR	KL.KeyField	=	SER.Name_Key	)
						AND	KL.ItemTable	IN	(	'Name',
													'Individual',
													'Organisation'	)	)	)
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GetAdditionalObservationKeys') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_GetAdditionalObservationKeys'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_GetAdditionalObservationKeys TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_GetAdditionalObservationKeys TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_GetAdditionalObservationKeys TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_GetAdditionalObservationKeys TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_GetAdditionalObservationKeys TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_GetAdditionalObservationKeys TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_GetAdditionalLocationKeys') 
	   AND    ObjectProperty(Id, N'IsProcedure') = 1)
    DROP PROCEDURE dbo.usp_GetAdditionalLocationKeys
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	
			Gets a table containing location key values for inclusion in a 
			custody reassignment.

  Created:	February 2009

  Last revision information:
    $Revision: 1 $
    $Date: 13/02/09 11:46 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_GetAdditionalLocationKeys
AS

	SELECT DISTINCT L.Location_Key 
	FROM		Location L 
	LEFT JOIN	Location_Designation	LD 
			ON	LD.Location_Key			=	L.Location_Key 
	LEFT JOIN	Location_Feature		LF 
			ON	LF.Location_Key			=	L.Location_Key 
	LEFT JOIN	Management_Aim			MA 
			ON	MA.Location_Feature_Key	=	LF.Location_Feature_Key 
	LEFT JOIN	Tenure					T 
			ON	T.Location_Key			=	L.Location_Key
	INNER JOIN	#Key_List				KL
			ON	KL.ItemTable			=	'Location'
			AND	(	LD.Authority		=	KL.KeyField
				OR	MA.Authority		=	KL.KeyField
				OR	T.Owned_By			=	KL.KeyField	)
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GetAdditionalLocationKeys') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_GetAdditionalLocationKeys'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_GetAdditionalLocationKeys TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_GetAdditionalLocationKeys TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_GetAdditionalLocationKeys TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_GetAdditionalLocationKeys TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_GetAdditionalLocationKeys TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_GetAdditionalLocationKeys TO [Dev - JNCC SQL]
END
GO