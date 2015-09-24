/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_GetAdditionalObservationKeys') 
	   AND    ObjectProperty(Id, N'IsProcedure') = 1)
    DROP PROCEDURE dbo.usp_GetAdditionalObservationKeys
GO
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_ObservationKeys_Get_FromKeylist') 
	   AND    ObjectProperty(Id, N'IsProcedure') = 1)
    DROP PROCEDURE dbo.usp_ObservationKeys_Get_FromKeylist
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
    $Revision: 2 $
    $Date: 4/03/09 8:57 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_ObservationKeys_Get_FromKeylist
	@IncludeObservations	BIT
AS
	/*	It is faster to join seperately for each type of key in the key list 
		and then combine the results using UNION ALL than to perform all the
		filtering in a single SELECT statement and a complicated final join
		to the #Key_List table. As a result, prepare for a nasty text wall. */

	-- Join to the Taxon_Occurrence keys:
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField					=	TXO.Taxon_Occurrence_Key
			AND	KL.ItemTable				=	'Taxon_Occurrence'

	--	Join to the Survey keys:
	UNION ALL
	
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField					=	S.Survey_Key
			AND	KL.ItemTable				=	'Survey'	

	--	Join to the Survey_Event keys:
	UNION ALL
	
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField					=	SE.Survey_Event_Key
			AND	KL.ItemTable				=	'Survey_Event'	

	--	Join to the Sample keys:
	UNION ALL
	
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField					=	SA.Sample_Key
			AND	KL.ItemTable				=	'Sample'

	--	Join to the Biotope_Occurrence keys:
	UNION ALL
	
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField					=	BIO.Biotope_Occurrence_Key
			AND	KL.ItemTable				=	'Biotope_Occurrence'	

	--	Join to the Location keys and their observations if @IncludeObservations is 1:
	UNION ALL
	
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField	IN	(SE.Location_Key,	SA.Location_Key)
			AND	KL.ItemTable				=	'Location'	
	WHERE		@IncludeObservations		=	1	

	--	Join to the Name keys and their observations if @IncludeObservations is 1:
	UNION ALL
	
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField		IN	(S.Run_By, SER.Name_Key)
			AND	KL.ItemTable	IN	('Name', 'Individual', 'Organisation')
	WHERE		@IncludeObservations		=	1

	--	Join to the Survey_Event_Owner keys:
	UNION ALL
	
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField					=	SEO.Survey_Event_Owner_Key
			AND	KL.ItemTable				=	'Survey_Event_Owner'	

	--	Join to the Survey_Event_Owner_Type keys:
	UNION ALL
	
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField					=	SEOT.Survey_Event_Owner_Type_Key
			AND	KL.ItemTable				=	'Survey_Event_Owner_Type'	

	--	Join to the Survey_Tag keys:
	UNION ALL
	
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField					=	ST.Survey_Tag_Key
			AND	KL.ItemTable				=	'Survey_Tag'	

	--	Join to the Concept keys:
	UNION ALL
	
	SELECT		S.Survey_Key,
				S.Custodian					AS Survey_Custodian, 
				R1.Source_Key				AS Survey_Reference,
				S1.Custodian				AS Survey_Reference_Custodian, 
				S.Run_By,
				RBN.Custodian				AS Run_By_Custodian,
				SE.Survey_Event_Key, 
				SE.Custodian				AS Survey_Event_Custodian,
				R2.Source_Key				AS Event_Reference,
				S2.Custodian				AS Event_Reference_Custodian,
				SER.Name_Key				AS SE_Recorder_Key, 
				SERN.Custodian				AS SE_Recorder_Custodian,
				SE.Location_Key				AS Event_Location_Key,
				SEL.Custodian				AS Event_Location_Custodian,
				SA.Sample_Key, 
				SA.Custodian				AS Sample_Custodian,
				R3.Source_Key				AS Sample_Reference,
				S3.Custodian				AS Sample_Reference_Custodian,
				SA.Location_Key				AS Sample_Location_Key,
				SAL.Custodian				AS Sample_Location_Custodian,
				TXO.Taxon_Occurrence_Key, 
				TXO.Custodian				AS Taxon_Occurrence_Custodian,
				R4.Source_Key				AS Taxon_Occurrence_Reference,
				S4.Custodian				AS Taxon_Occurrence_Reference_Custodian,
				BIO.Biotope_Occurrence_Key, 
				BIO.Custodian				AS Biotope_Occurrence_Custodian,
				R5.Source_Key				AS Biotope_Occurrence_Reference,
				S5.Custodian				AS Biotope_Occurrence_Reference_Custodian,
				SEO.Survey_Event_Owner_Key,
				SEO.Custodian				AS Survey_Event_Owner_Custodian,
				SEOT.Survey_Event_Owner_Type_Key,
				SEOT.Custodian				AS Survey_Event_Owner_Type_Custodian,
				ST.Survey_Tag_Key,
				ST.Custodian				AS Survey_Tag_Custodian,
				C.Concept_Key,
				C.Custodian					AS Concept_Custodian
	FROM		Survey	S
	LEFT JOIN	Survey_Event				SE 
			ON	SE.Survey_Key				=	S.Survey_Key
	LEFT JOIN	Name						RBN
			ON	S.Run_By					=	RBN.Name_Key
	LEFT JOIN	Location					SEL
			ON	SEL.Location_Key			=	SE.Location_Key
	LEFT JOIN	Sample						SA 
			ON	SA.Survey_Event_Key			=	SE.Survey_Event_Key
	LEFT JOIN	Location					SAL
			ON	SAL.Location_Key			=	SA.Location_Key
	LEFT JOIN	Taxon_Occurrence			TXO
			ON	TXO.Sample_Key				=	SA.Sample_Key
	LEFT JOIN	Biotope_Occurrence			BIO 
			ON	BIO.Sample_Key				=	SA.Sample_Key
	-- References for surveys
	LEFT JOIN	Survey_Sources				SS 
			ON	SS.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Reference					R1 
			ON	R1.Source_Key				=	SS.Source_Key
	LEFT JOIN	Source						S1
			ON	S1.Source_Key				=	R1.Source_Key
	LEFT JOIN	Survey_Event_Owner			SEO
			ON	SEO.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Survey_Event_Owner_Type		SEOT
			ON	SEOT.Survey_Event_Owner_Type_Key
											=	SEO.Survey_Event_Owner_Type_Key
	LEFT JOIN	Survey_Tag					ST
			ON	ST.Survey_Key				=	SE.Survey_Key
	LEFT JOIN	Concept						C
			ON	C.Concept_Key				=	ST.Concept_Key
	-- References and recorder for events
	LEFT JOIN	Survey_Event_Sources		SES 
			ON	SES.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Reference					R2 
			ON	R2.Source_Key				=	SES.Source_Key
	LEFT JOIN	Source						S2
			ON	S2.Source_Key				=	R2.Source_Key
	LEFT JOIN	Survey_Event_Recorder		SER 
			ON	SER.Survey_Event_Key		=	SE.Survey_Event_Key
	LEFT JOIN	Name						SERN
			ON	SERN.Name_Key				=	SER.Name_Key
	-- References for samples
	LEFT JOIN	Sample_Sources				SAS 
			ON	SAS.Sample_Key				=	SA.Sample_Key 
	LEFT JOIN	Reference					R3 
			ON	R3.Source_Key				=	SAS.Source_Key
	LEFT JOIN	Source						S3
			ON	S3.Source_Key				=	R3.Source_Key
	-- References for taxon occurrences
	LEFT JOIN	Taxon_Occurrence_Sources	TOS 
			ON	TOS.Taxon_Occurrence_Key	=	TXO.Taxon_Occurrence_Key 
	LEFT JOIN	Reference					R4 
			ON	R4.Source_Key				=	TOS.Source_Key
	LEFT JOIN	Source						S4
			ON	S4.Source_Key				=	R4.Source_Key
	-- References for Biotope Occurrences
	LEFT JOIN	Biotope_Occurrence_Sources	BOS 
			ON	BOS.Biotope_Occurrence_Key	=	BIO.Biotope_Occurrence_Key 
	LEFT JOIN	Reference R5 
			ON	R5.Source_Key				=	BOS.Source_Key
	LEFT JOIN	Source						S5
			ON	S5.Source_Key				=	R5.Source_Key
	INNER JOIN	#Key_List					KL
			ON	KL.KeyField					=	C.Concept_Key
			AND	KL.ItemTable				=	'Concept'

GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ObservationKeys_Get_FromKeylist') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ObservationKeys_Get_FromKeylist'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ObservationKeys_Get_FromKeylist TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ObservationKeys_Get_FromKeylist TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_ObservationKeys_Get_FromKeylist TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_ObservationKeys_Get_FromKeylist TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_ObservationKeys_Get_FromKeylist TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ObservationKeys_Get_FromKeylist TO [Dev - JNCC SQL]
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
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_LocationKeys_Get_FromKeylist') 
	   AND    ObjectProperty(Id, N'IsProcedure') = 1)
    DROP PROCEDURE dbo.usp_LocationKeys_Get_FromKeylist
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
    $Revision: 2 $
    $Date: 4/03/09 8:57 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_LocationKeys_Get_FromKeylist
	@Custodian	CHAR(8)
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
			AND	L.Custodian				=	@Custodian
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationKeys_Get_FromKeylist') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_LocationKeys_Get_FromKeylist'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationKeys_Get_FromKeylist TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationKeys_Get_FromKeylist TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_LocationKeys_Get_FromKeylist TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
       	GRANT EXECUTE ON dbo.usp_LocationKeys_Get_FromKeylist TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
       	GRANT EXECUTE ON dbo.usp_LocationKeys_Get_FromKeylist TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_LocationKeys_Get_FromKeylist TO [Dev - JNCC SQL]
END
GO