SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.ConceptCustodianInsert    Script Date: 15/12/2005 13:38:58 ******/

CREATE TRIGGER ConceptCustodianInsert ON dbo.Concept AFTER INSERT AS UPDATE Concept SET Concept.CUSTODIAN = SUBSTRING(Concept.Concept_Key, 1, 8) FROM Concept INNER JOIN INSERTED ON Concept.Concept_Key = INSERTED.Concept_Key WHERE Concept.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.tr_Concept_AuthorCopy    Script Date: 15/12/2005 13:38:58 ******/

/*===========================================================================*\
  Description:	This trigger updates the Author_Copy field in the Concept 
		table when the Author_And_Date field in the Term_Version 
		table is updated

  Created:	Nov 2003

  Last revision information:
    $Revision: 3 $
    $Date: 22/12/05 16:14 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Concept_AuthorCopy] ON [dbo].[Concept] 
FOR UPDATE, INSERT

AS
	IF UPDATE (Term_Version_Key)
	BEGIN
		UPDATE Concept
		SET Concept.Author_Copy=Term_Version.Author_And_Date
		FROM Concept C 
		INNER JOIN Inserted I ON C.Concept_Key=I.Concept_Key
		LEFT JOIN Term_Version on Term_Version.Term_Version_Key=C.Term_Version_Key
	
		IF @@ERROR <>0
			RAISERROR('Error updating Author_Copy in Concept table',16,1)
	END


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Concept_DesignationCustodianInsert    Script Date: 15/12/2005 13:38:58 ******/

CREATE TRIGGER Concept_DesignationCustodianInsert ON dbo.Concept_Designation AFTER INSERT AS UPDATE Concept_Designation SET Concept_Designation.CUSTODIAN = SUBSTRING(Concept_Designation.Concept_Designation_Key, 1, 8) FROM Concept_Designation INNER JOIN INSERTED ON Concept_Designation.Concept_Designation_Key = INSERTED.Concept_Designation_Key WHERE Concept_Designation.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Concept_GroupCustodianInsert    Script Date: 15/12/2005 13:38:58 ******/

CREATE TRIGGER Concept_GroupCustodianInsert ON dbo.Concept_Group AFTER INSERT AS UPDATE Concept_Group SET Concept_Group.CUSTODIAN = SUBSTRING(Concept_Group.Concept_Group_Key, 1, 8) FROM Concept_Group INNER JOIN INSERTED ON Concept_Group.Concept_Group_Key = INSERTED.Concept_Group_Key WHERE Concept_Group.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Concept_Group_VersionCustodianInsert    Script Date: 15/12/2005 13:38:58 ******/

CREATE TRIGGER Concept_Group_VersionCustodianInsert ON dbo.Concept_Group_Version AFTER INSERT AS UPDATE Concept_Group_Version SET Concept_Group_Version.CUSTODIAN = SUBSTRING(Concept_Group_Version.Concept_Group_Version_Key, 1, 8) FROM Concept_Group_Version INNER JOIN INSERTED ON Concept_Group_Version.Concept_Group_Version_Key = INSERTED.Concept_Group_Version_Key WHERE Concept_Group_Version.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Concept_HistoryCustodianInsert    Script Date: 15/12/2005 13:38:58 ******/

CREATE TRIGGER Concept_HistoryCustodianInsert ON dbo.Concept_History AFTER INSERT AS UPDATE Concept_History SET Concept_History.CUSTODIAN = SUBSTRING(Concept_History.Concept_History_Key, 1, 8) FROM Concept_History INNER JOIN INSERTED ON Concept_History.Concept_History_Key = INSERTED.Concept_History_Key WHERE Concept_History.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.tr_ConceptHistory_IsCurrentUpdate    Script Date: 15/12/2005 13:38:58 ******/


/*===========================================================================*\
  Description:	The Concept table has an IsCurrent field that is set 
		depending on the Concept_History records associated with it.
		If a Concept has no associated Concept_History records
		then Is_Current is set to 1. If the Concept is associated
		with a Concept_History record that has no expiry details,
		Is_Current is set to 1. Otherwise, Is_Current is set to 0.

  Type:		AFTER INSERT, UPDATE, DELETE

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 22/12/05 16:14 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_ConceptHistory_IsCurrentUpdate]
ON [dbo].[Concept_History]
AFTER INSERT, UPDATE, DELETE
AS 
	/*--------------------------------------------------------------*\
	  Declare local variables and set @IsCurrent to be 0 as default.
	\*--------------------------------------------------------------*/
	DECLARE @IsCurrent bit,
		@CurrentConceptKey char(16)

	SET @IsCurrent = 0

	/*--------------------------------------------------------------*\
	  Get the Concept_Key for the Concept_History record that has
	  just been updated.
	\*--------------------------------------------------------------*/
	IF UPDATE(Concept_Group_Version_To) OR UPDATE(To_Vague_Date_Type)
		SELECT @CurrentConceptKey = Concept_Key FROM Inserted 

	ELSE IF (SELECT Count(*) FROM Deleted) > 0 
		SELECT @CurrentConceptKey = Concept_Key FROM Deleted 
	
	/*--------------------------------------------------------------*\
	  If there has been a change that requires the IsCurrent field
	  to be altered, then go ahead and make the change.
	\*--------------------------------------------------------------*/	
	IF @CurrentConceptKey IS NOT NULL
	BEGIN
		/*-----------------------------------------------------------------*\
		  If Concept has no associated Concept_History records (i.e. CH 
		  record just deleted).
		\*-----------------------------------------------------------------*/
		SELECT 	@IsCurrent = 	CASE WHEN Count(*) = 0 	THEN 1
								ELSE 0
					END
		FROM 	Concept_History
		WHERE	Concept_Key = @CurrentConceptKey

		/*-----------------------------------------------------------------*\
		  Check to see if Concept is associated with any Concept_History 
		  records that have no expiry date.
		\*-----------------------------------------------------------------*/
		IF @IsCurrent = 0
			SELECT 	@IsCurrent = 	CASE WHEN Count(*) = 0 	THEN 0
									ELSE 1
						END
			FROM 	Concept_History
			WHERE	Concept_Key = @CurrentConceptKey
			AND 	Concept_Group_Version_To IS NULL
			AND 	To_Vague_Date_Type IS NULL

		/*----------------------------------------*\
		  Actually update the Concept record.
		\*----------------------------------------*/
		UPDATE 	Concept
		SET	Is_Current = @IsCurrent
		WHERE	Concept_Key = @CurrentConceptKey
	END


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Concept_RankCustodianInsert    Script Date: 15/12/2005 13:38:58 ******/

CREATE TRIGGER Concept_RankCustodianInsert ON dbo.Concept_Rank AFTER INSERT AS UPDATE Concept_Rank SET Concept_Rank.CUSTODIAN = SUBSTRING(Concept_Rank.Concept_Rank_Key, 1, 8) FROM Concept_Rank INNER JOIN INSERTED ON Concept_Rank.Concept_Rank_Key = INSERTED.Concept_Rank_Key WHERE Concept_Rank.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Concept_RelationCustodianInsert    Script Date: 15/12/2005 13:38:58 ******/

CREATE TRIGGER Concept_RelationCustodianInsert ON dbo.Concept_Relation AFTER INSERT AS UPDATE Concept_Relation SET Concept_Relation.CUSTODIAN = SUBSTRING(Concept_Relation.Concept_Relation_Key, 1, 8) FROM Concept_Relation INNER JOIN INSERTED ON Concept_Relation.Concept_Relation_Key = INSERTED.Concept_Relation_Key WHERE Concept_Relation.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.DomainCustodianInsert    Script Date: 15/12/2005 13:38:58 ******/

CREATE TRIGGER DomainCustodianInsert ON dbo.[Domain] AFTER INSERT AS UPDATE Domain SET Domain.CUSTODIAN = SUBSTRING(Domain.Domain_Key, 1, 8) FROM Domain INNER JOIN INSERTED ON Domain.Domain_Key = INSERTED.Domain_Key WHERE Domain.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Domain_HyperlinkCustodianInsert    Script Date: 15/12/2005 13:38:58 ******/

CREATE TRIGGER Domain_HyperlinkCustodianInsert ON dbo.Domain_Hyperlink AFTER INSERT AS UPDATE Domain_Hyperlink SET Domain_Hyperlink.CUSTODIAN = SUBSTRING(Domain_Hyperlink.Domain_Hyperlink_Key, 1, 8) FROM Domain_Hyperlink INNER JOIN INSERTED ON Domain_Hyperlink.Domain_Hyperlink_Key = INSERTED.Domain_Hyperlink_Key WHERE Domain_Hyperlink.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Local_DomainCustodianInsert    Script Date: 15/12/2005 13:38:59 ******/

CREATE TRIGGER Local_DomainCustodianInsert ON dbo.Local_Domain AFTER INSERT AS UPDATE Local_Domain SET Local_Domain.CUSTODIAN = SUBSTRING(Local_Domain.Local_Domain_Key, 1, 8) FROM Local_Domain INNER JOIN INSERTED ON Local_Domain.Local_Domain_Key = INSERTED.Local_Domain_Key WHERE Local_Domain.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Meaning_RelationCustodianInsert    Script Date: 15/12/2005 13:38:59 ******/

CREATE TRIGGER Meaning_RelationCustodianInsert ON dbo.Meaning_Relation AFTER INSERT AS UPDATE Meaning_Relation SET Meaning_Relation.CUSTODIAN = SUBSTRING(Meaning_Relation.Meaning_Relation_Key, 1, 8) FROM Meaning_Relation INNER JOIN INSERTED ON Meaning_Relation.Meaning_Relation_Key = INSERTED.Meaning_Relation_Key WHERE Meaning_Relation.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Semantic_RelationCustodianInsert    Script Date: 15/12/2005 13:38:59 ******/

CREATE TRIGGER Semantic_RelationCustodianInsert ON dbo.Semantic_Relation AFTER INSERT AS UPDATE Semantic_Relation SET Semantic_Relation.CUSTODIAN = SUBSTRING(Semantic_Relation.Semantic_Relation_Key, 1, 8) FROM Semantic_Relation INNER JOIN INSERTED ON Semantic_Relation.Semantic_Relation_Key = INSERTED.Semantic_Relation_Key WHERE Semantic_Relation.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Subject_AreaCustodianInsert    Script Date: 15/12/2005 13:38:59 ******/

CREATE TRIGGER Subject_AreaCustodianInsert ON dbo.Subject_Area AFTER INSERT AS UPDATE Subject_Area SET Subject_Area.CUSTODIAN = SUBSTRING(Subject_Area.Subject_Area_Key, 1, 8) FROM Subject_Area INNER JOIN INSERTED ON Subject_Area.Subject_Area_Key = INSERTED.Subject_Area_Key WHERE Subject_Area.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.tr_TermVersion_AuthorCopy    Script Date: 15/12/2005 13:38:59 ******/


/*===========================================================================*\
  Description:	This trigger updates the Author_Copy field in the Concept 
		table when the Author_And_Date field in the Term_Version 
		table is updated

  Created:	Nov 2003

  Last revision information:
    $Revision: 3 $
    $Date: 22/12/05 16:14 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_TermVersion_AuthorCopy] ON [dbo].[Term_Version] 
FOR UPDATE, INSERT

AS
	IF UPDATE (Author_And_Date)
	BEGIN
		UPDATE Concept
		SET Concept.Author_Copy=I.Author_And_Date
		FROM Concept INNER JOIN Inserted I
		ON Concept.Term_Version_Key=I.Term_Version_Key
	
		IF @@ERROR <>0
			RAISERROR('Error updating Author_Copy in Concept table',16,1)
	END


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Term_VersionCustodianInsert    Script Date: 15/12/2005 13:38:59 ******/

 CREATE TRIGGER Term_VersionCustodianInsert ON dbo.Term_Version AFTER INSERT AS UPDATE Term_Version SET Term_Version.CUSTODIAN = SUBSTRING(Term_Version.Term_Version_Key, 1, 8) FROM Term_Version INNER JOIN INSERTED ON Term_Version.Term_Version_Key = INSERTED.Term_Version_Key WHERE Term_Version.CUSTODIAN IS NULL

GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Thesaurus_FactCustodianInsert    Script Date: 15/12/2005 13:38:59 ******/

CREATE TRIGGER Thesaurus_FactCustodianInsert ON dbo.Thesaurus_Fact AFTER INSERT AS UPDATE Thesaurus_Fact SET Thesaurus_Fact.CUSTODIAN = SUBSTRING(Thesaurus_Fact.Thesaurus_Fact_Key, 1, 8) FROM Thesaurus_Fact INNER JOIN INSERTED ON Thesaurus_Fact.Thesaurus_Fact_Key = INSERTED.Thesaurus_Fact_Key WHERE Thesaurus_Fact.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Thesaurus_Relation_TypeCustodianInsert    Script Date: 15/12/2005 13:38:59 ******/

CREATE TRIGGER Thesaurus_Relation_TypeCustodianInsert ON dbo.Thesaurus_Relation_Type AFTER INSERT AS UPDATE Thesaurus_Relation_Type SET Thesaurus_Relation_Type.CUSTODIAN = SUBSTRING(Thesaurus_Relation_Type.Thesaurus_Relation_Type_Key, 1, 8) FROM Thesaurus_Relation_Type INNER JOIN INSERTED ON Thesaurus_Relation_Type.Thesaurus_Relation_Type_Key = INSERTED.Thesaurus_Relation_Type_Key WHERE Thesaurus_Relation_Type.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO
--try
/****** Object:  Trigger dbo.Thesaurus_Relation_Type_UsageCustodianInsert    Script Date: 15/12/2005 13:38:59 ******/

CREATE TRIGGER Thesaurus_Relation_Type_UsageCustodianInsert ON dbo.Thesaurus_Relation_Type_Usage AFTER INSERT AS UPDATE Thesaurus_Relation_Type_Usage SET Thesaurus_Relation_Type_Usage.CUSTODIAN = SUBSTRING(Thesaurus_Relation_Type_Usage.Thesaurus_Relation_Type_Usage_Key, 1, 8) FROM Thesaurus_Relation_Type_Usage INNER JOIN INSERTED ON Thesaurus_Relation_Type_Usage.Thesaurus_Relation_Type_Usage_Key = INSERTED.Thesaurus_Relation_Type_Usage_Key WHERE Thesaurus_Relation_Type_Usage.CUSTODIAN IS NULL


GO

SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO

