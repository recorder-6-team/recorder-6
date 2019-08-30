/****** SQL FOR CHANGES IN WORKING OF Inactive Locations ******/


ALTER TABLE IW_MATCH_RULE
ADD  
  New_entry_Procedure_Multiple varchar(50)

GO


Update IW_Match_Rule set  New_entry_Procedure_Multiple =  New_entry_Procedure


GO


/****** Object:  StoredProcedure [dbo].[usp_IWMatchNewEntry_Name]    Script Date: 08/29/2019 16:40:45 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


/*===========================================================================*\
  Description:	Create a new individual from an import value mutiple entries.

  Parameters:	
	@ImportValue	The raw name to parse and insert in database.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 22/02/06 10:58 $
    $Author: Johnvanbreda $
    Changed by M Weideli to bring in line with the
    parsing of names used in IW processing. Also not to save the records if they
    are not sufficiently complete. 
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_Name_Multiple]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
		
   UPDATE #Names set Title  = dbo.ufn_IWParseImportValue(Import_Value,1)
   UPDATE #Names set Forename  = dbo.ufn_IWParseImportValue(Import_Value,2)
   UPDATE #Names set Initials  = dbo.ufn_IWParseImportValue(Import_Value,3)
   UPDATE #Names set Surname  = dbo.ufn_IWParseImportValue(Import_Value,4)

   EXECUTE spNextKey 'Name', @Key OUTPUT

   
   INSERT INTO [Name] (	
		Name_Key, Organisation, Entered_By,Entry_Date
				) 
		SELECT @Key,0,@EnteredBy,GETDATE()
		FROM #Names WHERE 
		LEN(Surname) > 2 AND (Initials IS NOT NULL 
	    OR ForeName is not null) AND IMPORT_VALUE = @ImportValue
	    AND NOT EXISTS (SELECT * FROM INDIVIDUAL I WHERE
	    dbo.ufn_CompareNames(#Names.Title,#Names.Forename,#Names.Initials,
	    #Names.Surname,I.Title,I.FORENAME,I.INITIALS,I.SURNAME) > 13) 
	    
	    INSERT INTO Individual (
			Name_Key, Title, Forename, Initials, Surname, Entered_By,Entry_Date
		) 
		SELECT @Key,Title,Forename,Initials,Surname,@enteredBy,GETDATE()
		FROM #Names WHERE IMPORT_VALUE = @ImportValue AND 
		EXISTS (SELECT * FROM NAME WHERE NAME.NAME_KEY = @Key)       	    	
   
		UPDATE	#Names
		SET	Match_Value = dbo.ufn_GetFormattedName(@Key),
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
			
	    WHERE Import_Value = @ImportValue
        AND EXISTS(SELECT * FROM INDIVIDUAL WHERE 
        NAME_KEY = @Key)  	  

        UPDATE	#Names
		SET	Notes = 'Add failed. Exists or insufficient info.'
	    WHERE Import_Value = @ImportValue
        AND NOT EXISTS(SELECT * FROM INDIVIDUAL WHERE 
        NAME_KEY = @Key)  


GO

GRANT EXECUTE ON [dbo].[usp_IWMatchNewEntry_Name_Multiple] TO PUBLIC

GO


/*===========================================================================*\
  Description:	Create a new individual from an import value.

  Parameters:	
	@ImportValue	The raw name to parse and insert in database.
	@EnteredBy

  Created:	June 2004

  Last revision information:
    $Revision: 7 $
    $Date: 22/02/06 10:58 $
    $Author: Johnvanbreda $
    Changed by M Weideli to allow the creation of any name where 
    the user wishes to do so. Assumes the users know if the name shodul be created.   
\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchNewEntry_Name]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16)
		
   UPDATE #Names set Title  = dbo.ufn_IWParseImportValue(Import_Value,1)
   UPDATE #Names set Forename  = dbo.ufn_IWParseImportValue(Import_Value,2)
   UPDATE #Names set Initials  = dbo.ufn_IWParseImportValue(Import_Value,3)
   UPDATE #Names set Surname  = dbo.ufn_IWParseImportValue(Import_Value,4)

   EXECUTE spNextKey 'Name', @Key OUTPUT

   
   INSERT INTO [Name] (	
	    Name_Key, Organisation, Entered_By,Entry_Date
				) 
        	SELECT @Key,0,@EnteredBy,GETDATE()
		FROM #Names WHERE LEN(Surname) > 1 AND 
		IMPORT_VALUE = @ImportValue
	        
	    INSERT INTO Individual (
			Name_Key, Title, Forename, Initials, Surname, Entered_By,Entry_Date
		) 
		SELECT @Key,Title,Forename,Initials,Surname,@enteredBy,GETDATE()
		FROM #Names WHERE IMPORT_VALUE = @ImportValue AND 
		EXISTS (SELECT * FROM NAME WHERE NAME.NAME_KEY = @Key)       	    	
   
		UPDATE	#Names
		SET	Match_Value = dbo.ufn_GetFormattedName(@Key),
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
			
	    WHERE Import_Value = @ImportValue
        AND EXISTS(SELECT * FROM INDIVIDUAL WHERE 
        NAME_KEY = @Key)  	  

   


GO

 
Update IW_MATCH_RULE SET New_entry_Procedure_Multiple = 'usp_IWMatchNewEntry_Name_Multiple' WHERE IW_MATCH_RULE_KEY = 'SYSTEM0100000000'
   

GO

/****** Object:  StoredProcedure [dbo].[usp_IWMatchRule_Select]    Script Date: 08/29/2019 23:14:14 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:	returns details of a match rule

  Parameters:	@Key - IW_Match_Rule_Key

  Created:	June 2004

  Last November 2018

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IWMatchRule_Select]
	@Key CHAR(16)
AS
	SELECT 	[Sequence], 
		Item_Name, 
		Control_Type,
		Imported_Data_Insert_Sql,
		Remembered_Matches_Procedure,
		Match_Procedure,
		Record_Matches_Procedure,
		New_Entry_Procedure,
		Requires_Checklist,
		Set_Match_Procedure,
		Table_Create_Sql,
		Key_To_Caption_Procedure,
		Search_Type,
		Checklists_Select_Procedure,
		Termlist_Select_Procedure,
                Exclude_Unmatched_Procedure,
        Update_Notes_Procedure,
	    Display_Notes_Procedure,
	    Detailed_Notes_Procedure,
	    New_Entry_Procedure_Multiple
	FROM 	IW_Match_Rule
	WHERE 	IW_Match_Rule_Key = @Key
	ORDER BY [Sequence]

