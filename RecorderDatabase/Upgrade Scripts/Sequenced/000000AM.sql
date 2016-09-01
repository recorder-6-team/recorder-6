/****** Changes to stop mutiple display of 0 grid refs on import wizard matching screen  ******/

/****** Object:  StoredProcedure [dbo].[usp_IW_Imported_Insert_Locations_Grid_Ref]    Script Date: 08/27/2016 17:09:41 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO



/*===========================================================================*\
  Description:	Populate import table with matched values for unique matched only.

  Parameters:	<none>

  Created:	Nov 2015

  $Author: MikeWeideli $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_IW_Imported_Insert_Locations_Grid_Ref]
AS
	DECLARE @GridRefs varchar(100), @LocationName varchar(100)
	DECLARE @CRLF char(2)
	SET @CRLF=CHAR(13)+CHAR(10)
	
		
	DECLARE curLocations CURSOR FOR
		SELECT Import_Value FROM #Locations    
	OPEN curLocations
	FETCH NEXT FROM curLocations INTO @LocationName  
	WHILE @@Fetch_Status = 0 BEGIN
        
        SET @GridRefs = ''
       
        
          SELECT @GridRefs = CASE 
		  WHEN CharIndex(SYSTEM0100000001_Spatial_Ref, @GridRefs) = 0 AND SYSTEM0100000001_Spatial_Ref <> ''
	  	  THEN @GridRefs + @CRLF + SYSTEM0100000001_Spatial_Ref + '|' + SYSTEM0100000001_system
		   ELSE @GridRefs END
		   FROM #Master
	       WHERE SYSTEM0100000000_Data = @LocationName     -- Remove superfluous leading carriage return.
  	     
  	    
  	     	
		   
         IF SubString(@GridRefs, 1, 2) = @CRLF
	     SET @GridRefs = SubString(@GridRefs, 3, Len(@GridRefs))     -- Update field in match table.
		
         UPDATE #Locations
	     SET Import_Grid_Reference = @GridRefs
		 WHERE Import_Value = @LocationName
		
		 FETCH NEXT FROM curLocations INTO @LocationName
	END
	CLOSE curLocations
	DEALLOCATE curLocations
	

	  

