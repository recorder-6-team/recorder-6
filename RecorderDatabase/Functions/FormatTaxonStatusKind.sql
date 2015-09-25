If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[FormatTaxonStatusKind]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function FormatTaxonStatusKind'
        DROP FUNCTION [dbo].[FormatTaxonStatusKind]
    END
GO

    PRINT 'Creating function FormatTaxonStatusKind'
GO

    /*
    $History: FormatTaxonStatusKind.sql $
 * 
 * *****************  Version 2  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 14:57
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * Permissions corrected.
 * 
 * *****************  Version 1  *****************
 * User: Bencollier   Date: 7/11/02    Time: 16:31
 * Created in $/JNCC/Development/Build/SQL Scripts/Functions
 * Initial Build
    */

CREATE FUNCTION dbo.FormatTaxonStatusKind(@TaxonListItemKey char(16))
RETURNS varchar(8000)
--
--	DESCRIPTION
--	Function to return a semi-colon sperated string of all the taxon lists that the 
--	specified taxon appears in.
--	
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@TaxonListItemKey	Taxon List Item Key to perform manipulation on
--
--
--	AUTHOR:	Ben Collier, Dorset Software
--	CREATED: 07/11/2002
--

AS
BEGIN

--****************************************************************************************************
DECLARE @ReturnString varchar(8000)
DECLARE @ItemName varchar(200)

DECLARE csrStatusListName CURSOR
FOR
SELECT DISTINCT TAXON_LIST.ITEM_NAME
FROM
	(TAXON_LIST_ITEM AS TLI
	LEFT JOIN
		(TAXON_VERSION
		LEFT JOIN
			(TAXON
			LEFT JOIN
				(TAXON_VERSION AS TV2
				LEFT JOIN
					(TAXON_LIST_ITEM AS TLI2
					LEFT JOIN
						(TAXON_LIST_VERSION
						LEFT JOIN
							TAXON_LIST
						ON TAXON_LIST_VERSION.TAXON_LIST_KEY = TAXON_LIST.TAXON_LIST_KEY)
					ON TLI2.TAXON_LIST_VERSION_KEY = TAXON_LIST_VERSION.TAXON_LIST_VERSION_KEY)
				ON TV2.TAXON_VERSION_KEY = TLI2.TAXON_VERSION_KEY)
			ON TAXON.TAXON_KEY = TV2.TAXON_KEY)
		ON TAXON_VERSION.TAXON_KEY = TAXON.TAXON_KEY)
	ON TLI.TAXON_VERSION_KEY = TAXON_VERSION.TAXON_VERSION_KEY) 
where TLI.taxon_list_item_key = @TaxonListItemKey

OPEN csrStatusListName

FETCH NEXT FROM csrStatusListName INTO @ReturnString
WHILE @@FETCH_STATUS = 0
BEGIN
	FETCH NEXT FROM csrStatusListName INTO @ItemName
	IF @@FETCH_STATUS = 0 SELECT @ReturnString = @ReturnString + ';' + @ItemName
END

CLOSE csrStatusListName
DEALLOCATE csrStatusListName

RETURN @ReturnString
--****************************************************************************************************

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[FormatTaxonStatusKind]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function FormatTaxonStatusKind'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.FormatTaxonStatusKind TO [Dev - JNCC SQL]
	END
GO
