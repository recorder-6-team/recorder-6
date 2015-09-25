/*===========================================================================*\
  Description:	Checks to see if the Record_Type table has a record called
		'Field Record'. If it does, it updates all of the records
		in the Taxon_Occurrence table that have Null as their
		Record_Type_Key, to use the 'Field Record' Record_Type_Key.
		This is in response to ID6930, but John told me to do
		an upgrade script for JNCC.

  Created:	August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/04 11:49 $
    $Author: Anthonysimpson $

\*===========================================================================*/
DECLARE @RecordTypeKey char(16)

SELECT 	@RecordTypeKey = Record_Type_Key
FROM	Record_Type
WHERE	Short_Name LIKE 'Field Record'

IF @RecordTypeKey IS NOT NULL
	UPDATE 	Taxon_Occurrence
	SET	Record_Type_Key = @RecordTypeKey
	WHERE	Record_Type_Key IS NULL 