�
 TDMNBNXML 0  TPF0�	TdmNBNXMLdmNBNXMLOldCreateOrder	LeftTop� HeightWidthN 
TJNCCQueryqrySpecialXMLElementsCommandTimeout 
Parameters SQL.Strings!select * from SPECIAL_XML_ELEMENT  ParseSQL	Left$Top  
TJNCCQueryqryLocalDTDTextCommandTimeout 
Parameters SQL.Stringsselect * from DTD_FRAGMENT ParseSQL	Left$Top`  
TJNCCQueryqrySpecialTablesCommandTimeout 
Parameters SQL.Strings!select * from SPECIAL_XML_ELEMENTwhere type='T' ParseSQL	Left� Top  
TJNCCQueryqryFindExistingFreeTermCommandTimeout 
ParametersName
short_nameSize�Value   SQL.Stringsselect * from  where short_name = :short_name ParseSQL	Left� Top`  
TJNCCQueryqryTermListDescCommandTimeout 
ParametersNametableSize�Value   SQL.Strings!select description from term_listwhere table=:table ParseSQL	LeftTop  
TJNCCQueryqryRelatedOccurrencesCommandTimeout 
Parameters ParseSQL	Left,Top�   
TJNCCQueryqryListItemCommandTimeout 
Parameters ParseSQL	Left� Top�    