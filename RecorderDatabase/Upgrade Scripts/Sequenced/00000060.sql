/*===========================================================================*\
  Description:
	Adds a special xml element 'keyword' mapping to the 'term' table. This is
	necessary as the 'term' tag is already used by the dtd.

  Created:	January 2009

  Last revision information:
    $Revision: 1 $
    $Date: 26/01/09 9:55 $
    $Author: Pauldavies $

\*===========================================================================*/

IF NOT EXISTS (SELECT 1 FROM Special_Xml_Element WHERE Name = 'keyword' AND Type = 'T')
	INSERT INTO Special_Xml_Element (
		Name,
		Type,
		Data )
	VALUES	(
		'keyword',
		'T',
		'Term' )