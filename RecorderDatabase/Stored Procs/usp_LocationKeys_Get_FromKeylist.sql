/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
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
    $Revision: 1 $
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