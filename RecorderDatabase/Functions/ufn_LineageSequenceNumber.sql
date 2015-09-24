/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_LineageSequenceNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_LineageSequenceNumber
GO

/*===========================================================================*\
  Description:	Sequence number from the last level of the given lineage.

  Parameters:   @lineage				Lineage

  Created:		Jan 2004

  Last revision information:
	$Revision: 4 $
	$Date: 6/05/04 11:02 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_LineageSequenceNumber(
	@lineage			VARCHAR(900))
RETURNS
	VARCHAR(8)
AS
BEGIN
	DECLARE     @len		INT,
				@result		VARCHAR(8)

	SET			@len		=	CHARINDEX('\', REVERSE(@lineage)) - 1

	IF @len < 0
	BEGIN
		SET			@len		=	LEN(@lineage)
	END

	RETURN		RIGHT(@lineage, @len)
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_LineageSequenceNumber]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_LineageSequenceNumber'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_LineageSequenceNumber TO [Dev - JNCC SQL]
	END
GO
