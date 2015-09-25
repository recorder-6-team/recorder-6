/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[ufn_Base36Sum]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_Base36Sum
GO

/*===========================================================================*\
  Description:	Base 36 representation of the sum of two integers, one of
				which is given in its base 36 representation.

				'#ERROR!' if @operand_1 is not a valid representation of a
				non-negative integer in base 36, or if the sum would be
				negative.

  Parameters:   @operand_1				First operand (base 36 representation)
				@operand_2				Second operand

  Created:		Dec 2003

  Last revision information:
	$Revision: 4 $
	$Date: 6/05/04 11:03 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_Base36Sum(
	@operand_1		VARCHAR(16),
	@operand_2		INT)
RETURNS
	VARCHAR(16)
AS
BEGIN
	DECLARE		@sum				VARCHAR(16),
				@char_index			INT,
				@operand_1_char		CHAR,
				@temp				INT

	SELECT		@sum		=	'',
				@char_index	=	LEN(@operand_1),
				@operand_1	=	UPPER(@operand_1),
				@temp		=	@operand_2

	WHILE		@char_index > 0
	BEGIN
		SET			@operand_1_char	=	SUBSTRING(@operand_1, @char_index, 1)

		IF @operand_1_char >= '0' AND @operand_1_char <= '9'
		BEGIN
			SET			@temp	=	@temp + ASCII(@operand_1_char) - ASCII('0')
		END
		ELSE IF @operand_1_char >= 'A' AND @operand_1_char <= 'Z'
		BEGIN
			SET			@temp	=	@temp + ASCII(@operand_1_char) - ASCII('A') + 10
		END
		ELSE
		BEGIN
			RETURN		'#ERROR!'
		END

		SET			@sum	=	CASE WHEN ((36 + (@temp % 36)) % 36) < 10
									THEN CHAR(ASCII('0') + ((36 + (@temp % 36)) % 36))
									ELSE CHAR(ASCII('A') + ((36 + (@temp % 36)) % 36) - 10)
								END
								+ @sum

		SET			@temp		=	@temp / 36
									- CASE WHEN (@temp % 36) < 0 THEN 1 ELSE 0 END
		SET			@char_index	=	@char_index - 1
	END

	IF @temp < 0 OR (@temp > 0 AND LEN(@sum) = 16)
	BEGIN
		RETURN		'#ERROR!'
	END

	WHILE @temp > 0
	BEGIN
		SET			@sum	=	CASE WHEN (@temp % 36) < 10
									THEN CHAR(ASCII('0') + (@temp % 36))
									ELSE CHAR(ASCII('A') + (@temp % 36) - 10)
								END
								+ @sum

		SET			@temp	=	@temp / 36
	END

	SET			@sum	=	REPLACE(LTRIM(REPLACE(@sum, '0', ' ')), ' ', '0')
	RETURN		CASE WHEN @sum = '' THEN '0' ELSE @sum END
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_Base36Sum]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_Base36Sum'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_Base36Sum TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_Base36Sum TO [Dev - JNCC SQL]
	END
GO
