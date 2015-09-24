/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_IWMatchNewEntry_Name]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_IWMatchNewEntry_Name]
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

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_IWMatchNewEntry_Name]
	@ImportValue varchar(100),
	@EnteredBy char(16)
AS
	DECLARE @Key char(16),
		@Surname varchar(30),
		@Forename varchar(20),
		@Initials varchar(8),
		@Title varchar(4),
		@Working varchar(100),
		@Part varchar(50),
		@CurrentPos int,
		@NextPos int

	/*===================================================*\
	  Break down the ImportValue into name constituents.
	\*===================================================*/
	SET	@Surname = ''
	SET	@Working = @ImportValue
	SET 	@CurrentPos = 1
	SET	@NextPos = 1

	-- Rule 1. Check for 'xxx' or 'van xxx'
	IF CharIndex('.', @Working) = 0 BEGIN
		SET @NextPos = CharIndex(' ', @Working)
		IF @NextPos = 0
			SET @Surname = @Working
		ELSE BEGIN
			SET @Surname = SubString(@Working, @CurrentPos, @NextPos - 1)
			IF @Surname IN ('van', 'von') BEGIN
				SET @NextPos = CharIndex(' ', @Working, @NextPos + 1)
				IF @NextPos = 0
					SET @Surname = @Working
				ELSE
					SET @Surname = ''
			END ELSE
				SET @Surname = ''
		END
		IF @Surname <> ''
			SET @Working = ''
	END

	-- Rule 2. Check for 'xxx, ###' or 'van xxx, ###'
	IF (@Surname = '') BEGIN
		SET @NextPos = CharIndex(',', @Working)
		IF @NextPos <> 0 BEGIN
			SET @Surname = SubString(@Working, 1, @NextPos - 1)
			SET @Working = LTrim(SubString(@Working, @NextPos + 1, Len(@Working)))
		END
	END

	-- Rule 3. Check for '### xxx'/'###.xxx' or '### van xxx'/'###.van xxx'
	IF (@Surname = '') AND (Right(@Working, 1) <> '.') BEGIN
		SET @Working = Reverse(@Working)
		SET @NextPos = CharIndex(' ', @Working)
		IF (@NextPos = 0) OR (CharIndex('.', SubString(@Working, 1, @NextPos - 1)) <> 0)
			SET @NextPos = CharIndex('.', @Working)
		-- Surname is last word.
		SET @Surname = Reverse(SubString(@Working, 1, @NextPos - 1))
		SET @CurrentPos = @NextPos + 1
		-- Check for 'van/von' too
		SET @NextPos = CharIndex(' ', @Working, @CurrentPos)
		IF (@NextPos = 0) OR (CharIndex('.', SubString(@Working, @CurrentPos, @NextPos - 1)) <> 0)
			SET @NextPos = CharIndex('.', @Working, @CurrentPos)
		SET @Part = RTrim(SubString(@Working, @CurrentPos, Abs(@CurrentPos - @NextPos)))
		IF Reverse(@Part) IN ('van', 'von') BEGIN
			SET @Surname = LTrim(Reverse(SubString(@Working, 1, @NextPos - 1)))
			SET @CurrentPos = @NextPos + 1
		END

		SET @Working = Reverse(LTrim(SubString(@Working, @CurrentPos, Len(@Working))))
	END

	-- Rule 4. Check for 'xxx #.'/'xxx ###.' or 'van xxx #.'/'van xxx ###.'
	IF @Surname = '' BEGIN
		SET @NextPos = CharIndex(' ', @Working)
		IF @NextPos=0 BEGIN
			RAISERROR('No surname', 16, 1)
			GOTO JustExit
		END
		SET @Surname = SubString(@Working, 1, @NextPos - 1)
		IF @Surname IN ('van', 'von') BEGIN
			SET @NextPos = CharIndex(' ', @Working, @NextPos + 1)
			SET @Surname = SubString(@Working, 1, @NextPos - 1)
		END

		SET @Working = SubString(@Working, @NextPos + 1, Len(@Working))
	END

	-- Parse remainder into Forename, Initials and Title
	SET @Part     = '';
	SET @Forename = '';
	SET @Initials = '';
	SET @Title    = '';
	SET @CurrentPos = 1

	WHILE @CurrentPos <= Len(@Working) BEGIN
		IF SubString(@Working, @CurrentPos, 1) IN (' ', '.') BEGIN
			IF Len(@Part) = 1
				SET @Initials = @Initials + @Part + '.'
			ELSE
			IF @Part IN ('mr', 'mrs' ,'ms' ,'miss' ,'dr' ,'sir' ,'lord' ,'rev' ,'col')
				SET @Title = @Part
			ELSE
				SET @Forename = RTrim(LTrim(@Forename + ' ' + @Part))
			SET @Part = ''
		END ELSE
			SET @Part = @Part + SubString(@Working, @CurrentPos, 1)
	
		SET @CurrentPos = @CurrentPos + 1
	END

	IF @Part <> ''  
		IF Len(@Part) = 1
			SET @Initials = @Initials + @Part + '.'
		ELSE
		IF @Part IN ('mr', 'mrs' ,'ms' ,'miss' ,'dr' ,'sir' ,'lord' ,'rev' ,'col')
			SET @Title = @Part
		ELSE
			SET @Forename = RTrim(LTrim(@Forename + ' ' + @Part))

	IF @Forename = '' SET @Forename = NULL
	IF @Initials = '' SET @Initials = NULL
	IF @Title = '' SET @Title = NULL

	/*===================================================*\
	  Now create new Name and Individual records
	\*===================================================*/
	EXECUTE spNextKey 'Name', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO [Name] (	
			Name_Key, Organisation, Entered_By
		) VALUES (
			@Key, 0, @EnteredBy
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		INSERT INTO Individual (
			Name_Key, Surname, Forename, Initials, Title, Entered_By
		) VALUES (
			@Key, @Surname, @Forename, @Initials, @Title, @EnteredBy
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- And update import table with new data.
		UPDATE	#Names
		SET	Match_Value = dbo.ufn_GetFormattedName(@Key),
			Match_Key = @Key,
			Match_Count = 1,
			Manual_Match = 1,
			Remembered = 0
		WHERE	Import_Value = @ImportValue
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

JustExit:
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_IWMatchNewEntry_Name') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_IWMatchNewEntry_Name'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Name TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Name TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_IWMatchNewEntry_Name TO [Dev - JNCC SQL]
END
GO