
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[FormatDatePart]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
    BEGIN
        PRINT 'Dropping function FormatDatePart'
        DROP FUNCTION [dbo].[FormatDatePart]
    END
GO

    PRINT 'Creating function FormatDatePart'
GO

    


/*
    $History: FormatDatePart.sql $
 * 
 * *****************  Version 4  *****************
 * User: Qingsun      Date: 18/11/08   Time: 15:56
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * CCN 297 : when the year is leap year, Feb will have 29 days instead of
 * 28 days.
 * 
 * *****************  Version 3  *****************
 * User: Anthonysimpson Date: 20/05/04   Time: 14:57
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * Permissions corrected.
 * 
 * *****************  Version 2  *****************
 * User: Johnvanbreda Date: 18/03/03   Time: 13:58
 * Updated in $/JNCC/Development/Build/SQL Scripts/Functions
 * JNCC600
 * Fixed old dates

    */

CREATE FUNCTION dbo.FormatDatePart(@VagueDateStart int, @VagueDateEnd int, @VagueDateType varchar(2), @ISMonth bit = 1)
RETURNS int
--
--	DESCRIPTION
--	Function to return either a year or month number given a vague date start, end and type.
--	
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@VagueDateStart		Vague date start
--	@VagueDateEnd		Vague date end
--	@VagueDateType		Vague date type
--	@ISMonth			Return month or year
--
--
--	AUTHOR:	Ben Collier, Dorset Software
--	CREATED: 05/11/2002
--

AS
BEGIN

--****************************************************************************************************
--constants
declare @D1 int
set @D1=365 							-- days in 1 year
declare @D4 int
set @D4 = @D1 * 4 + 1			-- days in 4 years, inc leap year
declare @D100 int
set @D100 = @D4 * 25 - 1 	-- days in 100 years (no leap year on 100th year)
declare @D400 int
set @D400 = @D100 * 4 + 1	-- days in 400 years - every 400 years you do  get a leap year
--variables
declare @T int
declare @Y int
declare @M int
declare @D int
declare @I int
-- get number of days since 1/1/01 
set @T = @VagueDateStart+693593
set @Y=1
-- find number of whole 400 year blocks
set @Y = @T / @D400
set @T = @T - @Y * @D400
set @Y = @Y * 400 + 1
set @I = @T / @D100
set @D = @T - @I * @D100
if @I=4 begin
  set @I = @I - 1
  set @D = @D + @D100
end
set @Y = @Y + @I * 100
set @I = @D / @D4
set @D = @D - @I * @D4
set @Y = @Y + @I * 4
set @I = @D / @D1
set @D = @D - @I * @D1
if @I = 4 begin
  set @I = @I - 1
  set @D = @D + @D1
end
set @Y=@Y + @I
if @IsMonth=1 begin
  set @M=1
  while 1=1 begin
    set @I = case @M
		when 1 then 31
		when 2 then case when( (@Y % 4 = 0 AND @Y  % 100 != 0) OR @Y  % 400 = 0) then 29 else 28 end
		when 3 then 31
		when 4 then 30
	  	when 5 then 31
		when 6 then 30
		when 7 then 31
		when 8 then 31
		when 9 then 30
		when 10 then 31
		when 11 then 30
		when 12 then 31
    end
    if @D<@I break
    set @D = @D - @I
    set @M = @M + 1
  end
  return @M
end
else
  return @Y

RETURN ''
--****************************************************************************************************

END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[FormatDatePart]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function FormatDatePart'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.FormatDatePart TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.FormatDatePart TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.FormatDatePart TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.FormatDatePart TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.FormatDatePart TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.FormatDatePart TO [Dev - JNCC SQL]
	END
GO

