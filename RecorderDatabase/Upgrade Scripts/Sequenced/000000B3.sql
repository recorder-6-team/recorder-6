/****** Remove UDF before recreating/


If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[LCReturnMonthEndDay]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
   DROP FUNCTION [dbo].[LCReturnMonthEndDay]

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[LCReturnDateType]') AND OBJECTPROPERTY(Id, N'IsScalarFunction') = 1)
   DROP FUNCTION [dbo].[LCReturnDateType]

GO

/****** Object:  UserDefinedFunction [dbo].[LCReturnMonthEndDay]    Script Date: 03/05/2017 19:57:57 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		Michael Weideli
-- Create date: March 2017
-- Description:	Given a month and year returns the end day 

-- =============================================
CREATE FUNCTION [dbo].[LCReturnMonthEndDay]
(@Month int, @Year int)
RETURNS int
AS
BEGIN
DECLARE
@Date as varchar(10),
@DateTime as DateTime,
@MonthEnd as DateTime

SET @Date = cast(@Year as varchar(4)) + '-' + cast(@Month as varchar(2))+ '-01'
SEt @DateTime = Convert(varchar(30),cast(@Date as datetime),102)

SET @MonthEnd = DATEADD(s,-1,DATEADD(mm, DATEDIFF(m,0,@DateTime)+1,0))

Return datepart(day,@MonthEnd)
END

GO

GRANT EXECUTE ON [dbo].[LCReturnMonthEndDay] TO PUBLIC

GO

/****** where a date type is DD returns a more appropriate date type******/

/****** Object:  UserDefinedFunction [dbo].[LCReturnDateType]    Script Date: 03/05/2017 19:50:19 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		Michael Weideli
-- Create date: March 2017
-- Description:	Take a vague date start and vague date end and return
-- a vague date type for a DD vague date
 
-- =============================================
CREATE FUNCTION [dbo].[LCReturnDateType]
(@VagueDateStart int, @VagueDateEnd int, @VagueDateType varchar(2))
RETURNS varchar(2)
AS
BEGIN
declare @StartDay integer
declare @StartMonth integer
declare @StartYear integer
declare @StartMonthEnd integer
declare @EndDay integer
declare @EndMonth integer
declare @EndYear integer
declare @DayMonthEnd integer
declare @FullDateStart varchar(10)
declare @FullDateEnd varchar(10)
declare @NuVagueDateType varchar(2) 


if @VagueDateType <> 'DD' begin
  Set @NuVagueDateType = @VagueDateType 
end else begin   
  SET @NuVagueDateType = 'DD' 
  if @VagueDateStart = @VagueDateEnd begin 
    set @NuVagueDateType = 'D'
  end else begin
    Set @FullDateStart = dbo.LCReturnDate(@VagueDateStart,@VagueDateStart,'F')     
    Set @FullDateEnd = dbo.LCReturnDate(@VagueDateEnd,@VagueDateEnd,'F')     
     
    Set @StartDay = cast(left(@FullDateStart,2) as int)  
    Set @EndDay = cast(left(@FullDateEnd,2) as int) 
    Set @StartMonth = cast(substring(@FullDateStart,4,2) as int) 
    Set @EndMonth = cast(substring(@FullDateEnd,4,2) as int) 
    Set @StartYear = cast(substring(@FullDateStart,7,4) as int) 
    Set @EndYear = cast(substring(@FullDateEnd,7,4) as int) 
    Set @DayMonthEnd = [dbo].[LCReturnMonthEndDay](@EndMonth,@EndYear)
 
    If @StartDay = 1 and @StartMonth = 1 and @EndDay = 31 and 
      @EndMonth = 12  begin
      if @StartYear = @EndYear begin
        Set @NuVagueDateType = 'Y'
      end else begin
        Set @NuVagueDateType = 'YY' 
      end  
    end else
      if @StartYear = @EndYear and  @StartDay = 1 and @EndDay = @DayMonthEnd Begin
        if @StartMonth = @EndMonth begin
          Set @NuVagueDateType = 'O'
        end else begin
          Set @NuVagueDateType = 'M'   
        end
      end    
     
 
 
  end  
end
Return  @NuVagueDateType

END

GO

GRANT EXECUTE ON [dbo].[LCReturnDateType] TO PUBLIC
