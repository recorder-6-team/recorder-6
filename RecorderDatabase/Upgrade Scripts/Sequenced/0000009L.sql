SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Mantis 234: Truncation of field in LCFormatMeasureMent   
\*============================================================================*/


/* 
 * *****************  Version 2  *****************
 * User: Mike Weideli
 * Created by Littlefield Consultancy 
  Modified for Mantis 234 - 3 October 2014 

    */

ALTER FUNCTION [dbo].[LCFormatMeasurement]
(@MUnit varchar(40), @MQual varchar(40), @Mdata varchar(20))
RETURNS varchar(110)
--
--	DESCRIPTION
--	Function to return a the Measurement formatted as a string
--	
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@MUnit			Short name for Unit
--	@MQualifier		Short name for Qualifier
--	@MData 		Actual Data
--	CREATED: 25/12/2005
--

AS
BEGIN


--****************************************************************************************************
DECLARE @NuQual varchar(80)
DECLARE @NuUnit varchar(40)
DECLARE @RETURNDATA varchar (110)
set @NUUnit = @MUnit
set @NuQual = @MQual
set @RETURNDATA = ''
if @NuUnit = 'None'
begin 
  set @NuUnit = ''
end
if @NuQual = 'None' or  CAST(@Mdata as varchar) = @NuQual 
begin 
  set  @NuQual = ''
end  
else
  set  @NuQual = ' of '  + @NuQual 
   

IF @MData is not NULL
begin

   set @RETURNDATA =   @MData + ' ' + @NuUnit + @NuQual
end 

RETURN @RETURNDATA

END
