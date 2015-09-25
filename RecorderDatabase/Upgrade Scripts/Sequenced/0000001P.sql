--	
--	Script to alter the table Survey_event_owner so that the
--	ENTRY_DATE and CHANGED_DATE columns are datetime type not shortdate
--
--	Written by KJG
--	on 3/12/2004
--

--	FIRST THE entry_date
--	First drop the entry date constraint

alter table survey_event_owner
drop constraint DF_SURVEYE_EVENT_OWNERSHIP_ENTRY_DATE
-- then change data type
alter table survey_event_owner
alter column entry_date datetime
--	then re-add the constraint
alter table survey_event_owner
add constraint DF_SURVEYE_EVENT_OWNERSHIP_ENTRY_DATE default getdate() for entry_date

--	NOW THE Changed_Date
alter table survey_event_owner
alter column changed_date datetime
