--change Report Wizard to use varchar rather than text column for sample recorders
update REPORT_FIELD
set field_type='varchar', field_size=8000
where REPORT_FIELD_KEY='NBNSYS0000000049'