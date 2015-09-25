alter table Specimen
alter column Number varchar(30)
go

update IW_Column_Type
set Maximum_Length = 30 where Item_Name = 'Specimen Number'
go

update IW_Output_Field
set Data_Type = 'VARCHAR(30)' where IW_Output_Field_Key = 'SYSTEM0100000021'
go