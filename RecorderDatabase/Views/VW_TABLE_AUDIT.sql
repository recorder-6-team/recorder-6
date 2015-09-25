if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[VW_AUDITLOG]') )
 drop view dbo.VW_AUDITLOG
GO


create view dbo.VW_AUDITLOG as
--
--	Table				Linked Table
--
--	Address				Name
--	Biotope				Biotope_List_Item
--	Biotope_Determination		Biotope_Occurrence
--	Biotope_Fact			Biotope_List_Item
--	Biotope_List_Item	
--	Biotope_Occurrence	
--	Biotope_Occurrence_Data		Biotope_Occurrence
--	Biotope_Occurrence_Sources	Biotope_Occurrence
--	Biotope_Sources			Biotope_List_Item
--	Location_Boundary		Location
--	Communication			Name
--	Contact_Number			Name
--	Grid_Square			Location
--	Individual	
--	Land_Parcel			Location
--	Location	
--	Location_Admin_Area		Location
--	Location_Boundary		Location
--	Location_Data			Location
--	Location_Designation		Location
--	Location_Relation		Location
--	Location_Sources		Location
--	Location_Use			Location
--	Name_Relation			Name
--	Names_Sources			Name
--	Organisation	
--	Reference	
--	Reference_Author		Reference
--	Sample	
--	Sample_Data			Sample
--	Sample_Recorder			Sample
--	Sample_Relation			Sample
--	Sample_Sources			Sample
--	Specimen			Taxon_Occurrence
--	Survey	
--	Survey_Event	
--	Survey_Event_Owner		Survey_Event
--	Survey_Event_Recorder		Survey_Event
--	Survey_Sources 			Survey
--	Taxon				Taxon_List_Item
--	Taxon_Designation		Taxon_List_Item
--	Taxon_Determination		Taxon_Occurrence
--	Taxon_Fact			Taxon_List_Item
--	Taxon_List_Item	
--	Taxon_Occurrence	
--	Taxon_Occurrence_Data		Taxon_Occurrence
--	Taxon_Occurrence_Relation	Taxon_Occurrence
--	Taxon_Occurrence_Sources	Taxon_Occurrence
--	Taxon_Sources			Taxon_List_Item
--	Taxon_User_Name			Taxon_List_Item
--	Taxon_Version			Taxon_List_Item
--	Tenure				Location

	select 'Address' as BaseTable,
		Address_key as recordkey,
		'Name' as navigatetable,
		Name_key as navigatekey,
		dbo.ufn_GetFormattedName( Name_key ) + ISNULL(', ' + Address_1, '') +ISNULL(', ' + Address_Postcode, '') as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Address mt
union
	select 'Biotope',
		Biotope_key as recordkey,
		'Biotope_list_item' as navigatetable,
		Biotope_key as navigatekey,
		short_term as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Biotope mt
union
	select 'biotope_determination',
		biotope_determination_key as recordkey,
		'biotope_occurrence' as navigatetable,
		biotope_occurrence_key as navigatekey,
		short_term as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from biotope_determination mt
	join biotope_list_item bli on bli.biotope_list_item_key = mt.biotope_list_item_key
	join biotope b on b.biotope_key = bli.biotope_key
	where mt.preferred = 1

union
	select 'Biotope_fact' as [Table],
		Biotope_fact_key as recordkey,
		'Biotope_list_item' as navigatetable,
		Biotope_key as navigatekey,
		Title + ISNULL(', ' + left(dbo.ufn_RtfToPlainText([Data]),20), '') as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Biotope_fact mt

union
	select 'biotope_list_item',
		biotope_list_item_key as recordkey,
		'biotope_list_item' as navigatetable,
		biotope_list_item_key as navigatekey,
		short_term as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from biotope_list_item mt
	join biotope b on b.biotope_key = mt.biotope_key

union
	select 'biotope_occurrence',
		mt.biotope_occurrence_key as recordkey,
		'biotope_occurrence' as navigatetable,
		mt.biotope_occurrence_key as navigatekey,
		b.short_term as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from biotope_occurrence mt
	join biotope_determination bd on bd.biotope_occurrence_key = mt.biotope_occurrence_key and bd.preferred = 1
	join biotope_list_item bli on bli.biotope_list_item_key = bd.biotope_list_item_key
	join biotope b on b.biotope_key = bli.biotope_key
union
	select 'biotope_occurrence_data',
		biotope_occurrence_data_key as recordkey,
		'biotope_occurrence' as navigatetable,
		biotope_occurrence_key as navigatekey,
		[data] + ' ' + mu.short_name + ' ' + mq.short_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from biotope_occurrence_data mt
	join measurement_Qualifier mq on mq.measurement_qualifier_key = mt.measurement_qualifier_key
	join measurement_unit mu on mu.measurement_unit_key = mt.measurement_unit_key


union
	select 'communication',
		communication_key as recordkey,
		'Name' as navigatetable,
		Name_key_1 as navigatekey,
		communication_type + ISNULL(', ' + dbo.ufn_GetFormattedName(Name_Key_1), '') as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from communication mt
union
	select 'contact_number',
		contact_number_key as recordkey,
		'Name' as navigatetable,
		Name_key as navigatekey,
		dbo.ufn_GetFormattedName(Name_Key) as RecordData,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entry_Date else Changed_Date END as EditDate,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entered_by else Changed_By END as Editor
	from contact_number mt
union
--	note the extra join
	select 'grid_square',
		grid_square_key as recordkey,
		'location' as navigatetable,
		mt.location_key as navigatekey,
		spatial_ref + ISNULL(', ' + convert( varchar(100), [size]),'') + ISNULL(', ' + convert(varchar(100),Lat),'') + ISNULL(', ' + convert(varchar(100),long), '') as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from grid_square mt
union
	select 'Individual' as [Table],
		Name_key as recordkey,
		'Individual' as navigatetable,
		Name_key as navigatekey,
		dbo.ufn_GetFormattedName( Name_key ) as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Individual MT
union
	select 'land_parcel',
		land_parcel_key as recordkey,
		'location' as navigatetable,
		mt.location_key as navigatekey,
		Land_parcel_number as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from land_parcel mt
	left outer join location l on l.location_key = mt.location_key
union
	select 'location',
		mt.location_key as recordkey,
		'location' as navigatetable,
		mt.location_key as navigatekey,
		ln.item_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from location mt
	join location_name ln on ln.location_key = mt.location_key and preferred = 1
union
	select 'location_boundary',
		location_boundary_key as recordkey,
		'location' as navigatetable,
		mt.location_key as navigatekey,
		ln.item_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from location_boundary mt
	join location_name ln on ln.location_key = mt.location_key and preferred = 1
union
	select 'location_data',
		location_data_key as recordkey,
		'location' as navigatetable,
		mt.location_key as navigatekey,
		[data] + ISNULL(' ' + mu.short_name,'') + ISNULL(' ' + mq.short_name,'') + ISNULL(', ' + left(dbo.ufn_RtfToPlainText(L.Description),20),'') as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from location_data mt
	join measurement_Qualifier mq on mq.measurement_qualifier_key = mt.measurement_qualifier_key
	join measurement_unit mu on mu.measurement_unit_key = mt.measurement_unit_key
	join location L on L.location_key = mt.location_key
union
	select 'Location_relation' as [Table],
		Location_relation_key as recordkey,
		'Location' as navigatetable,
		mt.location_key_1 as navigatekey,
		'Between ''' + ln.item_name + ''' And ''' + ln2.item_name +'''' as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Location_relation mt
	join location_name ln on ln.location_key = mt.location_key_1 and ln.preferred = 1
	join location_name ln2 on ln2.location_key = mt.location_key_2 and ln2.preferred = 1

union
	select 'Location_feature' as [Table],
		location_feature_key as recordkey,
		'Location' as navigatetable,
		mt.location_key as navigatekey,
		item_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Location_feature mt
	join location L on L.location_key = mt.location_key
union
	select 'Location_Use' as [Table],
		Location_Use_key as recordkey,
		'Location' as navigatetable,
		mt.location_key as navigatekey,
		[Location_Use] as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Location_Use mt
	join location L on L.location_key = mt.location_key
union
	select 'Name_relation',
		Name_key_1 as recordkey,
		'name' as navigatetable,
		name_key_1 as navigatekey,
		'Between ''' + dbo.ufn_GetFormattedName( name_key_1)  + ''' And ''' + dbo.ufn_GetFormattedName( name_key_2) +'''' as RecordData,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entry_Date else Changed_Date END as EditDate,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entered_by else Changed_By END as Editor
	from [name_relation]
union
	select 'Organisation',
		name_key as recordkey,
		'Organisation' as navigatetable,
		name_key as navigatekey,
		full_name as RecordData,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entry_Date else Changed_Date END as EditDate,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entered_by else Changed_By END as Editor
	from [Organisation]
union
	select 'Reference',
		source_key as recordkey,
		'Reference' as navigatetable,
		source_key as navigatekey,
		left(dbo.ufn_RtfToPlainText([Title]),40) as RecordData,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entry_Date else Changed_Date END as EditDate,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entered_by else Changed_By END as Editor
	from reference mt
union
	select 'Reference_author',
		author_key as recordkey,
		'Reference' as navigatetable,
		mt.source_key as navigatekey,
		initials + ISNULL(' ' +item_name,'') + ISNULL(', ' + left(dbo.ufn_RtfToPlainText(r.Title),40),'')  as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Reference_author mt
	join reference r on r.source_key = mt.source_key
union
	select 'Sample' as [Table],
		Sample_key as recordkey,
		'location' as navigatetable,
		mt.location_key as navigatekey,
		mt.spatial_ref as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from [Sample] MT
	join location l on l.location_key = mt.location_key
union
	select 'specimen',
		specimen_key as recordkey,
		'taxon_occurrence' as navigatetable,
		mt.taxon_occurrence_key as navigatekey,
		left(Location,20) +ISNULL(', ' + tli.preferred_name, '') as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from specimen mt
	join taxon_determination td on td.taxon_occurrence_key = mt.taxon_occurrence_key and td.preferred = 1
	join index_taxon_name tli on tli.taxon_list_item_key = td.taxon_list_item_key
union
	select 'survey',
		survey_key as recordkey,
		'survey' as navigatetable,
		survey_key as navigatekey,
		left(item_name,20) as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from survey mt
union
	select 'Survey_event' as [Table],
		Survey_event_key as recordkey,
		'Survey' as navigatetable,
		mt.Survey_key as navigatekey,
		mt.spatial_ref +ISNULL(', ' + left(item_name,20),'') as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Survey_event MT
	join Survey s on s.Survey_key = mt.location_key

union
	select 'Survey_event_owner' as [Table],
		Survey_event_owner_key as recordkey,
		'Survey_event' as navigatetable,
		mt.Survey_event_key as navigatekey,
		dbo.ufn_GetFormattedName( mt.name_key ) +ISNULL(', ' + left(location_name,20), '') as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Survey_event_owner MT
	join Survey_event s on s.Survey_key = mt.survey_event_key

union
	select 'Survey_event_recorder' as [Table],
		SE_recorder_key as recordkey,
		'Survey_event' as navigatetable,
		mt.Survey_event_key as navigatekey,
		dbo.ufn_GetFormattedName( mt.name_key ) +ISNULL(', ' + left(location_name,20), '') as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Survey_event_recorder MT
	join Survey_event s on s.Survey_key = mt.survey_event_key
union
	select 'taxon',
		taxon_key as recordkey,
		'taxon_list_item' as navigatetable,
		taxon_key as navigatekey,
		item_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from taxon mt
union
	select 'taxon_designation',
		taxon_designation_key as recordkey,
		'taxon_list_item' as navigatetable,
		mt.taxon_list_item_key as navigatekey,
		tdt.short_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from taxon_designation mt
	join taxon_designation_type tdt on tdt.taxon_designation_type_key = mt.taxon_designation_type_key

union
	select 'taxon_determination',
		taxon_determination_key as recordkey,
		'taxon_occurrence' as navigatetable,
		taxon_occurrence_key as navigatekey,
		tli.preferred_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from taxon_determination mt
	join index_taxon_name tli on tli.taxon_list_item_key = mt.taxon_list_item_key
	where mt.preferred = 1

union
	select 'taxon_fact',
		taxon_fact_key as recordkey,
		'taxon_list_item' as navigatetable,
		tv.taxon_key as navigatekey,
		title as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from taxon_fact mt
	join taxon_version tv on tv.taxon_version_key = mt.taxon_version_key

union
	select 'taxon_list_item',
		mt.taxon_list_item_key as recordkey,
		'taxon_list_item' as navigatetable,
		mt.taxon_list_item_key as navigatekey,
		itn.preferred_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from taxon_list_item mt
	join index_taxon_name itn on itn.taxon_list_item_key = mt.taxon_list_item_key
--	join taxon_version tv on tv.taxon_version_key = mt.taxon_version_key
--	join taxon t on tv.taxon_key = tv.taxon_key

union
	select 'taxon_occurrence',
		mt.taxon_occurrence_key as recordkey,
		'taxon_occurrence' as navigatetable,
		mt.taxon_occurrence_key as navigatekey,
		itn.preferred_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from taxon_occurrence mt
	join taxon_determination td on td.taxon_occurrence_key = mt.taxon_occurrence_key  and td.preferred = 1
	join index_taxon_name itn on itn.taxon_list_item_key = td.taxon_list_item_key

union
	select 'taxon_occurrence_data',
		taxon_occurrence_data_key as recordkey,
		'taxon_occurrence' as navigatetable,
		mt.taxon_occurrence_key as navigatekey,
		data collate SQL_LATIN1_GENERAL_CP1_CI_AS + ' ' + mu.short_name + ' ' + mq.short_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from taxon_occurrence_data mt
	join measurement_Qualifier mq on mq.measurement_qualifier_key = mt.measurement_qualifier_key
	join measurement_unit mu on mu.measurement_unit_key = mt.measurement_unit_key
	join taxon_occurrence L on L.taxon_occurrence_key = mt.taxon_occurrence_key
union
	select 'taxon_occurrence_relation',
		taxon_occurrence_relation_key as recordkey,
		'taxon_occurrence' as navigatetable,
		taxon_occurrence_key_1 as navigatekey,
		'Between ''' + itn.preferred_name  + ''' And ''' + itn2.preferred_name +'''' as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from [taxon_occurrence_relation] mt
	join taxon_determination td on td.taxon_occurrence_key = mt.taxon_occurrence_key_1 and td.preferred = 1
	join index_taxon_name itn on itn.taxon_list_item_key = td.taxon_list_item_key
	join taxon_determination td2 on td2.taxon_occurrence_key = mt.taxon_occurrence_key_2 and td2.preferred = 1
	join index_taxon_name itn2 on itn2.taxon_list_item_key = td2.taxon_list_item_key
union
	select 'taxon_user_name',
		taxon_user_name_key as recordkey,
		'taxon_list_item' as navigatetable,
		mt.taxon_list_item_key as navigatekey,
		item_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from taxon_user_name mt
	join taxon_list_item tli on tli.taxon_list_item_key = mt.taxon_list_item_key
union
	select 'taxon_version',
		mt.taxon_version_key as recordkey,
		'taxon_list_item' as navigatetable,
		mt.taxon_key as navigatekey,
		item_name as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from taxon_version mt
	join taxon t on t.taxon_key = mt.taxon_key
union
	select 'tenure',
		mt.tenure_key as recordkey,
		'tenure' as navigatetable,
		mt.tenure_key as navigatekey,
		tt.short_name + ISNULL(', ' +left(dbo.ufn_RtfToPlainText(l.Description),40), '') as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from tenure mt
	join location L on L.location_key = mt.location_key
	join tenure_type tt on tt.tenure_type_key = mt.tenure_type_key
		

/*union
	select 'location_admin_areas',
		location_admin_areas_key as recordkey,
		'location' as navigatetable,
		mt.location_key as navigatekey,
		left(dbo.ufn_RtfToPlainText([Description]),40) as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from location_admin_areas mt
	left outer join location l on l.location_key = mt.location_key
union

	select 'location_admin_areas',
		location_admin_areas_key as recordkey,
		'location' as navigatetable,
		mt.location_key as navigatekey,
		left(dbo.ufn_RtfToPlainText([Description]),40) as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from location_admin_areas mt
	left outer join location l on l.location_key = mt.location_key
union

	select 'biotope_occurrence_sources',
		source_key as recordkey,
		'biotope_occurrence' as navigatetable,
		biotope_occurrence_key as navigatekey,
		source_key as RecordData,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entry_Date else Changed_Date END as EditDate,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entered_by else Changed_By END as Editor
	from biotope_occurrence_sources mt
union

	select 'biotope_sources',
		biotope_sources_key as recordkey,
		'biotope_list_item' as navigatetable,
		biotope_list_item_key as navigatekey,
		source_key as RecordData,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entry_Date else Changed_Date END as EditDate,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(Entry_Date,0)>ISNULL(Changed_Date,0) then Entered_by else Changed_By END as Editor
	from biotope_sources mt
union
	select 'Location_Sources' as [Table],
		location_Sources_key as recordkey,
		'Location' as navigatetable,
		mt.location_key as navigatekey,
		item_name +  ', ' + left(dbo.ufn_RtfToPlainText(L.Description),20) as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Location_Sources mt
	join location L on L.location_key = mt.location_key
union
	select 'sample_data',
		sample_data_key as recordkey,
		'sample' as navigatetable,
		mt.sample_key as navigatekey,
		[data] + ' ' + mu.short_name + ' ' + mq.short_name + ', sample:- ' + spatial_ref as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from sample_data mt
	join measurement_Qualifier mq on mq.measurement_qualifier_key = mt.measurement_qualifier_key
	join measurement_unit mu on mu.measurement_unit_key = mt.measurement_unit_key
	join sample s on s.sample_key = mt.sample_key
union
	select 'Sample_relation' as [Table],
		Sample_relation_key as recordkey,
		'Sample' as navigatetable,
		mt.Sample_key_1 as navigatekey,
		'Between ''' + left(dbo.ufn_RtfToPlainText(L.comment),20) + ''' And ''' + left(dbo.ufn_RtfToPlainText(L2.comment),20) +'''' as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from Sample_relation mt
	join [Sample] L on L.Sample_key = mt.Sample_key_1
	join [Sample] L2 on L2.Sample_key = mt.Sample_key_2

union
	select 'taxon_occurrence_Sources' as [Table],
		Source_link_key as recordkey,
		'taxon_occurrence' as navigatetable,
		mt.taxon_occurrence_key as navigatekey,
		item_name +  ', ' + left(dbo.ufn_RtfToPlainText(L.Description),20) as RecordData,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entry_Date else mt.Changed_Date END as EditDate,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then 'Created' else 'Edited' END as ActionTaken,
		CASE WHEN ISNULL(mt.Entry_Date,0)>ISNULL(mt.Changed_Date,0) then mt.Entered_by else mt.Changed_By END as Editor
	from taxon_occurrence_Sources mt
	join taxon_occurrence L on L.taxon_occurrence_key = mt.taxon_occurrence_key

*/



go
grant select on dbo.VW_AUDITLOG to public

