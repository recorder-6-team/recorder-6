-- Select an example admin area to ensure this is the UK setup, otherwise
-- this script would fail.
IF EXISTS(SELECT 1 FROM Admin_Area WHERE Admin_Area_Key='NBNSYS0000000869') 
BEGIN
	BEGIN TRANSACTION
	
	IF EXISTS ( SELECT      1
	            FROM        Location
	            WHERE       Location_Key    =   'JNCCIMPW00000001')
	BEGIN
	    /* -------------------------------------------------------------------------
	       Assume import wizard already installed; just make data system supplied.
	     */
	    UPDATE      Location_Type
	    SET         System_Supplied_Data    =   1
	    WHERE       Location_Type_Key       =   'JNCCIMPW00000001'
	
	    IF @@ERROR <> 0 GOTO Failed
	
	    UPDATE      Location
	    SET         System_Supplied_Data    =   1
	    WHERE       Location_Key            BETWEEN 'JNCCIMPW00000001' AND 'JNCCIMPW00000115'
	    OR          Location_Key            BETWEEN 'JNCCIMPW00000118' AND 'JNCCIMPW00000157'
	
	    IF @@ERROR <> 0 GOTO Failed
	
	    UPDATE      Location_Admin_Areas
	    SET         System_Supplied_Data        =   1
	    WHERE       Location_Admin_Areas_Key    BETWEEN 'JNCCIMPW00000003' AND 'JNCCIMPW00000115'
	    OR          Location_Key                BETWEEN 'JNCCIMPW00000118' AND 'JNCCIMPW00000157'
	
	    IF @@ERROR <> 0 GOTO Failed
	END
	ELSE
	BEGIN
	    /* -------------------------------------------------------------------------
	       Location types
	     */
	    INSERT INTO Location_Type (Location_Type_Key, Short_Name, Long_Name, Description, Authority, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000001', 'Vice-county', 'Vice-county', 'Vice-county vague sites', 'JNCC', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    /* -------------------------------------------------------------------------
	       Locations
	     */
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000001', 'Watsonian Vice-counties', NULL, 'SV00', 'OSGB', 49.7661857973024, -7.55644848245938, 'JNCCIMPW00000001', NULL, 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000002', 'Irish Vice-counties', NULL, 'V00', 'OSNI', 51.2189791210786, -10.8631268518042, 'JNCCIMPW00000001', NULL, 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000003', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '1', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000004', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SV91', 'OSGB', 49.9090546464691, -6.31852708617013, 'JNCCIMPW00000001', '1a', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000005', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TL7624', 'OSGB', 51.8862948348636, 0.557682132830028, 'JNCCIMPW00000001', '2', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000006', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TL4573', 'OSGB', 52.3355071095829, 0.128420753608317, 'JNCCIMPW00000001', '3', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000007', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SJ9623', 'OSGB', 52.8042079042742, -2.05934095281631, 'JNCCIMPW00000001', '4', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000008', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SH5653', 'OSGB', 53.0545270355953, -4.14877302386607, 'JNCCIMPW00000001', '5', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000009', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SD6611', 'OSGB', 53.5942324236824, -2.51376051276825, 'JNCCIMPW00000001', '6', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000010', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NY5201', 'OSGB', 54.4020553500471, -2.73949182419885, 'JNCCIMPW00000001', '7', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000011', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NT3424', 'OSGB', 55.5052972841607, -3.04505188192294, 'JNCCIMPW00000001', '8', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000012', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NO0156', 'OSGB', 56.6853693305703, -3.61631494541407, 'JNCCIMPW00000001', '9', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000013', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '10', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000014', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '11', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000015', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '12', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000016', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '13', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000017', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TQ5618', 'OSGB', 50.9397631979483, 0.220638213457241, 'JNCCIMPW00000001', '14', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000018', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TR0450', 'OSGB', 51.2121915206526, 0.921135165431895, 'JNCCIMPW00000001', '15', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000019', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TQ5857', 'OSGB', 51.2896769689955, 0.26617103134951, 'JNCCIMPW00000001', '16', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000020', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TQ1554', 'OSGB', 51.2730199053534, -0.35121225129172, 'JNCCIMPW00000001', '17', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000021', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TQ6792', 'OSGB', 51.6015723140172, 0.411655805990794, 'JNCCIMPW00000001', '18', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000022', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TL7624', 'OSGB', 51.8862948348636, 0.557682132830028, 'JNCCIMPW00000001', '19', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000023', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TL2317', 'OSGB', 51.8375996791438, -0.214530490125234, 'JNCCIMPW00000001', '20', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000024', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TQ2085', 'OSGB', 51.5506340729822, -0.269066626901935, 'JNCCIMPW00000001', '21', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000025', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SU5483', 'OSGB', 51.5428320883252, -1.22123453617845, 'JNCCIMPW00000001', '22', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000026', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SP5011', 'OSGB', 51.794963606339, -1.27490716742886, 'JNCCIMPW00000001', '23', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000027', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SP8413', 'OSGB', 51.8088700543554, -0.781455267133106, 'JNCCIMPW00000001', '24', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000028', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TM3066', 'OSGB', 52.2436413076553, 1.36942867183581, 'JNCCIMPW00000001', '25', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000029', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TL8562', 'OSGB', 52.2246846997161, 0.708899586644112, 'JNCCIMPW00000001', '26', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000030', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TG2410', 'OSGB', 52.641067221958, 1.31120110573751, 'JNCCIMPW00000001', '27', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000031', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TF7913', 'OSGB', 52.6847255191332, 0.648503775870444, 'JNCCIMPW00000001', '28', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000032', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TL4573', 'OSGB', 52.3355071095829, 0.128420753608317, 'JNCCIMPW00000001', '29', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000033', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TL0842', 'OSGB', 52.0654133260817, -0.424317965596802, 'JNCCIMPW00000001', '30', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000034', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TL2077', 'OSGB', 52.3774944434294, -0.23690995922579, 'JNCCIMPW00000001', '31', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000035', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SP8171', 'OSGB', 52.3307165749164, -0.811196118067039, 'JNCCIMPW00000001', '32', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000036', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SP0621', 'OSGB', 51.8870867268763, -1.91281162586894, 'JNCCIMPW00000001', '33', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000037', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'ST7399', 'OSGB', 51.6886441797899, -2.39063045161071, 'JNCCIMPW00000001', '34', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000038', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SO3403', 'OSGB', 51.7213771772378, -2.95557233025928, 'JNCCIMPW00000001', '35', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000039', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SO4946', 'OSGB', 52.1095603412302, -2.74478898850225, 'JNCCIMPW00000001', '36', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000040', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SO8958', 'OSGB', 52.2197005425673, -2.16103679041353, 'JNCCIMPW00000001', '37', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000041', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SP2870', 'OSGB', 52.3269917255278, -1.58909651141818, 'JNCCIMPW00000001', '38', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000042', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SJ9623', 'OSGB', 52.8042079042742, -2.05934095281631, 'JNCCIMPW00000001', '39', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000043', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SJ5104', 'OSGB', 52.6311881474254, -2.72406106775255, 'JNCCIMPW00000001', '40', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000044', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SS8891', 'OSGB', 51.6062120233463, -3.61750292736915, 'JNCCIMPW00000001', '41', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000045', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SO0131', 'OSGB', 51.9682178631718, -3.44124079696038, 'JNCCIMPW00000001', '42', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000046', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SO1264', 'OSGB', 52.2667153254626, -3.2896833922967, 'JNCCIMPW00000001', '43', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000047', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SN4924', 'OSGB', 51.893628247238, -4.19468664769226, 'JNCCIMPW00000001', '44', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000048', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SM9622', 'OSGB', 51.8587939792974, -4.96285712529402, 'JNCCIMPW00000001', '45', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000049', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SN5862', 'OSGB', 52.2374529720803, -4.07978101780477, 'JNCCIMPW00000001', '46', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000050', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SJ0401', 'OSGB', 52.5979456511907, -3.41752251714767, 'JNCCIMPW00000001', '47', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000051', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SH7927', 'OSGB', 52.8265967321888, -3.79606548396137, 'JNCCIMPW00000001', '48', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000052', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SH5653', 'OSGB', 53.0545270355953, -4.14877302386607, 'JNCCIMPW00000001', '49', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000053', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SJ1154', 'OSGB', 53.0755051673767, -3.32865331545712, 'JNCCIMPW00000001', '50', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000054', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SJ1869', 'OSGB', 53.211444929899, -3.22801969003133, 'JNCCIMPW00000001', '51', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000055', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SH4481', 'OSGB', 53.3026693258522, -4.34134374788714, 'JNCCIMPW00000001', '52', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000056', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TF1337', 'OSGB', 52.9181990875089, -0.319153819857892, 'JNCCIMPW00000001', '53', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000057', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TF1786', 'OSGB', 53.3576477024495, -0.241793462642486, 'JNCCIMPW00000001', '54', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000058', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SK6510', 'OSGB', 52.6834444475402, -1.03835961422529, 'JNCCIMPW00000001', '55', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000059', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SK6762', 'OSGB', 53.1506278407773, -0.998043665102289, 'JNCCIMPW00000001', '56', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000060', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SK2758', 'OSGB', 53.1181987911859, -1.59653528690321, 'JNCCIMPW00000001', '57', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000061', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SJ6875', 'OSGB', 53.2707455176287, -2.47988129856694, 'JNCCIMPW00000001', '58', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000062', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SD6611', 'OSGB', 53.5942324236824, -2.51376051276825, 'JNCCIMPW00000001', '59', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000063', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SD5352', 'OSGB', 53.9617500846952, -2.71643322903629, 'JNCCIMPW00000001', '60', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000064', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'TA0147', 'OSGB', 53.9090964795117, -0.462323676535182, 'JNCCIMPW00000001', '61', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000065', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SE6791', 'OSGB', 54.3100413756748, -0.970088016394968, 'JNCCIMPW00000001', '62', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000066', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SE3314', 'OSGB', 53.6212632178361, -1.50103167034053, 'JNCCIMPW00000001', '63', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000067', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SE1254', 'OSGB', 53.9817241374795, -1.8169947500696, 'JNCCIMPW00000001', '64', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000068', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SE0796', 'OSGB', 54.3593304801556, -1.89227031609514, 'JNCCIMPW00000001', '65', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000069', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NZ1937', 'OSGB', 54.7275233695606, -1.70494592359183, 'JNCCIMPW00000001', '66', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000070', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NY9578', 'OSGB', 55.0963386190488, -2.07835886173771, 'JNCCIMPW00000001', '67', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000071', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NU0123', 'OSGB', 55.5007679824193, -1.98416801212024, 'JNCCIMPW00000001', '68', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000072', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NY5201', 'OSGB', 54.4020553500471, -2.73949182419885, 'JNCCIMPW00000001', '69', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000073', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NY3336', 'OSGB', 54.7144328224284, -3.04013609087748, 'JNCCIMPW00000001', '70', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000074', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SC3382', 'OSGB', 54.2063253943745, -4.56088494277378, 'JNCCIMPW00000001', '71', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000075', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NY0692', 'OSGB', 55.2132352256866, -3.47751751846352, 'JNCCIMPW00000001', '72', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000076', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NX6572', 'OSGB', 55.024114494349, -4.11204727340643, 'JNCCIMPW00000001', '73', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000077', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NX2155', 'OSGB', 54.8576288810018, -4.7890269847413, 'JNCCIMPW00000001', '74', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000078', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NS3718', 'OSGB', 55.428712379615, -4.57622131477483, 'JNCCIMPW00000001', '75', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000079', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NS4161', 'OSGB', 55.8160620318781, -4.53790540026185, 'JNCCIMPW00000001', '76', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000080', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NS8243', 'OSGB', 55.6661577073615, -3.87616697014077, 'JNCCIMPW00000001', '77', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000081', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NT1736', 'OSGB', 55.6105136252752, -3.31776405842254, 'JNCCIMPW00000001', '78', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000082', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NT3424', 'OSGB', 55.5052972841607, -3.04505188192294, 'JNCCIMPW00000001', '79', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000083', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NT5817', 'OSGB', 55.4450485863574, -2.66401153479848, 'JNCCIMPW00000001', '80', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000084', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NT7153', 'OSGB', 55.7694883736445, -2.46228256213485, 'JNCCIMPW00000001', '81', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000085', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NT5570', 'OSGB', 55.9210143445482, -2.72013494347032, 'JNCCIMPW00000001', '82', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000086', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NT2555', 'OSGB', 55.7825128798585, -3.19598422954273, 'JNCCIMPW00000001', '83', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000087', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NT0073', 'OSGB', 55.9396740368069, -3.60113097714637, 'JNCCIMPW00000001', '84', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000088', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NO3204', 'OSGB', 56.2237696471312, -3.09678269800401, 'JNCCIMPW00000001', '85', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000089', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NS6288', 'OSGB', 56.0649558246731, -4.21682152031779, 'JNCCIMPW00000001', '86', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000090', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NN6706', 'OSGB', 56.2279982791491, -4.14555554215199, 'JNCCIMPW00000001', '87', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000091', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NO0156', 'OSGB', 56.6853693305703, -3.61631494541407, 'JNCCIMPW00000001', '89', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000092', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NO4456', 'OSGB', 56.6924993466546, -2.91441929349725, 'JNCCIMPW00000001', '90', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000093', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NO7586', 'OSGB', 56.9647155720886, -2.41118933343477, 'JNCCIMPW00000001', '91', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000094', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NJ4502', 'OSGB', 57.1058511853703, -2.90806459140335, 'JNCCIMPW00000001', '92', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000095', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NJ7840', 'OSGB', 57.4499942167278, -2.36662529863907, 'JNCCIMPW00000001', '93', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000096', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NJ3837', 'OSGB', 57.4193476913487, -3.03237395842925, 'JNCCIMPW00000001', '94', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000097', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NJ1343', 'OSGB', 57.4691355717188, -3.45065535933447, 'JNCCIMPW00000001', '95', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000098', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NH5916', 'OSGB', 57.2132118242639, -4.33492904306699, 'JNCCIMPW00000001', '96', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000099', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NM9880', 'OSGB', 56.8675062047039, -5.31451839061809, 'JNCCIMPW00000001', '97', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000100', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NN0817', 'OSGB', 56.3065441060262, -5.10400232254687, 'JNCCIMPW00000001', '98', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000101', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NS3689', 'OSGB', 56.0657294872176, -4.63463844268211, 'JNCCIMPW00000001', '99', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000102', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'NN7240', 'OSGB', 56.5346666964448, -4.0815449515854, 'JNCCIMPW00000001', '88', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000103', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '100', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000104', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '101', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000105', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '102', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000106', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '103', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000107', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '104', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000108', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '105', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000109', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '106', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000110', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '107', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000111', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '108', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000112', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '109', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000113', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '110', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000114', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '111', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000115', 'Watsonian Vice-county', 'JNCCIMPW00000001', 'SW6038', 'OSGB', 50.192675246495, -5.36294351293106, 'JNCCIMPW00000001', '112', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000118', 'Irish Vice-county', 'JNCCIMPW00000002', 'V76', 'OSNI', 51.7779534454147, -9.88384851759108, 'JNCCIMPW00000001', 'H1', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000119', 'Irish Vice-county', 'JNCCIMPW00000002', 'Q91', 'OSNI', 52.2312625586302, -9.61019619918731, 'JNCCIMPW00000001', 'H2', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000120', 'Irish Vice-county', 'JNCCIMPW00000002', 'W35', 'OSNI', 51.6988444252492, -9.01257975307686, 'JNCCIMPW00000001', 'H3', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000121', 'Irish Vice-county', 'JNCCIMPW00000002', 'W58', 'OSNI', 51.9704788025075, -8.72763205400929, 'JNCCIMPW00000001', 'H4', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000122', 'Irish Vice-county', 'JNCCIMPW00000002', 'W98', 'OSNI', 51.9726367444214, -8.14553250010611, 'JNCCIMPW00000001', 'H5', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000123', 'Irish Vice-county', 'JNCCIMPW00000002', 'S41', 'OSNI', 52.2407928892156, -7.414365338521, 'JNCCIMPW00000001', 'H6', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000124', 'Irish Vice-county', 'JNCCIMPW00000002', 'S24', 'OSNI', 52.5113976251274, -7.70538834049108, 'JNCCIMPW00000001', 'H7', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000125', 'Irish Vice-county', 'JNCCIMPW00000002', 'R44', 'OSNI', 52.5084643511321, -8.88378418088684, 'JNCCIMPW00000001', 'H8', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000126', 'Irish Vice-county', 'JNCCIMPW00000002', 'R38', 'OSNI', 52.8665728087345, -9.03955588669404, 'JNCCIMPW00000001', 'H9', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000127', 'Irish Vice-county', 'JNCCIMPW00000002', 'R97', 'OSNI', 52.7811905706083, -8.14821501515412, 'JNCCIMPW00000001', 'H10', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000128', 'Irish Vice-county', 'JNCCIMPW00000002', 'S65', 'OSNI', 52.5982932902696, -7.11440906560307, 'JNCCIMPW00000001', 'H11', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000129', 'Irish Vice-county', 'JNCCIMPW00000002', 'T03', 'OSNI', 52.4127888585871, -6.53018980183185, 'JNCCIMPW00000001', 'H12', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000130', 'Irish Vice-county', 'JNCCIMPW00000002', 'S87', 'OSNI', 52.7753597486027, -6.81441606880537, 'JNCCIMPW00000001', 'H13', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000131', 'Irish Vice-county', 'JNCCIMPW00000002', 'S49', 'OSNI', 52.9594717359, -7.4047015817784, 'JNCCIMPW00000001', 'H14', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000132', 'Irish Vice-county', 'JNCCIMPW00000002', 'M82', 'OSNI', 53.2301048666028, -8.29952164552896, 'JNCCIMPW00000001', 'H15', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000133', 'Irish Vice-county', 'JNCCIMPW00000002', 'M04', 'OSNI', 53.4006931851673, -9.50363906659437, 'JNCCIMPW00000001', 'H16', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000134', 'Irish Vice-county', 'JNCCIMPW00000002', 'M65', 'OSNI', 53.4984800335851, -8.60282190976874, 'JNCCIMPW00000001', 'H17', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000135', 'Irish Vice-county', 'JNCCIMPW00000002', 'N22', 'OSNI', 53.2301048666028, -7.70047835447104, 'JNCCIMPW00000001', 'H18', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000136', 'Irish Vice-county', 'JNCCIMPW00000002', 'N91', 'OSNI', 53.1330472471569, -6.65515906415494, 'JNCCIMPW00000001', 'H19', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000137', 'Irish Vice-county', 'JNCCIMPW00000002', 'T19', 'OSNI', 52.9496920395693, -6.36324584564569, 'JNCCIMPW00000001', 'H20', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000138', 'Irish Vice-county', 'JNCCIMPW00000002', 'O24', 'OSNI', 53.396528887005, -6.19578303457581, 'JNCCIMPW00000001', 'H21', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000139', 'Irish Vice-county', 'JNCCIMPW00000002', 'N96', 'OSNI', 53.5821391982573, -6.64094384200521, 'JNCCIMPW00000001', 'H22', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000140', 'Irish Vice-county', 'JNCCIMPW00000002', 'N45', 'OSNI', 53.4984800335851, -7.39717809023125, 'JNCCIMPW00000001', 'H23', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000141', 'Irish Vice-county', 'JNCCIMPW00000002', 'N27', 'OSNI', 53.679332342998, -7.69730062811271, 'JNCCIMPW00000001', 'H24', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000142', 'Irish Vice-county', 'JNCCIMPW00000002', 'M88', 'OSNI', 53.7691864802263, -8.30334536067687, 'JNCCIMPW00000001', 'H25', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000143', 'Irish Vice-county', 'JNCCIMPW00000002', 'M48', 'OSNI', 53.7661167881173, -8.90997991884054, 'JNCCIMPW00000001', 'H26', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000144', 'Irish Vice-county', 'JNCCIMPW00000002', 'G01', 'OSNI', 54.0294420987641, -9.52623547289444, 'JNCCIMPW00000001', 'H27', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000145', 'Irish Vice-county', 'JNCCIMPW00000002', 'G62', 'OSNI', 54.1274231357016, -8.61191490079605, 'JNCCIMPW00000001', 'H28', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000146', 'Irish Vice-county', 'JNCCIMPW00000002', 'H10', 'OSNI', 53.9491803972586, -7.84767516147721, 'JNCCIMPW00000001', 'H29', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000147', 'Irish Vice-county', 'JNCCIMPW00000002', 'H50', 'OSNI', 53.9468629407272, -7.2384114293079, 'JNCCIMPW00000001', 'H30', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000148', 'Irish Vice-county', 'JNCCIMPW00000002', 'J00', 'OSNI', 53.9396224425141, -6.47704541471796, 'JNCCIMPW00000001', 'H31', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000149', 'Irish Vice-county', 'JNCCIMPW00000002', 'H72', 'OSNI', 54.1242159396587, -6.92921850331412, 'JNCCIMPW00000001', 'H32', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000150', 'Irish Vice-county', 'JNCCIMPW00000002', 'H21', 'OSNI', 54.0387407784764, -7.69469550610125, 'JNCCIMPW00000001', 'H33', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000151', 'Irish Vice-county', 'JNCCIMPW00000002', 'H19', 'OSNI', 54.757791253895, -7.84465467934861, 'JNCCIMPW00000001', 'H34', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000152', 'Irish Vice-county', 'JNCCIMPW00000002', 'B91', 'OSNI', 54.9374678266332, -8.15603712953786, 'JNCCIMPW00000001', 'H35', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000153', 'Irish Vice-county', 'JNCCIMPW00000002', 'H57', 'OSNI', 54.5757379296337, -7.22673186949349, 'JNCCIMPW00000001', 'H36', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000154', 'Irish Vice-county', 'JNCCIMPW00000002', 'J03', 'OSNI', 54.2090769637474, -6.4671488526676, 'JNCCIMPW00000001', 'H37', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000155', 'Irish Vice-county', 'JNCCIMPW00000002', 'J44', 'OSNI', 54.2895054032131, -5.84974983029078, 'JNCCIMPW00000001', 'H38', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000156', 'Irish Vice-county', 'JNCCIMPW00000002', 'D21', 'OSNI', 54.9231538691401, -6.12810711517527, 'JNCCIMPW00000001', 'H39', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location (Location_Key, Description, Parent_Key, Spatial_Ref, Spatial_Ref_System, Lat, Long, Location_Type_Key, File_Code, Spatial_Ref_Qualifier, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000157', 'Irish Vice-county', 'JNCCIMPW00000002', 'C81', 'OSNI', 54.9311605406268, -6.75186533669106, 'JNCCIMPW00000001', 'H40', 'Site Centroid', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    /* -------------------------------------------------------------------------
	       Location names
	     */
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000001', 'Vice-counties', 1, 'JNCCIMPW00000001', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000002', 'Irish Vice-counties', 1, 'JNCCIMPW00000002', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000003', 'West Cornwall', 1, 'JNCCIMPW00000003', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000004', 'Scilly Isles', 1, 'JNCCIMPW00000004', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000005', 'East Cornwall', 1, 'JNCCIMPW00000005', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000006', 'South Devon', 1, 'JNCCIMPW00000006', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000007', 'North Devon', 1, 'JNCCIMPW00000007', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000008', 'South Somerset', 1, 'JNCCIMPW00000008', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000009', 'North Somerset', 1, 'JNCCIMPW00000009', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000010', 'North Wiltshire', 1, 'JNCCIMPW00000010', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000011', 'South Wiltshire', 1, 'JNCCIMPW00000011', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000012', 'Dorset', 1, 'JNCCIMPW00000012', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000013', 'Isle of Wight', 1, 'JNCCIMPW00000013', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000014', 'South Hampshire', 1, 'JNCCIMPW00000014', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000015', 'North Hampshire', 1, 'JNCCIMPW00000015', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000016', 'West Sussex', 1, 'JNCCIMPW00000016', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000017', 'East Sussex', 1, 'JNCCIMPW00000017', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000018', 'East Kent', 1, 'JNCCIMPW00000018', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000019', 'West Kent', 1, 'JNCCIMPW00000019', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000020', 'Surrey', 1, 'JNCCIMPW00000020', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000021', 'South Essex', 1, 'JNCCIMPW00000021', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000022', 'North Essex', 1, 'JNCCIMPW00000022', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000023', 'Hertfordshire', 1, 'JNCCIMPW00000023', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000024', 'Middlesex', 1, 'JNCCIMPW00000024', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000025', 'Berkshire', 1, 'JNCCIMPW00000025', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000026', 'Oxfordshire', 1, 'JNCCIMPW00000026', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000027', 'Buckinghamshire', 1, 'JNCCIMPW00000027', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000028', 'East Suffolk', 1, 'JNCCIMPW00000028', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000029', 'West Suffolk', 1, 'JNCCIMPW00000029', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000030', 'East Norfolk', 1, 'JNCCIMPW00000030', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000031', 'West Norfolk', 1, 'JNCCIMPW00000031', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000032', 'Cambridgeshire', 1, 'JNCCIMPW00000032', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000033', 'Bedfordshire', 1, 'JNCCIMPW00000033', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000034', 'Huntingdonshire', 1, 'JNCCIMPW00000034', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000035', 'Northamptonshire', 1, 'JNCCIMPW00000035', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000036', 'East Gloucestershire', 1, 'JNCCIMPW00000036', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000037', 'West Gloucestershire', 1, 'JNCCIMPW00000037', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000038', 'Monmouthshire', 1, 'JNCCIMPW00000038', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000039', 'Herefordshire', 1, 'JNCCIMPW00000039', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000040', 'Worcestershire', 1, 'JNCCIMPW00000040', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000041', 'Warwickshire', 1, 'JNCCIMPW00000041', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000042', 'Staffordshire', 1, 'JNCCIMPW00000042', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000043', 'Shropshire', 1, 'JNCCIMPW00000043', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000044', 'Glamorganshire', 1, 'JNCCIMPW00000044', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000045', 'Breconshire', 1, 'JNCCIMPW00000045', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000046', 'Radnorshire', 1, 'JNCCIMPW00000046', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000047', 'Caerfyrddyn (Carmarthenshire)', 1, 'JNCCIMPW00000047', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000048', 'Pembrokeshire', 1, 'JNCCIMPW00000048', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000049', 'Ceredigion (Cardiganshire)', 1, 'JNCCIMPW00000049', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000050', 'Montgomeryshire', 1, 'JNCCIMPW00000050', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000051', 'Meirionydd (Merionithshire)', 1, 'JNCCIMPW00000051', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000052', 'Caernarfon (Caernarvonshire)', 1, 'JNCCIMPW00000052', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000053', 'Denbighshire', 1, 'JNCCIMPW00000053', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000054', 'Flintshire', 1, 'JNCCIMPW00000054', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000055', 'Anglesey', 1, 'JNCCIMPW00000055', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000056', 'South Lincolnshire', 1, 'JNCCIMPW00000056', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000057', 'North Lincolnshire', 1, 'JNCCIMPW00000057', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000058', 'Leicestershire & Rutland', 1, 'JNCCIMPW00000058', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000059', 'Nottinghamshire', 1, 'JNCCIMPW00000059', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000060', 'Derbyshire', 1, 'JNCCIMPW00000060', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000061', 'Cheshire', 1, 'JNCCIMPW00000061', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000062', 'South Lancashire', 1, 'JNCCIMPW00000062', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000063', 'West Lancashire', 1, 'JNCCIMPW00000063', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000064', 'South-east Yorkshire', 1, 'JNCCIMPW00000064', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000065', 'North-east Yorkshire', 1, 'JNCCIMPW00000065', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000066', 'South-west Yorkshire', 1, 'JNCCIMPW00000066', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000067', 'Mid-west Yorkshire', 1, 'JNCCIMPW00000067', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000068', 'North-west Yorkshire', 1, 'JNCCIMPW00000068', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000069', 'Durham', 1, 'JNCCIMPW00000069', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000070', 'South Northumberland', 1, 'JNCCIMPW00000070', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000071', 'North Northumberland (Cheviotland)', 1, 'JNCCIMPW00000071', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000072', 'Westmorland & North Lancashire', 1, 'JNCCIMPW00000072', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000073', 'Cumberland', 1, 'JNCCIMPW00000073', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000074', 'Isle of Man', 1, 'JNCCIMPW00000074', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000075', 'Dumfriesshire', 1, 'JNCCIMPW00000075', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000076', 'Kirkudbrightshire', 1, 'JNCCIMPW00000076', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000077', 'Wigtownshire', 1, 'JNCCIMPW00000077', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000078', 'Ayrshire', 1, 'JNCCIMPW00000078', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000079', 'Renfrewshire', 1, 'JNCCIMPW00000079', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000080', 'Lanarkshire', 1, 'JNCCIMPW00000080', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000081', 'Peebleshire', 1, 'JNCCIMPW00000081', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000082', 'Selkirkshire', 1, 'JNCCIMPW00000082', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000083', 'Roxburghshire', 1, 'JNCCIMPW00000083', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000084', 'Berwickshire', 1, 'JNCCIMPW00000084', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000085', 'East Lothian (Haddingtonshire)', 1, 'JNCCIMPW00000085', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000086', 'Midlothian (Edinburgh)', 1, 'JNCCIMPW00000086', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000087', 'West Lothian (Linlithgowshire)', 1, 'JNCCIMPW00000087', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000088', 'Fifeshire', 1, 'JNCCIMPW00000088', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000089', 'Stirlingshire', 1, 'JNCCIMPW00000089', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000090', 'West Perthshire', 1, 'JNCCIMPW00000090', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000091', 'East Perthshire', 1, 'JNCCIMPW00000091', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000092', 'Angus (Forfarshire)', 1, 'JNCCIMPW00000092', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000093', 'Kincardineshire', 1, 'JNCCIMPW00000093', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000094', 'South Aberdeenshire', 1, 'JNCCIMPW00000094', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000095', 'North Aberdeenshire', 1, 'JNCCIMPW00000095', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000096', 'Banffshire', 1, 'JNCCIMPW00000096', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000097', 'Moray (Elgin)', 1, 'JNCCIMPW00000097', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000098', 'East Inverness & Nairn (Easterness)', 1, 'JNCCIMPW00000098', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000099', 'West Inverness (Westerness)', 1, 'JNCCIMPW00000099', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000100', 'Main Argyll', 1, 'JNCCIMPW00000100', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000101', 'Dunbartonshire', 1, 'JNCCIMPW00000101', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000102', 'Mid Perthshire', 1, 'JNCCIMPW00000102', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000103', 'Clyde Islands', 1, 'JNCCIMPW00000103', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000104', 'Kintyre', 1, 'JNCCIMPW00000104', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000105', 'South Ebudes', 1, 'JNCCIMPW00000105', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000106', 'Mid Ebudes', 1, 'JNCCIMPW00000106', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000107', 'North Ebudes', 1, 'JNCCIMPW00000107', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000108', 'West Ross.', 1, 'JNCCIMPW00000108', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000109', 'East Ross.', 1, 'JNCCIMPW00000109', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000110', 'East Sutherland', 1, 'JNCCIMPW00000110', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000111', 'West Sutherland', 1, 'JNCCIMPW00000111', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000112', 'Caithness', 1, 'JNCCIMPW00000112', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000113', 'Outer Hebrides', 1, 'JNCCIMPW00000113', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000114', 'Orkney', 1, 'JNCCIMPW00000114', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000115', 'Shetland (Zetland)', 1, 'JNCCIMPW00000115', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000118', 'South Kerry', 1, 'JNCCIMPW00000118', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000119', 'North Kerry', 1, 'JNCCIMPW00000119', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000120', 'West Cork', 1, 'JNCCIMPW00000120', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000121', 'Mid Cork', 1, 'JNCCIMPW00000121', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000122', 'East Cork', 1, 'JNCCIMPW00000122', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000123', 'Waterford', 1, 'JNCCIMPW00000123', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000124', 'South Tipperary', 1, 'JNCCIMPW00000124', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000125', 'Limerick', 1, 'JNCCIMPW00000125', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000126', 'Clare', 1, 'JNCCIMPW00000126', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000127', 'North Tipperary', 1, 'JNCCIMPW00000127', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000128', 'Kilkenny', 1, 'JNCCIMPW00000128', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000129', 'Wexford', 1, 'JNCCIMPW00000129', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000130', 'Carlow', 1, 'JNCCIMPW00000130', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000131', 'Leix (Queen''s County)', 1, 'JNCCIMPW00000131', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000132', 'South-east Galway', 1, 'JNCCIMPW00000132', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000133', 'West Galway', 1, 'JNCCIMPW00000133', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000134', 'North-east Galway', 1, 'JNCCIMPW00000134', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000135', 'Offlay (King''s County)', 1, 'JNCCIMPW00000135', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000136', 'Kildare', 1, 'JNCCIMPW00000136', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000137', 'Wicklow', 1, 'JNCCIMPW00000137', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000138', 'Dublin', 1, 'JNCCIMPW00000138', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000139', 'Meath', 1, 'JNCCIMPW00000139', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000140', 'West Meath', 1, 'JNCCIMPW00000140', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000141', 'Longford', 1, 'JNCCIMPW00000141', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000142', 'Roscommon', 1, 'JNCCIMPW00000142', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000143', 'East Mayo', 1, 'JNCCIMPW00000143', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000144', 'West Mayo', 1, 'JNCCIMPW00000144', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000145', 'Sligo', 1, 'JNCCIMPW00000145', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000146', 'Leitrim', 1, 'JNCCIMPW00000146', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000147', 'Cavan', 1, 'JNCCIMPW00000147', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000148', 'Louth', 1, 'JNCCIMPW00000148', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000149', 'Monaghan', 1, 'JNCCIMPW00000149', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000150', 'Fermanagh', 1, 'JNCCIMPW00000150', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000151', 'East Donegal', 1, 'JNCCIMPW00000151', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000152', 'West Donegal', 1, 'JNCCIMPW00000152', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000153', 'Tyrone', 1, 'JNCCIMPW00000153', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000154', 'Armagh', 1, 'JNCCIMPW00000154', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000155', 'Down', 1, 'JNCCIMPW00000155', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000156', 'Antrim', 1, 'JNCCIMPW00000156', 'NBNSYS0000000001', '20001012')
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Name (Location_Name_Key, Item_Name, Preferred, Location_Key, Entered_By, Entry_Date)
	    VALUES ('JNCCIMPW00000157', 'Londonderry', 1, 'JNCCIMPW00000157', 'NBNSYS0000000001', '20001012')
	
	    /* -------------------------------------------------------------------------
	       Location admin areas
	     */
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000003', 'NBNSYS0000000869', 'JNCCIMPW00000003', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000004', 'NBNSYS0000000870', 'JNCCIMPW00000004', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000005', 'NBNSYS0000000871', 'JNCCIMPW00000005', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000006', 'NBNSYS0000000872', 'JNCCIMPW00000006', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000007', 'NBNSYS0000000873', 'JNCCIMPW00000007', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000008', 'NBNSYS0000000874', 'JNCCIMPW00000008', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000009', 'NBNSYS0000000875', 'JNCCIMPW00000009', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000010', 'NBNSYS0000000876', 'JNCCIMPW00000010', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000011', 'NBNSYS0000000877', 'JNCCIMPW00000011', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000012', 'NBNSYS0000000878', 'JNCCIMPW00000012', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000013', 'NBNSYS0000000879', 'JNCCIMPW00000013', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000014', 'NBNSYS0000000880', 'JNCCIMPW00000014', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000015', 'NBNSYS0000000881', 'JNCCIMPW00000015', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000016', 'NBNSYS0000000882', 'JNCCIMPW00000016', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000017', 'NBNSYS0000000883', 'JNCCIMPW00000017', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000018', 'NBNSYS0000000884', 'JNCCIMPW00000018', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000019', 'NBNSYS0000000885', 'JNCCIMPW00000019', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000020', 'NBNSYS0000000886', 'JNCCIMPW00000020', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000021', 'NBNSYS0000000887', 'JNCCIMPW00000021', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000022', 'NBNSYS0000000888', 'JNCCIMPW00000022', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000023', 'NBNSYS0000000889', 'JNCCIMPW00000023', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000024', 'NBNSYS0000000890', 'JNCCIMPW00000024', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000025', 'NBNSYS0000000891', 'JNCCIMPW00000025', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000026', 'NBNSYS0000000892', 'JNCCIMPW00000026', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000027', 'NBNSYS0000000893', 'JNCCIMPW00000027', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000028', 'NBNSYS0000000894', 'JNCCIMPW00000028', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000029', 'NBNSYS0000000895', 'JNCCIMPW00000029', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000030', 'NBNSYS0000000896', 'JNCCIMPW00000030', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000031', 'NBNSYS0000000897', 'JNCCIMPW00000031', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000032', 'NBNSYS0000000898', 'JNCCIMPW00000032', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000033', 'NBNSYS0000000899', 'JNCCIMPW00000033', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000034', 'NBNSYS0000000900', 'JNCCIMPW00000034', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000035', 'NBNSYS0000000901', 'JNCCIMPW00000035', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000036', 'NBNSYS0000000902', 'JNCCIMPW00000036', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000037', 'NBNSYS0000000903', 'JNCCIMPW00000037', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000038', 'NBNSYS0000000904', 'JNCCIMPW00000038', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000039', 'NBNSYS0000000905', 'JNCCIMPW00000039', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000040', 'NBNSYS0000000906', 'JNCCIMPW00000040', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000041', 'NBNSYS0000000907', 'JNCCIMPW00000041', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000042', 'NBNSYS0000000908', 'JNCCIMPW00000042', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000043', 'NBNSYS0000000909', 'JNCCIMPW00000043', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000044', 'NBNSYS0000000910', 'JNCCIMPW00000044', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000045', 'NBNSYS0000000911', 'JNCCIMPW00000045', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000046', 'NBNSYS0000000912', 'JNCCIMPW00000046', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000047', 'NBNSYS0000000913', 'JNCCIMPW00000047', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000048', 'NBNSYS0000000914', 'JNCCIMPW00000048', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000049', 'NBNSYS0000000915', 'JNCCIMPW00000049', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000050', 'NBNSYS0000000916', 'JNCCIMPW00000050', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000051', 'NBNSYS0000000917', 'JNCCIMPW00000051', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000052', 'NBNSYS0000000918', 'JNCCIMPW00000052', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000053', 'NBNSYS0000000919', 'JNCCIMPW00000053', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000054', 'NBNSYS0000000920', 'JNCCIMPW00000054', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000055', 'NBNSYS0000000921', 'JNCCIMPW00000055', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000056', 'NBNSYS0000000922', 'JNCCIMPW00000056', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000057', 'NBNSYS0000000923', 'JNCCIMPW00000057', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000058', 'NBNSYS0000000924', 'JNCCIMPW00000058', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000059', 'NBNSYS0000000925', 'JNCCIMPW00000059', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000060', 'NBNSYS0000000926', 'JNCCIMPW00000060', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000061', 'NBNSYS0000000927', 'JNCCIMPW00000061', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000062', 'NBNSYS0000000928', 'JNCCIMPW00000062', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000063', 'NBNSYS0000000929', 'JNCCIMPW00000063', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000064', 'NBNSYS0000000930', 'JNCCIMPW00000064', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000065', 'NBNSYS0000000931', 'JNCCIMPW00000065', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000066', 'NBNSYS0000000932', 'JNCCIMPW00000066', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000067', 'NBNSYS0000000933', 'JNCCIMPW00000067', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000068', 'NBNSYS0000000934', 'JNCCIMPW00000068', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000069', 'NBNSYS0000000935', 'JNCCIMPW00000069', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000070', 'NBNSYS0000000936', 'JNCCIMPW00000070', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000071', 'NBNSYS0000000937', 'JNCCIMPW00000071', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000072', 'NBNSYS0000000938', 'JNCCIMPW00000072', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000073', 'NBNSYS0000000939', 'JNCCIMPW00000073', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000074', 'NBNSYS0000000940', 'JNCCIMPW00000074', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000075', 'NBNSYS0000000941', 'JNCCIMPW00000075', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000076', 'NBNSYS0000000942', 'JNCCIMPW00000076', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000077', 'NBNSYS0000000943', 'JNCCIMPW00000077', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000078', 'NBNSYS0000000944', 'JNCCIMPW00000078', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000079', 'NBNSYS0000000945', 'JNCCIMPW00000079', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000080', 'NBNSYS0000000946', 'JNCCIMPW00000080', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000081', 'NBNSYS0000000947', 'JNCCIMPW00000081', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000082', 'NBNSYS0000000948', 'JNCCIMPW00000082', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000083', 'NBNSYS0000000949', 'JNCCIMPW00000083', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000084', 'NBNSYS0000000950', 'JNCCIMPW00000084', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000085', 'NBNSYS0000000951', 'JNCCIMPW00000085', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000086', 'NBNSYS0000000952', 'JNCCIMPW00000086', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000087', 'NBNSYS0000000953', 'JNCCIMPW00000087', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000088', 'NBNSYS0000000954', 'JNCCIMPW00000088', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000089', 'NBNSYS0000000955', 'JNCCIMPW00000089', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000090', 'NBNSYS0000000956', 'JNCCIMPW00000090', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000091', 'NBNSYS0000000957', 'JNCCIMPW00000091', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000092', 'NBNSYS0000000958', 'JNCCIMPW00000092', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000093', 'NBNSYS0000000959', 'JNCCIMPW00000093', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000094', 'NBNSYS0000000960', 'JNCCIMPW00000094', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000095', 'NBNSYS0000000961', 'JNCCIMPW00000095', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000096', 'NBNSYS0000000962', 'JNCCIMPW00000096', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000097', 'NBNSYS0000000963', 'JNCCIMPW00000097', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000098', 'NBNSYS0000000964', 'JNCCIMPW00000098', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000099', 'NBNSYS0000000965', 'JNCCIMPW00000099', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000100', 'NBNSYS0000000966', 'JNCCIMPW00000100', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000101', 'NBNSYS0000000967', 'JNCCIMPW00000101', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000102', 'NBNSYS0000000968', 'JNCCIMPW00000102', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000103', 'NBNSYS0000000969', 'JNCCIMPW00000103', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000104', 'NBNSYS0000000970', 'JNCCIMPW00000104', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000105', 'NBNSYS0000000971', 'JNCCIMPW00000105', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000106', 'NBNSYS0000000972', 'JNCCIMPW00000106', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000107', 'NBNSYS0000000973', 'JNCCIMPW00000107', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000108', 'NBNSYS0000000974', 'JNCCIMPW00000108', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000109', 'NBNSYS0000000975', 'JNCCIMPW00000109', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000110', 'NBNSYS0000000976', 'JNCCIMPW00000110', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000111', 'NBNSYS0000000977', 'JNCCIMPW00000111', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000112', 'NBNSYS0000000978', 'JNCCIMPW00000112', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000113', 'NBNSYS0000000979', 'JNCCIMPW00000113', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000114', 'NBNSYS0000000980', 'JNCCIMPW00000114', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000115', 'NBNSYS0000000981', 'JNCCIMPW00000115', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000118', 'NBNSYS0000011948', 'JNCCIMPW00000118', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000119', 'NBNSYS0000011949', 'JNCCIMPW00000119', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000120', 'NBNSYS0000011950', 'JNCCIMPW00000120', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000121', 'NBNSYS0000011951', 'JNCCIMPW00000121', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000122', 'NBNSYS0000011952', 'JNCCIMPW00000122', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000123', 'NBNSYS0000011953', 'JNCCIMPW00000123', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000124', 'NBNSYS0000011954', 'JNCCIMPW00000124', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000125', 'NBNSYS0000011955', 'JNCCIMPW00000125', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000126', 'NBNSYS0000011956', 'JNCCIMPW00000126', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000127', 'NBNSYS0000011957', 'JNCCIMPW00000127', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000128', 'NBNSYS0000011958', 'JNCCIMPW00000128', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000129', 'NBNSYS0000011959', 'JNCCIMPW00000129', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000130', 'NBNSYS0000011960', 'JNCCIMPW00000130', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000131', 'NBNSYS0000011961', 'JNCCIMPW00000131', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000132', 'NBNSYS0000011962', 'JNCCIMPW00000132', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000133', 'NBNSYS0000011963', 'JNCCIMPW00000133', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000134', 'NBNSYS0000011964', 'JNCCIMPW00000134', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000135', 'NBNSYS0000011965', 'JNCCIMPW00000135', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000136', 'NBNSYS0000011966', 'JNCCIMPW00000136', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000137', 'NBNSYS0000011967', 'JNCCIMPW00000137', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000138', 'NBNSYS0000011968', 'JNCCIMPW00000138', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000139', 'NBNSYS0000011969', 'JNCCIMPW00000139', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000140', 'NBNSYS0000011970', 'JNCCIMPW00000140', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000141', 'NBNSYS0000011971', 'JNCCIMPW00000141', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000142', 'NBNSYS0000011972', 'JNCCIMPW00000142', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000143', 'NBNSYS0000011973', 'JNCCIMPW00000143', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000144', 'NBNSYS0000011974', 'JNCCIMPW00000144', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000145', 'NBNSYS0000011975', 'JNCCIMPW00000145', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000146', 'NBNSYS0000011976', 'JNCCIMPW00000146', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000147', 'NBNSYS0000011977', 'JNCCIMPW00000147', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000148', 'NBNSYS0000011978', 'JNCCIMPW00000148', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000149', 'NBNSYS0000011979', 'JNCCIMPW00000149', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000150', 'NBNSYS0000011980', 'JNCCIMPW00000150', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000151', 'NBNSYS0000011981', 'JNCCIMPW00000151', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000152', 'NBNSYS0000011982', 'JNCCIMPW00000152', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000153', 'NBNSYS0000011983', 'JNCCIMPW00000153', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000154', 'NBNSYS0000011984', 'JNCCIMPW00000154', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000155', 'NBNSYS0000011985', 'JNCCIMPW00000155', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000156', 'NBNSYS0000011986', 'JNCCIMPW00000156', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	
	    INSERT INTO Location_Admin_Areas (Location_Admin_Areas_Key, Admin_Area_Key, Location_Key, Entered_By, Entry_Date, System_Supplied_Data)
	    VALUES ('JNCCIMPW00000157', 'NBNSYS0000011987', 'JNCCIMPW00000157', 'NBNSYS0000000001', '20001012', 1)
	    IF @@ERROR <> 0 GOTO Failed
	END
	
	COMMIT TRANSACTION
	RETURN
	
	
	Failed:
	ROLLBACK TRANSACTION
	RETURN
END