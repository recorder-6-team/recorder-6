SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Mantis 61: Measurement Unit Value Not Exporting  
\*============================================================================*/

DELETE FROM DATABASE_RELATIONSHIP WHERE RELATIONSHIP_KEY = 'LC00002300000001'

GO

Insert Into DataBase_Relationship
       (Relationship_Key,Relationship_Name,Master_Table,
        Master_Field,Detail_Table,Detail_Field,Follow_up,
        Follow_Down,One_To_One)
        VALUES
        ('LC00002300000001','MUV_To_MU','MEASURMENT_UNIT','MEASUREMENT_UNIT_KEY',
          'MEASUREMENT_UNIT_VALUE','MEASUREMENT_UNIT_KEY',0,1,0)
 