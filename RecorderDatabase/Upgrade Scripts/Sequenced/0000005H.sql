-- Script created by Sarah Shaw to rectify a disparity between R2002 and R6
-- updates the length of the DATA field in LOCATION_DATA to 12 chars (rather than 10)


ALTER TABLE LOCATION_DATA
ALTER COLUMN DATA varchar(12) not null