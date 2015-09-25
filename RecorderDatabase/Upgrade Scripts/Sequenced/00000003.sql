--try
--Index to make selecting data for an occurrence faster
CREATE INDEX IX_TAXON_OCCURRENCE_KEY
ON TAXON_OCCURRENCE_DATA(TAXON_OCCURRENCE_KEY)