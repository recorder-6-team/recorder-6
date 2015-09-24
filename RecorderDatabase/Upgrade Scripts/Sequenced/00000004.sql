--Drop the taxon_list_version_key and create it as non-clustered, as it affects name searching.
drop index INDEX_TAXON_NAME.IX_TAXON_LIST_VERSION_KEY
create index IX_TAXON_LIST_VERSION_KEY on INDEX_TAXON_NAME (TAXON_LIST_VERSION_KEY)