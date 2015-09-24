/* Add only users need update rights, as when they add sort codes might change in 
    other records */
GRANT UPDATE ON Taxon_List_Item TO R2k_AddOnly

/* Add only and full edit users can edit taxon dict so need to be able to 
    apply name server info */
GRANT EXECUTE ON usp_IndexTaxonName_ApplyNameServer TO R2K_AddOnly
GRANT EXECUTE ON usp_IndexTaxonName_ApplyNameServer TO R2K_FullEdit
