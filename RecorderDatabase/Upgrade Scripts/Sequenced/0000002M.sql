
/* Additional flag for full-edit users, restriction to own data. */
ALTER TABLE dbo.[USER] ADD
	FULL_EDIT_OWN_DATA BIT NOT NULL CONSTRAINT DF_USER_FULL_EDIT_OWN_DATA DEFAULT 0
