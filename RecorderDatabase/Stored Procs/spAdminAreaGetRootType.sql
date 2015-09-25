    if exists (Select * from sysobjects where type = 'P' and uid = user_ID('dbo') and name = 'spAdminAreaGetRootType')
    drop proc dbo.spAdminAreaGetRootType
go

create procedure dbo.spAdminAreaGetRootType
    -- Get the ADMIN_TYPE of the specified admin area's root ancestor.

    -- Will run forever if it encounters a cycle in the parent-child 
    -- relationship between admin areas. This obviously will only happen if the
    -- data are broken.
    @AdminAreaKey as char(16),
    @RootTypeKey as char(16) out
as
    set nocount on

    declare     @ParentKey  char(16)

    select      @RootTypeKey =   null    
            -- return null if admin area doesn't exist

    while (1 = 1)
    begin
        select      @ParentKey      =   PARENT,
                    @RootTypeKey    =   ADMIN_TYPE_KEY
        from        ADMIN_AREA
        where       ADMIN_AREA_KEY  =   @AdminAreaKey

        if (@ParentKey is null) break

        select      @AdminAreaKey   =   @ParentKey
    end
go

    IF EXISTS (SELECT * FROM sysobjects WHERE Id = OBJECT_ID('dbo.spAdminAreaGetRootType') AND SysStat & 0xf = 4)
    BEGIN
        PRINT 'Setting up security on procedure spAdminAreaGetRootType'
        	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'Dev- JNCC SQL')
        		GRANT EXECUTE ON dbo.spAdminAreaGetRootType TO [Dev- JNCC SQL]
    
    		IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_AddOnly')
        		GRANT EXECUTE ON dbo.spAdminAreaGetRootType TO [R2k_AddOnly]
    
    		IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_Administrator')
        		GRANT EXECUTE ON dbo.spAdminAreaGetRootType TO [R2k_Administrator]
    
    		IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_FullEdit')
        		GRANT EXECUTE ON dbo.spAdminAreaGetRootType TO [R2k_FullEdit]
    
    		IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_RecordCardsOnly')
        		GRANT EXECUTE ON dbo.spAdminAreaGetRootType TO [R2k_RecordCardsOnly]
    END
GO

