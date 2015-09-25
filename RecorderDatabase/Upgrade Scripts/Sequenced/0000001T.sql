/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'dbo.ufn_RtfToPlaintext')
       AND    Type IN ('FN', 'IF', 'TF'))
    DROP FUNCTION ufn_RtfToPlaintext
GO

/*===========================================================================*\
  Description:  Plaintext of RTF text.

                A simplistic RTF parser. Doesn't handle unicode data,
                right-to-left text etc.  Embedded drawings etc. are discarded.

  Parameters:   @rtf                    RTF text

  Created:

  Last revision information:
    $Revision: 1 $
    $Date: 20/01/05 11:14 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_RtfToPlaintext (
    @rtf    VARCHAR(8000))
RETURNS
    VARCHAR(8000)
AS
BEGIN
    IF @rtf IS NULL
            OR SUBSTRING(@rtf, 1, 5) COLLATE SQL_Latin1_General_CP1_CS_AS
                    <> '{\rtf' COLLATE SQL_Latin1_General_CP1_CS_AS
        /* not an RTF string */
        RETURN  @rtf

    DECLARE     @result         VARCHAR(8000),
                @input_length   INT,
                @pos            INT,
                @char           CHAR,
                @braces         INT,
                @in_control     BIT,
                @in_parameter   BIT,
                @word           VARCHAR(100),
                @destination    INT

    DECLARE     @stack TABLE (
                Lvl             INT PRIMARY KEY,
                Destination     INT)

    SELECT      @result         =   '',
                @input_length   =   LEN(@rtf),
                @pos            =   1,
                @braces         =   0,
                @in_control     =   0,
                @in_parameter   =   0,
                @word           =   '',
                @destination    =   0

    WHILE @pos <= @input_length
    BEGIN
        SELECT      @char           =   SUBSTRING(@rtf, @pos, 1),
                    @pos            =   @pos + 1

        IF @in_control = 1
        BEGIN
            /* processing a control word */
            IF @char = ' '
            BEGIN
                SELECT      @in_control     =   0
            END
            ELSE IF @char = '-'
                    OR @char COLLATE SQL_Latin1_General_CP1_CS_AS BETWEEN '0' AND '9'
            BEGIN
                SELECT      @in_control     =   0,
                            @in_parameter   =   1
            END
            ELSE IF (@char COLLATE SQL_Latin1_General_CP1_CS_AS) NOT BETWEEN 'a' AND 'z'
            BEGIN
                IF @word = ''
                    /* control symbol */
                    SELECT      @word           =   @char,
                                @in_control     =   0
                ELSE
                    /* push char back */
                    SELECT      @pos            =   @pos - 1,
                                @in_control     =   0
            END
            ELSE
            BEGIN
                SELECT      @word   =   @word + @char
            END

            IF @in_control = 0
            BEGIN
                IF @word = 'rtf'
                    SELECT          @destination    =   1
                ELSE IF @word IN (
                        '*',
                        'upr',
                        'ud',
                        'fonttbl',
                        'colortbl',
                        'stylesheet',
                        'listoverride',
                        'info',
                        'header',
                        'footer',
                        'headerl',
                        'headerr',
                        'headerf',
                        'footerl',
                        'footerr',
                        'footerf',
                        'pict',
                        'object',
                        'shp',
                        'footnote',
                        'field',
                        'formfield',
                        'xe',
                        'tc')
                    SELECT          @destination    =   0
                ELSE IF @destination = 1
                BEGIN
                    IF @word IN ('\', '{', '}')
                        SELECT          @result         =   @result + @word
                    ELSE IF @word IN ('~', 'enspace', 'qmspace', 'tab')
                        SELECT          @result         =   @result + ' '
                    ELSE IF @word = 'emdash'
                        SELECT          @result         =   @result + '--'
                    ELSE IF @word IN ('endash', '_')
                        SELECT          @result         =   @result + '-'
                    ELSE IF @word = 'bullet'
                        SELECT          @result         =   @result + '*'
                    ELSE IF @word IN ('lquote', 'rquote')
                        SELECT          @result         =   @result + ''''
                    ELSE IF @word IN ('ldblquote', 'rdblquote')
                        SELECT          @result         =   @result + '"'
                    ELSE IF @word IN ('par', CHAR(10), CHAR(13))
                        SELECT          @result         =   @result + ' '
                END
                
                SELECT      @word   =   ''
            END
        END
        ELSE IF @in_parameter = 1
        BEGIN
            /* processing a numeric parameter */
            IF @char = ' '
            BEGIN
                SELECT      @in_parameter   =   0
            END
            ELSE IF @char COLLATE SQL_Latin1_General_CP1_CS_AS NOT BETWEEN '0' AND '9'
            BEGIN
                /* push char back */
                SELECT      @pos            =   @pos - 1,
                            @in_parameter   =   0
            END
        END
        ELSE IF @char = '\'
        BEGIN
            SELECT      @in_control     =   1
        END
        ELSE IF @char = '{'
        BEGIN
            INSERT INTO @stack (
                        Lvl,
                        Destination)
            VALUES      (@braces,
                        @destination)

            SELECT      @braces         =   @braces + 1
        END
        ELSE IF @char = '}'
        BEGIN
            IF @braces > 0
            BEGIN
                SELECT      @braces         =   @braces - 1

                SELECT      @destination    =   Destination
                FROM        @stack
                WHERE       Lvl             =   @braces

                DELETE FROM @stack
                WHERE       Lvl             =   @braces
            END
        END
        ELSE IF @destination = 1
        BEGIN
            SET         @result     =   @result
                                        + CASE ASCII(@char)
                                            WHEN 9 THEN ' '
                                            WHEN 10 THEN ''
                                            WHEN 13 THEN ''
                                            ELSE @char
                                        END
        END
     END

    RETURN      LTRIM(RTRIM(@result))
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'dbo.ufn_RtfToPlaintext')
       AND    Type IN ('FN', 'IF', 'TF'))
BEGIN
    PRINT 'Setting up security on function ufn_RtfToPlaintext'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.ufn_RtfToPlaintext TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.ufn_RtfToPlaintext TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.ufn_RtfToPlaintext TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.ufn_RtfToPlaintext TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.ufn_RtfToPlaintext TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.ufn_RtfToPlaintext TO [Dev - JNCC SQL]
END
GO

