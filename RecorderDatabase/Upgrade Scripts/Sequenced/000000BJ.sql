/****** Fixes link to species lookup on NBN ******/
Update SETTING SET [DATA] = 'https://species.nbnatlas.org/search/?q=' WHERE
[NAME] = 'GatewayURL'
