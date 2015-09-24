/*============================================================================*\
  Increases the size of the Data column in the setting table to be able to hold
  more data.
\*============================================================================*/

ALTER	TABLE	dbo.Setting
ALTER	COLUMN	Data	VARCHAR(250) NULL