/*===========================================================================*\
  Update the maximum length allowed for fields mapped to a Term List's
  Short Name field.
\*===========================================================================*/
-- Record Type
UPDATE	IW_Column_Type
SET		Maximum_Length = 40
WHERE	IW_Column_Type_Key = 'SYSTEM010000000I'

-- Association Type (Relationship type)
UPDATE	IW_Column_Type
SET		Maximum_Length = 40
WHERE	IW_Column_Type_Key = 'SYSTEM0100000009'

-- Sample Type
UPDATE	IW_Column_Type
SET		Maximum_Length = 20
WHERE	IW_Column_Type_Key = 'SYSTEM010000000J'

-- Specimen Type
UPDATE	IW_Column_Type
SET		Maximum_Length = 20
WHERE	IW_Column_Type_Key = 'SYSTEM010000000N'

-- Substrate
UPDATE	IW_Column_Type
SET		Maximum_Length = 20
WHERE	IW_Column_Type_Key = 'SYSTEM010000000O'

