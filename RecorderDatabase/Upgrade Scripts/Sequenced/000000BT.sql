/********* Changes to Settings and Change To Matching to Use Preferred Taxa  *****************/
DELETE FROM SETTING WHERE [NAME] = 'DictStat'
GO
DELETE FROM SETTING WHERE [NAME] = 'DictBlk'
GO
INSERT INTO SETTING VALUES('DictStat','Original','Unknown')
GO
INSERT INTO SETTING VALUES('DictBlk','50','50')
GO
INSERT INTO SETTING VALUES('IndexChk','YES','YES')
