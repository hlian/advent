module D10Input where

import qualified Data.Map as Map
import Data.Map (Map)

data Bin = Bot Int | Out Int deriving (Show, Eq)
data LoHi = LoHi Bin Bin deriving (Show, Eq)

values :: Map Int [Int]
values = Map.fromList $
  [ (107, [47])
  , (111, [3])
  , (120, [23])
  , (124, [11])
  , (13, [5])
  , (156, [53])
  , (160, [61])
  , (169, [13])
  , (18, [37])
  , (185, [19, 29])
  , (189, [17])
  , (194, [41])
  , (197, [7])
  , (199, [31])
  , (205, [67])
  , (21, [71])
  , (30, [73])
  , (32, [43])
  , (39, [59])
  , (93, [2])
  ]

code :: Map Int LoHi
code = Map.fromList $
  [ (153, LoHi (Bot 105) (Bot 10))
  , (0, LoHi (Bot 9) (Bot 89))
  , (1, LoHi (Bot 25) (Bot 44))
  , (10, LoHi (Bot 161) (Bot 178))
  , (100, LoHi (Bot 40) (Bot 203))
  , (101, LoHi (Bot 22) (Bot 43))
  , (102, LoHi (Bot 108) (Bot 105))
  , (103, LoHi (Bot 43) (Bot 75))
  , (104, LoHi (Bot 97) (Bot 102))
  , (105, LoHi (Bot 136) (Bot 161))
  , (106, LoHi (Bot 17) (Bot 84))
  , (107, LoHi (Bot 160) (Bot 184))
  , (108, LoHi (Bot 180) (Bot 136))
  , (109, LoHi (Out 9) (Bot 126))
  , (11, LoHi (Bot 117) (Bot 16))
  , (110, LoHi (Bot 72) (Bot 53))
  , (111, LoHi (Out 11) (Bot 163))
  , (112, LoHi (Bot 94) (Bot 147))
  , (113, LoHi (Bot 103) (Bot 92))
  , (114, LoHi (Bot 188) (Bot 202))
  , (115, LoHi (Bot 181) (Bot 190))
  , (116, LoHi (Bot 23) (Bot 168))
  , (117, LoHi (Out 17) (Bot 174))
  , (118, LoHi (Bot 11) (Bot 154))
  , (119, LoHi (Bot 113) (Bot 46))
  , (12, LoHi (Out 4) (Bot 125))
  , (120, LoHi (Bot 30) (Bot 183))
  , (121, LoHi (Bot 27) (Bot 193))
  , (122, LoHi (Bot 58) (Bot 48))
  , (123, LoHi (Bot 142) (Bot 172))
  , (124, LoHi (Bot 93) (Bot 130))
  , (125, LoHi (Out 1) (Bot 28))
  , (126, LoHi (Out 10) (Bot 180))
  , (127, LoHi (Bot 66) (Bot 65))
  , (128, LoHi (Bot 141) (Bot 115))
  , (129, LoHi (Bot 52) (Bot 57))
  , (13, LoHi (Bot 120) (Bot 54))
  , (130, LoHi (Bot 80) (Bot 146))
  , (131, LoHi (Bot 60) (Bot 143))
  , (132, LoHi (Bot 177) (Bot 66))
  , (133, LoHi (Bot 127) (Bot 22))
  , (134, LoHi (Out 16) (Bot 12))
  , (135, LoHi (Bot 203) (Bot 152))
  , (136, LoHi (Bot 134) (Bot 73))
  , (137, LoHi (Bot 129) (Bot 94))
  , (138, LoHi (Bot 106) (Bot 85))
  , (139, LoHi (Out 5) (Bot 81))
  , (14, LoHi (Bot 31) (Bot 104))
  , (140, LoHi (Bot 155) (Bot 191))
  , (141, LoHi (Bot 46) (Bot 181))
  , (142, LoHi (Bot 153) (Bot 172))
  , (143, LoHi (Bot 165) (Bot 140))
  , (144, LoHi (Bot 121) (Bot 193))
  , (145, LoHi (Bot 55) (Bot 157))
  , (146, LoHi (Bot 204) (Bot 122))
  , (147, LoHi (Bot 128) (Bot 7))
  , (148, LoHi (Bot 151) (Bot 17))
  , (149, LoHi (Bot 3) (Bot 4))
  , (15, LoHi (Bot 182) (Bot 1))
  , (150, LoHi (Bot 173) (Bot 118))
  , (151, LoHi (Bot 146) (Bot 162))
  , (152, LoHi (Bot 206) (Bot 166))
  , (154, LoHi (Bot 16) (Bot 98))
  , (155, LoHi (Bot 154) (Bot 82))
  , (156, LoHi (Bot 111) (Bot 173))
  , (157, LoHi (Bot 5) (Bot 176))
  , (158, LoHi (Out 7) (Bot 59))
  , (159, LoHi (Bot 67) (Bot 8))
  , (16, LoHi (Bot 174) (Bot 132))
  , (160, LoHi (Bot 39) (Bot 56))
  , (161, LoHi (Bot 73) (Bot 24))
  , (162, LoHi (Bot 122) (Bot 3))
  , (163, LoHi (Out 19) (Bot 117))
  , (164, LoHi (Bot 49) (Bot 71))
  , (165, LoHi (Bot 118) (Bot 155))
  , (166, LoHi (Bot 87) (Bot 26))
  , (167, LoHi (Bot 202) (Bot 33))
  , (168, LoHi (Bot 114) (Bot 167))
  , (169, LoHi (Bot 21) (Bot 60))
  , (17, LoHi (Bot 162) (Bot 149))
  , (170, LoHi (Bot 158) (Bot 69))
  , (171, LoHi (Bot 36) (Bot 77))
  , (172, LoHi (Bot 10) (Bot 178))
  , (173, LoHi (Bot 163) (Bot 11))
  , (174, LoHi (Out 0) (Bot 177))
  , (175, LoHi (Bot 135) (Bot 201))
  , (176, LoHi (Bot 95) (Bot 123))
  , (177, LoHi (Out 8) (Bot 19))
  , (178, LoHi (Bot 24) (Bot 159))
  , (179, LoHi (Bot 101) (Bot 103))
  , (18, LoHi (Bot 189) (Bot 110))
  , (180, LoHi (Out 18) (Bot 134))
  , (181, LoHi (Bot 90) (Bot 41))
  , (182, LoHi (Bot 37) (Bot 25))
  , (183, LoHi (Bot 100) (Bot 135))
  , (184, LoHi (Bot 56) (Bot 23))
  , (185, LoHi (Bot 18) (Bot 110))
  , (186, LoHi (Bot 38) (Bot 188))
  , (187, LoHi (Bot 104) (Bot 209))
  , (188, LoHi (Bot 138) (Bot 74))
  , (189, LoHi (Bot 13) (Bot 72))
  , (19, LoHi (Out 14) (Bot 158))
  , (190, LoHi (Bot 41) (Bot 171))
  , (191, LoHi (Bot 82) (Bot 6))
  , (192, LoHi (Bot 69) (Bot 164))
  , (193, LoHi (Bot 63) (Bot 20))
  , (194, LoHi (Bot 107) (Bot 40))
  , (195, LoHi (Bot 1) (Bot 36))
  , (196, LoHi (Bot 2) (Bot 37))
  , (197, LoHi (Bot 32) (Bot 9))
  , (198, LoHi (Bot 4) (Bot 137))
  , (199, LoHi (Bot 169) (Bot 131))
  , (2, LoHi (Bot 164) (Bot 99))
  , (20, LoHi (Bot 147) (Bot 7))
  , (200, LoHi (Out 15) (Bot 61))
  , (201, LoHi (Bot 152) (Bot 166))
  , (202, LoHi (Bot 74) (Bot 208))
  , (203, LoHi (Bot 116) (Bot 206))
  , (204, LoHi (Bot 143) (Bot 58))
  , (205, LoHi (Bot 197) (Bot 0))
  , (206, LoHi (Bot 168) (Bot 87))
  , (207, LoHi (Bot 200) (Bot 96))
  , (208, LoHi (Bot 78) (Bot 121))
  , (209, LoHi (Bot 102) (Bot 153))
  , (21, LoHi (Bot 156) (Bot 150))
  , (22, LoHi (Bot 65) (Bot 62))
  , (23, LoHi (Bot 186) (Bot 114))
  , (24, LoHi (Bot 42) (Bot 159))
  , (25, LoHi (Bot 86) (Bot 145))
  , (26, LoHi (Bot 33) (Bot 144))
  , (27, LoHi (Bot 34) (Bot 63))
  , (28, LoHi (Out 12) (Bot 139))
  , (29, LoHi (Bot 198) (Bot 34))
  , (3, LoHi (Bot 48) (Bot 35))
  , (30, LoHi (Bot 194) (Bot 100))
  , (31, LoHi (Bot 109) (Bot 97))
  , (32, LoHi (Bot 124) (Bot 76))
  , (33, LoHi (Bot 208) (Bot 144))
  , (34, LoHi (Bot 137) (Bot 112))
  , (35, LoHi (Bot 68) (Bot 52))
  , (36, LoHi (Bot 44) (Bot 77))
  , (37, LoHi (Bot 99) (Bot 86))
  , (38, LoHi (Bot 89) (Bot 138))
  , (39, LoHi (Bot 205) (Bot 79))
  , (4, LoHi (Bot 35) (Bot 129))
  , (40, LoHi (Bot 184) (Bot 116))
  , (41, LoHi (Bot 195) (Bot 171))
  , (42, LoHi (Bot 125) (Bot 67))
  , (43, LoHi (Bot 62) (Bot 196))
  , (44, LoHi (Bot 145) (Bot 70))
  , (45, LoHi (Bot 191) (Bot 64))
  , (46, LoHi (Bot 92) (Bot 90))
  , (47, LoHi (Out 6) (Bot 200))
  , (48, LoHi (Bot 45) (Bot 68))
  , (49, LoHi (Bot 47) (Bot 207))
  , (5, LoHi (Bot 187) (Bot 95))
  , (50, LoHi (Bot 176) (Bot 123))
  , (51, LoHi (Bot 14) (Bot 187))
  , (52, LoHi (Bot 83) (Bot 119))
  , (53, LoHi (Bot 175) (Bot 201))
  , (54, LoHi (Bot 183) (Bot 175))
  , (55, LoHi (Bot 51) (Bot 5))
  , (56, LoHi (Bot 79) (Bot 186))
  , (57, LoHi (Bot 119) (Bot 141))
  , (58, LoHi (Bot 140) (Bot 45))
  , (59, LoHi (Out 2) (Bot 47))
  , (6, LoHi (Bot 133) (Bot 101))
  , (60, LoHi (Bot 150) (Bot 165))
  , (61, LoHi (Out 13) (Bot 109))
  , (62, LoHi (Bot 192) (Bot 2))
  , (63, LoHi (Bot 112) (Bot 20))
  , (64, LoHi (Bot 6) (Bot 179))
  , (65, LoHi (Bot 170) (Bot 192))
  , (66, LoHi (Bot 19) (Bot 170))
  , (67, LoHi (Bot 28) (Bot 8))
  , (68, LoHi (Bot 64) (Bot 83))
  , (69, LoHi (Bot 59) (Bot 49))
  , (7, LoHi (Bot 115) (Bot 190))
  , (70, LoHi (Bot 157) (Bot 50))
  , (71, LoHi (Bot 207) (Bot 88))
  , (72, LoHi (Bot 54) (Bot 53))
  , (73, LoHi (Bot 12) (Bot 42))
  , (74, LoHi (Bot 85) (Bot 78))
  , (75, LoHi (Bot 196) (Bot 182))
  , (76, LoHi (Bot 130) (Bot 151))
  , (77, LoHi (Bot 70) (Bot 50))
  , (78, LoHi (Bot 29) (Bot 27))
  , (79, LoHi (Bot 0) (Bot 38))
  , (8, LoHi (Bot 139) (Bot 81))
  , (80, LoHi (Bot 131) (Bot 204))
  , (81, LoHi (Out 20) (Out 3))
  , (82, LoHi (Bot 98) (Bot 133))
  , (83, LoHi (Bot 179) (Bot 113))
  , (84, LoHi (Bot 149) (Bot 198))
  , (85, LoHi (Bot 84) (Bot 29))
  , (86, LoHi (Bot 91) (Bot 55))
  , (87, LoHi (Bot 167) (Bot 26))
  , (88, LoHi (Bot 96) (Bot 14))
  , (89, LoHi (Bot 148) (Bot 106))
  , (9, LoHi (Bot 76) (Bot 148))
  , (90, LoHi (Bot 15) (Bot 195))
  , (91, LoHi (Bot 88) (Bot 51))
  , (92, LoHi (Bot 75) (Bot 15))
  , (93, LoHi (Bot 199) (Bot 80))
  , (94, LoHi (Bot 57) (Bot 128))
  , (95, LoHi (Bot 209) (Bot 142))
  , (96, LoHi (Bot 61) (Bot 31))
  , (97, LoHi (Bot 126) (Bot 108))
  , (98, LoHi (Bot 132) (Bot 127))
  , (99, LoHi (Bot 71) (Bot 91))
  ]

exampleValues :: Map Int [Int]
exampleValues =
  Map.fromList [ (2, [5, 2])
               , (1, [3])
               ]

exampleCode :: Map Int LoHi
exampleCode = Map.fromList $
  [ (2, LoHi (Bot 1) (Bot 0))
  , (1, LoHi (Out 1) (Bot 0))
  , (0, LoHi (Out 2) (Out 0))
  ]
