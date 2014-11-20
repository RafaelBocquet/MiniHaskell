{-# OPTIONS_GHC -w #-}
module Syntax.Full.Parser where

import Syntax.Full.Token
import Syntax.Expression
import Syntax.Type
import Syntax.Name
import Syntax.Module
import Syntax.Location

import Control.Applicative
import Control.Monad

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.State

import Debug.Trace
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t32 t38 t39 t40 t41 t42 t43 t44 t49 t50 t51 t52 t53 t54 t55 t56 t57 t58 t59 t60 t61 t62 t63 t64 t65 t66 t67 t68 t69 t70 t71 t72 t73 t74 t75 t76 t77 t78 t79 t80 t81 t82 t83 t84 t85 t86 t87 t88 t89 t90 t91 t92 t93 t94 t95 t96 t97 t98 t99 t100 t101 t102 t103 t104 t105 t106 t107 t108 t109 t110
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 ([TopDeclaration])
	| HappyAbsSyn23 (ModuleName)
	| HappyAbsSyn24 (TopDeclaration)
	| HappyAbsSyn26 ([VariableDeclaration])
	| HappyAbsSyn28 (Fixity)
	| HappyAbsSyn29 ((SyntaxName, [Pattern SyntaxName]))
	| HappyAbsSyn30 (Expression SyntaxName)
	| HappyAbsSyn31 (DataConstructor SyntaxName)
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 ((SyntaxName, [SyntaxName]))
	| HappyAbsSyn34 (MonoType SyntaxName)
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39
	| HappyAbsSyn40 t40
	| HappyAbsSyn41 t41
	| HappyAbsSyn42 t42
	| HappyAbsSyn43 t43
	| HappyAbsSyn44 t44
	| HappyAbsSyn49 t49
	| HappyAbsSyn50 t50
	| HappyAbsSyn51 t51
	| HappyAbsSyn52 t52
	| HappyAbsSyn53 t53
	| HappyAbsSyn54 t54
	| HappyAbsSyn55 t55
	| HappyAbsSyn56 t56
	| HappyAbsSyn57 t57
	| HappyAbsSyn58 t58
	| HappyAbsSyn59 t59
	| HappyAbsSyn60 t60
	| HappyAbsSyn61 t61
	| HappyAbsSyn62 t62
	| HappyAbsSyn63 t63
	| HappyAbsSyn64 t64
	| HappyAbsSyn65 t65
	| HappyAbsSyn66 t66
	| HappyAbsSyn67 t67
	| HappyAbsSyn68 t68
	| HappyAbsSyn69 t69
	| HappyAbsSyn70 t70
	| HappyAbsSyn71 t71
	| HappyAbsSyn72 t72
	| HappyAbsSyn73 t73
	| HappyAbsSyn74 t74
	| HappyAbsSyn75 t75
	| HappyAbsSyn76 t76
	| HappyAbsSyn77 t77
	| HappyAbsSyn78 t78
	| HappyAbsSyn79 t79
	| HappyAbsSyn80 t80
	| HappyAbsSyn81 t81
	| HappyAbsSyn82 t82
	| HappyAbsSyn83 t83
	| HappyAbsSyn84 t84
	| HappyAbsSyn85 t85
	| HappyAbsSyn86 t86
	| HappyAbsSyn87 t87
	| HappyAbsSyn88 t88
	| HappyAbsSyn89 t89
	| HappyAbsSyn90 t90
	| HappyAbsSyn91 t91
	| HappyAbsSyn92 t92
	| HappyAbsSyn93 t93
	| HappyAbsSyn94 t94
	| HappyAbsSyn95 t95
	| HappyAbsSyn96 t96
	| HappyAbsSyn97 t97
	| HappyAbsSyn98 t98
	| HappyAbsSyn99 t99
	| HappyAbsSyn100 t100
	| HappyAbsSyn101 t101
	| HappyAbsSyn102 t102
	| HappyAbsSyn103 t103
	| HappyAbsSyn104 t104
	| HappyAbsSyn105 t105
	| HappyAbsSyn106 t106
	| HappyAbsSyn107 t107
	| HappyAbsSyn108 t108
	| HappyAbsSyn109 t109
	| HappyAbsSyn110 t110

action_0 (137) = happyShift action_5
action_0 (20) = happyGoto action_3
action_0 (21) = happyGoto action_4
action_0 _ = happyReduce_31

action_1 (111) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (165) = happyAccept
action_3 _ = happyFail

action_4 (162) = happyShift action_11
action_4 (22) = happyGoto action_10
action_4 _ = happyFail

action_5 (112) = happyShift action_8
action_5 (116) = happyShift action_9
action_5 (9) = happyGoto action_6
action_5 (23) = happyGoto action_7
action_5 _ = happyFail

action_6 _ = happyReduce_34

action_7 (142) = happyShift action_15
action_7 _ = happyFail

action_8 _ = happyReduce_7

action_9 _ = happyReduce_8

action_10 (164) = happyShift action_14
action_10 _ = happyFail

action_11 (67) = happyGoto action_12
action_11 (95) = happyGoto action_13
action_11 _ = happyReduce_163

action_12 (111) = happyShift action_2
action_12 (123) = happyShift action_28
action_12 (124) = happyShift action_29
action_12 (130) = happyShift action_30
action_12 (141) = happyShift action_31
action_12 (155) = happyShift action_32
action_12 (4) = happyGoto action_17
action_12 (16) = happyGoto action_18
action_12 (24) = happyGoto action_19
action_12 (25) = happyGoto action_20
action_12 (26) = happyGoto action_21
action_12 (27) = happyGoto action_22
action_12 (29) = happyGoto action_23
action_12 (61) = happyGoto action_24
action_12 (88) = happyGoto action_25
action_12 (90) = happyGoto action_26
action_12 (91) = happyGoto action_27
action_12 _ = happyFail

action_13 (158) = happyShift action_16
action_13 _ = happyReduce_122

action_14 _ = happyReduce_29

action_15 _ = happyReduce_30

action_16 _ = happyReduce_162

action_17 _ = happyReduce_21

action_18 (146) = happyReduce_153
action_18 (157) = happyReduce_153
action_18 (68) = happyGoto action_51
action_18 (96) = happyGoto action_52
action_18 _ = happyReduce_165

action_19 _ = happyReduce_149

action_20 _ = happyReduce_155

action_21 _ = happyReduce_39

action_22 _ = happyReduce_40

action_23 (147) = happyShift action_50
action_23 (30) = happyGoto action_49
action_23 _ = happyFail

action_24 _ = happyReduce_22

action_25 (72) = happyGoto action_47
action_25 (95) = happyGoto action_48
action_25 (100) = happyGoto action_44
action_25 _ = happyReduce_163

action_26 (146) = happyShift action_45
action_26 (157) = happyShift action_46
action_26 _ = happyFail

action_27 (67) = happyGoto action_41
action_27 (72) = happyGoto action_42
action_27 (95) = happyGoto action_43
action_27 (100) = happyGoto action_44
action_27 _ = happyReduce_163

action_28 (112) = happyShift action_37
action_28 (5) = happyGoto action_40
action_28 _ = happyFail

action_29 (112) = happyShift action_37
action_29 (5) = happyGoto action_35
action_29 (33) = happyGoto action_39
action_29 _ = happyFail

action_30 (112) = happyShift action_8
action_30 (116) = happyShift action_9
action_30 (9) = happyGoto action_6
action_30 (23) = happyGoto action_38
action_30 _ = happyFail

action_31 (112) = happyShift action_37
action_31 (5) = happyGoto action_35
action_31 (33) = happyGoto action_36
action_31 _ = happyFail

action_32 (113) = happyShift action_34
action_32 (6) = happyGoto action_33
action_32 _ = happyFail

action_33 (156) = happyShift action_88
action_33 _ = happyFail

action_34 _ = happyReduce_3

action_35 (70) = happyGoto action_86
action_35 (98) = happyGoto action_87
action_35 _ = happyReduce_169

action_36 (147) = happyShift action_85
action_36 _ = happyFail

action_37 _ = happyReduce_2

action_38 _ = happyReduce_35

action_39 (147) = happyShift action_84
action_39 _ = happyFail

action_40 (111) = happyShift action_2
action_40 (4) = happyGoto action_83
action_40 _ = happyFail

action_41 (163) = happyShift action_82
action_41 _ = happyFail

action_42 (111) = happyShift action_2
action_42 (123) = happyShift action_28
action_42 (124) = happyShift action_29
action_42 (141) = happyShift action_31
action_42 (155) = happyShift action_32
action_42 (4) = happyGoto action_17
action_42 (16) = happyGoto action_18
action_42 (25) = happyGoto action_81
action_42 (26) = happyGoto action_21
action_42 (27) = happyGoto action_22
action_42 (29) = happyGoto action_23
action_42 (61) = happyGoto action_24
action_42 (90) = happyGoto action_26
action_42 _ = happyFail

action_43 (158) = happyShift action_70
action_43 _ = happyReduce_122

action_44 _ = happyReduce_127

action_45 (32) = happyGoto action_74
action_45 (34) = happyGoto action_75
action_45 (35) = happyGoto action_76
action_45 (75) = happyGoto action_77
action_45 (85) = happyGoto action_78
action_45 (97) = happyGoto action_79
action_45 (103) = happyGoto action_80
action_45 _ = happyReduce_167

action_46 (111) = happyShift action_2
action_46 (155) = happyShift action_32
action_46 (4) = happyGoto action_17
action_46 (16) = happyGoto action_73
action_46 (61) = happyGoto action_24
action_46 _ = happyFail

action_47 (111) = happyShift action_2
action_47 (123) = happyShift action_28
action_47 (124) = happyShift action_29
action_47 (130) = happyShift action_30
action_47 (141) = happyShift action_31
action_47 (155) = happyShift action_32
action_47 (4) = happyGoto action_17
action_47 (16) = happyGoto action_18
action_47 (24) = happyGoto action_71
action_47 (25) = happyGoto action_20
action_47 (26) = happyGoto action_21
action_47 (27) = happyGoto action_22
action_47 (29) = happyGoto action_23
action_47 (61) = happyGoto action_24
action_47 (90) = happyGoto action_26
action_47 (91) = happyGoto action_72
action_47 _ = happyFail

action_48 (158) = happyShift action_70
action_48 _ = happyFail

action_49 _ = happyReduce_41

action_50 (122) = happyShift action_66
action_50 (129) = happyShift action_67
action_50 (136) = happyShift action_68
action_50 (148) = happyShift action_69
action_50 (46) = happyGoto action_61
action_50 (47) = happyGoto action_62
action_50 (73) = happyGoto action_63
action_50 (101) = happyGoto action_64
action_50 (110) = happyGoto action_65
action_50 _ = happyReduce_193

action_51 _ = happyReduce_46

action_52 (111) = happyShift action_2
action_52 (112) = happyShift action_8
action_52 (116) = happyShift action_9
action_52 (143) = happyShift action_58
action_52 (155) = happyShift action_59
action_52 (159) = happyShift action_60
action_52 (4) = happyGoto action_53
action_52 (9) = happyGoto action_54
action_52 (52) = happyGoto action_55
action_52 (58) = happyGoto action_56
action_52 (64) = happyGoto action_57
action_52 _ = happyReduce_123

action_53 (152) = happyShift action_149
action_53 _ = happyReduce_102

action_54 _ = happyReduce_105

action_55 _ = happyReduce_164

action_56 _ = happyReduce_106

action_57 _ = happyReduce_107

action_58 _ = happyReduce_104

action_59 (111) = happyShift action_2
action_59 (112) = happyShift action_8
action_59 (116) = happyShift action_9
action_59 (143) = happyShift action_58
action_59 (155) = happyShift action_59
action_59 (159) = happyShift action_60
action_59 (4) = happyGoto action_53
action_59 (9) = happyGoto action_142
action_59 (50) = happyGoto action_143
action_59 (51) = happyGoto action_144
action_59 (52) = happyGoto action_145
action_59 (58) = happyGoto action_56
action_59 (64) = happyGoto action_57
action_59 (83) = happyGoto action_148
action_59 (107) = happyGoto action_147
action_59 _ = happyReduce_186

action_60 (111) = happyShift action_2
action_60 (112) = happyShift action_8
action_60 (116) = happyShift action_9
action_60 (143) = happyShift action_58
action_60 (155) = happyShift action_59
action_60 (159) = happyShift action_60
action_60 (4) = happyGoto action_53
action_60 (9) = happyGoto action_142
action_60 (50) = happyGoto action_143
action_60 (51) = happyGoto action_144
action_60 (52) = happyGoto action_145
action_60 (58) = happyGoto action_56
action_60 (64) = happyGoto action_57
action_60 (83) = happyGoto action_146
action_60 (107) = happyGoto action_147
action_60 _ = happyReduce_186

action_61 (113) = happyShift action_137
action_61 (114) = happyShift action_138
action_61 (117) = happyShift action_139
action_61 (118) = happyShift action_140
action_61 (161) = happyShift action_141
action_61 (10) = happyGoto action_134
action_61 (11) = happyGoto action_135
action_61 (45) = happyGoto action_136
action_61 _ = happyReduce_47

action_62 _ = happyReduce_83

action_63 _ = happyReduce_89

action_64 _ = happyReduce_128

action_65 (111) = happyShift action_127
action_65 (112) = happyShift action_8
action_65 (115) = happyShift action_128
action_65 (116) = happyShift action_9
action_65 (119) = happyShift action_129
action_65 (120) = happyShift action_130
action_65 (121) = happyShift action_131
action_65 (155) = happyShift action_132
action_65 (159) = happyShift action_133
action_65 (8) = happyGoto action_118
action_65 (9) = happyGoto action_119
action_65 (18) = happyGoto action_120
action_65 (19) = happyGoto action_121
action_65 (48) = happyGoto action_122
action_65 (54) = happyGoto action_123
action_65 (55) = happyGoto action_124
action_65 (57) = happyGoto action_125
action_65 (63) = happyGoto action_126
action_65 _ = happyFail

action_66 (122) = happyShift action_66
action_66 (129) = happyShift action_67
action_66 (136) = happyShift action_68
action_66 (148) = happyShift action_69
action_66 (46) = happyGoto action_117
action_66 (47) = happyGoto action_62
action_66 (73) = happyGoto action_63
action_66 (101) = happyGoto action_64
action_66 (110) = happyGoto action_65
action_66 _ = happyReduce_193

action_67 (122) = happyShift action_66
action_67 (129) = happyShift action_67
action_67 (136) = happyShift action_68
action_67 (148) = happyShift action_69
action_67 (46) = happyGoto action_116
action_67 (47) = happyGoto action_62
action_67 (73) = happyGoto action_63
action_67 (101) = happyGoto action_64
action_67 (110) = happyGoto action_65
action_67 _ = happyReduce_193

action_68 (162) = happyShift action_115
action_68 _ = happyFail

action_69 (74) = happyGoto action_112
action_69 (96) = happyGoto action_113
action_69 (102) = happyGoto action_114
action_69 _ = happyReduce_165

action_70 (158) = happyReduce_162
action_70 (163) = happyReduce_162
action_70 _ = happyReduce_171

action_71 _ = happyReduce_148

action_72 (67) = happyGoto action_111
action_72 (72) = happyGoto action_42
action_72 (95) = happyGoto action_43
action_72 (100) = happyGoto action_44
action_72 _ = happyReduce_163

action_73 _ = happyReduce_152

action_74 _ = happyReduce_42

action_75 _ = happyReduce_49

action_76 _ = happyReduce_143

action_77 _ = happyReduce_52

action_78 (151) = happyShift action_110
action_78 _ = happyReduce_51

action_79 (111) = happyShift action_2
action_79 (112) = happyShift action_106
action_79 (116) = happyShift action_107
action_79 (155) = happyShift action_108
action_79 (159) = happyShift action_109
action_79 (4) = happyGoto action_100
action_79 (13) = happyGoto action_101
action_79 (36) = happyGoto action_102
action_79 (37) = happyGoto action_103
action_79 (60) = happyGoto action_104
action_79 (65) = happyGoto action_105
action_79 _ = happyFail

action_80 _ = happyReduce_130

action_81 _ = happyReduce_154

action_82 _ = happyReduce_33

action_83 (142) = happyShift action_99
action_83 (78) = happyGoto action_97
action_83 (79) = happyGoto action_98
action_83 _ = happyReduce_135

action_84 (112) = happyShift action_37
action_84 (155) = happyShift action_96
action_84 (5) = happyGoto action_91
action_84 (17) = happyGoto action_92
action_84 (31) = happyGoto action_93
action_84 (62) = happyGoto action_94
action_84 (86) = happyGoto action_95
action_84 _ = happyFail

action_85 (34) = happyGoto action_90
action_85 (35) = happyGoto action_76
action_85 (75) = happyGoto action_77
action_85 (85) = happyGoto action_78
action_85 (97) = happyGoto action_79
action_85 (103) = happyGoto action_80
action_85 _ = happyReduce_167

action_86 _ = happyReduce_50

action_87 (111) = happyShift action_2
action_87 (4) = happyGoto action_89
action_87 _ = happyReduce_125

action_88 _ = happyReduce_116

action_89 _ = happyReduce_168

action_90 _ = happyReduce_36

action_91 _ = happyReduce_23

action_92 (69) = happyGoto action_187
action_92 (97) = happyGoto action_188
action_92 _ = happyReduce_167

action_93 _ = happyReduce_145

action_94 _ = happyReduce_24

action_95 (149) = happyShift action_186
action_95 _ = happyReduce_37

action_96 (114) = happyShift action_185
action_96 (7) = happyGoto action_184
action_96 _ = happyFail

action_97 _ = happyReduce_38

action_98 _ = happyReduce_134

action_99 (162) = happyShift action_183
action_99 (66) = happyGoto action_182
action_99 _ = happyFail

action_100 _ = happyReduce_54

action_101 _ = happyReduce_61

action_102 (151) = happyReduce_174
action_102 (156) = happyReduce_174
action_102 (157) = happyReduce_174
action_102 (158) = happyReduce_174
action_102 (160) = happyReduce_174
action_102 (163) = happyReduce_174
action_102 _ = happyReduce_166

action_103 _ = happyReduce_53

action_104 _ = happyReduce_55

action_105 _ = happyReduce_56

action_106 _ = happyReduce_15

action_107 _ = happyReduce_16

action_108 (151) = happyShift action_180
action_108 (156) = happyShift action_181
action_108 (157) = happyReduce_191
action_108 (34) = happyGoto action_175
action_108 (35) = happyGoto action_76
action_108 (71) = happyGoto action_176
action_108 (75) = happyGoto action_77
action_108 (85) = happyGoto action_78
action_108 (92) = happyGoto action_177
action_108 (97) = happyGoto action_79
action_108 (99) = happyGoto action_178
action_108 (103) = happyGoto action_80
action_108 (109) = happyGoto action_179
action_108 _ = happyReduce_167

action_109 (160) = happyShift action_174
action_109 (34) = happyGoto action_173
action_109 (35) = happyGoto action_76
action_109 (75) = happyGoto action_77
action_109 (85) = happyGoto action_78
action_109 (97) = happyGoto action_79
action_109 (103) = happyGoto action_80
action_109 _ = happyReduce_167

action_110 (35) = happyGoto action_172
action_110 (75) = happyGoto action_77
action_110 (97) = happyGoto action_79
action_110 (103) = happyGoto action_80
action_110 _ = happyReduce_167

action_111 (163) = happyShift action_171
action_111 _ = happyFail

action_112 (151) = happyShift action_170
action_112 _ = happyFail

action_113 (111) = happyShift action_2
action_113 (112) = happyShift action_8
action_113 (116) = happyShift action_9
action_113 (143) = happyShift action_58
action_113 (155) = happyShift action_59
action_113 (159) = happyShift action_60
action_113 (4) = happyGoto action_53
action_113 (9) = happyGoto action_54
action_113 (52) = happyGoto action_169
action_113 (58) = happyGoto action_56
action_113 (64) = happyGoto action_57
action_113 _ = happyFail

action_114 _ = happyReduce_129

action_115 (67) = happyGoto action_168
action_115 (95) = happyGoto action_13
action_115 _ = happyReduce_163

action_116 (113) = happyShift action_137
action_116 (114) = happyShift action_138
action_116 (117) = happyShift action_139
action_116 (118) = happyShift action_140
action_116 (140) = happyShift action_167
action_116 (161) = happyShift action_141
action_116 (10) = happyGoto action_134
action_116 (11) = happyGoto action_135
action_116 (45) = happyGoto action_136
action_116 _ = happyFail

action_117 (113) = happyShift action_137
action_117 (114) = happyShift action_138
action_117 (117) = happyShift action_139
action_117 (118) = happyShift action_140
action_117 (139) = happyShift action_166
action_117 (161) = happyShift action_141
action_117 (10) = happyGoto action_134
action_117 (11) = happyGoto action_135
action_117 (45) = happyGoto action_136
action_117 _ = happyFail

action_118 _ = happyReduce_25

action_119 _ = happyReduce_27

action_120 _ = happyReduce_90

action_121 _ = happyReduce_91

action_122 (113) = happyReduce_172
action_122 (114) = happyReduce_172
action_122 (117) = happyReduce_172
action_122 (118) = happyReduce_172
action_122 (128) = happyReduce_172
action_122 (139) = happyReduce_172
action_122 (140) = happyReduce_172
action_122 (156) = happyReduce_172
action_122 (157) = happyReduce_172
action_122 (158) = happyReduce_172
action_122 (160) = happyReduce_172
action_122 (161) = happyReduce_172
action_122 (163) = happyReduce_172
action_122 _ = happyReduce_192

action_123 _ = happyReduce_26

action_124 _ = happyReduce_28

action_125 _ = happyReduce_95

action_126 _ = happyReduce_96

action_127 _ = happyReduce_5

action_128 _ = happyReduce_6

action_129 _ = happyReduce_92

action_130 _ = happyReduce_93

action_131 _ = happyReduce_94

action_132 (113) = happyShift action_137
action_132 (114) = happyShift action_138
action_132 (117) = happyShift action_139
action_132 (118) = happyShift action_140
action_132 (122) = happyShift action_66
action_132 (129) = happyShift action_67
action_132 (136) = happyShift action_68
action_132 (148) = happyShift action_69
action_132 (156) = happyReduce_183
action_132 (157) = happyReduce_183
action_132 (10) = happyGoto action_163
action_132 (11) = happyGoto action_164
action_132 (46) = happyGoto action_160
action_132 (47) = happyGoto action_62
action_132 (73) = happyGoto action_63
action_132 (82) = happyGoto action_165
action_132 (101) = happyGoto action_64
action_132 (106) = happyGoto action_162
action_132 (110) = happyGoto action_65
action_132 _ = happyReduce_193

action_133 (122) = happyShift action_66
action_133 (129) = happyShift action_67
action_133 (136) = happyShift action_68
action_133 (148) = happyShift action_69
action_133 (157) = happyReduce_183
action_133 (160) = happyReduce_183
action_133 (46) = happyGoto action_160
action_133 (47) = happyGoto action_62
action_133 (73) = happyGoto action_63
action_133 (82) = happyGoto action_161
action_133 (101) = happyGoto action_64
action_133 (106) = happyGoto action_162
action_133 (110) = happyGoto action_65
action_133 _ = happyReduce_193

action_134 _ = happyReduce_79

action_135 _ = happyReduce_80

action_136 (122) = happyShift action_66
action_136 (129) = happyShift action_67
action_136 (136) = happyShift action_68
action_136 (148) = happyShift action_69
action_136 (47) = happyGoto action_159
action_136 (73) = happyGoto action_63
action_136 (101) = happyGoto action_64
action_136 (110) = happyGoto action_65
action_136 _ = happyReduce_193

action_137 _ = happyReduce_9

action_138 _ = happyReduce_11

action_139 _ = happyReduce_10

action_140 _ = happyReduce_12

action_141 (111) = happyShift action_127
action_141 (112) = happyShift action_8
action_141 (115) = happyShift action_128
action_141 (116) = happyShift action_9
action_141 (8) = happyGoto action_157
action_141 (9) = happyGoto action_158
action_141 _ = happyFail

action_142 (111) = happyShift action_2
action_142 (112) = happyShift action_8
action_142 (116) = happyShift action_9
action_142 (143) = happyShift action_58
action_142 (155) = happyShift action_59
action_142 (159) = happyShift action_60
action_142 (4) = happyGoto action_53
action_142 (9) = happyGoto action_54
action_142 (52) = happyGoto action_155
action_142 (58) = happyGoto action_56
action_142 (64) = happyGoto action_57
action_142 (76) = happyGoto action_156
action_142 _ = happyReduce_105

action_143 (114) = happyShift action_138
action_143 (118) = happyShift action_140
action_143 (11) = happyGoto action_154
action_143 _ = happyReduce_185

action_144 _ = happyReduce_98

action_145 _ = happyReduce_100

action_146 (160) = happyShift action_153
action_146 _ = happyFail

action_147 (157) = happyShift action_152
action_147 _ = happyReduce_140

action_148 (156) = happyShift action_151
action_148 _ = happyFail

action_149 (111) = happyShift action_2
action_149 (112) = happyShift action_8
action_149 (116) = happyShift action_9
action_149 (143) = happyShift action_58
action_149 (155) = happyShift action_59
action_149 (159) = happyShift action_60
action_149 (4) = happyGoto action_53
action_149 (9) = happyGoto action_54
action_149 (52) = happyGoto action_150
action_149 (58) = happyGoto action_56
action_149 (64) = happyGoto action_57
action_149 _ = happyFail

action_150 _ = happyReduce_103

action_151 _ = happyReduce_113

action_152 (111) = happyShift action_2
action_152 (112) = happyShift action_8
action_152 (116) = happyShift action_9
action_152 (143) = happyShift action_58
action_152 (155) = happyShift action_59
action_152 (159) = happyShift action_60
action_152 (4) = happyGoto action_53
action_152 (9) = happyGoto action_142
action_152 (50) = happyGoto action_219
action_152 (51) = happyGoto action_144
action_152 (52) = happyGoto action_145
action_152 (58) = happyGoto action_56
action_152 (64) = happyGoto action_57
action_152 _ = happyFail

action_153 _ = happyReduce_119

action_154 (111) = happyShift action_2
action_154 (112) = happyShift action_8
action_154 (116) = happyShift action_9
action_154 (143) = happyShift action_58
action_154 (155) = happyShift action_59
action_154 (159) = happyShift action_60
action_154 (4) = happyGoto action_53
action_154 (9) = happyGoto action_142
action_154 (51) = happyGoto action_218
action_154 (52) = happyGoto action_145
action_154 (58) = happyGoto action_56
action_154 (64) = happyGoto action_57
action_154 _ = happyFail

action_155 (111) = happyShift action_2
action_155 (112) = happyShift action_8
action_155 (116) = happyShift action_9
action_155 (143) = happyShift action_58
action_155 (155) = happyShift action_59
action_155 (159) = happyShift action_60
action_155 (4) = happyGoto action_53
action_155 (9) = happyGoto action_54
action_155 (52) = happyGoto action_216
action_155 (58) = happyGoto action_56
action_155 (64) = happyGoto action_57
action_155 (94) = happyGoto action_217
action_155 _ = happyReduce_161

action_156 _ = happyReduce_101

action_157 (161) = happyShift action_215
action_157 _ = happyFail

action_158 (161) = happyShift action_214
action_158 _ = happyFail

action_159 _ = happyReduce_84

action_160 (113) = happyShift action_137
action_160 (114) = happyShift action_138
action_160 (117) = happyShift action_139
action_160 (118) = happyShift action_140
action_160 (161) = happyShift action_141
action_160 (10) = happyGoto action_134
action_160 (11) = happyGoto action_135
action_160 (45) = happyGoto action_136
action_160 _ = happyReduce_182

action_161 (160) = happyShift action_213
action_161 _ = happyFail

action_162 (157) = happyShift action_212
action_162 _ = happyReduce_139

action_163 (156) = happyShift action_211
action_163 _ = happyFail

action_164 (156) = happyShift action_210
action_164 _ = happyFail

action_165 (156) = happyShift action_209
action_165 _ = happyFail

action_166 (162) = happyShift action_208
action_166 _ = happyFail

action_167 (122) = happyShift action_66
action_167 (129) = happyShift action_67
action_167 (136) = happyShift action_68
action_167 (148) = happyShift action_69
action_167 (46) = happyGoto action_207
action_167 (47) = happyGoto action_62
action_167 (73) = happyGoto action_63
action_167 (101) = happyGoto action_64
action_167 (110) = happyGoto action_65
action_167 _ = happyReduce_193

action_168 (111) = happyShift action_2
action_168 (155) = happyShift action_32
action_168 (4) = happyGoto action_17
action_168 (16) = happyGoto action_18
action_168 (26) = happyGoto action_205
action_168 (27) = happyGoto action_22
action_168 (29) = happyGoto action_23
action_168 (61) = happyGoto action_24
action_168 (87) = happyGoto action_206
action_168 (90) = happyGoto action_26
action_168 _ = happyFail

action_169 (151) = happyReduce_173
action_169 _ = happyReduce_164

action_170 (122) = happyShift action_66
action_170 (129) = happyShift action_67
action_170 (136) = happyShift action_68
action_170 (148) = happyShift action_69
action_170 (46) = happyGoto action_204
action_170 (47) = happyGoto action_62
action_170 (73) = happyGoto action_63
action_170 (101) = happyGoto action_64
action_170 (110) = happyGoto action_65
action_170 _ = happyReduce_193

action_171 _ = happyReduce_32

action_172 _ = happyReduce_142

action_173 (160) = happyShift action_203
action_173 _ = happyFail

action_174 _ = happyReduce_59

action_175 _ = happyReduce_157

action_176 (156) = happyShift action_202
action_176 _ = happyFail

action_177 (156) = happyShift action_200
action_177 (157) = happyShift action_201
action_177 _ = happyFail

action_178 _ = happyReduce_126

action_179 (157) = happyShift action_199
action_179 _ = happyFail

action_180 (156) = happyShift action_198
action_180 _ = happyFail

action_181 _ = happyReduce_57

action_182 _ = happyReduce_136

action_183 (111) = happyShift action_2
action_183 (155) = happyShift action_32
action_183 (4) = happyGoto action_192
action_183 (16) = happyGoto action_18
action_183 (27) = happyGoto action_193
action_183 (29) = happyGoto action_194
action_183 (42) = happyGoto action_195
action_183 (61) = happyGoto action_24
action_183 (81) = happyGoto action_196
action_183 (90) = happyGoto action_26
action_183 (105) = happyGoto action_197
action_183 _ = happyReduce_180

action_184 (156) = happyShift action_191
action_184 _ = happyFail

action_185 _ = happyReduce_4

action_186 (112) = happyShift action_37
action_186 (155) = happyShift action_96
action_186 (5) = happyGoto action_91
action_186 (17) = happyGoto action_92
action_186 (31) = happyGoto action_190
action_186 (62) = happyGoto action_94
action_186 _ = happyFail

action_187 _ = happyReduce_48

action_188 (111) = happyShift action_2
action_188 (112) = happyShift action_106
action_188 (116) = happyShift action_107
action_188 (155) = happyShift action_108
action_188 (159) = happyShift action_109
action_188 (4) = happyGoto action_100
action_188 (13) = happyGoto action_101
action_188 (36) = happyGoto action_189
action_188 (37) = happyGoto action_103
action_188 (60) = happyGoto action_104
action_188 (65) = happyGoto action_105
action_188 _ = happyReduce_124

action_189 _ = happyReduce_166

action_190 _ = happyReduce_144

action_191 _ = happyReduce_117

action_192 (147) = happyShift action_50
action_192 (30) = happyGoto action_233
action_192 _ = happyReduce_21

action_193 _ = happyReduce_69

action_194 (147) = happyShift action_50
action_194 (30) = happyGoto action_232
action_194 _ = happyFail

action_195 _ = happyReduce_179

action_196 (163) = happyShift action_231
action_196 _ = happyFail

action_197 (163) = happyReduce_138
action_197 (72) = happyGoto action_230
action_197 (95) = happyGoto action_48
action_197 (100) = happyGoto action_44
action_197 _ = happyReduce_163

action_198 _ = happyReduce_60

action_199 (157) = happyReduce_190
action_199 _ = happyReduce_170

action_200 _ = happyReduce_115

action_201 (34) = happyGoto action_229
action_201 (35) = happyGoto action_76
action_201 (75) = happyGoto action_77
action_201 (85) = happyGoto action_78
action_201 (97) = happyGoto action_79
action_201 (103) = happyGoto action_80
action_201 _ = happyReduce_167

action_202 _ = happyReduce_58

action_203 _ = happyReduce_120

action_204 (113) = happyShift action_137
action_204 (114) = happyShift action_138
action_204 (117) = happyShift action_139
action_204 (118) = happyShift action_140
action_204 (161) = happyShift action_141
action_204 (10) = happyGoto action_134
action_204 (11) = happyGoto action_135
action_204 (45) = happyGoto action_136
action_204 _ = happyReduce_86

action_205 _ = happyReduce_147

action_206 (67) = happyGoto action_227
action_206 (72) = happyGoto action_228
action_206 (95) = happyGoto action_43
action_206 (100) = happyGoto action_44
action_206 _ = happyReduce_163

action_207 (113) = happyShift action_137
action_207 (114) = happyShift action_138
action_207 (117) = happyShift action_139
action_207 (118) = happyShift action_140
action_207 (128) = happyShift action_226
action_207 (161) = happyShift action_141
action_207 (10) = happyGoto action_134
action_207 (11) = happyGoto action_135
action_207 (45) = happyGoto action_136
action_207 _ = happyFail

action_208 (111) = happyShift action_2
action_208 (112) = happyShift action_8
action_208 (116) = happyShift action_9
action_208 (143) = happyShift action_58
action_208 (155) = happyShift action_59
action_208 (159) = happyShift action_60
action_208 (4) = happyGoto action_53
action_208 (9) = happyGoto action_142
action_208 (49) = happyGoto action_222
action_208 (50) = happyGoto action_223
action_208 (51) = happyGoto action_144
action_208 (52) = happyGoto action_145
action_208 (58) = happyGoto action_56
action_208 (64) = happyGoto action_57
action_208 (77) = happyGoto action_224
action_208 (89) = happyGoto action_225
action_208 _ = happyReduce_133

action_209 _ = happyReduce_112

action_210 _ = happyReduce_110

action_211 _ = happyReduce_109

action_212 (122) = happyShift action_66
action_212 (129) = happyShift action_67
action_212 (136) = happyShift action_68
action_212 (148) = happyShift action_69
action_212 (46) = happyGoto action_221
action_212 (47) = happyGoto action_62
action_212 (73) = happyGoto action_63
action_212 (101) = happyGoto action_64
action_212 (110) = happyGoto action_65
action_212 _ = happyReduce_193

action_213 _ = happyReduce_118

action_214 _ = happyReduce_81

action_215 _ = happyReduce_82

action_216 (111) = happyShift action_2
action_216 (112) = happyShift action_8
action_216 (116) = happyShift action_9
action_216 (143) = happyShift action_58
action_216 (155) = happyShift action_59
action_216 (159) = happyShift action_60
action_216 (4) = happyGoto action_53
action_216 (9) = happyGoto action_54
action_216 (52) = happyGoto action_216
action_216 (58) = happyGoto action_56
action_216 (64) = happyGoto action_57
action_216 (94) = happyGoto action_220
action_216 _ = happyReduce_161

action_217 _ = happyReduce_131

action_218 _ = happyReduce_99

action_219 (114) = happyShift action_138
action_219 (118) = happyShift action_140
action_219 (11) = happyGoto action_154
action_219 _ = happyReduce_184

action_220 _ = happyReduce_160

action_221 (113) = happyShift action_137
action_221 (114) = happyShift action_138
action_221 (117) = happyShift action_139
action_221 (118) = happyShift action_140
action_221 (161) = happyShift action_141
action_221 (10) = happyGoto action_134
action_221 (11) = happyGoto action_135
action_221 (45) = happyGoto action_136
action_221 _ = happyReduce_181

action_222 _ = happyReduce_132

action_223 (114) = happyShift action_138
action_223 (118) = happyShift action_140
action_223 (151) = happyShift action_240
action_223 (11) = happyGoto action_154
action_223 _ = happyFail

action_224 _ = happyReduce_151

action_225 (158) = happyShift action_238
action_225 (163) = happyShift action_239
action_225 _ = happyFail

action_226 (122) = happyShift action_66
action_226 (129) = happyShift action_67
action_226 (136) = happyShift action_68
action_226 (148) = happyShift action_69
action_226 (46) = happyGoto action_237
action_226 (47) = happyGoto action_62
action_226 (73) = happyGoto action_63
action_226 (101) = happyGoto action_64
action_226 (110) = happyGoto action_65
action_226 _ = happyReduce_193

action_227 (163) = happyShift action_236
action_227 _ = happyFail

action_228 (111) = happyShift action_2
action_228 (155) = happyShift action_32
action_228 (4) = happyGoto action_17
action_228 (16) = happyGoto action_18
action_228 (26) = happyGoto action_235
action_228 (27) = happyGoto action_22
action_228 (29) = happyGoto action_23
action_228 (61) = happyGoto action_24
action_228 (90) = happyGoto action_26
action_228 _ = happyFail

action_229 _ = happyReduce_156

action_230 (111) = happyShift action_2
action_230 (155) = happyShift action_32
action_230 (4) = happyGoto action_192
action_230 (16) = happyGoto action_18
action_230 (27) = happyGoto action_193
action_230 (29) = happyGoto action_194
action_230 (42) = happyGoto action_234
action_230 (61) = happyGoto action_24
action_230 (90) = happyGoto action_26
action_230 _ = happyFail

action_231 _ = happyReduce_121

action_232 _ = happyReduce_70

action_233 _ = happyReduce_71

action_234 _ = happyReduce_178

action_235 _ = happyReduce_146

action_236 (131) = happyShift action_243
action_236 _ = happyFail

action_237 (113) = happyShift action_137
action_237 (114) = happyShift action_138
action_237 (117) = happyShift action_139
action_237 (118) = happyShift action_140
action_237 (161) = happyShift action_141
action_237 (10) = happyGoto action_134
action_237 (11) = happyGoto action_135
action_237 (45) = happyGoto action_136
action_237 _ = happyReduce_85

action_238 (111) = happyShift action_2
action_238 (112) = happyShift action_8
action_238 (116) = happyShift action_9
action_238 (143) = happyShift action_58
action_238 (155) = happyShift action_59
action_238 (159) = happyShift action_60
action_238 (4) = happyGoto action_53
action_238 (9) = happyGoto action_142
action_238 (49) = happyGoto action_222
action_238 (50) = happyGoto action_223
action_238 (51) = happyGoto action_144
action_238 (52) = happyGoto action_145
action_238 (58) = happyGoto action_56
action_238 (64) = happyGoto action_57
action_238 (77) = happyGoto action_242
action_238 _ = happyReduce_133

action_239 _ = happyReduce_88

action_240 (122) = happyShift action_66
action_240 (129) = happyShift action_67
action_240 (136) = happyShift action_68
action_240 (148) = happyShift action_69
action_240 (46) = happyGoto action_241
action_240 (47) = happyGoto action_62
action_240 (73) = happyGoto action_63
action_240 (101) = happyGoto action_64
action_240 (110) = happyGoto action_65
action_240 _ = happyReduce_193

action_241 (113) = happyShift action_137
action_241 (114) = happyShift action_138
action_241 (117) = happyShift action_139
action_241 (118) = happyShift action_140
action_241 (161) = happyShift action_141
action_241 (10) = happyGoto action_134
action_241 (11) = happyGoto action_135
action_241 (45) = happyGoto action_136
action_241 _ = happyReduce_97

action_242 _ = happyReduce_150

action_243 (122) = happyShift action_66
action_243 (129) = happyShift action_67
action_243 (136) = happyShift action_68
action_243 (148) = happyShift action_69
action_243 (46) = happyGoto action_244
action_243 (47) = happyGoto action_62
action_243 (73) = happyGoto action_63
action_243 (101) = happyGoto action_64
action_243 (110) = happyGoto action_65
action_243 _ = happyReduce_193

action_244 (113) = happyShift action_137
action_244 (114) = happyShift action_138
action_244 (117) = happyShift action_139
action_244 (118) = happyShift action_140
action_244 (161) = happyShift action_141
action_244 (10) = happyGoto action_134
action_244 (11) = happyGoto action_135
action_244 (45) = happyGoto action_136
action_244 _ = happyReduce_87

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (UserName (snd $ identifierName happy_var_1)
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (UserName (snd $ identifierName happy_var_1)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (UserName (snd $ identifierName happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (UserName (snd $ identifierName happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (QName (fst $ identifierName happy_var_1) VariableName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  8 happyReduction_6
happyReduction_6 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (QName (fst $ identifierName happy_var_1) VariableName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  9 happyReduction_7
happyReduction_7 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (QName (fst $ identifierName happy_var_1) ConstructorName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (QName (fst $ identifierName happy_var_1) ConstructorName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  10 happyReduction_9
happyReduction_9 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (QName (fst $ identifierName happy_var_1) VariableName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (QName (fst $ identifierName happy_var_1) VariableName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  11 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (QName (fst $ identifierName happy_var_1) ConstructorName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (QName (fst $ identifierName happy_var_1) ConstructorName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (QName (fst $ identifierName happy_var_1) TypeVariableName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (QName (fst $ identifierName happy_var_1) TypeVariableName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  13 happyReduction_15
happyReduction_15 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn13
		 (QName (fst $ identifierName happy_var_1) TypeConstructorName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn13
		 (QName (fst $ identifierName happy_var_1) TypeConstructorName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (QName (fst $ identifierName happy_var_1) TypeVariableName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (QName (fst $ identifierName happy_var_1) TypeVariableName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  15 happyReduction_19
happyReduction_19 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (QName (fst $ identifierName happy_var_1) TypeConstructorName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  15 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (QName (fst $ identifierName happy_var_1) TypeConstructorName (UserName $ snd $ identifierName happy_var_1)
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  16 happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  16 happyReduction_22
happyReduction_22 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  17 happyReduction_23
happyReduction_23 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  17 happyReduction_24
happyReduction_24 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  18 happyReduction_26
happyReduction_26 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  19 happyReduction_27
happyReduction_27 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happyMonadReduce 3 20 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( makeModule happy_var_1 happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_30 = happySpecReduce_3  21 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  21 happyReduction_31
happyReduction_31  =  HappyAbsSyn21
		 (["Main"]
	)

happyReduce_32 = happyReduce 7 22 happyReduction_32
happyReduction_32 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn88  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (happy_var_3 ++ concat happy_var_5
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 5 22 happyReduction_33
happyReduction_33 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (concat happy_var_3
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_1  23 happyReduction_34
happyReduction_34 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn23
		 (let QName a _ (UserName b) = happy_var_1 in a ++ [b]
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  24 happyReduction_35
happyReduction_35 (HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (ImportDeclaration happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 4 25 happyReduction_36
happyReduction_36 ((HappyAbsSyn34  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ([TopTypeDeclaration (fst happy_var_2) (TypeDeclaration (snd happy_var_2) happy_var_4)]
	) `HappyStk` happyRest

happyReduce_37 = happyReduce 4 25 happyReduction_37
happyReduction_37 ((HappyAbsSyn86  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ([TopTypeDeclaration (fst happy_var_2) (DataDeclaration (snd happy_var_2) happy_var_4)]
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 4 25 happyReduction_38
happyReduction_38 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (undefined
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_1  25 happyReduction_39
happyReduction_39 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn22
		 (fmap TopVariableDeclaration happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  26 happyReduction_40
happyReduction_40 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  26 happyReduction_41
happyReduction_41 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn26
		 ([VariableDeclaration (fst happy_var_1) (snd happy_var_1) happy_var_2]
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  27 happyReduction_42
happyReduction_42 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn90  happy_var_1)
	 =  HappyAbsSyn26
		 (fmap (flip SignatureDeclaration happy_var_3) happy_var_1
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  28 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn28
		 (Infixl
	)

happyReduce_44 = happySpecReduce_1  28 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn28
		 (Infixr
	)

happyReduce_45 = happySpecReduce_1  28 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn28
		 (Infix
	)

happyReduce_46 = happySpecReduce_2  29 happyReduction_46
happyReduction_46 (HappyAbsSyn68  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn29
		 ((happy_var_1, happy_var_2)
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  30 happyReduction_47
happyReduction_47 (HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  31 happyReduction_48
happyReduction_48 (HappyAbsSyn69  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn31
		 (DataConstructor happy_var_1 happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  32 happyReduction_49
happyReduction_49 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  33 happyReduction_50
happyReduction_50 (HappyAbsSyn70  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn33
		 ((happy_var_1, happy_var_2)
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  34 happyReduction_51
happyReduction_51 (HappyAbsSyn85  happy_var_1)
	 =  HappyAbsSyn34
		 (foldr (\a b -> makeTypeApplication TyArrow [a, b]) (last happy_var_1) (init happy_var_1)
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  35 happyReduction_52
happyReduction_52 (HappyAbsSyn75  happy_var_1)
	 =  HappyAbsSyn34
		 (makeTypeApplication (head happy_var_1) (tail happy_var_1)
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  36 happyReduction_53
happyReduction_53 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  36 happyReduction_54
happyReduction_54 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn34
		 (TyVariable happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  36 happyReduction_55
happyReduction_55 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn34
		 (case length happy_var_1 of
              1 -> head happy_var_1
              _ -> makeTypeApplication
                    (TyConstant (QName ["Primitive"] TypeConstructorName (UserName (replicate (length happy_var_1 - 1) ','))))
                    happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  36 happyReduction_56
happyReduction_56 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn34
		 (TyApplication (TyConstant (QName ["Primitive"] TypeConstructorName (UserName "[]"))) happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  37 happyReduction_57
happyReduction_57 _
	_
	 =  HappyAbsSyn34
		 (TyConstant (QName ["Primitive"] TypeConstructorName (UserName "()"))
	)

happyReduce_58 = happySpecReduce_3  37 happyReduction_58
happyReduction_58 _
	(HappyAbsSyn71  happy_var_2)
	_
	 =  HappyAbsSyn34
		 (TyConstant (QName ["Primitive"] TypeConstructorName (UserName (replicate (length happy_var_2) ',')))
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  37 happyReduction_59
happyReduction_59 _
	_
	 =  HappyAbsSyn34
		 (TyConstant (QName ["Primitive"] TypeConstructorName (UserName "[]"))
	)

happyReduce_60 = happySpecReduce_3  37 happyReduction_60
happyReduction_60 _
	_
	_
	 =  HappyAbsSyn34
		 (TyArrow
	)

happyReduce_61 = happySpecReduce_1  37 happyReduction_61
happyReduction_61 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn34
		 (TyConstant happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  38 happyReduction_62
happyReduction_62 _
	_
	 =  HappyAbsSyn38
		 (0
	)

happyReduce_63 = happySpecReduce_2  38 happyReduction_63
happyReduction_63 _
	_
	 =  HappyAbsSyn38
		 (0
	)

happyReduce_64 = happySpecReduce_2  39 happyReduction_64
happyReduction_64 _
	_
	 =  HappyAbsSyn39
		 (0
	)

happyReduce_65 = happySpecReduce_2  39 happyReduction_65
happyReduction_65 _
	_
	 =  HappyAbsSyn39
		 (0
	)

happyReduce_66 = happySpecReduce_2  40 happyReduction_66
happyReduction_66 _
	_
	 =  HappyAbsSyn40
		 (0
	)

happyReduce_67 = happySpecReduce_2  40 happyReduction_67
happyReduction_67 _
	_
	 =  HappyAbsSyn40
		 (0
	)

happyReduce_68 = happySpecReduce_2  41 happyReduction_68
happyReduction_68 _
	_
	 =  HappyAbsSyn41
		 (0
	)

happyReduce_69 = happySpecReduce_1  42 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn42
		 (0
	)

happyReduce_70 = happySpecReduce_2  42 happyReduction_70
happyReduction_70 _
	_
	 =  HappyAbsSyn42
		 (0
	)

happyReduce_71 = happySpecReduce_2  42 happyReduction_71
happyReduction_71 _
	_
	 =  HappyAbsSyn42
		 (0
	)

happyReduce_72 = happySpecReduce_1  43 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn43
		 (0
	)

happyReduce_73 = happyReduce 4 43 happyReduction_73
happyReduction_73 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn43
		 (0
	) `HappyStk` happyRest

happyReduce_74 = happyReduce 5 43 happyReduction_74
happyReduction_74 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn43
		 (0
	) `HappyStk` happyRest

happyReduce_75 = happySpecReduce_3  43 happyReduction_75
happyReduction_75 _
	_
	_
	 =  HappyAbsSyn43
		 (0
	)

happyReduce_76 = happyReduce 5 43 happyReduction_76
happyReduction_76 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn43
		 (0
	) `HappyStk` happyRest

happyReduce_77 = happySpecReduce_2  44 happyReduction_77
happyReduction_77 _
	_
	 =  HappyAbsSyn44
		 (0
	)

happyReduce_78 = happySpecReduce_2  44 happyReduction_78
happyReduction_78 _
	_
	 =  HappyAbsSyn44
		 (0
	)

happyReduce_79 = happySpecReduce_1  45 happyReduction_79
happyReduction_79 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn30
		 (lll $ EVariable happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  45 happyReduction_80
happyReduction_80 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn30
		 (lll $ EVariable happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  45 happyReduction_81
happyReduction_81 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (lll $ EVariable happy_var_2
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  45 happyReduction_82
happyReduction_82 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn30
		 (lll $ EVariable happy_var_2
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  46 happyReduction_83
happyReduction_83 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  46 happyReduction_84
happyReduction_84 (HappyAbsSyn30  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (lll $ EApplication (lll $ EApplication happy_var_2 happy_var_1) happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happyReduce 6 47 happyReduction_85
happyReduction_85 ((HappyAbsSyn30  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (lll $ ECase happy_var_2
                    [ (Pattern (PConstructor (QName ["Base"] ConstructorName (UserName "True")) []) [], happy_var_4)
                    , (Pattern (PConstructor (QName ["Base"] ConstructorName (UserName "False")) []) [], happy_var_6)
                    ]
	) `HappyStk` happyRest

happyReduce_86 = happyReduce 4 47 happyReduction_86
happyReduction_86 ((HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn74  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (makeLambda happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_87 = happyMonadReduce 8 47 happyReduction_87
happyReduction_87 ((HappyAbsSyn30  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn87  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( do
                      decls <- makeDeclarationMap $ foldr addBinding Map.empty (concat happy_var_4)
                      return $ lll $ ELet decls happy_var_8)
	) (\r -> happyReturn (HappyAbsSyn30 r))

happyReduce_88 = happyReduce 6 47 happyReduction_88
happyReduction_88 (_ `HappyStk`
	(HappyAbsSyn89  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (lll $ ECase happy_var_2 (fmap fromJust $ filter isJust $ happy_var_5)
	) `HappyStk` happyRest

happyReduce_89 = happySpecReduce_1  47 happyReduction_89
happyReduction_89 (HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn30
		 (foldl (\a b -> lll $ EApplication a b) (head happy_var_1) (tail happy_var_1)
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  48 happyReduction_90
happyReduction_90 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn30
		 (lll $ EVariable happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  48 happyReduction_91
happyReduction_91 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn30
		 (lll $ EVariable happy_var_1
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  48 happyReduction_92
happyReduction_92 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 (lll $ EInteger (integerValue happy_var_1)
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  48 happyReduction_93
happyReduction_93 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 (lll $ EChar (charValue happy_var_1)
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  48 happyReduction_94
happyReduction_94 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 (foldl
                  (\a b -> lll $ EApplication (lll $ EApplication (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName ":"))) a) (lll $ EChar b))
                  (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName "[]")))
                  (stringValue happy_var_1)
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  48 happyReduction_95
happyReduction_95 (HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn30
		 (case length happy_var_1 of
                    0 -> lll $ EVariable (QName ["Primitive"] ConstructorName (UserName "()"))
                    1 -> head happy_var_1
                    _ -> foldl (\a b -> lll $ EApplication a b) (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName $ replicate (length happy_var_1 - 1) ','))) happy_var_1
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  48 happyReduction_96
happyReduction_96 (HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn30
		 (foldl
                  (\a b -> lll $ EApplication (lll $ EApplication (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName ":"))) a) b)
                  (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName "[]")))
                  happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_3  49 happyReduction_97
happyReduction_97 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 ((happy_var_1, happy_var_3)
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  50 happyReduction_98
happyReduction_98 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_3  50 happyReduction_99
happyReduction_99 (HappyAbsSyn51  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (Pattern (PConstructor happy_var_2 [happy_var_1, happy_var_3]) []
	)
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  51 happyReduction_100
happyReduction_100 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_2  51 happyReduction_101
happyReduction_101 (HappyAbsSyn76  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn51
		 (Pattern (PConstructor happy_var_1 happy_var_2) []
	)
happyReduction_101 _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  52 happyReduction_102
happyReduction_102 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn52
		 (Pattern PWildcard [happy_var_1]
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_3  52 happyReduction_103
happyReduction_103 (HappyAbsSyn52  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn52
		 (let Pattern pat as = happy_var_3 in Pattern pat (happy_var_1 : as)
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_1  52 happyReduction_104
happyReduction_104 _
	 =  HappyAbsSyn52
		 (Pattern PWildcard []
	)

happyReduce_105 = happySpecReduce_1  52 happyReduction_105
happyReduction_105 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn52
		 (Pattern (PConstructor happy_var_1 []) []
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1  52 happyReduction_106
happyReduction_106 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn52
		 (case happy_var_1 of
                  []    -> Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName "()")) []) []
                  [pat] -> pat
                  ps    -> Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName $ replicate (length happy_var_1 - 1) ',')) happy_var_1) []
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  52 happyReduction_107
happyReduction_107 (HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn52
		 (foldr
                (\x y -> Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName ":")) [x, y]) [])
                (Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName "[]")) []) [])
                happy_var_1
	)
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_3  53 happyReduction_108
happyReduction_108 _
	(HappyAbsSyn34  happy_var_2)
	_
	 =  HappyAbsSyn53
		 (happy_var_2
	)
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  54 happyReduction_109
happyReduction_109 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn54
		 (happy_var_2
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_3  55 happyReduction_110
happyReduction_110 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn55
		 (happy_var_2
	)
happyReduction_110 _ _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_3  56 happyReduction_111
happyReduction_111 _
	(HappyAbsSyn80  happy_var_2)
	_
	 =  HappyAbsSyn56
		 (happy_var_2
	)
happyReduction_111 _ _ _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  57 happyReduction_112
happyReduction_112 _
	(HappyAbsSyn82  happy_var_2)
	_
	 =  HappyAbsSyn57
		 (happy_var_2
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_3  58 happyReduction_113
happyReduction_113 _
	(HappyAbsSyn83  happy_var_2)
	_
	 =  HappyAbsSyn58
		 (happy_var_2
	)
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_3  59 happyReduction_114
happyReduction_114 _
	(HappyAbsSyn84  happy_var_2)
	_
	 =  HappyAbsSyn59
		 (happy_var_2
	)
happyReduction_114 _ _ _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_3  60 happyReduction_115
happyReduction_115 _
	(HappyAbsSyn92  happy_var_2)
	_
	 =  HappyAbsSyn60
		 (happy_var_2
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_3  61 happyReduction_116
happyReduction_116 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn61
		 (happy_var_2
	)
happyReduction_116 _ _ _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_3  62 happyReduction_117
happyReduction_117 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn62
		 (happy_var_2
	)
happyReduction_117 _ _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_3  63 happyReduction_118
happyReduction_118 _
	(HappyAbsSyn82  happy_var_2)
	_
	 =  HappyAbsSyn63
		 (happy_var_2
	)
happyReduction_118 _ _ _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_3  64 happyReduction_119
happyReduction_119 _
	(HappyAbsSyn83  happy_var_2)
	_
	 =  HappyAbsSyn64
		 (happy_var_2
	)
happyReduction_119 _ _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_3  65 happyReduction_120
happyReduction_120 _
	(HappyAbsSyn34  happy_var_2)
	_
	 =  HappyAbsSyn65
		 (happy_var_2
	)
happyReduction_120 _ _ _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_3  66 happyReduction_121
happyReduction_121 _
	(HappyAbsSyn81  happy_var_2)
	_
	 =  HappyAbsSyn66
		 (happy_var_2
	)
happyReduction_121 _ _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  67 happyReduction_122
happyReduction_122 (HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn67
		 (reverse happy_var_1
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  68 happyReduction_123
happyReduction_123 (HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn68
		 (reverse happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_1  69 happyReduction_124
happyReduction_124 (HappyAbsSyn97  happy_var_1)
	 =  HappyAbsSyn69
		 (reverse happy_var_1
	)
happyReduction_124 _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_1  70 happyReduction_125
happyReduction_125 (HappyAbsSyn98  happy_var_1)
	 =  HappyAbsSyn70
		 (reverse happy_var_1
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  71 happyReduction_126
happyReduction_126 (HappyAbsSyn99  happy_var_1)
	 =  HappyAbsSyn71
		 (reverse happy_var_1
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_1  72 happyReduction_127
happyReduction_127 (HappyAbsSyn100  happy_var_1)
	 =  HappyAbsSyn72
		 (reverse happy_var_1
	)
happyReduction_127 _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1  73 happyReduction_128
happyReduction_128 (HappyAbsSyn101  happy_var_1)
	 =  HappyAbsSyn73
		 (reverse happy_var_1
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_1  74 happyReduction_129
happyReduction_129 (HappyAbsSyn102  happy_var_1)
	 =  HappyAbsSyn74
		 (reverse happy_var_1
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_1  75 happyReduction_130
happyReduction_130 (HappyAbsSyn103  happy_var_1)
	 =  HappyAbsSyn75
		 (reverse happy_var_1
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_2  76 happyReduction_131
happyReduction_131 (HappyAbsSyn94  happy_var_2)
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn76
		 (happy_var_1 : happy_var_2
	)
happyReduction_131 _ _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_1  77 happyReduction_132
happyReduction_132 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn77
		 (Just happy_var_1
	)
happyReduction_132 _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_0  77 happyReduction_133
happyReduction_133  =  HappyAbsSyn77
		 (Nothing
	)

happyReduce_134 = happySpecReduce_1  78 happyReduction_134
happyReduction_134 (HappyAbsSyn79  happy_var_1)
	 =  HappyAbsSyn78
		 (Just happy_var_1
	)
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_0  78 happyReduction_135
happyReduction_135  =  HappyAbsSyn78
		 (Nothing
	)

happyReduce_136 = happySpecReduce_2  79 happyReduction_136
happyReduction_136 (HappyAbsSyn66  happy_var_2)
	_
	 =  HappyAbsSyn79
		 (happy_var_2
	)
happyReduction_136 _ _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_1  80 happyReduction_137
happyReduction_137 (HappyAbsSyn104  happy_var_1)
	 =  HappyAbsSyn80
		 (reverse happy_var_1
	)
happyReduction_137 _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_1  81 happyReduction_138
happyReduction_138 (HappyAbsSyn105  happy_var_1)
	 =  HappyAbsSyn81
		 (reverse happy_var_1
	)
happyReduction_138 _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_1  82 happyReduction_139
happyReduction_139 (HappyAbsSyn106  happy_var_1)
	 =  HappyAbsSyn82
		 (reverse happy_var_1
	)
happyReduction_139 _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_1  83 happyReduction_140
happyReduction_140 (HappyAbsSyn107  happy_var_1)
	 =  HappyAbsSyn83
		 (reverse happy_var_1
	)
happyReduction_140 _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_1  84 happyReduction_141
happyReduction_141 (HappyAbsSyn108  happy_var_1)
	 =  HappyAbsSyn84
		 (reverse happy_var_1
	)
happyReduction_141 _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_3  85 happyReduction_142
happyReduction_142 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn85  happy_var_1)
	 =  HappyAbsSyn85
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_142 _ _ _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_1  85 happyReduction_143
happyReduction_143 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn85
		 ([happy_var_1]
	)
happyReduction_143 _  = notHappyAtAll 

happyReduce_144 = happySpecReduce_3  86 happyReduction_144
happyReduction_144 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn86  happy_var_1)
	 =  HappyAbsSyn86
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_144 _ _ _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_1  86 happyReduction_145
happyReduction_145 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn86
		 ([happy_var_1]
	)
happyReduction_145 _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_3  87 happyReduction_146
happyReduction_146 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn87
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_146 _ _ _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_1  87 happyReduction_147
happyReduction_147 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn87
		 ([happy_var_1]
	)
happyReduction_147 _  = notHappyAtAll 

happyReduce_148 = happySpecReduce_3  88 happyReduction_148
happyReduction_148 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn88  happy_var_1)
	 =  HappyAbsSyn88
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_148 _ _ _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_1  88 happyReduction_149
happyReduction_149 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn88
		 ([happy_var_1]
	)
happyReduction_149 _  = notHappyAtAll 

happyReduce_150 = happySpecReduce_3  89 happyReduction_150
happyReduction_150 (HappyAbsSyn77  happy_var_3)
	_
	(HappyAbsSyn89  happy_var_1)
	 =  HappyAbsSyn89
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_150 _ _ _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_1  89 happyReduction_151
happyReduction_151 (HappyAbsSyn77  happy_var_1)
	 =  HappyAbsSyn89
		 ([happy_var_1]
	)
happyReduction_151 _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_3  90 happyReduction_152
happyReduction_152 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn90  happy_var_1)
	 =  HappyAbsSyn90
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_152 _ _ _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_1  90 happyReduction_153
happyReduction_153 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn90
		 ([happy_var_1]
	)
happyReduction_153 _  = notHappyAtAll 

happyReduce_154 = happySpecReduce_3  91 happyReduction_154
happyReduction_154 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn91
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_154 _ _ _  = notHappyAtAll 

happyReduce_155 = happySpecReduce_1  91 happyReduction_155
happyReduction_155 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn91
		 ([happy_var_1]
	)
happyReduction_155 _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_3  92 happyReduction_156
happyReduction_156 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn92  happy_var_1)
	 =  HappyAbsSyn92
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_156 _ _ _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_1  92 happyReduction_157
happyReduction_157 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn92
		 ([happy_var_1]
	)
happyReduction_157 _  = notHappyAtAll 

happyReduce_158 = happySpecReduce_3  93 happyReduction_158
happyReduction_158 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn93  happy_var_1)
	 =  HappyAbsSyn93
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_158 _ _ _  = notHappyAtAll 

happyReduce_159 = happySpecReduce_1  93 happyReduction_159
happyReduction_159 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn93
		 ([happy_var_1]
	)
happyReduction_159 _  = notHappyAtAll 

happyReduce_160 = happySpecReduce_2  94 happyReduction_160
happyReduction_160 (HappyAbsSyn94  happy_var_2)
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn94
		 (happy_var_1 : happy_var_2
	)
happyReduction_160 _ _  = notHappyAtAll 

happyReduce_161 = happySpecReduce_0  94 happyReduction_161
happyReduction_161  =  HappyAbsSyn94
		 ([]
	)

happyReduce_162 = happySpecReduce_2  95 happyReduction_162
happyReduction_162 (HappyTerminal happy_var_2)
	(HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn95
		 (happy_var_2 : happy_var_1
	)
happyReduction_162 _ _  = notHappyAtAll 

happyReduce_163 = happySpecReduce_0  95 happyReduction_163
happyReduction_163  =  HappyAbsSyn95
		 ([]
	)

happyReduce_164 = happySpecReduce_2  96 happyReduction_164
happyReduction_164 (HappyAbsSyn52  happy_var_2)
	(HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn96
		 (happy_var_2 : happy_var_1
	)
happyReduction_164 _ _  = notHappyAtAll 

happyReduce_165 = happySpecReduce_0  96 happyReduction_165
happyReduction_165  =  HappyAbsSyn96
		 ([]
	)

happyReduce_166 = happySpecReduce_2  97 happyReduction_166
happyReduction_166 (HappyAbsSyn34  happy_var_2)
	(HappyAbsSyn97  happy_var_1)
	 =  HappyAbsSyn97
		 (happy_var_2 : happy_var_1
	)
happyReduction_166 _ _  = notHappyAtAll 

happyReduce_167 = happySpecReduce_0  97 happyReduction_167
happyReduction_167  =  HappyAbsSyn97
		 ([]
	)

happyReduce_168 = happySpecReduce_2  98 happyReduction_168
happyReduction_168 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn98  happy_var_1)
	 =  HappyAbsSyn98
		 (happy_var_2 : happy_var_1
	)
happyReduction_168 _ _  = notHappyAtAll 

happyReduce_169 = happySpecReduce_0  98 happyReduction_169
happyReduction_169  =  HappyAbsSyn98
		 ([]
	)

happyReduce_170 = happySpecReduce_2  99 happyReduction_170
happyReduction_170 (HappyTerminal happy_var_2)
	(HappyAbsSyn109  happy_var_1)
	 =  HappyAbsSyn99
		 (happy_var_2 : happy_var_1
	)
happyReduction_170 _ _  = notHappyAtAll 

happyReduce_171 = happySpecReduce_2  100 happyReduction_171
happyReduction_171 (HappyTerminal happy_var_2)
	(HappyAbsSyn95  happy_var_1)
	 =  HappyAbsSyn100
		 (happy_var_2 : happy_var_1
	)
happyReduction_171 _ _  = notHappyAtAll 

happyReduce_172 = happySpecReduce_2  101 happyReduction_172
happyReduction_172 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn110  happy_var_1)
	 =  HappyAbsSyn101
		 (happy_var_2 : happy_var_1
	)
happyReduction_172 _ _  = notHappyAtAll 

happyReduce_173 = happySpecReduce_2  102 happyReduction_173
happyReduction_173 (HappyAbsSyn52  happy_var_2)
	(HappyAbsSyn96  happy_var_1)
	 =  HappyAbsSyn102
		 (happy_var_2 : happy_var_1
	)
happyReduction_173 _ _  = notHappyAtAll 

happyReduce_174 = happySpecReduce_2  103 happyReduction_174
happyReduction_174 (HappyAbsSyn34  happy_var_2)
	(HappyAbsSyn97  happy_var_1)
	 =  HappyAbsSyn103
		 (happy_var_2 : happy_var_1
	)
happyReduction_174 _ _  = notHappyAtAll 

happyReduce_175 = happySpecReduce_3  104 happyReduction_175
happyReduction_175 (HappyAbsSyn40  happy_var_3)
	_
	(HappyAbsSyn104  happy_var_1)
	 =  HappyAbsSyn104
		 (happy_var_3 : happy_var_1
	)
happyReduction_175 _ _ _  = notHappyAtAll 

happyReduce_176 = happySpecReduce_1  104 happyReduction_176
happyReduction_176 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn104
		 ([happy_var_1]
	)
happyReduction_176 _  = notHappyAtAll 

happyReduce_177 = happySpecReduce_0  104 happyReduction_177
happyReduction_177  =  HappyAbsSyn104
		 ([]
	)

happyReduce_178 = happySpecReduce_3  105 happyReduction_178
happyReduction_178 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn105  happy_var_1)
	 =  HappyAbsSyn105
		 (happy_var_3 : happy_var_1
	)
happyReduction_178 _ _ _  = notHappyAtAll 

happyReduce_179 = happySpecReduce_1  105 happyReduction_179
happyReduction_179 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn105
		 ([happy_var_1]
	)
happyReduction_179 _  = notHappyAtAll 

happyReduce_180 = happySpecReduce_0  105 happyReduction_180
happyReduction_180  =  HappyAbsSyn105
		 ([]
	)

happyReduce_181 = happySpecReduce_3  106 happyReduction_181
happyReduction_181 (HappyAbsSyn30  happy_var_3)
	_
	(HappyAbsSyn106  happy_var_1)
	 =  HappyAbsSyn106
		 (happy_var_3 : happy_var_1
	)
happyReduction_181 _ _ _  = notHappyAtAll 

happyReduce_182 = happySpecReduce_1  106 happyReduction_182
happyReduction_182 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn106
		 ([happy_var_1]
	)
happyReduction_182 _  = notHappyAtAll 

happyReduce_183 = happySpecReduce_0  106 happyReduction_183
happyReduction_183  =  HappyAbsSyn106
		 ([]
	)

happyReduce_184 = happySpecReduce_3  107 happyReduction_184
happyReduction_184 (HappyAbsSyn50  happy_var_3)
	_
	(HappyAbsSyn107  happy_var_1)
	 =  HappyAbsSyn107
		 (happy_var_3 : happy_var_1
	)
happyReduction_184 _ _ _  = notHappyAtAll 

happyReduce_185 = happySpecReduce_1  107 happyReduction_185
happyReduction_185 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn107
		 ([happy_var_1]
	)
happyReduction_185 _  = notHappyAtAll 

happyReduce_186 = happySpecReduce_0  107 happyReduction_186
happyReduction_186  =  HappyAbsSyn107
		 ([]
	)

happyReduce_187 = happySpecReduce_3  108 happyReduction_187
happyReduction_187 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn108  happy_var_1)
	 =  HappyAbsSyn108
		 (happy_var_3 : happy_var_1
	)
happyReduction_187 _ _ _  = notHappyAtAll 

happyReduce_188 = happySpecReduce_1  108 happyReduction_188
happyReduction_188 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn108
		 ([happy_var_1]
	)
happyReduction_188 _  = notHappyAtAll 

happyReduce_189 = happySpecReduce_0  108 happyReduction_189
happyReduction_189  =  HappyAbsSyn108
		 ([]
	)

happyReduce_190 = happySpecReduce_2  109 happyReduction_190
happyReduction_190 (HappyTerminal happy_var_2)
	(HappyAbsSyn109  happy_var_1)
	 =  HappyAbsSyn109
		 (happy_var_2 : happy_var_1
	)
happyReduction_190 _ _  = notHappyAtAll 

happyReduce_191 = happySpecReduce_0  109 happyReduction_191
happyReduction_191  =  HappyAbsSyn109
		 ([]
	)

happyReduce_192 = happySpecReduce_2  110 happyReduction_192
happyReduction_192 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn110  happy_var_1)
	 =  HappyAbsSyn110
		 (happy_var_2 : happy_var_1
	)
happyReduction_192 _ _  = notHappyAtAll 

happyReduce_193 = happySpecReduce_0  110 happyReduction_193
happyReduction_193  =  HappyAbsSyn110
		 ([]
	)

happyNewToken action sts stk [] =
	action 165 165 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	t | isVariableIdentifier t -> cont 111;
	t | isConstructorIdentifier t -> cont 112;
	t | isSymbolIdentifier t -> cont 113;
	t | isSymbolConstructorIdentifier t -> cont 114;
	t | isQVariableIdentifier t -> cont 115;
	t | isQConstructorIdentifier t -> cont 116;
	t | isQSymbolIdentifier t -> cont 117;
	t | isQSymbolConstructorIdentifier t -> cont 118;
	(tokenToken -> TkInteger _) -> cont 119;
	(tokenToken -> TkChar _) -> cont 120;
	(tokenToken -> TkString _) -> cont 121;
	(tokenToken -> TkCase) -> cont 122;
	(tokenToken -> TkClass) -> cont 123;
	(tokenToken -> TkData) -> cont 124;
	(tokenToken -> TkDefault) -> cont 125;
	(tokenToken -> TkDeriving) -> cont 126;
	(tokenToken -> TkDo) -> cont 127;
	(tokenToken -> TkElse) -> cont 128;
	(tokenToken -> TkIf) -> cont 129;
	(tokenToken -> TkImport) -> cont 130;
	(tokenToken -> TkIn) -> cont 131;
	(tokenToken -> TkInfix) -> cont 132;
	(tokenToken -> TkInfixl) -> cont 133;
	(tokenToken -> TkInfixr) -> cont 134;
	(tokenToken -> TkInstance) -> cont 135;
	(tokenToken -> TkLet) -> cont 136;
	(tokenToken -> TkModule) -> cont 137;
	(tokenToken -> TkNewtype) -> cont 138;
	(tokenToken -> TkOf) -> cont 139;
	(tokenToken -> TkThen) -> cont 140;
	(tokenToken -> TkType) -> cont 141;
	(tokenToken -> TkWhere) -> cont 142;
	(tokenToken -> TkUnderscore) -> cont 143;
	(tokenToken -> TkDoubleDot) -> cont 144;
	(tokenToken -> TkColon) -> cont 145;
	(tokenToken  -> TkDoubleColon) -> cont 146;
	(tokenToken -> TkEqual) -> cont 147;
	(tokenToken -> TkLambda) -> cont 148;
	(tokenToken -> TkPipe) -> cont 149;
	(tokenToken  -> TkLArrow) -> cont 150;
	(tokenToken  -> TkRArrow) -> cont 151;
	(tokenToken -> TkAt) -> cont 152;
	(tokenToken -> TkTilde) -> cont 153;
	(tokenToken  -> TkFatArrow) -> cont 154;
	(tokenToken -> TkLParen) -> cont 155;
	(tokenToken -> TkRParen) -> cont 156;
	(tokenToken -> TkComma) -> cont 157;
	(tokenToken -> TkSemiColon) -> cont 158;
	(tokenToken -> TkLBracket) -> cont 159;
	(tokenToken -> TkRBracket) -> cont 160;
	(tokenToken -> TkBackTick) -> cont 161;
	(tokenToken -> TkLBrace) -> cont 162;
	(tokenToken -> TkRBrace) -> cont 163;
	(tokenToken -> TkEOF) -> cont 164;
	_ -> happyError' (tk:tks)
	}

happyError_ 165 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => ParseMonad a -> (a -> ParseMonad b) -> ParseMonad b
happyThen = (>>=)
happyReturn :: () => a -> ParseMonad a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> ParseMonad a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> ParseMonad a
happyError' = parseError

parseModule tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data ParseError = ParseError
                deriving (Show)

type ParseMonad = ExceptT ParseError (State Int)

runParse :: ParseMonad a -> State Int (Either ParseError a)
runParse = runExceptT

generateName :: ParseMonad SyntaxName
generateName = do
  n <- get
  modify (+ 1)
  return $ GeneratedName n

lll = Locate noLocation

makeLambda :: [Pattern SyntaxName] -> Expression SyntaxName -> Expression SyntaxName
makeLambda pats e = foldr (\pat e -> lll $ ELambda (UserName "_") $ lll $ ECase (lll $ EVariable (QName [] VariableName (UserName "_"))) [(pat, e)]) e pats

data VariableDeclaration = VariableDeclaration SyntaxName [Pattern SyntaxName] (Expression SyntaxName)
                         | SignatureDeclaration SyntaxName (MonoType SyntaxName)
                         | PatternDeclaration (Pattern SyntaxName) (Expression SyntaxName)

type BindingMap = Map SyntaxName (Maybe (MonoType SyntaxName), [([Pattern SyntaxName], Expression SyntaxName)])

addBinding :: VariableDeclaration -> BindingMap -> BindingMap
addBinding (VariableDeclaration n p e) m = case Map.lookup n m of
  Nothing               -> Map.insert n (Nothing, [(p, e)]) m
  Just (t, [])          -> Map.insert n (t, [(p, e)]) m
  Just (t, es@((p', e'):_))
    | length p' == length p -> Map.insert n (t, (p, e):es) m
    | otherwise -> error "Bad pattern length ..."
addBinding (SignatureDeclaration n t) m = case Map.lookup n m of
  Nothing           -> Map.insert n (Just t, []) m
  Just (Just _, _)  -> error "Multiple signatures"
  Just (Nothing, e) -> Map.insert n (Just t, e) m
-- addBinding (PatternDeclaration p e) m = case 

makeDeclarationMap :: BindingMap -> ParseMonad (DeclarationMap SyntaxName)
makeDeclarationMap mp =
    Map.fromList <$> (\(a, b) -> do
      b' <- makeDeclaration b
      return (a, b')
    ) `mapM` Map.toList mp
  where
    makeDeclaration (t, e@((pats, _):_)) = do
      let ne = length pats
      ns <- forM [1..ne] $ const generateName
      let decl = foldr
            (((.).(.)) lll ELambda)
            (case ne of
              0 -> snd (head e)
              1 ->
                lll $ ECase
                (lll . EVariable . QName [] VariableName $ head ns)
                (fmap (\(pat, e) -> (head pat, e)) e)
              _ ->
                lll $ ECase
                (makeApplication (lll $ EVariable (QName ["Primitive"] ConstructorName (UserName $ replicate (ne - 1) ','))) (lll . EVariable . QName [] VariableName <$> ns))
                (fmap (\(pat, e) -> (Pattern (PConstructor (QName ["Primitive"] ConstructorName (UserName $ replicate (ne - 1) ',')) pat) [], e)) e)
            )
            ns
      return $ Declaration t decl

data TopDeclaration = ImportDeclaration ModuleName
                    | TopVariableDeclaration VariableDeclaration
                    | TopTypeDeclaration SyntaxName (TypeDeclaration SyntaxName)

makeModule :: ModuleName -> [TopDeclaration] -> ParseMonad (Module SyntaxName)
makeModule n tds = do
  let (m, bs) = foldl
        (flip addTopDeclaration)
        (Module n Set.empty Map.empty Map.empty, Map.empty)
        tds
  ds <- makeDeclarationMap bs
  return $ m { moduleDeclarations = ds }
  where
    addTopDeclaration (ImportDeclaration impName) (m, bs) = (m { moduleImport = Set.insert impName (moduleImport m) }, bs)
    addTopDeclaration (TopVariableDeclaration v) (m, bs)  = (m, addBinding v bs)
    addTopDeclaration (TopTypeDeclaration a b) (m, bs)    = (m { moduleTypeDeclarations = Map.insert a b (moduleTypeDeclarations m) }, bs)

parseError x = error $ "Parse error : " ++ show x
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
