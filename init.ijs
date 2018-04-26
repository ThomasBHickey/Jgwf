NB. Jgwf/init.ijs
NB. Nothing here yet!  Still trying to work my way through the various data structures.

NB. routine to display sections of bytes along with numeric and position info
createR3 =: 4 : 0  NB. (# to drop, # to take) createR3 bytes
	'drop take' =. x
	part =. take {. drop }. y
	parti =. a. i. part
	(<"0 part), (<"0 parti),: <"0 drop + i.take
)
NB.  Working with H-H1_LOSC_4_V2-1126259446-32.gwf

char_u =: 4 : 'a.i. x{y'
int2_u =: 4 : '0 ic (x,>:x){y'
int2_s =: 4 : '_1 ic (x,>:x){y'
int4_u =: 4 : '{.256#. |. a. i. (x+i.4){y'
int4_s =: 4 : '_2 ic (x,>:x){y'
int8_u =: 4 : '{.256#. |. a. i. (x+i.8){y'
int8_s =: 4 : '_3 ic (x+i.8){y'
real4  =: 4 : '_1 fc (x+i.4){y'
real8  =: 4 : '_2 fc (x+i.8){y'
nstr0 =: 4 : 0  NB. 2 byte length + null terminated string (null not returned)
	slen =. x int2_u y
	(x+2+i.<:slen) { y
)

hgwf =: fread 'c://Users/Thom/gravity/h.gwf'
NB. File Header -- FrHeader
'HEADER:'
assert 'IGWD'-:4{.hgwf

assert 8 1=5 6 char_u hgwf NB. Format version = 8.1?
assert 2 4 8 4 8 -: 7 8 9 10 11 char_u hgwf  NB. Expected sizes of numeric types
assert 16b1234 = 12 int2_u hgwf
assert 16b12345678 = 14 int4_u hgwf
NB. assert 16b123456789abcdef = _3 ic (18+i.8) { hgwf NB. the way it should work!
assert 81985529216486895 = 18 int8_u hgwf  NB. J seems to not be quite right

assert 0.00001 > 1p1 - 26 real4 hgwf
assert 0.00001 > 1p1 - 30 real8 hgwf

]frameLibrary =. >(38 char_u hgwf) { 'unknown';'frameL';'frameCPP'
]checksumScheme =. >(39 char_u hgwf) { 'none';'CRC'

NB. Start of frame
framep =. 40
'FRAME:'
]frameLength =.   framep int8_u hgwf
]r31 =. (framep, frameLength) createR3 hgwf
]framechkType =. >((p=.framep+8) char_u hgwf) { 'none';'CRC'
]frameclass =.   (p=.p+1) char_u hgwf 
]frameinstance =. (p=.p+1) int4_u hgwf
]framename =. (p=.p+4) nstr0 hgwf
]classnum =. (p=.p+2+>:#framename) int2_u hgwf
]comment =. (p=.p+2) nstr0 hgwf
]chksum =. (p=.p+2+>:#comment) int4_u hgwf

'FRSE'
]frsep=. framep+frameLength
]frseLength =. (p=.frsep) int8_u hgwf
]r32 =. (frsep, frseLength) createR3 hgwf
]frseChecksumScheme =. >((p=.p+8) char_u hgwf) { 'none';'CRC'

]frseFrameclass =. (p=.>:p) char_u hgwf
]frseInstance =. (p=.>:p) int4_u hgwf
]frseName =. (p=.p+4) nstr0 hgwf
]frseClass =.(p=.p+2+>:#frseName) nstr0 hgwf
]frseComment =. (p=.p+2+>:#frseClass) nstr0 hgwf
]frsechkSum =. (p=.p+2+>:#frseComment) int4_u hgwf

]nextp =. frsep + frseLength
(nextp,64) createR3 hgwf
 



