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

hgwf =: fread 'c://Users/Thom/gravity/h.gwf'
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

p =. 40
]r31 =. (p, 64) createR3 hgwf

]frameLength =.   {._3 ic (p+i.8) { hgwf
]framechkType =. ({.a.i.(p+8){hgwf) { 'none';'CRC'
]frameclass =.   {.a.i.(p+9){ hgwf 
]frameinstance =. {._2 ic (p+10+i.4) { hgwf
]namelength =.{._1 ic (14+i.2){f2
]framename =. (16+i.<:namelength){f2
]classnum =. {._1 ic (16+namelength+i.2){f2 
]commentlength =. {._1 ic (18+namelength+i.2){f2
]comment =. (20+namelength+i.<:commentlength){f2
]chksum =. _2 ic (20+namelength+commentlength+i.4){f2
p =. 40+frameLength
]r32 =. (p, 64) createR3 hgwf
]fhsLength =. _3 ic (p+i.8) { hgwf
]fhsChecksumScheme =. >(39{fHdri) { 'none';'CRC'

]fhsFrameclass =. a. i. (p+8) { hgwf
]fhsInstance =. _2 ic (p+9+i.4) { hgwf

