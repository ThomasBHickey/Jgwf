NB. Jgwf/init.ijs
NB. Nothing here yet!  Still trying to work my way through the various data structures.

createR3 =: 4 : 0  NB. (# to drop, # to take) createR3 bytes
	'drop take' =. x
	part =. take {. drop }. y
	parti =. a. i. part
	(<"0 part), (<"0 parti),: <"0 drop + i.take
)

hgwf =: fread 'c://Users/Thom/gravity/h.gwf'
fHdr =. 40 {. hgwf
assert 'IGWD'-:4{.fHdr
fHdri =. a. i. fHdr

assert 8 1=5 6{fHdri NB. Format version = 8.1?
assert 2 4 8 4 8 -: 7 8 9 10 11 { fHdri  NB. Expected sizes of numeric types
assert 16b1234 = _1 ic 12 13 { fHdr
assert 16b12345678 = _2 ic (14 + i.4) { fHdr
NB. assert 16b123456789abcdef = _3 ic (18+i.8) { fHdr  NB. the way it should work!
assert 81985529216486895 =  _3 ic (18+i.8) { fHdr  NB. J seems to not be quite right

assert 0.00001 > 1p1 - _1 fc (26+i.4) { fHdr
assert 0.00001 > 1p1 - _2 fc (30+i.8) { fHdr

]frameLibrary =. >(38{fHdri) { 'unknown';'frameL';'frameCPP'
]checksumScheme =. >(39{fHdri) { 'none';'CRC'

pos =. 40

f2 =. 64 {. 40 }. hgwf
f2i =. a. i. f2
r32 =. (<"0 f2), (<"0 f2i),: <"0 i.#f2
]frameLength =.   _3 ic (i.8) { f2
]framechkType =. (8{f2i) { 'none';'CRC'
]frameclass =. 9 { f2i
]frameinstance =. _2 ic (10+i.4) { f2
]namelength =.{._1 ic (14+i.2){f2
]framname =. (16+i.<:namelength){f2
]classnum =. {._1 ic (16+namelength+i.2){f2 
]commentlength =. {._1 ic (18+namelength+i.2){f2
]comment =. (20+namelength+i.<:commentlength){f2
]chksum =. _2 ic (20+namelength+commentlength+i.4){f2
p =. 20+namelength+commentlength+4
a. i. p{f2
f3 =. 84 {. p}. hgwf
f3i =. a. i. f3
r33 =.(<"0 f3), (<"0 f3i),: <"0 i.#f3

0 createR3 10{. hgwf
32 52 createR3 hgwf

