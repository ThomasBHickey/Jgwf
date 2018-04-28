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
int4_s =: 4 : '_2 ic (x+i.4){y'
int8_u =: 4 : '{.256#. |. a. i. (x+i.8){y'
int8_s =: 4 : '_3 ic (x+i.8){y'
real4  =: 4 : '_1 fc (x+i.4){y'
real8  =: 4 : '_2 fc (x+i.8){y'
nstr0 =: 4 : 0  NB. 2 byte length + null terminated string (null not returned)
	slen =. x int2_u y
	(x+2+i.<:slen) { y
)

validateFileHeader =: 3 : 0  NB. pass in file bytes
	assert 'IGWD'-:4{.hgwf
	assert 8 1=5 6 char_u hgwf NB. Format version = 8.1?
	assert 2 4 8 4 8 -: 7 8 9 10 11 char_u hgwf  NB. Expected sizes of numeric types
	assert 16b1234 = 12 int2_u hgwf
	assert 16b12345678 = 14 int4_u hgwf
	NB. assert 16b123456789abcdef = _3 ic (18+i.8) { hgwf NB. the way it should work!
	assert 81985529216486895 = 18 int8_u hgwf  NB. J seems to not be quite right
	assert 0.00001 > 1p1 - 26 real4 hgwf
	assert 0.00001 > 1p1 - 30 real8 hgwf
	assert 'frameCPP'-: >(38 char_u hgwf) { 'unknown';'frameL';'frameCPP' NB. Frame library
	assert 'CRC'-: >(39 char_u hgwf) { 'none';'CRC'  NB. Checksum scheme
	40 NB. length of file header
)

getCommon =: 4 : 0  NB. x=offset, y=bytes
	length =.   x int8_u y
	chkType =. (x+8) char_u y
	class =.   (x+8+1) char_u hgwf 
	instance =. (x+8+1+1) int4_u hgwf
	length,chkType,class,instance
)

getFrame=: 4 : 0 NB. x is offset, y bytes
  p =. x
  domore =. 100
  whilst. domore do.
	'length chkType class instance' =. p getCommon y
	NB.smoutput (p, length) createR3 y
	select. class
	  case. 1 do.
		'FrSHname FrSHclass FrSHcomment FrSHchkSum' =. (p+14) getFrSH y
	  case. 2 do.
		'FrSEname FRSEclass FRSEcomment FRSEchkSum' =. (p+14) getFrSE y
	  case. 3 do.
		(p+14) getFrameH y
	  case. do. smoutput 'New class';class
			  domore =. 1
	end.
	p =. p+length
	domore =. <:domore
  end.
  smoutput 'ending getFrame'
  smoutput 'getFrame:';'length:';length;'p';p;'class:';class;'inst:';instance
  smoutput ((p+14-length), length) createR3 y
  FrSHname;FrSHclass;FrSHcomment;FrSHchkSum
)

getFrSH=: 4 : 0 NB. x is offset, y bytes
	p =. x
	name =. p nstr0 y
	class =. (p=.p+2+>:#name) int2_u y
	comment =. (p=.p+2) nstr0 y
	chkSum =. (p=.p+2+>:#comment) int4_u y	
	smoutput 'getFrSH';name;class;comment;chkSum
	name;class;comment;chkSum
)

getFrSE=: 4 : 0 NB. x is offset, y bytes
	p =. x
	name =. p nstr0 y
	class=. (p=.p+2+>:#name) nstr0 y
	comment =. (p=.p+2+>:#class) nstr0 y
	chkSum  =. (p=.p+2+>:#comment) int4_u y
	smoutput 'getFrSE';name;class;comment;chkSum
	name;class;comment;chkSum
)

getFrameH=: 4 : 0 NB. x is offset into y bytes
	NB.smoutput (x,40) createR3 y
	p =. x
	name =. p nstr0 y
	run =. (p=.p+2+>:#name) int4_s y
	frame =. (p=.p+4) int4_u y
	dataQuality =. (p=.p+4) int4_u y
	GTimeS =. (p=.p+4) int4_u y
	GTimeN =. (p=.p+4) int4_u y
	ULeapS =. (p=.p+4) int2_u y
	dt =. (p=.p+2) real8 y
	smoutput 'getFRameH';name;run;frame;GTimeS;GTimeN;ULeapS;dt
)

hgwf =: fread 'c://Users/Thom/gravity/h.gwf'
]fileHeaderLength =. validateFileHeader hgwf
fileHeaderLength getFrame hgwf
