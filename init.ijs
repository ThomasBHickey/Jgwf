NB. Jgwf/init.ijs
NB. Nothing here yet!  Still trying to work my way through the various data structures.

require 'format/printf'
NB. routine to display sections of bytes along with numeric and position info
createR3 =: 4 : 0  NB. (# to drop, # to take) createR3 bytes
	'drop take' =. x
	part =. take {. drop }. y
	parti =. a. i. part
	(<"0 part), (<"0 parti),: <"0 drop + i.take
)
NB.  Working with H-H1_LOSC_4_V2-1126259446-32.gwf

CHAR_U =: 4 : '(x+1);a.i. x{y'
INT_2U =: 4 : '(x+2);0 ic (x,x+1){y'
INT_2S =: 4 : '(x+2);_1 ic (x,x+1){y'
INT_4U =: 4 : '(x+4);{.256#. |. a. i. (x+i.4){y'
INT_4S =: 4 : '(x+4);_2 ic (x+i.4){y'

INT_8U =: 4 : '(x+8);{.256#. |. a. i. (x+i.8){y'
INT_8S =: 4 : '(x+8);_3 ic (x+i.8){y'
REAL_4  =: 4 : '(x+4);_1 fc (x+i.4){y'
REAL_8  =: 4 : '(x+8);_2 fc (x+i.8){y'
STRING =: 4 : 0  NB. 2 byte length + null terminated string (null not returned)
	'ix slen' =. x INT_2U y
	(ix+{.slen);(ix+i.<:slen) { y
)
PTR_STRUCT =: 4  : '(x+6);(x+i.6){y'
COMPLEX_8	=: 4 : 0
	'xi r' =. x REAL_4 y
	'xi c' =. xi REAL_4 y
	xi;+.^:_1) r c
)
COMPLEX_16	=: 4 : 0
	'xi r' =. x REAL_8 y
	'xi c' =. xi REAL_8 y
	xi;+.^:_1) r c
)

validateFileHeader =: 3 : 0  NB. pass in file bytes
	assert ('IGWD',{.a.)-:5 {. y
	assert 8 1-:>1{5 6 CHAR_U y NB. Format version = 8.1?
	assert 2 4 8 4 8 -: >1{7 8 9 10 11 CHAR_U y  NB. Expected sizes of numeric types
	assert 16b1234 = >1{12 INT_2U y
	assert 16b12345678 = >1{14 INT_4U y
	NB. assert 16b123456789abcdef = _3 ic (18+i.8) { y NB. the way it should work!
	assert 81985529216486895 = >1{18 INT_8U y  NB. J seems to not be quite right
	assert 0.00001 > 1p1 - >1{26 REAL_4 y
	assert 0.00001 > 1p1 - >1{30 REAL_8 y
	assert 'frameCPP'-: >(>1{38 CHAR_U y) { 'unknown';'frameL';'frameCPP' NB. Frame library
	assert 'CRC'-: >(>1{39 CHAR_U y) { 'none';'CRC'  NB. Checksum scheme
	40 NB. length of file header
)

getCommon =: 4 : 0  NB. x=offset, y=bytes
	'ix length' =. x INT_8U y
	'ix chkType' =. ix CHAR_U y
	'ix class' =.   ix CHAR_U y 
	'ix instance' =. ix INT_4U y
	 ix;length;chkType;class;instance
)

getFrame=: 4 : 0 NB. x is offset, y bytes
  domore =. 10
  ix =. x
  whilst. domore do.
	'ix length chkType class instance' =. ix getCommon y
	smoutput 'getFrame domore loop ix:';ix;'length:';length;'class:';class
	select. class
	  case. 1 do.
		'ix FrSHname FrSHclass FrSHcomment FrSHchkSum' =. ix getFrSH y
	  case. 2 do.
		'ix FrSEname FRSEclass FRSEcomment FRSEchkSum' =. ix getFrSE y
	  case. do.
		if. class e. classes do.
			smoutput 'found used class';class
		else.
			smoutput 'New class in getFrame:';class
			ix getFrameH y
		end.
	end.
	domore =. <:domore
  end.
  smoutput 'ending getFrame'
  smoutput 'getFrame:';'length:';length;'ix';ix;'class:';class;'inst:';instance
  ix;FrSHname;FrSHclass;FrSHcomment;FrSHchkSum
)
getDict =: 4 : 0
  dict =. 0 3 $ 0
  ix =. x
  whilst. domore do.
	'ix length chkType class instance' =. ix getCommon y
	assert class=2
	'ix FrSEname FrSEclass FrSEcomment FrSEchkSum' =. ix getFrSE y
	domore =. -. FrSEname-:'chkSum'
	dict =. dict, FrSEname;FrSEclass;FrSEcomment
  end.
  smoutput 'got Dict: ix $dict';ix;($ix);#dict
  ix;<dict
)
		
getFrSH=: 4 : 0 NB. x is offset, y bytes
	'ix name' =. x STRING y
	'ix class' =. ix INT_2U y
	'ix comment' =. ix STRING y
	'ix chkSum' =. ix INT_4U y	
	smoutput 'getFrSH';name;class;comment;x
	'ix bdict' =. ix getDict y  NB. boxed dict
	smoutput 'ix after getDict:';ix
	dicts =: dicts,<bdict  NB. rebox it
	classes =: classes,class
	NB.smoutput 'here is dict'
	NB.smoutput dict 
	ix;name;class;comment;chkSum
)

getFrSE=: 4 : 0 NB. x is offset, y bytes
	'ix name' =. x STRING y
	'ix class'=. ix STRING y
	'ix comment' =. ix STRING y
	'ix chkSum'  =. ix INT_4U y
	NB. smoutput 'getFrSE';name;class;comment;chkSum
	ix;name;class;comment;chkSum
)

getFrameH=: 4 : 0 NB. x is offset into y bytes
	NB.smoutput (x,40) createR3 y
	'ix name' =. x STRING y
	'ix run' =. ix INT_4U y
	'ix frame' =. ix INT_4U y
	'ix dataQuality' =. ix INT_4U y
	'ix GTimeS' =. ix INT_4U y
	'ix GTimeN' =. ix INT_4U y
	'ix ULeapS' =. ix INT_2U y
	'ix dt' =. ix REAL_8 y
	smoutput 'getFRameH';name;run;frame;GTimeS;GTimeN;ULeapS;dt
)


hgwf =: fread 'c://Users/Thom/gravity/h.gwf'
runit =: 3 : 0
	dicts =: 0$0
	classes =: 0$0
	]fileHeaderLength =. validateFileHeader y
	fileHeaderLength getFrame y
)
runit hgwf
