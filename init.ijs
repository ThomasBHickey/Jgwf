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

classes =: 1,2  NB.  the only two classes to start with
real8  =: 4 : '_2 fc (x+i.8){y'
char_u =: 4 : '(x+1);a.i. x{y'
int2_u =: 4 : '(x+2);0 ic (x,x+1){y'
int2_s =: 4 : '(x+2);_1 ic (x,x+1){y'
int4_u =: 4 : '(x+4);{.256#. |. a. i. (x+i.4){y'
int4_s =: 4 : '(x+4);_2 ic (x+i.4){y'

int8_u =: 4 : '(x+8);{.256#. |. a. i. (x+i.8){y'
int8_s =: 4 : '(x+8);_3 ic (x+i.8){y'
real4  =: 4 : '(x+4);_1 fc (x+i.4){y'
real8  =: 4 : '(x+8);_2 fc (x+i.8){y'
nstr0 =: 4 : 0  NB. 2 byte length + null terminated string (null not returned)
	'ix slen' =. x int2_u y
	(ix+{.slen);(ix+i.<:slen) { y
)

validateFileHeader =: 3 : 0  NB. pass in file bytes
	assert ('IGWD',{.a.)-:5 {. y
	assert 8 1-:>1{5 6 char_u y NB. Format version = 8.1?
	assert 2 4 8 4 8 -: >1{7 8 9 10 11 char_u y  NB. Expected sizes of numeric types
	assert 16b1234 = >1{12 int2_u y
	assert 16b12345678 = >1{14 int4_u y
	NB. assert 16b123456789abcdef = _3 ic (18+i.8) { y NB. the way it should work!
	assert 81985529216486895 = >1{18 int8_u y  NB. J seems to not be quite right
	assert 0.00001 > 1p1 - >1{26 real4 y
	assert 0.00001 > 1p1 - >1{30 real8 y
	assert 'frameCPP'-: >(>1{38 char_u y) { 'unknown';'frameL';'frameCPP' NB. Frame library
	assert 'CRC'-: >(>1{39 char_u y) { 'none';'CRC'  NB. Checksum scheme
	40 NB. length of file header
)

getCommon =: 4 : 0  NB. x=offset, y=bytes
	'ix length' =. x int8_u y
	'ix chkType' =. ix char_u y
	'ix class' =.   ix char_u y 
	'ix instance' =. ix int4_u y
	 ix;length;chkType;class;instance
)

getFrame=: 4 : 0 NB. x is offset, y bytes
  domore =. 100
  whilst. domore do.
	'ix length chkType class instance' =. x getCommon y
	smoutput 'ix:';ix;'length:';length
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
  dict =. 0 3 $ 'name';'class';'comment'
  ix =. x
  whilst. domore do.
	'ix length chkType class instance' =. ix getCommon y
	assert class=2
	'ix FrSEname FrSEclass FrSEcomment FrSEchkSum' =. ix getFrSE y
	domore =. -. FrSEname-:'chkSum'
	dict =. dict, FrSEname;FrSEclass;FrSEcomment
  end.
  smoutput 'got Dict!'
  smoutput 'ix $dict';ix;($ix);#dict
  ix;<dict
)
		
getFrSH=: 4 : 0 NB. x is offset, y bytes
	'ix name' =. x nstr0 y
	'ix class' =. ix int2_u y
  smoutput 'getFrSH defining class';ix;class
	'ix comment' =. ix nstr0 y
  smoutput 'getFrSH comment';ix;comment
	'ix chkSum' =. ix int4_u y	
	smoutput 'getFrSH';name;class;comment;chkSum
	smoutput 'in getFrSH: ix';ix;'$ix',$ix
	'ix bdict' =. ix getDict y
	dict =. >bdict
	dicts =: dicts,dict
	classes =: classes,class
	NB.smoutput 'here is dict'
	NB.smoutput dict 
	ix;name;class;comment;chkSum
)

getFrSE=: 4 : 0 NB. x is offset, y bytes
	'ix name' =. x nstr0 y
	'ix class'=. ix nstr0 y
	'ix comment' =. ix nstr0 y
	'ix chkSum'  =. ix int4_u y
	NB. smoutput 'getFrSE';name;class;comment;chkSum
	ix;name;class;comment;chkSum
)

getFrameH=: 4 : 0 NB. x is offset into y bytes
	NB.smoutput (x,40) createR3 y
	'ix name' =. x nstr0 y
	'ix run' =. ix int4_s y
	'ix frame' =. ix int4_u y
	'ix dataQuality' =. ix int4_u y
	'ix GTimeS' =. ix int4_u y
	'ix GTimeN' =. ix int4_u y
	'ix ULeapS' =. ix int2_u y
	'ix dt' =. ix real8 y
	smoutput 'getFRameH';name;run;frame;GTimeS;GTimeN;ULeapS;dt
)

dicts =: 0$0
classes =: 1 2
hgwf =: fread 'c://Users/Thom/gravity/h.gwf'

]fileHeaderLength =. validateFileHeader hgwf
fileHeaderLength getFrame hgwf
