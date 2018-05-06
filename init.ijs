NB. Jgwf/init.ijs
NB. Able to extract the 32 seconds of data in H-H1_LOSC_4_V2-1126259446-32.gwf

require 'format/printf'
require 'arc/zlib'
NB. routine to display sections of bytes along with numeric and position info
createR3 =: 4 : 0  NB. (# to drop, # to take) createR3 bytes
	'drop take' =. x
	part =. take {. drop }. y
	parti =. a. i. part
	(<"0 part), (<"0 parti),: <"0 drop + i.take
)

CHAR_U =: 4 : '(x+1);a.i. x{y'
NB. CHAR2 =: 4 : '(x+2);(x,x+1){y'
CHARn =: 4 : 0
	'ix leng' =. x
	smoutput 'CHARn';ix;leng
	(ix+leng);(ix + i.leng) {y
)
NB. CHARnBytes =: 4 : 0
NB. 	'ix leng' =. x
NB. 	ix doNothing leng
NB. 	(ix+leng); leng {. ix}. y
NB. )
INT_2U =: 4 : '(x+2);{.0 ic (x,x+1){y'
INT_2Un =: 4 : 0
	'ix leng' =. x
	(ix+leng*2); 0 ic (ix+i.leng*2){y
)
INT_2S =: 4 : '(x+2);{._1 ic (x,x+1){y'
INT_4U =: 4 : '(x+4);{.256#. |. a. i. (x+i.4){y'
INT_4Un =: 4 : 0
	'ix leng' =. x
	res =. 0$0
	for. i. leng do.
	  'ix val' =. ix INT_4U y
	  res =. res,val
	end.
	ix;res
)
INT_4S =: 4 : '(x+4);{._2 ic (x+i.4){y'
INT_4Sn =: 4 : 0
	'ix leng' =. x
	(ix + leng*4); _2 ic (ix+i.leng*4){y
)
INT_8U =: 4 : '(x+8);{.256#. |. a. i. (x+i.8){y'
INT_8Un =: 4 : 0
	'ix leng' =. x
	res =. 0$0
	for. i.leng do.
	  'ix val' =. ix INT_8U y
	  res =. res, val
	end.
	ix;res
)
INT_8S =: 4 : '(x+8);{._3 ic (x+i.8){y'
REAL_4  =: 4 : '(x+4);{._1 fc (x+i.4){y'
REAL_8  =: 4 : '(x+8);{._2 fc (x+i.8){y'
REAL_8n =: 4 : 0
	'ix leng' =. x
	if. leng<100 do.  NB. Use selection
		(ix+8*leng);_2 fc ((0{ix) + i.8*leng){y
	else.
		(ix+8*leng);_2 fc (8*leng){. ix}. y
	end.
)
STRING =: 4 : 0  NB. 2 byte length + null terminated string (null not returned)
	'ix leng' =. x INT_2U y
	(ix+leng); (ix+i.<:leng){y
)
STRINGn =: 4 : 0 NB. returns (potentially) multiple strings
	'ix num' =. x
	res =. 0$0
	for. i.num do.
	  'ix str' =. ix STRING y
	  res =. res,<str
	end.
	ix;<res
)
PTR_STRUCT =: 4  : '(x+6);(x+i.6){y'
COMPLEX_8	=: 4 : 0
	'ix r' =. x REAL_4 y
	'ix c' =. ix REAL_4 y
	ix;(+.^:_1) r c
)
COMPLEX_16	=: 4 : 0
	'ix r' =. x REAL_8 y
	'ix c' =. ix REAL_8 y
	 ix;(+.^:_1) r c
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
  domore =. 100
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
		assert class e. classes
		'ix res' =. (ix;class) getFrDict y 
		smoutput 'class';class;'result';res
	end.
	domore =. <:domore
  end.
  smoutput 'ending getFrame'
  smoutput 'getFrame:';'length:';length;'ix';ix;'class:';class;'inst:';instance
  ix;FrSHname;FrSHclass;FrSHcomment;FrSHchkSum
)
splitBrack =: 3 : 0
	lbrack =: {.I.'['=y
	nth =. _1}. (>:lbrack)}. y
	(lbrack{.y);nth
)
doNothing =: 4 : 0
	smoutput 'doNothing';x;y
	x;'nothing'
)
doStop =: 3 : 0
	smoutput 'could stop here';y
)
getFrDict =: 4 : 0
	smoutput 'getFrDict'
	'ix class' =. x
	assert [dict =. >({.I. class = classes){dicts
	res =. 0$0
	for_row. dict do.
	  NB.smoutput 'getFrDict row';row
	  type =. >1{row
	  if. ']'=_1{type do. NB. type describes an array
		'base nth' =. splitBrack type
		smoutput 'looking for nth';nth; 'for';base
		if. */(nth) e. '0123456789' do.
		   smoutput 'nthcall'; nth,' n',base
		   smoutput '''ix val'' =. (ix, nth) ', base,'n y'
		   ".'''ix val'' =. (ix, ',nth,') ', base,'n y'
		   smoutput 'nth returns';ix;val
		else.
			smoutput 'bracketed type'; type; base; nth
			vnth =. >(I.(<nth) = 0{"1 dict){res
			smoutput 'vnth';vnth; '$vnth'; $vnth
			vnth doNothing base
			smoutput 'trying';'''ix val''=. (ix;vnth)', base,'n y'
			".'''ix val''=. (ix;vnth)', base,'n y'
			smoutput 'vnth returned';val;a. i. 4{. val
NB. 			if. (_1{ 2{. val) = 1{a. do.
NB. 			   doStop val
NB. 		      end.
		end.			
NB. 	  elseif. 'nAuxParam'-:_9{.type do.
NB. 		smoutput 'type:';type
NB. 		smoutput 'res so far:'; res; _1{res
NB. 		assert 0=>_1{res
NB. 		val =. 0
NB.        elseif. type -: 'CHARnBytes' do.
NB. 		smoutput 'type:'; type
NB. 		smoutput 'res so far:';res;_1{res
NB. 		smoutput '2{res:'; 2{res
NB. 		assert 2=>2{res  NB. GZip
NB. 		zleng =. _1 {res
NB. 		'ix zdata' =.(ix; zleng) CHARnBytes y
NB. 		NB. zdata fwrite jpath'~user/projects/Jgwf/samples/jgwf.zdata'
NB. 		if. doDecompress do.
NB. 		  smoutput 'decompressing';zleng; 'bytes of data'
NB. 		  uncomp =. zlib_uncompress zdata
NB. 		  NB. smoutput 'first real8'; 0 REAL_8 uncomp
NB. 		  reals =. _2 fc uncomp
NB. 		  smoutput 'length of reals';#reals
NB. 		  smoutput 'sample of reals:';(i.10){reals
NB. 		else.
NB. 		  smoutput'-----SKIPPING DECOMPRESS (doDecompress=0)----'
NB. 		end.
NB. 	  elseif. type -: 'INT_8UnDim' do.
NB. 		smoutput 'unDim'; _1{res
NB. 		assert 1=>_1{res
NB. 		'ix val' =. ix INT_8U y
NB. 		smoutput 'INT_8UnDim';val
NB. 	  elseif. type -: 'REAL_8nDim' do.
NB. 		smoutput 'REAL_8NDim'
NB. 		smoutput 'tail of res';_3{.res
NB. 		'ix val' =. ix REAL_8 y
NB. 	  elseif. type -: 'STRINGnDim' do.
NB. 		smoutput 'STRINGnDim'
NB. 		smoutput 'tail of res'; _3{.res
NB. 		'ix val' =. ix STRING y
	  elseif.do.
	  	". '''ix val'' =.ix ',type,' y' 	
	  NB. smoutput 'getFrDict';>0{row;type;val;ix
	  end.
	  res =. res,<val
	end.
	NB.smoutput 'getFrDict res';res
	ix;<res
)
	
getDict =: 4 : 0
  dict =. 0 3 $ 0
  ix =. x
  whilst. domore do.
	'ix length chkType class instance' =. ix getCommon y
	NB.assert class=2
	'ix FrSEname FrSEclass FrSEcomment FrSEchkSum' =. ix getFrSE y
	domore =. -. FrSEname-:'chkSum'
	dict =. dict, FrSEname;FrSEclass;FrSEcomment
  end.
  NB.smoutput 'got Dict: ix $dict';ix;($ix);#dict
  ix;<dict
)
		
getFrSH=: 4 : 0 NB. x is offset, y bytes
	'ix name' =. x STRING y
	'ix class' =. ix INT_2U y
	'ix comment' =. ix STRING y
	'ix chkSum' =. ix INT_4U y	
	smoutput 'getFrSH';name;class;comment;x
	'ix bdict' =. ix getDict y  NB. boxed dict
	NB.smoutput 'ix after getDict:';ix
	dicts =: dicts,<bdict  NB. rebox it
	classes =: classes,class
	ix;name;class;comment;chkSum
)

getFrSE=: 4 : 0 NB. x is offset, y bytes
	'ix name' =. x STRING y
	'ix class'=. ix STRING y
	if. 'PTR_STRUCT' -: (#'PTR_STRUCT'){. class do.
		class =. 'PTR_STRUCT'
	end.
NB. 	if. '[' e. class do.
NB. 		class =. (I. -. class e. '[]'){class
NB. 	end.
	'ix comment' =. ix STRING y
	'ix chkSum'  =. ix INT_4U y
	NB. smoutput 'getFrSE';name;class;comment;chkSum
	ix;name;class;comment;chkSum
)

NB. getFrameH=: 4 : 0 NB. x is offset into y bytes
NB. 	NB.smoutput (x,40) createR3 y
NB. 	'ix name' =. x STRING y
NB. 	'ix run' =. ix INT_4U y
NB. 	'ix frame' =. ix INT_4U y
NB. 	'ix dataQuality' =. ix INT_4U y
NB. 	'ix GTimeS' =. ix INT_4U y
NB. 	'ix GTimeN' =. ix INT_4U y
NB. 	'ix ULeapS' =. ix INT_2U y
NB. 	'ix dt' =. ix REAL_8 y
NB. 	smoutput 'getFRameH';name;run;frame;GTimeS;GTimeN;ULeapS;dt
NB. 	ix
NB. )
NB.

hgwf =: fread jpath'~user/projects/Jgwf/samples/H-H1_LOSC_4_V2-1126259446-32.gwf'
doDecompress =: 0
runit =: 3 : 0
	dicts =: 0;0
	classes =: 1 2
	]fileHeaderLength =. validateFileHeader y
	smoutput 'getFrame result'; fileHeaderLength getFrame y
)
runit hgwf
