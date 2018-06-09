NB. Jgwf/init.ijs
NB. Able to extract the 32 seconds of data in H-H1_LOSC_4_V2-1126259446-32.gwf

require 'format/printf'
require 'arc/zlib'
verbose =: 0
CRC =: 128!:3
NB. routine to display sections of bytes along with numeric and position info
createR3 =: 4 : 0  NB. (# to drop, # to take) createR3 bytes
	'drop take' =. x
	part =. take {. drop }. y
	parti =. a. i. part
	(<"0 part), (<"0 parti),: <"0 drop + i.take
)
CHAR_U =: 4 : '(x+1);a.i. x{y'
CHARn =: 4 : '(+/x);(({.x)+i.}.x){y'  NB. (ix, leng) CHARn data
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
REAL_4n =: 4 : 0
	'ix leng' =. x
	(ix+4*leng);_1 fc (4*leng){. ix}. y
)
REAL_8  =: 4 : '(x+8);{._2 fc (x+i.8){y'
REAL_8n =: 4 : 0
	'ix leng' =. x
	(ix+8*leng);_2 fc (8*leng){. ix}. y
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
	assert 
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
  domore =. 1
  results =. 0$0
  ix =. x
  whilst. domore do.
	startix =. ix
	'ix length chkType class instance' =. ix getCommon y
	data =. length{.startix}. y
	select. class
	  case. 1 do.
		'ix FrSHname FrSHclass FrSHcomment FrSHchkSum' =. ix getFrSH y
		classNames =: classNames,<FrSHname
	  case. 2 do.
		'ix FrSEname FrSEclass FrSEcomment FrSEchkSum' =. ix getFrSE y
NB. 		smoutput 'FrSEname';FrSEname;'FrSEclass:';FrSEclass
		domore =. -. FrSEname-:'chkSumFile'
	  case. do.
		assert class e. classes
		className =. >({.I. class = classes){classNames
		'ix res' =. (ix;class) getFrDict y
		if. verbose do.smoutput 'just got class';(<className),(<instance),res end.
		results =. results,<(<className),(<instance),res
NB. 	      smoutput 'length of results';#results
	end.
  end.
  if. verbose do.smoutput 'end of getFrame:';'length:';length;'ix';ix;'class:';class;'inst:';instance end.
  ix;<results
)
splitBracks =: 3 : 0
	dims =. 0$0
	lbrack =. I.'['=y
	base =. _1}.(>:{.lbrack){.y
	for_brack. lbrack do.
		nth =. (>:brack)}. y
		rbrack =. {.I.']'=nth
		dims =. dims, <rbrack{.nth
	end.
	base;dims
)
doStop =: 3 : 0
	smoutput 'could stop here';y
)
getFrDict =: 4 : 0
	'ix class' =. x
	className =. >({.I. class = classes){classNames
NB. 	smoutput 'getFrDict';className
	assert [dict =. >({.I. class = classes){dicts
	res =. 0$<className
	for_row. dict do.
	  type =. >1{row
	  if. ']'=_1{type do. NB. type describes an array
		baseParams =. splitBracks type
		base =. >{.baseParams
		totalDim =. 1
		for_param. }.baseParams do.
			if. */(>param) e. '0123456789' do.
				totalDim =. totalDim * ".>param
			else.
			  vnth =. >{.(I.param = 0{"1 dict){res
			  if. vnth = <:2^32x do.
				vnth =. 0
			  end.
			  totalDim =. totalDim *vnth
			  assert totalDim < (#y)-ix
			end.
		end.
   		".'''ix val'' =. (ix, totalDim)' , base,'n y'
		if. (totalDim>1000) do.
			smoutput 'found large';totalDim;base
			smoutput 'class';class; (I. class=classes){classNames
			smoutput 'compression:'; ]compress=.>{.(I.(<'compress')=0{"1 dict){res
			vtype =. >{.(I.(<'type')=0{"1 dict){res
			if. *./doDecompress, (compress=257), (vtype=2) do.  NB. vtype=2 =>REAL8
			  charData =. zlib_uncompress val
			  'ignorex val' =. (0, (#charData)%8) REAL_8n charData
			  r8=: val		NB. kludge that can be deleted
			end.
		end.
	  else.
	  	". '''ix val'' =.ix ',type,' y' 	
	  end.
	  res =. res,<val
	end.
	ix;<res
)
getDict =: 4 : 0
  dict =. 0 3 $ 0
  ix =. x
  whilst. domore do.
	'ix length chkType class instance' =. ix getCommon y
	'ix FrSEname FrSEclass FrSEcomment FrSEchkSum' =. ix getFrSE y
	domore =. -. FrSEname-:'chkSum'
	dict =. dict, FrSEname;FrSEclass;FrSEcomment
  end.
  ix;<dict
)
getFrSH=: 4 : 0 NB. x is offset, y bytes
	'ix name' =. x STRING y
	'ix class' =. ix INT_2U y
	'ix comment' =. ix STRING y
	'ix chkSum' =. ix INT_4U y	
	'ix bdict' =. ix getDict y  NB. boxed dict
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
	'ix comment' =. ix STRING y
	'ix chkSum'  =. ix INT_4U y
	ix;name;class;comment;chkSum
)

parseGWF =: 3 : 0  NB. pass in gwf data
	doDecompress =: 1
	dicts =: 0;0
	classes =: 1 2
	classNames =: 'FrSH';'FrSE'
	]fileHeaderLength =. validateFileHeader y
	'ix frames' =: fileHeaderLength getFrame y
	frames
)

findVect =: 4 : 0  NB. name findVect frames
   for_frame. frames do.
	if. 'FrVect' -: >0{>frame do.
	   opFrame =. >frame
NB. 	   smoutput 'vector';>2{opFrame
	   if. x-:>2{opFrame do.
NB. 		smoutput 'found Vector';x
NB. 		size =. 5{opFrame
		>7{opFrame
		return.
	   end.
	end.
   end.
)

test=: 3 : 0
	hgwf =. fread jpath'~user/projects/Jgwf/samples/H-H1_LOSC_4_V2-1126259446-32.gwf'
	NB.hgwf =. fread jpath'~user/projects/Jgwf/samples/H-H1_LOSC_CLN_4_V1-1187007040-2048.gwf'
	smoutput 'length of hwwf:';#hgwf
	frames =. parseGWF hgwf
	smoutput 'test found';(#frames);'frames'
)
NB.   plot 90 {.(#r8)%2}. r8
NB.r8mid2sec =. (+:4096){.(((#r8)%2)-4096)}.r8
NB.r8mid1sec =. 4096{.(((#r8)%2)-4096%2)}.r8