/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Bruno Pagano, projet Cristal, INRIA Rocquencourt          //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

.class public Compare {

// polymorphic physical equality		
.method public static bool eq(object,object) il managed 
 {
  .maxstack 2
// if (x==y) return true ;
      	ldarg.0
      	ldarg.1
      	beq RETURN_TRUE
      	ldarg.0
	brtrue STEP1
// (y==0) is not posssible here because (x==y) failed
        ldc.i4.0
	ret // RETURN_FALSE

STEP1:	//here x!=0
// if (y==0) return false ;
	ldarg.1
	brnull RETURN_FALSE

//now x!=0 && y!=0

// 1st case: handle int32, char and bool uniformly
// try to push an int representation of x
	ldarg.0
	isinst [mscorlib]System.Int32
	brfalse X_NOT_INT32
	ldarg.0
	castclass [mscorlib]System.Int32
	unbox [mscorlib]System.Int32
	ldind.i4
	br X_AS_INT
   X_NOT_INT32:		
	ldarg.0
	isinst [mscorlib]System.Boolean
	brfalse X_NOT_BOOL
	ldarg.0
	castclass [mscorlib]System.Boolean
	unbox [mscorlib]System.Boolean
	ldind.i1
	br X_AS_INT
   X_NOT_BOOL:
	ldarg.0
	isinst [mscorlib]System.Char
	brfalse NOT_INTS
	ldarg.0
	castclass [mscorlib]System.Char
	unbox [mscorlib]System.Char
	ldind.i2
   X_AS_INT:
// now do the same for y
	ldarg.1
	isinst [mscorlib]System.Int32
	brfalse Y_NOT_INT32
	ldarg.1
	castclass [mscorlib]System.Int32
	unbox [mscorlib]System.Int32
	ldind.i4
	br Y_AS_INT
   Y_NOT_INT32:		
	ldarg.1
	isinst [mscorlib]System.Boolean
	brfalse Y_NOT_BOOL
	ldarg.1
	castclass [mscorlib]System.Boolean
	unbox [mscorlib]System.Boolean
	ldind.i1
	br Y_AS_INT
   Y_NOT_BOOL:
/* static typing should ensure y is a char by now !
	ldarg.1
	isinst [mscorlib]System.Char
	brfalse NOT_INTS 
*/
	ldarg.1
	castclass [mscorlib]System.Char
	unbox [mscorlib]System.Char
	ldind.i2
   Y_AS_INT:	
	ceq
	ret

   NOT_INTS:
// if (x boxfloat)
	ldarg.0
	isinst [mscorlib]System.Double
	brfalse X_NOT_BFLOAT
// if (y boxfloat) then (is_eq ?) else 0
	ldarg.1
	isinst [mscorlib]System.Double
	brnull RETURN_FALSE
	ldarg.0
	castclass [mscorlib]System.Double
	unbox [mscorlib]System.Double
	ldind.r8
	ldarg.1
	castclass [mscorlib]System.Double
	unbox [mscorlib]System.Double
	ldind.r8
	ceq
	ret
   X_NOT_BFLOAT:	
		
// if (x boxint64)
	ldarg.0
	isinst [mscorlib]System.Int64
	brfalse X_NOT_BINT64
// if (y boxint64) then (is_eq ?) else 0
	ldarg.1
	isinst [mscorlib]System.Int64
	brnull RETURN_FALSE
	ldarg.0
	castclass [mscorlib]System.Int64
	unbox [mscorlib]System.Int64
	ldind.i8
	ldarg.1
	castclass [mscorlib]System.Int64
	unbox [mscorlib]System.Int64
	ldind.i8
	ceq
	ret
   X_NOT_BINT64:

// if (x boxnint)
	ldarg.0
	isinst [mscorlib]System.IntPtr
	brfalse X_NOT_BNINT
// if (y boxnint) then (is_eq ?) else 0
	ldarg.1
	isinst [mscorlib]System.IntPtr
	brnull RETURN_FALSE
	ldarg.0
	castclass [mscorlib]System.IntPtr
	unbox [mscorlib]System.IntPtr
	ldind.i
	ldarg.1
	castclass [mscorlib]System.IntPtr
	unbox [mscorlib]System.IntPtr
	ldind.i
	ceq
	ret
   X_NOT_BNINT:	

// default case is FALSE
    RETURN_FALSE :
	ldc.i4.0
	ret 
    RETURN_TRUE :
	ldc.i4.1
	ret	
 }
	
// polymorphic physical inequality		
.method public static bool noteq(object,object) il managed 
 {
  .maxstack 2
	ldarg.0
	ldarg.1
	call bool CamIL.Compare::eq(object,object)
	ldc.i4.0
	ceq
	ret
}	
			
		
// polymorphic value comparison					
// result integer is negative for v1 < v2 , positive for v1 > v2 and 0 for v1=v2
	.method public static int32 compare_val(object,object) il managed 
 {
  .maxstack 3
  .locals init (float64, float64, object[],object[],
            int32, int32, int32, int64,int64, native int, native int, string)
TAIL_CALL:
// if (x==y) return 0
      	ldarg.0
      	ldarg.1
      	beq RETURN_0
// if (x==0) return -1
      	ldarg.0
	brtrue X_NOT_NULL
	ldc.i4.M1
	ret // RETURN_M1

X_NOT_NULL:
// if (y==0) return 1
	ldarg.1
	brtrue Y_NOT_NULL
	ldc.i4.1
	ret // RETURN_1

Y_NOT_NULL:
// 1st case: handle int32, char and bool uniformly	
// try to push an int representation of x
	ldarg.0
	isinst [mscorlib]System.Int32
	brfalse X_NOT_INT32
	ldarg.0
	castclass [mscorlib]System.Int32
	unbox [mscorlib]System.Int32
	ldind.i4
	stloc.s 4
	br X_AS_INT
   X_NOT_INT32:		
	ldarg.0
	isinst [mscorlib]System.Boolean
	brfalse X_NOT_BOOL
	ldarg.0
	castclass [mscorlib]System.Boolean
	unbox [mscorlib]System.Boolean
	ldind.i1
	stloc.s 4
	br X_AS_INT
   X_NOT_BOOL:
	ldarg.0
	isinst [mscorlib]System.Char
	brfalse NOT_INTS
	ldarg.0
	castclass [mscorlib]System.Char
	unbox [mscorlib]System.Char
	ldind.i2
	stloc.s 4
   X_AS_INT:
// now do the same for y
	ldarg.1
	isinst [mscorlib]System.Int32
	brfalse Y_NOT_INT32
	ldarg.1
	castclass [mscorlib]System.Int32
	unbox [mscorlib]System.Int32
	ldind.i4
	stloc.s 5
	br Y_AS_INT
   Y_NOT_INT32:		
	ldarg.1
	isinst [mscorlib]System.Boolean
	brfalse Y_NOT_BOOL
	ldarg.1
	castclass [mscorlib]System.Boolean
	unbox [mscorlib]System.Boolean
	ldind.i1
	stloc.s 5
	br Y_AS_INT
   Y_NOT_BOOL:
/* static typing should ensure y is a char by now !
	ldarg.1
	isinst [mscorlib]System.Char
	brfalse NOT_INTS 
*/
	ldarg.1
	castclass [mscorlib]System.Char
	unbox [mscorlib]System.Char
	ldind.i2
	stloc.s 5
   Y_AS_INT:
	ldloc.s 4
	ldloc.s 5
	beq RETURN_0
// a sub instruction is not correct here (ie comparing min_int and max_int)
	ldloc.s 4
	ldloc.s 5
	blt RETURN_M1
	ldc.i4.1
	ret
NOT_INTS:
// if (t1==tfloat)
	ldarg.0
	isinst [mscorlib]System.Double	
        brnull NOT_FLOAT 
	ldarg.0
      	castclass [mscorlib]System.Double
	unbox [mscorlib]System.Double
	ldind.r8
        stloc.0
        ldarg.1
// Ici ca planterait si x est float mais pas y (impossible normalement)
        castclass [mscorlib]System.Double
	unbox [mscorlib]System.Double
	ldind.r8
        stloc.1
// if (x<y) return -1 ;
        ldloc.0
	ldloc.1
	blt RETURN_M1
// if (x>y) return 1 ;
        ldloc.0
	ldloc.1
	bgt RETURN_1
//  return 0 ;
        br RETURN_0
	
NOT_FLOAT:
// try to get a string out of arg0
	ldarg.0
	isinst char[]
	brnull T1_NOT_CHARARR
	ldarg.0
	castclass char[]
	newobj instance void [mscorlib]System.String::.ctor(char[])
	stloc 11
	br STRINGS
T1_NOT_CHARARR:	
	ldarg.0
	isinst string
	brnull T1_NOT_STRING
	ldarg.0
	castclass string
	stloc 11
	br STRINGS
T1_NOT_STRING:		
	ldarg.0
	isinst [mscorlib]System.Text.StringBuilder
        brnull NOT_STRINGS
	ldarg.0
      	castclass [mscorlib]System.Text.StringBuilder
        callvirt string [mscorlib]System.Text.StringBuilder::ToString()
	stloc 11
STRINGS:
	ldloc 11

	ldarg.1
	isinst char[]
	brnull T2_NOT_CHARARR
	ldarg.1
	castclass char[]
	newobj instance void [mscorlib]System.String::.ctor(char[])
	br CP_STRINGS
T2_NOT_CHARARR:
	ldarg.1
	isinst string
	brnull T2_NOT_STRING
	ldarg.1
	castclass string
	br CP_STRINGS
T2_NOT_STRING:		
	ldarg.1
      	castclass [mscorlib]System.Text.StringBuilder
        callvirt string [mscorlib]System.Text.StringBuilder::ToString()
CP_STRINGS:
	tail.
        call int32 [mscorlib]System.String::Compare(string, string)
        ret

NOT_STRINGS:
// if (x variant)
	ldarg.0
	isinst CamIL.Variant
	brfalse NOT_VARIANT
//y doit etre aussi un variant
	ldarg.0
	castclass CamIL.Variant
	ldarg.1
	castclass CamIL.Variant
	tail.
	callvirt instance int32 CamIL.Variant::vcompare(class CamIL.Variant)
	ret

NOT_VARIANT:	
// if (x record)
	ldarg.0
	isinst CamIL.Record
	brfalse X_NOT_RECORD
//y doit etre aussi un record
	ldarg.0
	castclass CamIL.Record
	ldarg.1
	castclass CamIL.Record
	tail.
	callvirt instance int32 CamIL.Record::compare(class CamIL.Record)
	ret

X_NOT_RECORD:	

// if (t1==ttab)
	ldarg.0
	isinst [mscorlib]System.Array
	brnull NOT_TTAB
// l2 = (objet[]) x 
	ldarg.0
	castclass class [mscorlib]System.Object[]
	stloc.s 2
// l3 = (objet[]) y 
	ldarg.1
	castclass class [mscorlib]System.Object[]
	stloc.s 3
// l4 = length (l2)
	ldloc.s 2
	ldlen
	stloc.s 4
// l5 = length (l3)
	ldloc.s 3
	ldlen
	stloc.s 5
// if (l4!=l5) return l4-l5 ;
	ldloc.s 4
	ldloc.s 5
	beq LENGTH_EQ
	ldloc.s 4
	ldloc.s 5
	sub
	ret
// if (l4==0) return 0 ;
LENGTH_EQ:
	ldloc.s 4
	brfalse RETURN_0

// maintenu pour les variants polymorphes
// attention, si les tableaux sont dispensés de tag, il faut classer les variants polymorphes dans une classe dédiée ...
// et virer le test de tag pour les tableaux
// du coup :  deux versions selon OLDCAMIL ou pas			
	ldloc.s 2
	call int32 CamIL.BoxInt::tag(object[])
	ldloc.s 3
	call int32 CamIL.BoxInt::tag(object[])
	sub
	dup
	stloc.s 5
	brfalse SAME_TAG
	ldloc.s 5
	ldc.i4.0
	bgt RETURN_1
	ldc.i4.M1
	ret
SAME_TAG:	
	ldloc.s 4
	ldc.i4.1
	sub
	stloc.s 4
	
	ldc.i4.0
	stloc.s 6
WHILE_TAB:		
	ldloc.s 4
	ldloc.s 6
	beq RETURN_0 // on est arrive a la fin du tableau, ils sont egaux
// l5=compare_val (l2[l6],l3[l6]) 
	ldloc.s 2
	ldloc.s 6
	ldelem.ref
	ldloc.s 3
	ldloc.s 6
	ldelem.ref
	call int32 CamIL.Compare::compare_val(object,object)
	stloc.s 5
// if (l5<>0) return l5
	ldloc.s 5
	brfalse ZERO
	ldloc.s 5
	ret
ZERO:
	ldloc.s 6
	ldc.i4.1
	add
	stloc.s 6
	br WHILE_TAB

NOT_TTAB:
// if (x boxint64)
	ldarg.0
	isinst [mscorlib]System.Int64
	brfalse X_NOT_BINT64
// if (y boxint) then compare else -1 
// cf byterun/compare.c : v1 long < v2 block
	ldarg.1
	isinst [mscorlib]System.Int64
	brnull RETURN_M1
	ldarg.0
	castclass [mscorlib]System.Int64
	unbox [mscorlib]System.Int64
	ldind.i8
	dup
	stloc  7
	ldarg.1
	castclass [mscorlib]System.Int64
	unbox [mscorlib]System.Int64
	ldind.i8
	dup
	stloc 8
	beq RETURN_0
// ici un simple sub ne marcherait pas sur entre min_int et max_int par exemple
	ldloc 7
	ldloc 8
	blt RETURN_M1
	ldc.i4.1
	ret
X_NOT_BINT64:	
// if (y boxint64) then 1 
	ldarg.1
	isinst [mscorlib]System.Int64
	brtrue RETURN_1
// if (x boxnint)
	ldarg.0
	isinst [mscorlib]System.IntPtr
	brfalse X_NOT_BNINT
// if (y boxnint) then compare else -1 
// cf byterun/compare.c : v1 long < v2 block
	ldarg.1
	isinst [mscorlib]System.IntPtr
	brnull RETURN_M1
	ldarg.0
	castclass [mscorlib]System.IntPtr
	unbox [mscorlib]System.IntPtr
	ldind.i
	dup
	stloc 9
	ldarg.1
	castclass [mscorlib]System.IntPtr
	unbox [mscorlib]System.IntPtr
	ldind.i
	dup
	stloc 10
	beq RETURN_0
// ici un simple sub ne marcherait pas sur entre min_int et max_int par exemple
	ldloc 9
	ldloc 10
	blt RETURN_M1
	ldc.i4.1
	ret
X_NOT_BNINT:	
// solution temporaire : se reporter sur un compare .NET ?
// mais on a seulement (=) ou (<>)	
//	ldarg.0
//	ldarg.1
//	callvirt instance bool [mscorlib]System.Object::Equals(object)
//	brfalse RETURN_1
RETURN_M1 :
        ldc.i4.M1
	ret
RETURN_0 :
	ldc.i4.0
	ret 
RETURN_1 :
	ldc.i4.1
	ret
 }
	
// normalization 1,0,-1. Is it useful ?
.method public static int32 compare(object,object) il managed 
 {
    .maxstack 2
    .locals init (int32)
	ldarg.0
	ldarg.1
	call int32 CamIL.Compare::compare_val(object,object)
	stloc.0
	ldloc.0
	ldc.i4.0
	ble INF_0
	ldc.i4.1
	ret
    INF_0:
	ldloc.0
	brfalse EQ_0
	ldc.i4.M1
	ret
    EQ_0:
	ldc.i4.0
	ret
 }

	
// polymorphic value equality test					
.method public static bool equal(object,object) il managed 
 {
// retourne 0 ou 1
  .maxstack 3
  .locals init (object[],object[],int32, int32, string)
// if (x==y) return true
      	ldarg.0
      	ldarg.1
      	beq RETURN_TRUE
// if (x==0) return 0
      	ldarg.0
	brtrue X_NOT_NULL
	ldc.i4.0
	ret

X_NOT_NULL:	
// if (y==0) return 0
	ldarg.1
	brtrue Y_NOT_NULL
	ldc.i4.0
	ret

Y_NOT_NULL:	
// 1st case: handle int32, char and bool uniformly	
// try to push an int representation of x
	ldarg.0
	isinst [mscorlib]System.Int32
	brfalse X_NOT_INT32
	ldarg.0
	castclass [mscorlib]System.Int32
	unbox [mscorlib]System.Int32
	ldind.i4
	br X_AS_INT
   X_NOT_INT32:		
	ldarg.0
	isinst [mscorlib]System.Boolean
	brfalse X_NOT_BOOL
	ldarg.0
	castclass [mscorlib]System.Boolean
	unbox [mscorlib]System.Boolean
	ldind.i1
	br X_AS_INT
   X_NOT_BOOL:
	ldarg.0
	isinst [mscorlib]System.Char
	brfalse NOT_INTS
	ldarg.0
	castclass [mscorlib]System.Char
	unbox [mscorlib]System.Char
	ldind.i2
   X_AS_INT:
// now do the same for y
	ldarg.1
	isinst [mscorlib]System.Int32
	brfalse Y_NOT_INT32
	ldarg.1
	castclass [mscorlib]System.Int32
	unbox [mscorlib]System.Int32
	ldind.i4
	br Y_AS_INT
   Y_NOT_INT32:		
	ldarg.1
	isinst [mscorlib]System.Boolean
	brfalse Y_NOT_BOOL
	ldarg.1
	castclass [mscorlib]System.Boolean
	unbox [mscorlib]System.Boolean
	ldind.i1
	br Y_AS_INT
   Y_NOT_BOOL:
/* static typing should ensure y is a char by now !
	ldarg.1
	isinst [mscorlib]System.Char
	brfalse NOT_INTS 
*/
	ldarg.1
	castclass [mscorlib]System.Char
	unbox [mscorlib]System.Char
	ldind.i2
   Y_AS_INT:	
	ceq
	ret

NOT_INTS:		
// if (t1==tfloat)
	ldarg.0
	isinst [mscorlib]System.Double		
        brnull NOT_FLOAT 
	ldarg.0
      	castclass [mscorlib]System.Double
	unbox [mscorlib]System.Double
	ldind.r8
        ldarg.1
// would crash if x is a float but y is not (static typing prevents this)
        castclass [mscorlib]System.Double
	unbox [mscorlib]System.Double
	ldind.r8
	ceq
	ret
	
NOT_FLOAT:
// try to get a string out of arg0
	ldarg.0
	isinst char[]
	brnull T1_NOT_CHARARR
	ldarg.0
	castclass char[]
	newobj instance void [mscorlib]System.String::.ctor(char[])
	stloc.s 4
	br STRINGS
T1_NOT_CHARARR:	
	ldarg.0
	isinst string
	brnull T1_NOT_STRING
	ldarg.0
	castclass string
	stloc.s 4
	br STRINGS
T1_NOT_STRING:		
	ldarg.0
	isinst [mscorlib]System.Text.StringBuilder
        brnull NOT_STRINGS
	ldarg.0
      	castclass [mscorlib]System.Text.StringBuilder
        callvirt string [mscorlib]System.Text.StringBuilder::ToString()
	stloc.s 4
STRINGS:
	ldloc.s 4

	ldarg.1
	isinst char[]
	brnull T2_NOT_CHARARR
	ldarg.1
	castclass char[]
	newobj instance void [mscorlib]System.String::.ctor(char[])
	br CP_STRINGS
T2_NOT_CHARARR:
	ldarg.1
	isinst string
	brnull T2_NOT_STRING
	ldarg.1
	castclass string
	br CP_STRINGS
T2_NOT_STRING:		
	ldarg.1
      	castclass [mscorlib]System.Text.StringBuilder
        callvirt string [mscorlib]System.Text.StringBuilder::ToString()
CP_STRINGS:		
        call int32 [mscorlib]System.String::Compare(string, string)
	ldc.i4.0
	ceq
        ret
	
NOT_STRINGS:
// if (x variant)
	ldarg.0
	isinst CamIL.Variant
	brfalse NOT_VARIANT
//y doit etre aussi un variant
	ldarg.0
	castclass CamIL.Variant
	ldarg.1
	castclass CamIL.Variant
	tail.
	callvirt instance bool CamIL.Variant::vequals(class CamIL.Variant)
	ret
	
NOT_VARIANT:		
// if (x record)
	ldarg.0
	isinst CamIL.Record
	brfalse X_NOT_RECORD
//y doit etre aussi un record
	ldarg.0
	castclass CamIL.Record
	ldarg.1
	castclass CamIL.Record
	tail.
	callvirt instance bool CamIL.Record::equals(class CamIL.Record)
	ret

X_NOT_RECORD:	

// if (t1==ttab)
	ldarg.0
	isinst [mscorlib]System.Array
	brnull NOT_TTAB
// l0 = (objet[]) x 
	ldarg.0
	castclass class [mscorlib]System.Object[]
	stloc.0
// l1 = (objet[]) y 
	ldarg.1
	castclass class [mscorlib]System.Object[]
	stloc.1
// l2 = length (l0)
	ldloc.0
	ldlen
	dup
	stloc.2
	ldloc.1
	ldlen
	beq LENGTH_EQ
	ldc.i4.0
	ret
// if (l2==0) return 1 ;
LENGTH_EQ:
	ldloc.2
	brfalse RETURN_TRUE
	ldloc.0
	call int32 CamIL.BoxInt::tag(object[])
	ldloc.1
	call int32 CamIL.BoxInt::tag(object[])
	ceq
// should have same tag to get a chance to be equal.
// this has to be tested before trying to compare unrelated values ...
	brfalse RETURN_FALSE
	
	ldc.i4.0
	stloc.3
WHILE_TAB:		
	ldloc.2
	ldloc.3
	beq RETURN_TRUE // on est arrive a la fin du tableau, ils sont egaux
//egalite de (l0[l3],l1[l3]) 
	ldloc.0
	ldloc.3
	ldelem.ref
	ldloc.1
	ldloc.3
	ldelem.ref
	call bool CamIL.Compare::equal(object,object)
	brfalse RETURN_FALSE
	ldloc.3
	ldc.i4.1
	add
	stloc.3
	br WHILE_TAB

NOT_TTAB:
// if (x boxint64)
	ldarg.0
	isinst [mscorlib]System.Int64
	brfalse X_NOT_BINT64
	ldarg.1
	isinst [mscorlib]System.Int64
	brnull RETURN_FALSE
	ldarg.0
	castclass [mscorlib]System.Int64
	unbox [mscorlib]System.Int64
	ldind.i8
	ldarg.1
	castclass [mscorlib]System.Int64
	unbox [mscorlib]System.Int64
	ldind.i8
	ceq
	ret
X_NOT_BINT64:	
// if (y boxint64) then 0 
	ldarg.1
	isinst [mscorlib]System.Int64
	brtrue RETURN_FALSE
// if (x boxnint)
	ldarg.0
	isinst [mscorlib]System.IntPtr
	brfalse X_NOT_BNINT
	ldarg.1
	isinst [mscorlib]System.IntPtr
	brnull RETURN_FALSE
	ldarg.0
	castclass [mscorlib]System.IntPtr
	unbox [mscorlib]System.IntPtr
	ldind.i
	ldarg.1
	castclass [mscorlib]System.IntPtr
	unbox [mscorlib]System.IntPtr
	ldind.i
	ceq
	ret

X_NOT_BNINT:	
// solution temporaire : se reporter sur un compare .NET ?
// mais on a seulement (=) ou (<>)	
//	ldarg.0
//	ldarg.1
//	callvirt instance bool [mscorlib]System.Object::Equals(object)
//	brfalse RETURN_1

RETURN_FALSE:
	ldc.i4.0
	ret 
RETURN_TRUE:
	ldc.i4.1
	ret
 }

// polymorphic value inequality	
.method public static bool notequal(object,object) il managed 
 {
    .maxstack 2
	ldarg.0
	ldarg.1
	call bool CamIL.Compare::equal(object,object)
	brfalse RETURN_1
	ldc.i4.0
	ret
    RETURN_1:
	ldc.i4.1
        ret
 }

	
.method public static bool lessthan(object,object) il managed 
 {
    .maxstack 2
	ldarg.0
	ldarg.1
	call int32 CamIL.Compare::compare_val(object,object)
	ldc.i4.0
        blt RETURN_1
	ldc.i4.0
	ret
    RETURN_1:
	ldc.i4.1
        ret
 }

.method public static bool lessequal(object,object) il managed 
 {
    .maxstack 2
	ldarg.0
	ldarg.1
	call int32 CamIL.Compare::compare_val(object,object)
	ldc.i4.0
        ble RETURN_1
	ldc.i4.0
	ret
    RETURN_1:
	ldc.i4.1
        ret
 }

	
.method public static bool greaterthan(object,object) il managed 
 {
    .maxstack 2
	ldarg.0
	ldarg.1
	call int32 CamIL.Compare::compare_val(object,object)
	ldc.i4.0
        bgt RETURN_1
	ldc.i4.0
	ret
    RETURN_1:
	ldc.i4.1
        ret
 }

.method public static bool greaterequal(object,object) il managed 
 {
    .maxstack 2
	ldarg.0
	ldarg.1
	call int32 CamIL.Compare::compare_val(object,object)
	ldc.i4.0
        bge RETURN_1
	ldc.i4.0
	ret
    RETURN_1:
	ldc.i4.1
        ret
 }

}