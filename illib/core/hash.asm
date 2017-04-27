/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//               Clément Capel                                         //
//             & Raphael Montelatici, équipe PPS                       //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 



	
.class public auto ansi Hash extends [mscorlib]System.Object
 {


.method public static int32 
        hash_univ_param ( int32, int32, class [mscorlib]System.Object) il managed
{
  ldarg.2
  call int32 CamIL.Hash::HashCode(class [mscorlib]System.Object)
  ret
}

	
.method public static int32 
        HashCode(class [mscorlib]System.Object) il managed
{
.locals init (float64 fl)
   ldarg.0
   dup
   brtrue OK1 //  = not brnull
   pop
   ldc.i4.0
   ret
OK1:
   dup
   isinst CamIL.Variant
   brnull OKA
   castclass CamIL.Variant
   callvirt instance int32 CamIL.Variant::hashcode()
   ret // already >0
OKA:	
   dup
   isinst CamIL.Record
   brnull OKB
   castclass CamIL.Record
   callvirt instance int32 CamIL.Record::hashcode()
   ret // already >0
OKB:	
   dup	
   isinst char[]
   brnull OKBB
   castclass char[]
   newobj instance void [mscorlib]System.String::.ctor(char[])
   call instance int32 [mscorlib]System.String::GetHashCode()
   br ABS
OKBB:	
   dup	
   isinst string
   brnull OKBBB
   castclass string
   call instance int32 [mscorlib]System.String::GetHashCode()
   br ABS
OKBBB:	
   dup	
   isinst [mscorlib]System.Text.StringBuilder
   brnull OK2
   castclass [mscorlib]System.Text.StringBuilder
   call instance string [mscorlib]System.Text.StringBuilder::ToString()
   call instance int32 [mscorlib]System.String::GetHashCode()
   br ABS
OK2:
   dup	
   isinst [mscorlib]System.Array
   brnull OK3
   castclass object[]
   dup
   ldlen
   ldc.i4.1 // object[] blocks have at least a tag
   ceq
   brfalse NOTEMPTYBLOCK
   ldc.i4.0
   ldelem.ref
   castclass [mscorlib]System.Int32
   unbox [mscorlib]System.Int32	
   ldind.i4
   br ABS
NOTEMPTYBLOCK:		
   call int32 CamIL.Hash::HashBlock(class [mscorlib]System.Object[])
   br ABS
OK3:		
   dup
   isinst [mscorlib]System.Int32
   brfalse OK4
   castclass [mscorlib]System.Int32
   unbox [mscorlib]System.Int32	
   ldind.i4
   br ABS
OK4:
   dup
   isinst [mscorlib]System.Double
   brnull OTHER
   castclass [mscorlib]System.Double
   unbox [mscorlib]System.Double
   ldind.r8
   stloc.s fl
   ldloca.s fl
   call instance int32 [mscorlib]System.Double::GetHashCode()
   br ABS
OTHER:	
   call instance int32 [mscorlib]System.Object::GetHashCode()
ABS:
   call int32 [mscorlib]System.Math::Abs(int32)	
   ret
}


.method public static int32  HashBlock(class [mscorlib]System.Object[]) il managed
{
  .maxstack  3
  .locals init (int32, int32)
  // code
 ldc.i4.0
 stloc.0
  // count
 ldc.i4.0
 stloc.1	
 br.s START_LOOP
LOOP:
  // code = code*19 + arg[i].HashCode()
  ldloc.0
  ldc.i4.s   19 // constant alpha
  mul
  ldarg.0
  ldloc.1
  ldelem.ref
  call int32 CamIL.Hash::HashCode(class [mscorlib]System.Object)
  add
  stloc.0
  ldloc.1
  ldc.i4.1
  add
  stloc.1
START_LOOP:	
  ldloc.1
  ldarg.0
  ldlen
  blt.s LOOP
  ldloc.0
  ret
} 


 }
	

