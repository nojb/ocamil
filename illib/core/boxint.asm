/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Bruno Pagano, projet Cristal, INRIA Rocquencourt          //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

.class public auto ansi serializable BoxInt extends [mscorlib]System.Object 
 {
		
  .method public static int32 tag (class [mscorlib]System.Object[]) il managed 
   {
    .maxstack 4
        ldarg.0
      	ldarg.0
        ldlen
	ldc.i4.1
	sub 
	ldelem.ref
        castclass [mscorlib]System.Int32
	unbox [mscorlib]System.Int32
	ldind.i4
      	ret
   }

  .method public static int32 safe_tag (class [mscorlib]System.Object) il managed 
   {
    .maxstack 1
	ldarg.0
	isinst CamIL.Variant
	brfalse NOT_VARIANT
	ldarg.0
	castclass CamIL.Variant
	ldfld int32 CamIL.Variant::tag
	ret
NOT_VARIANT:
	ldarg.0
	isinst char[]
	brtrue IS_STRING	
	ldarg.0
	isinst [mscorlib]System.String
	brtrue IS_STRING
	ldarg.0
	isinst [mscorlib]System.Text.StringBuilder
	brtrue IS_STRING
	ldarg.0
	isinst [mscorlib]System.Double
	brtrue IS_DOUBLE
	ldarg.0
	isinst CamIL.Record
	brfalse NOT_RECORD
	ldc.i4.0
	ret
NOT_RECORD:		
// so it's a all-purpose block
	ldarg.0
	castclass object[]
        tail.
        call int32 CamIL.BoxInt::tag(class [mscorlib]System.Object[]) 
      	ret
IS_STRING:
	ldc.i4 252 // le tag du bloc string
	ret
IS_DOUBLE:
	ldc.i4 253 // le tag du bloc float
	ret
   }


 }



