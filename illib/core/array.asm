/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Bruno Pagano, projet Cristal, INRIA Rocquencourt          //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

.class public auto ansi Array extends [mscorlib]System.Object 
 {
//Allocates a CamIL Block of tag 0, of size arg_0 and filled with arg_1
//Thus assumes that an array is a block with additionnal tag 0 at the end
  .method public static class [mscorlib]System.Object[]
     make_vect (int32,class [mscorlib]System.Object) il managed
   {
    .maxstack 4
    .locals init (int32 count)	
	ldarg.0
	stloc.0
	ldloc.0
	ldc.i4.1
	add
	newarr [mscorlib]System.Object
    WHILE:
        ldloc.0
	brfalse END
        dup
	ldloc.0
	ldc.i4.1
	sub
	stloc.0
	ldloc.0
	ldarg.1
	stelem.ref 
	br WHILE
    END:
	dup
	ldarg.0
	ldc.i4.0
	box [mscorlib]System.Int32
	stelem.ref
        ret
   }

//Checks that an index is inside an array
//This assumes an extra tag is at the end of the array
  .method public static void verif_bound (class [mscorlib]System.Object[],int32,string)
     il managed
   {
   .maxstack 2
	ldc.i4.0
	ldarg.1
	bgt RAISE
	ldarg.0
	ldlen
	ldc.i4.1
	sub
	ldarg.1
	ble RAISE
	ret
    RAISE:
	ldarg.2
        tail.
	call void CamIL.Exception::invalid_argument(class [mscorlib]System.String)
        ret
   }

  .method public static class [mscorlib]System.Object 
      get_safe (class [mscorlib]System.Object[],int32) il managed
   {
   .maxstack 3
	ldarg.0
	ldarg.1
	ldstr "Array.get"
	call void CamIL.Array::verif_bound(class [mscorlib]System.Object[],int32,string)
	ldarg.0
	ldarg.1
	ldelem.ref
	ret
   }

  .method public static void 
      set_safe (class [mscorlib]System.Object[],int32,class [mscorlib]System.Object) il managed
   {
   .maxstack 4
	ldarg.0
	ldarg.1
	ldstr "Array.set"
	call void CamIL.Array::verif_bound(class [mscorlib]System.Object[],int32,string)
	ldarg.0
	ldarg.1
	ldarg.2
	stelem.ref
	ret
   }
 }
