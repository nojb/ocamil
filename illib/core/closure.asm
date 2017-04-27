/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Bruno Pagano, projet Cristal, INRIA Rocquencourt          //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

.class public auto ansi serializable Closure 
 {
  .method public specialname rtspecialname instance void  .ctor() il managed 
   {
    .maxstack 1 
       	ldarg.0
       	call instance void [mscorlib]System.Object::.ctor() 
       	ret
   }

  .method public virtual instance 
     class [mscorlib]System.Object apply(class [mscorlib]System.Object) il managed
   {
    .maxstack 1
      	ldstr "the virtual apply method of a closure is not defined"
      	call void [mscorlib]System.Console::WriteLine(class [mscorlib]System.String)
      	ldarg.0
      	ret
   }

   .method public instance void threading_delegate() il managed
   {
    .maxstack 2
	ldarg.0
	ldnull
	callvirt instance object CamIL.Closure::apply(object)
	pop
   	ret
   }
	
 }

.class public auto ansi serializable PappClosure extends CamIL.Closure 
 {
  .field public int32 nargs
  .field public class [mscorlib]System.Object[] args 

  .method public specialname rtspecialname instance 
     void .ctor(int32) il managed
   {
    .maxstack 2
       	ldarg.0
       	call instance void CamIL.Closure::.ctor() 
       	ldarg.0
       	ldc.i4.0
       	stfld int32 CamIL.PappClosure::nargs
       	ldarg.0
       	ldarg.1
       	newarr [mscorlib]System.Object
       	stfld class [mscorlib]System.Object[] CamIL.PappClosure::args
       	ret
   }
  .method public static 
     void copyenvt (class CamIL.PappClosure,class CamIL.PappClosure) il managed
   {
    .maxstack 3
       	ldarg.0
       	ldfld class [mscorlib]System.Object[] CamIL.PappClosure::args
       	ldarg.1
       	ldfld class [mscorlib]System.Object[] CamIL.PappClosure::args
       	ldarg.0
       	ldfld int32 CamIL.PappClosure::nargs
       	tail.
       	call void [mscorlib]System.Array::Copy (class [mscorlib]System.Array,
                                      class [mscorlib]System.Array,int32)
       	ret
   }
  .method public static class CamIL.PappClosure gen_apply
     (class CamIL.PappClosure, class CamIL.PappClosure, class [mscorlib]System.Object) 
     il managed
   {
    .maxstack 3
    .locals init (int32)
       	ldarg.0
       	ldfld int32 CamIL.PappClosure::nargs
       	stloc.0 
       	ldarg.0
       	ldfld class [mscorlib]System.Object[] CamIL.PappClosure::args
       	ldarg.1
       	ldfld class [mscorlib]System.Object[] CamIL.PappClosure::args
       	ldloc.0
       	call void [mscorlib]System.Array::Copy (class [mscorlib]System.Array,class [mscorlib]System.Array,
                                      int32)
       	ldarg.1
       	ldfld class [mscorlib]System.Object[] CamIL.PappClosure::args
       	ldloc.0
       	ldarg.2
       	stelem.ref 
       	ldarg.1
       	ldloc.0
       	ldc.i4.1
       	add
       	stfld int32 CamIL.PappClosure::nargs
       	ldarg.1 
       	ret
   }

  .method public virtual instance 
     class [mscorlib]System.Object apply (class [mscorlib]System.Object) il managed  
   {
    .maxstack 1
      	ldstr "the virtual apply method of a PappClosure is not defined"
      	call void [mscorlib]System.Console::WriteLine(class [mscorlib]System.String)
      	ldarg.0
      	ret
   }
 }