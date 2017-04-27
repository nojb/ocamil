/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Bruno Pagano, projet Cristal, INRIA Rocquencourt          //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

	/*
.class public auto ansi serializable BoxFloat extends [mscorlib]System.Object 
 {
  .field public float64 v1

  .method public specialname rtspecialname instance 
     void .ctor(float64 n) il managed 
   {
    .maxstack  2
      	ldarg.0
      	call instance void [mscorlib]System.Object::.ctor()
      	ldarg.0
      	ldarg.1
      	stfld float64 CamIL.BoxFloat::v1
      	ret
}

	
.method public static float64 to_float (class CamIL.BoxFloat) il managed 
   {
    .maxstack 1
    ldarg.0
    ldfld float64 CamIL.BoxFloat::v1
    ret    
   }
 



 }
*/