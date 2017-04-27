/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//               Clément Capel, équipe PPS                             //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 


.class public auto ansi Weak extends [mscorlib]System.Object
 {

.method public static class [mscorlib]System.Object[] weak_create(int32) il managed
{

  .locals init (class [mscorlib]System.Object[], int32)
  // arg < 0
  ldarg.0
  ldc.i4.0
  blt.s EXCEPTION
  // arg > Sys.max_array_length
  ldarg.0
  ldc.i4 0x3fffff 
  blt.s CREATE
EXCEPTION:
  ldstr "Weak.create"
  call void CamIL.Exception::invalid_argument(class [mscorlib]System.String)
	
CREATE:	
  ldarg.0
  newarr [mscorlib]System.Object 
  ret
}

.method public static void  weak_set(class [mscorlib]System.Object[], int32, class [mscorlib]System.Object) il managed
{
  .maxstack 3

  // arg < 0
  ldarg.1
  ldc.i4.0
  blt.s EXCEPTION
  // arg > tab.length
  ldarg.1
  ldarg.0
  ldlen
  conv.i4
  blt.s SET
EXCEPTION:
  ldstr "Weak.set"
  call void CamIL.Exception::invalid_argument(class [mscorlib]System.String)	

SET:	
  ldarg.0
  ldarg.1
  ldelem.ref
  dup
  brtrue NOT_NULL
  pop	
  ldarg.0
  ldarg.1
  ldarg.2
  newobj instance void [mscorlib]System.WeakReference::.ctor(class [mscorlib]System.Object)
  stelem.ref
  ret
NOT_NULL:
  castclass [mscorlib]System.WeakReference
  ldarg.2
  call instance void [mscorlib]System.WeakReference::set_Target(class [mscorlib]System.Object)
  ret
}

.method public static int32 weak_check(class [mscorlib]System.Object[], int32) il managed
{
  .maxstack 3
   // arg < 0
  ldarg.1
  ldc.i4.0
  blt.s EXCEPTION
  // arg > tab.length
  ldarg.1
  ldarg.0
  ldlen
  conv.i4
  blt.s CHECK
EXCEPTION:
  ldstr "Weak.get"
  call void CamIL.Exception::invalid_argument(class [mscorlib]System.String)
CHECK:	
  ldarg.0
  ldarg.1
  ldelem.ref
  dup
  brtrue NOT_NULL
  pop	
  ldc.i4.0
  ret
NOT_NULL:
  castclass [mscorlib]System.WeakReference
  call instance bool [mscorlib]System.WeakReference::get_IsAlive()
  call int32 [mscorlib]System.Convert::ToInt32(bool)
  ret
}


.method public static class [mscorlib]System.Object
	 weak_get(class [mscorlib]System.Object[], int32) il managed
{
  .maxstack 2
   // arg < 0
  ldarg.1
  ldc.i4.0
  blt.s EXCEPTION
  // arg > tab.length
  ldarg.1
  ldarg.0
  ldlen
  conv.i4
  blt.s GET
EXCEPTION:
  ldstr "Weak.get"
  call void CamIL.Exception::invalid_argument(class [mscorlib]System.String)
GET:	
  ldarg.0
  ldarg.1
  ldelem.ref
  dup
  brtrue NOT_NULL
  pop	
  ldnull
  ret
NOT_NULL:	
  castclass [mscorlib]System.WeakReference
  call instance class [mscorlib]System.Object [mscorlib]System.WeakReference::get_Target()
  ret
}

.method public static class [mscorlib]System.Object
	 weak_get_copy(class [mscorlib]System.Object[], int32) il managed
{
  .maxstack 4
  // arg < 0
  ldarg.1
  ldc.i4.0
  blt.s EXCEPTION
  // arg > tab.length
  ldarg.1
  ldarg.0
  ldlen
  conv.i4
  blt.s GET_COPY
EXCEPTION:
  ldstr "Weak.get"
  call void CamIL.Exception::invalid_argument(class [mscorlib]System.String)
GET_COPY:
  ldarg.0
  ldarg.1
  ldelem.ref
  dup
  brtrue NOT_NULL
  pop	
  ldnull
  ret
NOT_NULL:	
  castclass [mscorlib]System.WeakReference
  call instance class [mscorlib]System.Object [mscorlib]System.WeakReference::get_Target()
  call class [mscorlib]System.Object CamIL.Obj::obj_dup(class [mscorlib]System.Object)


  ret
}

	
.method public static int32 length (class [mscorlib]System.Object[]) il managed{
	 .maxstack 1
	ldarg.0
	ldlen
	ret
}

}








