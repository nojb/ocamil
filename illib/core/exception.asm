/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Bruno Pagano, projet Cristal, INRIA Rocquencourt          //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

.class public auto ansi Exception extends [mscorlib]System.Exception
 {
  .field public class [mscorlib]System.Object[] v1


//block avec un tag 0 a la fin
  .method public static 
    object[] str2block(string) il managed
   {
   .maxstack 4
	ldc.i4.2
	newarr [mscorlib]System.Object
	dup
	ldc.i4.0
	ldarg.0
	stelem.ref
	dup
	ldc.i4.1
	ldc.i4.0
	box [mscorlib]System.Int32
	stelem.ref
	ret
   }

	
  .method public specialname rtspecialname instance
     void .ctor(class [mscorlib]System.Object[] fullblock) il managed
   {
    .maxstack 2
      	ldarg.0
      	call instance void [mscorlib]System.Exception::.ctor()
      	ldarg.0
      	ldarg.1
        stfld class [mscorlib]System.Object[] CamIL.Exception::v1
      	ret
   }

	
  .method public static void raise(class [mscorlib]System.Object[]) il managed
   {
    .maxstack  1
 	ldarg.0
	newobj instance void CamIL.Exception::.ctor(class [mscorlib]System.Object[])
	throw
   }

//tag 0 a la fin
  .method public static 
    void raise_blockstr(object[],class [mscorlib]System.String) il managed
   {
    .maxstack  4
    .locals init (class [mscorlib]System.Object[])
    	ldc.i4.3
      	newarr [mscorlib]System.Object
      	stloc.0
      	ldloc.0
      	ldc.i4.0
      	ldarg.0
	stelem.ref
      	ldloc.0
      	ldc.i4.1
      	ldarg.1
	newobj     instance void [mscorlib]System.Text.StringBuilder::.ctor(string)
      	stelem.ref
	ldloc.0
	ldc.i4.2
	ldc.i4.0
	box [mscorlib]System.Int32
	stelem.ref
      	ldloc.0
      	newobj instance void CamIL.Exception::.ctor(class [mscorlib]System.Object[])
      	throw
   } 

  .method public static void raise_cstexc(object[]) il managed
   {
    .maxstack  4
    .locals init (class [mscorlib]System.Object[])
    	ldc.i4.2
      	newarr [mscorlib]System.Object
      	stloc.0
      	ldloc.0
      	ldc.i4.0
      	ldarg.0
	stelem.ref
      	ldloc.0
      	ldc.i4.1
	ldc.i4.0
	box [mscorlib]System.Int32
	stelem.ref
      	ldloc.0
      	newobj instance void CamIL.Exception::.ctor(class [mscorlib]System.Object[])
      	throw
   } 

  .method public static void invalid_argument(class [mscorlib]System.String s) il managed
   {
    .maxstack 2
      	ldsfld object[] CamIL.Constant::Invalid_argument
      	ldarg.0
      	call void CamIL.Exception::raise_blockstr(object[],string)
      	ret
    }

 .method public static void Sys_error(class [mscorlib]System.String s) il managed
   {
    .maxstack 2
      	ldsfld object[] CamIL.Constant::Sys_error
      	ldarg.0
      	call void CamIL.Exception::raise_blockstr(object[],string)
      	ret
    }

  .method public static void failure(class [mscorlib]System.String s) il managed
   {
    .maxstack 2
      	ldsfld object[] CamIL.Constant::Failure
      	ldarg.0
      	call void CamIL.Exception::raise_blockstr(object[],string)
      	ret
   }

	
  .method public static object sys_exit(int32) il managed
   {
    .maxstack 1
	ldarg.0
	call void [mscorlib]System.Environment::Exit(int32)
	ldnull
	ret
   }
	
  .method public static object getCLIinnerException(object) il managed {
	ldarg.0
	castclass [mscorlib]System.Exception
	callvirt instance class [mscorlib]System.Exception [mscorlib]System.Exception::get_InnerException()
	ret
	}

   .method public static string getCLIExceptionMessage(object) il managed {
	ldarg.0
	castclass [mscorlib]System.Exception
	callvirt instance string [mscorlib]System.Exception::get_Message()
	ret
	}

   .method public static string getCLIExceptionName(object) il managed {
	ldarg.0
	castclass [mscorlib]System.Exception
	callvirt class [mscorlib]System.Type [mscorlib]System.Exception::GetType()
	callvirt string [mscorlib]System.Object::ToString()	
	ret
	}

		
  .method public static class CamIL.Exception embedCLI (class [mscorlib]System.Exception) il managed
   {
    .maxstack 5
	ldc.i4.4
	newarr object
	dup
	ldc.i4.0
      	ldsfld object[] CamIL.Constant::ManagedException
	stelem.ref
	dup
	ldc.i4.1
	ldarg.0
	callvirt class [mscorlib]System.Type [mscorlib]System.Exception::GetType()
	callvirt string [mscorlib]System.Object::ToString()
	stelem.ref
	dup
	ldc.i4.2
	ldarg.0
	stelem.ref
	dup
	ldc.i4.3
	ldc.i4.0
	box [mscorlib]System.Int32
	stelem.ref
      	newobj instance void CamIL.Exception::.ctor(object[])
	ret
   }
	
 }
