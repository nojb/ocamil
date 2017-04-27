/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Bruno Pagano, projet Cristal, INRIA Rocquencourt          //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

.class public auto ansi Constant extends [mscorlib]System.Object
 {
/*
  .field static public class [mscorlib]System.Type tint
  .field static public class [mscorlib]System.Type tint64
  .field static public class [mscorlib]System.Type tnint 
  .field static public class [mscorlib]System.Type tfloat 
  .field static public class [mscorlib]System.Type tstring
  .field static public class [mscorlib]System.Type tbldstr
  .field static public class [mscorlib]System.Type ttab
*/
  .field public static object[] Match_failure
  .field public static object[] Failure
  .field public static object[] Assert_failure	
  .field public static object[] Invalid_argument
  .field public static object[] Not_found
  .field public static object[] End_of_file
  .field public static object[] Out_of_memory
  .field public static object[] Stack_overflow
  .field public static object[] Sys_error

  .field public static object[] ManagedException

	
  .method public specialname rtspecialname static void .cctor() il managed 
   {
    .maxstack 1
/*	ldc.i4.0
	box [mscorlib]System.Int32
	call instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()	
	stsfld class [mscorlib]System.Type CamIL.Constant::tint
	ldc.i8 0
	box [mscorlib]System.Int64
	call instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()	
	stsfld class [mscorlib]System.Type CamIL.Constant::tint64
	ldc.i4.0
	conv.i
	box [mscorlib]System.IntPtr
	call instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()	
	stsfld class [mscorlib]System.Type CamIL.Constant::tnint
	ldc.r8 0.0
	box [mscorlib]System.Double
	call instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()	
	stsfld class [mscorlib]System.Type CamIL.Constant::tfloat
      	ldstr "[mscorlib]System.String"
      	call class [mscorlib]System.Type [mscorlib]System.Type::GetType(class [mscorlib]System.String)
      	stsfld class [mscorlib]System.Type CamIL.Constant::tstring
      	ldstr "[mscorlib]System.StringBuilder"
      	call class [mscorlib]System.Type [mscorlib]System.Type::GetType(class [mscorlib]System.String)
      	stsfld class [mscorlib]System.Type CamIL.Constant::tbldstr
      	ldstr "[mscorlib]System.Object[]"
      	call class [mscorlib]System.Type [mscorlib]System.Type::GetType(class [mscorlib]System.String)
      	stsfld class [mscorlib]System.Type CamIL.Constant::ttab
*/	
      	ldstr "Match_failure"
	call object[] CamIL.Exception::str2block(string)	
        stsfld object[] CamIL.Constant::Match_failure
      	ldstr "Failure"
	call object[] CamIL.Exception::str2block(string)
      	stsfld object[] CamIL.Constant::Failure
      	ldstr "Assert_failure"	
	call object[] CamIL.Exception::str2block(string)
      	stsfld object[] CamIL.Constant::Assert_failure
      	ldstr "Invalid_argument"
	call object[] CamIL.Exception::str2block(string)
      	stsfld object[] CamIL.Constant::Invalid_argument
        ldstr "Not_found"
	call object[] CamIL.Exception::str2block(string)
        stsfld object[] CamIL.Constant::Not_found
        ldstr "End_of_file"
	call object[] CamIL.Exception::str2block(string)
        stsfld object[] CamIL.Constant::End_of_file
        ldstr "Out_of_memory"
	call object[] CamIL.Exception::str2block(string)
        stsfld object[] CamIL.Constant::Out_of_memory
        ldstr "Stack_overflow"
	call object[] CamIL.Exception::str2block(string)
        stsfld object[] CamIL.Constant::Stack_overflow
        ldstr "Sys_error"
	call object[] CamIL.Exception::str2block(string)
        stsfld object[] CamIL.Constant::Sys_error
	ldstr "ManagedException"
	call object[] CamIL.Exception::str2block(string)
        stsfld object[] CamIL.Constant::ManagedException
      	ret
   }
 }
