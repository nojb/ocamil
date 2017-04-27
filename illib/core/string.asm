/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Bruno Pagano, projet Cristal, INRIA Rocquencourt          //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

.class  public auto ansi String extends [mscorlib]System.Object 
 {
  .method public static char[] string_create (int32) il managed
   {
    .maxstack 3
	ldarg.0
	newarr char
	ret
   }

  .method public static void set_safe(char[],int32,char) 
     il managed
   {
    .maxstack 3
        ldarg.1
	ldarg.0
	ldlen
	blt CORRECT_INDEX
	ldstr "String.set"
        tail.
	call void CamIL.Exception::invalid_argument(class [mscorlib]System.String)
	ret
    CORRECT_INDEX:
	ldarg.0
	ldarg.1
	ldarg.2
	stelem.i2
        ret
   }

  .method public static char get_safe(char[],int32) 
     il managed
   {
    .maxstack 2
        ldarg.1
	ldarg.0
	ldlen
	blt CORRECT_INDEX
	ldstr "String.get"
	call void CamIL.Exception::invalid_argument(class [mscorlib]System.String)
    CORRECT_INDEX:
	ldarg.0
	ldarg.1
	ldelem.i2
        ret
   }

  .method public static void blit_string (string,int32,char[],int32,int32)
    il managed
   {
    .maxstack 5
    .locals init (char[])
	ldarg.s 4
	newarr char
	stloc.0
	ldarg.0
	ldarg.1
	ldloc.0
	ldc.i4.0
	ldarg.s 4
        callvirt instance void [mscorlib]System.String::CopyTo(int32,char[],int32,int32)
    WHILE:
        ldarg.s 4
	brfalse END
	ldarg.s 4
	ldc.i4.1
	sub
	starg.s 4
	ldarg.2
	ldarg.3
	ldarg.s 4
	add
        ldloc.0
	ldarg.s 4
	ldelem.i2
	stelem.i2
	br WHILE
     END:  
        ret
    }
	
  .method public static void fill_string (char[],int32,int32,char)
    il managed
   {
    .maxstack 5
    WHILE:
        ldarg.2
	brfalse END
	ldarg.2
	ldc.i4.1
	sub
	starg.s 2
	ldarg.0
	ldarg.1
	ldarg.2
	add
	ldarg.3
	stelem.i2
	br WHILE
     END:  
        ret
    }

  .method public static string string_of_int (int32)
    il managed
   {
    .maxstack 1
	ldarg.0
	box        [mscorlib]System.Int32
	callvirt instance string [mscorlib]System.Object::ToString()
	ret
   }
	
  .method public static string format_float (string, float64)
    il managed
   {
    .maxstack 2
	ldarg.0
	ldarg.1
	box [mscorlib]System.Double
	call string [mscorlib]System.String::Format(string,object)
	ret
   }

  .method public static string format_int (string, int32)
    il managed
   {
    .maxstack 2
	ldarg.0
	ldarg.1
	box [mscorlib]System.Int32
	call string [mscorlib]System.String::Format(string,object)
	ret
   }

 .method public static string format_int64 (string, int64)
    il managed
   {
    .maxstack 2
	ldarg.0
	ldarg.1
	box [mscorlib]System.Int64
	call string [mscorlib]System.String::Format(string,object)
	ret
   }

 .method public static string format_nativeint (string, native int)
    il managed
   {
    .maxstack 2
	ldarg.0
	ldarg.1
	box [mscorlib]System.IntPtr
	call string [mscorlib]System.String::Format(string,object)
	ret
   }

	
  .method public static int32 int_of_string (string) 
    il managed
   {
    .maxstack 1
	.locals init (int32)
	.try{
	ldarg.0
	call int32 [mscorlib]System.Int32::Parse(class [mscorlib]System.String)
	stloc.0
	leave FIN
	}
	catch [mscorlib]System.FormatException {
	pop
	ldstr "int_of_string"
	call void CamIL.Exception::failure(string)
	leave FIN
	}
FIN:	ldloc.0
	ret
   }

  .method public static int64 int64_of_string (string) 
    il managed
   {
    .maxstack 1
	.locals init (int64)
	.try{
	ldarg.0
	call int64 [mscorlib]System.Int64::Parse(class [mscorlib]System.String)
	stloc.0
	leave FIN
	}
	catch [mscorlib]System.FormatException {
	pop
	ldstr "Int64.of_string"
	call void CamIL.Exception::failure(string)
	leave FIN
	}
FIN:	ldloc.0
	ret
   }

	
  .method public static string string_of_float (float64)
    il managed
   {
    .maxstack 2
	ldarga.s 0
	ldstr "en-US"
	newobj instance void [mscorlib]System.Globalization.CultureInfo::.ctor(string)
	call instance string [mscorlib]System.Double::ToString(class [mscorlib]System.IFormatProvider)
	ret
   }

  .method public static float64 float_of_string (string) 
    il managed
   {
    .maxstack 2
	ldarg.0
	// si la chaine est vide, retourner 0.0	
	callvirt instance int32 [mscorlib]System.String::get_Length()
	brfalse EMPTY
	ldarg.0
	ldstr "en-US"
	newobj instance void [mscorlib]System.Globalization.CultureInfo::.ctor(string)
	call float64 [mscorlib]System.Double::Parse(string, class [mscorlib]System.IFormatProvider)
	conv.r8
	ret
EMPTY:
	ldc.r8 0.0
	ret
   }

 .method public static string string_of_object (object) 
    il managed
   {
    .maxstack 1
	ldarg.0
	isinst [mscorlib]System.String
	brfalse NOT_STRING
	ldarg.0
	castclass [mscorlib]System.String
	ret	
NOT_STRING:
	ldarg.0
	isinst char[]
	brfalse NOT_CHARARRAY
	ldarg.0
	castclass char[]
	newobj instance void [mscorlib]System.String::.ctor(char[])
	ret
NOT_CHARARRAY:	
	ldarg.0
	castclass [mscorlib]System.Text.StringBuilder
	call instance class [mscorlib]System.String [mscorlib]System.Text.StringBuilder::ToString()
	ret
   }

 .method public static char[] chararray_of_object (object) 
    il managed
   {
    .maxstack 1
	ldarg.0
	isinst [mscorlib]System.String
	brfalse NOT_STRING
	ldarg.0
	castclass [mscorlib]System.String
	call instance char[] [mscorlib]System.String::ToCharArray()
	ret	
NOT_STRING:
	ldarg.0
	isinst char[]
	brfalse NOT_CHARARRAY
	ldarg.0
	castclass char[]
	ret
NOT_CHARARRAY:	
	ldarg.0
	castclass [mscorlib]System.Text.StringBuilder
	callvirt instance class [mscorlib]System.String [mscorlib]System.Text.StringBuilder::ToString()
	call instance char[] [mscorlib]System.String::ToCharArray()	
	ret
   }

	
 .method public static class [mscorlib]System.Text.StringBuilder bldstr_of_object (object) 
    il managed
   {
    .maxstack 1
	ldarg.0
	isinst [mscorlib]System.Text.StringBuilder
	brfalse NOT_STRINGBUILDER
	ldarg.0
	castclass [mscorlib]System.Text.StringBuilder
	ret	
NOT_STRINGBUILDER:
	ldarg.0
	isinst string
	brfalse NOT_STRING
	ldarg.0
	castclass [mscorlib]System.String
	newobj instance void [mscorlib]System.Text.StringBuilder::.ctor(string)
	ret
NOT_STRING:
	ldarg.0
	castclass char[]
	newobj instance void [mscorlib]System.String::.ctor(char[])
	newobj instance void [mscorlib]System.Text.StringBuilder::.ctor(string)
	ret
   }



  .method public static void string_set_error () il managed
   {
   .maxstack 1
	ldstr "String.set: immutable string"
	call void CamIL.Exception::failure(string)
	ret
   }

 }