/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//   Bruno Pagano, Raphael Montelatici, Emmanuel Chailloux             //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 


.class public auto ansi sealed CustomOpenFileDescriptorIn extends [mscorlib]System.MulticastDelegate
{
  .method public hidebysig specialname rtspecialname 
          instance void  .ctor(object 'object', native int 'method') runtime managed   {}

  .method public hidebysig virtual instance class [mscorlib]System.IO.BinaryReader 
          Invoke(string filename, int32 filemode, int32 fileaccess, int32 fileshare) runtime managed  {}

  .method public hidebysig newslot virtual 
          instance class [mscorlib]System.IAsyncResult 
          BeginInvoke(string filename, int32 filemode, int32 fileaccess, int32 fileshare, class [mscorlib]System.AsyncCallback callback, object 'object') runtime managed {}

  .method public hidebysig newslot virtual 
          instance class [mscorlib]System.IO.BinaryReader 
          EndInvoke(class [mscorlib]System.IAsyncResult result) runtime managed {}
}


.class public auto ansi sealed CustomOpenFileDescriptorOut extends [mscorlib]System.MulticastDelegate
{
  .method public hidebysig specialname rtspecialname 
          instance void  .ctor(object 'object', native int 'method') runtime managed   {}

  .method public hidebysig virtual instance class [mscorlib]System.IO.BinaryWriter 
          Invoke(string filename, int32 filemode, int32 fileaccess, int32 fileshare) runtime managed  {}

  .method public hidebysig newslot virtual 
          instance class [mscorlib]System.IAsyncResult 
          BeginInvoke(string filename, int32 filemode, int32 fileaccess, int32 fileshare, class [mscorlib]System.AsyncCallback callback, object 'object') runtime managed {}

  .method public hidebysig newslot virtual 
          instance class [mscorlib]System.IO.BinaryWriter 
          EndInvoke(class [mscorlib]System.IAsyncResult result) runtime managed {}
}

	
.class public auto ansi IO extends [mscorlib]System.Object
 {
	
.field static private class [mscorlib]System.IO.BinaryReader customStdIn
.field static private class [mscorlib]System.IO.BinaryWriter customStdOut
.field static private class [mscorlib]System.IO.BinaryWriter customStdError

.field static private class CamIL.CustomOpenFileDescriptorIn customFileIn
.field static private class CamIL.CustomOpenFileDescriptorOut customFileOut
			
 .method public static void set_stdin (class [mscorlib]System.IO.Stream)
     il managed 
   {
   .maxstack 1
	ldarg.0
	newobj instance void [mscorlib]System.IO.BinaryReader::.ctor(class [mscorlib]System.IO.Stream)
	stsfld class [mscorlib]System.IO.BinaryReader CamIL.IO::customStdIn
	ret
   }
		
 .method public static void set_stdout (class [mscorlib]System.IO.Stream)
     il managed 
   {
   .maxstack 1
	ldarg.0
	newobj instance void [mscorlib]System.IO.BinaryWriter::.ctor(class [mscorlib]System.IO.Stream)
	stsfld class [mscorlib]System.IO.BinaryWriter CamIL.IO::customStdOut
	ret
   }

 .method public static void set_stderr (class [mscorlib]System.IO.Stream)
     il managed 
   {
   .maxstack 1
	ldarg.0
	newobj instance void [mscorlib]System.IO.BinaryWriter::.ctor(class [mscorlib]System.IO.Stream)
	stsfld class [mscorlib]System.IO.BinaryWriter CamIL.IO::customStdError
	ret
   }

 .method public static void set_custom_file_in (class CamIL.CustomOpenFileDescriptorIn)
     il managed 
   {
   .maxstack 1
	ldarg.0
	stsfld class CamIL.CustomOpenFileDescriptorIn CamIL.IO::customFileIn
	ret
   }

 .method public static void set_custom_file_out (class CamIL.CustomOpenFileDescriptorOut)
     il managed 
   {
   .maxstack 1
	ldarg.0
	stsfld class CamIL.CustomOpenFileDescriptorOut CamIL.IO::customFileOut
	ret
   }		
	
  .method public static class [mscorlib]System.IO.BinaryReader get_stdin ()
     il managed 
   {
   .maxstack 1
	ldsfld class [mscorlib]System.IO.BinaryReader CamIL.IO::customStdIn
	brtrue customIn
	call class [mscorlib]System.IO.Stream [mscorlib]System.Console::OpenStandardInput()
	newobj instance void [mscorlib]System.IO.BinaryReader::.ctor(class [mscorlib]System.IO.Stream)
	ret
customIn:
	ldsfld class [mscorlib]System.IO.BinaryReader CamIL.IO::customStdIn
	ret
   }

	
  .method public static class [mscorlib]System.IO.BinaryWriter get_stdout ()
     il managed 
   {
   .maxstack 1
	ldsfld class [mscorlib]System.IO.BinaryWriter CamIL.IO::customStdOut
	brtrue customOut
	call class [mscorlib]System.IO.Stream [mscorlib]System.Console::OpenStandardOutput()
	newobj instance void [mscorlib]System.IO.BinaryWriter::.ctor(class [mscorlib]System.IO.Stream)
	ret
customOut:
	ldsfld class [mscorlib]System.IO.BinaryWriter CamIL.IO::customStdOut
	ret
   }

		

  .method public static class [mscorlib]System.IO.BinaryWriter get_stderr ()
     il managed 
   {
   .maxstack 1
	ldsfld class [mscorlib]System.IO.BinaryWriter CamIL.IO::customStdError
	brtrue customError
	call class [mscorlib]System.IO.Stream [mscorlib]System.Console::OpenStandardError()
	newobj instance void [mscorlib]System.IO.BinaryWriter::.ctor(class [mscorlib]System.IO.Stream)
	ret
customError:
	ldsfld class [mscorlib]System.IO.BinaryWriter CamIL.IO::customStdError
	ret
   }

  .method public static class [mscorlib]System.IO.BinaryReader open_descriptor_in (string,int32,int32,int32)
	     il managed 
   {
	.locals init (class [mscorlib]System.IO.BinaryReader)
        ldsfld class CamIL.CustomOpenFileDescriptorIn CamIL.IO::customFileIn
	brtrue CUSTOM
	.try{
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	newobj instance void [mscorlib]System.IO.FileStream::.ctor(class [mscorlib]System.String,valuetype [mscorlib]System.IO.FileMode,valuetype [mscorlib]System.IO.FileAccess,valuetype [mscorlib]System.IO.FileShare)
	newobj instance void [mscorlib]System.IO.BinaryReader::.ctor(class [mscorlib]System.IO.Stream)
	stloc.0
	leave FIN
	}
	catch [mscorlib]System.Exception {//plus fin ??
	pop
	ldstr "<to be completed>"
	call void CamIL.Exception::Sys_error(string)
	leave FIN_BIDON
	}
FIN:	ldloc.0
	ret
FIN_BIDON:
	ldnull
	ret
CUSTOM:
        ldsfld class CamIL.CustomOpenFileDescriptorIn CamIL.IO::customFileIn
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	callvirt instance class [mscorlib]System.IO.BinaryReader CamIL.CustomOpenFileDescriptorIn::Invoke(string,int32,int32,int32)
	ret
   }


	
// A FAIRE :	 quel comportement sur stdin (idem pour close_out)	
  .method public static void close_in (class [mscorlib]System.IO.BinaryReader)
     il managed 
   {
    .maxstack  1
	.try {
	ldarg.0
	callvirt instance void [mscorlib]System.IO.BinaryReader::Close()
	leave FIN
	}
	catch [mscorlib]System.Exception {
	pop
	leave FIN
	}
FIN:	ret
   }

		
  .method public static void flush(class [mscorlib]System.Object) il managed 
   {
    .maxstack  1
	.try {
	ldarg.0
	castclass [mscorlib]System.IO.BinaryWriter
	callvirt void [mscorlib]System.IO.BinaryWriter::Flush()
	leave FIN
	}
	catch [mscorlib]System.ObjectDisposedException {
	// En Caml, un flush sur un flot fermé reste stoique ...
	pop
	leave FIN
	}
FIN:	ret
   }
	
  .method public static int32 input_char (class [mscorlib]System.IO.BinaryReader)
     il managed 
   {
    .maxstack  1
    .locals init (unsigned int8)
	.try{
	ldarg.0
	callvirt instance unsigned int8 [mscorlib]System.IO.BinaryReader::ReadByte()
	stloc.0
	leave FIN
	}
	catch [mscorlib]System.ObjectDisposedException { 
	pop
	ldstr "<to be completed>"
	call void CamIL.Exception::Sys_error(string)
	leave FIN_BIDON
	}
	catch [mscorlib]System.IO.EndOfStreamException { 
	pop
	ldsfld object[] CamIL.Constant::End_of_file
	call void CamIL.Exception::raise_cstexc(object[])
	leave FIN_BIDON
	}
FIN:	ldloc.0
	ret
FIN_BIDON:
	ldc.i4.0
	ret
   }


  .method public static int32 unsafe_input (class [mscorlib]System.IO.BinaryReader, char[], int32, int32) 
  {
  .maxstack 3
  .locals init (int32 curpos, int32 end,int32 res) //curpos,end,res
	ldarg.2
	dup
	stloc.0
	ldarg.3
	add
	stloc.1
	ldc.i4.0
	stloc.2
  BEGINLOOP:	
	ldloc.0
	ldloc.1
	beq ENDLOOP
  .try
  {	
	ldarg.1
	ldloc.0
	ldarg.0
	callvirt instance unsigned int8 [mscorlib]System.IO.BinaryReader::ReadByte()
	conv.i2
	stelem.i2
	leave NOT_EOF
  }
  catch [mscorlib]System.IO.EndOfStreamException
  {
	pop
	leave ENDLOOP
  }
  NOT_EOF:		
	ldloc.2
	ldc.i4.1
	add
	stloc.2
	ldloc.0
	ldc.i4.1
	add
	stloc.0
	br BEGINLOOP
  ENDLOOP:	
	ldloc.2
	ret
  }


  .method public static char[] input_singleton_line (class [mscorlib]System.IO.BinaryReader)
     il managed 
   {
    .maxstack  3
    .locals init (char[])
	.try{
	ldc.i4.1
	call char[] CamIL.String::string_create(int32)
      	stloc.0
	ldloc.0
	ldc.i4.0
	ldarg.0
	callvirt instance unsigned int8 [mscorlib]System.IO.BinaryReader::ReadByte()
	conv.i2
	stelem.i2
	leave FIN
	}
	catch [mscorlib]System.ObjectDisposedException { 
	pop
	ldstr "<to be completed>"
	call void CamIL.Exception::Sys_error(string)
	leave FIN_BIDON
	}
	catch [mscorlib]System.IO.EndOfStreamException { 
	pop
	ldsfld object[] CamIL.Constant::End_of_file
	call void CamIL.Exception::raise_cstexc(object[])
	leave FIN_BIDON
	}
FIN:	ldloc.0
	ret
FIN_BIDON:
	ldnull
	ret
   }

  .method public static class [mscorlib]System.IO.BinaryWriter open_descriptor_out (string,int32,int32,int32)
     il managed 
   {
    .locals init (class [mscorlib]System.IO.BinaryWriter)
        ldsfld class CamIL.CustomOpenFileDescriptorOut CamIL.IO::customFileOut
	brtrue CUSTOM
	.try{
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	newobj instance void [mscorlib]System.IO.FileStream::.ctor(class [mscorlib]System.String,valuetype [mscorlib]System.IO.FileMode,valuetype [mscorlib]System.IO.FileAccess,valuetype [mscorlib]System.IO.FileShare)
	newobj instance void [mscorlib]System.IO.BinaryWriter::.ctor(class [mscorlib]System.IO.Stream)
	stloc.0
	leave FIN
	}
	catch [mscorlib]System.Exception {//plus fin ??
	pop
	ldstr "<to be completed>"
	call void CamIL.Exception::Sys_error(string)
	leave FIN_BIDON
	}
FIN:	ldloc.0
	ret
FIN_BIDON:
	ldnull
	ret
CUSTOM:
        ldsfld class CamIL.CustomOpenFileDescriptorOut CamIL.IO::customFileOut
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	callvirt instance class [mscorlib]System.IO.BinaryWriter CamIL.CustomOpenFileDescriptorOut::Invoke(string,int32,int32,int32)
	ret
   }



  .method public static void unsafe_output (class [mscorlib]System.IO.BinaryWriter,string,int32, int32 ) 
  {
  .maxstack 4
  .locals init (int32 curpos, int32 end,unsigned int8[] tab, int32 index)
	ldc.i4.0
	stloc.3
	
	ldarg.3
	newarr [mscorlib]System.Byte
	stloc.2

	ldarg.2
	dup
	stloc.0
	ldarg.3
	add
	stloc.1
  BEGINLOOP:	
	ldloc.0
	ldloc.1
	beq ENDLOOP
	ldloc.2
	ldloc.3
	ldarg.1
	ldloc.0
	callvirt instance char string::get_Chars(int32)
	conv.u1
	stelem.i1
	ldloc.0
	ldc.i4.1
	add
	stloc.0
	ldloc.3
	ldc.i4.1
	add
	stloc.3
	br BEGINLOOP
  ENDLOOP:
	.try {
	ldarg.0
	ldloc.2
	callvirt void [mscorlib]System.IO.BinaryWriter::Write(unsigned int8[])
	leave FIN
	}
	catch [mscorlib]System.ObjectDisposedException { 
	pop
	ldstr "<to be completed>"
	call void CamIL.Exception::Sys_error(string)
	leave FIN
	}
FIN:	ret
  }


	
  .method public static void output_char (class [mscorlib]System.IO.BinaryWriter, int32)
     il managed 
   {
    .maxstack  5
	.try{
	ldarg.0

	// compliqué à cause de mono (car Write(unsigned int8) est buggé)
	ldc.i4.1
	newarr [mscorlib]System.Byte
	dup
	ldc.i4.0
	ldarg.1
	conv.u1
	stelem.i1
	callvirt instance void [mscorlib]System.IO.BinaryWriter::Write(unsigned int8[])
	
	
	leave FIN
	}
	catch [mscorlib]System.ObjectDisposedException { 
	pop
	ldstr "<to be completed>"
	call void CamIL.Exception::Sys_error(string)
	leave FIN
	}
FIN:	ret
   }

	
  .method public static void close_out (class [mscorlib]System.IO.BinaryWriter)
     il managed 
   {
    .maxstack  1
      	ldarg.0
	callvirt instance void [mscorlib]System.IO.BinaryWriter::Close()
	ret
   }

  .method public static void seek_in (class [mscorlib]System.IO.BinaryReader, int32)
     il managed 
   {
   .maxstack 3
	ldarg.0
	callvirt instance class [mscorlib]System.IO.Stream [mscorlib]System.IO.BinaryReader::get_BaseStream()
	ldarg.1
	conv.i8
	ldc.i4.0 //(aka SeekOrigin Begin)	
	callvirt instance int64 [mscorlib]System.IO.Stream::Seek(int64,valuetype [mscorlib]System.IO.SeekOrigin)
	pop
	ret
   }

  .method public static int32 in_channel_length (class [mscorlib]System.IO.BinaryReader)
     il managed 
   {
   .maxstack 3
	ldarg.0
	callvirt instance class [mscorlib]System.IO.Stream [mscorlib]System.IO.BinaryReader::get_BaseStream()
	callvirt instance int64 [mscorlib]System.IO.Stream::get_Length()
	conv.i4
	ret
   }

	
  .method public static void seek_out (class [mscorlib]System.IO.BinaryWriter, int32)
     il managed 
   {
   .maxstack 3
	ldarg.0
	callvirt instance class [mscorlib]System.IO.Stream [mscorlib]System.IO.BinaryWriter::get_BaseStream()
	ldarg.1
	conv.i8
	ldc.i4.0 //(aka SeekOrigin Begin)	
	callvirt instance int64 [mscorlib]System.IO.Stream::Seek(int64,valuetype [mscorlib]System.IO.SeekOrigin)
	pop
	ret
   }

  .method public static int32 out_channel_length (class [mscorlib]System.IO.BinaryWriter)
     il managed 
   {
   .maxstack 3
	ldarg.0
	callvirt instance class [mscorlib]System.IO.Stream [mscorlib]System.IO.BinaryWriter::get_BaseStream()
	callvirt instance int64 [mscorlib]System.IO.Stream::get_Length()
	conv.i4
	ret
   }


	

/////// Serialization

  .method public static void serialize (class [mscorlib]System.IO.BinaryWriter,object) il managed
   {
   .maxstack 3
	.try{
	newobj     instance void [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter::.ctor()
	ldarg.0
	callvirt instance class [mscorlib]System.IO.Stream [mscorlib]System.IO.BinaryWriter::get_BaseStream()
	ldarg.1
	brnull SERIALIZE_NULL
	ldarg.1
	callvirt   instance void [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter::Serialize(class [mscorlib]System.IO.Stream, object)
	leave FIN
SERIALIZE_NULL:
	newobj   instance void CamIL.SerializableNull::.ctor()
	callvirt   instance void [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter::Serialize(class [mscorlib]System.IO.Stream, object)
	leave FIN
	}
	catch [mscorlib]System.ObjectDisposedException { 
	pop
	ldstr "<to be completed>"
	call void CamIL.Exception::Sys_error(string)
	leave FIN
	}
FIN:	ret
   }	


  .method public static object deserialize (class [mscorlib]System.IO.BinaryReader) il managed
   {
   .maxstack 3
	.locals init (object)
	.try{
	newobj     instance void [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter::.ctor()
	ldarg.0
	callvirt instance class [mscorlib]System.IO.Stream [mscorlib]System.IO.BinaryReader::get_BaseStream()
	callvirt   instance object [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter::Deserialize(class [mscorlib]System.IO.Stream)
	stloc.0
	leave FIN
	}
	catch [mscorlib]System.ObjectDisposedException { 
	pop
	ldstr "<to be completed>"
	call void CamIL.Exception::Sys_error(string)
	leave FIN_BIDON
	}
	catch [mscorlib]System.IO.EndOfStreamException { 
	pop
	ldsfld object[] CamIL.Constant::End_of_file
	call void CamIL.Exception::raise_cstexc(object[])
	leave FIN_BIDON
	}
	catch [mscorlib]System.Runtime.Serialization.SerializationException { 
	pop
	ldstr "input_value: bad object"
	call void CamIL.Exception::failure(string)
	leave FIN_BIDON
	}
	
FIN_BIDON:
	ldnull
	ret
FIN:	ldloc.0
	dup
	isinst class CamIL.SerializableNull
	brtrue DESERIALIZE_NULL
	ret
DESERIALIZE_NULL:
	pop
	ldnull
	ret
   }		
}

.class public auto ansi serializable SerializableNull extends [mscorlib]System.Object
{
  .method public hidebysig specialname rtspecialname instance void  .ctor() cil managed
  {
  .maxstack  1
	ldarg.0
	call instance void [mscorlib]System.Object::.ctor()
	ret
  } 

}

	