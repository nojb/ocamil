/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//   Clément Capel                                                     //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 
  .class public auto ansi Extern  extends [mscorlib]System.Object
  {

    .method private hidebysig static object[] 
            raw_output_value_to_buffer(class [mscorlib]System.Text.StringBuilder buff,
                                       object obj,
                                       int32 ofs,
                                       int32 len,
                                       object extern_flags,
                                       bool new_buffer) cil managed
    {
      // Code size       138 (0x8a)
      .maxstack  4
      .locals init (class [mscorlib]System.IO.MemoryStream V_0,
               class [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter V_1,
               unsigned int8[] V_2,
               int32 V_3,
               object[] V_4,
               object[] V_5)
      IL_0000:  newobj     instance void [mscorlib]System.IO.MemoryStream::.ctor()
      IL_0005:  stloc.0
      IL_0006:  newobj     instance void [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter::.ctor()
      IL_000b:  stloc.1
      IL_000c:  ldloc.1
      IL_000d:  ldloc.0
      IL_000e:  ldarg.1
	        dup
		brtrue SERIALIZE
		pop
		newobj   instance void CamIL.SerializableNull::.ctor()
SERIALIZE:
      IL_000f:  callvirt   instance void [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter::Serialize(class [mscorlib]System.IO.Stream,
                                                                                                                             object)
      IL_0014:  ldloc.0
      IL_0015:  callvirt   instance unsigned int8[] [mscorlib]System.IO.MemoryStream::GetBuffer()
      IL_001a:  stloc.2
      IL_001b:  ldarg.s    new_buffer
      IL_001d:  brfalse.s  IL_0028

      IL_001f:  ldarg.0
      IL_0020:  ldloc.2
      IL_0021:  ldlen
      IL_0022:  conv.i4
      IL_0023:  callvirt   instance void [mscorlib]System.Text.StringBuilder::set_Capacity(int32)
      IL_0028:  ldarg.3
      IL_0029:  ldloc.2
      IL_002a:  ldlen
      IL_002b:  conv.i4
      IL_002c:  bge.s      IL_003c

      IL_002e:  ldarg.s    new_buffer
      IL_0030:  brtrue.s   IL_003c

      IL_0032:  ldstr      "Marshal.to_buffer: buffer overflow"
      IL_0037:  call void CamIL.Exception::failure(class [mscorlib]System.String)
      IL_003c:  ldc.i4.0
      IL_003d:  stloc.3
      IL_003e:  br.s       IL_0062

      IL_0040:  ldarg.s    new_buffer
      IL_0042:  brfalse.s  IL_0051

      IL_0044:  ldarg.0
      IL_0045:  ldloc.2
      IL_0046:  ldloc.3
      IL_0047:  ldelem.u1
      IL_0048:  conv.u2
      IL_0049:  callvirt   instance class [mscorlib]System.Text.StringBuilder [mscorlib]System.Text.StringBuilder::Append(char)
      IL_004e:  pop
      IL_004f:  br.s       IL_005e

      IL_0051:  ldarg.0
      IL_0052:  ldloc.3
      IL_0053:  ldarg.2
      IL_0054:  add
      IL_0055:  ldloc.2
      IL_0056:  ldloc.3
      IL_0057:  ldelem.u1
      IL_0058:  conv.u2
      IL_0059:  callvirt   instance void [mscorlib]System.Text.StringBuilder::set_Chars(int32,
                                                                                        char)
      IL_005e:  ldloc.3
      IL_005f:  ldc.i4.1
      IL_0060:  add
      IL_0061:  stloc.3
      IL_0062:  ldloc.3
      IL_0063:  ldloc.2
      IL_0064:  ldlen
      IL_0065:  conv.i4
      IL_0066:  blt.s      IL_0040

      IL_0068:  ldc.i4.2
      IL_0069:  newarr     [mscorlib]System.Object
      IL_006e:  stloc.s    V_4
      IL_0070:  ldloc.s    V_4
      IL_0072:  ldc.i4.0
      IL_0073:  ldarg.0
      IL_0074:  stelem.ref
      IL_0075:  ldloc.s    V_4
      IL_0077:  ldc.i4.1
      IL_0078:  ldloc.2
      IL_0079:  ldlen
      IL_007a:  conv.i4
      IL_007b:  box        [mscorlib]System.Int32
      IL_0080:  stelem.ref
      IL_0081:  ldloc.s    V_4
      IL_0083:  stloc.s    V_5
      IL_0085:  br.s       IL_0087

      IL_0087:  ldloc.s    V_5
      IL_0089:  ret
    } // end of method Extern::raw_output_value_to_buffer

    .method public hidebysig static class [mscorlib]System.Text.StringBuilder 
            output_value_to_string(object obj,
                                   object extern_flags) cil managed
    {
      // Code size       36 (0x24)
      .maxstack  6
      .locals init (class [mscorlib]System.Text.StringBuilder V_0,
               object[] V_1,
               class [mscorlib]System.Text.StringBuilder V_2)
      IL_0000:  newobj     instance void [mscorlib]System.Text.StringBuilder::.ctor()
      IL_0005:  stloc.0
      IL_0006:  ldloc.0
      IL_0007:  ldarg.0
      IL_0008:  ldc.i4.0
      IL_0009:  ldloc.0
      IL_000a:  callvirt   instance int32 [mscorlib]System.Text.StringBuilder::get_Length()
      IL_000f:  ldarg.1
      IL_0010:  ldc.i4.1
      IL_0011:  call       object[] CamIL.Extern::raw_output_value_to_buffer(class [mscorlib]System.Text.StringBuilder,
                                                                             object,
                                                                             int32,
                                                                             int32,
                                                                             object,
                                                                             bool)
      IL_0016:  stloc.1
      IL_0017:  ldloc.1
      IL_0018:  ldc.i4.0
      IL_0019:  ldelem.ref
      IL_001a:  castclass  [mscorlib]System.Text.StringBuilder
      IL_001f:  stloc.2
      IL_0020:  br.s       IL_0022

      IL_0022:  ldloc.2
      IL_0023:  ret
    } // end of method Extern::output_value_to_string

    .method public hidebysig static int32 
            output_value_to_buffer(class [mscorlib]System.Text.StringBuilder buff,
                                   int32 ofs,
                                   int32 len,
                                   object obj,
                                   object extern_flags) cil managed
    {
      // Code size       27 (0x1b)
      .maxstack  6
      .locals init (object[] V_0,
               int32 V_1)
      IL_0000:  ldarg.0
      IL_0001:  ldarg.3
      IL_0002:  ldarg.1
      IL_0003:  ldarg.2
      IL_0004:  ldarg.s    extern_flags
      IL_0006:  ldc.i4.0
      IL_0007:  call       object[] CamIL.Extern::raw_output_value_to_buffer(class [mscorlib]System.Text.StringBuilder,
                                                                             object,
                                                                             int32,
                                                                             int32,
                                                                             object,
                                                                             bool)
      IL_000c:  stloc.0
      IL_000d:  ldloc.0
      IL_000e:  ldc.i4.1
      IL_000f:  ldelem.ref
      IL_0010:  unbox      [mscorlib]System.Int32
      IL_0015:  ldind.i4
      IL_0016:  stloc.1
      IL_0017:  br.s       IL_0019

      IL_0019:  ldloc.1
      IL_001a:  ret
    } // end of method Extern::output_value_to_buffer

    .method public hidebysig static object 
            input_value_from_string(class [mscorlib]System.Text.StringBuilder buff,
                                    int32 ofs) cil managed
    {
      // Code size       67 (0x43)
      .maxstack  4
      .locals init (class [mscorlib]System.IO.MemoryStream V_0,
               class [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter V_1,
               int32 V_2,
               object V_3)
      IL_0000:  newobj     instance void [mscorlib]System.IO.MemoryStream::.ctor()
      IL_0005:  stloc.0
      IL_0006:  newobj     instance void [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter::.ctor()
      IL_000b:  stloc.1
      IL_000c:  ldc.i4.0
      IL_000d:  stloc.2
      IL_000e:  br.s       IL_0024

      IL_0010:  ldloc.0
      IL_0011:  ldarg.0
      IL_0012:  ldarg.1
      IL_0013:  ldloc.2
      IL_0014:  add
      IL_0015:  callvirt   instance char [mscorlib]System.Text.StringBuilder::get_Chars(int32)
      IL_001a:  conv.u1
      IL_001b:  callvirt   instance void [mscorlib]System.IO.Stream::WriteByte(unsigned int8)
      IL_0020:  ldloc.2
      IL_0021:  ldc.i4.1
      IL_0022:  add
      IL_0023:  stloc.2
      IL_0024:  ldloc.2
      IL_0025:  ldarg.0
      IL_0026:  callvirt   instance int32 [mscorlib]System.Text.StringBuilder::get_Length()
      IL_002b:  ldarg.1
      IL_002c:  sub
      IL_002d:  blt.s      IL_0010

      IL_002f:  ldloc.0
      IL_0030:  ldc.i4.0
      IL_0031:  conv.i8
      IL_0032:  callvirt   instance void [mscorlib]System.IO.Stream::set_Position(int64)
      IL_0037:  ldloc.1
      IL_0038:  ldloc.0
      IL_0039:  callvirt   instance object [mscorlib]System.Runtime.Serialization.Formatters.Binary.BinaryFormatter::Deserialize(class [mscorlib]System.IO.Stream)
 	dup
	isinst class CamIL.SerializableNull
	brtrue DESERIALIZE_NULL
	ret
DESERIALIZE_NULL:
	pop
	ldnull
	ret
    } // end of method Extern::input_value_from_string

    .method public hidebysig static int32 
            marshal_data_size(class [mscorlib]System.Text.StringBuilder buff,
                              int32 ofs) cil managed
    {
      // Code size       13 (0xd)
      .maxstack  2
      .locals init (int32 V_0)
      IL_0000:  ldarg.0
      IL_0001:  callvirt   instance int32 [mscorlib]System.Text.StringBuilder::get_Length()
      IL_0006:  ldarg.1
      IL_0007:  sub
      IL_0008:  stloc.0
      IL_0009:  br.s       IL_000b

      IL_000b:  ldloc.0
      IL_000c:  ret
    } // end of method Extern::marshal_data_size

    .method public hidebysig specialname rtspecialname 
            instance void  .ctor() cil managed
    {
      // Code size       7 (0x7)
      .maxstack  1
      IL_0000:  ldarg.0
      IL_0001:  call       instance void [mscorlib]System.Object::.ctor()
      IL_0006:  ret
    } // end of method Extern::.ctor




	
  } // end of class Extern


