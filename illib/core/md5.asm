/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//               Clément Capel, équipe PPS                             //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 



	
.class public auto ansi Md5 extends [mscorlib]System.Object
 {

.method public static class [mscorlib]System.String 
        md5_string(class [mscorlib]System.String, int32, int32) il managed
{
  .maxstack  4
  .locals init (int8[], class [mscorlib]System.String, int32)
  newobj   instance void [mscorlib]System.Security.Cryptography.MD5CryptoServiceProvider::.ctor()
  ldarg.0  
  call       unsigned int8[] CamIL.Md5::string_to_byte_array(class [mscorlib]System.String)
  ldarg.1
  ldarg.2
  callvirt   instance unsigned int8[] [mscorlib]System.Security.Cryptography.HashAlgorithm::ComputeHash(unsigned int8[], int32, int32)
  call       class [mscorlib]System.String CamIL.Md5::ascii_string(unsigned int8[])
  ret
}

	
.method public static unsigned int8[] 
        string_to_byte_array(class [mscorlib]System.String sb) il managed
{
  .maxstack  5
  .locals init (unsigned int8[], int32)
  ldarg.0
  callvirt   instance int32 [mscorlib]System.String::get_Length()
  conv.ovf.u4 
  newarr     [mscorlib]System.Byte
  stloc.0
  ldc.i4.0
  stloc.1
  br.s START
LOOP:
  ldloc.0
  ldloc.1
  ldarg.0
  ldloc.1
  callvirt   instance char [mscorlib]System.String::get_Chars(int32)
  conv.u1
  stelem.i1
  ldloc.1
  ldc.i4.1
  add
  stloc.1
START:	
  ldloc.1
  ldloc.0
  ldlen
  conv.i4
  blt.s LOOP
  ldloc.0
  ret
} 

// Modified by Karim Tekkal 21_06_2007
   .method public static  hidebysig
           default string ascii_string (unsigned int8[] tab)  cil managed
    {
        // Method begins at RVA 0x2114
        // Code size 44 (0x2c)
        .maxstack 5
        .locals init (
                class [mscorlib]System.Text.StringBuilder       V_0,
                int32   V_1)
        IL_0000:  ldc.i4.s 0x10
        IL_0002:  newobj instance void class [mscorlib]System.Text.StringBuilder::.ctor(int32)
        IL_0007:  stloc.0
        IL_0008:  ldc.i4.0
        IL_0009:  stloc.1
        IL_000a:  br IL_001d

        IL_000f:  ldloc.0
        IL_0010:  ldarg.0
        IL_0011:  ldloc.1
        IL_0012:  ldelem.u1
        IL_0013:  callvirt instance class [mscorlib]System.Text.StringBuilder class [mscorlib]System.Text.StringBuilder::Append(unsigned int8)
        IL_0018:  stloc.0
        IL_0019:  ldloc.1
        IL_001a:  ldc.i4.1
        IL_001b:  add
        IL_001c:  stloc.1
        IL_001d:  ldloc.1
        IL_001e:  ldc.i4.s 0x10
        IL_0020:  blt IL_000f

        IL_0025:  ldloc.0
        IL_0026:  callvirt instance string class [mscorlib]System.Text.StringBuilder::ToString()
        IL_002b:  ret
}

.method public static class [mscorlib]System.String 
        md5_chan(class [mscorlib]System.IO.BinaryReader, int32) il managed
{
  .maxstack  4
  .locals init (int32, int32, int8[])

  // length-(position+param)<0
  ldarg.0
  callvirt   instance class [mscorlib]System.IO.Stream [mscorlib]System.IO.BinaryReader::get_BaseStream()
  callvirt   instance int64 [mscorlib]System.IO.Stream::get_Length()
  ldarg.0
  callvirt   instance class [mscorlib]System.IO.Stream [mscorlib]System.IO.BinaryReader::get_BaseStream()
  callvirt   instance int64 [mscorlib]System.IO.Stream::get_Position()
  ldarg.1
  conv.i8 
  add
  sub
  ldc.i4.0
  conv.i8 
  bge.s      ELSEIF

  // levée d'exception End_of_file
  ldc.i4.2
  newarr     [mscorlib]System.Object
  dup
  ldc.i4.0
  ldsfld     object[] CamIL.Constant::End_of_file
  stelem.ref
  dup
  ldc.i4.1
  ldc.i4.0
  box [mscorlib]System.Int32
  stelem.ref
  call       void CamIL.Exception::raise(object[])


  // param<0 
ELSEIF:
  ldarg.1
  ldc.i4.0
  bge.s      ELSE

  ldarg.0
  callvirt   instance class [mscorlib]System.IO.Stream [mscorlib]System.IO.BinaryReader::get_BaseStream()
  callvirt   instance int64 [mscorlib]System.IO.Stream::get_Length()
  ldarg.0
  callvirt   instance class [mscorlib]System.IO.Stream [mscorlib]System.IO.BinaryReader::get_BaseStream()
  callvirt   instance int64 [mscorlib]System.IO.Stream::get_Position()
  sub	
  conv.ovf.i4
  stloc.0
  br.s       READ
  
  // cas général
ELSE: 
  ldarg.1
  stloc.0 
  // lecture des bytes
READ:
  ldloc.0
  newarr     [mscorlib]System.Byte
  stloc.2
  ldc.i4.0
  stloc.1
  br.s       START_LOOP
  LOOP:
  ldloc.2
  ldloc.1
  ldarg.0
  callvirt   instance unsigned int8 [mscorlib]System.IO.BinaryReader::ReadByte()
  stelem.i1
  ldloc.1
  ldc.i4.1
  add
  stloc.1
START_LOOP: 
  ldloc.1
  ldloc.0
  blt.s      LOOP


  // hachage md5 du byte[] créé
  newobj     instance void [mscorlib]System.Security.Cryptography.MD5CryptoServiceProvider::.ctor()
  ldloc.2
  callvirt   instance unsigned int8[] [mscorlib]System.Security.Cryptography.HashAlgorithm::ComputeHash(unsigned int8[])
  call       class [mscorlib]System.String CamIL.Md5::ascii_string(unsigned int8[])
  ret
} 


}