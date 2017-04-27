.class public auto ansi beforefieldinit Jacare
       extends [mscorlib]System.Object
{
  .class auto ansi nested public beforefieldinit ClassNotFoundException
         extends [mscorlib]System.Exception
  {
    .field private string name
    .field private string assname
    .method public hidebysig specialname rtspecialname 
            instance void  .ctor(string assname,
                                 string name) cil managed
    {
      // Code size       21 (0x15)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  call       instance void [mscorlib]System.Exception::.ctor()
      IL_0006:  ldarg.0
      IL_0007:  ldarg.1
      IL_0008:  stfld      string CamIL.Jacare/ClassNotFoundException::assname
      IL_000d:  ldarg.0
      IL_000e:  ldarg.2
      IL_000f:  stfld      string CamIL.Jacare/ClassNotFoundException::name
      IL_0014:  ret
    } // end of method ClassNotFoundException::.ctor

  } // end of class ClassNotFoundException

  .class auto ansi nested public beforefieldinit FieldNotFoundException
         extends [mscorlib]System.Exception
  {
    .field private class [mscorlib]System.Type clazz
    .field private string name
    .field private class [mscorlib]System.Type typ
    .method public hidebysig specialname rtspecialname 
            instance void  .ctor(class [mscorlib]System.Type clazz,
                                 string name,
                                 class [mscorlib]System.Type typ) cil managed
    {
      // Code size       28 (0x1c)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  call       instance void [mscorlib]System.Exception::.ctor()
      IL_0006:  ldarg.0
      IL_0007:  ldarg.1
      IL_0008:  stfld      class [mscorlib]System.Type CamIL.Jacare/FieldNotFoundException::clazz
      IL_000d:  ldarg.0
      IL_000e:  ldarg.2
      IL_000f:  stfld      string CamIL.Jacare/FieldNotFoundException::name
      IL_0014:  ldarg.0
      IL_0015:  ldarg.3
      IL_0016:  stfld      class [mscorlib]System.Type CamIL.Jacare/FieldNotFoundException::typ
      IL_001b:  ret
    } // end of method FieldNotFoundException::.ctor

  } // end of class FieldNotFoundException

  .class auto ansi nested public beforefieldinit MethodNotFoundException
         extends [mscorlib]System.Exception
  {
    .field private class [mscorlib]System.Type clazz
    .field private string name
    .field private class [mscorlib]System.Type[] args
    .field private class [mscorlib]System.Type rtype
    .method public hidebysig specialname rtspecialname 
            instance void  .ctor(class [mscorlib]System.Type clazz,
                                 string name,
                                 class [mscorlib]System.Type[] args,
                                 class [mscorlib]System.Type rtype) cil managed
    {
      // Code size       36 (0x24)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  call       instance void [mscorlib]System.Exception::.ctor()
      IL_0006:  ldarg.0
      IL_0007:  ldarg.1
      IL_0008:  stfld      class [mscorlib]System.Type CamIL.Jacare/MethodNotFoundException::clazz
      IL_000d:  ldarg.0
      IL_000e:  ldarg.2
      IL_000f:  stfld      string CamIL.Jacare/MethodNotFoundException::name
      IL_0014:  ldarg.0
      IL_0015:  ldarg.3
      IL_0016:  stfld      class [mscorlib]System.Type[] CamIL.Jacare/MethodNotFoundException::args
      IL_001b:  ldarg.0
      IL_001c:  ldarg.s    rtype
      IL_001e:  stfld      class [mscorlib]System.Type CamIL.Jacare/MethodNotFoundException::rtype
      IL_0023:  ret
    } // end of method MethodNotFoundException::.ctor

  } // end of class MethodNotFoundException

  .class auto ansi nested public beforefieldinit ConstructorNotFoundException
         extends [mscorlib]System.Exception
  {
    .field private class [mscorlib]System.Type clazz
    .field private class [mscorlib]System.Type[] typ
    .method public hidebysig specialname rtspecialname 
            instance void  .ctor(class [mscorlib]System.Type clazz,
                                 class [mscorlib]System.Type[] typ) cil managed
    {
      // Code size       21 (0x15)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  call       instance void [mscorlib]System.Exception::.ctor()
      IL_0006:  ldarg.0
      IL_0007:  ldarg.1
      IL_0008:  stfld      class [mscorlib]System.Type CamIL.Jacare/ConstructorNotFoundException::clazz
      IL_000d:  ldarg.0
      IL_000e:  ldarg.2
      IL_000f:  stfld      class [mscorlib]System.Type[] CamIL.Jacare/ConstructorNotFoundException::typ
      IL_0014:  ret
    } // end of method ConstructorNotFoundException::.ctor

  } // end of class ConstructorNotFoundException

  .method public hidebysig static class [mscorlib]System.Type 
          find_class(string assname,
                     string name) cil managed
  {
    // Code size       140 (0x8c)
    .maxstack  3
    .locals init (class [mscorlib]System.Reflection.Assembly V_0,
             class [System]System.Text.RegularExpressions.Regex V_1,
             class [System]System.Text.RegularExpressions.Match V_2,
             string V_3,
             class [mscorlib]System.Type V_4,
             class [mscorlib]System.Type V_5)
    IL_0000:  ldarg.0
    IL_0001:  ldsfld     string [mscorlib]System.String::Empty
    IL_0006:  callvirt   instance bool [mscorlib]System.String::Equals(string)
    IL_000b:  brfalse.s  IL_0017

    IL_000d:  ldarg.1
    IL_000e:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_0013:  stloc.s    V_5
    IL_0015:  br.s       IL_0089

    .try
    {
      IL_0017:  ldarg.0
      IL_0018:  call       class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::Load(string)
      IL_001d:  stloc.0
      IL_001e:  leave.s    IL_0063

    }  // end .try
    catch [mscorlib]System.IO.FileNotFoundException 
    {
      IL_0020:  pop
      IL_0021:  ldstr      "(.*)mscorlib.dll"
      IL_0026:  ldc.i4.1
      IL_0027:  newobj     instance void [System]System.Text.RegularExpressions.Regex::.ctor(string,
                                                                                             valuetype [System]System.Text.RegularExpressions.RegexOptions)
      IL_002c:  stloc.1
      IL_002d:  ldloc.1
      IL_002e:  ldstr      "mscorlib"
      IL_0033:  call       class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::Load(string)
      IL_0038:  callvirt   instance string [mscorlib]System.Reflection.Assembly::get_CodeBase()
      IL_003d:  callvirt   instance class [System]System.Text.RegularExpressions.Match [System]System.Text.RegularExpressions.Regex::Match(string)
      IL_0042:  stloc.2
      IL_0043:  ldloc.2
      IL_0044:  ldstr      "$1"
      IL_0049:  callvirt   instance string [System]System.Text.RegularExpressions.Match::Result(string)
      IL_004e:  stloc.3
      IL_004f:  ldloc.3
      IL_0050:  ldarg.0
      IL_0051:  ldstr      ".dll"
      IL_0056:  call       string [mscorlib]System.String::Concat(string,
                                                                  string,
                                                                  string)
      IL_005b:  call       class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::LoadFrom(string)
      IL_0060:  stloc.0
      IL_0061:  leave.s    IL_0063

    }  // end handler
    IL_0063:  ldloc.0
    IL_0064:  brtrue.s   IL_006e

    IL_0066:  ldarg.0
    IL_0067:  ldarg.1
    IL_0068:  newobj     instance void CamIL.Jacare/ClassNotFoundException::.ctor(string,
                                                                            string)
    IL_006d:  throw

    IL_006e:  ldloc.0
    IL_006f:  ldarg.1
    IL_0070:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Reflection.Assembly::GetType(string)
    IL_0075:  stloc.s    V_4
    IL_0077:  ldloc.s    V_4
    IL_0079:  brfalse.s  IL_0081

    IL_007b:  ldloc.s    V_4
    IL_007d:  stloc.s    V_5
    IL_007f:  br.s       IL_0089

    IL_0081:  ldarg.0
    IL_0082:  ldarg.1
    IL_0083:  newobj     instance void CamIL.Jacare/ClassNotFoundException::.ctor(string,
                                                                            string)
    IL_0088:  throw

    IL_0089:  ldloc.s    V_5
    IL_008b:  ret
  } // end of method CamIL.Jacare::find_class

  .method public hidebysig static object 
          get_null(object bidon) cil managed
  {
    // Code size       6 (0x6)
    .maxstack  1
    .locals init (object V_0)
    IL_0000:  ldnull
    IL_0001:  stloc.0
    IL_0002:  br.s       IL_0004

    IL_0004:  ldloc.0
    IL_0005:  ret
  } // end of method CamIL.Jacare::get_null

  .method public hidebysig static int32  is_null(object o) cil managed
  {
    // Code size       13 (0xd)
    .maxstack  1
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  brtrue.s   IL_0007

    IL_0003:  ldc.i4.1
    IL_0004:  stloc.0
    IL_0005:  br.s       IL_000b

    IL_0007:  ldc.i4.0
    IL_0008:  stloc.0
    IL_0009:  br.s       IL_000b

    IL_000b:  ldloc.0
    IL_000c:  ret
  } // end of method CamIL.Jacare::is_null

  .method public hidebysig static int32  is_assignable_from(class [mscorlib]System.Type t1,
                                                            class [mscorlib]System.Type t2) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  callvirt   instance bool [mscorlib]System.Type::IsAssignableFrom(class [mscorlib]System.Type)
    IL_0007:  brfalse.s  IL_000d

    IL_0009:  ldc.i4.1
    IL_000a:  stloc.0
    IL_000b:  br.s       IL_0011

    IL_000d:  ldc.i4.0
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::is_assignable_from

  .method public hidebysig static class [mscorlib]System.Type 
          get_superclass(class [mscorlib]System.Type t) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  2
    .locals init (class [mscorlib]System.Type V_0)
    IL_0000:  ldarg.0
    IL_0001:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Type::get_BaseType()
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::get_superclass

  .method public hidebysig static class [mscorlib]System.Type 
          get_object_class(object o) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  1
    .locals init (class [mscorlib]System.Type V_0)
    IL_0000:  ldarg.0
    IL_0001:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::get_object_class

  .method public hidebysig static int32  is_instance_of(object o,
                                                        class [mscorlib]System.Type clazz) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  callvirt   instance bool [mscorlib]System.Type::IsInstanceOfType(object)
    IL_0007:  brfalse.s  IL_000d

    IL_0009:  ldc.i4.1
    IL_000a:  stloc.0
    IL_000b:  br.s       IL_0011

    IL_000d:  ldc.i4.0
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::is_instance_of

  .method private hidebysig static class [mscorlib]System.Type 
          convert_targ_sumtype(class CamIL.Variant item) cil managed
  {
    // Code size       342 (0x156)
    .maxstack  3
    .locals init (class [mscorlib]System.Type V_0,
             class [mscorlib]System.Reflection.Assembly V_1,
             class [mscorlib]System.Type V_2,
             class [mscorlib]System.Type V_3,
             int32 V_4)
    IL_0000:  ldarg.0
    IL_0001:  ldfld      int32 CamIL.Variant::tag
    IL_0006:  stloc.s    V_4
    IL_0008:  ldloc.s    V_4
    IL_000a:  switch     ( 
                          IL_004c,
                          IL_005c,
                          IL_006c,
                          IL_007c,
                          IL_008c,
                          IL_009c,
                          IL_00ac,
                          IL_00bc,
                          IL_00cc,
                          IL_00d9,
                          IL_00e6,
                          IL_00f3,
                          IL_0100,
                          IL_0113)
    IL_0047:  br         IL_0146

    IL_004c:  ldstr      "System.Void"
    IL_0051:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_0056:  stloc.3
    IL_0057:  br         IL_0154

    IL_005c:  ldstr      "System.Boolean"
    IL_0061:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_0066:  stloc.3
    IL_0067:  br         IL_0154

    IL_006c:  ldstr      "System.Byte"
    IL_0071:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_0076:  stloc.3
    IL_0077:  br         IL_0154

    IL_007c:  ldstr      "System.Char"
    IL_0081:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_0086:  stloc.3
    IL_0087:  br         IL_0154

    IL_008c:  ldstr      "System.Int16"
    IL_0091:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_0096:  stloc.3
    IL_0097:  br         IL_0154

    IL_009c:  ldstr      "System.Int32"
    IL_00a1:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_00a6:  stloc.3
    IL_00a7:  br         IL_0154

    IL_00ac:  ldstr      "System.Int32"
    IL_00b1:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_00b6:  stloc.3
    IL_00b7:  br         IL_0154

    IL_00bc:  ldstr      "System.Int64"
    IL_00c1:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_00c6:  stloc.3
    IL_00c7:  br         IL_0154

    IL_00cc:  ldstr      "System.Float"
    IL_00d1:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_00d6:  stloc.3
    IL_00d7:  br.s       IL_0154

    IL_00d9:  ldstr      "System.Double"
    IL_00de:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_00e3:  stloc.3
    IL_00e4:  br.s       IL_0154

    IL_00e6:  ldstr      "System.String"
    IL_00eb:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_00f0:  stloc.3
    IL_00f1:  br.s       IL_0154

    IL_00f3:  ldstr      "System.Object"
    IL_00f8:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_00fd:  stloc.3
    IL_00fe:  br.s       IL_0154

    IL_0100:  ldarg.0
    IL_0101:  castclass  Jacare.Tclazz_OF_type_argument
    IL_0106:  ldfld      object Jacare.Tclazz_OF_type_argument::x0
    IL_010b:  castclass  [mscorlib]System.Type
    IL_0110:  stloc.3
    IL_0111:  br.s       IL_0154

    IL_0113:  ldarg.0
    IL_0114:  castclass  Jacare.Tarray_OF_type_argument
    IL_0119:  ldfld      class CamIL.Variant Jacare.Tarray_OF_type_argument::x0
    IL_011e:  call       class [mscorlib]System.Type CamIL.Jacare::convert_targ_sumtype(class CamIL.Variant)
    IL_0123:  stloc.0
    IL_0124:  ldloc.0
    IL_0125:  callvirt   instance class [mscorlib]System.Reflection.Assembly [mscorlib]System.Type::get_Assembly()
    IL_012a:  stloc.1
    IL_012b:  ldloc.1
    IL_012c:  ldloc.0
    IL_012d:  callvirt   instance string [mscorlib]System.Type::get_FullName()
    IL_0132:  ldstr      "[]"
    IL_0137:  call       string [mscorlib]System.String::Concat(string,
                                                                string)
    IL_013c:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Reflection.Assembly::GetType(string)
    IL_0141:  stloc.2
    IL_0142:  ldloc.2
    IL_0143:  stloc.3
    IL_0144:  br.s       IL_0154

    IL_0146:  ldstr      "assert false !!!\n"
    IL_014b:  call       void [mscorlib]System.Console::Write(string)
    IL_0150:  ldnull
    IL_0151:  stloc.3
    IL_0152:  br.s       IL_0154

    IL_0154:  ldloc.3
    IL_0155:  ret
  } // end of method CamIL.Jacare::convert_targ_sumtype

  .method public hidebysig static class [mscorlib]System.Reflection.FieldInfo 
          get_fieldID(class [mscorlib]System.Type clazz,
                      string name,
                      class CamIL.Variant typ) cil managed
  {
    // Code size       49 (0x31)
    .maxstack  4
    .locals init (class [mscorlib]System.Reflection.FieldInfo V_0,
             class [mscorlib]System.Type V_1,
             class [mscorlib]System.Reflection.FieldInfo V_2)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldc.i4.s   20
    IL_0004:  callvirt   instance class [mscorlib]System.Reflection.FieldInfo [mscorlib]System.Type::GetField(string,
                                                                                                              valuetype [mscorlib]System.Reflection.BindingFlags)
    IL_0009:  stloc.0
    IL_000a:  ldarg.2
    IL_000b:  call       class [mscorlib]System.Type CamIL.Jacare::convert_targ_sumtype(class CamIL.Variant)
    IL_0010:  stloc.1
    IL_0011:  ldloc.0
    IL_0012:  brfalse.s  IL_0022

    IL_0014:  ldloc.0
    IL_0015:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Reflection.FieldInfo::get_FieldType()
    IL_001a:  ldloc.1
    IL_001b:  callvirt   instance bool [mscorlib]System.Type::Equals(class [mscorlib]System.Type)
    IL_0020:  brtrue.s   IL_002b

    IL_0022:  ldarg.0
    IL_0023:  ldarg.1
    IL_0024:  ldloc.1
    IL_0025:  newobj     instance void CamIL.Jacare/FieldNotFoundException::.ctor(class [mscorlib]System.Type,
                                                                            string,
                                                                            class [mscorlib]System.Type)
    IL_002a:  throw

    IL_002b:  ldloc.0
    IL_002c:  stloc.2
    IL_002d:  br.s       IL_002f

    IL_002f:  ldloc.2
    IL_0030:  ret
  } // end of method CamIL.Jacare::get_fieldID

  .method public hidebysig static class [mscorlib]System.Reflection.FieldInfo 
          get_static_fieldID(class [mscorlib]System.Type clazz,
                             string name,
                             class CamIL.Variant typ) cil managed
  {
    // Code size       49 (0x31)
    .maxstack  4
    .locals init (class [mscorlib]System.Reflection.FieldInfo V_0,
             class [mscorlib]System.Type V_1,
             class [mscorlib]System.Reflection.FieldInfo V_2)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldc.i4.s   24
    IL_0004:  callvirt   instance class [mscorlib]System.Reflection.FieldInfo [mscorlib]System.Type::GetField(string,
                                                                                                              valuetype [mscorlib]System.Reflection.BindingFlags)
    IL_0009:  stloc.0
    IL_000a:  ldarg.2
    IL_000b:  call       class [mscorlib]System.Type CamIL.Jacare::convert_targ_sumtype(class CamIL.Variant)
    IL_0010:  stloc.1
    IL_0011:  ldloc.0
    IL_0012:  brfalse.s  IL_0022

    IL_0014:  ldloc.0
    IL_0015:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Reflection.FieldInfo::get_FieldType()
    IL_001a:  ldloc.1
    IL_001b:  callvirt   instance bool [mscorlib]System.Type::Equals(class [mscorlib]System.Type)
    IL_0020:  brtrue.s   IL_002b

    IL_0022:  ldarg.0
    IL_0023:  ldarg.1
    IL_0024:  ldloc.1
    IL_0025:  newobj     instance void CamIL.Jacare/FieldNotFoundException::.ctor(class [mscorlib]System.Type,
                                                                            string,
                                                                            class [mscorlib]System.Type)
    IL_002a:  throw

    IL_002b:  ldloc.0
    IL_002c:  stloc.2
    IL_002d:  br.s       IL_002f

    IL_002f:  ldloc.2
    IL_0030:  ret
  } // end of method CamIL.Jacare::get_static_fieldID

  .method private hidebysig static class [mscorlib]System.Reflection.MethodInfo 
          common_get_methodID(class [mscorlib]System.Type clazz,
                              string name,
                              object[] typ,
                              valuetype [mscorlib]System.Reflection.BindingFlags flgs) cil managed
  {
    // Code size       131 (0x83)
    .maxstack  6
    .locals init (object[] V_0,
             class CamIL.Variant V_1,
             class [mscorlib]System.Type[] V_2,
             class [mscorlib]System.Type V_3,
             int32 V_4,
             class [mscorlib]System.Reflection.MethodInfo V_5,
             class [mscorlib]System.Reflection.MethodInfo V_6)
    IL_0000:  ldarg.2
    IL_0001:  ldc.i4.0
    IL_0002:  ldelem.ref
    IL_0003:  castclass  object[]
    IL_0008:  stloc.0
    IL_0009:  ldarg.2
    IL_000a:  ldc.i4.1
    IL_000b:  ldelem.ref
    IL_000c:  castclass  CamIL.Variant
    IL_0011:  stloc.1
    IL_0012:  ldloc.0
    IL_0013:  ldlen
    IL_0014:  conv.i4
    IL_0015:  ldc.i4.1
    IL_0016:  sub
    IL_0017:  newarr     [mscorlib]System.Type
    IL_001c:  stloc.2
    IL_001d:  ldloc.1
    IL_001e:  call       class [mscorlib]System.Type CamIL.Jacare::convert_targ_sumtype(class CamIL.Variant)
    IL_0023:  stloc.3
    IL_0024:  ldc.i4.0
    IL_0025:  stloc.s    V_4
    IL_0027:  br.s       IL_0041

    IL_0029:  ldloc.2
    IL_002a:  ldloc.s    V_4
    IL_002c:  ldloc.0
    IL_002d:  ldloc.s    V_4
    IL_002f:  ldelem.ref
    IL_0030:  castclass  CamIL.Variant
    IL_0035:  call       class [mscorlib]System.Type CamIL.Jacare::convert_targ_sumtype(class CamIL.Variant)
    IL_003a:  stelem.ref
    IL_003b:  ldloc.s    V_4
    IL_003d:  ldc.i4.1
    IL_003e:  add
    IL_003f:  stloc.s    V_4
    IL_0041:  ldloc.s    V_4
    IL_0043:  ldloc.2
    IL_0044:  ldlen
    IL_0045:  conv.i4
    IL_0046:  blt.s      IL_0029

    IL_0048:  ldarg.0
    IL_0049:  ldarg.1
    IL_004a:  ldc.i4.s   16
    IL_004c:  ldarg.3
    IL_004d:  or
    IL_004e:  ldnull
    IL_004f:  ldloc.2
    IL_0050:  ldc.i4.0
    IL_0051:  newarr     [mscorlib]System.Reflection.ParameterModifier
    IL_0056:  callvirt   instance class [mscorlib]System.Reflection.MethodInfo [mscorlib]System.Type::GetMethod(string,
                                                                                                                valuetype [mscorlib]System.Reflection.BindingFlags,
                                                                                                                class [mscorlib]System.Reflection.Binder,
                                                                                                                class [mscorlib]System.Type[],
                                                                                                                valuetype [mscorlib]System.Reflection.ParameterModifier[])
    IL_005b:  stloc.s    V_5
    IL_005d:  ldloc.s    V_5
    IL_005f:  brfalse.s  IL_0070

    IL_0061:  ldloc.s    V_5
    IL_0063:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Reflection.MethodInfo::get_ReturnType()
    IL_0068:  ldloc.3
    IL_0069:  callvirt   instance bool [mscorlib]System.Type::Equals(class [mscorlib]System.Type)
    IL_006e:  brtrue.s   IL_007a

    IL_0070:  ldarg.0
    IL_0071:  ldarg.1
    IL_0072:  ldloc.2
    IL_0073:  ldloc.3
    IL_0074:  newobj     instance void CamIL.Jacare/MethodNotFoundException::.ctor(class [mscorlib]System.Type,
                                                                             string,
                                                                             class [mscorlib]System.Type[],
                                                                             class [mscorlib]System.Type)
    IL_0079:  throw

    IL_007a:  ldloc.s    V_5
    IL_007c:  stloc.s    V_6
    IL_007e:  br.s       IL_0080

    IL_0080:  ldloc.s    V_6
    IL_0082:  ret
  } // end of method CamIL.Jacare::common_get_methodID

  .method public hidebysig static class [mscorlib]System.Reflection.MethodInfo 
          get_methodID(class [mscorlib]System.Type clazz,
                       string name,
                       object[] typ) cil managed
  {
    // Code size       14 (0xe)
    .maxstack  4
    .locals init (class [mscorlib]System.Reflection.MethodInfo V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  ldc.i4.4
    IL_0004:  call       class [mscorlib]System.Reflection.MethodInfo CamIL.Jacare::common_get_methodID(class [mscorlib]System.Type,
                                                                                                  string,
                                                                                                  object[],
                                                                                                  valuetype [mscorlib]System.Reflection.BindingFlags)
    IL_0009:  stloc.0
    IL_000a:  br.s       IL_000c

    IL_000c:  ldloc.0
    IL_000d:  ret
  } // end of method CamIL.Jacare::get_methodID

  .method public hidebysig static class [mscorlib]System.Reflection.MethodInfo 
          get_static_methodID(class [mscorlib]System.Type clazz,
                              string name,
                              object[] typ) cil managed
  {
    // Code size       14 (0xe)
    .maxstack  4
    .locals init (class [mscorlib]System.Reflection.MethodInfo V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  ldc.i4.8
    IL_0004:  call       class [mscorlib]System.Reflection.MethodInfo CamIL.Jacare::common_get_methodID(class [mscorlib]System.Type,
                                                                                                  string,
                                                                                                  object[],
                                                                                                  valuetype [mscorlib]System.Reflection.BindingFlags)
    IL_0009:  stloc.0
    IL_000a:  br.s       IL_000c

    IL_000c:  ldloc.0
    IL_000d:  ret
  } // end of method CamIL.Jacare::get_static_methodID

  .method public hidebysig static class [mscorlib]System.Reflection.ConstructorInfo 
          get_constructorID(class [mscorlib]System.Type clazz,
                            object[] typ) cil managed
  {
    // Code size       75 (0x4b)
    .maxstack  5
    .locals init (class [mscorlib]System.Type[] V_0,
             int32 V_1,
             class [mscorlib]System.Reflection.ConstructorInfo V_2,
             class [mscorlib]System.Reflection.ConstructorInfo V_3)
    IL_0000:  ldarg.1
    IL_0001:  ldlen
    IL_0002:  conv.i4
    IL_0003:  ldc.i4.1
    IL_0004:  sub
    IL_0005:  newarr     [mscorlib]System.Type
    IL_000a:  stloc.0
    IL_000b:  ldc.i4.0
    IL_000c:  stloc.1
    IL_000d:  br.s       IL_0023

    IL_000f:  ldloc.0
    IL_0010:  ldloc.1
    IL_0011:  ldarg.1
    IL_0012:  ldloc.1
    IL_0013:  ldelem.ref
    IL_0014:  castclass  CamIL.Variant
    IL_0019:  call       class [mscorlib]System.Type CamIL.Jacare::convert_targ_sumtype(class CamIL.Variant)
    IL_001e:  stelem.ref
    IL_001f:  ldloc.1
    IL_0020:  ldc.i4.1
    IL_0021:  add
    IL_0022:  stloc.1
    IL_0023:  ldloc.1
    IL_0024:  ldloc.0
    IL_0025:  ldlen
    IL_0026:  conv.i4
    IL_0027:  blt.s      IL_000f

    IL_0029:  ldarg.0
    IL_002a:  ldc.i4.s   20
    IL_002c:  ldnull
    IL_002d:  ldloc.0
    IL_002e:  ldc.i4.0
    IL_002f:  newarr     [mscorlib]System.Reflection.ParameterModifier
    IL_0034:  callvirt   instance class [mscorlib]System.Reflection.ConstructorInfo [mscorlib]System.Type::GetConstructor(valuetype [mscorlib]System.Reflection.BindingFlags,
                                                                                                                          class [mscorlib]System.Reflection.Binder,
                                                                                                                          class [mscorlib]System.Type[],
                                                                                                                          valuetype [mscorlib]System.Reflection.ParameterModifier[])
    IL_0039:  stloc.2
    IL_003a:  ldloc.2
    IL_003b:  brtrue.s   IL_0045

    IL_003d:  ldarg.0
    IL_003e:  ldloc.0
    IL_003f:  newobj     instance void CamIL.Jacare/ConstructorNotFoundException::.ctor(class [mscorlib]System.Type,
                                                                                  class [mscorlib]System.Type[])
    IL_0044:  throw

    IL_0045:  ldloc.2
    IL_0046:  stloc.3
    IL_0047:  br.s       IL_0049

    IL_0049:  ldloc.3
    IL_004a:  ret
  } // end of method CamIL.Jacare::get_constructorID

  .method private hidebysig static object 
          convert_sumtype(class CamIL.Variant item) cil managed
  {
    // Code size       248 (0xf8)
    .maxstack  1
    .locals init (object V_0,
             object V_1,
             int32 V_2)
    IL_0000:  ldnull
    IL_0001:  stloc.0
    IL_0002:  ldarg.0
    IL_0003:  ldfld      int32 CamIL.Variant::tag
    IL_0008:  stloc.2
    IL_0009:  ldloc.2
    IL_000a:  switch     ( 
                          IL_003c,
                          IL_0052,
                          IL_0069,
                          IL_007d,
                          IL_0091,
                          IL_00a4,
                          IL_00a6,
                          IL_00ba,
                          IL_00ce,
                          IL_00e2)
    IL_0037:  br         IL_00f0

    IL_003c:  ldarg.0
    IL_003d:  castclass  Jacare.Boolean_OF_argument
    IL_0042:  ldfld      bool Jacare.Boolean_OF_argument::x0
    IL_0047:  box        [mscorlib]System.Boolean
    IL_004c:  stloc.0
    IL_004d:  br         IL_00f2

    IL_0052:  ldarg.0
    IL_0053:  castclass  Jacare.Byte_OF_argument
    IL_0058:  ldfld      int32 Jacare.Byte_OF_argument::x0
    IL_005d:  conv.u1
    IL_005e:  box        [mscorlib]System.Byte
    IL_0063:  stloc.0
    IL_0064:  br         IL_00f2

    IL_0069:  ldarg.0
    IL_006a:  castclass  Jacare.Char_OF_argument
    IL_006f:  ldfld      int32 Jacare.Char_OF_argument::x0
    IL_0074:  conv.u2
    IL_0075:  box        [mscorlib]System.Char
    IL_007a:  stloc.0
    IL_007b:  br.s       IL_00f2

    IL_007d:  ldarg.0
    IL_007e:  castclass  Jacare.Short_OF_argument
    IL_0083:  ldfld      int32 Jacare.Short_OF_argument::x0
    IL_0088:  conv.i2
    IL_0089:  box        [mscorlib]System.Int16
    IL_008e:  stloc.0
    IL_008f:  br.s       IL_00f2

    IL_0091:  ldarg.0
    IL_0092:  castclass  Jacare.Camlint_OF_argument
    IL_0097:  ldfld      int32 Jacare.Camlint_OF_argument::x0
    IL_009c:  box        [mscorlib]System.Int32
    IL_00a1:  stloc.0
    IL_00a2:  br.s       IL_00f2

    IL_00a4:  br.s       IL_00f2

    IL_00a6:  ldarg.0
    IL_00a7:  castclass  Jacare.Camlint_OF_argument
    IL_00ac:  ldfld      int32 Jacare.Camlint_OF_argument::x0
    IL_00b1:  conv.i8
    IL_00b2:  box        [mscorlib]System.Int64
    IL_00b7:  stloc.0
    IL_00b8:  br.s       IL_00f2

    IL_00ba:  ldarg.0
    IL_00bb:  castclass  Jacare.Float_OF_argument
    IL_00c0:  ldfld      float64 Jacare.Float_OF_argument::x0
    IL_00c5:  conv.r4
    IL_00c6:  box        [mscorlib]System.Single
    IL_00cb:  stloc.0
    IL_00cc:  br.s       IL_00f2

    IL_00ce:  ldarg.0
    IL_00cf:  castclass  Jacare.Double_OF_argument
    IL_00d4:  ldfld      float64 Jacare.Double_OF_argument::x0
    IL_00d9:  conv.r8
    IL_00da:  box        [mscorlib]System.Double
    IL_00df:  stloc.0
    IL_00e0:  br.s       IL_00f2

    IL_00e2:  ldarg.0
    IL_00e3:  castclass  Jacare.Obj_OF_argument
    IL_00e8:  ldfld      object Jacare.Obj_OF_argument::x0
    IL_00ed:  stloc.0
    IL_00ee:  br.s       IL_00f2

    IL_00f0:  br.s       IL_00f2

    IL_00f2:  ldloc.0
    IL_00f3:  stloc.1
    IL_00f4:  br.s       IL_00f6

    IL_00f6:  ldloc.1
    IL_00f7:  ret
  } // end of method CamIL.Jacare::convert_sumtype

  .method public hidebysig static object 
          call_constructor(class [mscorlib]System.Reflection.ConstructorInfo ctor,
                           object[] vargs) cil managed
  {
    // Code size       53 (0x35)
    .maxstack  4
    .locals init (object[] V_0,
             int32 V_1,
             object V_2)
    IL_0000:  ldarg.1
    IL_0001:  ldlen
    IL_0002:  conv.i4
    IL_0003:  ldc.i4.1
    IL_0004:  sub
    IL_0005:  newarr     [mscorlib]System.Object
    IL_000a:  stloc.0
    IL_000b:  ldc.i4.0
    IL_000c:  stloc.1
    IL_000d:  br.s       IL_0023

    IL_000f:  ldloc.0
    IL_0010:  ldloc.1
    IL_0011:  ldarg.1
    IL_0012:  ldloc.1
    IL_0013:  ldelem.ref
    IL_0014:  castclass  CamIL.Variant
    IL_0019:  call       object CamIL.Jacare::convert_sumtype(class CamIL.Variant)
    IL_001e:  stelem.ref
    IL_001f:  ldloc.1
    IL_0020:  ldc.i4.1
    IL_0021:  add
    IL_0022:  stloc.1
    IL_0023:  ldloc.1
    IL_0024:  ldloc.0
    IL_0025:  ldlen
    IL_0026:  conv.i4
    IL_0027:  blt.s      IL_000f

    IL_0029:  ldarg.0
    IL_002a:  ldloc.0
    IL_002b:  callvirt   instance object [mscorlib]System.Reflection.ConstructorInfo::Invoke(object[])
    IL_0030:  stloc.2
    IL_0031:  br.s       IL_0033

    IL_0033:  ldloc.2
    IL_0034:  ret
  } // end of method CamIL.Jacare::call_constructor

  .method public hidebysig static object 
          call_object_method(object ob,
                             class [mscorlib]System.Reflection.MethodInfo mi,
                             object[] vargs) cil managed
  {
    // Code size       54 (0x36)
    .maxstack  4
    .locals init (object[] V_0,
             int32 V_1,
             object V_2)
    IL_0000:  ldarg.2
    IL_0001:  ldlen
    IL_0002:  conv.i4
    IL_0003:  ldc.i4.1
    IL_0004:  sub
    IL_0005:  newarr     [mscorlib]System.Object
    IL_000a:  stloc.0
    IL_000b:  ldc.i4.0
    IL_000c:  stloc.1
    IL_000d:  br.s       IL_0023

    IL_000f:  ldloc.0
    IL_0010:  ldloc.1
    IL_0011:  ldarg.2
    IL_0012:  ldloc.1
    IL_0013:  ldelem.ref
    IL_0014:  castclass  CamIL.Variant
    IL_0019:  call       object CamIL.Jacare::convert_sumtype(class CamIL.Variant)
    IL_001e:  stelem.ref
    IL_001f:  ldloc.1
    IL_0020:  ldc.i4.1
    IL_0021:  add
    IL_0022:  stloc.1
    IL_0023:  ldloc.1
    IL_0024:  ldloc.0
    IL_0025:  ldlen
    IL_0026:  conv.i4
    IL_0027:  blt.s      IL_000f

    IL_0029:  ldarg.1
    IL_002a:  ldarg.0
    IL_002b:  ldloc.0
    IL_002c:  callvirt   instance object [mscorlib]System.Reflection.MethodInfo::Invoke(object,
                                                                                        object[])
    IL_0031:  stloc.2
    IL_0032:  br.s       IL_0034

    IL_0034:  ldloc.2
    IL_0035:  ret
  } // end of method CamIL.Jacare::call_object_method

  .method public hidebysig static int32  call_camlint_method(object ob,
                                                             class [mscorlib]System.Reflection.MethodInfo mi,
                                                             object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_object_method(object,
                                                           class [mscorlib]System.Reflection.MethodInfo,
                                                           object[])
    IL_0008:  unbox      [mscorlib]System.Int32
    IL_000d:  ldind.i4
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_camlint_method

  .method public hidebysig static int32  call_boolean_method(object ob,
                                                             class [mscorlib]System.Reflection.MethodInfo mi,
                                                             object[] vargs) cil managed
  {
    // Code size       26 (0x1a)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_object_method(object,
                                                           class [mscorlib]System.Reflection.MethodInfo,
                                                           object[])
    IL_0008:  unbox      [mscorlib]System.Boolean
    IL_000d:  ldind.i1
    IL_000e:  brfalse.s  IL_0014

    IL_0010:  ldc.i4.1
    IL_0011:  stloc.0
    IL_0012:  br.s       IL_0018

    IL_0014:  ldc.i4.0
    IL_0015:  stloc.0
    IL_0016:  br.s       IL_0018

    IL_0018:  ldloc.0
    IL_0019:  ret
  } // end of method CamIL.Jacare::call_boolean_method

  .method public hidebysig static int32  call_char_method(object ob,
                                                          class [mscorlib]System.Reflection.MethodInfo mi,
                                                          object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_object_method(object,
                                                           class [mscorlib]System.Reflection.MethodInfo,
                                                           object[])
    IL_0008:  unbox      [mscorlib]System.Char
    IL_000d:  ldind.u2
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_char_method

  .method public hidebysig static int32  call_byte_method(object ob,
                                                          class [mscorlib]System.Reflection.MethodInfo mi,
                                                          object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_object_method(object,
                                                           class [mscorlib]System.Reflection.MethodInfo,
                                                           object[])
    IL_0008:  unbox      [mscorlib]System.Byte
    IL_000d:  ldind.u1
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_byte_method

  .method public hidebysig static int32  call_short_method(object ob,
                                                           class [mscorlib]System.Reflection.MethodInfo mi,
                                                           object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_object_method(object,
                                                           class [mscorlib]System.Reflection.MethodInfo,
                                                           object[])
    IL_0008:  unbox      [mscorlib]System.Int16
    IL_000d:  ldind.i2
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_short_method

  .method public hidebysig static float64 
          call_double_method(object ob,
                             class [mscorlib]System.Reflection.MethodInfo mi,
                             object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (float64 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_object_method(object,
                                                           class [mscorlib]System.Reflection.MethodInfo,
                                                           object[])
    IL_0008:  unbox      [mscorlib]System.Double
    IL_000d:  ldind.r8
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_double_method

  .method public hidebysig static int64  call_long_method(object ob,
                                                          class [mscorlib]System.Reflection.MethodInfo mi,
                                                          object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int64 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_object_method(object,
                                                           class [mscorlib]System.Reflection.MethodInfo,
                                                           object[])
    IL_0008:  unbox      [mscorlib]System.Int64
    IL_000d:  ldind.i8
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_long_method

  .method public hidebysig static string 
          string_to_cts(class [mscorlib]System.Text.StringBuilder sb) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  1
    .locals init (string V_0)
    IL_0000:  ldarg.0
    IL_0001:  callvirt   instance string [mscorlib]System.Text.StringBuilder::ToString()
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::string_to_cts

  .method public hidebysig static class [mscorlib]System.Text.StringBuilder 
          string_from_cts(string s) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  2
    .locals init (class [mscorlib]System.Text.StringBuilder V_0)
    IL_0000:  ldarg.0
    IL_0001:  newobj     instance void [mscorlib]System.Text.StringBuilder::.ctor(string)
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::string_from_cts

  .method public hidebysig static object 
          get_object_field(object ob,
                           class [mscorlib]System.Reflection.FieldInfo fi) cil managed
  {
    // Code size       12 (0xc)
    .maxstack  2
    .locals init (object V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  callvirt   instance object [mscorlib]System.Reflection.FieldInfo::GetValue(object)
    IL_0007:  stloc.0
    IL_0008:  br.s       IL_000a

    IL_000a:  ldloc.0
    IL_000b:  ret
  } // end of method CamIL.Jacare::get_object_field

  .method public hidebysig static int32  get_camlint_field(object ob,
                                                           class [mscorlib]System.Reflection.FieldInfo fi) cil managed
  {
    // Code size       18 (0x12)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  callvirt   instance object [mscorlib]System.Reflection.FieldInfo::GetValue(object)
    IL_0007:  unbox      [mscorlib]System.Int32
    IL_000c:  ldind.i4
    IL_000d:  stloc.0
    IL_000e:  br.s       IL_0010

    IL_0010:  ldloc.0
    IL_0011:  ret
  } // end of method CamIL.Jacare::get_camlint_field

  .method public hidebysig static int32  get_byte_field(object ob,
                                                        class [mscorlib]System.Reflection.FieldInfo fi) cil managed
  {
    // Code size       18 (0x12)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  callvirt   instance object [mscorlib]System.Reflection.FieldInfo::GetValue(object)
    IL_0007:  unbox      [mscorlib]System.Byte
    IL_000c:  ldind.u1
    IL_000d:  stloc.0
    IL_000e:  br.s       IL_0010

    IL_0010:  ldloc.0
    IL_0011:  ret
  } // end of method CamIL.Jacare::get_byte_field

  .method public hidebysig static int32  get_char_field(object ob,
                                                        class [mscorlib]System.Reflection.FieldInfo fi) cil managed
  {
    // Code size       18 (0x12)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  callvirt   instance object [mscorlib]System.Reflection.FieldInfo::GetValue(object)
    IL_0007:  unbox      [mscorlib]System.Char
    IL_000c:  ldind.u2
    IL_000d:  stloc.0
    IL_000e:  br.s       IL_0010

    IL_0010:  ldloc.0
    IL_0011:  ret
  } // end of method CamIL.Jacare::get_char_field

  .method public hidebysig static int32  get_short_field(object ob,
                                                         class [mscorlib]System.Reflection.FieldInfo fi) cil managed
  {
    // Code size       18 (0x12)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  callvirt   instance object [mscorlib]System.Reflection.FieldInfo::GetValue(object)
    IL_0007:  unbox      [mscorlib]System.Int16
    IL_000c:  ldind.i2
    IL_000d:  stloc.0
    IL_000e:  br.s       IL_0010

    IL_0010:  ldloc.0
    IL_0011:  ret
  } // end of method CamIL.Jacare::get_short_field

  .method public hidebysig static float64 
          get_double_field(object ob,
                           class [mscorlib]System.Reflection.FieldInfo fi) cil managed
  {
    // Code size       18 (0x12)
    .maxstack  2
    .locals init (float64 V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  callvirt   instance object [mscorlib]System.Reflection.FieldInfo::GetValue(object)
    IL_0007:  unbox      [mscorlib]System.Double
    IL_000c:  ldind.r8
    IL_000d:  stloc.0
    IL_000e:  br.s       IL_0010

    IL_0010:  ldloc.0
    IL_0011:  ret
  } // end of method CamIL.Jacare::get_double_field

  .method public hidebysig static int64  get_long_field(object ob,
                                                        class [mscorlib]System.Reflection.FieldInfo fi) cil managed
  {
    // Code size       18 (0x12)
    .maxstack  2
    .locals init (int64 V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  callvirt   instance object [mscorlib]System.Reflection.FieldInfo::GetValue(object)
    IL_0007:  unbox      [mscorlib]System.Int64
    IL_000c:  ldind.i8
    IL_000d:  stloc.0
    IL_000e:  br.s       IL_0010

    IL_0010:  ldloc.0
    IL_0011:  ret
  } // end of method CamIL.Jacare::get_long_field

  .method public hidebysig static object 
          set_object_field(object ob,
                           class [mscorlib]System.Reflection.FieldInfo fi,
                           object val) cil managed
  {
    // Code size       14 (0xe)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  ldarg.2
    IL_0003:  callvirt   instance void [mscorlib]System.Reflection.FieldInfo::SetValue(object,
                                                                                       object)
    IL_0008:  ldnull
    IL_0009:  stloc.0
    IL_000a:  br.s       IL_000c

    IL_000c:  ldloc.0
    IL_000d:  ret
  } // end of method CamIL.Jacare::set_object_field

  .method public hidebysig static object 
          set_camlint_field(object ob,
                            class [mscorlib]System.Reflection.FieldInfo fi,
                            int32 val) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  ldarg.2
    IL_0003:  box        [mscorlib]System.Int32
    IL_0008:  callvirt   instance void [mscorlib]System.Reflection.FieldInfo::SetValue(object,
                                                                                       object)
    IL_000d:  ldnull
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::set_camlint_field

  .method public hidebysig static object 
          set_byte_field(object ob,
                         class [mscorlib]System.Reflection.FieldInfo fi,
                         int32 val) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  conv.u1
    IL_0004:  box        [mscorlib]System.Byte
    IL_0009:  call       object CamIL.Jacare::set_object_field(object,
                                                         class [mscorlib]System.Reflection.FieldInfo,
                                                         object)
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::set_byte_field

  .method public hidebysig static object 
          set_char_field(object ob,
                         class [mscorlib]System.Reflection.FieldInfo fi,
                         int32 val) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  conv.u2
    IL_0004:  box        [mscorlib]System.Char
    IL_0009:  call       object CamIL.Jacare::set_object_field(object,
                                                         class [mscorlib]System.Reflection.FieldInfo,
                                                         object)
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::set_char_field

  .method public hidebysig static object 
          set_short_field(object ob,
                          class [mscorlib]System.Reflection.FieldInfo fi,
                          int32 val) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  conv.i2
    IL_0004:  box        [mscorlib]System.Int16
    IL_0009:  call       object CamIL.Jacare::set_object_field(object,
                                                         class [mscorlib]System.Reflection.FieldInfo,
                                                         object)
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::set_short_field

  .method public hidebysig static object 
          set_float_field(object ob,
                          class [mscorlib]System.Reflection.FieldInfo fi,
                          float64 val) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  conv.r4
    IL_0004:  box        [mscorlib]System.Single
    IL_0009:  call       object CamIL.Jacare::set_object_field(object,
                                                         class [mscorlib]System.Reflection.FieldInfo,
                                                         object)
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::set_float_field

  .method public hidebysig static object 
          set_double_field(object ob,
                           class [mscorlib]System.Reflection.FieldInfo fi,
                           float64 val) cil managed
  {
    // Code size       18 (0x12)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  box        [mscorlib]System.Double
    IL_0008:  call       object CamIL.Jacare::set_object_field(object,
                                                         class [mscorlib]System.Reflection.FieldInfo,
                                                         object)
    IL_000d:  stloc.0
    IL_000e:  br.s       IL_0010

    IL_0010:  ldloc.0
    IL_0011:  ret
  } // end of method CamIL.Jacare::set_double_field

  .method public hidebysig static object 
          set_long_field(object ob,
                         class [mscorlib]System.Reflection.FieldInfo fi,
                         int64 val) cil managed
  {
    // Code size       18 (0x12)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  box        [mscorlib]System.Int64
    IL_0008:  call       object CamIL.Jacare::set_object_field(object,
                                                         class [mscorlib]System.Reflection.FieldInfo,
                                                         object)
    IL_000d:  stloc.0
    IL_000e:  br.s       IL_0010

    IL_0010:  ldloc.0
    IL_0011:  ret
  } // end of method CamIL.Jacare::set_long_field

  .method public hidebysig static int32  get_array_length(object[] ob) cil managed
  {
    // Code size       8 (0x8)
    .maxstack  1
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldlen
    IL_0002:  conv.i4
    IL_0003:  stloc.0
    IL_0004:  br.s       IL_0006

    IL_0006:  ldloc.0
    IL_0007:  ret
  } // end of method CamIL.Jacare::get_array_length

  .method public hidebysig static class [mscorlib]System.Array 
          new_object_array(int32 size,
                           class [mscorlib]System.Type typ) cil managed
  {
    // Code size       17 (0x11)
    .maxstack  2
    .locals init (object[] V_0)
    IL_0000:  ldarg.1
    IL_0001:  ldarg.0
    IL_0002:  call       class [mscorlib]System.Array [mscorlib]System.Array::CreateInstance(class [mscorlib]System.Type,
                                                                                             int32)
    IL_0010:  ret
  } // end of method CamIL.Jacare::new_object_array

	
  .method public hidebysig static object 
          get_object_array_element(class [mscorlib]System.Array arr,
                                   int32 i) cil managed
  {
    // Code size       8 (0x8)
    .maxstack  2
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
	      callvirt instance object [mscorlib]System.Array::GetValue(int32)
    IL_0007:  ret
  } // end of method CamIL.Jacare::get_object_array_element

  .method public hidebysig static object 
          set_object_array_element(class [mscorlib]System.Array arr,
                                   int32 i,
                                   object val) cil managed
  {
    // Code size       10 (0xa)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.2
    IL_0002:  ldarg.1
	      callvirt instance void [mscorlib]System.Array::SetValue(object,int32)
    IL_0004:  ldnull
    IL_0009:  ret
  } // end of method CamIL.Jacare::set_object_array_element

	
  .method public hidebysig static class [mscorlib]System.Array 
          new_boolean_array(int32 size) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  1
    .locals init (class [mscorlib]System.Array V_0)
    IL_0000:  ldarg.0
    IL_0001:  newarr     [mscorlib]System.Boolean
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::new_boolean_array

  .method public hidebysig static int32  get_boolean_array_element(class [mscorlib]System.Array arr,
                                                                   int32 i) cil managed
  {
    // Code size       20 (0x14)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  bool[]
    IL_0006:  ldarg.1
    IL_0007:  ldelem.i1
    IL_0008:  brfalse.s  IL_000e

    IL_000a:  ldc.i4.1
    IL_000b:  stloc.0
    IL_000c:  br.s       IL_0012

    IL_000e:  ldc.i4.0
    IL_000f:  stloc.0
    IL_0010:  br.s       IL_0012

    IL_0012:  ldloc.0
    IL_0013:  ret
  } // end of method CamIL.Jacare::get_boolean_array_element

  .method public hidebysig static object 
          set_boolean_array_element(class [mscorlib]System.Array arr,
                                    int32 i,
                                    int32 val) cil managed
  {
    // Code size       21 (0x15)
    .maxstack  4
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  bool[]
    IL_0006:  ldarg.1
    IL_0007:  ldarg.2
    IL_0008:  ldc.i4.0
    IL_0009:  ceq
    IL_000b:  ldc.i4.0
    IL_000c:  ceq
    IL_000e:  stelem.i1
    IL_000f:  ldnull
    IL_0010:  stloc.0
    IL_0011:  br.s       IL_0013

    IL_0013:  ldloc.0
    IL_0014:  ret
  } // end of method CamIL.Jacare::set_boolean_array_element

  .method public hidebysig static class [mscorlib]System.Array 
          new_byte_array(int32 size) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  1
    .locals init (class [mscorlib]System.Array V_0)
    IL_0000:  ldarg.0
    IL_0001:  newarr     [mscorlib]System.Byte
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::new_byte_array

  .method public hidebysig static int32  get_byte_array_element(class [mscorlib]System.Array arr,
                                                                int32 i) cil managed
  {
    // Code size       13 (0xd)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  unsigned int8[]
    IL_0006:  ldarg.1
    IL_0007:  ldelem.u1
    IL_0008:  stloc.0
    IL_0009:  br.s       IL_000b

    IL_000b:  ldloc.0
    IL_000c:  ret
  } // end of method CamIL.Jacare::get_byte_array_element

  .method public hidebysig static object 
          set_byte_array_element(class [mscorlib]System.Array arr,
                                 int32 i,
                                 int32 val) cil managed
  {
    // Code size       16 (0x10)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  unsigned int8[]
    IL_0006:  ldarg.1
    IL_0007:  ldarg.2
    IL_0008:  conv.u1
    IL_0009:  stelem.i1
    IL_000a:  ldnull
    IL_000b:  stloc.0
    IL_000c:  br.s       IL_000e

    IL_000e:  ldloc.0
    IL_000f:  ret
  } // end of method CamIL.Jacare::set_byte_array_element

  .method public hidebysig static object 
          get_byte_array_region(class [mscorlib]System.Array arr,
                                int32 srcidx,
                                class [mscorlib]System.Text.StringBuilder str,
                                int32 dstidx,
                                int32 length) cil managed
  {
    // Code size       38 (0x26)
    .maxstack  5
    .locals init (int32 V_0,
             object V_1)
    IL_0000:  ldc.i4.0
    IL_0001:  stloc.0
    IL_0002:  br.s       IL_001b

    IL_0004:  ldarg.2
    IL_0005:  ldarg.3
    IL_0006:  ldloc.0
    IL_0007:  add
    IL_0008:  ldarg.0
    IL_0009:  castclass  unsigned int8[]
    IL_000e:  ldarg.1
    IL_000f:  ldloc.0
    IL_0010:  add
    IL_0011:  ldelem.u1
    IL_0012:  callvirt   instance void [mscorlib]System.Text.StringBuilder::set_Chars(int32,
                                                                                      char)
    IL_0017:  ldloc.0
    IL_0018:  ldc.i4.1
    IL_0019:  add
    IL_001a:  stloc.0
    IL_001b:  ldloc.0
    IL_001c:  ldarg.s    length
    IL_001e:  blt.s      IL_0004

    IL_0020:  ldnull
    IL_0021:  stloc.1
    IL_0022:  br.s       IL_0024

    IL_0024:  ldloc.1
    IL_0025:  ret
  } // end of method CamIL.Jacare::get_byte_array_region

  .method public hidebysig static object 
          set_byte_array_region(string str,
                                int32 srcidx,
                                class [mscorlib]System.Array arr,
                                int32 dstidx,
                                int32 length) cil managed
  {
    // Code size       39 (0x27)
    .maxstack  5
    .locals init (int32 V_0,
             object V_1)
    IL_0000:  ldc.i4.0
    IL_0001:  stloc.0
    IL_0002:  br.s       IL_001c

    IL_0004:  ldarg.2
    IL_0005:  castclass  unsigned int8[]
    IL_000a:  ldarg.3
    IL_000b:  ldloc.0
    IL_000c:  add
    IL_000d:  ldarg.0
    IL_000e:  ldarg.1
    IL_000f:  ldloc.0
    IL_0010:  add
    IL_0011:  callvirt   instance char [mscorlib]System.String::get_Chars(int32)
    IL_0016:  conv.u1
    IL_0017:  stelem.i1
    IL_0018:  ldloc.0
    IL_0019:  ldc.i4.1
    IL_001a:  add
    IL_001b:  stloc.0
    IL_001c:  ldloc.0
    IL_001d:  ldarg.s    length
    IL_001f:  blt.s      IL_0004

    IL_0021:  ldnull
    IL_0022:  stloc.1
    IL_0023:  br.s       IL_0025

    IL_0025:  ldloc.1
    IL_0026:  ret
  } // end of method CamIL.Jacare::set_byte_array_region

  .method public hidebysig static class [mscorlib]System.Array 
          new_char_array(int32 size) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  1
    .locals init (class [mscorlib]System.Array V_0)
    IL_0000:  ldarg.0
    IL_0001:  newarr     [mscorlib]System.Char
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::new_char_array

  .method public hidebysig static int32  get_char_array_element(class [mscorlib]System.Array arr,
                                                                int32 i) cil managed
  {
    // Code size       13 (0xd)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  char[]
    IL_0006:  ldarg.1
    IL_0007:  ldelem.u2
    IL_0008:  stloc.0
    IL_0009:  br.s       IL_000b

    IL_000b:  ldloc.0
    IL_000c:  ret
  } // end of method CamIL.Jacare::get_char_array_element

  .method public hidebysig static object 
          set_char_array_element(class [mscorlib]System.Array arr,
                                 int32 i,
                                 int32 val) cil managed
  {
    // Code size       16 (0x10)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  char[]
    IL_0006:  ldarg.1
    IL_0007:  ldarg.2
    IL_0008:  conv.u2
    IL_0009:  stelem.i2
    IL_000a:  ldnull
    IL_000b:  stloc.0
    IL_000c:  br.s       IL_000e

    IL_000e:  ldloc.0
    IL_000f:  ret
  } // end of method CamIL.Jacare::set_char_array_element

  .method public hidebysig static class [mscorlib]System.Array 
          new_short_array(int32 size) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  1
    .locals init (class [mscorlib]System.Array V_0)
    IL_0000:  ldarg.0
    IL_0001:  newarr     [mscorlib]System.Int16
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::new_short_array

  .method public hidebysig static int32  get_short_array_element(class [mscorlib]System.Array arr,
                                                                 int32 i) cil managed
  {
    // Code size       13 (0xd)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  int16[]
    IL_0006:  ldarg.1
    IL_0007:  ldelem.i2
    IL_0008:  stloc.0
    IL_0009:  br.s       IL_000b

    IL_000b:  ldloc.0
    IL_000c:  ret
  } // end of method CamIL.Jacare::get_short_array_element

  .method public hidebysig static object 
          set_short_array_element(class [mscorlib]System.Array arr,
                                  int32 i,
                                  int32 val) cil managed
  {
    // Code size       16 (0x10)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  int16[]
    IL_0006:  ldarg.1
    IL_0007:  ldarg.2
    IL_0008:  conv.i2
    IL_0009:  stelem.i2
    IL_000a:  ldnull
    IL_000b:  stloc.0
    IL_000c:  br.s       IL_000e

    IL_000e:  ldloc.0
    IL_000f:  ret
  } // end of method CamIL.Jacare::set_short_array_element

  .method public hidebysig static class [mscorlib]System.Array 
          new_int_array(int32 size) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  1
    .locals init (class [mscorlib]System.Array V_0)
    IL_0000:  ldarg.0
    IL_0001:  newarr     [mscorlib]System.Int32
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::new_int_array

  .method public hidebysig static int32  get_camlint_array_element(class [mscorlib]System.Array arr,
                                                                   int32 i) cil managed
  {
    // Code size       13 (0xd)
    .maxstack  2
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  int32[]
    IL_0006:  ldarg.1
    IL_0007:  ldelem.i4
    IL_0008:  stloc.0
    IL_0009:  br.s       IL_000b

    IL_000b:  ldloc.0
    IL_000c:  ret
  } // end of method CamIL.Jacare::get_camlint_array_element

  .method public hidebysig static object 
          set_camlint_array_element(class [mscorlib]System.Array arr,
                                    int32 i,
                                    int32 val) cil managed
  {
    // Code size       15 (0xf)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  int32[]
    IL_0006:  ldarg.1
    IL_0007:  ldarg.2
    IL_0008:  stelem.i4
    IL_0009:  ldnull
    IL_000a:  stloc.0
    IL_000b:  br.s       IL_000d

    IL_000d:  ldloc.0
    IL_000e:  ret
  } // end of method CamIL.Jacare::set_camlint_array_element

  .method public hidebysig static class [mscorlib]System.Array 
          new_float_array(int32 size) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  1
    .locals init (class [mscorlib]System.Array V_0)
    IL_0000:  ldarg.0
    IL_0001:  newarr     [mscorlib]System.Single
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::new_float_array

  .method public hidebysig static float64 
          get_float_array_element(class [mscorlib]System.Array arr,
                                  int32 i) cil managed
  {
    // Code size       14 (0xe)
    .maxstack  2
    .locals init (float64 V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  float32[]
    IL_0006:  ldarg.1
    IL_0007:  ldelem.r4
    IL_0008:  conv.r8
    IL_0009:  stloc.0
    IL_000a:  br.s       IL_000c

    IL_000c:  ldloc.0
    IL_000d:  ret
  } // end of method CamIL.Jacare::get_float_array_element

  .method public hidebysig static object 
          set_float_array_element(class [mscorlib]System.Array arr,
                                  int32 i,
                                  float64 val) cil managed
  {
    // Code size       16 (0x10)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  float32[]
    IL_0006:  ldarg.1
    IL_0007:  ldarg.2
    IL_0008:  conv.r4
    IL_0009:  stelem.r4
    IL_000a:  ldnull
    IL_000b:  stloc.0
    IL_000c:  br.s       IL_000e

    IL_000e:  ldloc.0
    IL_000f:  ret
  } // end of method CamIL.Jacare::set_float_array_element

  .method public hidebysig static class [mscorlib]System.Array 
          new_double_array(int32 size) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  1
    .locals init (class [mscorlib]System.Array V_0)
    IL_0000:  ldarg.0
    IL_0001:  newarr     [mscorlib]System.Double
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::new_double_array

  .method public hidebysig static float64 
          get_double_array_element(class [mscorlib]System.Array arr,
                                   int32 i) cil managed
  {
    // Code size       13 (0xd)
    .maxstack  2
    .locals init (float64 V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  float64[]
    IL_0006:  ldarg.1
    IL_0007:  ldelem.r8
    IL_0008:  stloc.0
    IL_0009:  br.s       IL_000b

    IL_000b:  ldloc.0
    IL_000c:  ret
  } // end of method CamIL.Jacare::get_double_array_element

  .method public hidebysig static object 
          set_double_array_element(class [mscorlib]System.Array arr,
                                   int32 i,
                                   float64 val) cil managed
  {
    // Code size       15 (0xf)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  float64[]
    IL_0006:  ldarg.1
    IL_0007:  ldarg.2
    IL_0008:  stelem.r8
    IL_0009:  ldnull
    IL_000a:  stloc.0
    IL_000b:  br.s       IL_000d

    IL_000d:  ldloc.0
    IL_000e:  ret
  } // end of method CamIL.Jacare::set_double_array_element

  .method public hidebysig static class [mscorlib]System.Array 
          new_long_array(int32 size) cil managed
  {
    // Code size       11 (0xb)
    .maxstack  1
    .locals init (class [mscorlib]System.Array V_0)
    IL_0000:  ldarg.0
    IL_0001:  newarr     [mscorlib]System.Double
    IL_0006:  stloc.0
    IL_0007:  br.s       IL_0009

    IL_0009:  ldloc.0
    IL_000a:  ret
  } // end of method CamIL.Jacare::new_long_array

  .method public hidebysig static int64  get_long_array_element(class [mscorlib]System.Array arr,
                                                                int32 i) cil managed
  {
    // Code size       13 (0xd)
    .maxstack  2
    .locals init (int64 V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  int64[]
    IL_0006:  ldarg.1
    IL_0007:  ldelem.i8
    IL_0008:  stloc.0
    IL_0009:  br.s       IL_000b

    IL_000b:  ldloc.0
    IL_000c:  ret
  } // end of method CamIL.Jacare::get_long_array_element

  .method public hidebysig static object 
          set_double_array_element(class [mscorlib]System.Array arr,
                                   int32 i,
                                   int64 val) cil managed
  {
    // Code size       15 (0xf)
    .maxstack  3
    .locals init (object V_0)
    IL_0000:  ldarg.0
    IL_0001:  castclass  int64[]
    IL_0006:  ldarg.1
    IL_0007:  ldarg.2
    IL_0008:  stelem.i8
    IL_0009:  ldnull
    IL_000a:  stloc.0
    IL_000b:  br.s       IL_000d

    IL_000d:  ldloc.0
    IL_000e:  ret
  } // end of method CamIL.Jacare::set_double_array_element

  .method private hidebysig static void  generate_cast(object[] item,
                                                       class [mscorlib]System.Type argType,
                                                       class [mscorlib]System.Reflection.Emit.ILGenerator ilgen) cil managed
  {
    // Code size       375 (0x177)
    .maxstack  4
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldc.i4.1
    IL_0002:  ldelem.ref
    IL_0003:  unbox      [mscorlib]System.Int32
    IL_0008:  ldind.i4
    IL_0009:  stloc.0
    IL_000a:  ldloc.0
    IL_000b:  switch     ( 
                          IL_003d,
                          IL_0070,
                          IL_0095,
                          IL_00ba,
                          IL_00df,
                          IL_0100,
                          IL_0102,
                          IL_0123,
                          IL_0145,
                          IL_0166)
    IL_0038:  br         IL_0174

    IL_003d:  ldarg.2
    IL_003e:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Pop
    IL_0043:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_0048:  ldarg.0
    IL_0049:  ldc.i4.0
    IL_004a:  ldelem.ref
    IL_004b:  unbox      [mscorlib]System.Int32
    IL_0050:  ldind.i4
    IL_0051:  brtrue.s   IL_0060

    IL_0053:  ldarg.2
    IL_0054:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldc_I4_0
    IL_0059:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_005e:  br.s       IL_006b

    IL_0060:  ldarg.2
    IL_0061:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldc_I4_1
    IL_0066:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_006b:  br         IL_0176

    IL_0070:  ldarg.2
    IL_0071:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Pop
    IL_0076:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_007b:  ldarg.2
    IL_007c:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldc_I4
    IL_0081:  ldarg.0
    IL_0082:  ldc.i4.0
    IL_0083:  ldelem.ref
    IL_0084:  unbox      [mscorlib]System.Int32
    IL_0089:  ldind.i4
    IL_008a:  conv.u1
    IL_008b:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          unsigned int8)
    IL_0090:  br         IL_0176

    IL_0095:  ldarg.2
    IL_0096:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Pop
    IL_009b:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_00a0:  ldarg.2
    IL_00a1:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldc_I4
    IL_00a6:  ldarg.0
    IL_00a7:  ldc.i4.0
    IL_00a8:  ldelem.ref
    IL_00a9:  unbox      [mscorlib]System.Int32
    IL_00ae:  ldind.i4
    IL_00af:  conv.u2
    IL_00b0:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          int32)
    IL_00b5:  br         IL_0176

    IL_00ba:  ldarg.2
    IL_00bb:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Pop
    IL_00c0:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_00c5:  ldarg.2
    IL_00c6:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldc_I4
    IL_00cb:  ldarg.0
    IL_00cc:  ldc.i4.0
    IL_00cd:  ldelem.ref
    IL_00ce:  unbox      [mscorlib]System.Int32
    IL_00d3:  ldind.i4
    IL_00d4:  conv.i2
    IL_00d5:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          int16)
    IL_00da:  br         IL_0176

    IL_00df:  ldarg.2
    IL_00e0:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Pop
    IL_00e5:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_00ea:  ldarg.2
    IL_00eb:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldc_I4
    IL_00f0:  ldarg.0
    IL_00f1:  ldc.i4.0
    IL_00f2:  ldelem.ref
    IL_00f3:  unbox      [mscorlib]System.Int32
    IL_00f8:  ldind.i4
    IL_00f9:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          int32)
    IL_00fe:  br.s       IL_0176

    IL_0100:  br.s       IL_0176

    IL_0102:  ldarg.2
    IL_0103:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Pop
    IL_0108:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_010d:  ldarg.2
    IL_010e:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldc_I8
    IL_0113:  ldarg.0
    IL_0114:  ldc.i4.0
    IL_0115:  ldelem.ref
    IL_0116:  unbox      [mscorlib]System.Int32
    IL_011b:  ldind.i4
    IL_011c:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          int32)
    IL_0121:  br.s       IL_0176

    IL_0123:  ldarg.2
    IL_0124:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Pop
    IL_0129:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_012e:  ldarg.2
    IL_012f:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldc_R8
    IL_0134:  ldarg.0
    IL_0135:  ldc.i4.0
    IL_0136:  ldelem.ref
    IL_0137:  unbox      [mscorlib]System.Double
    IL_013c:  ldind.r8
    IL_013d:  conv.r4
    IL_013e:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          float32)
    IL_0143:  br.s       IL_0176

    IL_0145:  ldarg.2
    IL_0146:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Pop
    IL_014b:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_0150:  ldarg.2
    IL_0151:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldc_R8
    IL_0156:  ldarg.0
    IL_0157:  ldc.i4.0
    IL_0158:  ldelem.ref
    IL_0159:  unbox      [mscorlib]System.Double
    IL_015e:  ldind.r8
    IL_015f:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          float64)
    IL_0164:  br.s       IL_0176

    IL_0166:  ldarg.2
    IL_0167:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Castclass
    IL_016c:  ldarg.1
    IL_016d:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          class [mscorlib]System.Type)
    IL_0172:  br.s       IL_0176

    IL_0174:  br.s       IL_0176

    IL_0176:  ret
  } // end of method CamIL.Jacare::generate_cast

  .method public hidebysig static object 
          call_nonvirtual_object_method(object ob,
                                        class [mscorlib]System.Reflection.MethodInfo mi,
                                        object[] vargs) cil managed
  {
    // Code size       393 (0x189)
    .maxstack  6
    .locals init (object[] V_0,
             int32 V_1,
             class [mscorlib]System.Reflection.ParameterInfo[] V_2,
             class [mscorlib]System.AppDomain V_3,
             class [mscorlib]System.Reflection.AssemblyName V_4,
             class [mscorlib]System.Reflection.Emit.AssemblyBuilder V_5,
             class [mscorlib]System.Reflection.Emit.ModuleBuilder V_6,
             class [mscorlib]System.Reflection.Emit.TypeBuilder V_7,
             class [mscorlib]System.Type V_8,
             class [mscorlib]System.Type[] V_9,
             int32 V_10,
             class [mscorlib]System.Reflection.Emit.MethodBuilder V_11,
             class [mscorlib]System.Reflection.Emit.ILGenerator V_12,
             int32 V_13,
             class [mscorlib]System.Type V_14,
             object[] V_15,
             int32 V_16,
             object V_17)
    IL_0000:  ldarg.2
    IL_0001:  ldlen
    IL_0002:  conv.i4
    IL_0003:  ldc.i4.1
    IL_0004:  sub
    IL_0005:  newarr     [mscorlib]System.Object
    IL_000a:  stloc.0
    IL_000b:  ldc.i4.0
    IL_000c:  stloc.1
    IL_000d:  br.s       IL_0023

    IL_000f:  ldloc.0
    IL_0010:  ldloc.1
    IL_0011:  ldarg.2
    IL_0012:  ldloc.1
    IL_0013:  ldelem.ref
    IL_0014:  castclass  CamIL.Variant
    IL_0019:  call       object CamIL.Jacare::convert_sumtype(class CamIL.Variant)
    IL_001e:  stelem.ref
    IL_001f:  ldloc.1
    IL_0020:  ldc.i4.1
    IL_0021:  add
    IL_0022:  stloc.1
    IL_0023:  ldloc.1
    IL_0024:  ldloc.0
    IL_0025:  ldlen
    IL_0026:  conv.i4
    IL_0027:  blt.s      IL_000f

    IL_0029:  ldarg.1
    IL_002a:  callvirt   instance class [mscorlib]System.Reflection.ParameterInfo[] [mscorlib]System.Reflection.MethodBase::GetParameters()
    IL_002f:  stloc.2
    IL_0030:  call       class [mscorlib]System.AppDomain [mscorlib]System.Threading.Thread::GetDomain()
    IL_0035:  stloc.3
    IL_0036:  newobj     instance void [mscorlib]System.Reflection.AssemblyName::.ctor()
    IL_003b:  stloc.s    V_4
    IL_003d:  ldloc.s    V_4
    IL_003f:  ldstr      "dynjacareAssembly"
    IL_0044:  callvirt   instance void [mscorlib]System.Reflection.AssemblyName::set_Name(string)
    IL_0049:  ldloc.3
    IL_004a:  ldloc.s    V_4
    IL_004c:  ldc.i4.1
    IL_004d:  callvirt   instance class [mscorlib]System.Reflection.Emit.AssemblyBuilder [mscorlib]System.AppDomain::DefineDynamicAssembly(class [mscorlib]System.Reflection.AssemblyName,
                                                                                                                                           valuetype [mscorlib]System.Reflection.Emit.AssemblyBuilderAccess)
    IL_0052:  stloc.s    V_5
    IL_0054:  ldloc.s    V_5
    IL_0056:  ldstr      "TheDynModule!!!"
    IL_005b:  callvirt   instance class [mscorlib]System.Reflection.Emit.ModuleBuilder [mscorlib]System.Reflection.Emit.AssemblyBuilder::DefineDynamicModule(string)
    IL_0060:  stloc.s    V_6
    IL_0062:  ldloc.s    V_6
    IL_0064:  ldstr      "dynjacareClass"
    IL_0069:  callvirt   instance class [mscorlib]System.Reflection.Emit.TypeBuilder [mscorlib]System.Reflection.Emit.ModuleBuilder::DefineType(string)
    IL_006e:  stloc.s    V_7
    IL_0070:  ldstr      "System.Object"
    IL_0075:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_007a:  stloc.s    V_8
    IL_007c:  ldloc.0
    IL_007d:  ldlen
    IL_007e:  conv.i4
    IL_007f:  ldc.i4.1
    IL_0080:  add
    IL_0081:  newarr     [mscorlib]System.Type
    IL_0086:  stloc.s    V_9
    IL_0088:  ldc.i4.0
    IL_0089:  stloc.s    V_10
    IL_008b:  br.s       IL_009a

    IL_008d:  ldloc.s    V_9
    IL_008f:  ldloc.s    V_10
    IL_0091:  ldloc.s    V_8
    IL_0093:  stelem.ref
    IL_0094:  ldloc.s    V_10
    IL_0096:  ldc.i4.1
    IL_0097:  add
    IL_0098:  stloc.s    V_10
    IL_009a:  ldloc.s    V_10
    IL_009c:  ldloc.s    V_9
    IL_009e:  ldlen
    IL_009f:  conv.i4
    IL_00a0:  blt.s      IL_008d

    IL_00a2:  ldloc.s    V_7
    IL_00a4:  ldstr      "CallMe"
    IL_00a9:  ldc.i4.s   22
    IL_00ab:  ldarg.1
    IL_00ac:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Reflection.MethodInfo::get_ReturnType()
    IL_00b1:  ldloc.s    V_9
    IL_00b3:  callvirt   instance class [mscorlib]System.Reflection.Emit.MethodBuilder [mscorlib]System.Reflection.Emit.TypeBuilder::DefineMethod(string,
                                                                                                                                                  valuetype [mscorlib]System.Reflection.MethodAttributes,
                                                                                                                                                  class [mscorlib]System.Type,
                                                                                                                                                  class [mscorlib]System.Type[])
    IL_00b8:  stloc.s    V_11
    IL_00ba:  ldloc.s    V_11
    IL_00bc:  callvirt   instance class [mscorlib]System.Reflection.Emit.ILGenerator [mscorlib]System.Reflection.Emit.MethodBuilder::GetILGenerator()
    IL_00c1:  stloc.s    V_12
    IL_00c3:  ldloc.s    V_12
    IL_00c5:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldarg_0
    IL_00ca:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_00cf:  ldloc.s    V_12
    IL_00d1:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Castclass
    IL_00d6:  ldarg.1
    IL_00d7:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Reflection.MemberInfo::get_DeclaringType()
    IL_00dc:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          class [mscorlib]System.Type)
    IL_00e1:  ldc.i4.0
    IL_00e2:  stloc.s    V_13
    IL_00e4:  br.s       IL_0115

    IL_00e6:  ldloc.s    V_12
    IL_00e8:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldarg
    IL_00ed:  ldloc.s    V_13
    IL_00ef:  ldc.i4.1
    IL_00f0:  add
    IL_00f1:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          int32)
    IL_00f6:  ldarg.2
    IL_00f7:  ldloc.s    V_13
    IL_00f9:  ldelem.ref
    IL_00fa:  castclass  object[]
    IL_00ff:  ldloc.2
    IL_0100:  ldloc.s    V_13
    IL_0102:  ldelem.ref
    IL_0103:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Reflection.ParameterInfo::get_ParameterType()
    IL_0108:  ldloc.s    V_12
    IL_010a:  call       void CamIL.Jacare::generate_cast(object[],
                                                    class [mscorlib]System.Type,
                                                    class [mscorlib]System.Reflection.Emit.ILGenerator)
    IL_010f:  ldloc.s    V_13
    IL_0111:  ldc.i4.1
    IL_0112:  add
    IL_0113:  stloc.s    V_13
    IL_0115:  ldloc.s    V_13
    IL_0117:  ldloc.0
    IL_0118:  ldlen
    IL_0119:  conv.i4
    IL_011a:  blt.s      IL_00e6

    IL_011c:  ldloc.s    V_12
    IL_011e:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Call
    IL_0123:  ldarg.1
    IL_0124:  ldnull
    IL_0125:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::EmitCall(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                              class [mscorlib]System.Reflection.MethodInfo,
                                                                                              class [mscorlib]System.Type[])
    IL_012a:  ldloc.s    V_12
    IL_012c:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ret
    IL_0131:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_0136:  ldloc.s    V_7
    IL_0138:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Reflection.Emit.TypeBuilder::CreateType()
    IL_013d:  stloc.s    V_14
    IL_013f:  ldloc.0
    IL_0140:  ldlen
    IL_0141:  conv.i4
    IL_0142:  ldc.i4.1
    IL_0143:  add
    IL_0144:  newarr     [mscorlib]System.Object
    IL_0149:  stloc.s    V_15
    IL_014b:  ldloc.s    V_15
    IL_014d:  ldc.i4.0
    IL_014e:  ldarg.0
    IL_014f:  stelem.ref
    IL_0150:  ldc.i4.0
    IL_0151:  stloc.s    V_16
    IL_0153:  br.s       IL_0166

    IL_0155:  ldloc.s    V_15
    IL_0157:  ldloc.s    V_16
    IL_0159:  ldc.i4.1
    IL_015a:  add
    IL_015b:  ldloc.0
    IL_015c:  ldloc.s    V_16
    IL_015e:  ldelem.ref
    IL_015f:  stelem.ref
    IL_0160:  ldloc.s    V_16
    IL_0162:  ldc.i4.1
    IL_0163:  add
    IL_0164:  stloc.s    V_16
    IL_0166:  ldloc.s    V_16
    IL_0168:  ldloc.0
    IL_0169:  ldlen
    IL_016a:  conv.i4
    IL_016b:  blt.s      IL_0155

    IL_016d:  ldloc.s    V_14
    IL_016f:  ldstr      "CallMe"
    IL_0174:  ldc.i4     0x100
    IL_0179:  ldnull
    IL_017a:  ldnull
    IL_017b:  ldloc.s    V_15
    IL_017d:  callvirt   instance object [mscorlib]System.Type::InvokeMember(string,
                                                                             valuetype [mscorlib]System.Reflection.BindingFlags,
                                                                             class [mscorlib]System.Reflection.Binder,
                                                                             object,
                                                                             object[])
    IL_0182:  stloc.s    V_17
    IL_0184:  br.s       IL_0186

    IL_0186:  ldloc.s    V_17
    IL_0188:  ret
  } // end of method CamIL.Jacare::call_nonvirtual_object_method

  .method public hidebysig static int32  call_nonvirtual_camlint_method(object ob,
                                                                        class [mscorlib]System.Reflection.MethodInfo mi,
                                                                        object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_nonvirtual_object_method(object,
                                                                      class [mscorlib]System.Reflection.MethodInfo,
                                                                      object[])
    IL_0008:  unbox      [mscorlib]System.Int32
    IL_000d:  ldind.i4
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_nonvirtual_camlint_method

  .method public hidebysig static int32  call_nonvirtual_boolean_method(object ob,
                                                                        class [mscorlib]System.Reflection.MethodInfo mi,
                                                                        object[] vargs) cil managed
  {
    // Code size       26 (0x1a)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_nonvirtual_object_method(object,
                                                                      class [mscorlib]System.Reflection.MethodInfo,
                                                                      object[])
    IL_0008:  unbox      [mscorlib]System.Boolean
    IL_000d:  ldind.i1
    IL_000e:  brfalse.s  IL_0014

    IL_0010:  ldc.i4.1
    IL_0011:  stloc.0
    IL_0012:  br.s       IL_0018

    IL_0014:  ldc.i4.0
    IL_0015:  stloc.0
    IL_0016:  br.s       IL_0018

    IL_0018:  ldloc.0
    IL_0019:  ret
  } // end of method CamIL.Jacare::call_nonvirtual_boolean_method

  .method public hidebysig static int32  call_nonvirtual_char_method(object ob,
                                                                     class [mscorlib]System.Reflection.MethodInfo mi,
                                                                     object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_nonvirtual_object_method(object,
                                                                      class [mscorlib]System.Reflection.MethodInfo,
                                                                      object[])
    IL_0008:  unbox      [mscorlib]System.Char
    IL_000d:  ldind.u2
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_nonvirtual_char_method

  .method public hidebysig static int32  call_nonvirtual_byte_method(object ob,
                                                                     class [mscorlib]System.Reflection.MethodInfo mi,
                                                                     object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_nonvirtual_object_method(object,
                                                                      class [mscorlib]System.Reflection.MethodInfo,
                                                                      object[])
    IL_0008:  unbox      [mscorlib]System.Byte
    IL_000d:  ldind.u1
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_nonvirtual_byte_method

  .method public hidebysig static int32  call_nonvirtual_short_method(object ob,
                                                                      class [mscorlib]System.Reflection.MethodInfo mi,
                                                                      object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_nonvirtual_object_method(object,
                                                                      class [mscorlib]System.Reflection.MethodInfo,
                                                                      object[])
    IL_0008:  unbox      [mscorlib]System.Int16
    IL_000d:  ldind.i2
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_nonvirtual_short_method

  .method public hidebysig static float64 
          call_nonvirtual_double_method(object ob,
                                        class [mscorlib]System.Reflection.MethodInfo mi,
                                        object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (float64 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_nonvirtual_object_method(object,
                                                                      class [mscorlib]System.Reflection.MethodInfo,
                                                                      object[])
    IL_0008:  unbox      [mscorlib]System.Double
    IL_000d:  ldind.r8
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_nonvirtual_double_method

  .method public hidebysig static int64  call_nonvirtual_long_method(object ob,
                                                                     class [mscorlib]System.Reflection.MethodInfo mi,
                                                                     object[] vargs) cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int64 V_0)
    IL_0000:  ldarg.0
    IL_0001:  ldarg.1
    IL_0002:  ldarg.2
    IL_0003:  call       object CamIL.Jacare::call_nonvirtual_object_method(object,
                                                                      class [mscorlib]System.Reflection.MethodInfo,
                                                                      object[])
    IL_0008:  unbox      [mscorlib]System.Int64
    IL_000d:  ldind.i8
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method CamIL.Jacare::call_nonvirtual_long_method

  .method public hidebysig static object 
          box_valuetype(class [mscorlib]System.Type vtype,
                        int32 val) cil managed
  {
    // Code size       221 (0xdd)
    .maxstack  6
    .locals init (class [mscorlib]System.AppDomain V_0,
             class [mscorlib]System.Reflection.AssemblyName V_1,
             class [mscorlib]System.Reflection.Emit.AssemblyBuilder V_2,
             class [mscorlib]System.Reflection.Emit.ModuleBuilder V_3,
             class [mscorlib]System.Reflection.Emit.TypeBuilder V_4,
             class [mscorlib]System.Type V_5,
             class [mscorlib]System.Type[] V_6,
             class [mscorlib]System.Reflection.Emit.MethodBuilder V_7,
             class [mscorlib]System.Reflection.Emit.ILGenerator V_8,
             class [mscorlib]System.Type V_9,
             object[] V_10,
             object V_11,
             class [mscorlib]System.Type[] V_12,
             object[] V_13)
    IL_0000:  call       class [mscorlib]System.AppDomain [mscorlib]System.Threading.Thread::GetDomain()
    IL_0005:  stloc.0
    IL_0006:  newobj     instance void [mscorlib]System.Reflection.AssemblyName::.ctor()
    IL_000b:  stloc.1
    IL_000c:  ldloc.1
    IL_000d:  ldstr      "dynjacareAssembly"
    IL_0012:  callvirt   instance void [mscorlib]System.Reflection.AssemblyName::set_Name(string)
    IL_0017:  ldloc.0
    IL_0018:  ldloc.1
    IL_0019:  ldc.i4.1
    IL_001a:  callvirt   instance class [mscorlib]System.Reflection.Emit.AssemblyBuilder [mscorlib]System.AppDomain::DefineDynamicAssembly(class [mscorlib]System.Reflection.AssemblyName,
                                                                                                                                           valuetype [mscorlib]System.Reflection.Emit.AssemblyBuilderAccess)
    IL_001f:  stloc.2
    IL_0020:  ldloc.2
    IL_0021:  ldstr      "TheDynModule"
    IL_0026:  ldc.i4.0
    IL_0027:  callvirt   instance class [mscorlib]System.Reflection.Emit.ModuleBuilder [mscorlib]System.Reflection.Emit.AssemblyBuilder::DefineDynamicModule(string,
                                                                                                                                                             bool)
    IL_002c:  stloc.3
    IL_002d:  ldloc.3
    IL_002e:  ldstr      "dynjacareClass"
    IL_0033:  callvirt   instance class [mscorlib]System.Reflection.Emit.TypeBuilder [mscorlib]System.Reflection.Emit.ModuleBuilder::DefineType(string)
    IL_0038:  stloc.s    V_4
    IL_003a:  ldstr      "System.Object"
    IL_003f:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_0044:  stloc.s    V_5
    IL_0046:  ldc.i4.1
    IL_0047:  newarr     [mscorlib]System.Type
    IL_004c:  stloc.s    V_12
    IL_004e:  ldloc.s    V_12
    IL_0050:  ldc.i4.0
    IL_0051:  ldstr      "System.Int32"
    IL_0056:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetType(string)
    IL_005b:  stelem.ref
    IL_005c:  ldloc.s    V_12
    IL_005e:  stloc.s    V_6
    IL_0060:  ldloc.s    V_4
    IL_0062:  ldstr      "CallMe"
    IL_0067:  ldc.i4.s   22
    IL_0069:  ldloc.s    V_5
    IL_006b:  ldloc.s    V_6
    IL_006d:  callvirt   instance class [mscorlib]System.Reflection.Emit.MethodBuilder [mscorlib]System.Reflection.Emit.TypeBuilder::DefineMethod(string,
                                                                                                                                                  valuetype [mscorlib]System.Reflection.MethodAttributes,
                                                                                                                                                  class [mscorlib]System.Type,
                                                                                                                                                  class [mscorlib]System.Type[])
    IL_0072:  stloc.s    V_7
    IL_0074:  ldloc.s    V_7
    IL_0076:  callvirt   instance class [mscorlib]System.Reflection.Emit.ILGenerator [mscorlib]System.Reflection.Emit.MethodBuilder::GetILGenerator()
    IL_007b:  stloc.s    V_8
    IL_007d:  ldloc.s    V_8
    IL_007f:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ldarg_0
    IL_0084:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_0089:  ldloc.s    V_8
    IL_008b:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Box
    IL_0090:  ldarg.0
    IL_0091:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode,
                                                                                          class [mscorlib]System.Type)
    IL_0096:  ldloc.s    V_8
    IL_0098:  ldsfld     valuetype [mscorlib]System.Reflection.Emit.OpCode [mscorlib]System.Reflection.Emit.OpCodes::Ret
    IL_009d:  callvirt   instance void [mscorlib]System.Reflection.Emit.ILGenerator::Emit(valuetype [mscorlib]System.Reflection.Emit.OpCode)
    IL_00a2:  ldloc.s    V_4
    IL_00a4:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Reflection.Emit.TypeBuilder::CreateType()
    IL_00a9:  stloc.s    V_9
    IL_00ab:  ldc.i4.1
    IL_00ac:  newarr     [mscorlib]System.Object
    IL_00b1:  stloc.s    V_13
    IL_00b3:  ldloc.s    V_13
    IL_00b5:  ldc.i4.0
    IL_00b6:  ldarg.1
    IL_00b7:  box        [mscorlib]System.Int32
    IL_00bc:  stelem.ref
    IL_00bd:  ldloc.s    V_13
    IL_00bf:  stloc.s    V_10
    IL_00c1:  ldloc.s    V_9
    IL_00c3:  ldstr      "CallMe"
    IL_00c8:  ldc.i4     0x100
    IL_00cd:  ldnull
    IL_00ce:  ldnull
    IL_00cf:  ldloc.s    V_10
    IL_00d1:  callvirt   instance object [mscorlib]System.Type::InvokeMember(string,
                                                                             valuetype [mscorlib]System.Reflection.BindingFlags,
                                                                             class [mscorlib]System.Reflection.Binder,
                                                                             object,
                                                                             object[])
    IL_00d6:  stloc.s    V_11
    IL_00d8:  br.s       IL_00da

    IL_00da:  ldloc.s    V_11
    IL_00dc:  ret
  } // end of method CamIL.Jacare::box_valuetype

  .method public hidebysig specialname rtspecialname 
          instance void  .ctor() cil managed
  {
    // Code size       7 (0x7)
    .maxstack  1
    IL_0000:  ldarg.0
    IL_0001:  call       instance void [mscorlib]System.Object::.ctor()
    IL_0006:  ret
  } // end of method CamIL.Jacare::.ctor

} // end of class CamIL.Jacare


// =============================================================

//*********** DISASSEMBLY COMPLETE ***********************
