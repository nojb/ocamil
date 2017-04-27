  .class public auto ansi beforefieldinit PAInfo
         extends [mscorlib]System.Object
  {


    .method public hidebysig static class [mscorlib]System.Reflection.Assembly
            CoreCamILAssembly() cil managed
    {
      .maxstack 1
	call class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::GetExecutingAssembly()
	ret
    }

 .method public hidebysig static string 
            assemblyPath(class [mscorlib]System.Reflection.Assembly) cil managed
    {
      .maxstack 1
	ldarg.0
	callvirt instance string [mscorlib]System.Reflection.Assembly::get_EscapedCodeBase()
	ret
    }
	
		
    .method public hidebysig static string 
            getPublicKey(class [mscorlib]System.Reflection.Assembly a) cil managed
    {
      // Code size       75 (0x4b)
      .maxstack  3
      .locals init (class [mscorlib]System.Reflection.AssemblyName V_0,
               unsigned int8[] V_1,
               string V_2,
               unsigned int8 V_3,
               string V_4,
               unsigned int8[] V_5,
               int32 V_6)
    IL_0000:  ldarg.0
    IL_0001:  callvirt   instance class [mscorlib]System.Reflection.AssemblyName [mscorlib]System.Reflection.Assembly::GetName()
    IL_0006:  stloc.0
    IL_0007:  ldloc.0
    IL_0008:  callvirt   instance unsigned int8[] [mscorlib]System.Reflection.AssemblyName::GetPublicKeyToken()
    IL_000d:  stloc.1
    IL_000e:  ldstr      ""
    IL_0013:  stloc.2
    IL_0014:  ldloc.1
    IL_0015:  brtrue.s   IL_0020

    IL_0017:  ldstr      ""
    IL_001c:  stloc.s    V_4
    IL_001e:  br.s       IL_0054

    IL_0020:  ldloc.1
    IL_0021:  stloc.s    V_5
    IL_0023:  ldc.i4.0
    IL_0024:  stloc.s    V_6
    IL_0026:  br.s       IL_0047

    IL_0028:  ldloc.s    V_5
    IL_002a:  ldloc.s    V_6
    IL_002c:  ldelem.u1
    IL_002d:  stloc.3
    IL_002e:  ldloc.2
    IL_002f:  ldloca.s   V_3
    IL_0031:  ldstr      "x2"
    IL_0036:  call       instance string [mscorlib]System.Byte::ToString(string)
    IL_003b:  call       string [mscorlib]System.String::Concat(string,
                                                                string)
    IL_0040:  stloc.2
    IL_0041:  ldloc.s    V_6
    IL_0043:  ldc.i4.1
    IL_0044:  add
    IL_0045:  stloc.s    V_6
    IL_0047:  ldloc.s    V_6
    IL_0049:  ldloc.s    V_5
    IL_004b:  ldlen
    IL_004c:  conv.i4
    IL_004d:  blt.s      IL_0028

    IL_004f:  ldloc.2
    IL_0050:  stloc.s    V_4
    IL_0052:  br.s       IL_0054

    IL_0054:  ldloc.s    V_4
    IL_0056:  ret
    } // end of method PAInfo::getPublicKey

    .method private hidebysig static void 
            printPublicKey(class [mscorlib]System.Reflection.Assembly a) cil managed
    {
      // Code size       12 (0xc)
      .maxstack  1
      IL_0000:  ldarg.0
      IL_0001:  call       string CamIL.PAInfo::getPublicKey(class [mscorlib]System.Reflection.Assembly)
      IL_0006:  call       void [mscorlib]System.Console::WriteLine(string)
      IL_000b:  ret
    } // end of method PAInfo::printPublicKey

    .method public hidebysig static string 
            getVersion(class [mscorlib]System.Reflection.Assembly a) cil managed
    {
      // Code size       42 (0x2a)
      .maxstack  3
      .locals init (class [mscorlib]System.Reflection.AssemblyName V_0,
               string V_1,
               string V_2)
      IL_0000:  ldarg.0
      IL_0001:  callvirt   instance class [mscorlib]System.Reflection.AssemblyName [mscorlib]System.Reflection.Assembly::GetName()
      IL_0006:  stloc.0
      IL_0007:  ldloc.0
      IL_0008:  callvirt   instance class [mscorlib]System.Version [mscorlib]System.Reflection.AssemblyName::get_Version()
      IL_000d:  callvirt   instance string [mscorlib]System.Version::ToString()
      IL_0012:  stloc.1
      IL_0013:  ldloc.1
      IL_0014:  ldstr      "."
      IL_0019:  ldstr      ":"
      IL_001e:  callvirt   instance string [mscorlib]System.String::Replace(string,
                                                                            string)
      IL_0023:  stloc.1
      IL_0024:  ldloc.1
      IL_0025:  stloc.2
      IL_0026:  br.s       IL_0028

      IL_0028:  ldloc.2
      IL_0029:  ret
    } // end of method PAInfo::getVersion

    .method private hidebysig static void 
            printVersion(class [mscorlib]System.Reflection.Assembly a) cil managed
    {
      // Code size       12 (0xc)
      .maxstack  1
      IL_0000:  ldarg.0
      IL_0001:  call       string CamIL.PAInfo::getVersion(class [mscorlib]System.Reflection.Assembly)
      IL_0006:  call       void [mscorlib]System.Console::WriteLine(string)
      IL_000b:  ret
    } // end of method PAInfo::printVersion

    .method public hidebysig static class [mscorlib]System.Reflection.Assembly 
            findAssembly(string argv0) cil managed
    {
      // Code size       87 (0x57)
      .maxstack  3
      .locals init (class [mscorlib]System.Reflection.Assembly V_0,
               class [System]System.Text.RegularExpressions.Regex V_1,
               class [System]System.Text.RegularExpressions.Match V_2,
               string V_3,
               class [mscorlib]System.Reflection.Assembly V_4)
      .try
      {
        IL_0000:  ldstr      "(.*)mscorlib.dll"
        IL_0005:  ldc.i4.1
        IL_0006:  newobj     instance void [System]System.Text.RegularExpressions.Regex::.ctor(string,
                                                                                               valuetype [System]System.Text.RegularExpressions.RegexOptions)
        IL_000b:  stloc.1
        IL_000c:  ldloc.1
        IL_000d:  ldstr      "mscorlib"
        IL_0012:  call       class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::Load(string)
        IL_0017:  callvirt   instance string [mscorlib]System.Reflection.Assembly::get_CodeBase()
        IL_001c:  callvirt   instance class [System]System.Text.RegularExpressions.Match [System]System.Text.RegularExpressions.Regex::Match(string)
        IL_0021:  stloc.2
        IL_0022:  ldloc.2
        IL_0023:  ldstr      "$1"
        IL_0028:  callvirt   instance string [System]System.Text.RegularExpressions.Match::Result(string)
        IL_002d:  stloc.3
        .try
        {
          IL_002e:  ldarg.0
          IL_002f:  call       class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::LoadFrom(string)
          IL_0034:  stloc.0
          IL_0035:  leave.s    IL_0047

        }  // end .try
        catch [mscorlib]System.IO.FileNotFoundException 
        {
          IL_0037:  pop
          IL_0038:  ldloc.3
          IL_0039:  ldarg.0
          IL_003a:  call       string [mscorlib]System.String::Concat(string,
                                                                      string)
          IL_003f:  call       class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::LoadFrom(string)
          IL_0044:  stloc.0
          IL_0045:  leave.s    IL_0047

        }  // end handler
        IL_0047:  leave.s    IL_004f

      }  // end .try
      catch [mscorlib]System.Exception 
      {
        IL_0049:  pop
        IL_004a:  ldnull
        IL_004b:  stloc.s    V_4
        IL_004d:  leave.s    IL_0054

      }  // end handler
      IL_004f:  ldloc.0
      IL_0050:  stloc.s    V_4
      IL_0052:  br.s       IL_0054

      IL_0054:  ldloc.s    V_4
      IL_0056:  ret
    } // end of method PAInfo::findAssembly

    .method public hidebysig static int32 
            isFound(class [mscorlib]System.Reflection.Assembly a) cil managed
    {
      // Code size       13 (0xd)
      .maxstack  1
      .locals init (int32 V_0)
      IL_0000:  ldarg.0
      IL_0001:  brtrue.s   IL_0007

      IL_0003:  ldc.i4.0
      IL_0004:  stloc.0
      IL_0005:  br.s       IL_000b

      IL_0007:  ldc.i4.1
      IL_0008:  stloc.0
      IL_0009:  br.s       IL_000b

      IL_000b:  ldloc.0
      IL_000c:  ret
    } // end of method PAInfo::isFound

    .method private hidebysig static void 
            Usage() cil managed
    {
      // Code size       11 (0xb)
      .maxstack  1
      IL_0000:  ldstr      "Usage : CamIL.PAInfo <library full path> <k|v|a>"
      IL_0005:  call       void [mscorlib]System.Console::WriteLine(string)
      IL_000a:  ret
    } // end of method PAInfo::Usage

    .method public hidebysig static int32 
            Main(string[] argv) cil managed
    {
      // Code size       148 (0x94)
      .maxstack  3
      .locals init (bool V_0,
               bool V_1,
               class [mscorlib]System.Reflection.Assembly V_2,
               int32 V_3)
      IL_0000:  ldarg.0
      IL_0001:  ldlen
      IL_0002:  conv.i4
      IL_0003:  ldc.i4.2
      IL_0004:  beq.s      IL_0012

      IL_0006:  call       void CamIL.PAInfo::Usage()
      IL_000b:  ldc.i4.1
      IL_000c:  stloc.3
      IL_000d:  br         IL_0092

      IL_0012:  ldc.i4.0
      IL_0013:  dup
      IL_0014:  stloc.1
      IL_0015:  stloc.0
      IL_0016:  ldarg.0
      IL_0017:  ldc.i4.1
      IL_0018:  ldelem.ref
      IL_0019:  ldstr      "v"
      IL_001e:  call       bool [mscorlib]System.String::op_Equality(string,
                                                                     string)
      IL_0023:  brfalse.s  IL_0029

      IL_0025:  ldc.i4.1
      IL_0026:  stloc.0
      IL_0027:  br.s       IL_005a

      IL_0029:  ldarg.0
      IL_002a:  ldc.i4.1
      IL_002b:  ldelem.ref
      IL_002c:  ldstr      "k"
      IL_0031:  call       bool [mscorlib]System.String::op_Equality(string,
                                                                     string)
      IL_0036:  brfalse.s  IL_003c

      IL_0038:  ldc.i4.1
      IL_0039:  stloc.1
      IL_003a:  br.s       IL_005a

      IL_003c:  ldarg.0
      IL_003d:  ldc.i4.1
      IL_003e:  ldelem.ref
      IL_003f:  ldstr      "a"
      IL_0044:  call       bool [mscorlib]System.String::op_Equality(string,
                                                                     string)
      IL_0049:  brfalse.s  IL_0051

      IL_004b:  ldc.i4.1
      IL_004c:  stloc.0
      IL_004d:  ldc.i4.1
      IL_004e:  stloc.1
      IL_004f:  br.s       IL_005a

      IL_0051:  call       void CamIL.PAInfo::Usage()
      IL_0056:  ldc.i4.1
      IL_0057:  stloc.3
      IL_0058:  br.s       IL_0092

      IL_005a:  ldarg.0
      IL_005b:  ldc.i4.0
      IL_005c:  ldelem.ref
      IL_005d:  call       class [mscorlib]System.Reflection.Assembly CamIL.PAInfo::findAssembly(string)
      IL_0062:  stloc.2
      IL_0063:  ldloc.2
      IL_0064:  brtrue.s   IL_007c

      IL_0066:  ldstr      "Unable to find "
      IL_006b:  ldarg.0
      IL_006c:  ldc.i4.0
      IL_006d:  ldelem.ref
      IL_006e:  call       string [mscorlib]System.String::Concat(string,
                                                                  string)
      IL_0073:  call       void [mscorlib]System.Console::WriteLine(string)
      IL_0078:  ldc.i4.1
      IL_0079:  stloc.3
      IL_007a:  br.s       IL_0092

      IL_007c:  ldloc.1
      IL_007d:  brfalse.s  IL_0085

      IL_007f:  ldloc.2
      IL_0080:  call       void CamIL.PAInfo::printPublicKey(class [mscorlib]System.Reflection.Assembly)
      IL_0085:  ldloc.0
      IL_0086:  brfalse.s  IL_008e

      IL_0088:  ldloc.2
      IL_0089:  call       void CamIL.PAInfo::printVersion(class [mscorlib]System.Reflection.Assembly)
      IL_008e:  ldc.i4.0
      IL_008f:  stloc.3
      IL_0090:  br.s       IL_0092

      IL_0092:  ldloc.3
      IL_0093:  ret
    } // end of method PAInfo::Main

    .method public hidebysig specialname rtspecialname 
            instance void  .ctor() cil managed
    {
      // Code size       7 (0x7)
      .maxstack  1
      IL_0000:  ldarg.0
      IL_0001:  call       instance void [mscorlib]System.Object::.ctor()
      IL_0006:  ret
    } // end of method PAInfo::.ctor

  } // end of class PAInfo
