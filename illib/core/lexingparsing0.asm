.class public auto ansi beforefieldinit LexingParsing
       extends [mscorlib]System.Object
{
  .field private static int32 refill_lexbuf
  .field private static int32 lex_buffer
  .field private static int32 lex_buffer_len
  .field private static int32 lex_abs_pos
  .field private static int32 lex_start_pos
  .field private static int32 lex_curr_pos
  .field private static int32 lex_last_pos
  .field private static int32 lex_last_action
  .field private static int32 lex_eof_reached
  .field private static int32 lex_base
  .field private static int32 lex_backtrk
  .field private static int32 lex_default
  .field private static int32 lex_trans
  .field private static int32 lex_check
  .method private hidebysig static int32 
          extract_short(char[] s, int32 n) cil managed
  {
    // Code size       49 (0x31)
    .maxstack  4
    .locals init (int32 V_0,
             int32 V_1)
    IL_0000:  ldarg.0
    IL_0001:  ldc.i4.2
    IL_0002:  ldarg.1
    IL_0003:  mul
    IL_0004:  ldelem.i2
    IL_0009:  ldarg.0
    IL_000a:  ldc.i4.2
    IL_000b:  ldarg.1
    IL_000c:  mul
    IL_000d:  ldc.i4.1
    IL_000e:  add
    IL_000f:  ldelem.i2
    IL_0014:  ldc.i4.8
    IL_0015:  shl
    IL_0016:  add
    IL_0017:  stloc.0
    IL_0018:  ldloc.0
    IL_0019:  ldc.i4     0x8000
    IL_001e:  and
    IL_001f:  brtrue.s   IL_0025

    IL_0021:  ldloc.0
    IL_0022:  stloc.1
    IL_0023:  br.s       IL_002f

    IL_0025:  ldloc.0
    IL_0026:  ldc.i4     0x10000
    IL_002b:  sub
    IL_002c:  stloc.1
    IL_002d:  br.s       IL_002f

    IL_002f:  ldloc.1
    IL_0030:  ret
  } // end of method CamIL.LexingParsing::extract_short

  .method public hidebysig static int32  lex_engine(object[] tbl,
                                                    int32 start_state,
                                                    object[] lexbuf) cil managed
  {
    // Code size       466 (0x1d2)
    .maxstack  4
    .locals init (int32 V_0,
             int32 V_1,
             int32 V_2,
             int32 V_3,
             int32 V_4)
    IL_0000:  ldarg.1
    IL_0001:  stloc.0
    IL_0002:  ldloc.0
    IL_0003:  ldc.i4.0
    IL_0004:  blt.s      IL_0047

    IL_0006:  ldarg.2
    IL_0007:  ldsfld     int32 CamIL.LexingParsing::lex_last_pos
    IL_000c:  ldarg.2
    IL_000d:  ldsfld     int32 CamIL.LexingParsing::lex_curr_pos
    IL_0012:  ldelem.ref
    IL_0013:  unbox      [mscorlib]System.Int32
    IL_0018:  ldind.i4
    IL_0019:  box        [mscorlib]System.Int32
    IL_001e:  stelem.ref
    IL_001f:  ldarg.2
    IL_0020:  ldsfld     int32 CamIL.LexingParsing::lex_start_pos
    IL_0025:  ldarg.2
    IL_0026:  ldsfld     int32 CamIL.LexingParsing::lex_curr_pos
    IL_002b:  ldelem.ref
    IL_002c:  unbox      [mscorlib]System.Int32
    IL_0031:  ldind.i4
    IL_0032:  box        [mscorlib]System.Int32
    IL_0037:  stelem.ref
    IL_0038:  ldarg.2
    IL_0039:  ldsfld     int32 CamIL.LexingParsing::lex_last_action
    IL_003e:  ldc.i4.m1
    IL_003f:  box        [mscorlib]System.Int32
    IL_0044:  stelem.ref
    IL_0045:  br.s       IL_004c

    IL_0047:  ldloc.0
    IL_0048:  neg
    IL_0049:  ldc.i4.1
    IL_004a:  sub
    IL_004b:  stloc.0
    IL_004c:  br         IL_01ca

    IL_0051:  ldarg.0
    IL_0052:  ldsfld     int32 CamIL.LexingParsing::lex_base
    IL_0057:  ldelem.ref
    IL_0058:  castclass char[]
    IL_005d:  ldloc.0
    IL_005e:  call       int32 CamIL.LexingParsing::extract_short(char[],int32)
    IL_0063:  stloc.1
    IL_0064:  ldloc.1
    IL_0065:  ldc.i4.0
    IL_0066:  bge.s      IL_0073

    IL_0068:  ldloc.1
    IL_0069:  neg
    IL_006a:  ldc.i4.1
    IL_006b:  sub
    IL_006c:  stloc.s    V_4
    IL_006e:  br         IL_01cf

    IL_0073:  ldarg.0
    IL_0074:  ldsfld     int32 CamIL.LexingParsing::lex_backtrk
    IL_0079:  ldelem.ref
    IL_007a:  castclass char[]
    IL_007f:  ldloc.0
    IL_0080:  call       int32 CamIL.LexingParsing::extract_short(char[],int32)
    IL_0085:  stloc.2
    IL_0086:  ldloc.2
    IL_0087:  ldc.i4.0
    IL_0088:  blt.s      IL_00b0

    IL_008a:  ldarg.2
    IL_008b:  ldsfld     int32 CamIL.LexingParsing::lex_last_pos
    IL_0090:  ldarg.2
    IL_0091:  ldsfld     int32 CamIL.LexingParsing::lex_curr_pos
    IL_0096:  ldelem.ref
    IL_0097:  unbox      [mscorlib]System.Int32
    IL_009c:  ldind.i4
    IL_009d:  box        [mscorlib]System.Int32
    IL_00a2:  stelem.ref
    IL_00a3:  ldarg.2
    IL_00a4:  ldsfld     int32 CamIL.LexingParsing::lex_last_action
    IL_00a9:  ldloc.2
    IL_00aa:  box        [mscorlib]System.Int32
    IL_00af:  stelem.ref
    IL_00b0:  ldarg.2
    IL_00b1:  ldsfld     int32 CamIL.LexingParsing::lex_curr_pos
    IL_00b6:  ldelem.ref
    IL_00b7:  unbox      [mscorlib]System.Int32
    IL_00bc:  ldind.i4
    IL_00bd:  ldarg.2
    IL_00be:  ldsfld     int32 CamIL.LexingParsing::lex_buffer_len
    IL_00c3:  ldelem.ref
    IL_00c4:  unbox      [mscorlib]System.Int32
    IL_00c9:  ldind.i4
    IL_00ca:  blt.s      IL_00ee

    IL_00cc:  ldarg.2
    IL_00cd:  ldsfld     int32 CamIL.LexingParsing::lex_eof_reached
    IL_00d2:  ldelem.ref
    IL_00d3:  unbox      [mscorlib]System.Int32
    IL_00d8:  ldind.i4
    IL_00d9:  brtrue.s   IL_00e6

    IL_00db:  ldloc.0
    IL_00dc:  neg
    IL_00dd:  ldc.i4.1
    IL_00de:  sub
    IL_00df:  stloc.s    V_4
    IL_00e1:  br         IL_01cf

    IL_00e6:  ldc.i4     0x100
    IL_00eb:  stloc.3
    IL_00ec:  br.s       IL_0128

    IL_00ee:  ldarg.2
    IL_00ef:  ldsfld     int32 CamIL.LexingParsing::lex_buffer
    IL_00f4:  ldelem.ref
    IL_00f5:  castclass char[]
    IL_00fa:  ldarg.2
    IL_00fb:  ldsfld     int32 CamIL.LexingParsing::lex_curr_pos
    IL_0100:  ldelem.ref
    IL_0101:  unbox      [mscorlib]System.Int32
    IL_0106:  ldind.i4
    IL_0107:  ldelem.i2 //callvirt   instance char [mscorlib]System.Text.StringBuilder::get_Chars(int32)
    IL_010c:  stloc.3
    IL_010d:  ldarg.2
    IL_010e:  ldsfld     int32 CamIL.LexingParsing::lex_curr_pos
    IL_0113:  ldarg.2
    IL_0114:  ldsfld     int32 CamIL.LexingParsing::lex_curr_pos
    IL_0119:  ldelem.ref
    IL_011a:  unbox      [mscorlib]System.Int32
    IL_011f:  ldind.i4
    IL_0120:  ldc.i4.1
    IL_0121:  add
    IL_0122:  box        [mscorlib]System.Int32
    IL_0127:  stelem.ref
    IL_0128:  ldarg.0
    IL_0129:  ldsfld     int32 CamIL.LexingParsing::lex_check
    IL_012e:  ldelem.ref
    IL_012f:  castclass char[]
    IL_0134:  ldloc.1
    IL_0135:  ldloc.3
    IL_0136:  add
    IL_0137:  call       int32 CamIL.LexingParsing::extract_short(char[],int32)
    IL_013c:  ldloc.0
    IL_013d:  bne.un.s   IL_0156

    IL_013f:  ldarg.0
    IL_0140:  ldsfld     int32 CamIL.LexingParsing::lex_trans
    IL_0145:  ldelem.ref
    IL_0146:  castclass char[]
    IL_014b:  ldloc.1
    IL_014c:  ldloc.3
    IL_014d:  add
    IL_014e:  call       int32 CamIL.LexingParsing::extract_short(char[],int32)
    IL_0153:  stloc.0
    IL_0154:  br.s       IL_0169

    IL_0156:  ldarg.0
    IL_0157:  ldsfld     int32 CamIL.LexingParsing::lex_default
    IL_015c:  ldelem.ref
    IL_015d:  castclass char[]
    IL_0162:  ldloc.0
    IL_0163:  call       int32 CamIL.LexingParsing::extract_short(char[],int32)
    IL_0168:  stloc.0
    IL_0169:  ldloc.0
    IL_016a:  ldc.i4.0
    IL_016b:  bge.s      IL_01b5

    IL_016d:  ldarg.2
    IL_016e:  ldsfld     int32 CamIL.LexingParsing::lex_curr_pos
    IL_0173:  ldarg.2
    IL_0174:  ldsfld     int32 CamIL.LexingParsing::lex_last_pos
    IL_0179:  ldelem.ref
    IL_017a:  unbox      [mscorlib]System.Int32
    IL_017f:  ldind.i4
    IL_0180:  box        [mscorlib]System.Int32
    IL_0185:  stelem.ref
    IL_0186:  ldarg.2
    IL_0187:  ldsfld     int32 CamIL.LexingParsing::lex_last_action
    IL_018c:  ldelem.ref
    IL_018d:  unbox      [mscorlib]System.Int32
    IL_0192:  ldind.i4
    IL_0193:  ldc.i4.m1
    IL_0194:  bne.un.s   IL_01a2

    IL_0196:  ldstr      "lexing: empty token"
    IL_019b:  call       void CamIL.Exception::failure(string)
    IL_01a0:  br.s       IL_01b3

    IL_01a2:  ldarg.2
    IL_01a3:  ldsfld     int32 CamIL.LexingParsing::lex_last_action
    IL_01a8:  ldelem.ref
    IL_01a9:  unbox      [mscorlib]System.Int32
    IL_01ae:  ldind.i4
    IL_01af:  stloc.s    V_4
    IL_01b1:  br.s       IL_01cf

    IL_01b3:  br.s       IL_01ca

    IL_01b5:  ldloc.3
    IL_01b6:  ldc.i4     0x100
    IL_01bb:  bne.un.s   IL_01ca

    IL_01bd:  ldarg.2
    IL_01be:  ldsfld     int32 CamIL.LexingParsing::lex_eof_reached
    IL_01c3:  ldc.i4.0
    IL_01c4:  box        [mscorlib]System.Int32
    IL_01c9:  stelem.ref
    IL_01ca:  br         IL_0051

    IL_01cf:  ldloc.s    V_4
    IL_01d1:  ret
  } // end of method CamIL.LexingParsing::lex_engine

  .method private hidebysig specialname rtspecialname static 
          void  .cctor() cil managed
  {
    // Code size       85 (0x55)
    .maxstack  1
    IL_0000:  ldc.i4.0
    IL_0001:  stsfld     int32 CamIL.LexingParsing::refill_lexbuf
    IL_0006:  ldc.i4.1
    IL_0007:  stsfld     int32 CamIL.LexingParsing::lex_buffer
    IL_000c:  ldc.i4.2
    IL_000d:  stsfld     int32 CamIL.LexingParsing::lex_buffer_len
    IL_0012:  ldc.i4.3
    IL_0013:  stsfld     int32 CamIL.LexingParsing::lex_abs_pos
    IL_0018:  ldc.i4.4
    IL_0019:  stsfld     int32 CamIL.LexingParsing::lex_start_pos
    IL_001e:  ldc.i4.5
    IL_001f:  stsfld     int32 CamIL.LexingParsing::lex_curr_pos
    IL_0024:  ldc.i4.6
    IL_0025:  stsfld     int32 CamIL.LexingParsing::lex_last_pos
    IL_002a:  ldc.i4.7
    IL_002b:  stsfld     int32 CamIL.LexingParsing::lex_last_action
    IL_0030:  ldc.i4.8
    IL_0031:  stsfld     int32 CamIL.LexingParsing::lex_eof_reached
    IL_0036:  ldc.i4.0
    IL_0037:  stsfld     int32 CamIL.LexingParsing::lex_base
    IL_003c:  ldc.i4.1
    IL_003d:  stsfld     int32 CamIL.LexingParsing::lex_backtrk
    IL_0042:  ldc.i4.2
    IL_0043:  stsfld     int32 CamIL.LexingParsing::lex_default
    IL_0048:  ldc.i4.3
    IL_0049:  stsfld     int32 CamIL.LexingParsing::lex_trans
    IL_004e:  ldc.i4.4
    IL_004f:  stsfld     int32 CamIL.LexingParsing::lex_check
    IL_0054:  ret
  } // end of method CamIL.LexingParsing::.cctor

  .method public hidebysig specialname rtspecialname 
          instance void  .ctor() cil managed
  {
    // Code size       7 (0x7)
    .maxstack  1
    IL_0000:  ldarg.0
    IL_0001:  call       instance void [mscorlib]System.Object::.ctor()
    IL_0006:  ret
  } // end of method CamIL.LexingParsing::.ctor

} // end of class CamIL.LexingParsing


// =============================================================

//*********** DISASSEMBLY COMPLETE ***********************
