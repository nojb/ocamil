.class public auto ansi beforefieldinit LexingParsing
       extends [mscorlib]System.Object
{
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
    IL_0004:  ldelem.i2 //callvirt   instance char [mscorlib]System.Text.StringBuilder::get_Chars(int32)	
    IL_0009:  ldarg.0
    IL_000a:  ldc.i4.2
    IL_000b:  ldarg.1
    IL_000c:  mul
    IL_000d:  ldc.i4.1
    IL_000e:  add
    IL_000f:  ldelem.i2 //callvirt   instance char [mscorlib]System.Text.StringBuilder::get_Chars(int32)
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

  .method public hidebysig static int32  lex_engine(class Lexing.lex_tables tbl,
                                                    int32 start_state,
                                                    class Lexing.lexbuf lexbuf) cil managed
  {
    // Code size       305 (0x131)
    .maxstack  3
    .locals init (int32 V_0,
             int32 V_1,
             int32 V_2,
             int32 V_3,
             int32 V_4)
    IL_0000:  ldarg.1
    IL_0001:  stloc.0
    IL_0002:  ldloc.0
    IL_0003:  ldc.i4.0
    IL_0004:  blt.s      IL_0027

    IL_0006:  ldarg.2
    IL_0007:  ldarg.2
    IL_0008:  ldfld      int32 Lexing.lexbuf::lex_curr_pos
    IL_000d:  stfld      int32 Lexing.lexbuf::lex_last_pos
    IL_0012:  ldarg.2
    IL_0013:  ldarg.2
    IL_0014:  ldfld      int32 Lexing.lexbuf::lex_curr_pos
    IL_0019:  stfld      int32 Lexing.lexbuf::lex_start_pos
    IL_001e:  ldarg.2
    IL_001f:  ldc.i4.m1
    IL_0020:  stfld      int32 Lexing.lexbuf::lex_last_action
    IL_0025:  br.s       IL_002c

    IL_0027:  ldloc.0
    IL_0028:  neg
    IL_0029:  ldc.i4.1
    IL_002a:  sub
    IL_002b:  stloc.0
    IL_002c:  br         IL_0129

    IL_0031:  ldarg.0
    IL_0032:  ldfld      char[] Lexing.lex_tables::lex_base
    IL_0037:  ldloc.0
    IL_0038:  call       int32 CamIL.LexingParsing::extract_short(char[],int32)
    IL_003d:  stloc.1
    IL_003e:  ldloc.1
    IL_003f:  ldc.i4.0
    IL_0040:  bge.s      IL_004d

    IL_0042:  ldloc.1
    IL_0043:  neg
    IL_0044:  ldc.i4.1
    IL_0045:  sub
    IL_0046:  stloc.s    V_4
    IL_0048:  br         IL_012e

    IL_004d:  ldarg.0
    IL_004e:  ldfld      char[] Lexing.lex_tables::lex_backtrk
    IL_0053:  ldloc.0
    IL_0054:  call       int32 CamIL.LexingParsing::extract_short(char[], int32)
    IL_0059:  stloc.2
    IL_005a:  ldloc.2
    IL_005b:  ldc.i4.0
    IL_005c:  blt.s      IL_0071

    IL_005e:  ldarg.2
    IL_005f:  ldarg.2
    IL_0060:  ldfld      int32 Lexing.lexbuf::lex_curr_pos
    IL_0065:  stfld      int32 Lexing.lexbuf::lex_last_pos
    IL_006a:  ldarg.2
    IL_006b:  ldloc.2
    IL_006c:  stfld      int32 Lexing.lexbuf::lex_last_action
    IL_0071:  ldarg.2
    IL_0072:  ldfld      int32 Lexing.lexbuf::lex_curr_pos
    IL_0077:  ldarg.2
    IL_0078:  ldfld      int32 Lexing.lexbuf::lex_buffer_len
    IL_007d:  blt.s      IL_009a

    IL_007f:  ldarg.2
    IL_0080:  ldfld      bool Lexing.lexbuf::lex_eof_reached
    IL_0085:  brtrue.s   IL_0092

    IL_0087:  ldloc.0
    IL_0088:  neg
    IL_0089:  ldc.i4.1
    IL_008a:  sub
    IL_008b:  stloc.s    V_4
    IL_008d:  br         IL_012e

    IL_0092:  ldc.i4     0x100
    IL_0097:  stloc.3
    IL_0098:  br.s       IL_00ba

    IL_009a:  ldarg.2
    IL_009b:  ldfld      char[] Lexing.lexbuf::lex_buffer
    IL_00a0:  ldarg.2
    IL_00a1:  ldfld      int32 Lexing.lexbuf::lex_curr_pos
    IL_00a6:  ldelem.i2 //callvirt   instance char [mscorlib]System.Text.StringBuilder::get_Chars(int32)
    IL_00ab:  stloc.3
    IL_00ac:  ldarg.2
    IL_00ad:  ldarg.2
    IL_00ae:  ldfld      int32 Lexing.lexbuf::lex_curr_pos
    IL_00b3:  ldc.i4.1
    IL_00b4:  add
    IL_00b5:  stfld      int32 Lexing.lexbuf::lex_curr_pos
    IL_00ba:  ldarg.0
    IL_00bb:  ldfld      char[] Lexing.lex_tables::lex_check
    IL_00c0:  ldloc.1
    IL_00c1:  ldloc.3
    IL_00c2:  add
    IL_00c3:  call       int32 CamIL.LexingParsing::extract_short(char[], int32)
    IL_00c8:  ldloc.0
    IL_00c9:  bne.un.s   IL_00dc

    IL_00cb:  ldarg.0
    IL_00cc:  ldfld      char[] Lexing.lex_tables::lex_trans
    IL_00d1:  ldloc.1
    IL_00d2:  ldloc.3
    IL_00d3:  add
    IL_00d4:  call       int32 CamIL.LexingParsing::extract_short(char[], int32)
    IL_00d9:  stloc.0
    IL_00da:  br.s       IL_00e9

    IL_00dc:  ldarg.0
    IL_00dd:  ldfld      char[] Lexing.lex_tables::lex_default
    IL_00e2:  ldloc.0
    IL_00e3:  call       int32 CamIL.LexingParsing::extract_short(char[], int32)
    IL_00e8:  stloc.0
    IL_00e9:  ldloc.0
    IL_00ea:  ldc.i4.0
    IL_00eb:  bge.s      IL_011a

    IL_00ed:  ldarg.2
    IL_00ee:  ldarg.2
    IL_00ef:  ldfld      int32 Lexing.lexbuf::lex_last_pos
    IL_00f4:  stfld      int32 Lexing.lexbuf::lex_curr_pos
    IL_00f9:  ldarg.2
    IL_00fa:  ldfld      int32 Lexing.lexbuf::lex_last_action
    IL_00ff:  ldc.i4.m1
    IL_0100:  bne.un.s   IL_010e

    IL_0102:  ldstr      "lexing: empty token"
    IL_0107:  call       void CamIL.Exception::failure(string)
    IL_010c:  br.s       IL_0118

    IL_010e:  ldarg.2
    IL_010f:  ldfld      int32 Lexing.lexbuf::lex_last_action
    IL_0114:  stloc.s    V_4
    IL_0116:  br.s       IL_012e

    IL_0118:  br.s       IL_0129

    IL_011a:  ldloc.3
    IL_011b:  ldc.i4     0x100
    IL_0120:  bne.un.s   IL_0129

    IL_0122:  ldarg.2
    IL_0123:  ldc.i4.0
    IL_0124:  stfld      bool Lexing.lexbuf::lex_eof_reached
    IL_0129:  br         IL_0031

    IL_012e:  ldloc.s    V_4
    IL_0130:  ret
  } // end of method CamIL.LexingParsing::lex_engine

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

