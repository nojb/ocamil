/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Raphael Montelatici, Equipe PPS                           //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

.class public auto ansi serializable Record 
 {
  .method public specialname rtspecialname instance void  .ctor() il managed 
   {
    .maxstack 1 
       	ldarg.0
       	call instance void [mscorlib]System.Object::.ctor() 
       	ret
   }

  .method public virtual instance bool equals(class CamIL.Record) il managed
   {
    .maxstack 1
      	ldstr "the virtual equals method of a record is not defined"
      	call void [mscorlib]System.Console::WriteLine(class [mscorlib]System.String)
      	ldc.i4.0
      	ret
   }

  .method public virtual instance int32 compare(class CamIL.Record) il managed
   {
    .maxstack 1
      	ldstr "the virtual compare method of a record is not defined"
      	call void [mscorlib]System.Console::WriteLine(class [mscorlib]System.String)
      	ldc.i4.0
      	ret
   }
	
  .method public virtual instance class CamIL.Record duplicate() il managed
   {
    .maxstack 1
      	ldstr "the virtual duplication method of a record is not defined"
      	call void [mscorlib]System.Console::WriteLine(class [mscorlib]System.String)
      	ldarg.0
      	ret
   }
  
  .method public virtual instance int32 hashcode() il managed
   {
    .maxstack 1
      	ldstr "the virtual hashcode method of a record is not defined"
      	call void [mscorlib]System.Console::WriteLine(class [mscorlib]System.String)
	ldc.i4.0
      	ret
   }
	
 }

.class public auto ansi serializable Variant 
 {
  .field public int32 tag
  .field public int32 size
	
  .method public specialname rtspecialname instance void .ctor(int32) il managed 
   {
    .maxstack 2
       	ldarg.0
       	call instance void [mscorlib]System.Object::.ctor()
	ldarg.0
	ldarg.1
	stfld int32 CamIL.Variant::tag
       	ret
   }

  .method public instance bool vequals(class CamIL.Variant) il managed
   {
    .maxstack 2
	ldarg.0
	ldarg.1
	beq PHYSICALLY_EQUAL
	ldarg.0
	ldfld int32 CamIL.Variant::tag
	ldarg.1
	ldfld int32 CamIL.Variant::tag
	ceq
	brfalse DIFFERENT_TAG
	ldarg.0
	ldarg.1
	tail.
	callvirt bool CamIL.Variant::equals(class CamIL.Variant)
	ret
DIFFERENT_TAG:	
	ldc.i4.0
	ret
PHYSICALLY_EQUAL:	
	ldc.i4.1
	ret
   }

  .method public instance int32 vcompare(class CamIL.Variant) il managed
   {
   .maxstack 2
	ldarg.0
	ldarg.1
	beq PHYSICALLY_EQUAL
	ldarg.0
	ldfld int32 CamIL.Variant::tag
	ldarg.1
	ldfld int32 CamIL.Variant::tag
	sub
	dup
	brfalse SAMETAG
	ret
SAMETAG:
	pop
	ldarg.0
	ldarg.1
	callvirt instance int32 CamIL.Variant::compare(class CamIL.Variant)
	ret
PHYSICALLY_EQUAL:	
	ldc.i4.0
	ret
   }


  .method public virtual instance bool equals(class CamIL.Variant) il managed
   {
    .maxstack 1
      	ldstr "the virtual equals method of a variant is not defined"
      	call void [mscorlib]System.Console::WriteLine(class [mscorlib]System.String)
      	ldc.i4.0
      	ret
   }

  .method public virtual instance int32 compare(class CamIL.Variant) il managed
   {
    .maxstack 1
      	ldstr "the virtual equals method of a variant is not defined"
      	call void [mscorlib]System.Console::WriteLine(class [mscorlib]System.String)
      	ldc.i4.0
      	ret
   }

		
  .method public virtual instance class CamIL.Variant duplicate() il managed
   {
    .maxstack 1
      	ldstr "the virtual duplication method of a variant is not defined"
      	call void [mscorlib]System.Console::WriteLine(class [mscorlib]System.String)
      	ldarg.0
      	ret
   }

  .method public virtual instance int32 hashcode() il managed
   {
    .maxstack 1
      	ldstr "the virtual hashcode method of a variant is not defined"
      	call void [mscorlib]System.Console::WriteLine(class [mscorlib]System.String)
	ldc.i4.0
      	ret
   }

	
		
 }


	
// Disassembled from :
// type 'a camil_list = Nil | Cons of 'a * 'a camil_list
// type 'a camil_option = None | Some of 'a

	
		
  .class public auto ansi serializable Nil_OF_camil_list
         extends CamIL.Variant
  {
    .method public specialname rtspecialname 
            instance void  .ctor() cil managed
    {
      // Code size       15 (0xf)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  ldc.i4.0
      IL_0002:  call       instance void CamIL.Variant::.ctor(int32)
      IL_0007:  ldarg.0
      IL_0008:  ldc.i4.0
      IL_0009:  stfld      int32 CamIL.Variant::size
      IL_000e:  ret
    } // end of method Nil_OF_camil_list::.ctor

    .method public virtual instance int32 
            compare(class CamIL.Variant A_0) cil managed
    {
      // Code size       2 (0x2)
      .maxstack  1
      IL_0000:  ldc.i4.0
      IL_0001:  ret
    } // end of method Nil_OF_camil_list::compare

    .method public virtual instance bool 
            equals(class CamIL.Variant A_0) cil managed
    {
      // Code size       2 (0x2)
      .maxstack  1
      IL_0000:  ldc.i4.1
      IL_0001:  ret
    } // end of method Nil_OF_camil_list::equals

    .method public virtual instance class CamIL.Variant 
            duplicate() cil managed
    {
      // Code size       2 (0x2)
      .maxstack  1
      IL_0000:  ldarg.0
      IL_0001:  ret
    } // end of method Nil_OF_camil_list::duplicate

    .method public virtual instance int32 
            hashcode() cil managed
    {
      // Code size       2 (0x2)
      .maxstack  1
      IL_0000:  ldc.i4.0
      IL_0001:  ret
    } // end of method Nil_OF_camil_list::hashcode

  } // end of class Nil_OF_camil_list

  .class public auto ansi serializable Cons_OF_camil_list
         extends CamIL.Variant
  {
    .field public object x0
    .field public class CamIL.Variant x1
    .method public specialname rtspecialname 
            instance void  .ctor() cil managed
    {
      // Code size       15 (0xf)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  ldc.i4.1
      IL_0002:  call       instance void CamIL.Variant::.ctor(int32)
      IL_0007:  ldarg.0
      IL_0008:  ldc.i4.0
      IL_0009:  stfld      int32 CamIL.Variant::size
      IL_000e:  ret
    } // end of method Cons_OF_camil_list::.ctor

    .method public specialname rtspecialname 
            instance void  .ctor(object A_0,
                                 class CamIL.Variant A_1) cil managed
    {
      // Code size       29 (0x1d)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  ldc.i4.1
      IL_0002:  call       instance void CamIL.Variant::.ctor(int32)
      IL_0007:  ldarg.0
      IL_0008:  ldc.i4.2
      IL_0009:  stfld      int32 CamIL.Variant::size
      IL_000e:  ldarg.0
      IL_000f:  ldarg.1
      IL_0010:  stfld      object CamIL.Cons_OF_camil_list::x0
      IL_0015:  ldarg.0
      IL_0016:  ldarg.2
      IL_0017:  stfld      class CamIL.Variant CamIL.Cons_OF_camil_list::x1
      IL_001c:  ret
    } // end of method Cons_OF_camil_list::.ctor

    .method public virtual instance int32 
            compare(class CamIL.Variant A_0) cil managed
    {
      // Code size       55 (0x37)
      .maxstack  2
      .locals init (class CamIL.Cons_OF_camil_list V_0,
               int32 V_1)
      IL_0000:  ldarg.1
      IL_0001:  castclass  CamIL.Cons_OF_camil_list
      IL_0006:  stloc.0
      IL_0007:  ldarg.0
      IL_0008:  ldfld      object CamIL.Cons_OF_camil_list::x0
      IL_000d:  ldloc.0
      IL_000e:  ldfld      object CamIL.Cons_OF_camil_list::x0
      IL_0013:  call       int32 CamIL.Compare::compare(object,
                                                                    object)
      IL_0018:  dup
      IL_0019:  stloc.1
      IL_001a:  brfalse    IL_0021

      IL_001f:  ldloc.1
      IL_0020:  ret

      IL_0021:  ldarg.0
      IL_0022:  ldfld      class CamIL.Variant CamIL.Cons_OF_camil_list::x1
      IL_0027:  ldloc.0
      IL_0028:  ldfld      class CamIL.Variant CamIL.Cons_OF_camil_list::x1
      IL_002d:  callvirt   instance int32 CamIL.Variant::vcompare(class CamIL.Variant)
      IL_0032:  ret

      IL_0033:  ldc.i4.1
      IL_0034:  ret

      IL_0035:  ldc.i4.0
      IL_0036:  ret
    } // end of method Cons_OF_camil_list::compare

    .method public virtual instance bool 
            equals(class CamIL.Variant A_0) cil managed
    {
      // Code size       57 (0x39)
      .maxstack  2
      .locals init (class CamIL.Cons_OF_camil_list V_0)
      IL_0000:  ldarg.1
      IL_0001:  castclass  CamIL.Cons_OF_camil_list
      IL_0006:  stloc.0
      IL_0007:  ldarg.0
      IL_0008:  ldfld      object CamIL.Cons_OF_camil_list::x0
      IL_000d:  ldloc.0
      IL_000e:  ldfld      object CamIL.Cons_OF_camil_list::x0
      IL_0013:  call       bool CamIL.Compare::equal(object,
                                                                  object)
      IL_0018:  brfalse    IL_0035

      IL_001d:  ldarg.0
      IL_001e:  ldfld      class CamIL.Variant CamIL.Cons_OF_camil_list::x1
      IL_0023:  ldloc.0
      IL_0024:  ldfld      class CamIL.Variant CamIL.Cons_OF_camil_list::x1
      IL_0029:  callvirt   instance bool CamIL.Variant::vequals(class CamIL.Variant)
      IL_002e:  brfalse    IL_0035

      IL_0033:  ldc.i4.1
      IL_0034:  ret

      IL_0035:  ldc.i4.0
      IL_0036:  ret

      IL_0037:  ldc.i4.1
      IL_0038:  ret
    } // end of method Cons_OF_camil_list::equals

    .method public virtual instance class CamIL.Variant 
            duplicate() cil managed
    {
      // Code size       18 (0x12)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  ldfld      object CamIL.Cons_OF_camil_list::x0
      IL_0006:  ldarg.0
      IL_0007:  ldfld      class CamIL.Variant CamIL.Cons_OF_camil_list::x1
      IL_000c:  newobj     instance void CamIL.Cons_OF_camil_list::.ctor(object,
                                                                        class CamIL.Variant)
      IL_0011:  ret
    } // end of method Cons_OF_camil_list::duplicate

    .method public virtual instance int32 
            hashcode() cil managed
    {
      // Code size       37 (0x25)
      .maxstack  2
      IL_0000:  ldc.i4.0
      IL_0001:  ldc.i4.s   19
      IL_0003:  mul
      IL_0004:  ldarg.0
      IL_0005:  ldfld      object CamIL.Cons_OF_camil_list::x0
      IL_000a:  call       int32 CamIL.Hash::HashCode(object)
      IL_000f:  add
      IL_0010:  ldc.i4.s   19
      IL_0012:  mul
      IL_0013:  ldarg.0
      IL_0014:  ldfld      class CamIL.Variant CamIL.Cons_OF_camil_list::x1
      IL_0019:  call       int32 CamIL.Hash::HashCode(object)
      IL_001e:  add
      IL_001f:  call       int32 [mscorlib]System.Math::Abs(int32)
      IL_0024:  ret
    } // end of method Cons_OF_camil_list::hashcode

  } // end of class Cons_OF_camil_list

  .class public auto ansi serializable None_OF_camil_option
         extends CamIL.Variant
  {
    .method public specialname rtspecialname 
            instance void  .ctor() cil managed
    {
      // Code size       15 (0xf)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  ldc.i4.0
      IL_0002:  call       instance void CamIL.Variant::.ctor(int32)
      IL_0007:  ldarg.0
      IL_0008:  ldc.i4.0
      IL_0009:  stfld      int32 CamIL.Variant::size
      IL_000e:  ret
    } // end of method None_OF_camil_option::.ctor

    .method public virtual instance int32 
            compare(class CamIL.Variant A_0) cil managed
    {
      // Code size       2 (0x2)
      .maxstack  1
      IL_0000:  ldc.i4.0
      IL_0001:  ret
    } // end of method None_OF_camil_option::compare

    .method public virtual instance bool 
            equals(class CamIL.Variant A_0) cil managed
    {
      // Code size       2 (0x2)
      .maxstack  1
      IL_0000:  ldc.i4.1
      IL_0001:  ret
    } // end of method None_OF_camil_option::equals

    .method public virtual instance class CamIL.Variant 
            duplicate() cil managed
    {
      // Code size       2 (0x2)
      .maxstack  1
      IL_0000:  ldarg.0
      IL_0001:  ret
    } // end of method None_OF_camil_option::duplicate

    .method public virtual instance int32 
            hashcode() cil managed
    {
      // Code size       2 (0x2)
      .maxstack  1
      IL_0000:  ldc.i4.0
      IL_0001:  ret
    } // end of method None_OF_camil_option::hashcode

  } // end of class None_OF_camil_option

  .class public auto ansi serializable Some_OF_camil_option
         extends CamIL.Variant
  {
    .field public object x0
    .method public specialname rtspecialname 
            instance void  .ctor() cil managed
    {
      // Code size       15 (0xf)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  ldc.i4.1
      IL_0002:  call       instance void CamIL.Variant::.ctor(int32)
      IL_0007:  ldarg.0
      IL_0008:  ldc.i4.0
      IL_0009:  stfld      int32 CamIL.Variant::size
      IL_000e:  ret
    } // end of method Some_OF_camil_option::.ctor

    .method public specialname rtspecialname 
            instance void  .ctor(object A_0) cil managed
    {
      // Code size       22 (0x16)
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0001:  ldc.i4.1
      IL_0002:  call       instance void CamIL.Variant::.ctor(int32)
      IL_0007:  ldarg.0
      IL_0008:  ldc.i4.1
      IL_0009:  stfld      int32 CamIL.Variant::size
      IL_000e:  ldarg.0
      IL_000f:  ldarg.1
      IL_0010:  stfld      object CamIL.Some_OF_camil_option::x0
      IL_0015:  ret
    } // end of method Some_OF_camil_option::.ctor

    .method public virtual instance int32 
            compare(class CamIL.Variant A_0) cil managed
    {
      // Code size       29 (0x1d)
      .maxstack  2
      .locals init (class CamIL.Some_OF_camil_option V_0,
               int32 V_1)
      IL_0000:  ldarg.1
      IL_0001:  castclass  CamIL.Some_OF_camil_option
      IL_0006:  stloc.0
      IL_0007:  ldarg.0
      IL_0008:  ldfld      object CamIL.Some_OF_camil_option::x0
      IL_000d:  ldloc.0
      IL_000e:  ldfld      object CamIL.Some_OF_camil_option::x0
      IL_0013:  call       int32 CamIL.Compare::compare(object,
                                                                    object)
      IL_0018:  ret

      IL_0019:  ldc.i4.1
      IL_001a:  ret

      IL_001b:  ldc.i4.0
      IL_001c:  ret
    } // end of method Some_OF_camil_option::compare

    .method public virtual instance bool 
            equals(class CamIL.Variant A_0) cil managed
    {
      // Code size       35 (0x23)
      .maxstack  2
      .locals init (class CamIL.Some_OF_camil_option V_0)
      IL_0000:  ldarg.1
      IL_0001:  castclass  CamIL.Some_OF_camil_option
      IL_0006:  stloc.0
      IL_0007:  ldarg.0
      IL_0008:  ldfld      object CamIL.Some_OF_camil_option::x0
      IL_000d:  ldloc.0
      IL_000e:  ldfld      object CamIL.Some_OF_camil_option::x0
      IL_0013:  call       bool CamIL.Compare::equal(object,
                                                                  object)
      IL_0018:  brfalse    IL_001f

      IL_001d:  ldc.i4.1
      IL_001e:  ret

      IL_001f:  ldc.i4.0
      IL_0020:  ret

      IL_0021:  ldc.i4.1
      IL_0022:  ret
    } // end of method Some_OF_camil_option::equals

    .method public virtual instance class CamIL.Variant 
            duplicate() cil managed
    {
      // Code size       12 (0xc)
      .maxstack  1
      IL_0000:  ldarg.0
      IL_0001:  ldfld      object CamIL.Some_OF_camil_option::x0
      IL_0006:  newobj     instance void CamIL.Some_OF_camil_option::.ctor(object)
      IL_000b:  ret
    } // end of method Some_OF_camil_option::duplicate

    .method public virtual instance int32 
            hashcode() cil managed
    {
      // Code size       22 (0x16)
      .maxstack  2
      IL_0000:  ldc.i4.0
      IL_0001:  ldc.i4.s   19
      IL_0003:  mul
      IL_0004:  ldarg.0
      IL_0005:  ldfld      object CamIL.Some_OF_camil_option::x0
      IL_000a:  call       int32 CamIL.Hash::HashCode(object)
      IL_000f:  add
      IL_0010:  call       int32 [mscorlib]System.Math::Abs(int32)
      IL_0015:  ret
    } // end of method Some_OF_camil_option::hashcode

  } // end of class Some_OF_camil_option


	
// for optimized constant constructors
		
  .class public auto ansi serializable Variant_OCC
         extends CamIL.Variant
  {
    .method public specialname rtspecialname 
            instance void  .ctor(int32) cil managed
    {
      .maxstack  2
      ldarg.0
      ldarg.1
      call       instance void CamIL.Variant::.ctor(int32)
      ldarg.0
      ldc.i4.0
      stfld      int32 CamIL.Variant::size
      ret
    } 

    .method public virtual instance int32 
            compare(class CamIL.Variant A_0) cil managed
    {
      .maxstack  1
      ldc.i4.0
      ret
    } 

    .method public virtual instance bool 
            equals(class CamIL.Variant A_0) cil managed
    {
      .maxstack  1
      ldc.i4.1
      ret
    } 

    .method public virtual instance class CamIL.Variant 
            duplicate() cil managed
    {
      .maxstack  1
      ldarg.0
      ret
    } 

    .method public virtual instance int32 
            hashcode() cil managed
    {
      .maxstack  1
      ldc.i4.0
      ret
    } 

  } 
	

	