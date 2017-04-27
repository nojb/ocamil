/////////////////////////////////////////////////////////////////////////
//                                                                     //
//                               CamIL                                 // 
//                                                                     // 
//           Bruno Pagano, projet Cristal, INRIA Rocquencourt          //
//                                                                     //
/////////////////////////////////////////////////////////////////////////

// $Id: obj.asm,v 1.16 2006/07/23 03:04:14 montela Exp $
	
.class public auto ansi Obj extends [mscorlib]System.Object 
 {

// remplace %identity dans les cas ou on veut etre sur d'avoir les versions boxees des objets ...
// probablement a remplacer avec une primitive builtin
  .method public static class [mscorlib]System.Object boxed_identity (class [mscorlib]System.Object)
     il managed 
   {
    .maxstack 1
	ldarg.0
	ret
    }

	
  .method public static bool obj_is_int(object)
     il managed 
   {
    .maxstack 1
	ldarg.0
	brnull ISINT // special case for unit ()
	ldarg.0
	isinst [mscorlib]System.Int32
	brtrue ISINT
	ldarg.0
	isinst [mscorlib]System.Char
	brtrue ISINT
	ldarg.0
	isinst [mscorlib]System.Boolean
	brtrue ISINT
	ldc.i4.0
	ret
ISINT:	
	ldc.i4.1
	ret
   }

 .method public static bool obj_is_bool(object)
     il managed 
   {
	ldarg.0
	isinst [mscorlib]System.Boolean
	brtrue IS
	ldc.i4.0
	ret
IS:	
	ldc.i4.1
	ret
   }


 .method public static bool obj_is_char(object)
     il managed 
   {
	ldarg.0
	isinst [mscorlib]System.Char
	brtrue IS
	ldc.i4.0
	ret
IS:	
	ldc.i4.1
	ret
   }

	
  .method public static class [mscorlib]System.Object obj_dup (class [mscorlib]System.Object)
     il managed 
   {
    .maxstack 4
    .locals init (class [mscorlib]System.Object[],int32,class [mscorlib]System.Object[], char[])
// if (x==0) return NULL ;	
      	ldarg.0
        brtrue NOT_NULL
	ldnull
	ret

     NOT_NULL:
// if "arg est un variant" appeler sa méthode de duplication ;
	ldarg.0
	isinst CamIL.Variant
	brnull NOT_VARIANT
	ldarg.0
	castclass CamIL.Variant
	tail.
	callvirt instance class CamIL.Variant CamIL.Variant::duplicate()
	ret
     NOT_VARIANT:	
// if "arg est un record" appeler sa méthode de duplication ;
	ldarg.0
	isinst CamIL.Record
	brnull NOT_RECORD
	ldarg.0
	castclass CamIL.Record
	tail.
	callvirt instance class CamIL.Record CamIL.Record::duplicate()
	ret
     NOT_RECORD:	
// should duplicate float ? they have a value semantic but are represented as blocks !!
// if "arg est un char[]"
	ldarg.0
        isinst char[]
        brnull NOT_CHARARR
	ldarg.0
	castclass char[]
	dup
	ldlen
	stloc.1
	castclass [mscorlib]System.Array
	ldloc.1
	newarr char
	dup
	stloc.3
	castclass [mscorlib]System.Array
	ldloc.1
	call void [mscorlib]System.Array::Copy(class [mscorlib]System.Array,class [mscorlib]System.Array,int32)
	ldloc.3
	ret
    NOT_CHARARR:	
// if "arg est une string" return new String(x) ;
	ldarg.0
        isinst [mscorlib]System.String
        brnull NOT_STRING
	ldarg.0
	castclass [mscorlib]System.String
	tail.
	call string [mscorlib]System.String::Copy(string)
	ret
     NOT_STRING:
// if "arg est StringBuilder"  return new BuilderString(x.ToString()) ;
	ldarg.0
        isinst [mscorlib]System.Text.StringBuilder
        brnull NOT_BUILDER
	ldarg.0
	castclass [mscorlib]System.Text.StringBuilder
	callvirt instance class [mscorlib]System.String  [mscorlib]System.Text.StringBuilder::ToString()
	newobj instance void [mscorlib]System.Text.StringBuilder::.ctor(class [mscorlib]System.String)
	ret
     NOT_BUILDER:
// if "on a un tableau" then return x
	ldarg.0
	isinst [mscorlib]System.Array
        brtrue X_TTAB 
// ici c'est le cas par defaut, on retourne l'argument lui-meme
	ldarg.0
	ret
// a0 = (objet[]) x 
     X_TTAB:	
	ldarg.0
	castclass class [mscorlib]System.Object[]
	stloc.0
// a1 = length (a0)
	ldloc.0
	ldlen
	stloc.1
// a2 = new Object[a1]
	ldloc.1
	newarr [mscorlib]System.Object
	stloc.2
// while (a1<>0) {
    WHILE_BEGIN:
	ldloc.1
	brfalse WHILE_END
// a1 = a1 - 1
	ldloc.1
	ldc.i4.1
	sub
	stloc.1
// a2[a1]=a0[a1]
	ldloc.2
	ldloc.1
	ldloc.0
	ldloc.1
	ldelem.ref
// DANS LA VERSION INCORRECTE ON APPELAIT ICI RECURSIVEMENT OBJ_DUP ..
	stelem.ref
// }
	br WHILE_BEGIN
    WHILE_END:
	ldloc.2 
	ret
   } 
	
  .method public static class [mscorlib]System.Object block(int32,int32) il managed 
   {
    .maxstack 4
	ldarg.1
	ldc.i4.1
	add
	newarr [mscorlib]System.Object
	dup
	ldarg.1
	ldarg.0
	box [mscorlib]System.Int32
	stelem.ref
	ret
   }

  .method public static void obj_set_tag (class [mscorlib]System.Object,int32) il managed 
   {
// Envoyer une exception ML Invalid_argument en cas de string ....
   .maxstack 3
	ldarg.0
	castclass object[]
	dup
	ldlen
	ldc.i4.1
	sub
	ldarg.1
	box [mscorlib]System.Int32
	stelem.ref
	ret	
   }

  .method public static int32 variant_size (object) il managed 
   {
   .maxstack 1
	ldarg.0
	isinst CamIL.Variant
	brfalse NOTVARIANT
	ldarg.0
	castclass CamIL.Variant
	ldfld int32 CamIL.Variant::size
	ret
NOTVARIANT:
	ldc.i4.M1
	ret
   }

  .method public static void reflection_setfield (object,int32,object) il managed
   {
   .maxstack 3
	ldarg.0
	castclass object[]
	ldarg.1
	ldarg.2
	stelem.ref
	ret
   }

// TODO: test for records and launch Invalid_argument 	
// works for variants and object[] blocks	
  .method public static object reflection_getfield (object,int32) il managed
   {
   .locals init (class CamIL.Variant)
   .maxstack 3
	ldarg.0
	isinst CamIL.Variant
	brfalse NOTVARIANT
	ldarg.0
	castclass CamIL.Variant
	dup
	stloc.0
     	call instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()	
	ldstr "x"
	ldarg.1
	box [mscorlib]System.Int32
	call string [mscorlib]System.String::Concat(object,object)
	callvirt   instance class [mscorlib]System.Reflection.FieldInfo [mscorlib]System.Type::GetField(string)	
	ldloc.0
	callvirt   instance object [mscorlib]System.Reflection.FieldInfo::GetValue(object)
	ret
NOTVARIANT:		
	ldarg.0
	castclass object[]
	ldarg.1
	ldelem.ref
	ret
   }

// works for records
  .method public static object reflection_getrecordfield (object,string) il managed
   {
   .locals init (class CamIL.Record)
   .maxstack 3
	ldarg.0
	castclass CamIL.Record
	dup
	stloc.0
     	call instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()	
	ldarg.1
	callvirt   instance class [mscorlib]System.Reflection.FieldInfo [mscorlib]System.Type::GetField(string)
	ldloc.0
	callvirt   instance object [mscorlib]System.Reflection.FieldInfo::GetValue(object)
	ret
   }

}