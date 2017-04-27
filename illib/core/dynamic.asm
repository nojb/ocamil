.class public auto ansi Dynamic extends [mscorlib]System.Object 
 {

 .field static public class [mscorlib]System.Reflection.Emit.AssemblyBuilder current_assemblybuilder
 .field static public class [mscorlib]System.Reflection.Assembly current_assembly
 .field static public class [mscorlib]System.Reflection.Emit.ModuleBuilder current_modulebuilder

 .method public static void init_generator (string namespace) il managed
  {
	call       class [mscorlib]System.AppDomain [mscorlib]System.Threading.Thread::GetDomain()	
	newobj     instance void [mscorlib]System.Reflection.AssemblyName::.ctor()
	dup
	ldarg.0
	callvirt   instance void [mscorlib]System.Reflection.AssemblyName::set_Name(string)
	ldc.i4.3 // Valuetype AssemblyAccess.RunAndSave
	callvirt   instance class [mscorlib]System.Reflection.Emit.AssemblyBuilder [mscorlib]System.AppDomain::DefineDynamicAssembly(class [mscorlib]System.Reflection.AssemblyName,valuetype [mscorlib]System.Reflection.Emit.AssemblyBuilderAccess)
	dup
	stsfld class [mscorlib]System.Reflection.Emit.AssemblyBuilder CamIL.Dynamic::current_assemblybuilder
	ldstr      "EmittedModule"
	ldarg.0
	ldstr ".dll"
	call string [mscorlib]System.String::Concat(string,string)
	callvirt   instance class [mscorlib]System.Reflection.Emit.ModuleBuilder [mscorlib]System.Reflection.Emit.AssemblyBuilder::DefineDynamicModule(string,string)
	stsfld class [mscorlib]System.Reflection.Emit.ModuleBuilder CamIL.Dynamic::current_modulebuilder
	ret
  }

	
 .method public static void save_dll (string filename) il managed
  {
	ldsfld class [mscorlib]System.Reflection.Emit.AssemblyBuilder CamIL.Dynamic::current_assemblybuilder
	ldarg.0
	callvirt   instance void [mscorlib]System.Reflection.Emit.AssemblyBuilder::Save(string)
	ret
  }		
	

// .method public static class [mscorlib]System.Type gen_type () il managed
//  {


//  }
	
	
	
 .method public static void toplevel_phrase_noarg (string,string,string)
     il managed 
   {
	.locals init (class [mscorlib]System.Type, object, class [mscorlib]System.Reflection.Assembly)
	
// Load DLL, create instance and type reference
	ldarg.0
	call class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::LoadFrom(string)
	stloc.2
	ldloc.2
	ldarg.1
	callvirt instance class [mscorlib]System.Type [mscorlib]System.Reflection.Assembly::GetType(string)
	stloc.0
// Invoke method
.try{
	ldloc.0
	ldarg.2
	ldc.i4 0x100 //System.Reflection.BindingFlags::InvokeMethod
	ldnull
	//ldloc.1
	ldnull
	ldc.i4.0
	newarr object
	callvirt instance object [mscorlib]System.Type::InvokeMember(string,valuetype [mscorlib]System.Reflection.BindingFlags,class [mscorlib]System.Reflection.Binder,object,object[])
	pop
	leave END
}
	catch [mscorlib]System.Reflection.TargetInvocationException {
	callvirt instance class [mscorlib]System.Exception [mscorlib]System.Reflection.TargetInvocationException::get_InnerException()
	throw
	leave END
	}
	
END:	ret
   }

		
 .method public static void toplevel_phrase_onearg (string,string,string,object)
     il managed 
   {
	.maxstack 9
	.locals init (class [mscorlib]System.Type, object, class [mscorlib]System.Reflection.Assembly)
	
// Load DLL, create instance and type reference
	ldarg.0
	call class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::LoadFrom(string)
	stloc.2
	ldloc.2
	ldarg.1
	callvirt instance class [mscorlib]System.Type [mscorlib]System.Reflection.Assembly::GetType(string)
	stloc.0
// Invoke method
.try{
	ldloc.0
	ldarg.2
	ldc.i4 0x100 //System.Reflection.BindingFlags::InvokeMethod
	ldnull
	//ldloc.1
	ldnull
	ldc.i4.1
	newarr object
	dup
	ldc.i4.0
	ldarg.3
	stelem.ref
	callvirt instance object [mscorlib]System.Type::InvokeMember(string,valuetype [mscorlib]System.Reflection.BindingFlags,class [mscorlib]System.Reflection.Binder,object,object[])
	pop
	leave END
}
	catch [mscorlib]System.Reflection.TargetInvocationException {
	callvirt instance class [mscorlib]System.Exception [mscorlib]System.Reflection.TargetInvocationException::get_InnerException()
	throw
	leave END
	}
	
END:	ret
   }



	////////

		
	
 .method public static void test_toplevel_phrase_noarg (class [mscorlib]System.Reflection.Assembly,string,string)
     il managed 
   {
	.locals init (class [mscorlib]System.Type, object, class [mscorlib]System.Reflection.Assembly)
	
// Load DLL, create instance and type reference
	ldarg.0
//	call class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::LoadFrom(string)
	stloc.2
	ldloc.2
	ldarg.1
	callvirt instance class [mscorlib]System.Type [mscorlib]System.Reflection.Assembly::GetType(string)
	stloc.0
// Invoke method
.try{
	ldloc.0
	ldarg.2
	ldc.i4 0x100 //System.Reflection.BindingFlags::InvokeMethod
	ldnull
	//ldloc.1
	ldnull
	ldc.i4.0
	newarr object
	callvirt instance object [mscorlib]System.Type::InvokeMember(string,valuetype [mscorlib]System.Reflection.BindingFlags,class [mscorlib]System.Reflection.Binder,object,object[])
	pop
	leave END
}
	catch [mscorlib]System.Reflection.TargetInvocationException {
	callvirt instance class [mscorlib]System.Exception [mscorlib]System.Reflection.TargetInvocationException::get_InnerException()
	throw
	leave END
	}
	
END:	ret
   }

		
 .method public static void test_toplevel_phrase_onearg (class [mscorlib]System.Reflection.Assembly,string,string,object)
     il managed 
   {
	.maxstack 9
	.locals init (class [mscorlib]System.Type, object, class [mscorlib]System.Reflection.Assembly)
	
// Load DLL, create instance and type reference
	ldarg.0
//	call class [mscorlib]System.Reflection.Assembly [mscorlib]System.Reflection.Assembly::LoadFrom(string)
	stloc.2
	ldloc.2
	ldarg.1
	callvirt instance class [mscorlib]System.Type [mscorlib]System.Reflection.Assembly::GetType(string)
	stloc.0
// Invoke method
.try{
	ldloc.0
	ldarg.2
	ldc.i4 0x100 //System.Reflection.BindingFlags::InvokeMethod
	ldnull
	//ldloc.1
	ldnull
	ldc.i4.1
	newarr object
	dup
	ldc.i4.0
	ldarg.3
	stelem.ref
	callvirt instance object [mscorlib]System.Type::InvokeMember(string,valuetype [mscorlib]System.Reflection.BindingFlags,class [mscorlib]System.Reflection.Binder,object,object[])
	pop
	leave END
}
	catch [mscorlib]System.Reflection.TargetInvocationException {
	callvirt instance class [mscorlib]System.Exception [mscorlib]System.Reflection.TargetInvocationException::get_InnerException()
	throw
	leave END
	}
	
END:	ret
   }

	
	

	
		
	
}