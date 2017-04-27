/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//         Clément Capel, Raphael Montelatici, équipe PPS              //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

.class public auto ansi Sys extends [mscorlib]System.Object
 {
  .field static private object[] argv

  .method public static void set_argv(object) il managed
        {
	.locals init(string[],int32) 
	.maxstack 5
.try {
	call string[] [mscorlib]System.Environment::GetCommandLineArgs()
	stloc.0
	leave OK
	}
catch [mscorlib]System.Security.SecurityException {
	pop
	leave NOPERMISSION
	}
OK:		
	ldloc.0
	ldlen
	dup
	stloc.1
	ldc.i4.1
	add
	newarr object
	dup
	ldloc.1
	ldc.i4.0
	box [mscorlib]System.Int32	
	stelem.ref
BEGINLOOP:	
	ldloc.1
	ldc.i4.0
	beq ENDLOOP	
	ldloc.1
	ldc.i4.1
	sub
	stloc.1
	dup
	ldloc.1
	ldloc.0
	ldloc.1
	ldelem.ref
        callvirt instance char[] [mscorlib]System.String::ToCharArray()
	stelem.ref
	br BEGINLOOP	
ENDLOOP:
	stsfld object[] CamIL.Sys::argv
	ret
NOPERMISSION:
	// on met un block conventionnel avec juste un nom d'executable bidon
	ldc.i4.2
	newarr object
	dup
	ldc.i4.1
	ldc.i4.0
	box [mscorlib]System.Int32
	stelem.ref
	dup
	ldc.i4.0
	ldstr "APPLET:NO_PERMISSION"
        callvirt instance char[] [mscorlib]System.String::ToCharArray()
	stelem.ref
	stsfld object[] CamIL.Sys::argv
	ret
	}

	
 .method public static void shift_argv(object) il managed
        {
	//utilisé par le toplevel en exécution de script

	.locals init(object[],int32) 
	.maxstack 7
	ldsfld     object[] Sys.Top::global
	ldc.i4.0
	ldelem.ref
	castclass object[]
	stloc.0
	ldsfld     object[] Sys.Top::global
	ldc.i4.0
	ldloc.0
	ldlen
	dup
	stloc.1
	ldc.i4.1
	sub
	newarr object
BEGINLOOP:	
	ldloc.1
	ldc.i4.1
	beq ENDLOOP	
	ldloc.1
	ldc.i4.1
	sub
	stloc.1
	dup
	ldloc.1
	ldc.i4.1
	sub
	ldloc.0
	ldloc.1
	ldelem.ref
	stelem.ref
	br BEGINLOOP	
ENDLOOP:
	stelem.ref	
	ret
	}

  .method public static string get_environment_variable(string) il managed
	{
	.locals init (string)
	ldarg.0
	call string [mscorlib]System.Environment::GetEnvironmentVariable(string)
	dup
	stloc.0
	brnull ISNULL
	ldloc.0
	ret
ISNULL:
	ldstr ""
	ret
	}
	
  .method public static object[] get_argv() il managed
	{
	ldsfld object[] CamIL.Sys::argv
	ret
	}


  .method public static int32 sys_random_seed() il managed
        {
	.maxstack 1
	newobj instance void [mscorlib]System.Random::.ctor()	
	callvirt instance int32 [mscorlib]System.Random::Next()
	ret 
	}

  .method public static int32 sys_file_exists(string) il managed
        {
	.maxstack 1
	ldarg.0

	call bool [mscorlib]System.IO.File::Exists(string)
	call int32 [mscorlib]System.Convert::ToInt32(bool)
	ret
	}

  .method public static void sys_remove(string) il managed
        {
	
	.maxstack 2
	ldarg.0
	call  int32 CamIL.Sys::sys_file_exists(string)
	brtrue REMOVE
	ldarg.0
	ldstr ": No such file or directory"
	call string [mscorlib]System.String::Concat(class [mscorlib]System.Object,class [mscorlib]System.Object)
	call void CamIL.Exception::Sys_error(class [mscorlib]System.String)
REMOVE:
	ldarg.0

	call void [mscorlib]System.IO.File::Delete(string)
	ret
	}

	
.method public static void sys_rename(string,string) il managed
        {
	.maxstack 2
	ldarg.0
	call  int32 CamIL.Sys::sys_file_exists(string)
	brtrue RENAME
	ldarg.0
	ldstr ": No such file or directory"
	call string [mscorlib]System.String::Concat(class [mscorlib]System.Object,class [mscorlib]System.Object)
	call void CamIL.Exception::Sys_error(class [mscorlib]System.String)
RENAME:
	ldarg.0

	ldarg.1

	call void [mscorlib]System.IO.File::Move(string,string)
	ret
	}


.method public static void sys_chdir(string) il managed
        {
	.maxstack 2
	ldarg.0

	call bool [mscorlib]System.IO.Directory::Exists(string)
	brtrue CHDIR
	ldarg.0
	ldstr ": No such file or directory"
	call string [mscorlib]System.String::Concat(class [mscorlib]System.Object,class [mscorlib]System.Object)
	call void CamIL.Exception::Sys_error(class [mscorlib]System.String)
CHDIR:
	ldarg.0

	call void [mscorlib]System.Environment::set_CurrentDirectory(string)
	ret
	}


.method public static string sys_getcwd() il managed
        {
	.maxstack 1
	call string [mscorlib]System.Environment::get_CurrentDirectory()
	ret
	}


.method public static int32 sys_system_command(string) il managed
        {
	.maxstack 4
	.locals init (class [System]System.Text.RegularExpressions.Match,string,
			class [System]System.Diagnostics.ProcessStartInfo,string,int32)

	.try {
	ldstr      "\\s*([\\\\|\\w|:|\/|\\.|_|\\$]+)\\s*([^>]*)(>\\s*(?'out'.*))?"
	ldc.i4.1
	newobj     instance void [System]System.Text.RegularExpressions.Regex::.ctor(string,  valuetype [System]System.Text.RegularExpressions.RegexOptions)
	ldarg.0
	callvirt   instance class [System]System.Text.RegularExpressions.Match [System]System.Text.RegularExpressions.Regex::Match(string)
        stloc.0

	ldloc.0
	ldstr      "$1"
	callvirt   instance string [System]System.Text.RegularExpressions.Match::Result(string)
	ldstr      "\\ "
	ldstr      " "
	callvirt   instance string [mscorlib]System.String::Replace(string, string)

	ldloc.0
	ldstr      "$2"
	callvirt   instance string [System]System.Text.RegularExpressions.Match::Result(string)
	ldstr      "\\ "
	ldstr      " "
	callvirt   instance string [mscorlib]System.String::Replace(string, string)
	
	newobj instance void [System]System.Diagnostics.ProcessStartInfo::.ctor(string,  string)
	dup
	ldc.i4.0 //false
	callvirt instance void 	[System]System.Diagnostics.ProcessStartInfo::set_UseShellExecute(bool)
	stloc.2

	newobj	instance void [System]System.Diagnostics.Process::.ctor()
	
	// rediriger éventuellement la sortie si on a matché un '>'
	ldloc.0
	ldstr      "${out}"
	callvirt   instance string [System]System.Text.RegularExpressions.Match::Result(string)
	stloc.1
	ldloc.1
	ldstr ""
	call bool [mscorlib]System.String::Equals(string,string)
	brtrue NOREDIRECT
	
	ldloc.2
	ldc.i4.1 //true
	callvirt   instance void [System]System.Diagnostics.ProcessStartInfo::set_RedirectStandardOutput(bool)

	dup
	ldloc.2
	callvirt instance void [System]System.Diagnostics.Process::set_StartInfo(class [System]System.Diagnostics.ProcessStartInfo)
	dup
	callvirt  instance bool [System]System.Diagnostics.Process::Start()
	pop
	dup
	callvirt  instance void [System]System.Diagnostics.Process::WaitForExit()
	dup
	callvirt instance class [mscorlib]System.IO.StreamReader [System]System.Diagnostics.Process::get_StandardOutput()
	callvirt instance string [mscorlib]System.IO.StreamReader::ReadToEnd()
	stloc.3
	
	ldloc.1
	newobj instance void [mscorlib]System.IO.StreamWriter::.ctor(string)
	dup
	ldloc.3
	callvirt instance void [mscorlib]System.IO.StreamWriter::Write(string)
	callvirt instance void [mscorlib]System.IO.StreamWriter::Close()
	
	callvirt   instance int32 [System]System.Diagnostics.Process::get_ExitCode()
	stloc 4
	leave END
NOREDIRECT:
	dup
	ldloc.2
	callvirt instance void [System]System.Diagnostics.Process::set_StartInfo(class [System]System.Diagnostics.ProcessStartInfo)
	dup
	callvirt  instance bool [System]System.Diagnostics.Process::Start()
	pop
	dup
	callvirt  instance void [System]System.Diagnostics.Process::WaitForExit()
	callvirt  instance int32 [System]System.Diagnostics.Process::get_ExitCode()
	stloc 4
	leave END
	}
	catch [mscorlib]System.Exception {
	pop
	ldc.i4.2
	stloc 4
	leave END
	}
END:	ldloc 4
	ret
	}

	
.method public static float64 sys_time() il managed
        {  
	.maxstack  2
	.locals init (valuetype [mscorlib]System.TimeSpan time)
	
	call class [System]System.Diagnostics.Process [System]System.Diagnostics.Process::GetCurrentProcess()
	callvirt instance valuetype [mscorlib]System.TimeSpan [System]System.Diagnostics.Process::get_TotalProcessorTime()
	stloc.s time
	ldloca.s time
	call instance float64 [mscorlib]System.TimeSpan::get_TotalSeconds()
	ret
	}

//Ajout du 19/05 pour l'equivalent de Unix.gettimeofday
//Rajouter les jours etc... et depuis 1970
.method public static float64 sys_timeofday(object) il managed {
  .locals init (valuetype [mscorlib]System.DateTime V_0)
    call       valuetype [mscorlib]System.DateTime [mscorlib]System.DateTime::get_Now()
    stloc.0
    ldloca.s   V_0
    call       instance int32 [mscorlib]System.DateTime::get_Hour()
    conv.r8
    ldc.r8     3600.
    mul
    ldloca.s   V_0
    call       instance int32 [mscorlib]System.DateTime::get_Minute()
    conv.r8
    ldc.r8     60.
    mul
    add
    ldloca.s   V_0
    call       instance int32 [mscorlib]System.DateTime::get_Millisecond()
    conv.r8
    ldc.r8     1000.
    div
    add
    ldloca.s   V_0
    call       instance int32 [mscorlib]System.DateTime::get_Second()
    conv.r8
    add	
    ret
}
	
	


.method public static class [mscorlib]System.Object[]  sys_read_directory(string) il managed
        {  
	.maxstack  4
	.locals init (string[], object[], int32)
	ldarg.0

	call bool [mscorlib]System.IO.Directory::Exists(string)
	brtrue READ
	ldarg.0
	ldstr ": No such file or directory"
	call string [mscorlib]System.String::Concat(class [mscorlib]System.Object,class [mscorlib]System.Object)
	call void CamIL.Exception::Sys_error(class [mscorlib]System.String)
READ:
	ldarg.0
	call  string[] [mscorlib]System.IO.Directory::GetFiles(string)
	stloc.0
	ldloc.0
	ldlen
	ldc.i4.1 //place pour le tag
	add
	newarr [mscorlib]System.Object
	stloc.1
	ldc.i4.0
	stloc.2
	br.s START_LOOP
LOOP:	
	ldloc.1
	ldloc.2
	ldloc.0
	ldloc.2
	ldelem.ref
        callvirt instance char[] [mscorlib]System.String::ToCharArray()
	stelem.ref
	ldloc.2
	ldc.i4.1
	add
	stloc.2
START_LOOP:	
	ldloc.2
	ldloc.0
	ldlen
	conv.i4
	blt.s LOOP
	ldloc.1
	
	dup
	ldloc.2
	ldc.i4.0
	box [mscorlib]System.Int32
	stelem.ref
	
	ret
	}







	
 }