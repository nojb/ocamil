/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Raphael Montelatici, Equipe PPS                           //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

.class  public auto ansi Threads extends [mscorlib]System.Object 
 {
  .method public static class [mscorlib]System.Threading.Thread 
     self () il managed
   {
    .maxstack 1
	call class [mscorlib]System.Threading.Thread [mscorlib]System.Threading.Thread::get_CurrentThread()
	ret	
   }

  .method public static string get_name(class [mscorlib]System.Threading.Thread) il managed
   {
    .maxstack 1
	ldarg.0
	call instance string [mscorlib]System.Threading.Thread::get_Name()
	ret
   }

  .method public static void abort(class [mscorlib]System.Threading.Thread) il managed
   {
    .maxstack 1
	ldarg.0
	call instance void [mscorlib]System.Threading.Thread::Abort()
	ret
   }
	
  .method public static void join(class [mscorlib]System.Threading.Thread) il managed
   {
    .maxstack 1
	ldarg.0
	call instance void [mscorlib]System.Threading.Thread::Join()
	ret
   }

	
  .method public static class [mscorlib]System.Threading.Thread create(class CamIL.Closure,string) il managed
   {
    .maxstack 3
//	ldtoken CamIL.Threads
//        call class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)
//	call void [mscorlib]System.Threading.Monitor::Enter(object)
	ldarg.0
	ldftn instance void CamIL.Closure::threading_delegate()
	newobj instance void [mscorlib]System.Threading.ThreadStart::.ctor(object,native int)
	newobj instance void [mscorlib]System.Threading.Thread::.ctor(class [mscorlib]System.Threading.ThreadStart)
	dup
	ldarg.1
	call instance void [mscorlib]System.Threading.Thread::set_Name(string)
	dup
	callvirt   instance void [mscorlib]System.Threading.Thread::Start()
//	ldtoken CamIL.Threads
//       call class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)
//	call void [mscorlib]System.Threading.Monitor::Exit(object)
	ret
   }
	

 .method public static void delay(int32,int32,int32,int32) il managed
   {
    .locals init (valuetype [mscorlib]System.TimeSpan TS)	
    .maxstack 5
	ldloca.s TS
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	call    instance void [mscorlib]System.TimeSpan::.ctor(int32, int32, int32, int32)
	ldloc.0
	call    void [mscorlib]System.Threading.Thread::Sleep(valuetype [mscorlib]System.TimeSpan)
	ret
   }

  .method public static void sleep(class [mscorlib]System.Threading.Thread) il managed
   {
    .maxstack 1
	ldarg.0
	call instance void [mscorlib]System.Threading.Thread::Interrupt()
	ret
   }
	
  .method public static void wakeup(class [mscorlib]System.Threading.Thread) il managed
   {
    .maxstack 1
	ldarg.0
	call instance void [mscorlib]System.Threading.Thread::Resume()
	ret
   }


	
// SECTION MUTEX

  .method public static class [mscorlib]System.Threading.Mutex mutex_create(string) il managed
   {
   .maxstack 2
	ldc.i4.0
	ldarg.0
	newobj instance void [mscorlib]System.Threading.Mutex::.ctor(bool,string)
	ret	
   }	

	
  .method public static int32 mutex_waitone(class [mscorlib]System.Threading.Mutex,int32) il managed
   {
   .maxstack 3
	ldarg.0
	ldarg.1
	ldc.i4.0
	callvirt instance bool [mscorlib]System.Threading.Mutex::WaitOne(int32,bool)
	conv.i4
	ret	
   }
	
  .method public static void mutex_release(class [mscorlib]System.Threading.Mutex) il managed
   {
   .maxstack 1
	ldarg.0
	callvirt instance void [mscorlib]System.Threading.Mutex::ReleaseMutex()
	ret	
   }
	

 }