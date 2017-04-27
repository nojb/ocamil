/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//               Clément Capel, équipe PPS                             //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 


.class public auto ansi Gc_ctrl extends [mscorlib]System.Object
 {

  
.method public static void gc_compaction() il managed
{
  call void [mscorlib]System.GC::Collect()
  ret
}

 }
