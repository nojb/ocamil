
  .class public auto ansi beforefieldinit Graphics
         extends [System.Windows.Forms]System.Windows.Forms.Control
  {
    .class sequential ansi sealed nested public beforefieldinit state
           extends [mscorlib]System.ValueType
    {
      .field public int32 X
      .field public int32 Y
      .field public bool Clic
      .method public hidebysig specialname rtspecialname 
              instance void  .ctor(int32 x,
                                   int32 y,
                                   bool clic) cil managed
      {
        // Code size       22 (0x16)
        .maxstack  2
        IL_0000:  ldarg.0
        IL_0001:  ldarg.1
        IL_0002:  stfld      int32 CamIL.Graphics/state::X
        IL_0007:  ldarg.0
        IL_0008:  ldarg.2
        IL_0009:  stfld      int32 CamIL.Graphics/state::Y
        IL_000e:  ldarg.0
        IL_000f:  ldarg.3
        IL_0010:  stfld      bool CamIL.Graphics/state::Clic
        IL_0015:  ret
      } // end of method state::.ctor

    } // end of class state

    .class sequential ansi sealed nested private beforefieldinit evenement
           extends [mscorlib]System.ValueType
    {
      .field public int32 kind
      .field public int32 mouse_x
      .field public int32 mouse_y
      .field public bool button
      .field public int32 key
      .method public hidebysig specialname rtspecialname 
              instance void  .ctor(int32 kind,
                                   int32 mouse_x,
                                   int32 mouse_y,
                                   bool button,
                                   int32 key) cil managed
      {
        // Code size       38 (0x26)
        .maxstack  2
        IL_0000:  ldarg.0
        IL_0001:  ldarg.1
        IL_0002:  stfld      int32 CamIL.Graphics/evenement::kind
        IL_0007:  ldarg.0
        IL_0008:  ldarg.2
        IL_0009:  stfld      int32 CamIL.Graphics/evenement::mouse_x
        IL_000e:  ldarg.0
        IL_000f:  ldarg.3
        IL_0010:  stfld      int32 CamIL.Graphics/evenement::mouse_y
        IL_0015:  ldarg.0
        IL_0016:  ldarg.s    button
        IL_0018:  stfld      bool CamIL.Graphics/evenement::button
        IL_001d:  ldarg.0
        IL_001e:  ldarg.s    key
        IL_0020:  stfld      int32 CamIL.Graphics/evenement::key
        IL_0025:  ret
      } // end of method evenement::.ctor

    } // end of class evenement

    .field public static bool open
    .field public static class CamIL.Graphics g
    .field public static class [System.Windows.Forms]System.Windows.Forms.Form gform
    .field public static class [System.Drawing]System.Drawing.Graphics gc
    .field public static class [System.Drawing]System.Drawing.Graphics buffer
    .field public static class [System.Drawing]System.Drawing.Graphics buffer2
    .field public static class [System.Drawing]System.Drawing.Bitmap im
    .field public static class [System.Drawing]System.Drawing.Bitmap im2
    .field public static int32 c_x
    .field public static int32 c_y
    .field public static string title
    .field public static class [System.Drawing]System.Drawing.Font font
    .field public static class [System.Drawing]System.Drawing.SolidBrush brush
    .field public static class [System.Drawing]System.Drawing.Pen pen
    .field private static valuetype CamIL.Graphics/state current_state
    .field private static object current_state_lock
    .field private static int32 size_queue
    .field private static valuetype CamIL.Graphics/evenement[] gr_queue
    .field private static int32 gr_head
    .field private static int32 gr_tail
    .field private static bool display_mode
    .field private static int32 display_synchronize_lag
    .field private static int32 display_synchronize_lag_counter

	
    .method public hidebysig static void  start_frame() cil managed
    {
      // Code size       11 (0xb)
      .maxstack  1
                ldsfld     class [System.Windows.Forms]System.Windows.Forms.Form CamIL.Graphics::gform
		call       void [System.Windows.Forms]System.Windows.Forms.Application::Run(class [System.Windows.Forms]System.Windows.Forms.Form)
		ret
    } // end of method Graphics::start_frame

    .method public hidebysig static void  gr_display_mode(bool b) cil managed
    {
      .maxstack  1
      ldarg.0
      stsfld bool CamIL.Graphics::display_mode
      ret
    }

    .method public hidebysig static void  gr_display_synchronize_lag(int32 x) cil managed
    {
      .maxstack  1
      ldarg.0
      stsfld int32 CamIL.Graphics::display_synchronize_lag
      ret
    }
    
    .method public hidebysig static void  gr_synchronize() cil managed
    {
	.locals init (class [System.Drawing]System.Drawing.Bitmap)
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	ldsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im2
	ldc.i4.0
	ldc.i4.0
	callvirt   instance void [System.Drawing]System.Drawing.Graphics::DrawImageUnscaled(class [System.Drawing]System.Drawing.Image,int32,int32)

	ldsfld int32 CamIL.Graphics::display_synchronize_lag_counter
	ldc.i4.1
	add
	ldsfld int32 CamIL.Graphics::display_synchronize_lag
	rem
	dup
	stsfld int32 CamIL.Graphics::display_synchronize_lag_counter		

	brtrue LAG	
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	ldsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im2
	ldc.i4.0
	ldc.i4.0
	callvirt   instance void [System.Drawing]System.Drawing.Graphics::DrawImageUnscaled(class [System.Drawing]System.Drawing.Image,int32,int32)
LAG:	
	ret
    }
	
    .method public hidebysig static void 
            gr_open_graph(string s) cil managed
    {
      // Code size       456 (0x1c8)
      .maxstack  4
      .locals init (class [System]System.Text.RegularExpressions.Regex V_0,
               int32 V_1,
               int32 V_2,
               int32 V_3,
               int32 V_4,
               valuetype [System.Drawing]System.Drawing.Color V_5,
               class [mscorlib]System.Threading.Thread V_6,
               valuetype [System.Drawing]System.Drawing.Size V_7,
               class CamIL.Graphics V_8)
      IL_0000:  ldsfld     class [System.Windows.Forms]System.Windows.Forms.Form CamIL.Graphics::gform
      IL_0005:  brtrue    IL_00c5 

      IL_0007:  newobj     instance void CamIL.Graphics::.ctor()
      IL_000c:  stsfld     class CamIL.Graphics CamIL.Graphics::g
                newobj     instance void [System.Windows.Forms]System.Windows.Forms.Form::.ctor()
		dup
	        stsfld     class [System.Windows.Forms]System.Windows.Forms.Form CamIL.Graphics::gform
	        call       instance class [System.Windows.Forms]System.Windows.Forms.Control/ControlCollection [System.Windows.Forms]System.Windows.Forms.Control::get_Controls()
  	        ldsfld     class CamIL.Graphics CamIL.Graphics::g
		callvirt   instance void [System.Windows.Forms]System.Windows.Forms.Control/ControlCollection::Add(class [System.Windows.Forms]System.Windows.Forms.Control)
// Analyse de la chaine argument
      IL_0011:  ldstr      "( ((\\d+)\\*(\\d+))\?(\\+(\\d+)\\-(\\d+))\?)\?"
      IL_0016:  newobj     instance void [System]System.Text.RegularExpressions.Regex::.ctor(string)
      IL_001b:  stloc.0
// Dimensions de la fenetre
      .try
      {
        IL_001c:  ldloc.0
        IL_001d:  ldarg.0
        IL_0023:  callvirt   instance class [System]System.Text.RegularExpressions.Match [System]System.Text.RegularExpressions.Regex::Match(string)
        IL_0028:  ldstr      "$3"
        IL_002d:  callvirt   instance string [System]System.Text.RegularExpressions.Match::Result(string)
        IL_0032:  call       int32 [mscorlib]System.Convert::ToInt32(string)
        IL_0037:  stloc.1
        IL_0038:  ldloc.0
        IL_0039:  ldarg.0
        IL_003f:  callvirt   instance class [System]System.Text.RegularExpressions.Match [System]System.Text.RegularExpressions.Regex::Match(string)
        IL_0044:  ldstr      "$4"
        IL_0049:  callvirt   instance string [System]System.Text.RegularExpressions.Match::Result(string)
        IL_004e:  call       int32 [mscorlib]System.Convert::ToInt32(string)
        IL_0053:  stloc.2
        IL_0054:  ldsfld     class [System.Windows.Forms]System.Windows.Forms.Form CamIL.Graphics::gform
	// form should be bigger because of place taken by its decoration
        IL_0059:  ldloc.1
	  ldc.i4 8
	  add
        IL_005a:  ldloc.2
	  ldc.i4 34
	  add
        IL_005b:  newobj     instance void [System.Drawing]System.Drawing.Size::.ctor(int32,
                                                                                      int32)
        IL_0060:  callvirt   instance void [System.Windows.Forms]System.Windows.Forms.Form::set_Size(valuetype [System.Drawing]System.Drawing.Size)
          ldsfld     class CamIL.Graphics CamIL.Graphics::g
          ldloc.1
          ldloc.2
          newobj     instance void [System.Drawing]System.Drawing.Size::.ctor(int32,int32)
          callvirt   instance void [System.Windows.Forms]System.Windows.Forms.Control::set_Size(valuetype [System.Drawing]System.Drawing.Size)
	
        IL_0065:  leave.s    IL_006a

      }  // end .try
      catch [mscorlib]System.FormatException 
      {
        IL_0067:  pop
        IL_0068:  leave.s    IL_006a

      }  // end handler

// Positionnement de la fenetre	
      .try
      {
        IL_006a:  ldloc.0
        IL_006b:  ldarg.0
        IL_0071:  callvirt   instance class [System]System.Text.RegularExpressions.Match [System]System.Text.RegularExpressions.Regex::Match(string)
        IL_0076:  ldstr      "$6"
        IL_007b:  callvirt   instance string [System]System.Text.RegularExpressions.Match::Result(string)
        IL_0080:  call       int32 [mscorlib]System.Convert::ToInt32(string)
        IL_0085:  stloc.3
        IL_0086:  ldloc.0
        IL_0087:  ldarg.0
        IL_008d:  callvirt   instance class [System]System.Text.RegularExpressions.Match [System]System.Text.RegularExpressions.Regex::Match(string)
        IL_0092:  ldstr      "$7"
        IL_0097:  callvirt   instance string [System]System.Text.RegularExpressions.Match::Result(string)
        IL_009c:  call       int32 [mscorlib]System.Convert::ToInt32(string)
        IL_00a1:  stloc.s    V_4
        IL_00a3:  ldsfld     class [System.Windows.Forms]System.Windows.Forms.Form  CamIL.Graphics::gform
        IL_00a8:  ldloc.3
        IL_00a9:  ldloc.s    V_4
        IL_00ab:  newobj     instance void [System.Drawing]System.Drawing.Point::.ctor(int32,
                                                                                       int32)
        IL_00b0:  callvirt   instance void [System.Windows.Forms]System.Windows.Forms.Form::set_DesktopLocation(valuetype [System.Drawing]System.Drawing.Point)
        IL_00b5:  ldsfld     class [System.Windows.Forms]System.Windows.Forms.Form CamIL.Graphics::gform
        IL_00ba:  ldc.i4.0
        IL_00bb:  callvirt   instance void [System.Windows.Forms]System.Windows.Forms.Form::set_StartPosition(valuetype [System.Windows.Forms]System.Windows.Forms.FormStartPosition)
        IL_00c0:  leave.s    IL_00c5

      }  // end .try
      catch [mscorlib]System.FormatException 
      {
        IL_00c2:  pop
        IL_00c3:  leave.s    IL_00c5

      }  // end handler
      IL_00c5:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_00ca:  call       valuetype [System.Drawing]System.Drawing.Color [System.Drawing]System.Drawing.Color::get_White()
      IL_00cf:  callvirt   instance void [System.Windows.Forms]System.Windows.Forms.Control::set_BackColor(valuetype [System.Drawing]System.Drawing.Color)
      IL_00d4:  ldc.i4.0
      IL_00d5:  stsfld     int32 CamIL.Graphics::c_x
      IL_00da:  ldc.i4.0
      IL_00db:  stsfld     int32 CamIL.Graphics::c_y
      IL_00e0:  call       valuetype [System.Drawing]System.Drawing.Color [System.Drawing]System.Drawing.Color::get_Black()
      IL_00e5:  stloc.s    V_5
      IL_00e7:  ldstr      "Arial"
      IL_00ec:  ldc.r4     8.
      IL_00f1:  newobj     instance void [System.Drawing]System.Drawing.Font::.ctor(string,
                                                                                    float32)
      IL_00f6:  stsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      IL_00fb:  ldloc.s    V_5
      IL_00fd:  newobj     instance void [System.Drawing]System.Drawing.SolidBrush::.ctor(valuetype [System.Drawing]System.Drawing.Color)
      IL_0102:  stsfld     class [System.Drawing]System.Drawing.SolidBrush CamIL.Graphics::brush
      IL_0107:  ldloc.s    V_5
      IL_0109:  ldc.r4     1.
      IL_010e:  newobj     instance void [System.Drawing]System.Drawing.Pen::.ctor(valuetype [System.Drawing]System.Drawing.Color,
                                                                                   float32)
      IL_0113:  stsfld     class [System.Drawing]System.Drawing.Pen CamIL.Graphics::pen
// Mise en place du titre
      IL_0118:  ldsfld     string CamIL.Graphics::title
      IL_011d:  brtrue.s   IL_0130
      IL_011f:  ldstr      "Caml graphics"
      IL_0129:  call       void CamIL.Graphics::gr_set_window_title(string)
      IL_012e:  br.s       IL_013a
      IL_0130:  ldsfld     string CamIL.Graphics::title
      IL_0135:  call       void CamIL.Graphics::gr_set_window_title(string)

// Initialisation du thread de la fenetre pour start_frame
		ldsfld     class [System.Windows.Forms]System.Windows.Forms.Form CamIL.Graphics::gform
		brnull NO_STARTFRAME_1
      IL_013a:  ldnull
      IL_013b:  ldftn      void CamIL.Graphics::start_frame()
      IL_0141:  newobj     instance void [mscorlib]System.Threading.ThreadStart::.ctor(object,
                                                                                       native int)
      IL_0146:  newobj     instance void [mscorlib]System.Threading.Thread::.ctor(class [mscorlib]System.Threading.ThreadStart)
      IL_014b:  stloc.s    V_6
NO_STARTFRAME_1:	
// Default values for display_mode
		ldc.i4.1
		stsfld bool CamIL.Graphics::display_mode
		ldc.i4 10
		stsfld int32 CamIL.Graphics::display_synchronize_lag
		ldc.i4.0
		stsfld int32 CamIL.Graphics::display_synchronize_lag_counter
		
// Initialisation d'un bitmap de la bonne taille et creation des buffer
      IL_014d:  call       valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.SystemInformation::get_PrimaryMonitorMaximizedWindowSize()
      IL_0152:  stloc.s    V_7
      IL_0154:  ldloca.s   V_7
      IL_0156:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Width()
      IL_015b:  ldloca.s   V_7
      IL_015d:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_0162:  newobj     instance void [System.Drawing]System.Drawing.Bitmap::.ctor(int32,
                                                                                      int32)
      IL_0167:  stsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im
      IL_016c:  ldsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im
      IL_0171:  call       class [System.Drawing]System.Drawing.Graphics [System.Drawing]System.Drawing.Graphics::FromImage(class [System.Drawing]System.Drawing.Image)
      IL_0176:  stsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer

		ldloca.s   V_7
		call       instance int32 [System.Drawing]System.Drawing.Size::get_Width()
		ldloca.s   V_7
		call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
		newobj     instance void [System.Drawing]System.Drawing.Bitmap::.ctor(int32,
                                                                                      int32)
		stsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im2
		ldsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im2
		call       class [System.Drawing]System.Drawing.Graphics [System.Drawing]System.Drawing.Graphics::FromImage(class [System.Drawing]System.Drawing.Image)
		stsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
// Lancement du thread (start_frame)
		ldsfld     class [System.Windows.Forms]System.Windows.Forms.Form CamIL.Graphics::gform
		brnull NO_STARTFRAME_2
      IL_017b:  ldloc.s    V_6
      IL_017d:  callvirt   instance void [mscorlib]System.Threading.Thread::Start()
      IL_0182:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0187:  dup
      IL_0188:  stloc.s    V_8
      IL_018a:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_018f:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
        IL_0194:  call       bool [mscorlib]System.Threading.Monitor::Wait(object)
        IL_0199:  pop
        IL_019a:  leave.s    IL_01a4

      }  // end .try
      finally
      {
        IL_019c:  ldloc.s    V_8
        IL_019e:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_01a3:  endfinally
      }  // end handler
NO_STARTFRAME_2:	
// Creation de l'objet Graphics sous-jacent	
      IL_01a4:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
		callvirt instance class [System.Drawing]System.Drawing.Graphics [System.Windows.Forms]System.Windows.Forms.Control::CreateGraphics()
      IL_01b3:  stsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
      IL_01b8:  call       void CamIL.Graphics::gr_clear_graph()
      IL_01bd:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_01c2:  callvirt   instance void [System.Windows.Forms]System.Windows.Forms.Control::Refresh()
      IL_01c7:  ret
    } // end of method Graphics::gr_open_graph

    .method public hidebysig static void 
            gr_close_graph() cil managed
    {
      // Code size       11 (0xb)
      .maxstack  1
		ldsfld class [System.Windows.Forms]System.Windows.Forms.Form CamIL.Graphics::gform
		brnull NOFORM
		ldsfld class [System.Windows.Forms]System.Windows.Forms.Form CamIL.Graphics::gform
		callvirt   instance void [System.Windows.Forms]System.Windows.Forms.Form::Close()
		ldnull
		stsfld class [System.Windows.Forms]System.Windows.Forms.Form CamIL.Graphics::gform
NOFORM:	
		ret
    } // end of method Graphics::gr_close_graph

    .method public hidebysig static int32 
            gr_current_x() cil managed
    {
      // Code size       10 (0xa)
      .maxstack  1
      .locals init (int32 V_0)
      IL_0000:  ldsfld     int32 CamIL.Graphics::c_x
      IL_0005:  stloc.0
      IL_0006:  br.s       IL_0008

      IL_0008:  ldloc.0
      IL_0009:  ret
    } // end of method Graphics::gr_current_x

    .method public hidebysig static int32 
            gr_current_y() cil managed
    {
      // Code size       10 (0xa)
      .maxstack  1
      .locals init (int32 V_0)
      IL_0000:  ldsfld     int32 CamIL.Graphics::c_y
      IL_0005:  stloc.0
      IL_0006:  br.s       IL_0008

      IL_0008:  ldloc.0
      IL_0009:  ret
    } // end of method Graphics::gr_current_y

    .method family hidebysig virtual instance void 
            OnPaint(class [System.Windows.Forms]System.Windows.Forms.PaintEventArgs e) cil managed
    {
      // Code size       51 (0x33)
      .maxstack  5
      .locals init (class [System.Drawing]System.Drawing.Bitmap V_0)

      IL_0000:  ldsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im
      IL_0005:  dup
      IL_0006:  stloc.0
      IL_0007:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_000c:  ldarg.1
        IL_000d:  callvirt   instance class [System.Drawing]System.Drawing.Graphics [System.Windows.Forms]System.Windows.Forms.PaintEventArgs::get_Graphics()
        IL_0012:  ldsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im
        IL_0017:  ldarg.1
        IL_0018:  callvirt   instance valuetype [System.Drawing]System.Drawing.Rectangle [System.Windows.Forms]System.Windows.Forms.PaintEventArgs::get_ClipRectangle()
        IL_001d:  dup
        IL_0023:  ldc.i4.2
        IL_0024:  callvirt   instance void [System.Drawing]System.Drawing.Graphics::DrawImage(class [System.Drawing]System.Drawing.Image,
                                                                                              valuetype [System.Drawing]System.Drawing.Rectangle,
                                                                                              valuetype [System.Drawing]System.Drawing.Rectangle,
                                                                                              valuetype [System.Drawing]System.Drawing.GraphicsUnit)
        IL_0029:  leave.s    IL_0032

      }  // end .try
      finally
      {
        IL_002b:  ldloc.0
        IL_002c:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_0031:  endfinally
      }  // end handler

      IL_0032:  ret
    } // end of method Graphics::OnPaint

    .method family hidebysig virtual instance void 
            OnHandleCreated(class [mscorlib]System.EventArgs e) cil managed
    {
      // Code size       32 (0x20)
      .maxstack  2
      .locals init (class CamIL.Graphics V_0)
      IL_0000:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0005:  dup
      IL_0006:  stloc.0
      IL_0007:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_000c:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
        IL_0011:  call       void [mscorlib]System.Threading.Monitor::PulseAll(object)
        IL_0016:  leave.s    IL_001f

      }  // end .try
      finally
      {
        IL_0018:  ldloc.0
        IL_0019:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_001e:  endfinally
      }  // end handler
      IL_001f:  ret
    } // end of method Graphics::OnHandleCreated


		
		
	
    .method private hidebysig specialname rtspecialname static 
            void  .cctor() cil managed
    {
      // Code size       67 (0x43)
      .maxstack  4
      IL_0000:  ldc.i4.0
      IL_0001:  stsfld     bool CamIL.Graphics::open
      IL_0006:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
      IL_000b:  ldc.i4.0
      IL_000c:  ldc.i4.0
      IL_000d:  ldc.i4.0
      IL_000e:  call       instance void CamIL.Graphics/state::.ctor(int32,
                                                                     int32,
                                                                     bool)
      IL_0013:  newobj     instance void [mscorlib]System.Object::.ctor()
      IL_0018:  stsfld     object CamIL.Graphics::current_state_lock
      IL_001d:  ldc.i4     0x100
      IL_0022:  stsfld     int32 CamIL.Graphics::size_queue
      IL_0027:  ldsfld     int32 CamIL.Graphics::size_queue
      IL_002c:  newarr     CamIL.Graphics/evenement
      IL_0031:  stsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
      IL_0036:  ldc.i4.0
      IL_0037:  stsfld     int32 CamIL.Graphics::gr_head
      IL_003c:  ldc.i4.0
      IL_003d:  stsfld     int32 CamIL.Graphics::gr_tail
      IL_0042:  ret
    } // end of method Graphics::.cctor

    .method public hidebysig specialname rtspecialname 
            instance void  .ctor() cil managed
    {
      // Code size       7 (0x7)
      .maxstack  1
      IL_0000:  ldarg.0
      IL_0001:  call       instance void [System.Windows.Forms]System.Windows.Forms.Control::.ctor()
      IL_0006:  ret
    } // end of method Graphics::.ctor



    .method public hidebysig static void gr_moveto(int32 x,int32 y) cil managed
    {
      // Code size       13 (0xd)
      .maxstack  1
      IL_0000:  ldarg.0
      IL_0001:  stsfld     int32 CamIL.Graphics::c_x
      IL_0006:  ldarg.1
      IL_0007:  stsfld     int32 CamIL.Graphics::c_y
      IL_000c:  ret
    }

    .method public hidebysig static void gr_set_window_title(string sb) cil managed
    {
      // Code size       23 (0x17)
      .maxstack  2
      IL_0000:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0005:  ldarg.0
      IL_000b:  callvirt   instance void [System.Windows.Forms]System.Windows.Forms.Control::set_Text(string)
      IL_0010:  ldarg.0
      IL_0011:  stsfld     string CamIL.Graphics::title
      IL_0016:  ret
    }

    .method public hidebysig static void gr_set_color(int32 col) cil managed
    {
      // Code size       56 (0x38)
      .maxstack  4
      .locals init (valuetype [System.Drawing]System.Drawing.Color V_0)
	ldarg.0
	ldc.i4.m1
	beq.s transparent
      IL_0000:  ldarg.0
      IL_0001:  ldc.i4     0x10000
      IL_0006:  div
      IL_0007:  ldarg.0
      IL_0008:  ldc.i4     0x100
      IL_000d:  div
      IL_000e:  ldc.i4     0x100
      IL_0013:  rem
      IL_0014:  ldarg.0
      IL_0015:  ldc.i4     0x100
      IL_001a:  rem
      IL_001b:  call       valuetype [System.Drawing]System.Drawing.Color [System.Drawing]System.Drawing.Color::FromArgb(int32,
                                                                                                                         int32,
                                                                                                                         int32)
	br ok_color
transparent:
	ldc.i4.0
	ldc.i4 255
	dup
	dup
        call       valuetype [System.Drawing]System.Drawing.Color [System.Drawing]System.Drawing.Color::FromArgb(int32,int32,int32,int32)
ok_color:	  stloc.0
      IL_0021:  ldsfld     class [System.Drawing]System.Drawing.SolidBrush CamIL.Graphics::brush
      IL_0026:  ldloc.0
      IL_0027:  callvirt   instance void [System.Drawing]System.Drawing.SolidBrush::set_Color(valuetype [System.Drawing]System.Drawing.Color)
      IL_002c:  ldsfld     class [System.Drawing]System.Drawing.Pen CamIL.Graphics::pen
      IL_0031:  ldloc.0
      IL_0032:  callvirt   instance void [System.Drawing]System.Drawing.Pen::set_Color(valuetype [System.Drawing]System.Drawing.Color)
      IL_0037:  ret

    } // end of method Graphics::gr_set_color

	


    .method public hidebysig static void 
            gr_set_font(string s) cil managed
    {
      // Code size       27 (0x1b)
      .maxstack  3
      IL_0000:  ldarg.0
      IL_0006:  ldsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      IL_000b:  callvirt   instance float32 [System.Drawing]System.Drawing.Font::get_Size()
      IL_0010:  newobj     instance void [System.Drawing]System.Drawing.Font::.ctor(string,
                                                                                    float32)
      IL_0015:  stsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      IL_001a:  ret
    } // end of method Graphics::gr_set_font

    .method public hidebysig static void 
            gr_set_text_size(int32 size) cil managed
    {
      // Code size       23 (0x17)
      .maxstack  3
      IL_0000:  ldsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      IL_0005:  callvirt   instance class [System.Drawing]System.Drawing.FontFamily [System.Drawing]System.Drawing.Font::get_FontFamily()
      IL_000a:  ldarg.0
      IL_000b:  conv.r4
      IL_000c:  newobj     instance void [System.Drawing]System.Drawing.Font::.ctor(class [System.Drawing]System.Drawing.FontFamily,
                                                                                    float32)
      IL_0011:  stsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      IL_0016:  ret
    } // end of method Graphics::gr_set_text_size

    .method public hidebysig static object[] 
            gr_text_size(string sb) cil managed
    {
      // Code size       82 (0x52)
      .maxstack  4
      .locals init (valuetype [System.Drawing]System.Drawing.Size V_0,
               object[] V_1,
               object[] V_2,
               valuetype [System.Drawing]System.Drawing.SizeF V_3)
      IL_0000:  ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
      IL_0005:  ldarg.0
      IL_000b:  ldsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      IL_0010:  callvirt   instance valuetype [System.Drawing]System.Drawing.SizeF [System.Drawing]System.Drawing.Graphics::MeasureString(string,
                                                                                                                                          class [System.Drawing]System.Drawing.Font)
      IL_0015:  stloc.3
      IL_0016:  ldloca.s   V_3
      IL_0018:  call       instance valuetype [System.Drawing]System.Drawing.Size [System.Drawing]System.Drawing.SizeF::ToSize()
      IL_001d:  stloc.0
      IL_001e:  ldc.i4.3
      IL_001f:  newarr     [mscorlib]System.Object
      IL_0024:  stloc.1
      IL_0025:  ldloc.1
      IL_0026:  ldc.i4.0
      IL_0027:  ldloca.s   V_0
      IL_0029:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Width()
      IL_002e:  box [mscorlib]System.Int32
      IL_0033:  stelem.ref
      IL_0034:  ldloc.1
      IL_0035:  ldc.i4.1
      IL_0036:  ldloca.s   V_0
      IL_0038:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_003d:  box [mscorlib]System.Int32
      IL_0042:  stelem.ref
      IL_0043:  ldloc.1
      IL_0044:  ldc.i4.2
      IL_0045:  ldc.i4.0
      IL_0046:  box [mscorlib]System.Int32
      IL_004b:  stelem.ref
      IL_004c:  ldloc.1
      IL_004d:  stloc.2
      IL_004e:  br.s       IL_0050

      IL_0050:  ldloc.2
      IL_0051:  ret
    } // end of method Graphics::gr_text_size


    .method public hidebysig static void 
            gr_set_line_width(int32 width) cil managed
    {
      // Code size       13 (0xd)
      .maxstack  2
      IL_0000:  ldsfld     class [System.Drawing]System.Drawing.Pen CamIL.Graphics::pen
      IL_0005:  ldarg.0
      IL_0006:  conv.r4
      IL_0007:  callvirt   instance void [System.Drawing]System.Drawing.Pen::set_Width(float32)
      IL_000c:  ret
    } // end of method Graphics::gr_set_line_width

    .method public hidebysig static int32 
            gr_size_x() cil managed
    {
      // Code size       23 (0x17)
      .maxstack  2
      .locals init (int32 V_0,
               valuetype [System.Drawing]System.Drawing.Size V_1)
      IL_0000:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0005:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_000a:  stloc.1
      IL_000b:  ldloca.s   V_1
      IL_000d:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Width()
      IL_0012:  stloc.0
      IL_0013:  br.s       IL_0015

      IL_0015:  ldloc.0
      IL_0016:  ret
    } // end of method Graphics::gr_size_x

    .method public hidebysig static int32 
            gr_size_y() cil managed
    {
      // Code size       23 (0x17)
      .maxstack  2
      .locals init (int32 V_0,
               valuetype [System.Drawing]System.Drawing.Size V_1)
      IL_0000:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0005:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_000a:  stloc.1
      IL_000b:  ldloca.s   V_1
      IL_000d:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_0012:  stloc.0
      IL_0013:  br.s       IL_0015

      IL_0015:  ldloc.0
      IL_0016:  ret
    } // end of method Graphics::gr_size_y

	
	
    .method public hidebysig static void 
            gr_sound(int32 x,
                     int32 y) cil managed
    {
      // Code size       1 (0x1)
      .maxstack  0
      IL_0000:  ret
    } // end of method Graphics::gr_sound

    .method private hidebysig static void 
            gr_check_open() cil managed
    {
      // Code size       1 (0x1)
      .maxstack  0
      IL_0000:  ret
    } // end of method Graphics::gr_check_open
		
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



	
    .method private hidebysig static void buffer_lineto(int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b) cil managed
    {
      .maxstack  7
      .locals init (valuetype [System.Drawing]System.Drawing.Size V_0)
      IL_0000:  ldarg.2
      IL_0005:  ldsfld     class [System.Drawing]System.Drawing.Pen CamIL.Graphics::pen
      IL_000a:  ldsfld     int32 CamIL.Graphics::c_x
      IL_000f:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0014:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_0019:  stloc.0
      IL_001a:  ldloca.s   V_0
      IL_001c:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_0021:  ldsfld     int32 CamIL.Graphics::c_y
      IL_0026:  sub
      IL_0027:  ldarg.0
      IL_0028:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_002d:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_0032:  stloc.0
      IL_0033:  ldloca.s   V_0
      IL_0035:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_003a:  ldarg.1
      IL_003b:  sub
      IL_003c:  callvirt   instance void [System.Drawing]System.Drawing.Graphics::DrawLine(class [System.Drawing]System.Drawing.Pen,
                                                                                           int32,
                                                                                           int32,
                                                                                           int32,
                                                                                           int32)
		ret
      }
	
    .method public hidebysig static void gr_lineto(int32 x,int32 y) cil managed
    {
	ldsfld bool CamIL.Graphics::display_mode
	brfalse NODISPLAY	
	ldarg.0
	ldarg.1
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	call void  CamIL.Graphics::buffer_lineto(int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b)
	
	ldarg.0
	ldarg.1
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	call void  CamIL.Graphics::buffer_lineto(int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b)

	
NODISPLAY:
	ldarg.0
	ldarg.1
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
	call void  CamIL.Graphics::buffer_lineto(int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b)
	
	ldarg.0
	ldarg.1
	call void CamIL.Graphics::gr_moveto(int32,int32)
	ret

	
    }

	
    .method private hidebysig static void buffer_plot(int32 x, int32 y,class [System.Drawing]System.Drawing.Graphics b) cil managed
    {
      // Code size       187 (0xbb)
      .maxstack  7
      .locals init (valuetype [System.Drawing]System.Drawing.Size V_0)
      IL_0000:  ldarg.2
      IL_0005:  ldsfld     class [System.Drawing]System.Drawing.SolidBrush CamIL.Graphics::brush
      IL_000a:  ldarg.0
      IL_000b:  conv.r4
      IL_000c:  ldsfld     class [System.Drawing]System.Drawing.Pen CamIL.Graphics::pen
      IL_0011:  callvirt   instance float32 [System.Drawing]System.Drawing.Pen::get_Width()
      IL_0016:  ldc.r4     2.
      IL_001b:  div
      IL_001c:  sub
      IL_001d:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0022:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_0027:  stloc.0
      IL_0028:  ldloca.s   V_0
      IL_002a:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_002f:  conv.r4
      IL_0030:  ldsfld     class [System.Drawing]System.Drawing.Pen CamIL.Graphics::pen
      IL_0035:  callvirt   instance float32 [System.Drawing]System.Drawing.Pen::get_Width()
      IL_003a:  ldc.r4     2.
      IL_003f:  div
      IL_0040:  sub
      IL_0041:  ldarg.1
      IL_0042:  conv.r4
      IL_0043:  sub
      IL_0044:  ldsfld     class [System.Drawing]System.Drawing.Pen CamIL.Graphics::pen
      IL_0049:  callvirt   instance float32 [System.Drawing]System.Drawing.Pen::get_Width()
      IL_004e:  ldsfld     class [System.Drawing]System.Drawing.Pen CamIL.Graphics::pen
      IL_0053:  callvirt   instance float32 [System.Drawing]System.Drawing.Pen::get_Width()
      IL_0058:  callvirt   instance void [System.Drawing]System.Drawing.Graphics::FillRectangle(class [System.Drawing]System.Drawing.Brush,
                                                                                                float32,
                                                                                                float32,
                                                                                                float32,
                                                                                                float32)
      IL_00ba:  ret
    }


    .method public hidebysig static void gr_plot(int32 x,int32 y) cil managed
    {
	ldsfld bool CamIL.Graphics::display_mode
	brfalse NODISPLAY	
	ldarg.0
	ldarg.1
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	call void  CamIL.Graphics::buffer_plot(int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b)
	
	ldarg.0
	ldarg.1
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	call void  CamIL.Graphics::buffer_plot(int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b)

	
NODISPLAY:
	ldarg.0
	ldarg.1
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
	call void  CamIL.Graphics::buffer_plot(int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b)
	
	ldarg.0
	ldarg.1
	call void CamIL.Graphics::gr_moveto(int32,int32)
	ret
    }
	
    .method private hidebysig static void buffer_draw_string(string s,class [System.Drawing]System.Drawing.Graphics b) cil managed
    {
      // Code size       137 (0x89)
      .maxstack  8
      .locals init (valuetype [System.Drawing]System.Drawing.Size V_0)
      IL_0000:  ldarg.1
      IL_0005:  ldarg.0
      IL_000b:  ldsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      IL_0010:  ldsfld     class [System.Drawing]System.Drawing.SolidBrush CamIL.Graphics::brush
      IL_0015:  ldsfld     int32 CamIL.Graphics::c_x
      IL_001a:  conv.r4
      IL_001b:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0020:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_0025:  stloc.0
      IL_0026:  ldloca.s   V_0
      IL_0028:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_002d:  ldsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      IL_0032:  callvirt   instance int32 [System.Drawing]System.Drawing.Font::get_Height()
      IL_0037:  sub
      IL_0038:  ldsfld     int32 CamIL.Graphics::c_y
      IL_003d:  sub
      IL_003e:  conv.r4
      IL_003f:  callvirt   instance void [System.Drawing]System.Drawing.Graphics::DrawString(string,
                                                                                             class [System.Drawing]System.Drawing.Font,
                                                                                             class [System.Drawing]System.Drawing.Brush,
                                                                                             float32,
                                                                                             float32)
      IL_0088:  ret
    }

    .method public hidebysig static void gr_draw_string(string s) cil managed
    {
	ldsfld bool CamIL.Graphics::display_mode
	brfalse NODISPLAY	
	ldarg.0
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	call void  CamIL.Graphics::buffer_draw_string(string s,class [System.Drawing]System.Drawing.Graphics b)
	
	ldarg.0
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	call void  CamIL.Graphics::buffer_draw_string(string s,class [System.Drawing]System.Drawing.Graphics b)

	
NODISPLAY:
	ldarg.0
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
	call void  CamIL.Graphics::buffer_draw_string(string s,class [System.Drawing]System.Drawing.Graphics b)
	ret
    }	

	
    .method private hidebysig static void  buffer_draw_char(string s,class [System.Drawing]System.Drawing.Graphics b) cil managed
    {
      // Code size       147 (0x93)
      .maxstack  9
      .locals init (valuetype [System.Drawing]System.Drawing.Size V_0)
      IL_0000:  ldarg.1
      IL_0005:  ldarg.0

      IL_0010:  ldsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      IL_0015:  ldsfld     class [System.Drawing]System.Drawing.SolidBrush CamIL.Graphics::brush
      IL_001a:  ldsfld     int32 CamIL.Graphics::c_x
      IL_001f:  conv.r4
      IL_0020:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0025:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_002a:  stloc.0
      IL_002b:  ldloca.s   V_0
      IL_002d:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_0032:  ldsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      IL_0037:  callvirt   instance int32 [System.Drawing]System.Drawing.Font::get_Height()
      IL_003c:  sub
      IL_003d:  ldsfld     int32 CamIL.Graphics::c_y
      IL_0042:  sub
      IL_0043:  conv.r4
      IL_0044:  callvirt   instance void [System.Drawing]System.Drawing.Graphics::DrawString(string,
                                                                                             class [System.Drawing]System.Drawing.Font,
                                                                                             class [System.Drawing]System.Drawing.Brush,
                                                                                             float32,
                                                                                             float32)
      ret	
}
	
	

    .method public hidebysig static void  gr_draw_char(int32 s) cil managed
    {
	.locals init (string,valuetype [System.Drawing]System.Drawing.Size V_0,valuetype [System.Drawing]System.Drawing.SizeF)
        ldarg.0
	call       char [mscorlib]System.Convert::ToChar(int32)
        call       string [mscorlib]System.Convert::ToString(char)
        stloc.0
	
	ldsfld bool CamIL.Graphics::display_mode
	brfalse NODISPLAY	
	ldloc.0
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	call void  CamIL.Graphics::buffer_draw_char(string s,class [System.Drawing]System.Drawing.Graphics b)
	
	ldloc.0
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	call void  CamIL.Graphics::buffer_draw_char(string s,class [System.Drawing]System.Drawing.Graphics b)

	
NODISPLAY:
	ldloc.0
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
	call void  CamIL.Graphics::buffer_draw_char(string s,class [System.Drawing]System.Drawing.Graphics b)

// a la fin current_x est augmenté
      ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
      ldloc.0
      ldsfld     class [System.Drawing]System.Drawing.Font CamIL.Graphics::font
      callvirt   instance valuetype [System.Drawing]System.Drawing.SizeF [System.Drawing]System.Drawing.Graphics::MeasureString(string,class [System.Drawing]System.Drawing.Font)
// Voir avec un MeasureCharacterRanges plutot, mais c'est plus compliqué
      stloc.2
      ldloca.s   2   //un SizeF
      call       instance valuetype [System.Drawing]System.Drawing.Size [System.Drawing]System.Drawing.SizeF::ToSize()
      stloc.1   //un Size
      ldloca.s   1
      call       instance int32 [System.Drawing]System.Drawing.Size::get_Width()
      ldsfld int32 CamIL.Graphics::c_x
      add
      stsfld int32 CamIL.Graphics::c_x	
	
      ret
    }	


    .method public hidebysig static void gr_fill_rect(int32 vx, int32 vy, int32 vw, int32 vh) cil managed
    {
	ldsfld bool CamIL.Graphics::display_mode
	brfalse NODISPLAY	
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	call void  CamIL.Graphics::buffer_fill_rect(int32 vx, int32 vy, int32 vw, int32 vh,class [System.Drawing]System.Drawing.Graphics b)
	
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	call void  CamIL.Graphics::buffer_fill_rect(int32 vx, int32 vy, int32 vw, int32 vh,class [System.Drawing]System.Drawing.Graphics b)
	
NODISPLAY:
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
	call void  CamIL.Graphics::buffer_fill_rect(int32 vx, int32 vy, int32 vw, int32 vh,class [System.Drawing]System.Drawing.Graphics b)
	ret
    }
	
    .method private hidebysig static void buffer_fill_rect(int32 vx, int32 vy, int32 vw, int32 vh, class [System.Drawing]System.Drawing.Graphics b) cil managed
    {
      // Code size       81 (0x51)
      .maxstack  6
      .locals init (valuetype [System.Drawing]System.Drawing.Size V_0)
      IL_0000:  ldarg.s 4
      IL_0005:  ldsfld     class [System.Drawing]System.Drawing.SolidBrush CamIL.Graphics::brush
      IL_000a:  ldarg.0
      IL_000b:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0010:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_0015:  stloc.0
      IL_0016:  ldloca.s   V_0
      IL_0018:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_001d:  ldarg.3
      IL_001e:  sub
      IL_001f:  ldarg.1
      IL_0020:  sub
      IL_0021:  ldarg.2
      IL_0022:  ldarg.3
      IL_0023:  callvirt   instance void [System.Drawing]System.Drawing.Graphics::FillRectangle(class [System.Drawing]System.Drawing.Brush,
                                                                                                int32,
                                                                                                int32,
                                                                                                int32,
                                                                                                int32)
      IL_0050:  ret
    }

    .method public hidebysig static void gr_draw_rect(int32 vx, int32 vy, int32 vw, int32 vh) cil managed
    {
	ldsfld bool CamIL.Graphics::display_mode
	brfalse NODISPLAY	
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	call void  CamIL.Graphics::buffer_draw_rect(int32 vx, int32 vy, int32 vw, int32 vh,class [System.Drawing]System.Drawing.Graphics b)
	
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	call void  CamIL.Graphics::buffer_draw_rect(int32 vx, int32 vy, int32 vw, int32 vh,class [System.Drawing]System.Drawing.Graphics b)
	
NODISPLAY:
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
	call void  CamIL.Graphics::buffer_draw_rect(int32 vx, int32 vy, int32 vw, int32 vh,class [System.Drawing]System.Drawing.Graphics b)
	ret
    }	
	
    .method private hidebysig static void buffer_draw_rect (int32 vx, int32 vy, int32 vw, int32 vh, class [System.Drawing]System.Drawing.Graphics b) cil managed
    {
      // Code size       81 (0x51)
      .maxstack  6
      .locals init (valuetype [System.Drawing]System.Drawing.Size V_0)
      IL_0000:  ldarg.s 4
      IL_0005:  ldsfld     class [System.Drawing]System.Drawing.Pen CamIL.Graphics::pen
      IL_000a:  ldarg.0
      IL_000b:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0010:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_0015:  stloc.0
      IL_0016:  ldloca.s   V_0
      IL_0018:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_001d:  ldarg.3
      IL_001e:  sub
      IL_001f:  ldarg.1
      IL_0020:  sub
      IL_0021:  ldarg.2
      IL_0022:  ldarg.3
      IL_0023:  callvirt   instance void [System.Drawing]System.Drawing.Graphics::DrawRectangle(class [System.Drawing]System.Drawing.Pen,
                                                                                                int32,
                                                                                                int32,
                                                                                                int32,
                                                                                                int32)
      IL_0050:  ret
    }

    .method public hidebysig static void  gr_fill_arc(int32 vx, int32 vy, int32 vrx, int32 vry, int32 va1, int32 va2) cil managed
    {
	ldsfld bool CamIL.Graphics::display_mode
	brfalse NODISPLAY	
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldarg.s 4
	ldarg.s 5
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	call void  CamIL.Graphics::buffer_fill_arc(int32 vx, int32 vy, int32 vrx, int32 vry, int32 va1, int32 va2,class [System.Drawing]System.Drawing.Graphics b)
	
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldarg.s 4
	ldarg.s 5
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	call void  CamIL.Graphics::buffer_fill_arc(int32 vx, int32 vy, int32 vrx, int32 vry, int32 va1, int32 va2,class [System.Drawing]System.Drawing.Graphics b)
	
NODISPLAY:
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldarg.s 4
	ldarg.s 5
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
	call void  CamIL.Graphics::buffer_fill_arc(int32 vx, int32 vy, int32 vrx, int32 vry, int32 va1, int32 va2,class [System.Drawing]System.Drawing.Graphics b)
	ret
    }	

	
	
		
    .method private hidebysig static void  buffer_fill_arc(int32 vx, int32 vy, int32 vrx, int32 vry, int32 va1, int32 va2, class [System.Drawing]System.Drawing.Graphics b) cil managed
    {
      // Code size       111 (0x6f)
      .maxstack  9
      .locals init (valuetype [System.Drawing]System.Drawing.Size V_0)
      IL_0000:  ldarg.s 6
      IL_0005:  ldsfld     class [System.Drawing]System.Drawing.SolidBrush CamIL.Graphics::brush
      IL_000a:  ldarg.0
      IL_000b:  ldarg.2
      IL_000c:  ldc.i4.2
      IL_000d:  div
      IL_000e:  sub
      IL_000f:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0014:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_0019:  stloc.0
      IL_001a:  ldloca.s   V_0
      IL_001c:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_0021:  ldarg.1
      IL_0022:  sub
      IL_0023:  ldarg.3
      IL_0024:  ldc.i4.2
      IL_0025:  div
      IL_0026:  sub
      IL_0027:  ldarg.2
      IL_0028:  ldarg.3
      IL_0029:  ldarg.s    va1
      IL_002b:  neg
      IL_002c:  ldarg.s    va2
      IL_002e:  ldarg.s    va1
      IL_0030:  sub
      IL_0031:  neg
      IL_0032:  callvirt   instance void [System.Drawing]System.Drawing.Graphics::FillPie(class [System.Drawing]System.Drawing.Brush,
                                                                                          int32,
                                                                                          int32,
                                                                                          int32,
                                                                                          int32,
                                                                                          int32,
                                                                                          int32)
      IL_006e:  ret
    }

    .method public hidebysig static void  gr_draw_arc(int32 vx, int32 vy, int32 vrx, int32 vry, int32 va1, int32 va2) cil managed
    {
	ldsfld bool CamIL.Graphics::display_mode
	brfalse NODISPLAY	
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldarg.s 4
	ldarg.s 5
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	call void  CamIL.Graphics::buffer_draw_arc(int32 vx, int32 vy, int32 vrx, int32 vry, int32 va1, int32 va2,class [System.Drawing]System.Drawing.Graphics b)
	
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldarg.s 4
	ldarg.s 5
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	call void  CamIL.Graphics::buffer_draw_arc(int32 vx, int32 vy, int32 vrx, int32 vry, int32 va1, int32 va2,class [System.Drawing]System.Drawing.Graphics b)
	
NODISPLAY:
	ldarg.0
	ldarg.1
	ldarg.2
	ldarg.3
	ldarg.s 4
	ldarg.s 5
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
	call void  CamIL.Graphics::buffer_draw_arc(int32 vx, int32 vy, int32 vrx, int32 vry, int32 va1, int32 va2,class [System.Drawing]System.Drawing.Graphics b)
	ret
    }	
	
    .method private hidebysig static void  buffer_draw_arc(int32 vx, int32 vy, int32 vrx, int32 vry, int32 va1, int32 va2, class [System.Drawing]System.Drawing.Graphics b) cil managed
    {
      // Code size       111 (0x6f)
      .maxstack  9
      .locals init (valuetype [System.Drawing]System.Drawing.Size V_0)
      IL_0000:  ldarg.s 6
      IL_0005:  ldsfld     class [System.Drawing]System.Drawing.Pen CamIL.Graphics::pen
      IL_000a:  ldarg.0
      IL_000b:  ldarg.2
      IL_000c:  ldc.i4.2
      IL_000d:  div
      IL_000e:  sub
      IL_000f:  ldsfld     class CamIL.Graphics CamIL.Graphics::g
      IL_0014:  callvirt   instance valuetype [System.Drawing]System.Drawing.Size [System.Windows.Forms]System.Windows.Forms.Control::get_ClientSize()
      IL_0019:  stloc.0
      IL_001a:  ldloca.s   V_0
      IL_001c:  call       instance int32 [System.Drawing]System.Drawing.Size::get_Height()
      IL_0021:  ldarg.1
      IL_0022:  sub
      IL_0023:  ldarg.3
      IL_0024:  ldc.i4.2
      IL_0025:  div
      IL_0026:  sub
      IL_0027:  ldarg.2
      IL_0028:  ldarg.3
      IL_0029:  ldarg.s    va1
      IL_002b:  neg
      IL_002c:  ldarg.s    va2
      IL_002e:  ldarg.s    va1
      IL_0030:  sub
      IL_0031:  neg
      IL_0032:  callvirt   instance void [System.Drawing]System.Drawing.Graphics::DrawArc(class [System.Drawing]System.Drawing.Pen,
                                                                                          int32,
                                                                                          int32,
                                                                                          int32,
                                                                                          int32,
                                                                                          int32,
                                                                                          int32)
      IL_006e:  ret
    }

    .method private hidebysig static void  buffer_clear_graph(class [System.Drawing]System.Drawing.Graphics b) cil managed
    {
      .maxstack  2
      IL_0000:  ldarg.0
      IL_0005:  call       valuetype [System.Drawing]System.Drawing.Color [System.Drawing]System.Drawing.Color::get_White()
      IL_000a:  callvirt   instance void [System.Drawing]System.Drawing.Graphics::Clear(valuetype [System.Drawing]System.Drawing.Color)
      IL_001e:  ret
    }

    .method public hidebysig static void gr_clear_graph() cil managed
    {
	ldsfld bool CamIL.Graphics::display_mode
	brfalse NODISPLAY	
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	call void  CamIL.Graphics::buffer_clear_graph(class [System.Drawing]System.Drawing.Graphics b)
	
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	call void  CamIL.Graphics::buffer_clear_graph(class [System.Drawing]System.Drawing.Graphics b)
	
NODISPLAY:
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
	call void  CamIL.Graphics::buffer_clear_graph(class [System.Drawing]System.Drawing.Graphics b)
	ret
    }	

///////////////////////////////////////////////////////////////////////////

	
    .method public hidebysig static int32 
            gr_point_color(int32 x,
                           int32 y) cil managed
    {
      // Code size       74 (0x4a)
      .maxstack  4
      .locals init (valuetype [System.Drawing]System.Drawing.Color V_0,
               int32 V_1,
               class [System.Drawing]System.Drawing.Bitmap V_2)
      IL_0000:  ldsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im2
      IL_0005:  dup
      IL_0006:  stloc.2
      IL_0007:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_000c:  ldsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im2
        IL_0011:  ldarg.0
        IL_0012:  ldarg.1
        IL_0013:  callvirt   instance valuetype [System.Drawing]System.Drawing.Color [System.Drawing]System.Drawing.Bitmap::GetPixel(int32,
                                                                                                                                     int32)
        IL_0018:  stloc.0
        IL_0019:  leave.s    IL_0022

      }  // end .try
      finally
      {
        IL_001b:  ldloc.2
        IL_001c:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_0021:  endfinally
      }  // end handler
		ldloca.s  V_0
		call       instance unsigned int8 [System.Drawing]System.Drawing.Color::get_A()
		ldc.i4.0
		beq.s transparent
      IL_0022:  ldc.i4     0x10000
      IL_0027:  ldloca.s   V_0
      IL_0029:  call       instance unsigned int8 [System.Drawing]System.Drawing.Color::get_R()
      IL_002e:  mul
      IL_002f:  ldc.i4     0x100
      IL_0034:  ldloca.s   V_0
      IL_0036:  call       instance unsigned int8 [System.Drawing]System.Drawing.Color::get_G()
      IL_003b:  mul
      IL_003c:  add
      IL_003d:  ldloca.s   V_0
      IL_003f:  call       instance unsigned int8 [System.Drawing]System.Drawing.Color::get_B()
      IL_0044:  add
      IL_0045:  stloc.1
      IL_0046:  br.s       IL_0048

      IL_0048:  ldloc.1
      IL_0049:  ret
transparent:
	ldc.i4.m1
	ret
	
    } // end of method Graphics::gr_point_color


    .method public hidebysig static object gr_create_image(int32 x,int32 y) cil managed
    {
      .maxstack  2
	ldarg.0
	ldarg.1
	newobj instance void [System.Drawing]System.Drawing.Bitmap::.ctor(int32,int32)
	ret
    }
	
    .method public hidebysig static void gr_draw_image(object,int32 x,int32 y) cil managed
    {
	ldsfld bool CamIL.Graphics::display_mode
	brfalse NODISPLAY	
	ldarg.0
	ldarg.1
	ldarg.2
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::gc
	call void  CamIL.Graphics::buffer_draw_image(object,int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b)
	
	ldarg.0
	ldarg.1
	ldarg.2
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer
	call void  CamIL.Graphics::buffer_draw_image(object,int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b)
	
NODISPLAY:
	ldarg.0
	ldarg.1
	ldarg.2
	ldsfld     class [System.Drawing]System.Drawing.Graphics CamIL.Graphics::buffer2
	call void  CamIL.Graphics::buffer_draw_image(object,int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b)
	ret
    }
	
    .method private hidebysig static void buffer_draw_image(object,int32 x,int32 y,class [System.Drawing]System.Drawing.Graphics b) cil managed
    {
	.maxstack 5
        .locals init (int32 V_0)
	ldarg.3
	ldarg.0
	castclass class [System.Drawing]System.Drawing.Bitmap
	dup
	callvirt int32 [System.Drawing]System.Drawing.Bitmap::get_Height()
	stloc.0
	ldarg.1
	call int32 CamIL.Graphics::gr_size_y()
	ldarg.2
	sub
	ldloc.0
	sub
        callvirt   instance void [System.Drawing]System.Drawing.Graphics::DrawImage(class [System.Drawing]System.Drawing.Image,int32,int32)
	ret
    }

  .method public hidebysig static object gr_make_image(object[] tab) cil managed
  {
    // Code size       172 (0xac)
    .maxstack  4
    .locals init (int32 V_0,
             int32 V_1,
             class [System.Drawing]System.Drawing.Bitmap V_2,
             int32 V_3,
             int32 V_4,
             valuetype [System.Drawing]System.Drawing.Color V_5,
             int32 V_6,
             object V_7)
    IL_0000:  ldarg.0
    IL_0001:  ldlen
    IL_0002:  conv.i4
    IL_0003:  ldc.i4.1
    IL_0004:  sub
    IL_0005:  stloc.0
    IL_0006:  ldloc.0
    IL_0007:  ldc.i4.0
    IL_0008:  ble.s      IL_0019

    IL_000a:  ldarg.0
    IL_000b:  ldc.i4.0
    IL_000c:  ldelem.ref
    IL_000d:  castclass  object[]
    IL_0012:  ldlen
    IL_0013:  conv.i4
    IL_0014:  ldc.i4.1
    IL_0015:  sub
    IL_0016:  stloc.1
    IL_0017:  br.s       IL_001b

    IL_0019:  ldc.i4.0
    IL_001a:  stloc.1
    IL_001b:  ldloc.1
    IL_001c:  ldloc.0
    IL_001d:  newobj     instance void [System.Drawing]System.Drawing.Bitmap::.ctor(int32,
                                                                                    int32)
    IL_0022:  stloc.2
    IL_0023:  ldc.i4.0
    IL_0024:  stloc.3
    IL_0025:  br.s       IL_00a0

    IL_0027:  ldc.i4.0
    IL_0028:  stloc.s    V_4
    IL_002a:  br.s       IL_0097

    IL_002c:  ldarg.0
    IL_002d:  ldloc.s    V_4
    IL_002f:  ldelem.ref
    IL_0030:  castclass  object[]
    IL_0035:  ldloc.3
    IL_0036:  ldelem.ref
    IL_0037:  castclass  [mscorlib]System.Int32
	      unbox  [mscorlib]System.Int32
    IL_003c:  ldind.i4
    IL_0041:  stloc.s    V_6
    IL_0043:  ldloc.s    V_6
    IL_0045:  ldc.i4.m1
    IL_0046:  bne.un.s   IL_0061

    IL_0048:  ldc.i4.0
    IL_0049:  ldc.i4     0xff
    IL_004e:  ldc.i4     0xff
    IL_0053:  ldc.i4     0xff
    IL_0058:  call       valuetype [System.Drawing]System.Drawing.Color [System.Drawing]System.Drawing.Color::FromArgb(int32,
                                                                                                                       int32,
                                                                                                                       int32,
                                                                                                                       int32)
    IL_005d:  stloc.s    V_5
    IL_005f:  br.s       IL_0086

    IL_0061:  ldloc.s    V_6
    IL_0063:  ldc.i4     0x10000
    IL_0068:  div
    IL_0069:  ldloc.s    V_6
    IL_006b:  ldc.i4     0x100
    IL_0070:  div
    IL_0071:  ldc.i4     0x100
    IL_0076:  rem
    IL_0077:  ldloc.s    V_6
    IL_0079:  ldc.i4     0x100
    IL_007e:  rem
    IL_007f:  call       valuetype [System.Drawing]System.Drawing.Color [System.Drawing]System.Drawing.Color::FromArgb(int32,
                                                                                                                       int32,
                                                                                                                       int32)
    IL_0084:  stloc.s    V_5
    IL_0086:  ldloc.2
    IL_0087:  ldloc.3
    IL_0088:  ldloc.s    V_4
    IL_008a:  ldloc.s    V_5
    IL_008c:  callvirt   instance void [System.Drawing]System.Drawing.Bitmap::SetPixel(int32,
                                                                                       int32,
                                                                                       valuetype [System.Drawing]System.Drawing.Color)
    IL_0091:  ldloc.s    V_4
    IL_0093:  ldc.i4.1
    IL_0094:  add
    IL_0095:  stloc.s    V_4
    IL_0097:  ldloc.s    V_4
    IL_0099:  ldloc.0
    IL_009a:  blt.s      IL_002c

    IL_009c:  ldloc.3
    IL_009d:  ldc.i4.1
    IL_009e:  add
    IL_009f:  stloc.3
    IL_00a0:  ldloc.3
    IL_00a1:  ldloc.1
    IL_00a2:  blt.s      IL_0027

    IL_00a4:  ldloc.2
    IL_00a5:  stloc.s    V_7
    IL_00a7:  br.s       IL_00a9

    IL_00a9:  ldloc.s    V_7
    IL_00ab:  ret
  } // end of method lol::gr_make_image

	
  .method public hidebysig static object[] gr_dump_image(object img) cil managed
  {
    // Code size       180 (0xb4)
    .maxstack  4
    .locals init (class [System.Drawing]System.Drawing.Bitmap V_0,
             int32 V_1,
             int32 V_2,
             object[] V_3,
             int32 V_4,
             object[] V_5,
             int32 V_6,
             valuetype [System.Drawing]System.Drawing.Color V_7,
             int32 V_8,
             object[] V_9)
    IL_0000:  ldarg.0
    IL_0001:  castclass  [System.Drawing]System.Drawing.Bitmap
    IL_0006:  stloc.0
    IL_0007:  ldloc.0
    IL_0008:  callvirt   instance int32 [System.Drawing]System.Drawing.Image::get_Height()
    IL_000d:  stloc.1
    IL_000e:  ldloc.0
    IL_000f:  callvirt   instance int32 [System.Drawing]System.Drawing.Image::get_Width()
    IL_0014:  stloc.2
    IL_0015:  ldloc.1
    IL_0016:  ldc.i4.1
    IL_0017:  add
    IL_0018:  newarr     [mscorlib]System.Object
    IL_001d:  stloc.3
    IL_001e:  ldloc.3
    IL_001f:  ldloc.1
    IL_0020:  ldc.i4.0
    IL_0021:  box [mscorlib]System.Int32
    IL_0026:  stelem.ref
    IL_0027:  ldc.i4.0
    IL_0028:  stloc.s    V_4
    IL_002a:  br.s       IL_00a7

    IL_002c:  ldloc.2
    IL_002d:  ldc.i4.1
    IL_002e:  add
    IL_002f:  newarr     [mscorlib]System.Object
    IL_0034:  stloc.s    V_5
    IL_0036:  ldloc.s    V_5
    IL_0038:  ldloc.2
    IL_0039:  ldc.i4.0
    IL_003a:  box [mscorlib]System.Int32
    IL_003f:  stelem.ref
    IL_0040:  ldc.i4.0
    IL_0041:  stloc.s    V_6
    IL_0043:  br.s       IL_0096

    IL_0045:  ldloc.0
    IL_0046:  ldloc.s    V_6
    IL_0048:  ldloc.s    V_4
    IL_004a:  callvirt   instance valuetype [System.Drawing]System.Drawing.Color [System.Drawing]System.Drawing.Bitmap::GetPixel(int32,
                                                                                                                                 int32)
    IL_004f:  stloc.s    V_7
    IL_0051:  ldloca.s   V_7
    IL_0053:  call       instance unsigned int8 [System.Drawing]System.Drawing.Color::get_A()
    IL_0058:  brtrue.s   IL_005f

    IL_005a:  ldc.i4.m1
    IL_005b:  stloc.s    V_8
    IL_005d:  br.s       IL_0084

    IL_005f:  ldloca.s   V_7
    IL_0061:  call       instance unsigned int8 [System.Drawing]System.Drawing.Color::get_R()
    IL_0066:  ldc.i4     0x10000
    IL_006b:  mul
    IL_006c:  ldloca.s   V_7
    IL_006e:  call       instance unsigned int8 [System.Drawing]System.Drawing.Color::get_G()
    IL_0073:  ldc.i4     0x100
    IL_0078:  mul
    IL_0079:  add
    IL_007a:  ldloca.s   V_7
    IL_007c:  call       instance unsigned int8 [System.Drawing]System.Drawing.Color::get_B()
    IL_0081:  add
    IL_0082:  stloc.s    V_8
    IL_0084:  ldloc.s    V_5
    IL_0086:  ldloc.s    V_6
    IL_0088:  ldloc.s    V_8
    IL_008a:  box [mscorlib]System.Int32
    IL_008f:  stelem.ref
    IL_0090:  ldloc.s    V_6
    IL_0092:  ldc.i4.1
    IL_0093:  add
    IL_0094:  stloc.s    V_6
    IL_0096:  ldloc.s    V_6
    IL_0098:  ldloc.2
    IL_0099:  blt.s      IL_0045

    IL_009b:  ldloc.3
    IL_009c:  ldloc.s    V_4
    IL_009e:  ldloc.s    V_5
    IL_00a0:  stelem.ref
    IL_00a1:  ldloc.s    V_4
    IL_00a3:  ldc.i4.1
    IL_00a4:  add
    IL_00a5:  stloc.s    V_4
    IL_00a7:  ldloc.s    V_4
    IL_00a9:  ldloc.1
    IL_00aa:  blt.s      IL_002c

    IL_00ac:  ldloc.3
    IL_00ad:  stloc.s    V_9
    IL_00af:  br.s       IL_00b1

    IL_00b1:  ldloc.s    V_9
    IL_00b3:  ret
  } // end of method complents_graphics::gr_dump_image


	
  .method public hidebysig static int32  gr_blit_image(object img, int32 x, int32 y) cil managed
  {
    // Code size       103 (0x67)
    .maxstack  4
    .locals init (class [System.Drawing]System.Drawing.Bitmap V_0,
             int32 V_1,
             int32 V_2,
             class [System.Drawing]System.Drawing.Bitmap V_3,
             int32 V_4,
             int32 V_5,
             valuetype [System.Drawing]System.Drawing.Color V_6,
             int32 V_7)
    IL_0000:  ldarg.0
    IL_0001:  castclass  [System.Drawing]System.Drawing.Bitmap
    IL_0006:  stloc.0
    IL_0007:  ldloc.0
    IL_0008:  callvirt   instance int32 [System.Drawing]System.Drawing.Image::get_Width()
    IL_000d:  stloc.1
    IL_000e:  ldloc.0
    IL_000f:  callvirt   instance int32 [System.Drawing]System.Drawing.Image::get_Height()
    IL_0014:  stloc.2
    IL_0015:  ldsfld     class [System.Drawing]System.Drawing.Bitmap CamIL.Graphics::im
    IL_001a:  stloc.3
    IL_001b:  ldc.i4.0
    IL_001c:  stloc.s    V_4
    IL_001e:  br.s       IL_005a

    IL_0020:  ldc.i4.0
    IL_0021:  stloc.s    V_5
    IL_0023:  br.s       IL_004f

    IL_0025:  ldloc.3
    IL_0026:  ldarg.1
    IL_0027:  ldloc.s    V_5
    IL_0029:  add
    IL_002a:  call       int32 CamIL.Graphics::gr_size_y()
    IL_002f:  ldarg.2
    IL_0030:  sub
    IL_0031:  ldloc.2
    IL_0032:  sub
    IL_0033:  ldloc.s    V_4
    IL_0035:  add
    IL_0036:  callvirt   instance valuetype [System.Drawing]System.Drawing.Color [System.Drawing]System.Drawing.Bitmap::GetPixel(int32,
                                                                                                                                 int32)
    IL_003b:  stloc.s    V_6
    IL_003d:  ldloc.0
    IL_003e:  ldloc.s    V_5
    IL_0040:  ldloc.s    V_4
    IL_0042:  ldloc.s    V_6
    IL_0044:  callvirt   instance void [System.Drawing]System.Drawing.Bitmap::SetPixel(int32,
                                                                                       int32,
                                                                                       valuetype [System.Drawing]System.Drawing.Color)
    IL_0049:  ldloc.s    V_5
    IL_004b:  ldc.i4.1
    IL_004c:  add
    IL_004d:  stloc.s    V_5
    IL_004f:  ldloc.s    V_5
    IL_0051:  ldloc.1
    IL_0052:  blt.s      IL_0025

    IL_0054:  ldloc.s    V_4
    IL_0056:  ldc.i4.1
    IL_0057:  add
    IL_0058:  stloc.s    V_4
    IL_005a:  ldloc.s    V_4
    IL_005c:  ldloc.2
    IL_005d:  blt.s      IL_0020

    IL_005f:  ldc.i4.0
    IL_0060:  stloc.s    V_7
    IL_0062:  br.s       IL_0064

    IL_0064:  ldloc.s    V_7
    IL_0066:  ret
  } // end of method complements_graphics::gr_blit_image

	
////////////////////////////////////////////////////////////////////////////////////////////

	
    .method family hidebysig virtual instance void 
            OnMouseDown(class [System.Windows.Forms]System.Windows.Forms.MouseEventArgs e) cil managed
    {
      // Code size       133 (0x85)
      .maxstack  5
      .locals init (object V_0,
               valuetype CamIL.Graphics/evenement[] V_1)
      IL_0000:  ldsfld     object CamIL.Graphics::current_state_lock
      IL_0005:  dup
      IL_0006:  stloc.0
      IL_0007:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_000c:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_0011:  ldarg.1
        IL_0012:  callvirt   instance int32 [System.Windows.Forms]System.Windows.Forms.MouseEventArgs::get_X()
        IL_0017:  stfld      int32 CamIL.Graphics/state::X
        IL_001c:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_0021:  ldarg.1
        IL_0022:  callvirt   instance int32 [System.Windows.Forms]System.Windows.Forms.MouseEventArgs::get_Y()
        IL_0027:  stfld      int32 CamIL.Graphics/state::Y
        IL_002c:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_0031:  ldc.i4.1
        IL_0032:  stfld      bool CamIL.Graphics/state::Clic
        IL_0037:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
        IL_003c:  dup
        IL_003d:  stloc.1
        IL_003e:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
        .try
        {
          IL_0043:  ldc.i4.0
          IL_0044:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_0049:  ldfld      int32 CamIL.Graphics/state::X
          IL_004e:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_0053:  ldfld      int32 CamIL.Graphics/state::Y
          IL_0058:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_005d:  ldfld      bool CamIL.Graphics/state::Clic
          IL_0062:  ldc.i4.0
          IL_0063:  call       void CamIL.Graphics::gr_enqueue_event(int32,
                                                                     int32,
                                                                     int32,
                                                                     bool,
                                                                     int32)
          IL_0068:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
          IL_006d:  call       void [mscorlib]System.Threading.Monitor::PulseAll(object)
          IL_0072:  leave.s    IL_007b

        }  // end .try
        finally
        {
          IL_0074:  ldloc.1
          IL_0075:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
          IL_007a:  endfinally
        }  // end handler
        IL_007b:  leave.s    IL_0084

      }  // end .try
      finally
      {
        IL_007d:  ldloc.0
        IL_007e:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_0083:  endfinally
      }  // end handler
      IL_0084:  ret
    } // end of method Graphics::OnMouseDown

    .method family hidebysig virtual instance void 
            OnMouseUp(class [System.Windows.Forms]System.Windows.Forms.MouseEventArgs e) cil managed
    {
      // Code size       133 (0x85)
      .maxstack  5
      .locals init (object V_0,
               valuetype CamIL.Graphics/evenement[] V_1)
      IL_0000:  ldsfld     object CamIL.Graphics::current_state_lock
      IL_0005:  dup
      IL_0006:  stloc.0
      IL_0007:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_000c:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_0011:  ldarg.1
        IL_0012:  callvirt   instance int32 [System.Windows.Forms]System.Windows.Forms.MouseEventArgs::get_X()
        IL_0017:  stfld      int32 CamIL.Graphics/state::X
        IL_001c:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_0021:  ldarg.1
        IL_0022:  callvirt   instance int32 [System.Windows.Forms]System.Windows.Forms.MouseEventArgs::get_Y()
        IL_0027:  stfld      int32 CamIL.Graphics/state::Y
        IL_002c:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_0031:  ldc.i4.0
        IL_0032:  stfld      bool CamIL.Graphics/state::Clic
        IL_0037:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
        IL_003c:  dup
        IL_003d:  stloc.1
        IL_003e:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
        .try
        {
          IL_0043:  ldc.i4.1
          IL_0044:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_0049:  ldfld      int32 CamIL.Graphics/state::X
          IL_004e:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_0053:  ldfld      int32 CamIL.Graphics/state::Y
          IL_0058:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_005d:  ldfld      bool CamIL.Graphics/state::Clic
          IL_0062:  ldc.i4.0
          IL_0063:  call       void CamIL.Graphics::gr_enqueue_event(int32,
                                                                     int32,
                                                                     int32,
                                                                     bool,
                                                                     int32)
          IL_0068:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
          IL_006d:  call       void [mscorlib]System.Threading.Monitor::PulseAll(object)
          IL_0072:  leave.s    IL_007b

        }  // end .try
        finally
        {
          IL_0074:  ldloc.1
          IL_0075:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
          IL_007a:  endfinally
        }  // end handler
        IL_007b:  leave.s    IL_0084

      }  // end .try
      finally
      {
        IL_007d:  ldloc.0
        IL_007e:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_0083:  endfinally
      }  // end handler
      IL_0084:  ret
    } // end of method Graphics::OnMouseUp

    .method family hidebysig virtual instance void 
            OnMouseMove(class [System.Windows.Forms]System.Windows.Forms.MouseEventArgs e) cil managed
    {
      // Code size       122 (0x7a)
      .maxstack  5
      .locals init (object V_0,
               valuetype CamIL.Graphics/evenement[] V_1)
      IL_0000:  ldsfld     object CamIL.Graphics::current_state_lock
      IL_0005:  dup
      IL_0006:  stloc.0
      IL_0007:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_000c:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_0011:  ldarg.1
        IL_0012:  callvirt   instance int32 [System.Windows.Forms]System.Windows.Forms.MouseEventArgs::get_X()
        IL_0017:  stfld      int32 CamIL.Graphics/state::X
        IL_001c:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_0021:  ldarg.1
        IL_0022:  callvirt   instance int32 [System.Windows.Forms]System.Windows.Forms.MouseEventArgs::get_Y()
        IL_0027:  stfld      int32 CamIL.Graphics/state::Y
        IL_002c:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
        IL_0031:  dup
        IL_0032:  stloc.1
        IL_0033:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
        .try
        {
          IL_0038:  ldc.i4.3
          IL_0039:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_003e:  ldfld      int32 CamIL.Graphics/state::X
          IL_0043:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_0048:  ldfld      int32 CamIL.Graphics/state::Y
          IL_004d:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_0052:  ldfld      bool CamIL.Graphics/state::Clic
          IL_0057:  ldc.i4.0
          IL_0058:  call       void CamIL.Graphics::gr_enqueue_event(int32,
                                                                     int32,
                                                                     int32,
                                                                     bool,
                                                                     int32)
          IL_005d:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
          IL_0062:  call       void [mscorlib]System.Threading.Monitor::PulseAll(object)
          IL_0067:  leave.s    IL_0070

        }  // end .try
        finally
        {
          IL_0069:  ldloc.1
          IL_006a:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
          IL_006f:  endfinally
        }  // end handler
        IL_0070:  leave.s    IL_0079

      }  // end .try
      finally
      {
        IL_0072:  ldloc.0
        IL_0073:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_0078:  endfinally
      }  // end handler
      IL_0079:  ret
    } // end of method Graphics::OnMouseMove

    .method family hidebysig virtual instance void 
            OnKeyPress(class [System.Windows.Forms]System.Windows.Forms.KeyPressEventArgs e) cil managed
    {
      // Code size       105 (0x69)
      .maxstack  6
      .locals init (object V_0,
               valuetype CamIL.Graphics/evenement[] V_1)
      IL_0000:  ldsfld     object CamIL.Graphics::current_state_lock
      IL_0005:  dup
      IL_0006:  stloc.0
      IL_0007:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_000c:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
        IL_0011:  dup
        IL_0012:  stloc.1
        IL_0013:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
        .try
        {
          IL_0018:  ldc.i4.2  // code de la touche pressee
          IL_0019:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_001e:  ldfld      int32 CamIL.Graphics/state::X
          IL_0023:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_0028:  ldfld      int32 CamIL.Graphics/state::Y
          IL_002d:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
          IL_0032:  ldfld      bool CamIL.Graphics/state::Clic
          IL_0037:  ldarg.1
          IL_0038:  callvirt   instance char [System.Windows.Forms]System.Windows.Forms.KeyPressEventArgs::get_KeyChar()
          IL_003d:  call       unsigned int8 [mscorlib]System.Convert::ToByte(char)
          IL_0042:  call       int32 [mscorlib]System.Convert::ToInt32(unsigned int8)
          IL_0047:  call       void CamIL.Graphics::gr_enqueue_event(int32,
                                                                     int32,
                                                                     int32,
                                                                     bool,
                                                                     int32)
          IL_004c:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
          IL_0051:  call       void [mscorlib]System.Threading.Monitor::PulseAll(object)
          IL_0056:  leave.s    IL_005f

        }  // end .try
        finally
        {
          IL_0058:  ldloc.1
          IL_0059:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
          IL_005e:  endfinally
        }  // end handler
        IL_005f:  leave.s    IL_0068

      }  // end .try
      finally
      {
        IL_0061:  ldloc.0
        IL_0062:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_0067:  endfinally
      }  // end handler
      IL_0068:  ret
    } // end of method Graphics::OnKeyPress

    .method public hidebysig static object[]
            gr_wait_event(object list) cil managed
    {
	// Tient compte de la représentation des listes
	// et donc des blocs
      // Code size       96 (0x60)
      .maxstack  3
      .locals init (bool V_0,
               bool[] V_1,
               class CamIL.Variant V_2,
               int32 V_3,
               object[] V_4)
      IL_0000:  ldc.i4.0
      IL_0001:  stloc.0
      IL_0002:  ldc.i4.4
      IL_0003:  newarr     [mscorlib]System.Boolean
      IL_0008:  stloc.1
      IL_0009:  ldarg.0
		castclass CamIL.Variant
      IL_000a:  stloc.2
      IL_000b:  br.s       IL_0044
// Inspection de la liste d'événements
      IL_000d:  ldloc.2
		castclass CamIL.Cons_OF_camil_list 
		ldfld object CamIL.Cons_OF_camil_list::x0
		castclass  [mscorlib]System.Int32
		unbox  [mscorlib]System.Int32
		ldind.i4
      IL_0015:  stloc.3
      IL_0016:  ldloc.3
      IL_0017:  brtrue.s   IL_001d
// Ici on inscrit Button_down (tag 0)
      IL_0019:  ldloc.1
      IL_001a:  ldc.i4.0
      IL_001b:  ldc.i4.1
      IL_001c:  stelem.i1
      IL_001d:  ldloc.3
      IL_001e:  ldc.i4.1
      IL_001f:  bne.un.s   IL_0025
// Ici on inscrit Button_up (tag 1)
      IL_0021:  ldloc.1
      IL_0022:  ldc.i4.1
      IL_0023:  ldc.i4.1
      IL_0024:  stelem.i1
      IL_0025:  ldloc.3
      IL_0026:  ldc.i4.2
      IL_0027:  bne.un.s   IL_002d
// Ici on inscrit Key_pressed (tag 2)
      IL_0029:  ldloc.1
      IL_002a:  ldc.i4.2
      IL_002b:  ldc.i4.1
      IL_002c:  stelem.i1
      IL_002d:  ldloc.3
      IL_002e:  ldc.i4.3
      IL_002f:  bne.un.s   IL_0035
// Ici on inscrit Mouse_motion (tag 3)
      IL_0031:  ldloc.1
      IL_0032:  ldc.i4.3
      IL_0033:  ldc.i4.1
      IL_0034:  stelem.i1
      IL_0035:  ldloc.3
      IL_0036:  ldc.i4.4
      IL_0037:  bne.un.s   IL_003b
// Ici on inscrit Poll (tag 4)
      IL_0039:  ldc.i4.1
      IL_003a:  stloc.0
      IL_003b:  ldloc.2
		castclass CamIL.Cons_OF_camil_list 
		ldfld class CamIL.Variant CamIL.Cons_OF_camil_list::x1
      IL_0043:  stloc.2
      IL_0044:  ldloc.2
		ldfld int32 CamIL.Variant::tag
      IL_0045:  brtrue.s   IL_000d
// Liste examinée en entier
	
      IL_0047:  ldloc.0
      IL_0048:  brfalse.s  IL_0053

      IL_004a:  call       object[] CamIL.Graphics::gr_wait_event_poll()
      IL_004f:  stloc.s    V_4
      IL_0051:  br.s       IL_005d

      IL_0053:  ldloc.1
      IL_0054:  call       object[] CamIL.Graphics::gr_wait_event_blocking(bool[])
      IL_0059:  stloc.s    V_4
      IL_005b:  br.s       IL_005d

      IL_005d:  ldloc.s    V_4
      IL_005f:  ret
    } // end of method Graphics::gr_wait_event

    .method private hidebysig static object[] 
            gr_wait_event_poll() cil managed
    {
      // Code size       162 (0xa2)
      .maxstack  5
      .locals init (int32 V_0,
               bool V_1,
               int32 V_2,
               object[] V_3,
               object[] V_4,
               valuetype CamIL.Graphics/evenement[] V_5,
               object V_6)
      IL_0000:  ldc.i4.0
      IL_0001:  stloc.0
      IL_0002:  ldc.i4.0
      IL_0003:  stloc.1
      IL_0004:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
      IL_0009:  dup
      IL_000a:  stloc.s    V_5
      IL_000c:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_0011:  ldsfld     int32 CamIL.Graphics::gr_head
        IL_0016:  stloc.2
        IL_0017:  br.s       IL_004b

        IL_0019:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
        IL_001e:  ldloc.2
        IL_001f:  ldelema    CamIL.Graphics/evenement
        IL_0024:  ldfld      int32 CamIL.Graphics/evenement::kind
        IL_0029:  ldc.i4.2			// 2 est le numero d'evenement de touche pressee
        IL_002a:  bne.un.s   IL_0041

        IL_002c:  ldc.i4.1
        IL_002d:  stloc.1
        IL_002e:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
        IL_0033:  ldloc.2
        IL_0034:  ldelema    CamIL.Graphics/evenement
        IL_0039:  ldfld      int32 CamIL.Graphics/evenement::key
        IL_003e:  stloc.0
		  // on a trouve l'evenement qui nous interessait, ne pas oublier de retrecir la queue
		  ldloc.2
		  ldc.i4.1
		  add
		  ldsfld int32 CamIL.Graphics::size_queue
		  rem
	          stsfld int32 CamIL.Graphics::gr_head
	
        IL_003f:  br.s       IL_0053

        IL_0041:  ldloc.2
        IL_0042:  ldc.i4.1
        IL_0043:  add
        IL_0044:  ldsfld     int32 CamIL.Graphics::size_queue
        IL_0049:  rem
        IL_004a:  stloc.2
        IL_004b:  ldloc.2
        IL_004c:  ldsfld     int32 CamIL.Graphics::gr_tail
        IL_0051:  bne.un.s   IL_0019

        IL_0053:  leave.s    IL_005d

      }  // end .try
      finally
      {
        IL_0055:  ldloc.s    V_5
        IL_0057:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_005c:  endfinally
      }  // end handler
      IL_005d:  ldsfld     object CamIL.Graphics::current_state_lock
      IL_0062:  dup
      IL_0063:  stloc.s    V_6
      IL_0065:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_006a:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_006f:  ldfld      int32 CamIL.Graphics/state::X
        IL_0074:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_0079:  ldfld      int32 CamIL.Graphics/state::Y
        IL_007e:  ldsflda    valuetype CamIL.Graphics/state CamIL.Graphics::current_state
        IL_0083:  ldfld      bool CamIL.Graphics/state::Clic
        IL_0088:  ldloc.1					// si touche pressee
        IL_0089:  ldloc.0					// et quelle touche
        IL_008a:  call       object[] CamIL.Graphics::gr_wait_allocate_result(int32,
                                                                              int32,
                                                                              bool,
                                                                              bool,
                                                                              int32)
        IL_008f:  stloc.3
        IL_0090:  leave.s    IL_009a

      }  // end .try
      finally
      {
        IL_0092:  ldloc.s    V_6
        IL_0094:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_0099:  endfinally
      }  // end handler
      IL_009a:  ldloc.3
      IL_009b:  stloc.s    V_4
      IL_009d:  br.s       IL_009f

      IL_009f:  ldloc.s    V_4
      IL_00a1:  ret
    } // end of method Graphics::gr_wait_event_poll

    .method private hidebysig static object[] 
            gr_wait_event_blocking(bool[] mask) cil managed
    {
      // Code size       52 (0x34)
      .maxstack  2
      .locals init (object[] V_0,
               object[] V_1,
               valuetype CamIL.Graphics/evenement[] V_2)
      IL_0000:  br.s       IL_0030

      IL_0002:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
      IL_0007:  dup
      IL_0008:  stloc.2
      IL_0009:  call       void [mscorlib]System.Threading.Monitor::Enter(object)
      .try
      {
        IL_000e:  ldarg.0
        IL_000f:  call       object[] CamIL.Graphics::gr_wait_event_in_queue(bool[])
        IL_0014:  stloc.0
        IL_0015:  ldloc.0
        IL_0016:  brfalse.s  IL_001c

        IL_0018:  ldloc.0
        IL_0019:  stloc.1
        IL_001a:  leave.s    IL_0032

        IL_001c:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
        IL_0021:  call       bool [mscorlib]System.Threading.Monitor::Wait(object)
        IL_0026:  pop
        IL_0027:  leave.s    IL_0030

      }  // end .try
      finally
      {
        IL_0029:  ldloc.2
        IL_002a:  call       void [mscorlib]System.Threading.Monitor::Exit(object)
        IL_002f:  endfinally
      }  // end handler
      IL_0030:  br.s       IL_0002

      IL_0032:  ldloc.1
      IL_0033:  ret
    } // end of method Graphics::gr_wait_event_blocking

    .method private hidebysig static object[] 
            gr_wait_allocate_result(int32 mouse_x,
                                    int32 mouse_y,
                                    bool button,
                                    bool keypressed,
                                    int32 key) cil managed
    {
      // Code size       71 (0x47)
      .maxstack  5
      .locals init (object[] V_0,
               object[] V_1)
      IL_0000:  ldc.i4.5
      IL_0001:  newarr     [mscorlib]System.Object
      IL_0006:  stloc.0
      IL_0007:  ldloc.0
      IL_0008:  ldc.i4.0
      IL_0009:  ldarg.0
      IL_000a:  box [mscorlib]System.Int32
      IL_000f:  stelem.ref
      IL_0010:  ldloc.0
      IL_0011:  ldc.i4.1
		call int32 CamIL.Graphics::gr_size_y()
      IL_0012:  ldarg.1
		sub		
	
      IL_0013:  box [mscorlib]System.Int32
      IL_0018:  stelem.ref
      IL_0019:  ldloc.0
      IL_001a:  ldc.i4.2
      IL_001b:  ldarg.2
      IL_001c:  brtrue.s   IL_0021

		ldc.i4.0
		box [mscorlib]System.Int32
      IL_001f:  br.s       IL_0027

      IL_0021:  ldc.i4.1
      IL_0022:  box [mscorlib]System.Int32
      IL_0027:  stelem.ref
      IL_0028:  ldloc.0
      IL_0029:  ldc.i4.3
      IL_002a:  ldarg.3
      IL_002b:  brtrue.s   IL_0030

	        ldc.i4.0
		box [mscorlib]System.Int32
      IL_002e:  br.s       IL_0036

      IL_0030:  ldc.i4.1
      IL_0031:  box [mscorlib]System.Int32
      IL_0036:  stelem.ref
      IL_0037:  ldloc.0
      IL_0038:  ldc.i4.4
      IL_0039:  ldarg.s    key
      IL_003b:  box [mscorlib]System.Int32
      IL_0040:  stelem.ref
      IL_0041:  ldloc.0
      IL_0042:  stloc.1
      IL_0043:  br.s       IL_0045

      IL_0045:  ldloc.1
      IL_0046:  ret
    } // end of method Graphics::gr_wait_allocate_result

    .method private hidebysig static object[] 
            gr_wait_event_in_queue(bool[] mask) cil managed
    {
      // Code size       170 (0xaa)
      .maxstack  5
      .locals init (valuetype CamIL.Graphics/evenement V_0,
               object[] V_1)
      IL_0000:  br         IL_0095

      IL_0005:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
      IL_000a:  ldsfld     int32 CamIL.Graphics::gr_head
      IL_000f:  ldelema    CamIL.Graphics/evenement
      IL_0014:  ldobj      CamIL.Graphics/evenement
      IL_0019:  stloc.0
      IL_001a:  ldsfld     int32 CamIL.Graphics::gr_head
      IL_001f:  ldc.i4.1
      IL_0020:  add
      IL_0021:  ldsfld     int32 CamIL.Graphics::size_queue
      IL_0026:  rem
      IL_0027:  stsfld     int32 CamIL.Graphics::gr_head
      IL_002c:  ldloca.s   V_0
      IL_002e:  ldfld      int32 CamIL.Graphics/evenement::kind
      IL_0033:  ldc.i4.2
      IL_0034:  bne.un.s   IL_003b
	// ici on traite un evenement touche pressee
      IL_0036:  ldarg.0
      IL_0037:  ldc.i4.2
      IL_0038:  ldelem.i1
      IL_0039:  brtrue.s   IL_0067 // on y va s'il etait prevu d'attendre une touche pressee

      IL_003b:  ldloca.s   V_0
      IL_003d:  ldfld      int32 CamIL.Graphics/evenement::kind
      IL_0042:  brtrue.s   IL_0049

      IL_0044:  ldarg.0
      IL_0045:  ldc.i4.0
      IL_0046:  ldelem.i1
      IL_0047:  brtrue.s   IL_0067

      IL_0049:  ldloca.s   V_0
      IL_004b:  ldfld      int32 CamIL.Graphics/evenement::kind
      IL_0050:  ldc.i4.1
      IL_0051:  bne.un.s   IL_0058

      IL_0053:  ldarg.0
      IL_0054:  ldc.i4.1
      IL_0055:  ldelem.i1
      IL_0056:  brtrue.s   IL_0067

      IL_0058:  ldloca.s   V_0
      IL_005a:  ldfld      int32 CamIL.Graphics/evenement::kind
      IL_005f:  ldc.i4.3
      IL_0060:  bne.un.s   IL_0095

      IL_0062:  ldarg.0
      IL_0063:  ldc.i4.3
      IL_0064:  ldelem.i1
      IL_0065:  brfalse.s  IL_0095

      IL_0067:  ldloca.s   V_0
      IL_0069:  ldfld      int32 CamIL.Graphics/evenement::mouse_x
      IL_006e:  ldloca.s   V_0
      IL_0070:  ldfld      int32 CamIL.Graphics/evenement::mouse_y
      IL_0075:  ldloca.s   V_0
      IL_0077:  ldfld      bool CamIL.Graphics/evenement::button
      IL_007c:  ldloca.s   V_0
      IL_007e:  ldfld      int32 CamIL.Graphics/evenement::kind
      IL_0083:  ldc.i4.2
      IL_0084:  ceq
      IL_0086:  ldloca.s   V_0
      IL_0088:  ldfld      int32 CamIL.Graphics/evenement::key
      IL_008d:  call       object[] CamIL.Graphics::gr_wait_allocate_result(int32,
                                                                            int32,
                                                                            bool,
                                                                            bool,
                                                                            int32)
      IL_0092:  stloc.1
      IL_0093:  br.s       IL_00a8

      IL_0095:  ldsfld     int32 CamIL.Graphics::gr_head
      IL_009a:  ldsfld     int32 CamIL.Graphics::gr_tail
      IL_009f:  bne.un     IL_0005

      IL_00a4:  ldnull
      IL_00a5:  stloc.1
      IL_00a6:  br.s       IL_00a8

      IL_00a8:  ldloc.1
      IL_00a9:  ret
    } // end of method Graphics::gr_wait_event_in_queue

    .method private hidebysig static void 
            gr_enqueue_event(int32 kind,
                             int32 mouse_x,
                             int32 mouse_y,
                             bool button,
                             int32 key) cil managed
    {
      // Code size       57 (0x39)
      .maxstack  6

	        ldsfld     int32 CamIL.Graphics::gr_head
		ldsfld     int32 CamIL.Graphics::gr_tail
		ldc.i4.1
		add
		ldsfld int32 CamIL.Graphics::size_queue
		rem
		beq.s IL_0038 // si la queue est pleine, on n'enregistre pas	
		
		
//      IL_000a:  bne.un.s   IL_0038

      IL_000c:  ldsfld     valuetype CamIL.Graphics/evenement[] CamIL.Graphics::gr_queue
      IL_0011:  ldsfld     int32 CamIL.Graphics::gr_tail
      IL_0016:  ldelema    CamIL.Graphics/evenement
      IL_001b:  ldarg.0
      IL_001c:  ldarg.1
      IL_001d:  ldarg.2
      IL_001e:  ldarg.3
      IL_001f:  ldarg.s    key
      IL_0021:  call       instance void CamIL.Graphics/evenement::.ctor(int32,
                                                                         int32,
                                                                         int32,
                                                                         bool,
                                                                         int32)
      IL_0026:  ldsfld     int32 CamIL.Graphics::gr_tail
      IL_002b:  ldc.i4.1
      IL_002c:  add
      IL_002d:  ldsfld     int32 CamIL.Graphics::size_queue
      IL_0032:  rem
      IL_0033:  stsfld     int32 CamIL.Graphics::gr_tail
      IL_0038:  ret
    } // end of method Graphics::gr_enqueue_event

  } // end of class Graphics
