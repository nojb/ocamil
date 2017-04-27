/////////////////////////////////////////////////////////////////////////
//                                                                     // 
//                               CamIL                                 // 
//                                                                     // 
//           Raphael Montelatici      Equipe PPS                       //
//                                                                     //
///////////////////////////////////////////////////////////////////////// 

//Lignes commentées pour la version (core_camil, stdlib) fondus
	
//.assembly extern mscorlib
//{
//  .publickeytoken = (%%MSCORLIBTOKEN%%)
//  .ver %%MSCORLIBVER%%
//}

.assembly extern System
{
  .publickeytoken =  (%%MSCORLIBTOKEN%%)                       // .z\V.4..
  .ver %%MSCORLIBVER%%
}

.assembly extern System.Windows.Forms
{
  .publickeytoken = (%%MSCORLIBTOKEN%%)
  .ver %%MSCORLIBVER%%
}
	
.assembly extern System.Drawing
{
  .publickeytoken = (%%SYSTEMDRAWINGTOKEN%%)
  .ver %%MSCORLIBVER%%
}
	
//.assembly core_camil
//{
//  .hash algorithm 0x00008004
//  .ver 0:0:0:0
//}
	
//.module core_camil.dll
		
.namespace CamIL 
{
