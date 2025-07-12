!=============================================================================================
! when an error has occurred: ADD a traceback and RETURN to the caller 
!=============================================================================================
#define if_error_trace_and_RETURN(e,t) if (e%code>0) then;call e%addTrace(t);return;end if

!=============================================================================================
! when an error has occurred: ADD a traceback and EXIT from a loop 
!=============================================================================================
#define if_error_trace_and_EXIT(e,t) if (e%code>0) then;call e%addTrace(t);exit;end if

!=============================================================================================
! when an error has occurred: MOVE the g-hangler to the e-handler, ADD a traceback, RESET the 
! halting mode to its initial value and RETURN 
!=============================================================================================
#define setNtraceNreturn(g,t,e) if(g%code>0) then;call err_MoveAlloc(g,e);call e%addTrace(t);call err_resetHaltingMode(.true.);return;end if

!=============================================================================================
! when an error has occurred: MOVE the g-hangler to the e-handler, RESET the halting mode to 
! its initial value and RETURN
!=============================================================================================
#define setNreturn(g,e) if(g%code>0) then;call err_MoveAlloc(g,e);call err_resetHaltingMode(.true.);return;end if
