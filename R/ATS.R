#### Automated Trading System State Machine ####
a = c("state1", "otherstate")
STATES  = factor(1:2, levels=a,labels=a,ordered = F)

# transitions table (indexed by col 1 & 2)
# ----------------------------------------
#         current     input             next            Xition           Monitor
#           state     event            state               FUN               FUN
#         -------     -----            -----            ------           -------
#          incash  entersig      orderinbook  pmgr$writeOrders  pmgr$scanSignals
#     orderinbook         ?  orderwithbroker  trader$sendOrder                 ?
# orderwithbroker      fill      filledorder  portfolio$addTxn                 ?
#     filledorder  confrmtn     openposition                 ?                 ?
#    openposition  rebalsig     openposition                 ?  pmgr$scanSignals
#    openposition   exitsig   closedposition                 ?                 ?
#  closedposition       txn           incash  portfolio$addTxn                 ?
#
############################## 
# components
################
# DataHandler -> MarketEvent -> Strategy -> SignalEvent -> Portfolio -> 
#                                                |
# 
# SignalEvent = list(Symbol, Direction=c("Long", "Short"), timestamp)
# Portfolio = OrderManagementSystem (OMS) + RiskManagement
#    = c("Risk Management"=c("Exposure", "Position Sizing")) 


##########

# while eventPending(){
#   event = getNextEvent()
#   entry = transitions[currentState][event]
#   entry.executeAction()
#   currentState = entry.nextState()
# }
# 
# 
# current_state = STATE_RESET;
# event = EVENT_NONE;
# state_tbl_indx = 0;
# while (1)
# {
#   if (event != EVENT_NONE)
#   {
#     for (j=0; j<max_state_tbl_len; j++)
#     {
#       if (state_tbl[j].cur_state == current_state &&
#             state_tbl[j].event == event)
#       {
#         tmp_event = event;
#         event = EVENT_NONE;
#         (*state_tbl[j].transition_func)(
#           current_state,
#           state_tbl[j].new_state,
#           tmp_event);
#         current_state = state_tbl[j].new_state;
#         state_tbl_indx = j;
#         break;
#       }
#     }
#   }
#   (*state_tbl[state_tbl_indx].monitor_func)(current_state);
# }