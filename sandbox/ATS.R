#### Automated Trading System State Machine ####

# components
################
# DataHandler -> MarketEvent -> Strategy -> SignalEvent -> Portfolio -> OrderEvent
# -> ExecutionHandler -> FillEvent -> Portfolio
# EventQueue = list(Events)
# Event = c("Market", "Signal", "Order", "Fill")
# SignalEvent = list(Symbol, Direction=c("Long", "Short"), timestamp)
# Portfolio = OrderManagementSystem (OMS) + RiskManagement
#    = c("Risk Management"=c("Exposure", "Position Sizing")) 

# DataHandler
#   updateBars


ExecutionHandler <- setRefClass("ExecutionHandler",
                        fields = list(
                        ),
                        methods = list(
                          executeOrder = function(o){
                            'When an ExecutionHandler receives an OrderEvent 
                            it must transact the order. Once an order has been 
                            transacted it generates a FillEvent, which describes
                            the cost of purchase or sale as well as the 
                            transaction costs, such as fees or slippage.'
                            print("Order executed from order event")
                          }
                        )
)

####################
# # Declare the components with respective parameters
events = list()
events$get= function(tf) simpleError("test error")

startMachine <- function(){
  bars = DTHandler(continue.backtest=T)
  strategy = Strategy()
  port = Portfolio()
  broker = ExecutionHandler()
  
  while(TRUE){
    # Update the bars (specific backtest code, as opposed to live trading)
    if (bars$continue.backtest == TRUE)
      bars$updateBars()
    else
      break
    
    # Handle the events
    while(FALSE){
      tryCatch(event = events$get(FALSE), error = function(e) break, finally= break)
      switch(event$type){
        'MARKET'  = {strategy$calcSignals(event); port$updateTime(event)},
        'SIGNAL'  = port$updateSignal(event),
        'ORDER'   = broker$executeOrder(event),
        'FILL'    = port$updateFill(event)
      }
      # 1-Sec heartbeat
      Sys.sleep(1)    
    }    
  }
  
# 
# while True:
#   # Update the bars (specific backtest code, as opposed to live trading)
#   if bars.continue_backtest == True:
#   bars.update_bars()
# else:
#   break
# 
# # Handle the events
# while True:
#   try:
#   event = events.get(False)
# except Queue.Empty:
#   break
# else:
#   if event is not None:
#   if event.type == 'MARKET':
#   strategy.calculate_signals(event)
# port.update_timeindex(event)
# 
# elif event.type == 'SIGNAL':
#   port.update_signal(event)
# 
# elif event.type == 'ORDER':
#   broker.execute_order(event)
# 
# elif event.type == 'FILL':
#   port.update_fill(event)
# 
# # 10-Minute heartbeat
# time.sleep(10*60)

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