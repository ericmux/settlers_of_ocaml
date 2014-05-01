open Definition
open Constant
open Util
open Print
open InitialPhaseController

type game = state

let state_of_game g = g
let game_of_state s = s


let init_game () = game_of_state (gen_initial_state())


let handle_move ((_,_,_,(c,r)) as g: game) m = 
	print_update c m g; 
	match r with
	  | InitialRequest	-> InitialPhaseController.handle_move (g) (m)
      | RobberRequest	-> None, g
      | DiscardRequest	-> None, g
      | TradeRequest  	-> None, g
      | ActionRequest 	-> None, g

let presentation s = s
