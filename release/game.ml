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
	match m with
	  | InitialMove(p1,p2)		-> InitialPhaseController.handle_move (g) (p1,p2)
      | RobberMove(p,c)			-> None, g
      | DiscardMove(resources)	-> None, g
      | TradeResponse(accept)  	-> None, g
      | Action(action) 			-> None, g

let presentation s = s
