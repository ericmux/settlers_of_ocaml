open Definition
open Constant
open Util
open Print
open InitialPhaseController
open ActionController

type game = state

let state_of_game g = g
let game_of_state s = s


let init_game () = game_of_state (gen_initial_state())


let handle_move ((_,_,_,(c,r)) as g: game) m = 
	let print_update (w,g) = print_update c m g; w,g in 
	match m with
	  | InitialMove(p1,p2)		-> print_update (InitialPhaseController.handle_move (g) (p1,p2))
      | RobberMove(p,c)			-> print_update (None, g)
      | DiscardMove(resources)	-> print_update (None, g)
      | TradeResponse(accept)  	-> print_update (None, g)
      | Action(action) 			-> match action with 
      								  RollDice 					   -> print_update (ActionController.roll_dice g)
            						| MaritimeTrade(maritimetrade) -> print_update (None, g)
            						| DomesticTrade(trade)		   -> print_update (None, g)
            						| BuyBuild(build)			   -> print_update (None, g)
            						| PlayCard(playcard)		   -> print_update (None, g)
            						| EndTurn					   -> print_update (ActionController.end_turn g)

let presentation s = s
