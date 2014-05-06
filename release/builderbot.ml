open Definition
open Registry
open Constant
open Util
open ResourceManager

(** Give your bot a 2-20 character name. *)
let name = "builderbot"


module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = ()

  (* Invalid moves are overridden in game *)
  let handle_request ((_,p,t,n) as s: state) : move =
    let (c, r) = n in
    match r with
      | InitialRequest -> InitialMove(0, 0)
      | RobberRequest -> RobberMove(0, None)
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(true)
      | ActionRequest -> 
        if is_none t.dicerolled then Action(RollDice) 
        else if active_player_can_afford s cCOST_CARD then Action(BuyBuild(BuildCard)) 
        else Action(EndTurn)
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))