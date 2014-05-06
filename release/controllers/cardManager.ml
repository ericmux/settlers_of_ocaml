open Definition
open Constant
open Util
open Print
open ResourceManager

let deck_empty deck =
	let cards = reveal deck in 
	(List.length cards) = 0 



(* Generates a card if the player has enough resources, otherwise just returns the unmodified game. *)
let generate_card ((b,p,t,(c,r)) as game: state) = 
	if not (ResourceManager.active_player_can_afford game cCOST_CARD) then game
	else 
		let (_,_,deck,_,_) = b in  
		if deck_empty deck then game
		else 
			let deck = reveal deck in
			let card = get_some (pick_random deck) in
			let t' = {
				active = t.active;
				dicerolled = t.dicerolled;
				cardplayed = t.cardplayed;
				cardsbought = append_card (t.cardsbought) (card);
				tradesmade = t.tradesmade;
				pendingtrade = t.pendingtrade;
			} in ResourceManager.apply_cost_to_player (b,p,t',(c,r)) c cCOST_CARD

(* Merges all the built cards to the active player's hand. *)
let merge_cardsbought ((b,p,t,(color,r)) : state) = 
	let rec merge_cards cards cardsbought = 
		match cardsbought with
			| [] -> wrap_reveal cards
			| h::tl -> merge_cards (h::cards) (tl)
	in let map_cards (c,(inv,cards),troph) = 
		if (color = c) then (c,(inv,merge_cards (reveal cards) (reveal t.cardsbought)),troph) else (c,(inv,cards),troph)
	in let p' = List.map map_cards p
	in (b,p',t,(color,r))