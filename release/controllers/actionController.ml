open Definition
open Constant
open Util
open Print
open ResourceManager
open CardManager


let reset_turn ((b,p,t,(c,r)) as game: state) =
	let (_,p',_,_) = CardManager.merge_cardsbought game
	in let t' = {
			active = c;
			dicerolled = None;
			cardplayed = false;
			cardsbought = snd (cDEFAULT_HAND);
			tradesmade = 0;
			pendingtrade = None;	
	} in (b,p',t',(c,r))


let fwd ((b,p,t,(c,r)) : state) =
	(b,p,t,(next_turn c,r))

let end_turn ((b,p,t,(c,r)) as game: state) = None, fwd (reset_turn (game))

let roll_dice ((b,p,t,(c,r)) as game: state) =
	if not (is_none t.dicerolled) then end_turn game 
	else let t' = {
				active = t.active;
				dicerolled = Some (random_roll ());
				cardplayed = t.cardplayed;
				cardsbought = t.cardsbought;
				tradesmade = t.tradesmade;
				pendingtrade = t.pendingtrade;
	} in None, (ResourceManager.tick_resources (b,p,t',(c,r)) )


let buy_build ((b,p,t,(c,r)) as game: state) build = 
	match build with
		| BuildRoad(road)  -> None, game
        | BuildTown(point) -> None, game
        | BuildCity(point) -> None, game
        | BuildCard		   -> None, CardManager.generate_card game