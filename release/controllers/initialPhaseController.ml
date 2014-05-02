open Definition
open Constant
open Util
open Print
open ResourceManager


let structs ((b,_,_,_) : state) = 
	let (_,s,_,_,_) = b in s

let turn ((b,_,_,_) : state) = 
	let (_,s,_,_,_) = b in
	let (_,roads) = s in
	List.length roads

let fwd ((b,p,t,(c,r)) : state) =
	(b,p,t,(next_turn c,r))

let bwd ((b,p,t,(c,r)) : state) = 
	(b,p,t,(prev_turn c,r))

let is_valid game (p1,p2) = 
	let (is,roads) = structs game in
	let settlement = (List.nth is p1) in
	let empty_s = is_none settlement in
	let empty_r = not (List.exists (fun (_,(x,y)) -> ((x = p1) && (y = p2)) || ((x = p2) && (y = p1))) roads)
	in empty_r && empty_s && (p1 <> p2) 

let valid_line game = 
	let rec valid_line p1 = 
		let out_lines = List.map (fun x -> (p1,x)) (adjacent_points p1) in
		if List.exists (is_valid  game) out_lines then List.find (is_valid game) out_lines
		else valid_line (p1+1) 
	in valid_line 0 

let place_structs (((map,s,deck,dis,robber),p,t,(c,r)) as game :state) (p1,p2) =
	let (is,roads) = structs game in
	let is' = List.mapi (fun i x -> if i = p1 then Some(c,Town) else x) is in
	let roads' = (c,(p1,p2))::roads in
	((map,(is',roads'),deck,dis,robber),p,t,(c,r))

let place_structs_and_fetch (((map,s,deck,dis,robber),p,t,(c,r)) as game :state) (p1,p2) =
	let (is,roads) = structs game in
	let is' = List.mapi (fun i x -> if i = p1 then Some(c,Town) else x) is in
	let roads' = (c,(p1,p2))::roads in
	let p' = ResourceManager.fetch_resources c (p1,Town) p (fst map) in
	((map,(is',roads'),deck,dis,robber),p',t,(c,r))	


let handle_move game line  = 
	let handle_place game line pick_res = 
		if not pick_res then 
			(if is_valid game line then None, place_structs (game) (line)
			else let l = valid_line game in None, place_structs (game) (l))
		else
			(if is_valid game line then None, place_structs_and_fetch (game) (line)
			else let l = valid_line game in None, place_structs_and_fetch (game) (l))	
	in 
	if turn (game) < 4 then handle_place (fwd game) line false
	else if turn(game) = 4 then handle_place (game) line true
	else if turn(game) = 8 then None, game
	else handle_place (bwd game) line true