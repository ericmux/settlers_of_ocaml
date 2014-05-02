open Definition
open Constant
open Util
open Print



let scale_resources k (b1,w1,o1,l1,g1) = 
	(k*b1, k*w1, k*o1,k*l1, k*g1)

let settlement_to_resource terrain (c,s) = 
	match s with
		| Town -> scale_resources cRESOURCES_GENERATED_TOWN (single_resource_cost (get_some (resource_of_terrain terrain)))
		| City -> scale_resources cRESOURCES_GENERATED_CITY (single_resource_cost (get_some (resource_of_terrain terrain)))

let sum_resources (b1,w1,o1,l1,g1) (b2,w2,o2,l2,g2) = (b1 + b2, w1 + w2, o1 + o2, l1 + l2, g1 + g2)

let filter_hexes plist hex_list = 
	List.filter (fun (t,r) -> r <> 0) (List.mapi (fun i (t,r) -> if List.mem i plist then (t,r) else (t,0)) hex_list)

(* get list of settlements on the corners of a hex. That is, plist should be [piece_cornerns hex_index]. *)
let get_settlements plist s = 
	let structs = List.mapi (fun i x -> if List.mem i plist && (not (is_none x)) then Some(x) else None) s
	in let structs = List.filter (fun x -> not (is_none x)) structs
	in List.map get_some structs



let tick_terrain players (settlements,t) = 
	let settlements 	= List.map get_some settlements in
	let blue_structs 	= List.filter (fun (c,s) -> c = Blue) 	settlements in
	let red_structs 	= List.filter (fun (c,s) -> c = Red) 	settlements in
	let orange_structs 	= List.filter (fun (c,s) -> c = Orange) settlements in
	let white_structs 	= List.filter (fun (c,s) -> c = White) 	settlements in
	let tick_inventory color inv = 
		match color with
			| Blue 	 -> List.fold_left sum_resources inv (List.map (settlement_to_resource t) blue_structs)
			| Red 	 -> List.fold_left sum_resources inv (List.map (settlement_to_resource t) red_structs)
			| Orange -> List.fold_left sum_resources inv (List.map (settlement_to_resource t) orange_structs)
			| White  -> List.fold_left sum_resources inv (List.map (settlement_to_resource t) white_structs)
	in List.map (fun (c,(inv,cards),troph) -> (c,((tick_inventory c inv),cards),troph)) players  


(* Generates initial resources for player [color] from the settlement placed at [p1]  *)
let generate_initial_resources color players hex_list p1 s = 
	let add_res ((c,(inv,cards),troph) as p : player) = 
		if color <> c then p
		else let pieces = adjacent_pieces p1 in 
		let adj_hexes = filter_hexes pieces hex_list in
		let adj_hexes = List.map (fun (t,r) -> resource_of_terrain t) adj_hexes in
		let adj_hexes = List.filter (fun x -> not (is_none x)) adj_hexes in
		let adj_hexes = List.map get_some adj_hexes in
		let adj_hexes = List.map single_resource_cost adj_hexes in  
		let res_to_add = List.fold_left sum_resources inv adj_hexes in
		(c,(res_to_add,cards),troph)
	in List.map add_res players

(* Used to update all player's resources based on all settlements adjacent to the rolled hexes *)
let tick_resources (((map,s,deck,dis,robber),p,t,(c,r)) :state) = 
	let settlements_to_update hex_index = get_settlements (piece_corners hex_index) (fst s)
	in let roll = get_some t.dicerolled
	in let hex_list = List.mapi (fun i (t,r) -> if r = roll then Some(i,t) else None) (fst map)
	in let hex_list = List.filter (fun x -> not (is_none x)) hex_list
	in let hex_list = List.map get_some hex_list
	in let update_jobs = List.fold_left (fun a (i,t) -> ((settlements_to_update i),t)::a ) [] hex_list
	in let p' = List.fold_left tick_terrain p update_jobs
	in ((map,s,deck,dis,robber),p',t,(c,r))



