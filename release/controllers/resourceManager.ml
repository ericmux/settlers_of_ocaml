open Definition
open Constant
open Util
open Print



let denormalize_resources (b1,w1,o1,l1,g1) = 
	(cRESOURCES_GENERATED_TOWN*b1, cRESOURCES_GENERATED_TOWN*w1, cRESOURCES_GENERATED_TOWN*o1,
	cRESOURCES_GENERATED_TOWN*l1, cRESOURCES_GENERATED_TOWN*g1)

let sum_resources (b1,w1,o1,l1,g1) (b2,w2,o2,l2,g2) = (b1 + b2, w1 + w2, o1 + o2, l1 + l2, g1 + g2)

let filter_hexes plist hex_list = 
	List.filter (fun (t,r) -> r <> 0) (List.mapi (fun i (t,r) -> if List.mem i plist then (t,r) else (t,0)) hex_list)



(* To be used to update a player's resource based on all of his settlements *)
let tick_resources color game = game

(* Generates resources for player [color] from the settlement placed at [p1]  *)
let generate_resources color (p1,s) players hex_list = 
	let add_res ((c,(inv,cards),troph) as p : player) = 
		if color <> c then p
		else let pieces = adjacent_pieces p1 in 
		let adj_hexes = filter_hexes pieces hex_list in
		let adj_hexes = List.map (fun (t,r) -> get_some (resource_of_terrain t)) adj_hexes in
		let adj_hexes = List.map single_resource_cost adj_hexes in
		let adj_hexes = List.map denormalize_resources adj_hexes in  
		let res_to_add = List.fold_left sum_resources (0,0,0,0,0) adj_hexes in
		(c,(res_to_add,cards),troph)
	in List.map add_res players


