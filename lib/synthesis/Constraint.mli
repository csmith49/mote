type checker = Delta.t -> Delta.t option

val keep_selector : checker
val drop_dangling_edges : checker
val stay_connected : checker
val attribute_per_node : checker
val max_nodes : int -> checker
val max_edges : int -> checker