(*
  Brent Turner
  CSC 345 -- ML Homework
  mlfile.sml
*)
(*----------------------------------------------------------------------------*)



datatype 'a BT = empty | bTree of 'a BT * 'a * 'a BT;

(* 'a BT -> 'a list *)
fun Preorder empty = nil
  | Preorder (bTree (left, data, right)) = [data] @ Preorder left @ Preorder right;

fun Inorder empty = nil
  | Inorder (bTree (left, data, right)) = Inorder left @ [data] @ Inorder right;

fun Postorder empty = nil 
  | Postorder (bTree (left, data, right)) = Postorder left @ Postorder right @ [data];


(*
fun printreal ;

fun printint ;

fun printX ;

val identlevel ;

fun tab ;

fun displaynode ;

fun dash ;

fun displaytreeident ;

fun DisplayTree ;
*)


(*----------------------------------------------------------------------------*)



datatype X = A | B | C | D | E | F | H | G;

val t1 = bTree(1, bTree(2, bTree(3, empty, empty), bTree(4, empty, empty)),
	       bTree(5, bTree(6, empty, empty), bTree(7, empty, empty)));

val t2 = bTree(A, bTree(B, bTree(D, empty, empty), bTree(E, empty, empty)),
	       bTree(C, bTree(F, empty, empty), bTree(G, empty, empty)));

val t3 = bTree(1.22, bTree(2.33, empty, empty), bTree(3.44, empty, empty));

val t4 = bTree("A", bTree("B", bTree("C", bTree("E", empty, empty), empty),
			  bTree("D", bTree("F", empty, empty),
				bTree("G", bTree("H", empty, empty), empty))),
	       bTree("I", bTree("J", empty, bTree("K", bTree("L", empty, empty),
						  bTree("M", empty, empty))),
		     empty));
