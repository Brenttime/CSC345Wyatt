(*
  Brent Turner
  CSC 345 -- ML Homework
  mlfile.sml
*)
(*----------------------------------------------------------------------------*)

datatype X = A|B|C|D|E|F|G|H ;

datatype 'a BT = empty | bTree of 'a * 'a BT * 'a BT;

(*  Binary Tree Data  *)

val t1 = bTree(1,
           bTree(2,bTree(3,empty, empty), bTree(4,empty, empty)),
           bTree(5,bTree(6,empty, empty),bTree(7,empty, empty)));

val t2 = bTree(A,
           bTree(B,bTree(D,empty, empty), bTree(E,empty, empty)),
           bTree(C,bTree(F,empty, empty),bTree(G,empty, empty)));

val t3 = bTree(1.22, bTree(2.33,empty,empty), bTree(3.44,empty,empty));

val t4 = bTree("A",
           bTree("B",
                     bTree("C",bTree("E",empty,empty), empty),
                     bTree("D",
                           bTree("F",empty,empty),
                           bTree("G",bTree("H",empty,empty), empty))),
           bTree("I",
                     bTree("J",empty, bTree("K",
                        bTree("L",empty, empty),
                        bTree("M",empty, empty))),
                     empty));

(*----------------------------------------------------------------------------*)
(* Traversal Functions *)
fun Preorder empty = nil
  | Preorder (bTree(value, left, right)) = [value] @ Preorder(left) @ Preorder(right);

fun Inorder empty = nil
  | Inorder (bTree(value,left, right)) = (Inorder(left)) @ [value] @ (Inorder(right));

fun Postorder empty = nil 
  | Postorder (bTree(value, left, right)) = Postorder(left) @ Postorder(right) @ [value];

(*----------------------------------------------------------------------------*)
(* Print Functions *)
fun printInt n = print(Int.toString n);

fun printReal n = print(Real.toString n);

fun printX A = print "A"
  | printX B = print "B"
  | printX C = print "C"
  | printX D = print "D"
  | printX E = print "E"
  | printX F = print "F"
  | printX G = print "G"
  | printX H = print "H" ;

(*----------------------------------------------------------------------------*)
(* Dash & Tab Functions *)
val indentLevel = 2;

fun tab(tabLevel) = if tabLevel = 0 then print "" else (print " "; tab(tabLevel-1));

fun dash(tabLevel) = (tab(tabLevel * indentLevel); print"-"; print"\n");

(*----------------------------------------------------------------------------*)
(* Displaying Functions *)

(* Tabs then shows the node *)
fun displayNode(node, tabLevel, x: 'a -> 'b) = (tab (tabLevel * indentLevel); x node; print "\n");


fun displayTreeIndent(bTree(root,empty,empty), tabLevel, x) = (displayNode(root, tabLevel, x))
								  
  (* Root - tree - empty *)							       
  | displayTreeIndent(bTree(root, left, empty), tabLevel, x) = (displayNode(root, tabLevel, x);
							     displayTreeIndent(left, tabLevel+1, x);
							     dash(tabLevel+1))
								   
  (* Root - empty - tree *)								
  | displayTreeIndent(bTree(root, empty, right), tabLevel, x) = (displayNode(root, tabLevel, x);
							      dash(tabLevel+1);
							      displayTreeIndent(right, tabLevel+1, x))
								    
  (* Root - tree - tree *)								 
  | displayTreeIndent(bTree(root, left, right), tabLevel, x) = (displayNode(root, tabLevel, x);
							     displayTreeIndent(left, tabLevel+1, x);
							     displayTreeIndent(right, tabLevel+1, x))

  (* Exhuastive Case *)
  | displayTreeIndent(_, tabLevel, x) = print("");


fun displayTree(bTree(root,left,right), x: 'a -> 'b) = 
    let 
	val tabLevel = 0
    in
	(displayNode(root, tabLevel, x); displayTreeIndent(left, tabLevel + 1, x); displayTreeIndent(right, tabLevel + 1, x))
    end
	
  (* Exhuastive Case *)	
  | displayTree(_,x: 'a->'b) = print(""); 

(*----------------------------------------------------------------------------*)
(* Technical Matters *)

Control.Print.printDepth := 200;

Control.Print.printLength := 200;

Control.polyEqWarn := false;
