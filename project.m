type terme = X of int
| A
| B
| F of terme
| G of terme*terme
| H of terme * terme * terme ;;

exception Undefined;;
  
type equation = terme*terme;;

type systeme = equation list;;


let test = [(A,A);(F(X(1)),F(X(2));(B,B);(G(X(1),X(2)),A);(X(1),X(2));(A,X(1))];;

let te1 = [(F(X(1)),F(X(2)));(A,A);(G(X(3),X(4)),F(X(1)))];;

let term = H(G(X(1),B),F(A),G(X(2),B));;

let trm = G((F(X(1))),B);;

let sys1 = [(A,A);(F(X(1)),F(B));(B,B)];;

let ter = F(X(1));;
  
let ter11 =  F(X(2));;

let res = [(A,B);(F(X(A)),B)];;



  
let rec cas1 system = match system with
  |[]-> []
  |(t1,t2)::rest -> if(t1 = t2) then (cas1 rest)
		    else
		      (t1,t2)::(cas1 rest);;
  
let rec cas2 sys = match sys with
  |[] -> []
  |(X(i),X(a))::rest -> (X(i),X(a))::(cas2 rest)
  |(t,X(a))::rest -> (X(a),t)::(cas2 rest)
  |(t1,t2)::rest -> (t1,t2)::(cas2 rest);;


let rec cas3 sys = match sys with
  |[] -> []
  |(F(t1),F(t2))::rest -> (t1,t2)::(cas3 rest)
  |(G(t1,t2),G(t3,t4))::rest -> (t1,t3)::(t2,t4)::(cas3 rest)
  |(H(t1,t2,t3),H(t4,t5,t6))::rest -> (t1,t4)::(t2,t5)::(t3,t6)::(cas3 rest)
  |(F(t1),G(t2,t3))::rest ->  raise (Undefined)
  |(G(t1,t2),H(t3,t4,t5))::rest ->  raise (Undefined)
  |(F(t1),H(t2,t3,t4))::rest ->  raise (Undefined)				 
  |(G(t2,t3),F(t1))::rest ->  raise (Undefined)
  |(H(t3,t4,t5),G(t1,t2))::rest ->  raise (Undefined)
  |(H(t2,t3,t4),F(t1))::rest ->  raise (Undefined)
  |(t1,t2)::rest -> (t1,t2)::(cas3 rest)
;;

let rec cas4 sys1 sys2 = match sys2 with
  |[]->sys1
  |((X(i),t1))::rest-> if  (occu_sys_var  (X i) rest) then if (test_occurrence i t1) then raise (Undefined) 
								    else let a = (X(i),t1)::sys1 and b = substitution_systeme (X i) t1 sys2 in cas4 a b
else (sys2@((X(i),t1)::[]))
  |h::sys2-> (sys1@(h::[]));;
 				


  
let rec test_occurrence  x   t  = match t with
  |X(i)-> if(x = i) then true
	  else false 
  |A-> false
  |B-> false
  |F(t1)->  (test_occurrence x t1) 
  |G(t1,t2)-> (test_occurrence x t1) || (test_occurrence x t2)
  |H(t1,t2,t3)-> (test_occurrence x t1) || (test_occurrence x t2) || (test_occurrence x t3) 
  ;;


let rec occu_sys  t  sys = match sys with
    |[] -> false 
    |(t1,t2)::rest-> if  t1=t  || t2=t 
		     then true
		     else ( occu_sys t rest ) ;;


    
     			  
let rec substitution (X i) t1 t2 = match t2  with
   |A->A
   |B->B	  
   |(X a)-> if  a=i then t1 else t2   
   |F(t)-> F((substitution (X i) t1 t)) 
   |G(a,b)-> G((substitution (X i) t1 a),(substitution (X i) t1 b))  
   |H(a,b,c)-> H((substitution (X i) t1 a),(substitution (X i) t1 b),(substitution (X i) t1 c));;



  let rec occu_sys_var  (X i)  sys = match sys with
    |[] -> false 
    |(X(a),X(b))::rest-> if  a=i  || b=i 
		     then true
			 else ( occu_sys_var (X i) rest);;

  let rec substitution_systeme (X i) t sys  = match sys  with
  |[]->[]  
  |(t1,t2)::rest-> ((substitution (X i) t t1),(substitution (X i) t t2))::(substitution_systeme (X i) t rest) ;;  
     

		    

  let rec forme_res sys = match sys with
    |[] -> true 
    |(t1,t2)::rest -> if (t1 = t2 || (occu_sys t1 rest ))
		      then false
		      else if ( t1 == t2 && (occu_sys t1 rest) = false)
		      then true
		      else (forme_res rest) ;; 
	      
   
  let rec algo_unification sys = match sys with
    |[]->[]
    |(t1,t2)::rest->if forme_res (cas1(cas2(cas3(cas4 [] rest))))
		    then if ((cas1(cas2(cas3(cas4 [] rest)))) = rest)
			 then raise (Undefined)
			 else algo_unification (cas1(cas2(cas3(cas4 [] rest))))
		    else rest;;
