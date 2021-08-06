open Env
open Value
module R =
struct
  (* Création des opérations de manipulation des relations à valeur dans Value *)
  include Relation.Make(Value)
end
 (* Syntaxe abstraite *)
type attribute =
  | ColumnT of string*string
and
 booleen =
  | True
  | False
  | Unknown
and
 column =
  | Column of expression
  | ColumnId of expression * string
and
 projection =
  | Asterisk
  | Columns of column list
and
 source =
  | Id          of string
  | Query       of query
  | Comma       of source * source
  | Crossjoin   of source * source
  | Joinop      of source * joinop * source * condition
and 
 query =
  | Select of projection * source * condition
  | Select_Dist of projection * source * condition
and
 joinop =
  | LeftOuterJoin
  | RightOuterJoin
  | FullOuterJoin
  | InnerJoin
  | LeftJoin
  | RightJoin
  | FullJoin
and
 expression =
  | Attribute   of attribute
  | Addition of expression * expression
  | Soustraction of expression * expression
  | Multiplication of expression * expression
  | Division of expression * expression
  | Upper of expression
  | Lower of expression
  | UMinus of expression
  | Pipe of expression * expression
  | SubString of expression * expression * expression
  | Value of value
and
 condition =
  | Empty
  | Predicate   of predicate
  | Negation    of condition
  | Conjonction of condition * condition
  | Disjonction of condition * condition
  | Is          of condition * booleen
  | IsNot       of condition * booleen
and 
 predicate =
  | Cond of condition
  | Eq of expression * expression
  | Neq of expression * expression
  | Lt of expression * expression
  | Gt of expression * expression
  | Le of expression * expression
  | Ge of expression * expression
  | Between     of expression * expression * expression
  | NotBetween  of expression * expression * expression
  | IsNull      of expression
  | IsNotNull   of expression
;;

(* Conversion en chaîne de caractères pour affichage *)
let string_of_Joinop opr =
  match opr with
  | RightOuterJoin  ->  "RIGHT OUTER JOIN"
  | RightJoin       ->  "RIGHT JOIN"
  | FullJoin        ->  "FULL JOIN"
  | FullOuterJoin   ->  "FULL OUTER JOIN"
  | InnerJoin       ->  "INNER JOIN"
  | LeftJoin        ->  "LEFT JOIN"
  | LeftOuterJoin   ->  "LEFT OUTER JOIN"
;;


let string_of_booleen booleen=
  match booleen with
  | True           -> "TRUE"
  | False          -> "FALSE"
  | Unknown        -> "UNKNOWN"
;;
let rec pp_query query =
  match query with
  | Select(p,s,c) ->(Printf.sprintf "SELECT %s FROM %s WHERE %s" (pp_projection p) (pp_source s) (pp_condition c) )
  | Select_Dist(p,s,c)->( Printf.sprintf "SELECT DISTINCT %s FROM %s WHERE %s" (pp_projection p) (pp_source s) (pp_condition c))

and pp_source source =
  match source with
  | Id(id) -> Printf.sprintf "%s" id
  | Query(q) -> Printf.sprintf "(%s)" (pp_query q)
  | Crossjoin(source1,source2) -> Printf.sprintf "%s CROSS JOIN %s" (pp_source source1) (pp_source source2)
  | Joinop(source1,op,source2,c)->(Printf.sprintf " %s %s %s ON %s " (pp_source source1) (string_of_Joinop op) (pp_source source2) (pp_condition c) )
  | Comma(source1,source2) -> Printf.sprintf "%s,%s" (pp_source source1) (pp_source source2)

and pp_projection projection =
  match projection with
  | Asterisk->"*"
  | Columns(colum_list) ->pp_column colum_list


and pp_column colum_list =
  match colum_list with
  | []   -> ""
  | x::q -> ( match q with
              | [] -> (match x with Column(e) | ColumnId(e,_) -> pp_expression e)
              | _ -> Printf.sprintf "%s, %s" (match x with
                        | Column(e)   -> Printf.sprintf "%s" (pp_expression e)
                        | ColumnId(e,s) -> Printf.sprintf "%s AS %s " (pp_expression e) s) (pp_column q)
            )

and pp_condition cond =
  match cond with
  | Empty -> "TRUE"
  | Predicate(p)-> pp_predicate p
  | Negation(cond)-> pp_condition cond
  | Is (cond,booleen)  ->    Printf.sprintf "%s IS %s" (pp_condition cond) (string_of_booleen booleen)
  | IsNot(cond,booleen)  ->    Printf.sprintf "%s IS NOT %s" (pp_condition cond) (string_of_booleen booleen)
  | Conjonction(cond1,cond2) -> Printf.sprintf "(%s) AND (%s)" (pp_condition cond1) (pp_condition cond2)
  | Disjonction (cond1,cond2) -> Printf.sprintf "(%s) OR (%s)" (pp_condition cond1) (pp_condition cond2)

and pp_predicate pred =
  match pred with
  | Cond(cond)-> pp_condition cond
  | Eq (exp1,exp2)  -> Printf.sprintf "(%s) = (%s)" (pp_expression exp1) (pp_expression exp2)
  | Neq (exp1,exp2)  -> Printf.sprintf "(%s) <> (%s)" (pp_expression exp1) (pp_expression exp2)
  | Lt (exp1,exp2)  -> Printf.sprintf "(%s) < (%s)" (pp_expression exp1) (pp_expression exp2)
  | Gt (exp1,exp2)  -> Printf.sprintf "(%s) > (%s)" (pp_expression exp1) (pp_expression exp2)
  | Le (exp1,exp2)  -> Printf.sprintf "(%s) =< (%s)" (pp_expression exp1) (pp_expression exp2)
  | Ge (exp1,exp2)  -> Printf.sprintf "(%s) >= (%s)" (pp_expression exp1) (pp_expression exp2)
  | Between (exp1,exp2,exp3) ->Printf.sprintf "(%s) BETWEEN (%s) AND (%s)" (pp_expression exp1) (pp_expression exp2) (pp_expression exp3)
  | NotBetween (exp1,exp2,exp3) ->Printf.sprintf "(%s) NOT BETWEEN (%s) AND (%s)" (pp_expression exp1) (pp_expression exp2) (pp_expression exp3)
  | IsNull (expr) -> Printf.sprintf "(%s) IS NULL" (pp_expression expr)
  | IsNotNull (expr) -> Printf.sprintf "(%s) IS NOT NULL" (pp_expression expr)

and pp_expression expr=
  match expr with
  | Attribute(ColumnT(str1, str2)) -> Printf.sprintf "%s.%s" str1 str2
  | Addition (exp1,exp2) -> Printf.sprintf "(%s) + (%s)" (pp_expression exp1) (pp_expression exp2)
  | Soustraction (exp1,exp2) -> Printf.sprintf "(%s) - (%s)" (pp_expression exp1) (pp_expression exp2)
  | Multiplication (exp1,exp2) -> Printf.sprintf "(%s) * (%s)" (pp_expression exp1) (pp_expression exp2)
  | Division (exp1,exp2) -> Printf.sprintf "(%s) / (%s)" (pp_expression exp1) (pp_expression exp2)
  | Upper (exp) -> Printf.sprintf " UPPER (%s)" (pp_expression exp)
  | Lower (exp) -> Printf.sprintf " LOWER (%s)" (pp_expression exp)
  | UMinus (exp) -> Printf.sprintf " -(%s)" (pp_expression exp)
  | Pipe  (exp1,exp2) -> Printf.sprintf "(%s) || (%s)" (pp_expression exp1) (pp_expression exp2)
  | SubString  (exp1,exp2,exp3) -> Printf.sprintf "SUBSTRING((%s) FROM (%s) FOR (%s))" (pp_expression exp1)  (pp_expression exp2) (pp_expression exp3)
  | Value(const) -> Printf.sprintf "%s" (string_of_value const )
;;


(* Constructeurs d'expression *)
let cstInt cst = Value(VInt cst)
;;
let cstFloat cst = Value(VFloat cst )
;;
let cstString cst = Value(VVChar cst)
;;
let cstAttribute attr = Attribute(attr)
;;
let cstadd exp1 exp2=Addition(exp1,exp2)
;;
let cstsub exp1 exp2=Soustraction(exp1,exp2)
;;
let cstmul exp1 exp2=Multiplication(exp1,exp2)
;;
let cstdiv exp1 exp2=Division(exp1,exp2)

let cstUpper expr = Upper(expr)
;;
let cstUMinus expr = UMinus(expr)
;;
let cstLower expr = Lower(expr)
;;
let cstPipe exp1 exp2 = Pipe(exp1,exp2)
;;
let cstSubString exp1 exp2 exp3 = SubString(exp1,exp2,exp3)
;;

(* Constructeurs de condition *)

let cstEmpty = Empty 
;;
let cstPredicate pred = Predicate(pred)
;;
let cstNegation cond = Negation(cond)
;;
let cstConjonction c1 c2 = Conjonction(c1,c2)
;;
let cstDisjonction c1 c2 = Disjonction(c1,c2)
;;
let cstIs cond bo = Is(cond,bo)
;;
let cstIsNot cond bo = IsNot(cond,bo)
;;


(* Constructeurs de booleen *)
let cstTrue =True
;;
let cstFalse =False
;;
let cstUnknown =Unknown
;;

(* Constructeurs de query *)
let cstSelect_All  proj src cond = Select(proj,src,cond)
;;
let cstSelect_Dist proj src cond =
  match proj with
  | Asterisk  -> failwith "Le selecteur * n'est pas autorisé avec DISTINCT "
  | _ -> Select_Dist(proj,src,cond)
;;

(* Constructeurs de  column  *)
let cstColumn expr = [Column(expr)]
;;
let cstColumnId expr str = [ColumnId(expr,str)]
;;

(* Constructeurs projection  *)
let cstAsterisk = Asterisk
;;
let cstColumns list = Columns(list)
;;


(* Constructeurs des predicate *)
let cstCond cond = Cond(cond)
;;
let cstEq exp1 exp2 =Eq(exp1,exp2)
;;
let cstNeq exp1 exp2 =Neq(exp1,exp2)
;;
let cstLt exp1 exp2 =Lt(exp1,exp2)
;;
let cstGt exp1 exp2 =Gt(exp1,exp2)
;;
let cstLe exp1 exp2 =Le(exp1,exp2)
;;
let cstGe exp1 exp2 =Ge(exp1,exp2)
;;
let cstBetween exp1 exp2 exp3 = Between(exp1,exp2,exp3)
;;
let cstNotBetween exp1 exp2 exp3 = NotBetween(exp1,exp2,exp3)
;;
let cstIsNull expr = IsNull(expr)
;;
let cstIsNotNull expr = IsNotNull(expr)
;;

(* Constructeurs source *)
let cstId id = Id(id)
;;
let cstQuery query = Query(query)
;;
let cstComma src1 src2 = Comma(src1,src2)
;;
let cstCrossjoin src1 src2 = Crossjoin(src1,src2)
;;
let cstJoinop src1 op src2 cond = Joinop(src1,op,src2,cond)
;;

(* Constructeurs joinOp *)
let cstInnerJoin = InnerJoin
;;
let cstLeftJoin = LeftJoin
;;
let cstRightJoin = RightJoin
;;
let cstFullJoin = FullJoin
;;
let cstLeftOuterJoin = LeftOuterJoin
;;
let cstRightOuterJoin = RightOuterJoin
;;
let cstFullOuterJoin = FullOuterJoin
;;


(* Constructeurs d'attribute *)
let cstColumnT str str2 = ColumnT(str,str2)
;;
(* let cstPipe =Pipe;; *)
let concatListColumn l ll = l @ ll
;;
(* Evaluateur *)

let eval_booleen cond =
  match cond with
  | True -> true
  | False ->false
  | Unknown -> false
;;
let attributeFromTableColumn expr env =
  match expr with
  |  Attribute(ColumnT(source1,source2))-> (
     match (find source1 env) with
          | Some(_,a)->(
            match (find source2 a) with
            | Some(attribute1) -> attribute1
            |  _ -> failwith ( Printf.sprintf "L'attribu %s n'existe pas dans la table %s " source2 source1)
            )
          |None -> failwith ( Printf.sprintf "La table %s n'existe pas la base de données " source1)
  )
  | _ -> failwith "erreur"
;;
let conditionToAttribute cond env =
  match cond with
  | Predicate(Eq(e1,e2))->((attributeFromTableColumn e1 env),(attributeFromTableColumn e2 env))
  |  _ -> failwith "erreur "
;;
let rec eval_expr expr env x  = match expr with
  | Attribute(ColumnT(source1,source2)) ->(
      match (find source1 env) with
      | Some((a,b)) -> (
          match find source2 b with
          | Some(c) -> (match R.attribute c x with |Some(v)->v 
                                                   |None->NULL)
          | None -> failwith (Printf.sprintf "\"%s\" n'est pas une colonne dans la table" source2)
        )
      | None -> failwith (Printf.sprintf "La table \"%s\" n'existe pas" source1)
    )
  | Value(const) -> const
  | Addition (e1,e2)-> add (eval_expr e1 env x) (eval_expr e2 env x)
  | Soustraction (e1,e2)-> minus (eval_expr e1 env x) ( eval_expr e2 env x)
  | Multiplication (e1,e2)->mul (eval_expr e1 env x) (eval_expr e2 env x)
  | Division (e1,e2)-> div (eval_expr e1 env x) (eval_expr e2 env x)
  | Upper(expr)   -> (
      match (eval_expr expr env x) with
      | VVChar(s) -> VVChar(String.uppercase_ascii s)
      |_ -> failwith (" Errer: impossible de faire UPPER")
    )
  | Lower(expr)   ->(
      match (eval_expr expr env x) with
      | VVChar(s) -> VVChar(String.lowercase_ascii s)
      |_ -> failwith ( "Errer: impossible de faire LOWER ")
    )
  | UMinus(expr)  -> (
      match (eval_expr expr env x) with
      | VInt(i) -> VInt(-i)
      | VFloat(f)   -> VFloat(-.f)
      |_ -> failwith ("Errer: impossible de faire l'operateur UMinus")
    )
  | Pipe(e1,e2) ->concat (eval_expr e1 env x) (eval_expr e2 env x)

  | SubString(e1,e2,e3) -> (
      match (eval_expr e1 env x),(eval_expr e2 env x),(eval_expr e3 env x) with
      | VVChar(source1),VInt(i1),VInt(i2) -> VVChar (String.sub source1 i1 i2)
      | _,_,_ -> failwith (Printf.sprintf "Operateur SubString n'est pas applicable entre %s,%s et %s "
                      (pp_expression e1) (pp_expression e2) (pp_expression e3) )
  )
and eval_cond cond tuple env =
  match cond with
  | Empty-> true
  | Predicate(p) -> eval_predicate p tuple env
  | Negation(c)   ->not (eval_cond c tuple env)
  | Conjonction(c1,c2) ->(eval_cond c1 tuple env) && (eval_cond c2 tuple env)
  | Disjonction(c1,c2) ->(eval_cond c1 tuple env) || (eval_cond c2 tuple env)
  | Is(c1,b) ->(eval_cond c1 tuple env) == (eval_booleen b)
  | IsNot(c1,b) ->(eval_cond c1 tuple env) <> (eval_booleen b)
and eval_source src env =
  match src with
  | Id(id) -> (
      match (find id env ) with
        | Some((a,b))  -> a
        | _ -> failwith ("cette table n'existe pas !")
    )
  |Joinop(source1,jop,source2,cond) -> (
    let attribute1,attribute2 = (conditionToAttribute cond env) in
    match jop with
      | InnerJoin -> R.innerjoin (fun tuple1 tuple2 -> (R.attribute attribute1 tuple1) =  (R.attribute attribute2 tuple2)) (eval_source source1 env) (eval_source source2 env)
      | LeftJoin  -> R.inter (eval_source source1 env) (R.innerjoin (fun tuple1 tuple2 -> (R.attribute attribute1 tuple1) =  (R.attribute attribute2 tuple2)) (eval_source source1 env) (eval_source source2 env) )
      | RightJoin -> R.innerjoin (fun tuple1 tuple2 -> (R.attribute attribute1 tuple1) =  (R.attribute attribute2 tuple2)) (eval_source source1 env) (eval_source source2 env)
      | FullJoin  -> R.innerjoin (fun tuple1 tuple2 -> (R.attribute attribute1 tuple1) =  (R.attribute attribute2 tuple2)) (eval_source source1 env) (eval_source source2 env)
      | LeftOuterJoin -> R.leftouterjoin (fun tuple1 tuple2 -> (R.attribute attribute1 tuple1) =  (R.attribute attribute2 tuple2)) (eval_source source1 env) (eval_source source2 env)
      | RightOuterJoin-> R.innerjoin (fun tuple1 tuple2 -> (R.attribute attribute1 tuple1) =  (R.attribute attribute2 tuple2)) (eval_source source1 env) (eval_source source2 env)
      | FullOuterJoin -> R.fullouterjoin (fun tuple1 tuple2 -> (R.attribute attribute1 tuple1) =  (R.attribute attribute2 tuple2)) (eval_source source1 env) (eval_source source2 env)
    )
  | Comma(source1,source2) -> R.crossjoin (eval_source source1 env) (eval_source source2 env)
  | Crossjoin(source1,source2)-> R.crossjoin (eval_source source1 env) (eval_source source2 env)
  | _ -> failwith "la source n'existe pas"

and eval_project proje env relation = match proje with
  | Asterisk -> relation
  | Columns(l) -> (
      match l with
        | []   -> failwith "la liste de colonnes est vide "
        | _ -> R.projection [] relation
     )

and eval_predicate pred tuple env =
  match pred  with
  | Cond(cond) -> (eval_cond cond tuple env)
  | Eq (e1,e2) ->eq (eval_expr e1 env tuple) (eval_expr e2 env tuple)
  | Neq (e1,e2)->neq (eval_expr e1 env tuple) (eval_expr e2 env tuple)
  | Lt (e1,e2) ->lt (eval_expr e1 env tuple) (eval_expr e2 env tuple)
  | Gt (e1,e2) ->gt (eval_expr e1 env tuple) (eval_expr e2 env tuple)
  | Le (e1,e2) ->le (eval_expr e1 env tuple) (eval_expr e2 env tuple)
  | Ge (e1,e2) ->ge (eval_expr e1 env tuple) (eval_expr e2 env tuple)
  | Between(e1,e2,e3)   -> ((le (eval_expr e2 env tuple) (eval_expr e1 env tuple)) && (le (eval_expr e1 env tuple) (eval_expr e3 env tuple)))
  | NotBetween(e1,e2,e3)-> (not (le (eval_expr e2 env tuple) (eval_expr e1 env tuple)) || not (le (eval_expr e1 env tuple) (eval_expr e3 env tuple)))
  | IsNull(e) -> (
    match eval_expr e env tuple with
    | VVChar a -> ((String.length a) = 0)
    | NULL-> true
    | _ -> false
    )

  | IsNotNull(e)  -> (
    match eval_expr e env tuple with
    | VVChar a -> not ((String.length a) = 0)
    | NULL-> false
    | _ -> true                    )
and  eval_query env query =
  match query with
  | Select(proj,src,cond)  ->( R.selection (fun x->eval_cond cond x env) (eval_project proj env (eval_source src env) ) )
  | Select_Dist(proj,src,cond) ->( R.distinct (eval_project proj env (eval_source src env) ) )
;;