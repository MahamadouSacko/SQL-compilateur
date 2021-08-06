open Env
open Value
open R

type attribute = ColumnT of string * string
and booleen = True | False | Unknown
and column = Column of expression | ColumnId of expression * string
and projection = Asterisk | Columns of column list
and source =
    Id of string
  | Query of query
  | Comma of source * source
  | Crossjoin of source * source
  | Joinop of source * joinop * source * condition
and query =
    Select of projection * source * condition
  | Select_Dist of projection * source * condition
and joinop =
    LeftOuterJoin
  | RightOuterJoin
  | FullOuterJoin
  | InnerJoin
  | LeftJoin
  | RightJoin
  | FullJoin
and expression =
    Attribute of attribute
  | Addition of expression * expression
  | Soustraction of expression * expression
  | Multiplication of expression * expression
  | Division of expression * expression
  | Upper of expression
  | Lower of expression
  | UMinus of expression
  | Pipe of expression * expression
  | SubString of expression * expression * expression
  | Value of Value.value
and condition =
    Empty
  | Predicate of predicate
  | Negation of condition
  | Conjonction of condition * condition
  | Disjonction of condition * condition
  | Is of condition * booleen
  | IsNot of condition * booleen
and predicate =
    Cond of condition
  | Eq of expression * expression
  | Neq of expression * expression
  | Lt of expression * expression
  | Gt of expression * expression
  | Le of expression * expression
  | Ge of expression * expression
  | Between of expression * expression * expression
  | NotBetween of expression * expression * expression
  | IsNull of expression
  | IsNotNull of expression

val string_of_Joinop : joinop -> string
val string_of_booleen : booleen -> string
val pp_query : query -> string
val pp_source : source -> string
val pp_projection : projection -> string
val pp_column : column list -> string
val pp_condition : condition -> string
val pp_predicate : predicate -> string
val pp_expression : expression -> string

val cstInt: int -> expression
val cstFloat : float -> expression
val cstString : string -> expression
val cstAttribute : attribute -> expression
val cstadd : expression -> expression -> expression
val cstsub : expression -> expression -> expression
val cstmul : expression -> expression -> expression
val cstdiv : expression -> expression -> expression
val cstUpper : expression -> expression
val cstUMinus : expression -> expression
val cstLower : expression -> expression
val cstPipe : expression -> expression -> expression
val cstSubString : expression -> expression -> expression -> expression


val cstEmpty : condition
val cstPredicate : predicate -> condition
val cstNegation : condition -> condition
val cstConjonction : condition -> condition -> condition
val cstDisjonction : condition -> condition -> condition
val cstIs : condition -> booleen -> condition
val cstIsNot : condition -> booleen -> condition

val cstTrue : booleen
val cstFalse : booleen
val cstUnknown : booleen 

val cstSelect_All : projection -> source -> condition -> query
val cstSelect_Dist : projection -> source -> condition -> query

val cstColumn : expression -> column list
val cstColumnId : expression -> string -> column list

val cstAsterisk : projection 
val cstColumns : column list -> projection

val cstCond : condition -> predicate
val cstEq : expression -> expression -> predicate
val cstNeq : expression -> expression -> predicate
val cstLt : expression -> expression -> predicate
val cstGt : expression -> expression -> predicate
val cstLe : expression -> expression -> predicate
val cstGe : expression -> expression -> predicate
val cstBetween : expression -> expression -> expression -> predicate
val cstNotBetween : expression -> expression -> expression -> predicate 
val cstIsNull : expression -> predicate
val cstIsNotNull : expression -> predicate

val cstId : string -> source
val cstQuery : query -> source
val cstComma : source -> source -> source
val cstCrossjoin : source -> source -> source
val cstJoinop : source -> joinop -> source -> condition -> source

val cstInnerJoin : joinop
val cstLeftJoin : joinop
val cstRightJoin : joinop
val cstFullJoin : joinop
val cstLeftOuterJoin : joinop
val cstRightOuterJoin : joinop
val cstFullOuterJoin : joinop

val cstColumnT : string -> string -> attribute
val concatListColumn : 'a list -> 'a list -> 'a list


val eval_query: (R.relation * (R.attribute env)) env -> query -> R.relation