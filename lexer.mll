{
open Parser
exception Eof
}
(* Déclaration du dictionnaire (regexp -> terminal/token) *)

rule anlex = parse
  | [' ' '\t' '\n' '\r']      { anlex lexbuf (* Oubli des espacements et passages à la ligne *) }
  | "SELECT"                  { SELECT        }
  | "ALL"                     { ALL           }
  | "DISTINCT"                { DISTINCT      }
  | "FROM"                    { FROM          }
  | "WHERE"                   { WHERE         }
  | "LOWER"                   { LOWER         }
  | "UPPER"                   { UPPER         }
  | "SUBSTRING"               { SUBSTRING     }
  | "FOR"                     { FOR           }
  | "NOT"                     { NOT           }
  | "TRUE"                    { TRUE          }
  | "FALSE"                   { FALSE         }
  | "NULL"                    { NULL          }
  | "IS"                      { IS            }
  | "OR"                      { OR            }
  | "AND"                     { AND           }
  | "BETWEEN"                 { BETWEEN       }
  | "FULL"                    { FULL          }
  | "OUTER"                   { OUTER         }
  | "JOIN"                    { JOIN          }
  | "RIGHT"                   { RIGHT         }
  | "LEFT"                    { LEFT          }
  | "INNER"                   { INNER         }
  | "UNKNOWN"                 { UNKNOWN       }
  | "GROUP BY"                { GROUPBY       }
  | "CROSS JOIN"              { CROSSJOIN     }
  | "AS"                      { AS            }
  | "ON"                      { ON            }
  | '*'                       { ASTERISK      }
  | ','                       { COMMA         }
  | '.'                       { DOT           }
  | '('                       { LPAR          }
  | ')'                       { RPAR          }
  | '+'                       { PLUS          }
  | '-'                       { MINUS         }
  | '='                       { EQ            }
  | '<'                       { LT            }
  | '>'                       { GT            }
  | '/'                       { SLASH         }
  | ';'                       { TERM          }
  | "<>"                      { NEQ           }
  | "<="                      { LE            }
  | ">="                      { GE            }
  | "||"                      { PPIPE         }
  | "'"                       { (quote "" lexbuf) }
  | "\""                      { (qquote "" lexbuf) }
  | "--"                      { comment lexbuf}
  | ['0'-'9']+ as lxm         { INT(int_of_string lxm) }
  | ['0'-'9']+'.'['0'-'9']? as lxm { FLOAT(float_of_string lxm) }
  | ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9']* as lxm  {ID(lxm) }
  | ['a'-'z''A'-'Z''0'-'9']* as lxm  { STRING(lxm) }
  | eof                       { raise Eof     }
  | _  as lxm                 { (* Pour tout autre caractère : message sur la sortie erreur + oubli *)
                               Printf.eprintf "Unknown character '%c': ignored\n" lxm; flush stderr;
                               anlex lexbuf   }
  and quote s = parse
  | "'"                       { STRING(s)     }
  | _ as c                    { quote(Printf.sprintf "%s%c" s c) lexbuf }

  and qquote s = parse
  | "\""                      { ID(s)       }
  | _ as lxm                  { qquote(Printf.sprintf "%s%c" s lxm) lexbuf }

  and comment = parse
  | _                          { comment lexbuf}
  |'\n'                        { anlex lexbuf  }
  | eof                        { raise Eof     }