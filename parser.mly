%{
open Ast
%}

/* Déclaration des terminaux */
%token <string> STRING
%token <string> ID
%token <int> INT
%token <float> FLOAT

%token PPIPE COMMA DOT LPAR RPAR
%token NOT IS OR AND BETWEEN ASTERISK ON AS
%token SELECT FROM WHERE ALL DISTINCT LOWER UPPER SUBSTRING
%token FOR TRUE FALSE NULL FULL OUTER JOIN RIGHT LEFT INNER
%token PLUS MINUS EQ LT GT UMINUS NEQ LE GE 
%token SLASH TERM GROUPBY  CROSSJOIN UNKNOWN



/* Précédences (priorité + associativité) des terminaux */
%left PLUS MINUS
%left ASTERISK DIV
%left IS
%left NOT
%left AND
%left OR
%left FROM
%left PPIPE
%left COMMA
%right UMINUS
%left NEQ LE GE EQ LT GT
%left FOR TRUE FALSE NULL FULL OUTER JOIN RIGHT LEFT INNER CROSSJOIN


/* Déclaration du non-terminal axiome (ici, ansyn) et du type de son attribut */
%type <Ast.query> ansyn
%start ansyn

%%

/* Déclaration de la grammaire avec les actions sémantiques */

ansyn:
  | TERM ansyn              { $2 }
  | simple_query TERM       { $1 }
;


simple_query:
  | SELECT projection FROM source                           { cstSelect_All $2 $4 cstEmpty    }
  | SELECT ALL projection FROM source                       { cstSelect_All $3 $5 cstEmpty    }
  | SELECT DISTINCT projection FROM source                  { cstSelect_Dist $3 $5 cstEmpty   }
  | SELECT projection FROM source WHERE condition           { cstSelect_All $2 $4 $6          }
  | SELECT ALL projection FROM source WHERE condition       { cstSelect_All $3 $5 $7          }
  | SELECT DISTINCT projection FROM source WHERE condition  { cstSelect_Dist $3 $5 $7         }
;
expression:
  | attribute                                               { cstAttribute $1                  }
  | LPAR expression RPAR                                    { $2                               } 
  | INT                                                     { cstInt $1                        }
  | FLOAT                                                   { cstFloat $1                      }
  | STRING                                                  { cstString $1                     }
  | expression PLUS expression                              { cstadd $1  $3                    }
  | expression MINUS expression                             { cstsub $1  $3                    }
  | expression ASTERISK expression                          { cstmul $1  $3                    }
  | expression DIV expression                               { cstdiv $1  $3                    }
  | MINUS expression %prec UMINUS                           { cstUMinus $2                     }
  | expression PPIPE expression                             { cstPipe $1 $3                    }
  | UPPER LPAR expression RPAR                              { cstUpper $3                      }
  | LOWER LPAR expression RPAR                              { cstLower $3                      }
  | SUBSTRING LPAR expression FROM expression FOR expression RPAR { cstSubString $3 $5 $7      }
;
condition:
  | predicate                                            { cstPredicate $1                     }
  | NOT condition                                        { cstNegation $2                      }
  | condition AND condition                              { cstConjonction $1 $3                }
  | condition OR condition                               { cstDisjonction $1 $3                }
  | condition IS TRUE                                    { cstIs $1 cstTrue                    }
  | condition IS FALSE                                   { cstIs $1 cstFalse                   }
  | condition IS UNKNOWN                                 { cstIs $1 cstUnknown                 }
  | condition IS NOT TRUE                                { cstIsNot $1 cstTrue                 }
  | condition IS NOT FALSE                               { cstIsNot $1 cstFalse                }
  | condition IS NOT UNKNOWN                             { cstIsNot $1 cstUnknown              }
;
source:
  | ID                                                    { cstId $1                           }
  | LPAR simple_query RPAR                                { cstQuery $2                        }
  | source COMMA source                                   { cstComma $1 $3                     }
  | source CROSSJOIN source                               { cstCrossjoin $1 $3                 }
  | source JOIN source ON condition                       { cstJoinop $1 cstInnerJoin $3 $5    }
  | source RIGHT JOIN source ON condition                 { cstJoinop $1 cstRightJoin $4 $6    }
  | source RIGHT OUTER JOIN source ON condition           { cstJoinop $1 cstRightOuterJoin $5 $7}
  | source INNER JOIN  source ON condition                { cstJoinop $1 cstInnerJoin $4 $6    }
  | source LEFT JOIN  source ON condition                 { cstJoinop $1 cstLeftJoin $4 $6     }
  | source LEFT OUTER JOIN source ON condition            { cstJoinop $1 cstLeftOuterJoin $5 $7}
  | source FULL JOIN source ON condition                  { cstJoinop $1 cstFullJoin $4 $6     }
  | source FULL OUTER JOIN source ON condition            { cstJoinop $1 cstFullOuterJoin $5 $7}
;
attribute:
  | ID DOT ID                                             { cstColumnT $1 $3                  }
;
colmn:
  | expression                                            { cstColumn $1                       }
  | colmn COMMA colmn                                     { concatListColumn $1 $3             }
  | expression AS ID                                      { cstColumnId $1 $3                  } 
;

projection:
  | ASTERISK                                              { cstAsterisk                        }
  | colmn                                                 { cstColumns $1 }
;

predicate:
  | LPAR condition RPAR                                  { cstCond $2                        }
  | expression EQ expression                             { cstEq  $1  $3                     }
  | expression NEQ expression                            { cstNeq $1  $3                     }
  | expression LT expression                             { cstLt  $1  $3                     }
  | expression LE expression                             { cstLe  $1  $3                     }
  | expression GT expression                             { cstGt  $1  $3                     }
  | expression GE expression                             { cstGe  $1  $3                     }
  | expression BETWEEN expression AND expression         { cstBetween $1 $3 $5               }
  | expression NOT BETWEEN expression AND expression     { cstNotBetween $1 $4 $6            }
  | expression IS NULL                                   { cstIsNull $1                      }
  | expression IS NOT NULL                               { cstIsNotNull $1                   }
;