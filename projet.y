%{
  #include <ctype.h>
  #include <stdlib.h>
  #include <stdio.h>
  #include <stdarg.h>
  #include <limits.h>
  #include "stable.c"
  #include "types.h"
  #include "stable.h"
  #include "type_synth.h"
  int yylex(void);
  void yyerror(char const *);
  #define STACK_CAPACITY 50
  #define UNIT_MAX 1000
  #define MAXBUF 50
  #define LOOP_END "loop:end:"
  static int stack[STACK_CAPACITY];
  static size_t stack_size = 0;
  static unsigned int new_label_number();
  static void create_label(char *buf, size_t buf_size, const char *format, ...);
  void fail_with(const char *format, ...);
%}
%union {
	int entier;
	type_synth type;
	char *chaine;
	//symbol_type ts;
}
%token<entier> NUMBER VRAI FAUX ET OU NOT EQ SUP SUPEQ INF INFEQ ID L_INT L_BOOL PRINT RETURN PLUS MOINS FOIS DIVISE IF ELSE WHILE
%type<type> expr
%type<entier> fixif
%type<symbol_type> type
%left OU
%left ET
%left EQ
%left INF INFEQ SUP SUPEQ VRAI FAUX
%left '+''-'
%left '*''/'
//%precedence NOT
%precedence NON_ELSE
%precedence ELSE
%start lignes
%%

lignes : sdecl | sinstr;

sdecl : sdecl decl | %empty;

sinstr : sinstr instr | instr;

instr :
  ID '=' expr';' {
					symbol_table_entry *ste = search_symbol_table($1);
					if (ste == NULL) {
						fail_with("Erreur pas d'entree %s dans la table\n", $1);
					}
					if (ste->class != LOCAL_VARIABLE && ste->class != GLOBAL_VARIABLE) {
						fail_with("Erreur %s n'est pas une variable\n", $1);
					}
					if ((ste->desc[0] == INT_T && $3 != T_INT) || (ste->desc[0] == BOOL_T && $3 != T_BOOL)) {
						fail_with("Erreur le type de la variable et le type de l'expression ne correspondent pas\n");
					}
					printf("\tpop ax\n"
						   "\tconst bx, var:%s\n"
						   "storew ax,bx\n"
						  ,$1);
				 }
| PRINT expr';' {
					if ($2 == T_INT) {
						printf("\tconst ax,sp\n"
							   ""
							  );
					}
					if ($2 == T_BOOL) {
						if (true) {
							printfs("true");
						}
						if (false) {
							printfs("false");
						}
					}
				}
| IF '(' expr ')'  fixif instr {printf("\tjmp fin:%d\n"
			"\t:test:neg:%d\n", $5, $5);} ELSE instr { if ($3 != T_BOOL) { fail_with("erreur expr non bool"); } else {printf("\t:fin:%d\n", $5);}} ';'
| IF '('expr')' instr %prec NON_ELSE {}
//| whiletest instr   { loopend($1); }
;

/*
whiletest:
  while '(' expr ')'  { $$ = looptest($1, $3); }
;

while:
  WHILE               { $$ = preloop(); }
;
*/

fixif:
  %empty { int n = new_label_number();
					printf("\tconst cx,test:neg:%d\n"
						"\tpop ax\n"
						"\tconst bx,0\n"
						"\tcmp ax,bx\n"
						"\tjmpe cx\n", n); $<entier>$ = n; }

expr :
  NUMBER {
			printf("\tconst ax,%d\n", $1);
			printf("\tpush ax\n");
			$$ = T_INT;
		 }
| '(' expr ')' {$$ = $2;}
| VRAI { 
			printf("\tconst ax,1\n");
			printf("\tpush ax\n");
			$$ = T_BOOL;
	   }
| FAUX { 
			printf("\tconst ax,0\n");
			printf("\tpush ax\n");
			$$ = T_BOOL;
	   }
| expr ET expr { if ($1 != T_INT || $3 != T_INT) {
					$$ = ERR_TYPE;
				 } else {
					int y = stack[--stack_size];
					int x = stack[--stack_size];
					stack[stack_size ++] = x * y;
					$$ = T_BOOL;
				 }
			   }
| expr OU expr { if ($1 != T_INT || $3 != T_INT) {
					$$ = ERR_TYPE;
				 } else {
					int y = stack[--stack_size];
					int x = stack[--stack_size];
					stack[stack_size ++] = x + y;
					$$ = T_BOOL;
				 }
			   }
| NOT expr { if ($2 != T_BOOL) {
					$$ = ERR_TYPE;
			 } else {
				int x = stack[--stack_size];
				if (x == 0) {
					stack[stack_size ++] = 1;
				} else {
					stack[stack_size++] = 0;
				}					
				$$ = T_BOOL;
			 }
			}
| expr EQ expr { if ($1 != T_INT || $3 != T_INT) {
					$$ = ERR_TYPE;
				 } else {
					int y = stack[--stack_size];
					int x = stack[--stack_size];
					if (x == y) {
						stack[stack_size++] = 1;
					} else {
						stack[stack_size++] = 0;
					}
					$$ = T_BOOL;
				 }
			    }
| expr SUP expr { if ($1 != T_INT || $3 != T_INT) {
					$$ = ERR_TYPE;
				  } else {
					int y = stack[--stack_size];
					int x = stack[--stack_size];
					if (x > y) {
						stack[stack_size++] = 1;
					} else {
						stack[stack_size++] = 0;
					}
					$$ = T_BOOL;
				  }
			    }
| expr SUPEQ expr { if ($1 != T_INT || $3 != T_INT) {
						$$ = ERR_TYPE;
				    } else {
					int y = stack[--stack_size];
					int x = stack[--stack_size];
					if (x >= y) {
						stack[stack_size++] = 1;
					} else {
						stack[stack_size++] = 0;
					}
					$$ = T_BOOL;
				   }
			     }
| expr INF expr { if ($1 != T_INT || $3 != T_INT) {
					$$ = ERR_TYPE;
				  } else {
					int y = stack[--stack_size];
					int x = stack[--stack_size];
					if (x < y) {
						stack[stack_size++] = 1;
					} else {
						stack[stack_size++] = 0;
					}
					$$ = T_BOOL;
				  }
			    }
| expr INFEQ expr { if ($1 != T_INT || $3 != T_INT) {
					$$ = ERR_TYPE;
				    } else {
					int y = stack[--stack_size];
					int x = stack[--stack_size];
					if (x <= y) {
						stack[stack_size++] = 1;
					} else {
						stack[stack_size++] = 0;
					}
					$$ = T_BOOL;
				    }
			      }
| expr '+' expr { if ($1 == T_INT && $3 == T_INT) { 
					printf("\tpop bx\n");  
					printf("\tpop ax\n");  
					printf("\tadd ax,bx\n");  
					printf("\tpush ax\n");  
				  } else { 
						$$ = ERR_TYPE; 
					}
				}
| expr '-' expr { if ($1 == T_INT && $3 == T_INT) { 
					printf("\tpop ax\n");  
					printf("\tpop bx\n");  
					printf("sub ax,bx\n");  
					printf("push ax\n");  
				  } else { 
						$$ = ERR_TYPE; 
				    }
				}
| expr '*' expr { if ($1 == T_INT && $3 == T_INT) {
					printf("\tpop ax\n");  
					printf("\tpop bx\n");  
					printf("\tmul ax,bx\n");  
					printf("\tpush ax\n"); 
				  } else { 
						$$ = ERR_TYPE; 
				   }
				}
| expr '/' expr { if ($1 == T_INT && $3 == T_INT) { 
					printf("\tpop ax\n");  
					printf("\tpop bx\n");
					printf("\tdiv ax,bx\n");
					printf("\tpush ax\n");
				  } else { 
						$$ = ERR_ARITH;
					}
				}
| ID {}
;


type :
  L_INT  { $$ = INT_T; }
| L_BOOL { $$ = BOOL_T; }
;

decl : 
  L_BOOL '=' expr ';' { if (search_symbol_table($1) != NULL) { 
                          failwith("Erreur dans table des symbols\n");
                       } else {
                          symbol_table_entry *ste = new_symbol_table_entry($1);
                          ste->desc[0] = T_BOOL;
                         }
                       }
  | L_INT '=' expr ';' { if (search_symbol_table($1) != NULL) {
                          failwith("Erreur dans table des symbols\n");
                        } else {
                          symbol_table_entry *ste = new_symbol_table_entry($1);
                          ste->desc[0] = T_INT;
                         }
                       }
;

%%


void yyerror(char const *s) {
  fprintf(stderr, "%s\n", s);
}

int algo2asm (void){
printf("\begin{algo}{puissance}{a,b}\n\t\t\SET{p}{1}\n\t\DOFORI{k}{1}{b}\n\t\t\t\SET{p}{p*a}\n\t\OD\n\t\RETURN{p}\n\end{algo}\n");
yyparse();
return EXIT_SUCCESS;
}
int main (void){
algo2asm();
return EXIT_SUCCESS;
}


static unsigned int new_label_number() {
	static unsigned int current_label_number = 0u;
	if ( current_label_number == UINT_MAX ) {
		fail_with("Error: maximum label number reached!\n");
	}
	return current_label_number++;
}

static void create_label(char *buf, size_t buf_size, const char *format, ...) {
	va_list ap;
	va_start(ap, format);
	if ( vsnprintf(buf, buf_size, format, ap) >= buf_size ) {
	va_end(ap);
	fail_with("Error in label generation: size of label exceeds maximum size!\n");
	}
	va_end(ap);
}

/*
static int preloop(void) {
    int labelnb = new_label_number();
    printf(":" LOOP_BEGIN "%d\n", labelnb);
    return labelnb;
}

static int looptest(int labelnb, symbol_type expr_type) {
    if (expr_type != BOOL_T) {
        yyerror("while: type non booleen");
        clean_exit(EXIT_FAILURE);
    }
    printf("\tconst cx," LOOP_END "%d\n", labelnb);
    pop("ax");
    printf("\tconst bx,0\n");
    printf("\tcmp ax,bx\n");
    printf("\tjmpz cx\n");
    return labelnb;
}
*/

/*
Le code de la fonction fail_with, qui sert à sortir du programme sur 
erreur dont le message est passé en paramètre de la fonction à la 
manière de printf, est le suivant :
*/
void fail_with(const char *format, ...) {
	va_list ap;
	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);
	exit(EXIT_FAILURE);
}
