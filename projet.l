%{
  #include <limits.h>
  #include "type_synth.h"
  #include "projet.tab.h"
  #include "stable.c"
  #include "stable.h"
  #include "types.h"
  void string_to_int(int *v, const char *s);
%}
%option noyywrap
%%
[[:digit:]]+	{ string_to_int(&yylval.entier, yytext); return NUMBER; }
[[:blank:]]
print 			  { return PRINT; }
"+"           { return PLUS; }
"-"           { return MOINS; }
"*"           { return FOIS;}
"/"           { return DIVISE; }
"&&"			    { return ET; }
"||"			    { return OU; }
"!"				    { return NOT; }
"==" 			    { return EQ; }
"<" 			    { return INF; }
"<=" 			    { return INFEQ; }
">" 			    { return SUP; }
">=" 			    { return SUPEQ; }
true 			    { return VRAI; }
false 		  	{ return FAUX; }
"int" 			  { return L_INT; }
"bool"			  { return L_BOOL; }
"if"          { return IF ; }
"else"        { return ELSE; }
"while"       { return WHILE; }
id				    { char *r = malloc(strlen(yytext) + 1); strcpy(r, yytext); yylval.chaine = r; return ID;}
.|\n         	{ return yytext[0]; } // return le caractère que je viens de lire
%%

void string_to_int(int *r, const char *s) {
  char *p;
  long v;
  errno = 0;
  v = strtol(s, &p, 10);
  if ( ( *p != '\0' || ( errno == ERANGE 
                     && ( v == LONG_MIN || v == LONG_MAX ) ) ) 
       || ( v < INT_MIN || v > INT_MAX ) ) {
    fprintf(stderr, "Error converting string to int\n");
    exit(EXIT_FAILURE);
  } 
  *r = v;
}
