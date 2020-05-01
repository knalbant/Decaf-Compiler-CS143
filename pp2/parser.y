/* File: parser.y
 * --------------
 * Bison input file to generate the parser for the compiler.
 *
 * pp2: your job is to write a parser that will construct the parse tree
 *      and if no parse errors were found, print it.  The parser should 
 *      accept the language as described in specification, and as augmented 
 *      in the pp2 handout.
 */

%{

/* Just like lex, the text within this first region delimited by %{ and %}
 * is assumed to be C/C++ code and will be copied verbatim to the y.tab.c
 * file ahead of the definitions of the yyparse() function. Add other header
 * file inclusions or C++ variable declarations/prototypes that are needed
 * by your code here.
 */
#include "scanner.h" // for yylex
#include "parser.h"
#include "errors.h"

void yyerror(const char *msg); // standard error-handling routine

%}

/* The section before the first %% is the Definitions section of the yacc
 * input file. Here is where you declare tokens and types, add precedence
 * and associativity options, and so on.
 */
 
/* yylval 
 * ------
 * Here we define the type of the yylval global variable that is used by
 * the scanner to store attibute information about the token just scanned
 * and thus communicate that information to the parser. 
 *
 * pp2: You will need to add new fields to this union as you add different 
 *      attributes to your non-terminal symbols.
 */
%union {
    int integerConstant;
    bool boolConstant;
    char *stringConstant;
    double doubleConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    List<Decl*> *declList;
    Expr* expr;
    LValue *lvalue;
}


/* Tokens
 * ------
 * Here we tell yacc about all the token types that we are using.
 * Bison will assign unique numbers to these and export the #define
 * in the generated y.tab.h header file.
 */
%token   T_Void T_Bool T_Int T_Double T_String T_Class 
%token   T_LessEqual T_GreaterEqual T_Equal T_NotEqual T_Dims
%token   T_And T_Or T_Null T_Extends T_This T_Interface T_Implements
%token   T_While T_For T_If T_Else T_Return T_Break
%token   T_New T_NewArray T_Print T_ReadInteger T_ReadLine

%token   <identifier> T_Identifier
%token   <stringConstant> T_StringConstant 
%token   <integerConstant> T_IntConstant
%token   <doubleConstant> T_DoubleConstant
%token   <boolConstant> T_BoolConstant


/* Non-terminal types
 * ------------------
 * In order for yacc to assign/access the correct field of $$, $1, we
 * must to declare which field is appropriate for the non-terminal.
 * As an example, this first type declaration establishes that the DeclList
 * non-terminal uses the field named "declList" in the yylval union. This
 * means that when we are setting $$ for a reduction for DeclList ore reading
 * $n which corresponds to a DeclList nonterminal we are accessing the field
 * of the union named "declList" which is of type List<Decl*>.
 * pp2: You'll need to add many of these of your own.
 */
%type <declList>  DeclList 
%type <decl>      Decl
%type <expr>      Expr Constant
%type <lvalue>    LValue

/* Precedence Rules */

%nonassoc '='
%left T_Or
%left T_And
%nonassoc T_Equal T_NotEqual
%nonassoc '<' '>' T_LessEqual T_GreaterEqual
%left '+' '-'
%left '*' '/' '%'
%right UMINUS '!'
%left '[' '.'



%%
/* Rules
 * -----
 * All productions and actions should be placed between the start and stop
 * %% markers which delimit the Rules section.
	 
 */

/*
Program   :    DeclList            { 
                                      @1; 
                                      Program *program = new Program($1);
                                      // if no errors, advance to next phase
                                      if (ReportError::NumErrors() == 0) 
                                          program->Print(0);
                                    }
          | Expr
          ;

DeclList  :    DeclList Decl        { ($$=$1)->Append($2); }
          |    Decl                 { ($$ = new List<Decl*>)->Append($1); }
          ;

Decl      :    T_Void               { }
          ;
*/

Expr      : LValue '=' Expr { $$ = $1; } /* Fix */
          | Constant { $$ = $1; }
          | LValue { $$ = $1; }
          | T_This { $$ = new This(@1); }
          | Expr '+' Expr { $$ = new ArithmeticExpr($1, new Operator(@2, "+"), $3); }
          | Expr '-' Expr { $$ = new ArithmeticExpr($1, new Operator(@2, "-"), $3); }
          | Expr '*' Expr { $$ = new ArithmeticExpr($1, new Operator(@2, "*"), $3); }
          | Expr '/' Expr { $$ = new ArithmeticExpr($1, new Operator(@2, "/"), $3); }
          | Expr '<' Expr { $$ = new RelationalExpr($1, new Operator(@2, "<"), $3); }
          | Expr '>' Expr { $$ = new RelationalExpr($1, new Operator(@2, "<"), $3); }
          | Expr '%' Expr { $$ = new RelationalExpr($1, new Operator(@2, "<"), $3); }
          | Expr T_LessEqual    Expr { $$ = new RelationalExpr($1, new Operator(@2, "<="), $3); }
          | Expr T_GreaterEqual Expr { $$ = new RelationalExpr($1, new Operator(@2, ">="), $3); }
          ;


LValue    : T_Identifier  { $$ = new FieldAccess(nullptr, new Identifier(@1, $1)); }
          | Expr '.' T_Identifier { $$ = new FieldAccess($1, new Identifier(@3, $3)); }
          | Expr '[' Expr ']'  { $$ = new ArrayAccess(Join(@1, @4), $1, $3); }
          ;


Constant    : T_IntConstant { $$ = new IntConstant(@1, $1); }
            | T_BoolConstant { $$ = new BoolConstant(@1, $1); }
            | T_DoubleConstant { $$ = new DoubleConstant(@1, $1); }
            | T_StringConstant { $$ = new StringConstant(@1, $1); }
            ;

%%

/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated y.tab.c file.
 * This section is where you put definitions of helper functions.
 */

/* Function: InitParser
 * --------------------
 * This function will be called before any calls to yyparse().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the parser (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yydebug that controls whether yacc prints debugging information about
 * parser actions (shift/reduce) and contents of state stack during parser.
 * If set to false, no information is printed. Setting it to true will give
 * you a running trail that might be helpful when debugging your parser.
 * Please be sure the variable is set to false when submitting your final
 * version.
 */
void InitParser()
{
   PrintDebug("parser", "Initializing parser");
   yydebug = false;
}
