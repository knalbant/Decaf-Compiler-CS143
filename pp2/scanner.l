/* File:  scanner.l
 * ----------------
 * Lex input file to generate the scanner for the compiler.
 */

%{

/* The text within this first region delimited by %{ and %} is assumed to
 * be C/C++ code and will be copied verbatim to the lex.yy.c file ahead
 * of the definitions of the yylex() function. Add other header file inclusions
 * or C++ variable declarations/prototypes that are needed by your code here.
 */

#include <string.h>
#include <string>
#include <vector>
#include <algorithm>
#include <sstream>
#include "scanner.h"
#include "utility.h" // for PrintDebug()
#include "errors.h"
#include "parser.h"

/* Macro: YY_USER_ACTION 

 * ---------------------
 * This flex built-in macro can be defined to provide an action which is
 * always executed prior to any matched rule's action. Basically, it is
 * a way of having a piece of code common to all actions factored out to
 * this routine.  We already defined it for you and left the empty
 * function DoBeforeEachAction ready for your use as needed. It will
 * be called once for each pattern scanned from the file, before
 * executing its action.
 */
static void DoBeforeEachAction(); 
#define YY_USER_ACTION DoBeforeEachAction();

int lineno = 1;
int colno  = 1;

constexpr int TAB_WIDTH = 8;

static std::vector<std::string> lexed_lines;
static std::string current_line;

%}

%option noyywrap

 /* The section before the first %% is the Definitions section of the lex
  * input file. Here is where you set options for the scanner, define lex
  * states, and can set up definitions to give names to regular expressions
  * as a simple substitution mechanism that allows for more readable
  * entries in the Rules section later.
  */
IDENTIFIER       [A-Za-z][A-Z0-9_a-z]*

DIMS             "[]"

BOOL_LIT         "true"|"false"

DIGIT            [0-9]
HEX_DIGIT        [0-9a-fA-F]
DECIMAL_LIT      [0-9]+
HEX_PRE          0(x|X)
HEX_LIT          {HEX_PRE}{HEX_DIGIT}+
INT_LIT          {HEX_LIT}|{DECIMAL_LIT}

EXP              ([Ee][+-]?{DIGIT}+)
DOUBLE_LIT       {DIGIT}+"."{DIGIT}+?{EXP}?

STRING_LIT       \".*\"

DOUBLE_QUOTE     \"

VALID_STRING     {DOUBLE_QUOTE}[^\n\"]*{DOUBLE_QUOTE}
INVALID_STRING   {DOUBLE_QUOTE}[^\n\"]*

NEWLINE          "\n"|"\r\n"

%x MULTI_COMMENT
S_COMMENT        "//".*
M_COMMENT_S      "/*"
M_COMMENT_F      "*/"

LTE    <=
GTE    >=
EQ     ==
NEQ    !=
AND    &&
OR     "\|\|"

SINGLE_CHAR_OPS_PUNC  [+\-*/}{\[\])(\.,;!=%><]

%%             /* BEGIN RULES SECTION */
 /* All patterns and actions should be placed between the start and stop
  * %% markers which delimit the Rules section.
  */

 /*
  * The following set of definitions correspond to reserved words in the
  */
"int"             { return T_Int; }
"double"          { return T_Double; }
"bool"            { return T_Bool; }
"string"          { return T_String; }
"class"           { return T_Class; }
"null"            { return T_Null; }
{DIMS}            { return T_Dims; }

"void"            { return T_Void; }
"while"           { return T_While; }
"for"             { return T_For; }
"if"              { return T_If; }
"else"            { return T_Else; }
"return"          { return T_Return; }
"break"           { return T_Break; }
"extends"         { return T_Extends; }
"this"            { return T_This; }
"implements"      { return T_Implements; }
"interface"       { return T_Interface; }
"new"             { return T_New; }

"NewArray"        { return T_NewArray; }
"Print"           { return T_Print; }
"ReadInteger"     { return T_ReadInteger; }
"ReadLine"        { return T_ReadLine; }


 /* Deal with all multi-characer operators */
{AND} { return T_And; }
{OR}  { return T_Or; }
{LTE} { return T_LessEqual; }
{GTE} { return T_GreaterEqual; }
{EQ}  { return T_Equal; }
{NEQ} { return T_NotEqual; }

 /* Handle all single character operators and misc. punctuation */
{SINGLE_CHAR_OPS_PUNC} { return *yytext; }

 /*
  * The following actions deal with lexing various kinds of literals
  */
{VALID_STRING}    { yylval.stringConstant = strdup(yytext); return T_StringConstant; }
{INVALID_STRING}  { ReportError::UntermString(&yylloc, yytext); }
{BOOL_LIT}        { yylval.boolConstant = *yytext == 't'; return T_BoolConstant; }
{DOUBLE_LIT}      { yylval.doubleConstant = atof(yytext); return T_DoubleConstant; }
{HEX_LIT}         { std::stringstream ss(yytext);
                    ss >> std::hex >> yylval.integerConstant;
                    return T_IntConstant; }
{DECIMAL_LIT}     { yylval.integerConstant = atoi(yytext); return T_IntConstant; }


{S_COMMENT}                   { }
{M_COMMENT_S}                 { BEGIN(MULTI_COMMENT); }
<MULTI_COMMENT>{M_COMMENT_F}  { BEGIN(INITIAL); }
<MULTI_COMMENT>{NEWLINE}      { ++lineno; colno = 1; }
<MULTI_COMMENT>.              { }
<MULTI_COMMENT><<EOF>>        { BEGIN(INITIAL); ReportError::UntermComment(); }


{IDENTIFIER}      { size_t n = yyleng;
                    if (yyleng > MaxIdentLen) {
                        n = MaxIdentLen;
                        ReportError::LongIdentifier(&yylloc, yytext);
                    }
                    strncpy(yylval.identifier, yytext, n);
                    yylval.identifier[n] = '\0';
                    return T_Identifier; }

<*>[\t]           {
                    int len = yyleng * TAB_WIDTH;
                    yylloc.last_column = yylloc.first_column + len - 1;
                    colno = yylloc.first_column + len;
                  }

[ ]+              {  }

<*>{NEWLINE}      { 
                    current_line.pop_back();
                    colno = 1; 
                    lineno++; 
                    lexed_lines.push_back(std::move(current_line));
                    current_line.clear();
                  }

.                 { ReportError::UnrecogChar(&yylloc, *yytext); }


%%
/* The closing %% above marks the end of the Rules section and the beginning
 * of the User Subroutines section. All text from here to the end of the
 * file is copied verbatim to the end of the generated lex.yy.c file.
 * This section is where you put definitions of helper functions.
 */


/* Function: InitScanner
 * ---------------------
 * This function will be called before any calls to yylex().  It is designed
 * to give you an opportunity to do anything that must be done to initialize
 * the scanner (set global variables, configure starting state, etc.). One
 * thing it already does for you is assign the value of the global variable
 * yy_flex_debug that controls whether flex prints debugging information
 * about each token and what rule was matched. If set to false, no information
 * is printed. Setting it to true will give you a running trail that might
 * be helpful when debugging your scanner. Please be sure the variable is
 * set to false when submitting your final version.
 */
void InitScanner()
{
    PrintDebug("lex", "Initializing scanner");
    yy_flex_debug = false;
}


const char* GetLineNumbered(int num) {
    if ( num <= 0 || num >= lexed_lines.size() ) return nullptr;
    return lexed_lines[num - 1].c_str();
}

/* Function: DoBeforeEachAction()
 * ------------------------------
 * This function is installed as the YY_USER_ACTION. This is a place
 * to group code common to all actions.
 */
static void DoBeforeEachAction()
{
    yylloc.first_line = lineno;
    yylloc.first_column = colno;
    yylloc.last_column = yylloc.first_column + yyleng - 1;
    colno += yyleng;
    current_line += yytext;
}
