%{
	open Ic
%}

%token EOF
%token <int> V
%token T
%token B
%token N
%token AND
%token OR
%token I
%token E
%token X
%token LPAR RPAR

%left OR
%left AND
%left I
%left E
%left X
%left N

%start terminated_expr
%type <Ic.prop> terminated_expr

%%

terminated_expr:
	| expr EOF { $1 }

expr:
	| T 				{ TOP }
	| B					{ BOT }
	| N expr 			{ NOT $2 }
	| V					{ VAR $1 }
	| expr AND expr		{ CONJ ($1, $3)}
	| expr OR expr 		{ DISJ ($1, $3)}
	| expr I expr 		{ IMP ($1, $3)}
	| expr E expr 		{ EQUIV ($1, $3)}
	| expr X expr 		{ XOR ($1, $3)}
	| LPAR expr RPAR	{ $2 }

%%
