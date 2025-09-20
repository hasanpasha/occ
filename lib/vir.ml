type t =
|Program of program
|Declaration of declaration
|Block of block
|BlockItem of block_item
|Statement of statement
|ForInit of for_init
|Expression of expression

and program = declaration list

and declaration = 
|Function of { name: ident; body: block }
|Variable of { name: ident; init: expression option }

and block = block_item list

and block_item =
|Stmt of statement
|Decl of declaration

and statement =
|Return of expression
|Expr of expression
|Null
|If of { cond: expression; then': statement; else': statement option }
|Goto of ident
|LabeledStmt of { stmt: statement; label: ident }
|Compound of block
|Break of ident
|Continue of ident
|While of { cond: expression; body: statement; label: ident }
|DoWhile of {  body: statement; cond: expression; label: ident }
|For of { init: for_init; cond: expression option; post: expression option; body: statement; label: ident }
|Switch of { expr: expression; stmt: statement; label: ident; cases: (expression * ident) list; default: ident option }
|Case of { expr: expression; stmt: statement option; label: ident }
|Default of { stmt: statement option; label: ident }

and for_init =
|Decl of declaration
|Expr of expression option

and expression =
|IntLit of int
|LeftUnary of { operator: Token.t; rhs: expression }
|RightUnary of { operator: Token.t; lhs: expression }
|Binary of { operator: Token.t; lhs: expression; rhs: expression }
|Assignment of { operator: Token.t; lhs: expression; rhs: expression }
|Var of ident
|Conditional of { cond: expression; true': expression; false': expression }

and ident = string
[@@deriving show, eq]