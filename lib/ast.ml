type t =
|Program of program
|Declaration of declaration
|Block of block
|BlockItem of block_item
|Statement of statement
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
|LabeledStmt of { label: ident; stmt: statement }
|Compound of block
|Break
|Continue
|While of { cond: expression; body: statement }
|DoWhile of {  body: statement; cond: expression }
|For of { init: for_init; cond: expression option; post: expression option; body: statement }
|Case of { expr: expression; stmt: statement option }
|Switch of { expr: expression; stmt: statement }
|Default of statement option

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