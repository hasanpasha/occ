type t = declaration list

and declaration =
  | Variable of variable_declaration
  | Function of function_declaration

and variable_declaration = { name : ident; init : expression option }

and function_declaration = {
  name : ident;
  params : ident list;
  body : block option;
}

and block = block_item list
and block_item = Stmt of statement | Decl of declaration

and statement =
  | Return of expression
  | Expr of expression
  | Null
  | If of { cond : expression; then' : statement; else' : statement option }
  | Goto of ident
  | LabeledStmt of { stmt : statement; label : ident }
  | Compound of block
  | Break of ident
  | Continue of ident
  | While of { cond : expression; body : statement; label : ident }
  | DoWhile of { body : statement; cond : expression; label : ident }
  | For of {
      init : for_init;
      cond : expression option;
      post : expression option;
      body : statement;
      label : ident;
    }
  | Switch of {
      expr : expression;
      stmt : statement;
      label : ident;
      cases : (expression * ident) list;
      default : ident option;
    }
  | Case of { expr : expression; stmt : statement option; label : ident }
  | Default of { stmt : statement option; label : ident }

and for_init = VarDecl of variable_declaration | Expr of expression option

and expression =
  | IntLit of int
  | LeftUnary of { operator : Token.t; rhs : expression }
  | RightUnary of { operator : Token.t; lhs : expression }
  | Binary of { operator : Token.t; lhs : expression; rhs : expression }
  | Assignment of { operator : Token.t; lhs : expression; rhs : expression }
  | Var of ident
  | Conditional of {
      cond : expression;
      true' : expression;
      false' : expression;
    }
  | FunctionCall of { name : ident; args : expression list }

and ident = string [@@deriving show, eq]
