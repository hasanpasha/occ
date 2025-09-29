type t =
  (* identifier *)
  | Ident of string
  | IntLit of int
  (* symbols *)
  | LParen
  | RParen
  | LCub
  | RCub
  | Semi
  | Tilde
  | Excl
  | Plus
  | PlusPlus
  | PlusEqual
  | Minus
  | MinusMinus
  | MinusEqual
  | Astrsk
  | AstrskEqual
  | Slash
  | SlashEqual
  | Percnt
  | PercntEqual
  | Amp
  | AmpAmp
  | AmpEqual
  | Hat
  | HatEqual
  | Verbar
  | VerbarVarbar
  | VerbarEqual
  | Lt
  | LtLt
  | LtEqual
  | LtLtEqual
  | Gt
  | GtGt
  | GtEqual
  | GtGtEqual
  | Equal
  | EqualEqual
  | ExclEqual
  | Quest
  | Colon
  | Comma
  (* keywords *)
  | Return
  | If
  | Else
  | Goto
  | While
  | Do
  | For
  | Break
  | Continue
  | Switch
  | Case
  | Default
  (* types *)
  | Int
  | Void
  (* class *)
  | Static
  | Extern
[@@deriving show, eq]

type opt_t = t option [@@deriving show, eq]
