// Generated by gen-tokens.rkt
#pragma once
namespace ast {
  enum class Tok {
    TInvalid,
    TWhitespace,
    TBlockComment,
    TLineComment,
    TLinebreak,
    TIdent,
    TDefn,
    TIf,
    TElse,
    TLet,
    TIn,
    TTrue,
    TFalse,
    TInt,
    TFloat,
    TStr,
    TParOpen,
    TParClose,
    TBrOpen,
    TBrClose,
    TComma,
    TColon,
    TPlaceholder,
    TNe,
    TEq2,
    TEq,
    TLt,
    TGt,
    TLe,
    TGe,
    TOr1,
    TOr2,
    TAnd1,
    TAnd2,
    TShLeft,
    TShRight,
    TAdd,
    TSub,
    TMul,
    TDiv,
    TRem,
  };
}

#define TOKEN_PATTERNS {\
  {ast::Tok::TWhitespace, "[ \\t\\v\\f\\r]+"},\
  {ast::Tok::TBlockComment, "/\\*.*\\*/"},\
  {ast::Tok::TLineComment, "//[^\\n]*"},\
  {ast::Tok::TLinebreak, "[ \\t\\v\\f\\r]*\\n"},\
  {ast::Tok::TIdent, "[a-zA-Z_][a-zA-Z_0-9]*"},\
  {ast::Tok::TDefn, "defn"},\
  {ast::Tok::TIf, "if"},\
  {ast::Tok::TElse, "else"},\
  {ast::Tok::TLet, "let"},\
  {ast::Tok::TIn, "in"},\
  {ast::Tok::TTrue, "true"},\
  {ast::Tok::TFalse, "false"},\
  {ast::Tok::TInt, "0|[1-9][0-9]*"},\
  {ast::Tok::TFloat, "[0-9]+\\.[0-9]+"},\
  {ast::Tok::TStr, "\"([^\"\\\\]|\\\\[\"\\\\])*\""},\
  {ast::Tok::TParOpen, "\\("},\
  {ast::Tok::TParClose, "\\)"},\
  {ast::Tok::TBrOpen, "\\{"},\
  {ast::Tok::TBrClose, "\\}"},\
  {ast::Tok::TComma, ","},\
  {ast::Tok::TColon, ":"},\
  {ast::Tok::TPlaceholder, "_"},\
  {ast::Tok::TNe, "!="},\
  {ast::Tok::TEq2, "=="},\
  {ast::Tok::TEq, "="},\
  {ast::Tok::TLt, "<"},\
  {ast::Tok::TGt, ">"},\
  {ast::Tok::TLe, "<="},\
  {ast::Tok::TGe, ">="},\
  {ast::Tok::TOr1, "\\|"},\
  {ast::Tok::TOr2, "\\|\\|"},\
  {ast::Tok::TAnd1, "&"},\
  {ast::Tok::TAnd2, "&&"},\
  {ast::Tok::TShLeft, "<<"},\
  {ast::Tok::TShRight, ">>"},\
  {ast::Tok::TAdd, "\\+"},\
  {ast::Tok::TSub, "-"},\
  {ast::Tok::TMul, "\\*"},\
  {ast::Tok::TDiv, "/"},\
  {ast::Tok::TRem, "%"},\
}
