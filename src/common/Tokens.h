// Generated by gen-tokens.rkt
#pragma once
namespace tok {
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
    TThen,
    TLet,
    TIn,
    TTrue,
    TFalse,
    TFn,
    TForeign,
    TInt,
    TFloat,
    TStr,
    TParOpen,
    TTupleOpen,
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
    TAdd,
    TSub,
    TMul,
    TDiv,
    TRem,
    TBackslash,
    TLambda,
    TDot,
  };
}

#define TOKEN_PATTERNS {\
  {tok::Tok::TWhitespace, "[ \\t\\v\\f\\r]+"},\
  {tok::Tok::TBlockComment, "/\\*([^*]|\\*[^/])*\\*/"},\
  {tok::Tok::TLineComment, "(//|#!)[^\\n]*"},\
  {tok::Tok::TLinebreak, "[ \\t\\v\\f\\r]*\\n"},\
  {tok::Tok::TIdent, "[a-zA-Z_][a-zA-Z_0-9]*"},\
  {tok::Tok::TDefn, "defn"},\
  {tok::Tok::TIf, "if"},\
  {tok::Tok::TElse, "else"},\
  {tok::Tok::TThen, "then"},\
  {tok::Tok::TLet, "let"},\
  {tok::Tok::TIn, "in"},\
  {tok::Tok::TTrue, "true"},\
  {tok::Tok::TFalse, "false"},\
  {tok::Tok::TFn, "fn"},\
  {tok::Tok::TForeign, "foreign"},\
  {tok::Tok::TInt, "0|[1-9](_?[0-9])*"},\
  {tok::Tok::TFloat, "(0|[1-9](_?[0-9])*)?\\.[0-9](_?[0-9])*"},\
  {tok::Tok::TStr, "\"([^\"\\\\]|\\\\[\"\\\\tvfrn])*\""},\
  {tok::Tok::TParOpen, "\\("},\
  {tok::Tok::TTupleOpen, "#\\("},\
  {tok::Tok::TParClose, "\\)"},\
  {tok::Tok::TBrOpen, "\\{"},\
  {tok::Tok::TBrClose, "\\}"},\
  {tok::Tok::TComma, ","},\
  {tok::Tok::TColon, ":"},\
  {tok::Tok::TPlaceholder, "_"},\
  {tok::Tok::TNe, "!="},\
  {tok::Tok::TEq2, "=="},\
  {tok::Tok::TEq, "="},\
  {tok::Tok::TLt, "<"},\
  {tok::Tok::TGt, ">"},\
  {tok::Tok::TLe, "<="},\
  {tok::Tok::TGe, ">="},\
  {tok::Tok::TOr1, "\\|"},\
  {tok::Tok::TOr2, "\\|\\|"},\
  {tok::Tok::TAnd1, "&"},\
  {tok::Tok::TAnd2, "&&"},\
  {tok::Tok::TAdd, "\\+"},\
  {tok::Tok::TSub, "-"},\
  {tok::Tok::TMul, "\\*"},\
  {tok::Tok::TDiv, "/"},\
  {tok::Tok::TRem, "%"},\
  {tok::Tok::TBackslash, "\\\\"},\
  {tok::Tok::TLambda, "λ"},\
  {tok::Tok::TDot, "\\."},\
}
