tok::Tok fsm::Finished<tok::Tok>::rejecting() {
  return tok::Tok::TInvalid;
}

void fsm::Finished<tok::Tok>::merge(tok::Tok &lhs, tok::Tok rhs) {
  lhs = std::max(lhs, rhs);
}
