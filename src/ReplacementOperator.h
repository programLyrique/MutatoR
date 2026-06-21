// ReplacementOperator.h
#ifndef REPLACEMENT_OPERATOR_H
#define REPLACEMENT_OPERATOR_H

#include "Operator.h"

// Operators that mutate by replacing a node's operator symbol in place (e.g.
// '+' -> '-', '<' -> '>'). Deletion is a separate kind of operation and does
// not inherit from this class, so it is not forced to implement flip().
class ReplacementOperator : public Operator {
public:
    ReplacementOperator(SEXP symbol) : Operator(symbol) {}
    virtual ~ReplacementOperator() = default;

    // Replace the operator at `node` with this operator's counterpart.
    virtual void flip(SEXP& node) const = 0;
};

#endif // REPLACEMENT_OPERATOR_H
