#ifndef ARITHMETIC_OPERATOR_H
#define ARITHMETIC_OPERATOR_H

#include <vector>
#include "ReplacementOperator.h"

class ArithmeticOperator : public ReplacementOperator {
public:
    ArithmeticOperator(SEXP symbol) : ReplacementOperator(symbol) {}
    virtual ~ArithmeticOperator() = default;

    virtual std::string getType() const override {
        return "ArithmeticOperator";
    }
};

#endif