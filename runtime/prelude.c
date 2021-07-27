/*
 * This module implements the external functions from the
 * Curry prelude in C.
 */

#include <stdio.h>
#include "runtime.h"

#define UNIMPLEMENTED(NAME) { \
    fprintf(stderr, "External function " NAME " is not implemented yet!");\
    exit(0);\
}

void external_Prelude_prim_eqChar(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_eqChar")
}

void external_Prelude_prim_eqInt(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_eqInt")
}

void external_Prelude_prim_eqFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_eqFloat")
}

void external_Prelude_prim_ltEqChar(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_ltEqChar")
}

void external_Prelude_prim_ltEqInt(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_ltEqInt")
}

void external_Prelude_prim_ltEqFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_ltEqFloat")
}

void external_Prelude_prim_showCharLiteral(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_showCharLiteral")
}

void external_Prelude_prim_showStringLiteral(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_showStringLiteral")
}

void external_Prelude_prim_showIntLiteral(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_showIntLiteral")
}

void external_Prelude_prim_showFloatLiteral(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_showFloatLiteral")
}

void external_Prelude_prim_readCharLiteral(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_readCharLiteral")
}

void external_Prelude_prim_readStringLiteral(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_readStringLiteral")
}

void external_Prelude_prim_readNatLiteral(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_readNatLiteral")
}

void external_Prelude_prim_readFloatLiteral(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_readFloatLiteral")
}

void external_Prelude_prim_plusInt(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_plusInt")
}

void external_Prelude_prim_minusInt(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_minusInt")
}

void external_Prelude_prim_timesInt(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_timesInt")
}

void external_Prelude_prim_plusFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_plusFloat")
}

void external_Prelude_prim_minusFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_minusFloat")
}

void external_Prelude_prim_timesFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_timesFloat")
}

void external_Prelude_prim_negateFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_negateFloat")
}

void external_Prelude_prim_intToFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_intToFloat")
}

void external_Prelude_prim_divFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_divFloat")
}

void external_Prelude_prim_divInt(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_divInt")
}

void external_Prelude_prim_modInt(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_modInt")
}

void external_Prelude_prim_quotInt(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_quotInt")
}

void external_Prelude_prim_remInt(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_remInt")
}

void external_Prelude_prim_truncateFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_truncateFloat")
}

void external_Prelude_prim_roundFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_roundFloat")
}

void external_Prelude_prim_logFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_logFloat")
}

void external_Prelude_prim_expFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_expFloat")
}

void external_Prelude_prim_sqrtFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_sqrtFloat")
}

void external_Prelude_prim_sinFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_sinFloat")
}

void external_Prelude_prim_cosFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_cosFloat")
}

void external_Prelude_prim_tanFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_tanFloat")
}

void external_Prelude_prim_asinFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_asinFloat")
}

void external_Prelude_prim_acosFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_acosFloat")
}

void external_Prelude_prim_atanFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_atanFloat")
}

void external_Prelude_prim_sinhFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_sinhFloat")
}

void external_Prelude_prim_coshFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_coshFloat")
}

void external_Prelude_prim_tanhFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_tanhFloat")
}

void external_Prelude_prim_asinhFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_asinhFloat")
}

void external_Prelude_prim_acoshFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_acoshFloat")
}

void external_Prelude_prim_atanhFloat(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_atanhFloat")
}

void external_Prelude_prim_ord(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_ord")
}

void external_Prelude_prim_chr(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_chr")
}

void external_Prelude_dolexcl(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.dolexcl")
}

void external_Prelude_dolexclexcl(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.dolexclexcl")
}

void external_Prelude_dolhashhash(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.dolhashhash")
}

void external_Prelude_ensureNotFree(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.ensureNotFree")
}

void external_Prelude_bindIO(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.bindIO")
}

void external_Prelude_seqIO(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.seqIO")
}

void external_Prelude_returnIO(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.returnIO")
}

void external_Prelude_getChar(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.getChar")
}

void external_Prelude_prim_putChar(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_putChar")
}

void external_Prelude_prim_readFile(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_readFile")
}

void external_Prelude_prim_readFileContents(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_readFileContents")
}

void external_Prelude_prim_writeFile(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_writeFile")
}

void external_Prelude_prim_appendFile(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_appendFile")
}

void external_Prelude_catch(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.catch")
}

void external_Prelude_constrEq(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.constrEq")
}

void external_Prelude_nonstrictEq(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.nonstrictEq")
}

void external_Prelude_unifEqLinear(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.unifEqLinear")
}

void external_Prelude_ifVar(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.ifVar")
}

void external_Prelude_and(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.and")
}

void external_Prelude_failed(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.failed")
}

void external_Prelude_prim_error(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.prim_error")
}

void external_Prelude_apply(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.apply")
}

void external_Prelude_cond(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.cond")
}

void external_Prelude_letrec(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.letrec")
}

void external_Prelude_failure(struct CurryNode *node) {
    UNIMPLEMENTED("Prelude.failure")
}


