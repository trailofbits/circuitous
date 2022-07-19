# Copyright (c) 2021 Trail of Bits, Inc.

import defs.mov
import defs.lea
import defs.idiv
import defs.add

import defs.adc
import defs.sbb
import defs.shl
import defs.shr
import defs.xor
import defs.or_
import defs.and_
import defs.div
import defs.mul
import defs.imul
import defs.rcr
import defs.ror
import defs.rcl
import defs.ip
import defs.core
import defs.combinations

import defs.ebit
import defs.pop
import defs.push
import defs.timestamp
import defs.jmp
import defs.call
import defs.leave

import defs.ret
import defs.nop
import defs.neg
import defs.lahf
import defs.test
import defs.bsr
import defs.bt
import defs.movsx
import defs.movzx
import defs.cmc
import defs.sahf
import defs.bsf
import defs.bts
import defs.btr
import defs.btc

import defs.std
import defs.stc
import defs.cld
import defs.clc

import defs.popf
import defs.pushf

import defs.stringops
#import defs.x86

import defs.multistep.simple

def fetch(tests):
    out = set()
    out.update(_fetch_defs(defs.mov))
    out.update(_fetch_defs(defs.add))
    out.update(_fetch_defs(defs.lea))
    out.update(_fetch_defs(defs.idiv))

    out.update(_fetch_defs(defs.adc))
    out.update(_fetch_defs(defs.sbb))
    out.update(_fetch_defs(defs.shl))
    out.update(_fetch_defs(defs.shr))
    out.update(_fetch_defs(defs.xor))
    out.update(_fetch_defs(defs.or_))
    out.update(_fetch_defs(defs.and_))
    out.update(_fetch_defs(defs.div))

    out.update(_fetch_defs(defs.mul))
    out.update(_fetch_defs(defs.imul))
    out.update(_fetch_defs(defs.rcr))

    out.update(_fetch_defs(defs.ror))
    out.update(_fetch_defs(defs.rcl))
    out.update(_fetch_defs(defs.ip))

    out.update(_fetch_defs(defs.core))
    out.update(_fetch_defs(defs.combinations))

    out.update(_fetch_defs(defs.ebit))
    out.update(_fetch_defs(defs.pop))
    out.update(_fetch_defs(defs.push))
    out.update(_fetch_defs(defs.timestamp))
    out.update(_fetch_defs(defs.call))
    out.update(_fetch_defs(defs.jmp))

    out.update(_fetch_defs(defs.leave))
    out.update(_fetch_defs(defs.ret))

    out.update(_fetch_defs(defs.nop))
    out.update(_fetch_defs(defs.neg))
    out.update(_fetch_defs(defs.lahf))
    out.update(_fetch_defs(defs.test))
    out.update(_fetch_defs(defs.bsr))
    out.update(_fetch_defs(defs.bt))

    out.update(_fetch_defs(defs.movsx))
    out.update(_fetch_defs(defs.movzx))
    out.update(_fetch_defs(defs.cmc))
    out.update(_fetch_defs(defs.sahf))
    out.update(_fetch_defs(defs.bsf))

    out.update(_fetch_defs(defs.bts))
    out.update(_fetch_defs(defs.btr))
    out.update(_fetch_defs(defs.btc))

    out.update(_fetch_defs(defs.std))
    out.update(_fetch_defs(defs.stc))
    out.update(_fetch_defs(defs.cld))
    out.update(_fetch_defs(defs.clc))

    out.update(_fetch_defs(defs.popf))
    out.update(_fetch_defs(defs.pushf))

    out.update(_fetch_defs(defs.stringops))
    #out.update(_fetch_defs(defs.x86))

    out.update(_fetch_defs(defs.multistep.simple))

    return out


def _fetch_defs(mod):
    out = set()
    for x in mod.circuitous_tests:
        out.update(x)
    return out
