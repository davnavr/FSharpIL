namespace FSharpIL.Metadata.Cil

/// Represents a single CIL instruction (III.1).
type Opcode =
    | Nop = 0us
    | Break = 1us
    | Ldarg_0 = 2us
    | Ldarg_1 = 3us
    | Ldarg_2 = 4us
    | Ldarg_3 = 5us
    | Ldloc_0 = 6us
    | Ldloc_1 = 7us
    | Ldloc_2 = 8us
    | Ldloc_3 = 9us
    | Stloc_0 = 0xAus
    | Stloc_1 = 0xBus
    | Stloc_2 = 0xCus
    | Stloc_3 = 0xDus
    | Ldarg_s = 0xEus

    | Starg_s = 0x10us
    | Ldloc_s = 0x11us

    | Stloc_s = 0x13us
    | Ldnull = 0x14us
    | Ldc_i4_m1 = 0x15us
    | Ldc_i4_0 = 0x16us
    | Ldc_i4_1 = 0x17us
    | Ldc_i4_2 = 0x18us
    | Ldc_i4_3 = 0x19us
    | Ldc_i4_4 = 0x1Aus
    | Ldc_i4_5 = 0x1Bus
    | Ldc_i4_6 = 0x1Cus
    | Ldc_i4_7 = 0x1Dus
    | Ldc_i4_8 = 0x1Eus
    | Ldc_i4_s = 0x1Fus
    | Ldc_i4 = 0x20us
    | Ldc_i8 = 0x21us
    | Ldc_r4 = 0x22us
    | Ldc_r8 = 0x23us

    | Dup = 0x25us
    | Pop = 0x26us

    | Call = 0x28us

    | Ret = 0x2Aus
    | Br_s = 0x2Bus
    | Brfalse_s = 0x2Cus
    | Brnull_s = 0x2Cus
    | Brzero_s = 0x2Cus
    | Brinst_s = 0x2Dus
    | Brtrue_s = 0x2Dus
    | Beq_s = 0x2Eus
    | Bge_s = 0x2Fus

    | Bgt_s = 0x30us
    | Ble_s = 0x31us

    | Blt_s = 0x32us
    | Bne_un_s = 0x33us

    | Br = 0x38us
    | Brfalse = 0x39us

    | Beq = 0x3Bus
    | Bge = 0x3Cus

    | Ble = 0x3Eus
    | Blt = 0x3Fus
    | Bne_un = 0x40us

    | Ldind_u1 = 0x47us

    | Stind_i1 = 0x52us

    | Add = 0x58us
    | Sub = 0x59us
    | Mul = 0x5Aus
    | Div = 0x5Bus
    | Div_un = 0x5Bus
    | Rem = 0x5Dus
    | Rem_un = 0x5Eus
    | And = 0x5Fus
    | Or = 0x60us
    | Xor = 0x61us
    | Shl = 0x62us
    | Shr = 0x63us
    | Shr_un = 0x64us
    | Neg = 0x65us
    | Not = 0x66us
    | Conv_i1 = 0x67us
    | Conv_i2 = 0x68us
    | Conv_i4 = 0x69us
    | Conv_i8 = 0x6Aus
    | Conv_r4 = 0x6Bus
    | Conv_r8 = 0x6Cus
    | Conv_u4 = 0x6Dus
    | Conv_u8 = 0x6Eus
    | Callvirt = 0x6Fus

    | Ldstr = 0x72us
    | Newobj = 0x73us
    | Castclass = 0x74us

    | Conv_r_un = 0x76us

    | Throw = 0x7Aus
    | Ldfld = 0x7Bus
    | Ldflda = 0x7Cus
    | Stfld = 0x7Dus
    | Ldsfld = 0x7Eus
    | Ldsflda = 0x7Fus

    | Stsfld = 0x80us

    | Conv_ovf_i1_un = 0x82us
    | Conv_ovf_i2_un = 0x83us
    | Conv_ovf_i4_un = 0x84us
    | Conv_ovf_i8_un = 0x85us
    | Conv_ovf_u1_un = 0x86us
    | Conv_ovf_u2_un = 0x87us
    | Conv_ovf_u4_un = 0x88us
    | Conv_ovf_u8_un = 0x89us
    | Conv_ovf_i_un = 0x8Aus
    | Conv_ovf_u_un = 0x8Bus
    | Box = 0x8Cus
    | Newarr = 0x8Dus
    | Ldlen = 0x8Eus

    | Ldelem = 0xA3us
    | Stelem = 0xA4us

    | Conv_ovf_i1 = 0xB3us
    | Conv_ovf_u1 = 0xB4us
    | Conv_ovf_i2 = 0xB5us
    | Conv_ovf_u2 = 0xB6us
    | Conv_ovf_i4 = 0xB7us
    | Conv_ovf_u4 = 0xB8us
    | Conv_ovf_i8 = 0xB9us
    | Conv_ovf_u8 = 0xBAus

    | Ckfinite = 0xC3us

    | Conv_u2 = 0xD1us
    | Conv_u1 = 0xD2us
    | Conv_i = 0xD3us
    | Conv_ovf_i = 0xD4us
    | Conv_ovf_u = 0xD5us
    | Add_ovf = 0xD6us
    | Add_ovf_un = 0xD7us
    | Mul_ovf = 0xD8us
    | Mul_ovf_un = 0xD9us
    | Sub_ovf = 0xDAus
    | Sub_ovf_un = 0xDBus
    //| Endfault = 0xDCus
    | Endfinally = 0xDCus

    | Conv_u = 0xE0us

    | Arglist = 0xFE_00us
    | Ceq = 0xFE_01us
    | Cgt = 0xFE_02us
    | Cgt_un = 0xFE_03us
    | Clt = 0xFE_04us
    | Clt_un = 0xFE_05us
    | Ldftn = 0xFE_06us

    | Ldarg = 0xFE_09us

    | Starg = 0xFE_0Bus
    | Ldloc = 0xFE_0Cus

    | Stloc = 0xFE_0Eus
    | Localloc = 0xFE_0Fus

    | Endfilter = 0xFE_11us

    | Volatile_ = 0xFE_13us
    | Tail_ = 0xFE_14us

    | Cpblk = 0xFE_17us
    | Initblk = 0xFE_18us

    | Rethrow = 0xFE_1Aus

    | Refanytype = 0xFE_1Dus
    | Readonly_ = 0xFE_1Eus

[<RequireQualifiedAccess>]
module ParsedOpcode = // TODO: Module name should be Opcode.
    let private names = System.Collections.Generic.Dictionary<Opcode, string>()
    let name (opcode: Opcode) =
        match names.TryGetValue opcode with
        | true, existing -> existing
        | false, _ ->
            let name = opcode.ToString().ToLowerInvariant().Replace('_', '.')
            names.[opcode] <- name
            name

[<RequireQualifiedAccess>]
module Opcode =
    let size opcode =
        if opcode < Opcode.Arglist
        then 1u
        else 2u
