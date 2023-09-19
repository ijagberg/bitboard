use crate::Bitboard;

pub const INITIAL_STATE: Bitboard =
    Bitboard(0b1111111111111111000000000000000000000000000000001111111111111111);

const FILE_A: u64 = 0b0000000100000001000000010000000100000001000000010000000100000001;
pub const FILE_A_MASK: Bitboard = Bitboard::construct(FILE_A);
pub const FILE_A_CLEAR: Bitboard = Bitboard::construct(!FILE_A);

const FILE_B: u64 = 0b0000001000000010000000100000001000000010000000100000001000000010;
pub const FILE_B_MASK: Bitboard = Bitboard::construct(FILE_B);
pub const FILE_B_CLEAR: Bitboard = Bitboard::construct(!FILE_B);

const FILE_C: u64 = 0b0000010000000100000001000000010000000100000001000000010000000100;
pub const FILE_C_MASK: Bitboard = Bitboard::construct(FILE_C);
pub const FILE_C_CLEAR: Bitboard = Bitboard::construct(!FILE_C);

const FILE_D: u64 = 0b0000100000001000000010000000100000001000000010000000100000001000;
pub const FILE_D_MASK: Bitboard = Bitboard::construct(FILE_D);
pub const FILE_D_CLEAR: Bitboard = Bitboard::construct(!FILE_D);

const FILE_E: u64 = 0b0001000000010000000100000001000000010000000100000001000000010000;
pub const FILE_E_MASK: Bitboard = Bitboard::construct(FILE_E);
pub const FILE_E_CLEAR: Bitboard = Bitboard::construct(!FILE_E);

const FILE_F: u64 = 0b0010000000100000001000000010000000100000001000000010000000100000;
pub const FILE_F_MASK: Bitboard = Bitboard::construct(FILE_F);
pub const FILE_F_CLEAR: Bitboard = Bitboard::construct(!FILE_F);

const FILE_G: u64 = 0b0100000001000000010000000100000001000000010000000100000001000000;
pub const FILE_G_MASK: Bitboard = Bitboard::construct(FILE_G);
pub const FILE_G_CLEAR: Bitboard = Bitboard::construct(!FILE_G);

const FILE_H: u64 = 0b1000000010000000100000001000000010000000100000001000000010000000;
pub const FILE_H_MASK: Bitboard = Bitboard::construct(FILE_H);
pub const FILE_H_CLEAR: Bitboard = Bitboard::construct(!FILE_H);

const RANK_1: u64 = 0b0000000000000000000000000000000000000000000000000000000011111111;
pub const RANK_1_MASK: Bitboard = Bitboard::construct(RANK_1);
pub const RANK_1_CLEAR: Bitboard = Bitboard::construct(!RANK_1);

const RANK_2: u64 = 0b0000000000000000000000000000000000000000000000001111111100000000;
pub const RANK_2_MASK: Bitboard = Bitboard::construct(RANK_2);
pub const RANK_2_CLEAR: Bitboard = Bitboard::construct(!RANK_2);

const RANK_3: u64 = 0b0000000000000000000000000000000000000000111111110000000000000000;
pub const RANK_3_MASK: Bitboard = Bitboard::construct(RANK_3);
pub const RANK_3_CLEAR: Bitboard = Bitboard::construct(!RANK_3);

const RANK_4: u64 = 0b0000000000000000000000000000000011111111000000000000000000000000;
pub const RANK_4_MASK: Bitboard = Bitboard::construct(RANK_4);
pub const RANK_4_CLEAR: Bitboard = Bitboard::construct(!RANK_4);

const RANK_5: u64 = 0b0000000000000000000000001111111100000000000000000000000000000000;
pub const RANK_5_MASK: Bitboard = Bitboard::construct(RANK_5);
pub const RANK_5_CLEAR: Bitboard = Bitboard::construct(!RANK_5);

const RANK_6: u64 = 0b0000000000000000111111110000000000000000000000000000000000000000;
pub const RANK_6_MASK: Bitboard = Bitboard::construct(RANK_6);
pub const RANK_6_CLEAR: Bitboard = Bitboard::construct(!RANK_6);

const RANK_7: u64 = 0b0000000011111111000000000000000000000000000000000000000000000000;
pub const RANK_7_MASK: Bitboard = Bitboard::construct(RANK_7);
pub const RANK_7_CLEAR: Bitboard = Bitboard::construct(!RANK_7);

const RANK_8: u64 = 0b1111111100000000000000000000000000000000000000000000000000000000;
pub const RANK_8_MASK: Bitboard = Bitboard::construct(RANK_8);
pub const RANK_8_CLEAR: Bitboard = Bitboard::construct(!RANK_8);
