use mold_syntax::SyntaxKind;

#[derive(Clone, Copy)]
pub struct TokenSet(u128, u128, u128, u128);

impl TokenSet {
    pub const EMPTY: TokenSet = TokenSet(0, 0, 0, 0);

    pub const fn new(kinds: &[SyntaxKind]) -> TokenSet {
        let mut set = TokenSet(0, 0, 0, 0);
        let mut i = 0;
        while i < kinds.len() {
            set = set.with(kinds[i]);
            i += 1;
        }
        set
    }

    const fn with(self, kind: SyntaxKind) -> TokenSet {
        let bit = kind as u32;
        let (slot, offset) = (bit / 128, bit % 128);
        let mask = 1u128 << offset;
        match slot {
            0 => TokenSet(self.0 | mask, self.1, self.2, self.3),
            1 => TokenSet(self.0, self.1 | mask, self.2, self.3),
            2 => TokenSet(self.0, self.1, self.2 | mask, self.3),
            3 => TokenSet(self.0, self.1, self.2, self.3 | mask),
            _ => self,
        }
    }

    pub const fn contains(&self, kind: SyntaxKind) -> bool {
        let bit = kind as u32;
        let (slot, offset) = (bit / 128, bit % 128);
        let mask = 1u128 << offset;
        match slot {
            0 => self.0 & mask != 0,
            1 => self.1 & mask != 0,
            2 => self.2 & mask != 0,
            3 => self.3 & mask != 0,
            _ => false,
        }
    }

    pub const fn union(self, other: TokenSet) -> TokenSet {
        TokenSet(
            self.0 | other.0,
            self.1 | other.1,
            self.2 | other.2,
            self.3 | other.3,
        )
    }
}

#[macro_export]
macro_rules! T {
    [;] => { $crate::SyntaxKind::SEMICOLON };
    [,] => { $crate::SyntaxKind::COMMA };
    [.] => { $crate::SyntaxKind::DOT };
    [:] => { $crate::SyntaxKind::COLON };
    [::] => { $crate::SyntaxKind::DOUBLE_COLON };
    ['('] => { $crate::SyntaxKind::L_PAREN };
    [')'] => { $crate::SyntaxKind::R_PAREN };
    ['['] => { $crate::SyntaxKind::L_BRACKET };
    [']'] => { $crate::SyntaxKind::R_BRACKET };
    ['{'] => { $crate::SyntaxKind::L_BRACE };
    ['}'] => { $crate::SyntaxKind::R_BRACE };
    [=] => { $crate::SyntaxKind::EQ };
    [<>] => { $crate::SyntaxKind::NE };
    [<] => { $crate::SyntaxKind::LT };
    [<=] => { $crate::SyntaxKind::LE };
    [>] => { $crate::SyntaxKind::GT };
    [>=] => { $crate::SyntaxKind::GE };
    [+] => { $crate::SyntaxKind::PLUS };
    [-] => { $crate::SyntaxKind::MINUS };
    [*] => { $crate::SyntaxKind::STAR };
    [/] => { $crate::SyntaxKind::SLASH };
    [%] => { $crate::SyntaxKind::PERCENT };
    [||] => { $crate::SyntaxKind::PIPE_PIPE };
    [->] => { $crate::SyntaxKind::ARROW };
    [->>] => { $crate::SyntaxKind::ARROW_TEXT };
    [#>] => { $crate::SyntaxKind::HASH_ARROW };
    [#>>] => { $crate::SyntaxKind::HASH_ARROW_TEXT };
    [@>] => { $crate::SyntaxKind::AT_GT };
    [<@] => { $crate::SyntaxKind::LT_AT };
    [?] => { $crate::SyntaxKind::QUESTION };
    [?|] => { $crate::SyntaxKind::QUESTION_PIPE };
    [?&] => { $crate::SyntaxKind::QUESTION_AMP };
    [#-] => { $crate::SyntaxKind::HASH_MINUS };
    [@?] => { $crate::SyntaxKind::AT_QUESTION };
    [@@] => { $crate::SyntaxKind::AT_AT };
    [select] => { $crate::SyntaxKind::SELECT_KW };
    [from] => { $crate::SyntaxKind::FROM_KW };
    [where] => { $crate::SyntaxKind::WHERE_KW };
    [and] => { $crate::SyntaxKind::AND_KW };
    [or] => { $crate::SyntaxKind::OR_KW };
    [not] => { $crate::SyntaxKind::NOT_KW };
    [as] => { $crate::SyntaxKind::AS_KW };
    [join] => { $crate::SyntaxKind::JOIN_KW };
    [inner] => { $crate::SyntaxKind::INNER_KW };
    [left] => { $crate::SyntaxKind::LEFT_KW };
    [right] => { $crate::SyntaxKind::RIGHT_KW };
    [full] => { $crate::SyntaxKind::FULL_KW };
    [cross] => { $crate::SyntaxKind::CROSS_KW };
    [outer] => { $crate::SyntaxKind::OUTER_KW };
    [on] => { $crate::SyntaxKind::ON_KW };
    [using] => { $crate::SyntaxKind::USING_KW };
    [group] => { $crate::SyntaxKind::GROUP_KW };
    [by] => { $crate::SyntaxKind::BY_KW };
    [having] => { $crate::SyntaxKind::HAVING_KW };
    [order] => { $crate::SyntaxKind::ORDER_KW };
    [asc] => { $crate::SyntaxKind::ASC_KW };
    [desc] => { $crate::SyntaxKind::DESC_KW };
    [nulls] => { $crate::SyntaxKind::NULLS_KW };
    [first] => { $crate::SyntaxKind::FIRST_KW };
    [last] => { $crate::SyntaxKind::LAST_KW };
    [limit] => { $crate::SyntaxKind::LIMIT_KW };
    [offset] => { $crate::SyntaxKind::OFFSET_KW };
    [fetch] => { $crate::SyntaxKind::FETCH_KW };
    [distinct] => { $crate::SyntaxKind::DISTINCT_KW };
    [all] => { $crate::SyntaxKind::ALL_KW };
    [case] => { $crate::SyntaxKind::CASE_KW };
    [when] => { $crate::SyntaxKind::WHEN_KW };
    [then] => { $crate::SyntaxKind::THEN_KW };
    [else] => { $crate::SyntaxKind::ELSE_KW };
    [end] => { $crate::SyntaxKind::END_KW };
    [cast] => { $crate::SyntaxKind::CAST_KW };
    [null] => { $crate::SyntaxKind::NULL_KW };
    [true] => { $crate::SyntaxKind::TRUE_KW };
    [false] => { $crate::SyntaxKind::FALSE_KW };
    [is] => { $crate::SyntaxKind::IS_KW };
    [in] => { $crate::SyntaxKind::IN_KW };
    [between] => { $crate::SyntaxKind::BETWEEN_KW };
    [like] => { $crate::SyntaxKind::LIKE_KW };
    [ilike] => { $crate::SyntaxKind::ILIKE_KW };
    [exists] => { $crate::SyntaxKind::EXISTS_KW };
    [over] => { $crate::SyntaxKind::OVER_KW };
    [partition] => { $crate::SyntaxKind::PARTITION_KW };
    [rows] => { $crate::SyntaxKind::ROWS_KW };
    [range] => { $crate::SyntaxKind::RANGE_KW };
    [groups] => { $crate::SyntaxKind::GROUPS_KW };
    [unbounded] => { $crate::SyntaxKind::UNBOUNDED_KW };
    [preceding] => { $crate::SyntaxKind::PRECEDING_KW };
    [following] => { $crate::SyntaxKind::FOLLOWING_KW };
    [current] => { $crate::SyntaxKind::CURRENT_KW };
    [row] => { $crate::SyntaxKind::ROW_KW };
    [filter] => { $crate::SyntaxKind::FILTER_KW };
    [within] => { $crate::SyntaxKind::WITHIN_KW };
    [insert] => { $crate::SyntaxKind::INSERT_KW };
    [update] => { $crate::SyntaxKind::UPDATE_KW };
    [delete] => { $crate::SyntaxKind::DELETE_KW };
    [into] => { $crate::SyntaxKind::INTO_KW };
    [values] => { $crate::SyntaxKind::VALUES_KW };
    [set] => { $crate::SyntaxKind::SET_KW };
    [with] => { $crate::SyntaxKind::WITH_KW };
    [recursive] => { $crate::SyntaxKind::RECURSIVE_KW };
    [union] => { $crate::SyntaxKind::UNION_KW };
    [intersect] => { $crate::SyntaxKind::INTERSECT_KW };
    [except] => { $crate::SyntaxKind::EXCEPT_KW };
    [for] => { $crate::SyntaxKind::FOR_KW };
    [natural] => { $crate::SyntaxKind::NATURAL_KW };
    [lateral] => { $crate::SyntaxKind::LATERAL_KW };
    [only] => { $crate::SyntaxKind::ONLY_KW };
    [coalesce] => { $crate::SyntaxKind::COALESCE_KW };
    [nullif] => { $crate::SyntaxKind::NULLIF_KW };
    [greatest] => { $crate::SyntaxKind::GREATEST_KW };
    [least] => { $crate::SyntaxKind::LEAST_KW };
    [extract] => { $crate::SyntaxKind::EXTRACT_KW };
    [position] => { $crate::SyntaxKind::POSITION_KW };
    [substring] => { $crate::SyntaxKind::SUBSTRING_KW };
    [trim] => { $crate::SyntaxKind::TRIM_KW };
    [overlay] => { $crate::SyntaxKind::OVERLAY_KW };
    [leading] => { $crate::SyntaxKind::LEADING_KW };
    [trailing] => { $crate::SyntaxKind::TRAILING_KW };
    [both] => { $crate::SyntaxKind::BOTH_KW };
    [placing] => { $crate::SyntaxKind::PLACING_KW };
    [array] => { $crate::SyntaxKind::ARRAY_KW };
    [variadic] => { $crate::SyntaxKind::VARIADIC_KW };
}
