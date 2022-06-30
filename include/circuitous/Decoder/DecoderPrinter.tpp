
namespace circ::decoder {
    template < int L > requires SupportedEncodingSize< L >
    uint64_t OptionalBitArray< L >::to_uint64() const {
        auto if_ignore = [&](InputType i) {return negate( to_val( InputType::ignore ));};
        auto if_not_ignore = [&](InputType i) {return to_val( i );};
        return convert_to_uint64( if_ignore, if_not_ignore );
    }

    template < int L > requires SupportedEncodingSize< L >

    uint64_t OptionalBitArray< L >::ignored_bits_to_uint64() const {
        auto if_ignore = [&](InputType i) {return to_val( InputType::ignore );};
        auto if_not_ignore = [&](InputType i) {return negate( to_val( InputType::ignore ));};
        return convert_to_uint64( if_ignore, if_not_ignore );
    }

    template < int L > requires SupportedEncodingSize< L >

    uint64_t
    OptionalBitArray< L >::convert_to_uint64(auto if_ignore, auto if_not_ignore) const {
        uint64_t val = 0;
        for (std::size_t i = 0; i < L; i++) {
            if ((*this)[ i ] == InputType::ignore )
                val |= if_ignore((*this)[ i ] ) << i;
            else
                val |= if_not_ignore((*this)[ i ] ) << i;
        }
        return val;
    }

    template<int L> requires SupportedEncodingSize<L>
    bool OptionalBitArray<L>::contains_ignore_bit() const {
        for (auto &b: (*this)) {
            if ( b == InputType::ignore ) {
                return true;
            }
        }
        return false;
    }

    template<int L> requires SupportedEncodingSize<L>
    bool OptionalBitArray<L>::contains_only_ignore_bits() const {
        for (auto &b: (*this)) {
            if ( b != InputType::ignore ) {
                return false;
            }
        }
        return true;
    }
}