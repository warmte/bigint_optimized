#include "big_integer.h"
#include <string>
#include <climits>
#include <algorithm>
#include <cassert>
#include <iostream>

typedef uint32_t uint;
typedef uint64_t ull;

const ull MAX = (ull)UINT32_MAX + 1;

//      -----------------------------
//      |      MEMORY FUNCTIONS     |
//      -----------------------------

static uint *data_alloc(size_t len) {
    size_t *res = new size_t[sizeof(size_t) + len * sizeof(uint)];
    *res = 0;
    return (uint*)(res + 1);
}

static void refs_inc(uint *p) {
    size_t *num = (size_t*)p - 1;
    (*num)++;
}

static void refs_dec(uint *p) {
    size_t *num = (size_t*)p - 1;
    assert(*num != 0);
    (*num)--;
    if (*num == 0)
        delete[] num;
}

void big_integer::copy_on_write() {
    if (tsize <= SIZE)
        return;
    size_t *num = (size_t*)(digits.dynamic.data) - 1;
    if (*num < 2)
        return;
    uint *data = digits.dynamic.data, *ndata = data_alloc(tsize);
    std::copy(data, data + tsize, ndata);
    refs_dec(data);
    refs_inc(ndata);
    digits.dynamic.data = ndata;
}

uint* big_integer::get_data() {
    return tsize > SIZE ? digits.dynamic.data : digits.static_data;
}

uint const* big_integer::get_data() const {
    return tsize > SIZE ? digits.dynamic.data : digits.static_data;
}

//      ----------------------------
//      |  VECTOR-STYLE FUNCTIONS  |
//      ----------------------------

void big_integer::resize(size_t nlen, uint val, bool cap) {
    const size_t SIZE = 2;
    if (nlen == tsize)
        return;
    uint *cur_data = get_data(), *ndata;
    if (nlen <= SIZE) {
        ndata = digits.static_data;
    } else {
        ndata = data_alloc(nlen);
        digits.dynamic.capacity = nlen;
        refs_inc(ndata);
    }

    if (cur_data != ndata) {
        std::copy(cur_data, cur_data + std::min(tsize, nlen), ndata);
    }
    if (nlen > tsize) {
        std::fill(ndata + tsize, ndata + nlen, val);
    }

    if (tsize > SIZE)
        refs_dec(digits.dynamic.data);

    if (nlen > SIZE) {
        digits.dynamic.data = ndata;
    }
    if (cap)
        tsize = nlen;
}

uint big_integer::safe_get(size_t i, uint zero, uint *digits) const {
    return i < len() ? digits[i] : zero;
}

void big_integer::push_back(uint value) {
    if (tsize + 1 > SIZE) {
        copy_on_write();
        if (tsize + 1 >= digits.dynamic.capacity) {
            digits.dynamic.capacity *= 2;
            resize(digits.dynamic.capacity, 0, false);
        }
        digits.dynamic.data[tsize] = value;
    } else {
        digits.static_data[tsize] = value;
    }
    tsize++;
}

void big_integer::pop_back() {
    tsize--;
    if (tsize == SIZE) {
        uint *ndata = digits.dynamic.data;
        std::copy(ndata, ndata + SIZE, digits.static_data);
        refs_dec(ndata);
    }
}

uint big_integer::back() {
    uint *data = get_data();
    return data[tsize - 1];
}

//      ----------------------------
//      |   BIG_INTEGER FUNCTIONS  |
//      ----------------------------

big_integer::big_integer() {
    neg = false;
    tsize = 0;
    resize(1, 0);
}

big_integer::big_integer(int a) {
    neg = a < 0;
    tsize = 0;
    resize(1, (uint)a);
    normalize();
}

big_integer::big_integer(uint a) {
    neg = false;
    tsize = 0;
    resize(1, a);
    normalize();
}

big_integer::big_integer(bool negative, size_t l) {
    neg = negative;
    tsize = 0;
    resize(l, (uint)(neg ? (MAX - 1) : 0));
}

big_integer::big_integer(big_integer const &other) noexcept: neg(other.neg), tsize(other.tsize), digits(other.digits)  {
    if (tsize > SIZE)
        refs_inc(digits.dynamic.data);
}

big_integer::big_integer(std::string const &str) {
    big_integer res(0);
    tsize = 0;
    neg = false;
    bool negative = str[0] == '-';
    for (size_t i = negative ? 1 : 0; i < str.size(); ++i) {
        res = mul_uint_bigint(res, 10) + (str[i] - '0');
    }
    if (negative) {
        res = -res;
    }
    res.normalize();
    *this = res;
}

big_integer &big_integer::operator=(big_integer const &other) noexcept {
    if (tsize > SIZE)
        refs_dec(digits.dynamic.data);
    digits = other.digits;
    tsize = other.tsize;
    neg = other.neg;
    if (tsize > SIZE)
        refs_inc(digits.dynamic.data);
    return *this;
}

big_integer big_integer::operator-() const {
    big_integer res = *this;
    res.copy_on_write();
    uint *res_digits = res.get_data();
    res.neg = !neg;
    bool carry = true;
    size_t i = 0;
    for (i = 0; i < len() && carry; ++i) {
        res_digits[i] = -res_digits[i];
        carry = res_digits[i] == 0;
    }
    for (; i < len(); ++i) {
        res_digits[i] = ~res_digits[i];
    }
    if (carry) {
        if (!res.neg)
            res.push_back(1);
        else
            res.neg = false;
    }
    res.normalize();
    return res;
}
big_integer big_integer::operator+() const {
    return *this;
}

big_integer big_integer::operator~() const {
    big_integer res = *this;
    uint *res_digits = res.get_data();
    for (size_t i = 0; i < len(); ++i) {
        res_digits[i] = ~res_digits[i];
    }
    res.neg = !neg;
    res.normalize();
    return res;
}

big_integer& big_integer::operator++() {
    copy_on_write();
    bool carry = true;
    uint *digits = get_data();

    for (size_t i = 0; i < len() && carry; ++i) {
        digits[i]++;
        carry = digits[i] == 0;
    }
    if (carry) {
        if (!neg)
            push_back(1);
        else
            neg = false;
    }
    normalize();
    return *this;
}

big_integer& big_integer::operator--(){
    copy_on_write();
    return *this = *this - 1;
}

big_integer const big_integer::operator++(int) {
    copy_on_write();
    big_integer res = *this;
    ++*this;
    return res;
}

big_integer const big_integer::operator--(int){
    copy_on_write();
    big_integer res = *this;
    *this = *this - 1;
    return res;
}

big_integer abs(const big_integer &a) {
    return a.neg ? -a : a;
}

bool operator==(big_integer const &a, big_integer const &b) {
    uint const *a_digits = a.get_data(), *b_digits = b.get_data();
    if (a.neg != b.neg || a.len() != b.len()) {
        return false;
    }
    for (size_t i = 0; i < a.len(); ++i) {
        if (a_digits[i] != b_digits[i]) {
             return false;
        }
    }
    return true;
}

bool operator!=(big_integer const &a, big_integer const &b) {
    return !(a == b);
}

bool operator<(big_integer const &a, big_integer const &b) {
    uint const *a_digits = a.get_data(), *b_digits = b.get_data();
    if (a.neg != b.neg) {
        return a.neg;
    }
    if (a.len() != b.len()) {
        return (b.len() > a.len()) != a.neg;
    }

    for (size_t i = a.len() - 1; i != (size_t)-1; --i) {
        if (a_digits[i] > b_digits[i]) {
            return false;
        } else if (b_digits[i] > a_digits[i]) {
            return true;
        }
    }
    return false;
}

bool operator>(big_integer const &a, big_integer const &b) {
    return b < a;
}

bool operator<=(big_integer const &a, big_integer const &b) {
    return a < b || a == b;
}

bool operator>=(big_integer const &a, big_integer const &b) {
    return b < a || b == a;
}

big_integer operator+(big_integer first, big_integer const &second) {
    first.copy_on_write();
    first.change_len(second.len());
    uint zero = (first.neg ? uint(MAX - 1) : 0);
    uint zero_oth = (second.neg ? uint(MAX - 1) : 0);
    uint carry = 0;
    uint *first_digits = first.get_data();
    uint const *second_digits = second.get_data();
    for (size_t i = 0; i < second.len(); ++i) {
        ull cur = (ull)carry + first_digits[i] + second_digits[i];
        first_digits[i] = (uint)cur;
        carry = uint(cur >> 32);
    }
    for (size_t i = second.len(); i < first.len(); ++i) {
        ull cur = (ull)carry + first_digits[i] + zero_oth;
        first_digits[i] = (uint)cur;
        carry = uint(cur >> 32);
    }
    uint sum = zero + zero_oth + carry;
    first.neg = (sum >> 31) > 0;
    if (sum != (first.neg ? (MAX - 1) : 0)) {
        first.push_back(sum);
    }
    first.normalize();
    return first;
}

big_integer operator-(big_integer const &a, big_integer const &b) {
    return a + -b;
}

big_integer mul_uint_bigint(big_integer const &a, uint b) {
    big_integer res(a.neg, a.len() + 1);
    ull carry = 0;

    uint const *a_digits = a.get_data();
    uint *res_digits = res.get_data();

    for (size_t i = 0; i < a.len(); ++i) {
        ull cur = carry + (ull)a_digits[i] * b;
        res_digits[i] = (uint)cur;
        carry = cur >> 32;
    }
    res_digits[res.len() - 1] = (uint)carry;
    res.normalize();
    return res;
}

big_integer operator*(big_integer const &a, big_integer const &b) {
    if (a.neg || b.neg) {
        bool fl = a.neg != b.neg;
        return !fl ? (-a * -b) : -((a.neg ? -a : a) * (b.neg ? -b : b));
    }
    big_integer res = 0;
    res.change_len(a.len() + b.len() + 2);

    uint const *a_digits = a.get_data(), *b_digits = b.get_data();
    uint *res_digits = res.get_data();

    for (size_t i = 0; i < a.len(); ++i) {
        uint carry = 0;
        for (size_t j = 0; j < b.len(); ++j) {
            ull cur = (ull)a_digits[i] * b_digits[j] + carry + res_digits[i + j];
            res_digits[i + j] = (uint)cur;
            carry = (uint)(cur >> 32);
        }
        for (size_t j = b.len(); i + j < res.len() && carry; ++j) {
            ull cur = (ull)carry + res_digits[i + j];
            res_digits[i + j] = (uint)cur;
            carry = (uint)(cur >> 32);
        }
    }

    res.normalize();
    return res;
}

void shl_sub(big_integer &a, big_integer const &b, size_t sh) {
    a.copy_on_write();
    ull cur;
    bool carry = false;
    a.change_len(b.len() + sh);
    uint *a_digits = a.get_data();
    uint const *b_digits = b.get_data();

    for (size_t i = 0; i < b.len(); ++i) {
        cur = (ull)a_digits[i + sh] - b_digits[i] - carry;
        carry = (cur >> 63) > 0;
        a_digits[i + sh] = (uint)cur;
    }
    for (size_t i = b.len() + sh; i < a.len() && carry; ++i) {
        carry = (a_digits[i] == 0);
        a_digits[i]--;
    }
    a.neg = carry;
}

bool shl_more(big_integer const &a, big_integer const &b, size_t sh) {
    if (b.len() + sh != a.len())
        return a.len() > b.len() + sh;

    uint const *a_digits = a.get_data(), *b_digits = b.get_data();
    for (size_t i = b.len() - 1; i != (size_t)-1; i--) {
        if (a_digits[i + sh] != b_digits[i])
            return a_digits[i + sh] > b_digits[i];
    }
    return true;
}

big_integer operator/(big_integer a, big_integer b) {
    a.copy_on_write();
    b.copy_on_write();
    if (a.neg || b.neg) {
        bool fl = a.neg != b.neg;
        a = (a.neg ? -a : a) / (b.neg ? -b : b);
        return fl ? -a : a;
    }
    if (a < b || a == 0) {
        return 0;
    }
    a = a << __builtin_clz(b.back());
    b = b << __builtin_clz(b.back());
    size_t n = b.len(), m = a.len() - b.len();
    big_integer q(false, m + 1);
    uint* q_digits = q.get_data(), *b_digits = b.get_data();
    if (shl_more(a, b, m)) {
        q_digits[m] = 1;
        shl_sub(a, b, m);
    }

    uint zero = uint(MAX - a.neg);
    for (size_t i = m - 1; i != (size_t)-1; --i) {
        uint *a_digits = a.get_data();
        q_digits[i] = (uint)std::min(MAX - 1, ((ull(a.safe_get(n + i, zero, a_digits)) << 32) + a.safe_get(n + i - 1, zero, a_digits)) / b_digits[n - 1]);
        shl_sub(a, mul_uint_bigint(b, q_digits[i]), i);
        while (a.neg) {
            q_digits[i] -= 1;
            a += (b << (i * 32));
        }
    }
    q.normalize();
    return q;
}

big_integer operator%(big_integer const &a, big_integer const &b) {
    return a - (a / b) * b;
}

big_integer& big_integer::operator+=(big_integer const &other) {
    return *this = *this + other;
}
big_integer& big_integer::operator-=(big_integer const &other) {
    return *this = *this - other;
}
big_integer& big_integer::operator*=(big_integer const &other) {
    return *this = *this * other;
}
big_integer& big_integer::operator/=(big_integer const &other) {
    return *this = *this / other;
}
big_integer& big_integer::operator%=(big_integer const &other) {
    return *this = *this % other;
}

big_integer operator&(big_integer first, big_integer const &second) {
    first &= second;
    return first;
}

big_integer operator|(big_integer first, big_integer const &second) {
    first |= second;
    return first;
}

big_integer operator^(big_integer first, big_integer const &second) {
    first ^= second;
    return first;
}

big_integer operator<<(big_integer const &a, uint b) {
    big_integer res = a;
    size_t bc = b >> 5, br = b & 31;
    res.change_len(a.len() + bc + 1);
    uint* res_digits = res.get_data();
    if (bc > 0) {
        for (size_t i = res.len() - 1; i >= bc; --i)
            res_digits[i] = res_digits[i - bc];
        for (size_t i = bc - 1; i != (size_t)-1; --i) {
            res_digits[i] = 0;
        }
    } else {
        for (size_t i = res.len() - 1; i != (size_t)-1; --i)
            res_digits[i] = res_digits[i - bc];
    }
    if (br != 0) {
        for (size_t i = res.len() - 1; i > 0; --i) {
            res_digits[i] <<= br;
            res_digits[i] |= (res_digits[i - 1] >> (32 - br));
        }
        res_digits[0] <<= br;
    }
    res.normalize();
    return res;
}

big_integer operator>>(big_integer const &a, uint b) {
    big_integer res = a;
    uint *res_digits = res.get_data();
    uint zero = (a.neg ? (uint)(MAX - 1) : 0);
    size_t bc = b >> 5, br = b & 31;
    for (size_t i = 0; i < res.len(); ++i) {
        res_digits[i] = (i + bc < res.len() ? res_digits[i + bc] : zero);
    }
    if (br != 0) {
        for (size_t i = 0; i < res.len(); ++i) {
            res_digits[i] >>= br;
            res_digits[i] |= (i + 1 == res.len() ? zero : res_digits[i + 1]) << (32 - br);
        }
    }
    res.normalize();
    return res;
}

big_integer& big_integer::operator&=(big_integer const &other) {
    change_len(other.len());
    uint *digits = get_data();
    uint const *other_digits = other.get_data();
    uint zero = (other.neg ? uint(MAX - 1) : 0);
    for (size_t i = 0; i < other.len(); ++i) {
        digits[i] &= other_digits[i];
    }
    for (size_t i = other.len(); i < len(); ++i) {
        digits[i] &= zero;
    }
    neg &= other.neg;
    return *this;
}
big_integer& big_integer::operator|=(big_integer const &other) {
    change_len(other.len());
    uint *digits = get_data();
    uint const *other_digits = other.get_data();
    uint zero = (other.neg ? uint(MAX - 1) : 0);
    for (size_t i = 0; i < other.len(); ++i) {
        digits[i] |= other_digits[i];
    }
    for (size_t i = other.len(); i < len(); ++i) {
        digits[i] |= zero;
    }
    neg |= other.neg;
    return *this;
}
big_integer& big_integer::operator^=(big_integer const &other) {
    change_len(other.len());
    uint *digits = get_data();
    uint const *other_digits = other.get_data();
    uint zero = (other.neg ? uint(MAX - 1) : 0);
    for (size_t i = 0; i < other.len(); ++i) {
        digits[i] ^= other_digits[i];
    }
    for (size_t i = other.len(); i < len(); ++i) {
        digits[i] ^= zero;
    }
    neg ^= other.neg;
    return *this;
}
big_integer& big_integer::operator<<=(uint other) {
    return *this = *this << other;
}
big_integer& big_integer::operator>>=(uint other) {
    return *this = *this >> other;
}

std::string to_string(big_integer const &a) {
    std::string res;
    big_integer cur = abs(a);
    if (cur == 0) {
        return "0";
    }
    while (cur > 0) {
        res += (char)((cur % 10).get_data()[0] + '0');
        cur /= 10;
    }
    res += (a.neg ? "-" : "");
    std::reverse(res.begin(), res.end());
    return res;
}

std::ostream& operator<<(std::ostream& s, big_integer const& a) {
   return s << to_string(a);
}

size_t big_integer::len() const {
    return tsize;
}

void big_integer::normalize() {
    copy_on_write();
    while (len() > 0 && ((back() == 0 && !neg) || (back() == (MAX - 1) && neg))) {
        pop_back();
    }
    if (len() == 0) {
        resize(1, neg ? (uint)(MAX - 1) : 0);
    }
}

void big_integer::change_len(size_t nlen) {
    if (nlen <= len()) return;
    resize(nlen, (uint)(neg ? (MAX - 1) : 0));
}


big_integer::~big_integer() {
    if (tsize > SIZE)
        refs_dec(digits.dynamic.data);
}