#ifndef BIGINT_BIG_INTEGER_H
#define BIGINT_BIG_INTEGER_H

#include <string>
#include <memory>

struct big_integer {
    typedef uint32_t uint;
    typedef uint64_t ull;
    const static size_t SIZE = 2;

    big_integer();
    big_integer(int a);
    big_integer(uint a);
    big_integer (bool negative, size_t l);
    big_integer(big_integer const& other) noexcept;
    explicit big_integer(std::string const& str);

    big_integer& operator=(big_integer const& other) noexcept;
    big_integer operator-() const;
    big_integer operator+() const;
    big_integer operator~() const;

    big_integer& operator++();
    big_integer const operator++(int);

    big_integer& operator--();
    big_integer const operator--(int);
    friend big_integer abs(const big_integer &a);

    friend bool operator==(big_integer const& a, big_integer const& b);
    friend bool operator!=(big_integer const& a, big_integer const& b);
    friend bool operator<(big_integer const& a, big_integer const& b);
    friend bool operator>(big_integer const& a, big_integer const& b);
    friend bool operator<=(big_integer const& a, big_integer const& b);
    friend bool operator>=(big_integer const& a, big_integer const& b);

    friend big_integer operator+(big_integer a, big_integer const& b);
    friend big_integer operator-(big_integer const &a, big_integer const& b);
    friend big_integer operator*(big_integer const &a, big_integer const& b);
    friend big_integer operator/(big_integer a, big_integer b);
    friend big_integer operator%(big_integer const &a, big_integer const& b);

    big_integer& operator+=(big_integer const &other);
    big_integer& operator-=(big_integer const &other);
    big_integer& operator*=(big_integer const &other);
    big_integer& operator/=(big_integer const &other);
    big_integer& operator%=(big_integer const &other);

    friend big_integer operator&(big_integer a, big_integer const& b);
    friend big_integer operator|(big_integer a, big_integer const& b);
    friend big_integer operator^(big_integer a, big_integer const& b);
    friend big_integer operator<<(big_integer const &a, uint b);
    friend big_integer operator>>(big_integer const &a, uint b);

    big_integer& operator&=(big_integer const &other);
    big_integer& operator|=(big_integer const &other);
    big_integer& operator^=(big_integer const &other);
    big_integer& operator<<=(uint other);
    big_integer& operator>>=(uint other);

    friend std::string to_string(big_integer const& a);
    ~big_integer();
private:
    bool neg;
    size_t tsize;
    union {
        uint static_data[SIZE];
        struct {
            uint *data;
            size_t capacity;
        } dynamic;
    } digits;

    uint *get_data();
    uint const *get_data() const;
    void copy_on_write();

    size_t len() const;
    void normalize();
    void change_len(size_t nlen);
    void resize(size_t nlen, uint val = 0, bool cap = true);
    friend void shl_sub(big_integer &a, big_integer const &b, size_t sh);
    friend bool shl_more(big_integer const &a, big_integer const &b, size_t sh);

    friend big_integer mul_uint_bigint(big_integer const &num, uint b);
    uint safe_get(size_t i, uint zero, uint *digits) const;

    void push_back(uint val);
    void pop_back();
    uint back();

};

std::ostream& operator<<(std::ostream& s, big_integer const& a);



#endif //BIGINT_BIG_INTEGER_H
