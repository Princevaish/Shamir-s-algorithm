
#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <string>
#include <algorithm>
#include <sstream>
#include <cmath>
using namespace std;

// BigInteger class for handling 256-bit numbers
class BigInteger {
private:
    vector<long long> digits;
    bool negative;
    static const long long BASE = 1000000000LL; // 10^9
    
public:
    BigInteger() : negative(false) { digits.push_back(0); }
    
    BigInteger(long long num) : negative(num < 0) {
        if (num < 0) num = -num;
        if (num == 0) {
            digits.push_back(0);
        } else {
            while (num > 0) {
                digits.push_back(num % BASE);
                num /= BASE;
            }
        }
    }
    
    BigInteger(const string& str) {
        negative = false;
        digits.clear();
        string s = str;
        
        if (s[0] == '-') {
            negative = true;
            s = s.substr(1);
        }
        
        if (s.empty() || s == "0") {
            digits.push_back(0);
            negative = false;
            return;
        }
        
        // Process string in chunks of 9 digits
        for (int i = s.length(); i > 0; i -= 9) {
            int start = max(0, i - 9);
            string chunk = s.substr(start, i - start);
            digits.push_back(stoll(chunk));
        }
        
        removeLeadingZeros();
    }
    
    void removeLeadingZeros() {
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }
        if (digits.size() == 1 && digits[0] == 0) {
            negative = false;
        }
    }
    
    BigInteger operator+(const BigInteger& other) const {
        if (negative != other.negative) {
            if (negative) return other - (-*this);
            return *this - (-other);
        }
        
        BigInteger result;
        result.negative = negative;
        result.digits.clear();
        
        long long carry = 0;
        size_t maxSize = max(digits.size(), other.digits.size());
        
        for (size_t i = 0; i < maxSize || carry; i++) {
            long long sum = carry;
            if (i < digits.size()) sum += digits[i];
            if (i < other.digits.size()) sum += other.digits[i];
            
            result.digits.push_back(sum % BASE);
            carry = sum / BASE;
        }
        
        result.removeLeadingZeros();
        return result;
    }
    
    BigInteger operator-(const BigInteger& other) const {
        if (negative != other.negative) {
            return *this + (-other);
        }
        
        if (negative) return (-other) - (-*this);
        
        if (*this < other) {
            BigInteger result = other - *this;
            result.negative = true;
            return result;
        }
        
        BigInteger result;
        result.digits.clear();
        
        long long borrow = 0;
        for (size_t i = 0; i < digits.size(); i++) {
            long long diff = digits[i] - borrow;
            if (i < other.digits.size()) diff -= other.digits[i];
            
            if (diff < 0) {
                diff += BASE;
                borrow = 1;
            } else {
                borrow = 0;
            }
            
            result.digits.push_back(diff);
        }
        
        result.removeLeadingZeros();
        return result;
    }
    
    BigInteger operator*(const BigInteger& other) const {
        BigInteger result;
        result.digits.assign(digits.size() + other.digits.size(), 0);
        result.negative = (negative != other.negative);
        
        for (size_t i = 0; i < digits.size(); i++) {
            long long carry = 0;
            for (size_t j = 0; j < other.digits.size() || carry; j++) {
                long long prod = result.digits[i + j] + carry;
                if (j < other.digits.size()) {
                    prod += (long long)digits[i] * other.digits[j];
                }
                result.digits[i + j] = prod % BASE;
                carry = prod / BASE;
            }
        }
        
        result.removeLeadingZeros();
        return result;
    }
    
    BigInteger operator/(const BigInteger& divisor) const {
        if (divisor.isZero()) throw runtime_error("Division by zero");
        
        BigInteger dividend = *this;
        dividend.negative = false;
        BigInteger div = divisor;
        div.negative = false;
        
        if (dividend < div) return BigInteger(0);
        
        // Simple division for small numbers
        if (div.digits.size() == 1 && dividend.digits.size() <= 2) {
            long long divVal = div.digits[0];
            long long divdVal = dividend.digits[0];
            if (dividend.digits.size() == 2) {
                divdVal += dividend.digits[1] * BASE;
            }
            BigInteger result(divdVal / divVal);
            result.negative = (negative != divisor.negative) && !result.isZero();
            return result;
        }
        
        // For larger numbers, use string-based division
        BigInteger result(0);
        BigInteger current(0);
        string dividendStr = dividend.toString();
        
        for (char digit : dividendStr) {
            current = current * BigInteger(10) + BigInteger(digit - '0');
            
            int quotient = 0;
            while (current >= div) {
                current = current - div;
                quotient++;
            }
            
            result = result * BigInteger(10) + BigInteger(quotient);
        }
        
        result.negative = (negative != divisor.negative) && !result.isZero();
        return result;
    }
    
    bool operator<(const BigInteger& other) const {
        if (negative != other.negative) return negative;
        
        if (negative) return (-other) < (-*this);
        
        if (digits.size() != other.digits.size()) {
            return digits.size() < other.digits.size();
        }
        
        for (int i = digits.size() - 1; i >= 0; i--) {
            if (digits[i] != other.digits[i]) {
                return digits[i] < other.digits[i];
            }
        }
        return false;
    }
    
    bool operator==(const BigInteger& other) const {
        return negative == other.negative && digits == other.digits;
    }
    
    bool operator>=(const BigInteger& other) const {
        return !(*this < other);
    }
    
    BigInteger operator-() const {
        BigInteger result = *this;
        if (!isZero()) result.negative = !result.negative;
        return result;
    }
    
    bool isZero() const {
        return digits.size() == 1 && digits[0] == 0;
    }
    
    string toString() const {
        if (isZero()) return "0";
        
        string result;
        if (negative) result += "-";
        
        result += to_string(digits.back());
        for (int i = digits.size() - 2; i >= 0; i--) {
            string chunk = to_string(digits[i]);
            result += string(9 - chunk.length(), '0') + chunk;
        }
        
        return result;
    }
};
