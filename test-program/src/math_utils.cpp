#include "math_utils.h"

#include <limits>

int add(int a, int b) {
    return a + b;
}

int multiply(int a, int b) {
    return a * b;
}

Stats compute_stats(const std::vector<int>& values) {
    Stats s{};
    if (values.empty()) {
        s.mean = 0.0;
        s.min = 0.0;
        s.max = 0.0;
        return s;
    }

    long long sum = 0;
    int minVal = std::numeric_limits<int>::max();
    int maxVal = std::numeric_limits<int>::min();

    for (int v : values) {
        sum += v;
        if (v < minVal) minVal = v;
        if (v > maxVal) maxVal = v;
    }

    s.mean = static_cast<double>(sum) / values.size();
    s.min = minVal;
    s.max = maxVal;
    return s;
}

// Deliberately written in a style that triggers some warnings with -Wall -Wextra
bool is_strictly_increasing(const std::vector<int>& values) {
    if (values.size() <= 1) {
        return true;
    }

    // You can play with this to cause / fix warnings or behavior changes
    for (std::size_t i = 2; i < values.size(); ++i) {
        if (values[i] <= values[i - 1]) {
            return false;
        }
    }
    return true;
}

