#pragma once

#include <cstdint>
#include <vector>

// Simple struct to give us some data layout & symbols
struct Stats {
    double mean;
    double min;
    double max;
};

// A few functions to produce nice symbols
int add(int a, int b);
int multiply(int a, int b);
Stats compute_stats(const std::vector<int>& values);

// A function that you can "fix" to play with warnings & behavior
bool is_strictly_increasing(const std::vector<int>& values);

