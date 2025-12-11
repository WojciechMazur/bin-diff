#include "math_utils.h"
#include "logger.h"

#include <iostream>
#include <vector>
#include <string>

static std::vector<int> parse_args(int argc, char** argv) {
    std::vector<int> values;
    for (int i = 1; i < argc; ++i) {
        try {
            values.push_back(std::stoi(argv[i]));
        } catch (...) {
            log_warning(std::string("Ignoring invalid number: ") + argv[i]);
        }
    }
    return values;
}

int main(int argc, char** argv) {
    auto values = parse_args(argc, argv);

    if (values.empty()) {
        log_info("No numbers provided. Example: ./demo 1 2 3 4");
        return 0;
    }

    Stats s = compute_stats(values);

    std::cout << "Count: " << values.size() << "\n";
    std::cout << "Mean:  " << s.mean << "\n";
    std::cout << "Min:   " << s.min << "\n";
    std::cout << "Max:   " << s.max << "\n";

    bool increasing = is_strictly_increasing(values);
    std::cout << "Strictly increasing: " << (increasing ? "YES" : "NO") << "\n";

    int sum = 0;
    for (int v : values) {
        sum = add(sum, v);
    }

    std::cout << "Sum via add(): " << sum << "\n";
    std::cout << "Sum * 2 via multiply(): " << multiply(sum, 2) << "\n";

    return 0;
}

