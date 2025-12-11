#pragma once

#include <string>

// Simple string utility functions - should produce identical output from both build systems
std::string to_upper(const std::string& input);
std::string to_lower(const std::string& input);
std::string trim(const std::string& input);
bool starts_with(const std::string& str, const std::string& prefix);
bool ends_with(const std::string& str, const std::string& suffix);

