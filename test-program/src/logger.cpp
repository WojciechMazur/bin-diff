#include "logger.h"

#include <iostream>

namespace {
    void log_with_prefix(const char* prefix, const std::string& msg) {
        std::cout << prefix << msg << '\n';
    }
}

void log_info(const std::string& msg) {
    #ifdef VERBOSE_LOGS
    log_with_prefix("[INFORMATION] ", msg);
    #else
    log_with_prefix("[INFO] ", msg);
    #endif
}

void log_warning(const std::string& msg) {
    log_with_prefix("[WARN] ", msg);
}

void log_error(const std::string& msg) {
    log_with_prefix("[ERR ] ", msg);
}

