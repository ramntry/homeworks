#pragma once
#include <string>

enum OS
{
    Windows
    , MacOS
    , Linux
};

inline std::string os_to_string(OS os)
{
    return os == Windows ? "win"
        : (os == MacOS   ? "mac"
        :                  "lin");
}

