#include <string>
#include <fstream>
#include <iterator>
#include <iostream>

namespace {

template <typename T>
struct read_from_file_impl
{
    static inline void skip_ws_if_needed(std::ifstream &)
    {
    }

    typedef std::istream_iterator<T> iterator;
    static inline std::ifstream &source(std::ifstream &infile)
    {
        return infile;
    }
};

template <>
struct read_from_file_impl<char>
{
    static inline void skip_ws_if_needed(std::ifstream &infile)
    {
        infile.unsetf(std::ios::skipws);
    }

    // for char type is more speedy direct usage the underlying read buffer
    typedef std::istreambuf_iterator<char> iterator;
    static inline std::filebuf *source(std::ifstream &infile)
    {
        return infile.rdbuf();
    }
};

}

template <typename C = std::string>
C read_from_file(std::string const &filename)
{
    std::ifstream infile(filename);
    if (!infile) {
        std::cerr << "Couldn't open file: " << filename << std::endl;
        return C();
    }
    typedef read_from_file_impl<typename C::value_type> impl;

    // skipping whitespaces is the default behavior, but for char type this is wrong
    impl::skip_ws_if_needed(infile);

    typedef typename impl::iterator iterator;
    return C(iterator(impl::source(infile)), iterator());
}

