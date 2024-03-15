#if defined(__clang__)
    #pragma clang diagnostic push

    #pragma clang diagnostic ignored "-Wc++17-extensions"
    #pragma clang diagnostic ignored "-Wexpansion-to-defined"
#endif

#ifndef ALIAS_HPP
#define ALIAS_HPP

#define GENERIC_TYPE_ALIAS(expr, ...) template <__VA_ARGS__> using expr

#define FUNCTION_ALIAS(alias, ...)                                          \
  inline auto alias = [](auto &&... args)                                   \
    noexcept(noexcept(__VA_ARGS__(std::forward<decltype(args)>(args)...)))  \
    -> decltype(auto)                                                       \
  { return __VA_ARGS__(std::forward<decltype(args)>(args)...); }            \

#define GENERIC_FUNCTION_ALIAS(alias, original, ...) \
    template <__VA_ARGS__> FUNCTION_ALIAS(alias, original)

#endif // ALIAS_HPP

#if defined(__clang__)
#pragma clang diagnostic pop
#endif
