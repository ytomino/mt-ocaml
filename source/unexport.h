#define STATIC static
#define STATIC_DEFINITION static
#if defined(__GNUC__) || defined(__clang__)
#define UNUSED __attribute__((unused))
#else
#define UNUSED
#endif
