SRCS=" \
	api.c \
	metalexer.c \
	metaparser.c \
	metaparser_impl.c \
	meta_entries.c \
	meta_flags.c
meta_packages.c
meta_properties.c
meta_settings.c
meta_values.c
driver.c
utils.c"


$(BUILD_DIR)/%.c.o: %.c
	$(MKDIR_P) $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@


mkhdrs:

    srcs = [
        "api.c",
        # "debug.c",
        "metalexer.c",
        "metaparser.c",
        "metaparser_impl.c",
        "meta_entries.c",
        "meta_flags.c",
        "meta_packages.c",
        "meta_properties.c",
        "meta_settings.c",
        "meta_values.c",
        "driver.c",
        "utils.c",
        "@uthash//:include"
    ],
    outs = [
        "api.h",
        "metalexer.h",
        "metaparser.h",
        "metaparser_impl.h",
        "meta_entries.h",
        "meta_flags.h",
        "meta_packages.h",
        "meta_properties.h",
        "meta_settings.h",
        "meta_values.h",
        "utils.h",
        "driver.h"
    ],
    cmd = "\n".join([
        "SRC1=$(location driver.c)",
        "SRCDIR1=`dirname $$SRC1`",
        "$(location @makeheaders//:makeheaders) \\",
        "    $(location api.c) \\",
        "    $(location metalexer.c) \\",
        "    $(location metaparser.c) \\",
        "    $(location metaparser_impl.c) \\",
        "    $(location meta_entries.c) \\",
        "    $(location meta_flags.c) \\",
        "    $(location meta_packages.c) \\",
        "    $(location meta_properties.c) \\",
        "    $(location meta_settings.c) \\",
        "    $(location meta_values.c) \\",
        "    $(location driver.c) \\",
        "    $(location utils.c) \\",
        "    $(locations @uthash//:include)",
        "cp $${SRCDIR1}/*.h $(@D)",
    ]),
    tools = ["@makeheaders//:makeheaders"]
)
