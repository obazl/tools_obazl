load("@bazel_skylib//rules:common_settings.bzl", "bool_flag")

def bool_opt(name, flag="", default=False):
    _flag = flag if flag else "-" + name
    bool_flag( name = name, build_setting_default = default )
    native.config_setting( name = "enable_" + name, flag_values = { ":" + name: "True" })
    native.config_setting( name = "disable_" + name, flag_values = { ":" + name: "False" })
    return select({
        ":enable_" + name: [_flag],
        ":disable_" + name: ["-no" + _flag],
        # "//conditions:default": []
    }, no_match_error = "BOOL OPT FAIL: %s" % name)
