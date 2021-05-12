#include "log.h"
#if INTERFACE
#include "utarray.h"
#include "utstring.h"
#endif

#include "obazl_opam.h"

LOCAL UT_string *opam_switch;
LOCAL UT_string *opam_bin;
LOCAL UT_string *opam_lib;

char *run_cmd(char *cmd)
{
    static char buf[BUFSIZ];
    FILE *fp;

    if ((fp = popen(cmd, "r")) == NULL) {
        printf("Error opening pipe!\n");
        return NULL;
    }

    while (fgets(buf, sizeof buf, fp) != NULL) {
        /* printf("SWITCH: %s\n", buf); */
        buf[strcspn(buf, "\n")] = 0;
    }

    if(pclose(fp))  {
        printf("Command not found or exited with error status\n");
        return NULL;
    }
    return buf;
}

/* dup from opam_bootstrap.c */
void config_opam(char *_opam_switch)
{
    /*
      1. discover switch
         a. check env var OPAMSWITCH
         b. use -s option
         c. run 'opam var switch'
      2. discover lib dir: 'opam var lib'
     */

    utstring_new(opam_switch);
    utstring_new(opam_bin);
    utstring_new(opam_lib);

    /* FIXME: handle switch arg */
    char *cmd, *result;
    if (_opam_switch == NULL) {
        log_info("opam: using current switch");
        cmd = "opam var switch";

        result = run_cmd(cmd);
        if (result == NULL) {
            fprintf(stderr, "FAIL: run_cmd(%s)\n", cmd);
        } else
            utstring_printf(opam_switch, "%s", result);
    }

    cmd = "opam var bin";
    result = NULL;
    result = run_cmd(cmd);
    if (result == NULL) {
        log_fatal("FAIL: run_cmd(%s)\n", cmd);
        exit(EXIT_FAILURE);
    } else
        utstring_printf(opam_bin, "%s", result);

    cmd = "opam var lib";
    result = NULL;
    result = run_cmd(cmd);
    if (result == NULL) {
        log_fatal("FAIL: run_cmd(%s)\n", cmd);
        exit(EXIT_FAILURE);
    } else
        utstring_printf(opam_lib, "%s", result);

}

UT_array *inventory_opam(void)
{
    config_opam(NULL);
    log_debug("opam switch: %s", utstring_body(opam_switch));
    log_debug("opam bin: %s", utstring_body(opam_bin));
    log_debug("opam lib: %s", utstring_body(opam_lib));


    UT_array *opam_dirs;             /* string list */
    utarray_new(opam_dirs, &ut_str_icd);

    int rc = dirseq(utstring_body(opam_lib), opam_dirs);

    return opam_dirs;
}
